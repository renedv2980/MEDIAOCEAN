*          DATA SET REGAV18S   AT LEVEL 013 AS OF 05/01/02                      
*PHASE T81318C,+0                                                               
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLST                                                                
*INCLUDE RECUP                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'T81318 - REPPAK FILE MAINT - RATE CARD AVAIL MAINT'             
********************************************************************            
*                                                                               
*        NOV98 - RDETAIL RECORD ('Z' INV RATE RECORDS)                          
*                                                                               
********************************************************************            
T81318   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81318,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T81318+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
         ST    R5,RELO                                                          
*                                                                               
         L     RF,=V(RECUP)                                                     
         L     RE,RELO                                                          
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,VRECUP                                                        
*                                                                               
         L     RF,=V(DLFLD)                                                     
         L     RE,RELO                                                          
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,VDLFLD                                                        
*                                                                               
         L     RF,=A(DWNL)                                                      
         L     RE,RELO                                                          
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,ADWNL                                                         
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
*                                                                               
         LR    R3,RA               MOVE PROFILE TO LOCAL STORAGE                
         AH    R3,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R3                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
         DROP  R3                                                               
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
*                                                                               
         CLI   ACTNUM,ACTDEL       DELETE                                       
         BE    MAIN10                                                           
         CLI   ACTNUM,ACTREST      AND RESTORE ARE INVALID                      
         BE    MAIN10                                                           
         CLI   ACTNUM,ACTDIS       DISPLAY INVALID                              
         BNE   MAIN20                                                           
*                                                                               
MAIN10   DS    0H                                                               
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         J     ERREND                                                           
*                                                                               
MAIN20   DS    0H                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************************************               
*        DISPLAY RECORD                                                         
*****************************************************************               
DREC     DS    0H                                                               
         GOTO1 =A(DISPLIST),RR=RELO                                             
*                                                                               
DRECX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
*        PRINT REPORT                                                           
*****************************************************************               
PREP     DS    0H                                                               
         CLC   =C'DOWN',CONOUT     DOWNLOAD?                                    
         BNE   PREP10                                                           
*                                                                               
         GOTO1 ADWNL,DMCB,(RC),DWNINIT                                          
*                                                                               
         GOTO1 =A(DNLRPT),RR=RELO   DOWNLOADABLE                                
         B     PREPX                                                            
*                                                                               
PREP10   DS    0H                  REGULAR REPORT                               
         GOTO1 =A(PRNTRPT),RR=RELO                                              
*                                                                               
PREPX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
*        VALIDATE KEY                                                           
*****************************************************************               
VKEY     DS    0H                                                               
         MVC   REPHLD,AGENCY       SAVE THE REP                                 
*                                                                               
         XC    LASTKEY,LASTKEY                                                  
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,RNMSSTAH         VALIDATE STATION                             
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
         GOTO1 VALISTA                                                          
*                                                                               
         MVC   STAHLD,WORK                                                      
         MVI   STAHLD+4,C'T'                                                    
         CLI   WORK+4,C' '                                                      
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+4                                               
         CLI   WORK+40,C' '                                                     
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+40 CHECK SATTELITE                              
*                                                                               
         LA    R2,RNMCODH          VALIDATE RATE CODE                           
         CLI   5(R2),0                                                          
         JE    ERREND                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),3             MUST BE AT LEAST 3 CHARS                     
         JL    ERREND                                                           
         MVC   CODHLD,8(R2)                                                     
         OC    CODHLD,SPACES                                                    
*                                                                               
         LA    R2,RNMLENH          VALIDATE LENGTH                              
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
*                                                                               
         XC    LENHLD,LENHLD                                                    
*                                                                               
         LR    RE,R2                                                            
         LA    RE,8(RE)            POINT TO FIELD                               
         ZIC   RF,5(R2)                                                         
         ZIC   R1,5(R2)                                                         
*                                                                               
VK50     CLI   0(RE),C'0'          MUST BE NUMBERIC                             
         BL    VK60                                                             
         CLI   0(RE),C'9'                                                       
         BH    VK60                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,VK50                                                          
         B     VK70                                                             
*                                                                               
VK60     MVI   ERROR,INVALID                                                    
         C     RF,=F'1'            IF NOT IN LAST POSITION ERROR                
         JNE   ERREND                                                           
         BCTR  R1,0                CHECK FOR MINUTES/SECONDS INDICATOR          
         CLI   0(RE),C'M'                                                       
         BNE   *+12                                                             
         OI    LENHLD,X'80'                                                     
         B     VK70                                                             
         CLI   0(RE),C'S'                                                       
         JNE   ERREND                                                           
*                                                                               
VK70     BCTR  R1,0                CONVERT TO BINARY                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         STCM  R0,1,LENHLD+1                                                    
*                                                                               
         CLI   ACTEQU,ACTREP       REPORT?                                      
         BE    VKEYX               YES                                          
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,RNMQTRH          VALIDATE QUARTER                             
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,QTRTABA                                                       
         LA    RF,4                                                             
*                                                                               
VK100    DS    0H                  CHECK IF QUARTER IS IN TABLE                 
         MVI   ERROR,INVALID                                                    
         CLC   8(2,R2),0(RE)       IN TABLE?                                    
         BE    VK110                                                            
*                                                                               
         LA    RE,QTRTABAL(RE)     BUMP TO NEXT ENTRY                           
         BCT   RF,VK100                                                         
         J     ERREND                                                           
*                                                                               
VK110    DS    0H                                                               
         MVC   QTRHLD,1(RE)                                                     
         NI    QTRHLD,X'0F'        MAKE IT BINARY                               
*                                                                               
         MVC   WORK+1(2),2(RE)     START BRD DATE RANGE                         
         MVC   WORK+4(2),4(RE)     END BRD DATE RANGE                           
         MVC   QTRBIT,6(RE)        QTR BIT                                      
*                                                                               
         CLI   10(R2),C'/'                                                      
         JNE   ERREND                                                           
*                                                                               
         MVC   HALF,11(R2)         CHECK OUT THE YEAR                           
         LA    RE,HALF                                                          
         LA    RF,2                                                             
*                                                                               
VK130    CLI   0(RE),C'0'          MUST BE NUMERIC                              
         JL    ERREND                                                           
         CLI   0(RE),C'9'                                                       
         JH    ERREND                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,VK130                                                         
*                                                                               
         XC    DUB,DUB                                                          
*                                                                               
         MVC   DUB(2),11(R2)       MOVE IN YEAR YY                              
         MVC   DUB+2(4),=C'0101'   MM/DD                                        
         GOTO1 DATCON,DMCB,(0,DUB),(3,TMPDTEE)                                  
         MVC   YEARHLD,TMPDTEE     YEAR IN BINARY                               
*                                                                               
         MVC   WORK(1),YEARHLD     MOVE YEAR INTO DATE RANGE                    
         MVC   WORK+3(1),YEARHLD                                                
*                                                                               
*--GET THE BROADCAST DATE RANGE FOR THIS QTR                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(2,STENDQTR)                             
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(19,BRDQTRST)                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(2,STENDQTR+2)                         
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(19,BRDQTREN)                          
*                                                                               
         XC    WORK,WORK           GET FIRST AND LAST WEEK OF BRD YEAR          
         MVC   WORK+1(2),=X'010F'                                               
         MVC   WORK(1),YEARHLD                                                  
         MVC   WORK+4(2),=X'0C0F'                                               
         MVC   WORK+3(1),YEARHLD                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(19,STBRDYRJ)                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(19,ENBRDYRJ)                          
*                                                                               
         GOTO1 =A(CHKRTCD),RR=RELO   CHECK IF VALID RATE CODE                   
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP                                                 
         MVC   RINVKSTA,STAHLD                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    VKEYX                                                            
*                                                                               
         MVC   KEY(27),KEYSAVE     SET GENCON ERROR                             
*                                                                               
VKEYX    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
QTRTABA  DC    CL2'Q1',XL5'010F030F80'                                          
         DC    CL2'Q2',XL5'040F060F40'                                          
         DC    CL2'Q3',XL5'070F090F20'                                          
         DC    CL2'Q4',XL5'0A0F0C0F10'                                          
QTRTABAL EQU   7                                                                
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        VALIDATE RECORD                                                        
*****************************************************************               
VREC     DS    0H                                                               
         CLI   CURSCRN,0                                                        
         BNE   VR05                                                             
         XC    LASTKEY,LASTKEY                                                  
         GOTO1 =A(DISPLIST),RR=RELO                                             
*                                                                               
VR05     DS    0H                                                               
         GOTO1 =A(CHKOPTS),RR=RELO   CHECK OPTIONS                              
*                                                                               
         CLI   CURSCRN,WEEKSCRN      WEEKLY SCREEN?                             
         BE    VR150                                                            
*                                                                               
*  CHECK IF ANY NEW INPUT ON SCREEN                                             
*                                                                               
VR10     DS    0H                                                               
         GOTO1 =A(CHKSCRN),DMCB,RNASEL1H,RNAENDH,RR=RELO                        
         TM    MYFLAG,VALSCRN      VALIDATE SCREEN?                             
         BO    VR100                                                            
*                                                                               
         GOTO1 =A(DISPLIST),RR=RELO                                             
         B     VRECX                                                            
*                                                                               
VR100    DS    0H                                                               
         GOTO1 =A(VALLCOST),RR=RELO     CHECK COSTS ON LISTING SCREEN           
*                                                                               
         GOTO1 =A(VALLSEL),RR=RELO      CHECK SELECT FIELDS                     
         TM    MYFLAG,GOWKSCRN          SELECTED ANY HDRS?                      
         BO    VR160                                                            
         B     VRECX                                                            
*                                                                               
VR150    DS    0H                                                               
*                                                                               
*  CHECK IF ANY NEW INPUT ON SCREEN                                             
*                                                                               
         GOTO1 =A(CHKSCRN),DMCB,RNWEFF1H,RNWENDH,RR=RELO                        
         TM    MYFLAG,VALSCRN      NEW INPUT - VALIDATE SCREEN?                 
         BO    VR155               YES                                          
*                                                                               
*!!      L     RF,ACURHDR               A(HEADER IN KEYTAB)                     
*                                                                               
         LA    RF,KEYTAB                                                        
         AH    RF,KTABDISP         DISPLACEMENT INTO TABLE                      
*                                                                               
         USING KEYTABD,RF                                                       
         NI    KTFLAG,X'FF'-KTSEL       DONE WITH THIS HEADER                   
         B     VR160                                                            
         DROP  RF                                                               
*                                                                               
VR155    DS    0H                                                               
         GOTO1 =A(VALWWK),RR=RELO       VALIDATE WEEKLY SCREEN                  
*                                                                               
VR160    DS    0H                                                               
         GOTO1 =A(DISPWKLY),RR=RELO     DISPLAY WEEKLY BREAKOUT SCREEN          
         TM    MYFLAG,DISPSEL           DISPLAYED ALL HDRS SELECTED?            
         BO    VRECX                    NO                                      
*                                                                               
         GOTO1 =A(DISPLIST),RR=RELO     REDISPLAY LISTING SCREEN                
*                                                                               
VRECX    DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
ERREND   GOTO1 ERREX                                                            
*                                                                               
NEXTFLD  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*        PRINT REPORT                                                           
*****************************************************************               
PRNTRPT  NTR1  BASE=*,LABEL=*                                                   
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         NI    DMINBTS,X'7F'       TURN OFF READ FOR UPDATE                     
*                                                                               
         GOTO1 =A(CHKOPTS),RR=RELO   CHECK OPTIONS                              
         GOTO1 =A(CHKRTCD),RR=RELO   VALIDATE QTRS REQUESTED                    
*                                                                               
         XC    INVRFILT,INVRFILT                                                
         MVI   ERROR,INVALID                                                    
*                                                                               
         LA    R2,RNRINVH                                                       
         CLI   5(R2),0             ANY INV FILTER?                              
         BE    PR20                                                             
*                                                                               
         CLI   5(R2),4             ONLY 1 INV #?                                
         BNE   *+14                                                             
         MVC   INVRFILT(4),8(R2)                                                
         B     PR20                                                             
*                                                                               
         CLI   5(R2),9             RANGE OF INV#'S?                             
         JNE   ERREND                                                           
         CLI   RNRINV+4,C'-'                                                    
         JNE   ERREND                                                           
*                                                                               
         MVC   INVRFILT(4),8(R2)                                                
         MVC   INVRFILT+4(4),13(R2)                                             
*                                                                               
PR20     DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP                                                 
         MVC   RINVKSTA,STAHLD                                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     PR30                                                             
*                                                                               
PRSEQ    DS    0H                                                               
         LA    R6,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
PR30     DS    0H                                                               
         CLC   KEY(17),KEYSAVE                                                  
         BNE   PRNTRPTX                                                         
*                                                                               
         CLI   RINVKSRC,0          MUST BE A HEADER                             
         BNE   PRSEQ                                                            
         CLI   RINVKINV+3,0        ONLY NEW INVENTORIES ALLOWED                 
         BE    PRSEQ                                                            
*                                                                               
*  GET HDR TO SEE IF IT FALLS WITHIN THIS BRD YEAR                              
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         MVC   HDRKEY,KEY          SAVE AWAY HEADER KEY                         
*                                                                               
         L     R6,AIO                                                           
*                                                                               
*   IS HEADER IN DATE RANGE FOR THIS QUARTER?                                   
*&&DO                                                                           
         CLC   RINVPEFF(2),ENBRDYRC        HDR START > BRD YR END?              
         BH    PRSEQ                                                            
         OC    RINVPEFF+2(2),RINVPEFF+2    ANY END DATE ON HDR?                 
         BZ    PR40                        NO - IT'S IN THE DATE RANGE          
         CLC   RINVPEFF+2(2),STENDQTR      HDR END < BRD YR START?              
         BL    PRSEQ                                                            
*&&                                                                             
*                                                                               
         LA    RF,QTRRFILT                                                      
*                                                                               
PR35     DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    PRSEQ                                                            
*                                                                               
         CLC   RINVPEFF(2),4(RF)           HDR START > QTR END?                 
         BH    PR37                                                             
         OC    RINVPEFF+2(2),RINVPEFF+2    ANY END DATE ON HDR?                 
         BZ    PR40                        NO - IT'S IN THE DATE RANGE          
         CLC   RINVPEFF+2(2),2(RF)         HDR END < BRD YR START?              
         BNL   PR40                                                             
*                                                                               
PR37     DS    0H                                                               
         LA    RF,6(RF)                                                         
         B     PR35                                                             
*                                                                               
PR40     DS    0H                  CHECK FILTERS                                
         OC    DYPTRFLT,DYPTRFLT   ANY DAYPART FILTERS?                         
         BZ    PR70                                                             
         CLC   =C'ALL',DYPTRFLT    ALL DAYPART FILTERS?                         
         BE    PR70                                                             
*                                                                               
         LA    RF,RINVDP           DYPTS IN HDR                                 
         LA    R3,DYPTRFLT         DYPT FILTERS                                 
         LA    R5,MAXDPT#          MAX # OF DYPTS                               
*                                                                               
PR50     DS    0H                                                               
         CLI   0(RF),0             FOUND DYPT MATCH?                            
         BE    PRSEQ               NO                                           
*                                                                               
         LA    R3,DYPTRFLT         DYPT FILTERS                                 
         LA    R4,MAXDPT#                                                       
*                                                                               
PR60     DS    0H                                                               
         CLC   0(1,R3),0(RF)       MATCHING DYPT?                               
         BE    PR70                YES                                          
*                                                                               
         LA    R3,1(R3)            CHECK NEXT FILTER                            
         BCT   R4,PR60                                                          
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   R5,PR50                                                          
         B     PRSEQ               NO MATCHES FOUND                             
*                                                                               
PR70     DS    0H                                                               
         OC    INVRFILT,INVRFILT     ANY INV# FILTER?                           
         BZ    PR90                                                             
*                                                                               
         CLI   INVRFILT+4,0         FIND RANGE OF INV#'S?                       
         BNE   PR80                 YES                                         
*                                                                               
         CLC   RINVKINV,INVRFILT    SAME INV#                                   
         BNE   PRSEQ                                                            
         B     PR90                                                             
*                                                                               
PR80     DS    0H                                                               
         CLC   RINVKINV,INVRFILT    INV HEADER < INV FILT START?                
         BL    PRSEQ                                                            
         CLC   RINVKINV,INVRFILT+4  INV HEADER > INV FILT END?                  
         BH    PRSEQ                                                            
*                                                                               
PR90     DS    0H                                                               
         BAS   RE,PRNTHDR          PRINT HEADER INFO                            
*                                                                               
         GOTO1 =A(GETEQU#),RR=RELO                                              
         TM    MYFLAG,GOTEQU#      'Z' RECORD EXISTS?                           
         BO    PR100               NO - NO RATES TO DISPLAY                     
*                                                                               
         BAS   RE,BLDWKTBL         BUILD TABLE OF VALID BROAD. WEEKS            
*&&DO                                                                           
         BAS   RE,PRNTWEEK         PRINT WEEKLY DATES                           
*                                                                               
         MVC   P(7),=C'NO COST'                                                 
         BAS   RE,PRINT                                                         
*&&                                                                             
         B     PR170                                                            
*                                                                               
PR100    DS    0H                                                               
         LA    RF,QTRRFILT         REQUESTED QTRS                               
         MVC   CURQTFLT,1(RF)      CUR QTR TO FILTER ON                         
*                                                                               
         LA    RF,6(RF)                                                         
         ST    RF,ANXTQTR          A(NEXT QTR TO FILTER)                        
*                                                                               
PR103    DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   KEY(24),HDRKEY      COPY KEY UP TO SOURCE                        
*                                                                               
         MVI   RINVKSRC,C'Z'       GAV RATE RECORD                              
         MVC   RINVKNUM,EQUNUM     UNIQUE GAV NUMBER EQUATE                     
         MVC   RINVKYR,YEARHLD     AVAIL YEAR                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PR170               RATE RECORD DOESN'T EXIST YET                
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         L     R2,AIO3             BUILD TABLE FOR ALL ELEMENTS                 
         USING WKRATED,R2                                                       
         NI    MYFLAG,X'FF'-QTHASCST                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
PR105    DS    0H                                                               
         L     R6,AIO2                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'03'        GET WEEKLY RATE INFO                         
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING RIAVL,R6                                                         
*                                                                               
PR110    DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   PR160                                                            
*                                                                               
         CLC   RIAVQTR,CURQTFLT    THIS QUARTER?                                
         BNE   PR110                                                            
*                                                                               
         MVC   WKDATE,RIAVWEEK     WEEK DATE (JULIAN) INTO TABLE                
*                                                                               
         MVC   TMPDTEJ,HDRSTRTJ    START WEEK                                   
*                                                                               
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BZ    *+10                                                             
         MVC   TMPDTEJ,WEEKFILT    START WEEK                                   
*                                                                               
PR130    DS    0H                                                               
         CLC   RIAVWEEK,TMPDTEJ    BEFORE HEADER START?                         
         BNL   PR140               NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(8,RIAVWEEK),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,DUB)                                
         CLC   DUB(3),TMPDTEJ      FALL BETWEEN BROADCAST WEEKS?                
         BH    PR150               NO - GET NEXT WEEK                           
*&&DO                                                                           
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BNZ   *+8                                                              
         LA    R2,WKRATEDL(R2)     NO                                           
*&&                                                                             
         B     PR110                                                            
*                                                                               
PR140    DS    0H                                                               
         OC    HDRENDJ,HDRENDJ     ANY END DATE?                                
         BZ    PR150               NO                                           
         CLC   RIAVWEEK,HDRENDJ    AFTER HEADER END?                            
         BNH   PR150                                                            
         B     PR160                                                            
*                                                                               
PR150    DS    0H                                                               
         OC    RIAVAMT,RIAVAMT     ANY AMOUNT FOR THIS WEEK                     
         BZ    *+8                                                              
         OI    MYFLAG,QTHASCST     YES                                          
*                                                                               
         MVC   WKRATE,RIAVAMT      WEEKLY RATE AMOUNT INTO TABLE                
         MVC   WKFTNT,RIAVFTNT     FOOTNOTE                                     
         LA    R2,WKRATEDL(R2)                                                  
         B     PR110                                                            
*                                                                               
PR160    DS    0H                                                               
         MVI   0(R2),X'FF'         DENOTE END OF TABLE                          
*                                                                               
*              PRINT HERE                                                       
*                                                                               
         BAS   RE,PRNTWEEK         PRINT QUARTER'S WEEKS                        
*                                                                               
         TM    MYFLAG,NOWEEKS      ANY WEEKS PRINTED?                           
         BO    *+12                NO                                           
         BAS   RE,PRNTRATE         PRINT WEEKLY RATES                           
         BAS   RE,PRNTFTNT         PRINT SPECIAL FOOTNOTES                      
*                                                                               
PR170    XC    P,P                                                              
         TM    MYFLAG,NOWEEKS                                                   
         BO    *+8                                                              
         BAS   RE,PRINT                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     RF,ANXTQTR          A(NEXT QUARTER TO FILTER)                    
         CLI   0(RF),X'FF'         ANY MORE QUARTERS TO FILTER BY?              
         BE    PRSEQ                                                            
*                                                                               
         MVC   CURQTFLT,1(RF)      FILTER BY THIS QUARTER                       
         LA    RF,6(RF)                                                         
         ST    RF,ANXTQTR                                                       
         B     PR103                                                            
*                                                                               
PRNTRPTX DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*       PRINT FOOTNOTES                                                         
***********************************************************************         
PRNTFTNT NTR1                                                                   
         L     R2,AIO3                                                          
         USING WKRATED,R2                                                       
         LA    R6,P                                                             
         USING FTNTD,R6                                                         
         XC    P,P                                                              
*                                                                               
PRNTF10  DS    0H                                                               
         CLI   0(R2),X'FF'         NO MORE BROADCAST WEEKS?                     
         BE    PRNTFX                                                           
         OC    WKFTNT,WKFTNT       ANY FOOTNOTE?                                
         BZ    PRNTF20                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(8,0(R2)),(5,FTNTEFF)                                
         MVC   FTNTPGM,WKFTNT      FOOTNOTE                                     
         BAS   RE,PRINT                                                         
*                                                                               
PRNTF20  LA    R2,WKRATEDL(R2)     NEXT ENTRY IN TABLE                          
         B     PRNTF10                                                          
*                                                                               
PRNTFX   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*       PRINT WEEKLY RATES                                                      
***********************************************************************         
PRNTRATE NTR1                                                                   
         L     R2,AIO3                                                          
         USING WKRATED,R2                                                       
         LA    R6,P                                                             
         USING WEEKD,R6                                                         
         XC    P,P                                                              
*                                                                               
PRNTR10  DS    0H                                                               
         CLI   0(R2),X'FF'         NO MORE BROADCAST WEEKS?                     
         BE    PRNTRX                                                           
*                                                                               
         TM    MYFLAG,QTHASCST     DID THIS QUARTER HAVE ANY COSTS?             
         BO    *+14                YES                                          
         MVC   P(7),=C'NO COST'                                                 
         B     PRNTRX                                                           
*                                                                               
         OC    WKRATE,WKRATE       ANY RATE FOR THIS WEEK?                      
         BNZ   *+14                                                             
         MVC   WEEK1(3),=C'N/A'                                                 
         B     PRNTR20                                                          
*                                                                               
         MVC   DEFCOST,WKRATE      WEEKLY RATE                                  
         L     RF,DEFCOST                                                       
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
*                                                                               
         EDIT  (RF),(7,WEEK1),ALIGN=LEFT                                        
*                                                                               
PRNTR20  LA    R2,WKRATEDL(R2)     NEXT ENTRY IN TABLE                          
         LA    R6,WEEKDLQ(R6)      PRINT NEXT WEEK                              
         B     PRNTR10                                                          
*                                                                               
PRNTRX   DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*       PRINT WEEKS                                                             
***********************************************************************         
PRNTWEEK NTR1                                                                   
         L     R2,AIO3                                                          
         USING WKRATED,R2                                                       
         LA    R6,P                                                             
         USING WEEKD,R6                                                         
         XC    P,P                                                              
         NI    MYFLAG,X'FF'-NOWEEKS                                             
*                                                                               
PRNTW10  DS    0H                                                               
         CLI   0(R2),X'FF'         NO MORE BROADCAST WEEKS?                     
         BE    PRNTWX                                                           
         GOTO1 DATCON,DMCB,(8,0(R2)),(5,WEEK1)                                  
         LA    R2,WKRATEDL(R2)     NEXT ENTRY IN TABLE                          
         LA    R6,WEEKDLQ(R6)      PRINT NEXT WEEK                              
         B     PRNTW10                                                          
*                                                                               
PRNTWX   DS    0H                                                               
         LA    R6,P                                                             
         OC    WEEK1,WEEK1         ANY WEEKS PRINTED?                           
         BNZ   *+12                                                             
         OI    MYFLAG,NOWEEKS                                                   
         B     *+8                                                              
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*       BUILD TABLE OF BROADCAST WEEKS FOR QTR                                  
***********************************************************************         
BLDWKTBL NTR1                                                                   
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         L     R2,AIO3             TABLE FOR BROADCAST WEEKS                    
         USING WKRATED,R2                                                       
*                                                                               
         LA    R4,QTRRFILT                                                      
*                                                                               
BLDWK10  DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BE    BLDWK100                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R4)),(19,BRDQTRST)                              
         GOTO1 DATCON,DMCB,(2,4(R4)),(19,BRDQTREN)                              
*                                                                               
         MVC   TMPBRDCU,BRDQTRST   INITIALIZE TO START OF QTR                   
*                                                                               
         CLC   BRDQTRST,HDRSTRTJ   QTR < HDR?                                   
         BNL   BLDWK60             NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(8,HDRSTRTJ),(0,TMPDTEE)                             
         GOTO1 GETDAY,DMCB,(0,TMPDTEE),DUB                                      
         ZIC   R3,0(R1)            GET NUMBER OF DAY                            
         BCTR  R3,0                                                             
*                                                                               
         LNR   R3,R3               NEGATE VALUE                                 
*                                                                               
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R3)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,TMPBRDCU)                           
         B     BLDWK60                                                          
*                                                                               
BLDWK50  DS    0H                                                               
         GOTO1 DATCON,DMCB,(8,TMPBRDCU),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,TMPBRDCU)                           
*                                                                               
BLDWK60  CLC   TMPBRDCU,BRDQTREN   NEXT QTR?                                    
         BH    BLDWK80                                                          
*                                                                               
         OC    HDRENDJ,HDRENDJ     ANY INV END DATE?                            
         BZ    *+14                                                             
         CLC   TMPBRDCU,HDRENDJ    AFTER INV HAS ENDED?                         
         BH    BLDWK80                                                          
*                                                                               
         MVC   WKDATE,TMPBRDCU     WEEK DATE (JULIAN) INTO TABLE                
         MVC   TMPDTEJ,HDRSTRTJ    START WEEK                                   
*                                                                               
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BZ    *+10                                                             
         MVC   TMPDTEJ,WEEKFILT    START WEEK                                   
*                                                                               
         CLC   TMPBRDCU,TMPDTEJ    BEFORE HEADER START?                         
         BNL   BLDWK65             YES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(8,TMPBRDCU),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,DUB)                                
         CLC   DUB(3),TMPDTEJ      FALL BETWEEN BROADCAST WEEKS?                
         BH    BLDWK70             NO - GET NEXT WEEK                           
*&&DO                                                                           
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BNZ   *+8                                                              
         LA    R2,WKRATEDL(R2)     NO                                           
*&&                                                                             
         B     BLDWK50                                                          
*                                                                               
BLDWK65  DS    0H                                                               
         OC    HDRENDJ,HDRENDJ     ANY END DATE?                                
         BZ    BLDWK70             NO                                           
         CLC   TMPBRDCU,HDRENDJ    AFTER HEADER END?                            
         BNH   BLDWK70                                                          
*&&DO                                                                           
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BNZ   *+8                                                              
         LA    R2,WKRATEDL(R2)     NO                                           
*&&                                                                             
         B     BLDWK50                                                          
*                                                                               
BLDWK70  DS    0H                                                               
         LA    R2,WKRATEDL(R2)                                                  
         B     BLDWK50                                                          
*                                                                               
BLDWK80  DS    0H                                                               
         MVI   0(R2),X'FF'         PRINT OUT THIS QUARTERS INFO                 
         BAS   RE,PRNTWEEK                                                      
*                                                                               
         TM    MYFLAG,NOWEEKS      ANY WEEKS PRINTED?                           
         BO    *+18                NO                                           
         MVC   P(7),=C'NO COST'                                                 
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT                                                         
*                                                                               
         L     R2,AIO3             TABLE FOR BROADCAST WEEKS                    
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    R4,6(R4)            NEXT QUARTER FILTER                          
         B     BLDWK10                                                          
*                                                                               
BLDWK100 DS    0H                                                               
         MVI   0(R2),X'FF'         DENOTE END OF TABLE                          
         ST    R4,ANXTQTR                                                       
*                                                                               
BLDWKX   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
****************************************************************                
*              PRINT HEADER INFO                               *                
****************************************************************                
PRNTHDR  NTR1                                                                   
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         XC    DEFCOST,DEFCOST                                                  
         XC    HDRENDJ,HDRENDJ                                                  
*                                                                               
         LA    R2,P                                                             
         USING HEADD,R2                                                         
         XC    P,P                                                              
*                                                                               
         MVC   HEADINV(4),RINVKINV    INV #                                     
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(5,HEADEFF)                             
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(19,HDRSTRTJ)                           
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BE    PRHDR10                                                          
*                                                                               
         MVI   HEADEFF+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(5,HEADEFF+9)                         
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(19,HDRENDJ)                          
         DROP  R6                                                               
*                                                                               
PRHDR10  DS    0H                  DAY/TIME                                     
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         USING RIDTELEM,R6                                                      
*                                                                               
         GOTO1 UNDAY,DMCB,RIDTDAY,HEADDYTM          DAY                         
         LA    RE,17                                                            
         LA    R5,HEADDYTM                                                      
*                                                                               
PRHDR20  DS    0H                                                               
         CLI   0(R5),X'40'                                                      
         BNH   *+12                                                             
         LA    R5,1(R5)                                                         
         BCT   RE,PRHDR20                                                       
*                                                                               
         MVI   0(R5),C'/'                                                       
         GOTO1 UNTIME,DMCB,RIDTTIME,(0,1(R5))       TIME                        
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         USING RIPGELEM,R6                                                      
*                                                                               
         XC    HEADPGM,HEADPGM                                                  
         ZIC   R1,RIPGLEN                                                       
         SH    R1,=H'2'                                                         
         CH    R1,=H'27'           MAX OUTPUT SIZE                              
         BNL   PRHDR30                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   HEADPGM(0),RIPGNAME                                              
         B     *+10                                                             
*                                                                               
PRHDR30  MVC   HEADPGM,RIPGNAME                                                 
         DROP  R6                                                               
*                                                                               
         OC    DYPTRFLT,DYPTRFLT   ANY DAYPART FILTERS?                         
         BZ    *+14                                                             
*                                                                               
         BAS   RE,PRNDYPT          GET MATCHING DAYPARTS                        
         MVC   HEADDPT,WORK        PRINT DYPTS                                  
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
PRHDRX   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
****************************************************************                
*              GET MATCHING DAYPARTS                           *                
****************************************************************                
PRNDYPT  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         USING RINVPEL,R6                                                       
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         LA    R1,MAXDPT#                                                       
         LA    R2,DYPTRFLT         DAYPART FILTERS                              
         LA    R3,MAXDPT#                                                       
         LA    R6,RINVDP           DAYPARTS IN RECORD                           
         LA    R4,WORK             MATCH ON FILTERS AND IN RECORD               
*                                                                               
PRDPT10  DS    0H                                                               
         CLC   0(1,R2),0(R6)       FOUND MATCH?                                 
         BNE   PRDPT20             NO - BUMP TO NEXT FILTER                     
         MVC   0(1,R4),0(R2)       SAVE AWAY MATCHING DYPT                      
*                                                                               
         LA    R4,1(R4)                                                         
         LA    R1,MAXDPT#                                                       
         B     PRDPT30                                                          
*                                                                               
PRDPT20  DS    0H                                                               
         LA    R2,1(R2)            BUMP TO NEXT DYPT FILTER                     
         BCT   R1,PRDPT10                                                       
*                                                                               
PRDPT30  DS    0H                                                               
         LA    R2,DYPTRFLT                                                      
         LA    R6,1(R6)            BUMP TO NEXT DYPT IN RECORD                  
         BCT   R3,PRDPT10                                                       
*                                                                               
PRNDPTX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* REPORT HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,38,C'RATE CARD REPORT'                                        
         PSPEC H2,38,C'----------------'                                        
         PSPEC H2,1,AGYNAME                                                     
         PSPEC H2,76,C'RATE CARD:'                                              
         PSPEC H3,1,REQUESTOR                                                   
         PSPEC H3,76,C'QUARTER:'                                                
         PSPEC H4,1,C'STATION:'                                                 
         PSPEC H4,60,C'LENGTH:'                                                 
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1                                                                   
*                                                                               
* MOVE HEADINGS INTO H FIELDS                                                   
*                                                                               
         MVC   H2+87(8),CODHLD                                                  
         MVC   H4+67(L'RNRLEN),RNRLEN                                           
         MVC   H4+10(6),RNMSSTA                                                 
         MVC   H3+84(L'RNRQTR),RNRQTR                                           
*                                                                               
         OC    DYPTRFLT,DYPTRFLT   ANY DAYPART FILTER?                          
         BZ    HK10                                                             
         MVC   H4+24(8),=C'DAYPART:'                                            
         MVC   H4+33(6),DYPTRFLT                                                
*                                                                               
HK10     DS    0H                                                               
         OC    WEEKFILT,WEEKFILT   ANY WEEKLY FILTER?                           
         BZ    HK20                                                             
         MVC   H4+42(5),=C'WEEK:'                                               
         MVC   H4+48(8),RNRWEK                                                  
*                                                                               
HK20     DS    0H                                                               
         OC    INVRFILT,INVRFILT   ANY INV # FILTER?                            
         BZ    HK50                                                             
         MVC   H4+75(5),=C'INV#:'                                               
         MVC   H4+81(L'RNRINV),RNRINV                                           
*                                                                               
HK50     LA    R3,H6                                                            
         USING HEADD,R3                                                         
*                                                                               
         MVC   HEADINV,=CL8'INV #'                                              
         MVC   HEADPGM,=CL27'PROGRAM'                                           
         MVC   HEADDYTM,=CL18'DAY/TIME'                                         
         MVC   HEADEFF,=CL17'EFF DATE'                                          
         MVC   HEADDPT,=CL8'DAYPARTS'                                           
*                                                                               
         LA    R3,H7                                                            
         MVC   HEADINV,=CL8'--------'                                           
         MVC   HEADPGM,=CL27'---------------------------'                       
         MVC   HEADDYTM,=CL18'------------------'                               
         MVC   HEADEFF,=CL17'-----------------'                                 
         MVC   HEADDPT,=CL8'--------'                                           
*                                                                               
HOOKX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
PRREPF   DS    CL8'REPFILE'                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*****************************************************************               
*        PRINT DOWNLOADABLE REPORT                                              
*****************************************************************               
DNLRPT   NTR1  BASE=*,LABEL=*                                                   
         NI    DMINBTS,X'7F'       TURN OFF READ FOR UPDATE                     
*                                                                               
         BAS   RE,DNLHEAD          DOWNLOAD COLUMN HEADINGS                     
*                                                                               
         GOTO1 =A(CHKOPTS),RR=RELO   CHECK OPTIONS                              
         GOTO1 =A(CHKRTCD),RR=RELO   VALIDATE QTRS REQUESTED                    
*                                                                               
         XC    INVRFILT,INVRFILT                                                
         MVI   ERROR,INVALID                                                    
*                                                                               
         LA    R2,RNRINVH                                                       
         CLI   5(R2),0             ANY INV FILTER?                              
         BE    DNL20                                                            
*                                                                               
         CLI   5(R2),4             ONLY 1 INV #?                                
         BNE   *+14                                                             
         MVC   INVRFILT(4),8(R2)                                                
         B     DNL20                                                            
*                                                                               
         CLI   5(R2),9             RANGE OF INV#'S?                             
         JNE   ERREND                                                           
         CLI   RNRINV+4,C'-'                                                    
         JNE   ERREND                                                           
*                                                                               
         MVC   INVRFILT(4),8(R2)                                                
         MVC   INVRFILT+4(4),13(R2)                                             
*                                                                               
DNL20    DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP                                                 
         MVC   RINVKSTA,STAHLD                                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     DNL30                                                            
*                                                                               
DNLSEQ   DS    0H                                                               
         LA    R6,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
DNL30    DS    0H                                                               
         CLC   KEY(17),KEYSAVE                                                  
         BNE   DNLRPTX                                                          
*                                                                               
         CLI   RINVKSRC,0          MUST BE A HEADER                             
         BNE   DNLSEQ                                                           
         CLI   RINVKINV+3,0        ONLY NEW INVENTORIES ALLOWED                 
         BE    DNLSEQ                                                           
*                                                                               
*  GET HDR TO SEE IF IT FALLS WITHIN THIS BRD YEAR                              
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         MVC   HDRKEY,KEY          SAVE AWAY HEADER KEY                         
*                                                                               
         L     R6,AIO                                                           
*                                                                               
*   IS HEADER IN DATE RANGE FOR THIS QUARTER?                                   
*&&DO                                                                           
         CLC   RINVPEFF(2),ENBRDYRC        HDR START > BRD YR END?              
         BH    DNLSEQ                                                           
         OC    RINVPEFF+2(2),RINVPEFF+2    ANY END DATE ON HDR?                 
         BZ    DNL40                       NO - IT'S IN THE DATE RANGE          
         CLC   RINVPEFF+2(2),STENDQTR      HDR END < BRD YR START?              
         BL    DNLSEQ                                                           
*&&                                                                             
*                                                                               
         LA    RF,QTRRFILT                                                      
*                                                                               
DNL35    DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    DNLSEQ                                                           
*                                                                               
         CLC   RINVPEFF(2),4(RF)           HDR START > QTR END?                 
         BH    DNL37                                                            
         OC    RINVPEFF+2(2),RINVPEFF+2    ANY END DATE ON HDR?                 
         BZ    DNL40                       NO - IT'S IN THE DATE RANGE          
         CLC   RINVPEFF+2(2),2(RF)         HDR END < BRD YR START?              
         BNL   DNL40                                                            
*                                                                               
DNL37    DS    0H                                                               
         LA    RF,6(RF)                                                         
         B     DNL35                                                            
*                                                                               
DNL40    DS    0H                  CHECK FILTERS                                
         OC    DYPTRFLT,DYPTRFLT   ANY DAYPART FILTERS?                         
         BZ    DNL70                                                            
         CLC   =C'ALL',DYPTRFLT    ALL DAYPART FILTERS?                         
         BE    DNL70                                                            
*                                                                               
         LA    RF,RINVDP           DYPTS IN HDR                                 
         LA    R3,DYPTRFLT         DYPT FILTERS                                 
         LA    R5,MAXDPT#          MAX # OF DYPTS                               
*                                                                               
DNL50    DS    0H                                                               
         CLI   0(RF),0             FOUND DYPT MATCH?                            
         BE    DNLSEQ              NO                                           
*                                                                               
         LA    R3,DYPTRFLT         DYPT FILTERS                                 
         LA    R4,MAXDPT#                                                       
*                                                                               
DNL60    DS    0H                                                               
         CLC   0(1,R3),0(RF)       MATCHING DYPT?                               
         BE    DNL70               YES                                          
*                                                                               
         LA    R3,1(R3)            CHECK NEXT FILTER                            
         BCT   R4,DNL60                                                         
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   R5,DNL50                                                         
         B     DNLSEQ              NO MATCHES FOUND                             
*                                                                               
DNL70    DS    0H                                                               
         OC    INVRFILT,INVRFILT     ANY INV# FILTER?                           
         BZ    DNL90                                                            
*                                                                               
         CLI   INVRFILT+4,0         FIND RANGE OF INV#'S?                       
         BNE   DNL80                YES                                         
*                                                                               
         CLC   RINVKINV,INVRFILT    SAME INV#                                   
         BNE   DNLSEQ                                                           
         B     DNL90                                                            
*                                                                               
DNL80    DS    0H                                                               
         CLC   RINVKINV,INVRFILT    INV HEADER < INV FILT START?                
         BL    DNLSEQ                                                           
         CLC   RINVKINV,INVRFILT+4  INV HEADER > INV FILT END?                  
         BH    DNLSEQ                                                           
*                                                                               
DNL90    DS    0H                                                               
         BAS   RE,HDRINFO          GET HEADER INFO READY FOR DOWNLOAD           
*                                                                               
         GOTO1 =A(GETEQU#),RR=RELO                                              
         TM    MYFLAG,GOTEQU#      'Z' RECORD EXISTS?                           
         BO    DNL100              NO - NO RATES TO DISPLAY                     
*                                                                               
         BAS   RE,DNLWKTBL         BUILD TABLE OF VALID BROAD. WEEKS            
         BAS   RE,DNLWEEK          PRINT WEEKLY DATES                           
*                                                                               
         ZIC   R5,WKCOUNT          # OF 0 RATES TO DOWNLOAD                     
         GOTO1 DNLFFLD,DMCB,(R5),0    FILL ALL 52 FIELDS W/ 0                   
*                                                                               
         B     DNL170                                                           
*                                                                               
DNL100   DS    0H                                                               
         LA    RF,QTRRFILT         REQUESTED QTRS                               
         MVC   CURQTFLT,1(RF)      CUR QTR TO FILTER ON                         
*                                                                               
         LA    RF,6(RF)                                                         
         ST    RF,ANXTQTR          A(NEXT QTR TO FILTER)                        
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   KEY(24),HDRKEY      COPY KEY UP TO SOURCE                        
*                                                                               
         MVI   RINVKSRC,C'Z'       GAV RATE RECORD                              
         MVC   RINVKNUM,EQUNUM     UNIQUE GAV NUMBER EQUATE                     
         MVC   RINVKYR,YEARHLD     AVAIL YEAR                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DNL170              RATE RECORD DOESN'T EXIST YET                
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         L     R2,AIO3             BUILD TABLE FOR ALL ELEMENTS                 
         USING WKRATED,R2                                                       
         NI    MYFLAG,X'FF'-QTHASCST                                            
*                                                                               
         MVI   WKCOUNT,0                                                        
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
DNL105   DS    0H                                                               
         L     R6,AIO2                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'03'        GET WEEKLY RATE INFO                         
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING RIAVL,R6                                                         
*                                                                               
DNL110   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   DNL160                                                           
*                                                                               
         CLC   RIAVQTR,CURQTFLT    THIS QUARTER?                                
         BNE   DNL110                                                           
*                                                                               
         MVC   WKDATE,RIAVWEEK     WEEK DATE (JULIAN) INTO TABLE                
*                                                                               
         MVC   TMPDTEJ,HDRSTRTJ    START WEEK                                   
*                                                                               
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BZ    *+10                                                             
         MVC   TMPDTEJ,WEEKFILT    START WEEK                                   
*                                                                               
DNL130   DS    0H                                                               
         CLC   RIAVWEEK,TMPDTEJ    BEFORE HEADER START?                         
         BNL   DNL140              NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(8,RIAVWEEK),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,DUB)                                
         CLC   DUB(3),TMPDTEJ      FALL BETWEEN BROADCAST WEEKS?                
         BH    DNL150              NO - GET NEXT WEEK                           
*&&DO                                                                           
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BNZ   *+8                                                              
         LA    R2,WKRATEDL(R2)     NO                                           
*&&                                                                             
         B     DNL110                                                           
*                                                                               
DNL140   DS    0H                                                               
         OC    HDRENDJ,HDRENDJ     ANY END DATE?                                
         BZ    DNL150              NO                                           
         CLC   RIAVWEEK,HDRENDJ    AFTER HEADER END?                            
         BNH   DNL150                                                           
         B     DNL160                                                           
*                                                                               
DNL150   DS    0H                                                               
         OC    RIAVAMT,RIAVAMT     ANY AMOUNT FOR THIS WEEK                     
         BZ    *+8                                                              
         OI    MYFLAG,QTHASCST     YES                                          
*                                                                               
         MVC   WKRATE,RIAVAMT      WEEKLY RATE AMOUNT INTO TABLE                
         MVC   WKFTNT,RIAVFTNT     FOOTNOTE                                     
         LA    R2,WKRATEDL(R2)                                                  
*                                                                               
         ZIC   RF,WKCOUNT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,WKCOUNT                                                       
*                                                                               
         B     DNL110                                                           
*                                                                               
DNL160   DS    0H                                                               
         L     RF,ANXTQTR          A(NEXT QUARTER TO FILTER)                    
         CLI   0(RF),X'FF'         ANY MORE QUARTERS TO FILTER BY?              
         BE    DNL165                                                           
*                                                                               
         MVC   CURQTFLT,1(RF)      FILTER BY THIS QUARTER                       
         LA    RF,2(RF)                                                         
         ST    RF,ANXTQTR                                                       
         B     DNL105                                                           
*                                                                               
DNL165   DS    0H                                                               
         MVI   0(R2),X'FF'         DENOTE END OF TABLE                          
*                                                                               
*              PRINT HERE                                                       
*                                                                               
         BAS   RE,DNLWEEK          PRINT QUARTER'S WEEKS                        
         BAS   RE,DNLRATE          PRINT WEEKLY RATES                           
*                                                                               
DNL170   XC    P,P                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         B     DNLSEQ                                                           
*                                                                               
DNLRPTX  DS    0H                                                               
DNLXIT   XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*       DOWNLOAD WEEKLY RATES                                                   
***********************************************************************         
DNLRATE  NTR1                                                                   
         L     R2,AIO3                                                          
         USING WKRATED,R2                                                       
*                                                                               
         LA    RE,DWNTMP                                                        
         LA    RF,500                                                           
         XCEF                                                                   
*                                                                               
         LA    R6,DWNTMP                                                        
         USING WEEKD,R6                                                         
*                                                                               
         SR    R5,R5                                                            
*                                                                               
DNLR10   DS    0H                                                               
         CLI   0(R2),X'FF'         NO MORE BROADCAST WEEKS?                     
         BE    DNLR60                                                           
*                                                                               
         TM    MYFLAG,QTHASCST     DID THIS QUARTER HAVE ANY COSTS?             
         BO    DNLR20              YES                                          
         GOTO1 DNLFFLD,DMCB,52,0   FILL ALL 52 FIELDS W/ 0                      
         LA    R5,52                                                            
         B     DNLR60                                                           
*                                                                               
DNLR20   DS    0H                                                               
         OC    WKRATE,WKRATE       ANY RATE FOR THIS WEEK?                      
         BNZ   DNLR30                                                           
*                                                                               
         MVI   DWNFLD,C'0'                                                      
         MVI   PRTSIZE,8                                                        
         GOTO1 ADWNL,DMCB,(RC),DWNNUM                                           
*                                                                               
         B     DNLR50                                                           
*                                                                               
DNLR30   DS    0H                                                               
         MVC   DEFCOST,WKRATE      WEEKLY RATE                                  
         L     RF,DEFCOST                                                       
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
*                                                                               
         EDIT  (RF),(7,WEEK1),ALIGN=LEFT                                        
*                                                                               
         MVC   DWNFLD,WEEK1                                                     
         MVI   PRTSIZE,8                                                        
         GOTO1 ADWNL,DMCB,(RC),DWNNUM                                           
*                                                                               
DNLR50   DS    0H                                                               
         LA    R5,1(R5)            INCREMENT COUNTER                            
         LA    R2,WKRATEDL(R2)     NEXT ENTRY IN TABLE                          
         LA    R6,WEEKDLQ(R6)      PRINT NEXT WEEK                              
         B     DNLR10                                                           
*                                                                               
DNLR60   DS    0H                                                               
*&&DO                                                                           
         LA    R6,52                                                            
         SR    R6,R5                                                            
         BNP   DNLRX                                                            
*                                                                               
         GOTO1 DNLFFLD,DMCB,(R6),0   FILL IN REMAINING W/ ZEROES                
*&&                                                                             
DNLRX    DS    0H                                                               
         MVC   DWNFLD,SPACES       END OF LINE                                  
         GOTO1 ADWNL,DMCB,(RC),DWNEOL                                           
*                                                                               
         B     DNLXIT                                                           
         LTORG                                                                  
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*       NO COST FOR ALL WEEKS                                                   
***********************************************************************         
DNLFFLD  NTR1                                                                   
         L     R4,0(R1)            # OF FIELDS TO FILL                          
*                                                                               
         XC    DWNFLD,DWNFLD                                                    
*                                                                               
         CLI   4(R1),0             FILL WITH ZEROS?                             
         BNE   DNLFF20             NO                                           
*                                                                               
DNLFF10  DS    0H                  FILL WITH ZEROS                              
         MVI   DWNFLD,C'0'                                                      
         MVI   PRTSIZE,8                                                        
         GOTO1 ADWNL,DMCB,(RC),DWNNUM                                           
         BCT   R4,DNLFF10                                                       
         B     DNLFFX                                                           
*                                                                               
DNLFF20  DS    0H                  FILL WITH CHARACTERS                         
         MVI   DWNFLD,C' '                                                      
         MVI   PRTSIZE,1                                                        
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
         BCT   R4,DNLFF20                                                       
*                                                                               
DNLFFX   DS    0H                                                               
         MVC   DWNFLD,SPACES       END OF LINE                                  
         GOTO1 ADWNL,DMCB,(RC),DWNEOL                                           
         B     DNLXIT                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*       DOWNLOAD WEEKS                                                          
***********************************************************************         
DNLWEEK  NTR1                                                                   
         BAS   RE,DNLHDR           DOWNLOAD HEADER INFO FIRST                   
*                                                                               
         L     R2,AIO3                                                          
         USING WKRATED,R2                                                       
*                                                                               
         LA    RE,DWNTMP                                                        
         LA    RF,500                                                           
         XCEF                                                                   
*                                                                               
         LA    R6,DWNTMP                                                        
         USING WEEKD,R6                                                         
*                                                                               
         SR    R5,R5                                                            
*                                                                               
DNLW10   DS    0H                                                               
         CLI   0(R2),X'FF'         NO MORE BROADCAST WEEKS?                     
         BE    DNLW50                                                           
         GOTO1 DATCON,DMCB,(8,0(R2)),(5,WEEK1)                                  
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'WEEK1),WEEK1      BROADCAST WEEK                        
         LA    R1,L'WEEK1                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         LA    R5,1(R5)            UPDATE COUNTER                               
         LA    R2,WKRATEDL(R2)     NEXT ENTRY IN TABLE                          
         LA    R6,WEEKDLQ(R6)      PRINT NEXT WEEK                              
         B     DNLW10                                                           
*                                                                               
DNLW50   DS    0H                                                               
*&&DO                                                                           
         LA    R6,52                                                            
         SR    R6,R5                                                            
         BNP   DNLWX                                                            
*                                                                               
         GOTO1 DNLFFLD,DMCB,(R6),C' '   FILL IN REMAINING W/ C' '               
*&&                                                                             
DNLWX    DS    0H                                                               
         MVC   DWNFLD,SPACES       END OF LINE                                  
         GOTO1 ADWNL,DMCB,(RC),DWNEOL                                           
*                                                                               
         B     DNLXIT                                                           
         LTORG                                                                  
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*       BUILD TABLE OF BROADCAST WEEKS FOR QTR                                  
***********************************************************************         
DNLWKTBL NTR1                                                                   
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         L     R2,AIO3             TABLE FOR BROADCAST WEEKS                    
         USING WKRATED,R2                                                       
         MVI   WKCOUNT,0                                                        
*                                                                               
         LA    R4,QTRRFILT                                                      
*                                                                               
DNLWK10  DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BE    DNLWK100                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R4)),(19,BRDQTRST)                              
         GOTO1 DATCON,DMCB,(2,4(R4)),(19,BRDQTREN)                              
*                                                                               
         MVC   TMPBRDCU,BRDQTRST   INITIALIZE TO START OF QTR                   
*                                                                               
         CLC   BRDQTRST,HDRSTRTJ   QTR < HDR?                                   
         BNL   DNLWK60             NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(8,HDRSTRTJ),(0,TMPDTEE)                             
         GOTO1 GETDAY,DMCB,(0,TMPDTEE),DUB                                      
         ZIC   R3,0(R1)            GET NUMBER OF DAY                            
         BCTR  R3,0                                                             
*                                                                               
         LNR   R3,R3               NEGATE VALUE                                 
*                                                                               
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R3)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,TMPBRDCU)                           
         B     DNLWK60                                                          
*                                                                               
DNLWK50  DS    0H                                                               
         GOTO1 DATCON,DMCB,(8,TMPBRDCU),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,TMPBRDCU)                           
*                                                                               
DNLWK60  CLC   TMPBRDCU,BRDQTREN   NEXT QTR?                                    
         BH    DNLWK80                                                          
*                                                                               
         OC    HDRENDJ,HDRENDJ     ANY INV END DATE?                            
         BZ    *+14                                                             
         CLC   TMPBRDCU,HDRENDJ    AFTER INV HAS ENDED?                         
         BH    DNLWK80                                                          
*                                                                               
         MVC   WKDATE,TMPBRDCU     WEEK DATE (JULIAN) INTO TABLE                
         MVC   TMPDTEJ,HDRSTRTJ    START WEEK                                   
*                                                                               
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BZ    *+10                                                             
         MVC   TMPDTEJ,WEEKFILT    START WEEK                                   
*                                                                               
         CLC   TMPBRDCU,TMPDTEJ    BEFORE HEADER START?                         
         BNL   DNLWK65             YES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(8,TMPBRDCU),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,DUB)                                
         CLC   DUB(3),TMPDTEJ      FALL BETWEEN BROADCAST WEEKS?                
         BH    DNLWK70             NO - GET NEXT WEEK                           
*&&DO                                                                           
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BNZ   *+8                                                              
         LA    R2,WKRATEDL(R2)     NO                                           
*&&                                                                             
         B     DNLWK50                                                          
*                                                                               
DNLWK65  DS    0H                                                               
         OC    HDRENDJ,HDRENDJ     ANY END DATE?                                
         BZ    DNLWK70             NO                                           
         CLC   TMPBRDCU,HDRENDJ    AFTER HEADER END?                            
         BNH   DNLWK70                                                          
*&&DO                                                                           
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BNZ   *+8                                                              
         LA    R2,WKRATEDL(R2)     NO                                           
*&&                                                                             
         B     DNLWK50                                                          
*                                                                               
DNLWK70  DS    0H                                                               
         LA    R2,WKRATEDL(R2)                                                  
*                                                                               
         ZIC   RF,WKCOUNT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,WKCOUNT                                                       
*                                                                               
         B     DNLWK50                                                          
*                                                                               
DNLWK80  DS    0H                                                               
         LA    R4,6(R4)            NEXT QUARTER FILTER                          
         B     DNLWK10                                                          
*                                                                               
DNLWK100 DS    0H                                                               
         MVI   0(R2),X'FF'         DENOTE END OF TABLE                          
*                                                                               
DNLWKX   DS    0H                                                               
         B     DNLXIT                                                           
         LTORG                                                                  
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
****************************************************************                
*              GET HEADER INFO READY FOR DOWNLOAD              *                
****************************************************************                
HDRINFO  NTR1                                                                   
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         XC    DEFCOST,DEFCOST                                                  
         XC    HDRENDJ,HDRENDJ                                                  
*                                                                               
         XC    SVINV,SVINV                                                      
         XC    SVEFF,SVEFF                                                      
         XC    SVPGM,SVPGM                                                      
         XC    SVDPT,SVDPT                                                      
         XC    SVDYTM,SVDYTM                                                    
*                                                                               
         MVC   SVINV(4),RINVKINV    INV #                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(5,SVEFF)                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(19,HDRSTRTJ)                           
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BE    HDRINF10                                                         
*                                                                               
         MVI   SVEFF+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(5,SVEFF+9)                           
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(19,HDRENDJ)                          
         DROP  R6                                                               
*                                                                               
HDRINF10 DS    0H                  DAY/TIME                                     
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         USING RIDTELEM,R6                                                      
*                                                                               
         GOTO1 UNDAY,DMCB,RIDTDAY,SVDYTM          DAY                           
         LA    RE,17                                                            
         LA    R5,SVDYTM                                                        
*                                                                               
HDRINF20 DS    0H                                                               
         CLI   0(R5),X'40'                                                      
         BNH   *+12                                                             
         LA    R5,1(R5)                                                         
         BCT   RE,HDRINF20                                                      
*                                                                               
         MVI   0(R5),C'/'                                                       
         GOTO1 UNTIME,DMCB,RIDTTIME,(0,1(R5))       TIME                        
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         USING RIPGELEM,R6                                                      
*                                                                               
         XC    SVPGM,SVPGM                                                      
         ZIC   R1,RIPGLEN                                                       
         SH    R1,=H'2'                                                         
         CH    R1,=H'27'           MAX OUTPUT SIZE                              
         BNL   HDRINF30                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   SVPGM(0),RIPGNAME                                                
         B     *+10                                                             
*                                                                               
HDRINF30 MVC   SVPGM,RIPGNAME                                                   
         DROP  R6                                                               
*                                                                               
         OC    DYPTRFLT,DYPTRFLT   ANY DAYPART FILTERS?                         
         BZ    *+14                                                             
*                                                                               
         BAS   RE,DNLDYPT        GET MATCHING DAYPARTS                          
         MVC   SVDPT,WORK        PRINT DYPTS                                    
*                                                                               
HDRINFX  DS    0H                                                               
         B     DNLXIT                                                           
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
****************************************************************                
*              DOWNLOAD COLUMN HEADINGS                        *                
****************************************************************                
DNLHEAD  NTR1                                                                   
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(5),=C'INV #'                                              
         LA    R1,L'SVINV                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(7),=C'PROGRAM'                                            
         LA    R1,L'SVPGM                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(8),=C'DAY/TIME'                                           
         LA    R1,L'SVDYTM                                                      
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(8),=C'EFF DATE'                                           
         LA    R1,L'SVEFF                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(8),=C'DAYPARTS'                                           
         LA    R1,L'SVDPT                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES       END OF LINE                                  
         GOTO1 ADWNL,DMCB,(RC),DWNEOL                                           
*                                                                               
DNLHEADX DS    0H                                                               
         B     DNLXIT                                                           
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
****************************************************************                
*              DOWNLOAD HEADER                                 *                
****************************************************************                
DNLHDR   NTR1                                                                   
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SVINV),SVINV  INV#                                      
         LA    R1,L'SVINV                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SVPGM),SVPGM  PROGRAM NAME                              
         LA    R1,L'SVPGM                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SVDYTM),SVDYTM   DAY/TIME                               
         LA    R1,L'SVDYTM                                                      
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SVEFF),SVEFF  EFF DATE                                  
         LA    R1,L'SVEFF                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         MVC   DWNFLD,SPACES                                                    
         MVC   DWNFLD(L'SVDPT),SVDPT  DAYPARTS                                  
         LA    R1,L'SVDPT                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT                                          
*                                                                               
         ZIC   R4,WKCOUNT          # OF TOTAL WEEKLY FIELDS                     
         LA    RF,5                # OF HEADER FIELDS                           
         SR    R4,RF                                                            
         BNP   DNLHDR20                                                         
*                                                                               
         GOTO1 DNLFFLD,DMCB,(R4),C' '                                           
*                                                                               
DNLHDR20 DS    0H                                                               
         MVC   DWNFLD,SPACES       END OF LINE                                  
         GOTO1 ADWNL,DMCB,(RC),DWNEOL                                           
*                                                                               
DNLHDRX  DS    0H                                                               
         B     DNLXIT                                                           
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
****************************************************************                
*              GET MATCHING DAYPARTS                           *                
****************************************************************                
DNLDYPT  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         USING RINVPEL,R6                                                       
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         LA    R1,MAXDPT#                                                       
         LA    R2,DYPTRFLT         DAYPART FILTERS                              
         LA    R3,MAXDPT#                                                       
         LA    R6,RINVDP           DAYPARTS IN RECORD                           
         LA    R4,WORK             MATCH ON FILTERS AND IN RECORD               
*                                                                               
DNLDP10  DS    0H                                                               
         CLC   0(1,R2),0(R6)       FOUND MATCH?                                 
         BNE   DNLDP20             NO - BUMP TO NEXT FILTER                     
         MVC   0(1,R4),0(R2)       SAVE AWAY MATCHING DYPT                      
*                                                                               
         LA    R4,1(R4)                                                         
         LA    R1,MAXDPT#                                                       
         B     DNLDP30                                                          
*                                                                               
DNLDP20  DS    0H                                                               
         LA    R2,1(R2)            BUMP TO NEXT DYPT FILTER                     
         BCT   R1,DNLDP10                                                       
*                                                                               
DNLDP30  DS    0H                                                               
         LA    R2,DYPTRFLT                                                      
         LA    R6,1(R6)            BUMP TO NEXT DYPT IN RECORD                  
         BCT   R3,DNLDP10                                                       
*                                                                               
DNLDPTX  DS    0H                                                               
         B     DNLXIT                                                           
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        VALIDATE WEEKDLY BREAKOUT SCREEN                                       
*****************************************************************               
VALWWK   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   ERROR,INVALID                                                    
*                                                                               
         MVC   KEY(27),HDRKEY                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 =A(GETEQU#),RR=RELO     GET EQUATE # FOR 'Z' RECORD              
         TM    MYFLAG,GOTEQU#          'Z' REC ALREADY EXISTS?                  
         BO    VWWK20                  YES                                      
*                                                                               
         XC    DEFCOST,DEFCOST                                                  
         GOTO1 =A(ADDRDET),RR=RELO     ADD NEW RDETAIL REC W/ NO COSTS          
*                                                                               
VWWK20   DS    0H                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   KEY(24),HDRKEY                                                   
         MVI   RINVKSRC,C'Z'       RDETAIL RECORD TYPE                          
         MVC   RINVKNUM,EQUNUM     EQUATE NUMBER                                
         MVC   RINVKYR,YEARHLD     YEAR                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    VWWK25                                                           
         XC    DEFCOST,DEFCOST                                                  
         GOTO1 =A(ADDRDET),RR=RELO     ADD NEW RDETAIL REC W/ NO COSTS          
         B     VWWK20                                                           
*                                                                               
         BE    *+6                                                              
         DC    H'00'               'Z' RECORD SHOULD EXIST                      
*                                                                               
VWWK25   MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         DROP  R6                                                               
*                                                                               
         LA    R2,RNWEFF1H                                                      
         XC    DEFCOST,DEFCOST                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING RIAVL,R6                                                         
*                                                                               
VWWK30   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   VWWK100                                                          
*                                                                               
VWWK40   DS    0H                                                               
         LA    RF,RNWENDH          END OF SCREEN?                               
         CR    R2,RF                                                            
         BNL   VWWK100                                                          
*                                                                               
         CLI   8(R2),0             ANY WEEK HERE?                               
         BE    VWWK100             NO - DONE                                    
*                                                                               
         XC    DUB,DUB                                                          
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB                                        
         GOTO1 DATCON,DMCB,(0,DUB),(19,TMPDTEJ)                                 
*                                                                               
         CLC   RIAVQTR,QTRHLD      SAME QUARTER?                                
         BNE   VWWK30                                                           
*                                                                               
         CLC   TMPDTEJ,RIAVWEEK    SAME WEEK?                                   
         BNE   VWWK30                                                           
*                                                                               
         LA    RF,RNWEFF1H         1ST TIME THROUGH?                            
         CR    R2,RF                                                            
         BNE   *+16                                                             
         MVC   PRVCOST,RIAVAMT     INITIALIZE W/ 1ST WEEKS COSTS                
         MVC   DEFCOST,RIAVAMT                                                  
*                                                                               
         NI    RIAVFLG,X'FF'-RIAVEXCP                                           
*                                                                               
         BRAS  RE,NEXTFLD                                                       
         TM    4(R2),X'80'         NEW COST INPUT HERE?                         
         BO    VWWK50                                                           
*                                                                               
         CLC   PRVCOST,RIAVAMT     SAME COST AS PREVIOUS WEEK?                  
         BNE   *+14                                                             
         MVC   RIAVAMT,DEFCOST     TRICKLE DOWN COST                            
         B     VWWK70                                                           
*                                                                               
VWWK50   DS    0H                                                               
         CLI   8(R2),C'$'          CHANGE SINGLE WEEK ONLY?                     
         BNE   VWWK60                                                           
         CLI   9(R2),0             SHOULD BE COST HERE                          
         JE    ERREND                                                           
*                                                                               
         OI    RIAVFLG,RIAVEXCP    ONE WEEK EXCEPTION RATE                      
*                                                                               
         MVC   8(L'RNWCOS1-1,R2),9(R2)                                          
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                DECREMENT LENGTH                             
         STC   RF,5(R2)                                                         
         OI    1(R2),X'10'         NUMERIC                                      
         OI    4(R2),X'08'         VALID NUMERIC                                
         XC    PRVCOST,PRVCOST                                                  
*                                                                               
VWWK60   DS    0H                                                               
         GOTO1 VPACK                                                            
         LR    RF,R0               MOVE COST IN RF                              
         SR    RE,RE                                                            
         M     RE,=F'100'                                                       
         ST    RF,DEFCOST          SAVE THE COST                                
*                                                                               
         TM    RIAVFLG,RIAVEXCP    ONE WEEK EXCEPTION RATE                      
         BO    *+10                                                             
         MVC   PRVCOST,RIAVAMT     SAME AWAY PREVIOUS COST                      
*                                                                               
         MVC   RIAVAMT,DEFCOST     NEW WEEKLY COST IN RECORD                    
*                                                                               
VWWK70   DS    0H                                                               
         BRAS  RE,NEXTFLD                                                       
         MVC   RIAVFTNT,8(R2)      NEW FOOTNOTE IN RECORD                       
*                                                                               
         BRAS  RE,NEXTFLD                                                       
         B     VWWK30                                                           
         DROP  R6                                                               
*                                                                               
VWWK100  DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'EF'        GET ACTIVITY ELEMENT                         
         BRAS  RE,GETEL            IS THERE ALREADY ONE?                        
         BE    *+6                 NO                                           
         DC    H'00'                                                            
         USING RINVAEL,R6                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,RINVALST)                                   
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 =A(UPDRTCD),RR=RELO   CHECK IF VALID RATE CODE                   
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(27),HDRKEY                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
VALWWKX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        DISPLAY WEEKLY BREAKOUT SCREEN                                         
*        OUTPUT MYFLAG OI/ DISPSEL IF DISPLAYED                                 
*               THE HEADER SELECTED                                             
*****************************************************************               
DISPWKLY NTR1  BASE=*,LABEL=*                                                   
         NI    MYFLAG,X'FF'-DISPSEL                                             
         XC    KTABDISP,KTABDISP                                                
*                                                                               
         GOTO1 CALLOV,DMCB,(X'D2',RNMHEREH),0                                   
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVI   CURSCRN,WEEKSCRN                                                 
*                                                                               
         LA    R3,KEYTAB                                                        
         USING KEYTABD,R3                                                       
         LA    R4,MAXLST#                                                       
*                                                                               
DISPW10  DS    0H                                                               
         TM    KTFLAG,KTSEL        SELECTED THIS HDR?                           
         BZ    DISPW100            NO - CHECK NEXT ONE                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,KTREP      REP                                          
         MVC   RINVKSTA,KTSTA      STATION                                      
         MVC   RINVKINV,KTINV      INV #                                        
         MVC   RINVKSTD,KTSTD      EFF DATE                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'               HEADER SHOULD EXIST                          
*                                                                               
         MVC   HDRKEY,KEY          SAVE AWAY HDR KEY                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         LA    R2,RNWINFOH                                                      
         GOTO1 =A(DISPHDR),DMCB,(R2),RR=RELO    DISPLAY HEADER INFO             
*                                                                               
         GOTO1 =A(GETEQU#),RR=RELO     GET EQUATE # FOR 'Z' RECORD              
         TM    MYFLAG,GOTEQU#          'Z' REC ALREADY EXISTS?                  
         BZ    DISPW30                 NO - JUST DISPLAY                        
*                                                                               
         GOTO1 =A(DISPWWK#),RR=RELO    DISPLAY WEEKS AND RATES FROM             
         B     DISPW50                 'Z' RECORD                               
*                                                                               
DISPW30  DS    0H                                                               
         GOTO1 =A(DISPWWK),RR=RELO     DISPLAY WEEKS AND RATES                  
*                                                                               
DISPW50  DS    0H                                                               
         OI    MYFLAG,DISPSEL                                                   
         ST    R3,ACURHDR          A(CURRENT HEADER IN KEYTAB)                  
         B     DISPWKX                                                          
*                                                                               
DISPW100 DS    0H                                                               
         LA    R3,L'KEYTAB(R3)                                                  
*                                                                               
         LH    RF,KTABDISP                                                      
         LA    RF,L'KEYTAB(RF)                                                  
         STH   RF,KTABDISP                                                      
*                                                                               
         BCT   R4,DISPW10                                                       
*                                                                               
DISPWKX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*****************************************************************               
*        DISPLAY WEEKS (NO 'Z' RECORD EXISTS)                                   
*****************************************************************               
DISPWWK  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,RNWEFF1H                                                      
*                                                                               
         MVC   TMPBRDCU,BRDQTRST   INITIALIZE WITH BRD QTR START                
*                                                                               
DWWK10   DS    0H                                                               
         CLC   TMPBRDCU,BRDQTREN   PAST LAST WEEK IN QTR?                       
         BH    DWWKX                                                            
*                                                                               
         MVC   TMPDTEJ,HDRSTRTJ    INITIALIZE START BRD WK W/ HDR START         
*                                                                               
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BZ    *+10                                                             
         MVC   TMPDTEJ,WEEKFILT    USE WEEK FILTER AS START WK                  
*                                                                               
         CLC   TMPBRDCU,TMPDTEJ    WK < HDR START?                              
         BNL   DWWK20              YES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(8,TMPBRDCU),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,DUB)                                
         CLC   DUB(3),TMPDTEJ      FALL BETWEEN BROADCAST WEEKS?                
         BH    DWWK30                                                           
*                                                                               
         MVC   TMPBRDCU,DUB        RESET CURRENT BROADCAST WEEK                 
         B     DWWK10              NO - GET NEXTEL                              
*                                                                               
DWWK20   DS    0H                                                               
         OC    HDRENDJ,HDRENDJ     ANY END DATE?                                
         BZ    *+14                NO                                           
         CLC   TMPBRDCU,HDRENDJ    AFTER HEADER END?                            
         BNL   DWWKX                                                            
*                                                                               
DWWK30   DS    0H                                                               
         GOTO1 DATCON,DMCB,(8,TMPBRDCU),(5,8(R2))                               
         MVI   5(R2),8                                                          
*                                                                               
         BRAS  RE,NEXTFLD                                                       
         BRAS  RE,NEXTFLD                                                       
         BRAS  RE,NEXTFLD                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(8,TMPBRDCU),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,TMPBRDCU)                           
         B     DWWK10                                                           
*                                                                               
DWWKX    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        DISPLAY WEEKS/RATES/FOOTNOTES FROM 'Z' RECORD                          
*****************************************************************               
DISPWWK# NTR1  BASE=*,LABEL=*                                                   
         L     RF,AIO                                                           
         MVC   HDRKEY,0(RF)        SAVE AWAY HEADER KEY                         
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   KEY(24),HDRKEY                                                   
         MVI   RINVKSRC,C'Z'       RDETAIL RECORD TYPE                          
         MVC   RINVKNUM,EQUNUM     EQUATE NUMBER                                
         MVC   RINVKYR,YEARHLD     YEAR                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    DWWK#05                                                          
*                                                                               
         GOTO1 =A(DISPWWK),RR=RELO     DISPLAY WEEKS AND RATES                  
         B     DWWK#X                                                           
*                                                                               
DWWK#05  DS    0H                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         DROP  R6                                                               
*                                                                               
         LA    R2,RNWEFF1H                                                      
*                                                                               
         L     R6,AIO              BUILD NEW 'Z' RECORD                         
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING RIAVL,R6                                                         
*                                                                               
DWWK#10  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   DWWK#100                                                         
*                                                                               
         CLC   RIAVQTR,QTRHLD      THIS QUARTER?                                
         BNE   DWWK#10                                                          
*                                                                               
         MVC   TMPDTEJ,HDRSTRTJ    INITIALIZE START BRD WK W/ HDR START         
*                                                                               
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BZ    *+10                                                             
         MVC   TMPDTEJ,WEEKFILT    USE WEEK FILTER AS START WK                  
*                                                                               
         CLC   RIAVWEEK,TMPDTEJ    WK < HDR START?                              
         BNL   DWWK#20             YES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(8,RIAVWEEK),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,DUB)                                
         CLC   DUB(3),TMPDTEJ      FALL BETWEEN BROADCAST WEEKS?                
         BH    DWWK#30                                                          
         B     DWWK#10             NO - GET NEXTEL                              
*                                                                               
DWWK#20  DS    0H                                                               
         OC    HDRENDJ,HDRENDJ     ANY END DATE?                                
         BZ    *+14                NO                                           
         CLC   RIAVWEEK,HDRENDJ    AFTER HEADER END?                            
         BH    DWWK#10                                                          
*                                                                               
DWWK#30  DS    0H                                                               
         GOTO1 DATCON,DMCB,(8,RIAVWEEK),(5,8(R2))                               
         MVI   5(R2),8                                                          
         BRAS  RE,NEXTFLD                                                       
*                                                                               
         MVC   DEFCOST,RIAVAMT     RATE                                         
*                                                                               
         L     RF,DEFCOST                                                       
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
*                                                                               
         EDIT  (RF),(7,8(R2)),ALIGN=LEFT                                        
*                                                                               
         TM    RIAVFLG,RIAVEXCP    ONE WEEK EXCEPTION RATE?                     
         BZ    DWWK#40                                                          
         MVI   8(R2),C'$'                                                       
         EDIT  (RF),(7,9(R2)),ALIGN=LEFT                                        
*                                                                               
DWWK#40  DS    0H                                                               
         BRAS  RE,NEXTFLD                                                       
*                                                                               
         MVC   8(L'RIAVFTNT,R2),RIAVFTNT       FOOTNOTE                         
         OI    6(R2),X'80'                                                      
*                                                                               
         BRAS  RE,NEXTFLD                                                       
         B     DWWK#10                                                          
*                                                                               
DWWK#100 DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(27),HDRKEY      RESTORE HEADER RECORD                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
DWWK#X   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        CHECK SELECT FIELDS                                                    
*        OUTPUT MYFLAG OI/ GOWKSCRN IF HDR SELECTED                             
*****************************************************************               
VALLSEL  NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,INVALID                                                    
         NI    MYFLAG,X'FF'-GOWKSCRN                                            
*                                                                               
         LA    R2,RNASEL1H                                                      
         LA    R3,KEYTAB                                                        
         USING KEYTABD,R3                                                       
         LA    R4,MAXLST#                                                       
*                                                                               
VALLS10  DS    0H                                                               
         CLI   5(R2),0             SELECTED THIS HDR?                           
         BE    VALLS50             NO - CHECK NEXT ONE                          
*                                                                               
         CLI   8(R2),C'R'          SELECT FOR WEEKLY SCREEN?                    
         JNE   ERREND                                                           
         CLI   9(R2),C'+'          SELECT ALL FROM THIS POINT                   
         BNE   VALLS30                                                          
         OI    MYFLAG,GOWKSCRN                                                  
*                                                                               
VALLS20  DS    0H                  TRICKLE DOWN SELECT                          
         CLI   0(R3),X'FF'         END OF HEADERS?                              
         BE    VALLSELX                                                         
*                                                                               
         OI    KTFLAG,KTSEL        SELECT THIS HEADER                           
         LA    R3,L'KEYTAB(R3)                                                  
         B     VALLS20                                                          
*                                                                               
VALLS30  OI    KTFLAG,KTSEL        SELECT THIS HDR FOR WEEKLY SCRN              
         OI    MYFLAG,GOWKSCRN                                                  
*                                                                               
VALLS50  DS    0H                                                               
         BRAS  RE,NEXTFLD          BUMP TO NEXT HEADER                          
         BRAS  RE,NEXTFLD                                                       
         BRAS  RE,NEXTFLD                                                       
*                                                                               
         LA    R3,L'KEYTAB(R3)                                                  
         BCT   R4,VALLS10                                                       
*                                                                               
VALLSELX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*****************************************************************               
*        CHECK COSTS ON LISTING SCREEN                                          
*****************************************************************               
VALLCOST NTR1  BASE=*,LABEL=*                                                   
         LA    R2,RNASEL1H                                                      
         LA    R3,KEYTAB                                                        
         USING KEYTABD,R3                                                       
         LA    R4,MAXLST#          MAX # OF LIST ENTRIES                        
*                                                                               
VALLC10  DS    0H                                                               
         CLI   0(R3),0             ANY MORE RECORDS TO CHECK?                   
         BE    VALLC200            NO                                           
*                                                                               
         BRAS  RE,NEXTFLD                                                       
         BRAS  RE,NEXTFLD          BUMP TO COST FIELD                           
*                                                                               
         XC    KEY,KEY                                                          
         XC    DEFCOST,DEFCOST                                                  
*                                                                               
         TM    4(R2),X'80'         USER CHANGED THIS COST?                      
         BZ    VALLC100            NO - GO TO NEXT LINE                         
*                                                                               
         GOTO1 VPACK                                                            
         LR    RF,R0               MOVE COST IN RF                              
         SR    RE,RE                                                            
         M     RE,=F'100'                                                       
         ST    RF,DEFCOST          DEFAULT COST FOR THIS QUARTER                
*                                                                               
         LA    R6,KEY              GET THIS HEADER RECORD                       
         USING REINVREC,R6                                                      
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,KTREP      REP                                          
         MVC   RINVKSTA,KTSTA      STATION                                      
         MVC   RINVKINV,KTINV      INV #                                        
         MVC   RINVKSTD,KTSTD      EFF DATE                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'               HEADER SHOULD EXIST                          
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 =A(GETEQU#),RR=RELO     GET EQUATE # FOR 'Z' RECORD              
         TM    MYFLAG,GOTEQU#          'Z' REC ALREADY EXISTS?                  
         BZ    VALLC60                 NO - ADD NEW RECORD                      
*                                                                               
         MVC   SAVEKEY,KEY             CHECK IF RDETAIL REC ALREADY             
         XC    KEY,KEY             EXISTS                                       
*                                                                               
         MVC   KEY(27),SAVEKEY                                                  
*                                                                               
         MVI   RINVKSRC,C'Z'       GAV RATE RECORD                              
         MVC   RINVKNUM,EQUNUM     UNIQUE GAV NUMBER EQUATE                     
         MVC   RINVKYR,YEARHLD     AVAIL YEAR                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     ALREADY THERE?                               
         BNE   VALLC50                                                          
*                                                                               
         GOTO1 =A(UPRDET),RR=RELO      UPDATE RDETAIL REC W/ NEW COST           
         B     VALLC100                                                         
*                                                                               
VALLC50  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),SAVEKEY     RESTORE HDR KEY                              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'               HEADER SHOULD EXIST                          
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
VALLC60  DS    0H                                                               
         GOTO1 =A(ADDRDET),RR=RELO     ADD NEW RDETAIL REC W/ NEW COST          
*                                                                               
VALLC100 DS    0H                                                               
         BRAS  RE,NEXTFLD          BUMP TO NEXT LISTING LINE                    
         LA    R3,L'KEYTAB(R3)     " TO NEXT ENTRY IN TABLE                     
         BCT   R4,VALLC10                                                       
*                                                                               
VALLC200 DS    0H                                                               
*                                                                               
VALLCX   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
*****************************************************************               
*        ADD NEW 'Z' RECORD W/ NEW RATE                                         
*        INPUT   AIO MUST HAVE HEADER RECORD                                    
*****************************************************************               
ADDRDET  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*  ADD X'06' ELEMENT IN HEADER FOR THIS RDETAIL                                 
*                                                                               
         TM    MYFLAG,GOTEQU#      ALREADY GOT EQUATE?                          
         BO    ADDR05              YES                                          
*                                                                               
         GOTO1 =A(UPHDR06),RR=RELO                                              
*                                                                               
*  BUILD TABLE W/ START AND END BRD WEEKS FOR EACH QUARTER                      
*                                                                               
ADDR05   DS    0H                                                               
         GOTO1 =A(BLDBRDT),RR=RELO                                              
*                                                                               
         L     RF,AIO                                                           
         MVC   HDRKEY,0(RF)        SAVE AWAY HEADER KEY                         
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   KEY(24),HDRKEY                                                   
         MVI   RINVKSRC,C'Z'       RDETAIL RECORD TYPE                          
         MVC   RINVKNUM,EQUNUM     EQUATE NUMBER                                
         MVC   RINVKYR,YEARHLD     YEAR                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ADDR07                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
         GOTO1 =A(UPRDET),RR=RELO  RECORD EXISTS, JUST UPDATE                   
         B     ADDRDETX                                                         
*                                                                               
* !!!!   DC    H'00'               'Z' RECORD SHOULD NOT EXIST                  
*                                                                               
ADDR07   DS    0H                                                               
         MVC   AIO,AIO2                                                         
*                                                                               
         L     R6,AIO              BUILD NEW 'Z' RECORD                         
         XCEF  (R6),2000           CLEAR                                        
*                                                                               
         MVC   0(27,R6),KEYSAVE                                                 
*                                                                               
         MVC   RINVLEN,=H'34'      REC LENGTH SO FAR                            
*                                                                               
         LA    R3,BRDTAB                                                        
         USING BROADD,R3                                                        
*                                                                               
         MVC   TMPBRDCU,BRDSTART   INTIALIZE W/ FIRST WEEK OF YEAR              
*                                                                               
ADDRD10  DS    0H                                                               
         XC    ELEM,ELEM           BUILD WEEKLY ELEM W/ COST                    
         LA    R4,ELEM                                                          
         USING RIAVL,R4                                                         
         MVI   RIAVCODE,X'03'      ELEM TYPE                                    
         MVI   RIAVLEN,RIAVLENQ    ELEM LENGTH                                  
*                                                                               
         CLC   TMPBRDCU,ENBRDYRJ   FINISHED W/ THIS BRD YEAR?                   
         BH    ADDRD50             YES - ADD ACTIVITY ELEMENT                   
*                                                                               
         CLC   TMPBRDCU,BRDEND     PAST LAST WEEK IN THIS QTR?                  
         BNH   *+12                                                             
         LA    R3,L'BRDTAB(R3)     BUMP TO NEXT QTR                             
         B     ADDRD10                                                          
*                                                                               
         MVC   RIAVQTR,BRDQTR      QUARTER #                                    
         MVC   RIAVWEEK,TMPBRDCU   BROADCAST WEEK                               
*                                                                               
         CLC   BRDQTR,QTRHLD       RATE FOR THIS QUARTER?                       
         BNE   *+10                                                             
         MVC   RIAVAMT,DEFCOST     RATE                                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',ADDRREPF),(0,AIO),(R4),0                        
*                                                                               
*  GET NEXT BROADCAST WEEK                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(8,TMPBRDCU),(0,TMPDTEE)                             
*                                                                               
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,TMPBRDCU)                           
         B     ADDRD10                                                          
         DROP  R4                                                               
*                                                                               
ADDRD50  DS    0H                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R4,ELEM                                                          
         USING RINVAEL,R4                                                       
*                                                                               
         MVI   RINVACOD,X'EF'      ELEM CODE                                    
         MVI   RINVALEN,RINVALNQ   ELEM LENGTH                                  
         GOTO1 DATCON,DMCB,(5,0),(3,RINVAFST)                                   
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',ADDRREPF),(0,AIO),(R4),0                        
*                                                                               
         GOTO1 ADDREC              ADD NEW RATE RECORD                          
*                                                                               
         GOTO1 =A(UPDRTCD),RR=RELO   CHECK IF VALID RATE CODE                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET ORIGINAL HEADER BACK                     
*                                                                               
ADDRDETX DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
ADDRREPF DC    CL8'REPFILE'                                                     
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
*****************************************************************               
*       ADD X'06' ELEMENT IN HEADER FOR THIS RDETAIL                            
*****************************************************************               
UPHDR06  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM           BUILD WEEKLY ELEM W/ COST                    
         LA    R4,ELEM                                                          
         USING RIMAELEM,R4                                                      
*                                                                               
         MVI   RIMACODE,X'06'      ELEM TYPE                                    
         MVI   RIMALEN,RIMALENQ    ELEM LENGTH                                  
         MVC   RIMANUM,EQUNUM      EQUATE #                                     
         MVC   RIMAREP,REPHLD      REP CODE                                     
         MVC   RIMACDE,CODHLD      RATE CODE                                    
         MVC   RIMALNTH,LENHLD     SPOT LENGTH                                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',UPHDREPF),(0,AIO),(R4),0                        
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   HDRKEY,KEY                                                       
*                                                                               
         GOTO1 =A(UPDRTCD),RR=RELO   CHECK IF VALID RATE CODE                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET ORIGINAL HEADER BACK                     
*                                                                               
UPHDR06X DS    0H                                                               
         XIT1                                                                   
*                                                                               
UPHDREPF DC    CL8'REPFILE'                                                     
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*****************************************************************               
*  BUILD TABLE W/ START AND END BRD WEEKS FOR EACH QUARTER                      
*****************************************************************               
BLDBRDT  NTR1  BASE=*,LABEL=*                                                   
         XC    BRDTAB(BRDTABLN),BRDTAB                                          
*                                                                               
         LA    R3,BRDTAB                                                        
         USING BROADD,R3                                                        
         LA    R4,QTRTABX                                                       
*                                                                               
BLDBR10  DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    BLDBRDTX                                                         
*                                                                               
         MVC   BRDQTR,0(R4)        QUARTER #                                    
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),YEARHLD                                                  
         MVC   WORK+1(2),1(R4)     START BRD DATE RANGE                         
         MVC   WORK+3(1),YEARHLD                                                
         MVC   WORK+4(2),3(R4)     END BRD DATE RANGE                           
*                                                                               
*--GET THE BROADCAST DATE RANGE FOR THIS QTR                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(19,BRDSTART)                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(19,BRDEND)                            
*                                                                               
         LA    R3,BROADDLN(R3)                                                  
         LA    R4,QTRTABXL(R4)                                                  
         B     BLDBR10                                                          
*                                                                               
BLDBRDTX DS    0H                                                               
         MVI   0(R3),X'FF'         DENOTE END OF TABLE                          
         XIT1                                                                   
*                                                                               
QTRTABX  DC    XL1'01',XL4'010F030F'                                            
         DC    XL1'02',XL4'040F060F'                                            
         DC    XL1'03',XL4'070F090F'                                            
         DC    XL1'04',XL4'0A0F0C0F'                                            
         DC    XL1'FF'                                                          
QTRTABXL EQU   5                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        UPDATE EXISTING 'Z' RECORD W/ NEW RATE                                 
*        INPUT   AIO MUST HAVE HEADER RECORD                                    
*****************************************************************               
UPRDET   NTR1  BASE=*,LABEL=*                                                   
         MVC   HDRKEY,KEY          SAVE AWAY HEADER KEY                         
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   KEY(24),HDRKEY                                                   
         MVI   RINVKSRC,C'Z'       RDETAIL RECORD TYPE                          
         MVC   RINVKNUM,EQUNUM     EQUATE NUMBER                                
         MVC   RINVKYR,YEARHLD     YEAR                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'               'Z' RECORD SHOULD EXIST                      
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING RIAVL,R6                                                         
*                                                                               
UPRD10   DS    0H                                                               
         BRAS  RE,NEXTEL           ANY MORE WEEKS IN THIS QTR?                  
         BNE   UPRD20                                                           
*                                                                               
         CLC   RIAVQTR,QTRHLD      THIS QUARTER?                                
         BNE   UPRD10                                                           
*                                                                               
         MVC   RIAVAMT,DEFCOST     UPDATE THIS WEEK W/ NEW COST                 
         B     UPRD10                                                           
*                                                                               
UPRD20   DS    0H                                                               
         GOTO1 PUTREC              CHANGE 'Z' REC W/ NEW COST                   
*                                                                               
         GOTO1 =A(UPDRTCD),RR=RELO   CHECK IF VALID RATE CODE                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
UPRDETX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        CHECK IF ANY NEW INPUT ON SCREEN                                       
*        INPUT   PARAM1 - 1ST FIELD ON SCREEN                                   
*                PARAM2 - LAST FIELD ON SCREEN                                  
*        OUTPUT  MYFLAG - OI W/ VALSCRN IF USER INPUT                           
*****************************************************************               
CHKSCRN  NTR1  BASE=*,LABEL=*                                                   
         NI    MYFLAG,X'FF'-VALSCRN                                             
*                                                                               
         L     R2,0(R1)            1ST FIELD ON SCREEN                          
*                                                                               
CHKS10   DS    0H                                                               
         L     RF,4(R1)            LAST FIELD ON SCREEN                         
         CR    R2,RF                                                            
         BNL   CHKSCRNX                                                         
*                                                                               
         TM    4(R2),X'80'         NEW INPUT HERE?                              
         BNZ   *+12                                                             
         BRAS  RE,NEXTFLD          BUMP TO NEXT FIELD                           
         B     CHKS10                                                           
*                                                                               
         OI    MYFLAG,VALSCRN      NEW INPUT - MUST VALIDATE SCRN AGAIN         
*                                                                               
CHKSCRNX DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        DISPLAY THE LIST SCREEN                                                
*****************************************************************               
DISPLIST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 CALLOV,DMCB,(X'D3',RNMHEREH),0                                   
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVI   CURSCRN,LISTSCRN                                                 
*                                                                               
         LA    R2,RNASEL1H         1ST LINE                                     
*                                                                               
         LA    RE,KEYTAB           CLEAR TABLE OF KEYS                          
         LA    RF,KEYTABLN                                                      
         XCEF                                                                   
*                                                                               
         LA    R3,MAXLST#          MAX NUMBER OF LIST ENTRIES                   
         LA    R4,KEYTAB                                                        
         USING KEYTABD,R4                                                       
*                                                                               
         OC    LASTKEY,LASTKEY     1ST TIME THROUGH?                            
         BZ    DL10                YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),LASTKEY     GET LAST FROM PREVIOUS SCREEN                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    DLSEQ               START WITH NEXT RECORD                       
         DC    H'0'                KEY MUST EXIST                               
*                                                                               
DL10     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP                                                 
         MVC   RINVKSTA,STAHLD                                                  
         GOTO1 HIGH                                                             
         B     DL50                                                             
*                                                                               
DLSEQ    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
DL50     DS    0H                                                               
         LA    R6,KEY                                                           
*                                                                               
         CLC   KEY(17),KEYSAVE     SAME STATION?                                
         BE    *+14                                                             
         XC    LASTKEY,LASTKEY                                                  
         B     DISPLX              NO - DONE WITH LISTING                       
*                                                                               
         CLI   RINVKSRC,0          IS IT A HEADER?                              
         BNE   DLSEQ                                                            
         CLI   RINVKINV+3,0        ONLY NEW INV ALLOWED                         
         BE    DLSEQ                                                            
*                                                                               
         CLC   RINVKINV,INV#FILT   FILTER BY INV #                              
         BL    DLSEQ                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
*                                                                               
*   IS HEADER IN DATE RANGE FOR THIS QUARTER?                                   
*                                                                               
         CLC   RINVPEFF(2),STENDQTR+2      HDR START > QTR END?                 
         BH    DLSEQ                                                            
         OC    RINVPEFF+2(2),RINVPEFF+2    ANY END DATE ON HDR?                 
         BZ    DL60                        NO - IT'S IN THE DATE RANGE          
         CLC   RINVPEFF+2(2),STENDQTR      HDR END < QTR START?                 
         BL    DLSEQ                                                            
*                                                                               
DL60     DS    0H                  CHECK FILTERS                                
         OC    DYPTFILT,DYPTFILT   ANY DAYPART FILTERS?                         
         BZ    DL70                NO                                           
         LA    R6,RINVDP           DAYPARTS IN HDR                              
         LA    RF,6                                                             
*                                                                               
DL65     DS    0H                                                               
         CLI   0(R6),0             FOUND A MATCH?                               
         BE    DLSEQ               NO                                           
         CLC   DYPTFILT,0(R6)      SAME DAYPART?                                
         BE    DL70                YES - FOUND MATCH                            
*                                                                               
         LA    R6,1(R6)            TRY NEXT DAYPART IN HDR                      
         BCT   RF,DL65                                                          
         B     DLSEQ               NO MATCH FOUND                               
*                                                                               
DL70     DS    0H                                                               
         L     R6,AIO              MOVE KEY TO KEYTAB                           
*                                                                               
         GOTO1 =A(DISPHDR),DMCB,(R2),RR=RELO    DISPLAY HEADER INFO             
         GOTO1 =A(GETEQU#),RR=RELO     GET EQUATE NUMBER (Z) RECORD             
*                                                                               
         TM    MYFLAG,GOTEQU#      DOES THIS Z RECORD ALREADY EXIST?            
         BZ    DL75                NO                                           
         GOTO1 =A(DLRATE),DMCB,(R2),RR=RELO  DISPLAY RATE                       
*                                                                               
DL75     DS    0H                                                               
         MVC   KTREP,RINVKREP      REP                                          
         MVC   KTSTA,RINVKSTA      STATION                                      
         MVC   KTINV,RINVKINV      INV #                                        
         MVC   KTSTD,RINVKSTD      EFF DATE                                     
*                                                                               
         BRAS  RE,NEXTFLD                                                       
         BRAS  RE,NEXTFLD                                                       
         BRAS  RE,NEXTFLD                                                       
*                                                                               
         MVC   LASTKEY,KEY         SAVE AWAY LAST KEY LISTED                    
*                                                                               
         LA    R4,L'KEYTAB(R4)     NEXT ENTRY IN KEY TABLE                      
         BCT   R3,DLSEQ                                                         
*                                                                               
DISPLX   DS    0H                                                               
         MVI   0(R4),X'FF'         DENOTE END OF KEYTAB                         
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        DISPLAY RATE FOR THIS RECORD                                           
*        INPUT PARAM1 - A(LINE TO PRINT HDR INFO)                               
*****************************************************************               
DLRATE   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(LINE TO DISPLAY INFO)                      
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         XC    DEFCOST,DEFCOST                                                  
         XC    KEY,KEY                                                          
*                                                                               
         L     R6,AIO                                                           
         MVC   HDRKEY,0(R6)        SAVE AWAY HEADER KEY                         
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVC   KEY(24),HDRKEY      COPY KEY UP TO SOURCE                        
*                                                                               
         MVI   RINVKSRC,C'Z'       GET RDETAIL RECORD                           
         MVC   RINVKNUM,EQUNUM     EQUATE NUMBER                                
         MVC   RINVKYR,YEARHLD     YEAR                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     IF REC EXISTS, THEN DISPLAY RATE             
         BNE   DLR60                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING RIAVL,R6                                                         
*                                                                               
DLR10    DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   DLR50                                                            
*                                                                               
         CLC   RIAVQTR,QTRHLD      THIS QUARTER?                                
         BNE   DLR10                                                            
*                                                                               
         MVC   TMPDTEJ,HDRSTRTJ    INITIALIZE START BRD WK W/ HDR START         
*                                                                               
         OC    WEEKFILT,WEEKFILT   FILTER BY WEEK?                              
         BZ    *+10                                                             
         MVC   TMPDTEJ,WEEKFILT    USE WEEK FILTER AS START WK                  
*                                                                               
         CLC   RIAVWEEK,TMPDTEJ    WK < HDR START?                              
         BNL   DLR20               YES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(8,RIAVWEEK),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,DUB)                                
         CLC   DUB(3),TMPDTEJ      FALL BETWEEN BROADCAST WEEKS?                
         BH    DLR30                                                            
         B     DLR10               NO - GET NEXTEL                              
*                                                                               
DLR20    DS    0H                                                               
         OC    HDRENDJ,HDRENDJ     ANY END DATE?                                
         BZ    *+14                NO                                           
         CLC   RIAVWEEK,HDRENDJ    AFTER HEADER END?                            
         BH    DLR10                                                            
*                                                                               
DLR30    DS    0H                                                               
         OC    DEFCOST,DEFCOST     FOUND 1ST QUALIFIED WEEK?                    
         BNZ   *+14                                                             
         MVC   DEFCOST,RIAVAMT     MOVE IN FIRST WEEKLY COST                    
         B     DLR10                                                            
*                                                                               
         CLC   DEFCOST,RIAVAMT     SAME COST?                                   
         BE    DLR10                                                            
         MVI   8(R2),C'*'                                                       
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'                                                      
         B     DLR60                                                            
*                                                                               
DLR50    DS    0H                                                               
         L     RF,DEFCOST                                                       
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
*                                                                               
         EDIT  (RF),(7,8(R2)),ALIGN=LEFT                                        
         OI    6(R2),X'80'                                                      
*                                                                               
DLR60    DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    DEFCOST,DEFCOST                                                  
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY(27),HDRKEY      RESTORE HEADER SEQUENCE                      
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
DLRATEX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        GET EQUATE NUMBER FOR 'Z' RECORD                                       
*        INPUT    AIO MUST HAVE HEADER RECORD                                   
*        OUTPUT   EQUNUM - EQUATE NUMBER FOR 'Z' RECORD                         
*        OUTPUT   MYFLAG - OI W/ GOTEQU# IF 'Z' RECORD EXISTS                   
*****************************************************************               
GETEQU#  NTR1  BASE=*,LABEL=*                                                   
         NI    MYFLAG,X'FF'-GOTEQU#                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        MASTER AVAIL ELEMENT                         
*                                                                               
         LA    R5,1                NEXT AVAILABLE EQUATE                        
         STC   R5,EQUNUM                                                        
*                                                                               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GETEQ10  DS    0H                                                               
         BRAS  RE,NEXTEL           FOUND EQUATE NUMBER IN RECORD?               
         BNE   GETEQU#X            NO                                           
         USING RIMAELEM,R6                                                      
*                                                                               
         CLC   RIMAREP,REPHLD      SAME REP (CHILD)?                            
         BNE   GETEQ50             NO                                           
         CLC   RIMACDE,CODHLD      SAME AVAIL CODE?                             
         BNE   GETEQ50             NO                                           
         CLC   RIMALNTH,LENHLD     SAME AVAIL LENGTH?                           
         BNE   GETEQ50             NO                                           
*                                                                               
         MVC   EQUNUM,RIMANUM      FOUND EQUATE NUMBER                          
         OI    MYFLAG,GOTEQU#                                                   
         B     GETEQU#X                                                         
*                                                                               
GETEQ50  DS    0H                                                               
         CLC   EQUNUM,RIMANUM      SAME EQUATE NUMBER?                          
         BNE   *+12                NO - KEEP EQUNUM                             
         LA    R5,1(R5)                                                         
         STC   R5,EQUNUM                                                        
         B     GETEQ10                                                          
*                                                                               
GETEQU#X DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        DISPLAY HEADER INFO ON LINE                                            
*        INPUT PARAM1 - A(LINE TO DISPLAY HEADER INFO)                          
*****************************************************************               
DISPHDR  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(LINE TO DISPLAY INFO)                      
*                                                                               
         CLI   CURSCRN,WEEKSCRN    PRINT OUT HDR ON WEEKLY SCRN?                
         BE    DH10                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
DH10     DS    0H                                                               
         LA    R2,8(R2)                                                         
         USING LLINED,R2                                                        
*                                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   LLINV,RINVKINV      INV #                                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(5,LLEFF)                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(19,HDRSTRTJ)                           
*                                                                               
         XC    HDRENDJ,HDRENDJ                                                  
         CLI   RINVPEFF+2,0        ANY END DATE?                                
         BE    DH20                                                             
         MVI   LLEFF+8,C'-'                                                     
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(5,LLEFF+9)                           
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(19,HDRENDJ)                          
         DROP  R6                                                               
*                                                                               
DH20     DS    0H                  GET DAY AND TIME FROM HDR                    
         GOTO1 HELLO,DMCB,(C'G',DHREPF),(X'02',(R6)),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,12(R1)                                                        
         USING RIDTELEM,R6                                                      
*                                                                               
         GOTO1 UNDAY,DMCB,RIDTDAY,LLDYTIM           DAY                         
         LA    RE,17                                                            
         LA    R5,LLDYTIM                                                       
*                                                                               
DH40     CLI   0(R5),X'40'                                                      
         BNH   *+12                                                             
         LA    R5,1(R5)                                                         
         BCT   RE,DH40                                                          
*                                                                               
         MVI   0(R5),C'/'                                                       
         GOTO1 UNTIME,DMCB,RIDTTIME,(0,1(R5))       TIME                        
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              GET PROGRAM NAME                             
         GOTO1 HELLO,DMCB,(C'G',DHREPF),(X'03',(R6)),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,12(R1)                                                        
         USING RIPGELEM,R6                                                      
*                                                                               
         ZIC   R1,RIPGLEN                                                       
         SH    R1,=H'2'                                                         
         CH    R1,=H'18'           MAX OUTPUT SIZE                              
         BNL   DH50                                                             
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LLPGM(0),RIPGNAME                                                
         B     *+10                                                             
*                                                                               
DH50     MVC   LLPGM,RIPGNAME                                                   
         OI    6(R2),X'80'         TRANSMIT IT                                  
*                                                                               
DISPHDRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
DHREPF   DC    CL8'REPFILE'                                                     
         LTORG                                                                  
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
*****************************************************************               
*        CHECK OPTIONS                                                          
*****************************************************************               
CHKOPTS  NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,INVALID                                                    
         XC    DYPTFILT,DYPTFILT                                                
         XC    DYPTRFLT,DYPTRFLT                                                
         XC    WEEKFILT,WEEKFILT                                                
         XC    INV#FILT,INV#FILT                                                
*                                                                               
         LA    R2,RNMDPTH                                                       
         CLI   ACTEQU,ACTREP       IS THIS REPORT?                              
         BNE   *+8                                                              
         LA    R2,RNRDPTH                                                       
*                                                                               
         TM    4(R2),X'80'         NEW INPUT?                                   
         BZ    *+10                                                             
         XC    LASTKEY,LASTKEY                                                  
*                                                                               
         CLI   5(R2),0             ANY DAYPART FILTERS?                         
         BE    CHKO10                                                           
*                                                                               
         CLI   ACTEQU,ACTREP       IS THIS REPORT?                              
         BNE   *+14                                                             
         MVC   DYPTRFLT,8(R2)      REPORT DAYPART FILTERS                       
         B     *+10                                                             
*                                                                               
         MVC   DYPTFILT,8(R2)                                                   
*                                                                               
CHKO10   DS    0H                                                               
         LA    R2,RNMWEKH                                                       
         CLI   ACTEQU,ACTREP       IS THIS REPORT?                              
         BNE   *+8                                                              
         LA    R2,RNRWEKH                                                       
*                                                                               
         TM    4(R2),X'80'         NEW INPUT?                                   
         BZ    *+10                                                             
         XC    LASTKEY,LASTKEY                                                  
*                                                                               
         CLI   5(R2),0             ANY WEEKLY FILTER?                           
         BE    CHKO20                                                           
*                                                                               
         CLI   ACTEQU,ACTREP       IS THIS REPORT?                              
         BNE   CHKO12                                                           
         GOTO1 =A(GETBRDWK),DMCB,RNRWEK,RR=RELO   GET BROADCAST WEEK            
         B     CHKO15                                                           
*                                                                               
CHKO12   GOTO1 =A(GETBRDWK),DMCB,RNMWEK,RR=RELO   GET BROADCAST WEEK            
*                                                                               
CHKO15   MVC   WEEKFILT,TMPDTEJ    BROADCAST WEEK TO FILTER ON                  
*                                                                               
CHKO20   DS    0H                                                               
         LA    R2,RNMOPTH                                                       
         CLI   ACTEQU,ACTREP       IS THIS REPORT?                              
         BNE   *+8                                                              
         LA    R2,RNROPTH                                                       
*                                                                               
         TM    4(R2),X'80'         NEW INPUT?                                   
         BZ    *+10                                                             
         XC    LASTKEY,LASTKEY                                                  
*                                                                               
         CLI   5(R2),0             ANY NEW INV FILTER?                          
         BE    CHKOPTSX            NO                                           
*                                                                               
         CLI   ACTEQU,ACTREP       IS THIS REPORT?                              
         BNE   CHKO25                                                           
         GOTO1 SCANNER,DMCB,(0,RNROPTH),WORK                                    
         B     CHKO30                                                           
*                                                                               
CHKO25   GOTO1 SCANNER,DMCB,(0,RNMOPTH),WORK                                    
*                                                                               
CHKO30   CLI   DMCB+4,0            DID IT WORK?                                 
         JE    ERREND              NO                                           
*                                                                               
         LA    R3,WORK                                                          
         CLI   1(R3),0                                                          
         JE    ERREND                                                           
         CLC   =C'INV',12(R3)      INV # FILTER?                                
         JNE   ERREND                                                           
         MVC   INV#FILT,22(R3)     SAVE AWAY INV # FILTER                       
*                                                                               
CHKOPTSX DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        GET BROADCAST WEEK                                                     
*        INPUT  PARAM1 - DATE (EBCDIC)                                          
*        OUTPUT TMPDTEJ - BROADCAST WEEK (JULIAN)                               
*****************************************************************               
GETBRDWK NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,INVALID                                                    
         XC    DUB,DUB                                                          
*                                                                               
         L     R3,0(R1)            INPUT DATE                                   
         GOTO1 DATVAL,DMCB,(0,0(R3)),DUB                                        
         OC    DMCB(4),DMCB        VALID DATE?                                  
         JZ    ERREND                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,DUB),(0,TMPDTEE)                                  
         GOTO1 DATCON,DMCB,(0,DUB),(19,TMPDTEJ)                                 
*                                                                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),TMPBRD,GETDAY,ADDAY                    
         GOTO1 DATCON,DMCB,(0,TMPBRD),(19,TMPBRDST)                             
         GOTO1 DATCON,DMCB,(0,TMPBRD+6),(19,TMPBRDEN)                           
*                                                                               
         MVC   TMPBRDCU,TMPBRDST   CURRENT WEEK TO START WITH                   
*                                                                               
GETBR10  DS    0H                                                               
         CLC   TMPBRDCU,TMPDTEJ    BEFORE INPUT DATE?                           
         BE    GETBR50             YES                                          
         BL    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(8,TMPBRDCU),(0,TMPDTEE)                             
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,DUB)                                
*                                                                               
         CLC   TMPDTEJ,DUB         FALL BETWEEN BROADCAST WEEKS?                
         BE    GETBR45                                                          
         BL    GETBR50             YES                                          
         MVC   TMPBRDCU,DUB                                                     
         B     GETBR10                                                          
*                                                                               
GETBR45  MVC   TMPBRDCU,DUB        ACTUAL BROADCAST WEEK                        
*                                                                               
GETBR50  MVC   TMPDTEJ,TMPBRDCU    ACTUAL BROADCAST WEEK TO FILTER ON           
*                                                                               
         GOTO1 DATCON,DMCB,(8,TMPDTEJ),(2,TMPDTEC)                              
*                                                                               
GETBRDX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        VALIDATE RATE CODE KEY ENTERED (3E)                                    
*****************************************************************               
CHKRTCD  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,RNMCODH                                                       
         MVI   ERROR,INVALID                                                    
*                                                                               
         CLI   ACTEQU,ACTREP       REPORT?                                      
         BNE   *+8                                                              
         BAS   RE,GETYRHLD                                                      
*                                                                               
         LA    R6,KEY                                                           
         USING RARTREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,AGENCY                                                  
         MVC   RARTKCOD,CODHLD                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     WAS RECORD FOUND                             
         BNE   RTCDNFND                                                         
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   RTCDKEY,KEY         SAVE AWAY RATE RECORD KEY                    
*                                                                               
* CHECK IF STATION IS ACTIVE                                                    
*                                                                               
         MVI   ELCODE,X'04'                                                     
*                                                                               
CHKRT05  DS    0H                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CHKRT10  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   CHKRT15                                                          
*                                                                               
         CLC   2(5,R6),STAHLD      STATION INACTIVE OR PURGED?                  
         BNE   CHKRT10             CHECK NEXT ELEMENT                           
*                                                                               
         CLI   ELCODE,X'05'                                                     
         BNE   INACTERR                                                         
*                                                                               
         TM    18(R6),X'01'        RESTORED?                                    
         BO    CHKRT20             YES                                          
         B     INACTERR            STATION NOT ACTIVE FOR RATE CODE             
*                                                                               
CHKRT15  DS    0H                                                               
         CLI   ELCODE,X'05'                                                     
         BE    CHKRT20                                                          
         MVI   ELCODE,X'05'                                                     
         B     CHKRT05                                                          
*                                                                               
CHKRT20  DS    0H                                                               
         LA    R2,RNMLENH          SET UP HELLO LOOKUP INFO                     
         XC    DUB,DUB                                                          
         MVC   DUB(1),YEARHLD                                                   
         MVC   DUB+1(2),LENHLD                                                  
         GOTO1 HELLO,DMCB,(C'G',CHKREPF),(X'02',AIO),(3,DUB)                    
         CLI   12(R1),0                                                         
         BNE   RTCDNFND                                                         
*                                                                               
         L     R6,12(R1)                                                        
         USING RALQELEM,R6                                                      
*                                                                               
         CLI   ACTEQU,ACTREP       REPORT?                                      
         BE    CHKRT100                                                         
*                                                                               
         MVC   DUB(1),RALQQTR      MOVE QTR FROM RECORD                         
         OC    DUB(1),QTRBIT       OR IT WITH QTR FROM REQUEST                  
         CLC   DUB(1),RALQQTR      IF FIELD CHANGED THEN ERROR                  
         BNE   RTCDNFND                                                         
         B     CHKRTCDX                                                         
*                                                                               
CHKRT100 DS    0H                                                               
         BAS   RE,GQTRRFLT                                                      
         LA    R3,QTRRFILT         REQUESTED QUARTER FILTERS                    
*                                                                               
CHKRT110 DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    CHKRT120                                                         
*                                                                               
         MVC   DUB(1),RALQQTR      MOVE QTR FROM RECORD                         
         OC    DUB(1),0(R3)        OR IT WITH QTR FROM REQUEST                  
         CLC   DUB(1),RALQQTR      IF FIELD CHANGED THEN ERROR                  
         BE    CHKRTCDX                                                         
*                                                                               
         LA    R3,6(R3)            TEST NEXT QTR REQUESTED                      
         B     CHKRT110                                                         
*                                                                               
CHKRT120 DS    0H                  NO MATCH FOUND                               
         B     RTCDNFND                                                         
*                                                                               
CHKRTCDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
RTCDNFND DS    0H                                                               
         LA    R2,RNMCODH          RATE RECORD NOT FOUND                        
         MVC   RERROR(2),=AL2(RATENFND)                                         
         GOTO1 MYERROR                                                          
*                                                                               
INACTERR DS    0H                                                               
         LA    R2,RNMSSTAH         STATION NOT ACTIVE FOR RATE CARD             
         MVC   RERROR(2),=AL2(NOTACTIV)                                         
         GOTO1 MYERROR                                                          
*                                                                               
INVRQTRE DS    0H                                                               
         LA    R2,RNMCODH          INVALID QTR FOR THIS RATE REC                
         MVC   RERROR(2),=AL2(INVRQTR)                                          
         GOTO1 MYERROR                                                          
*                                                                               
CHKREPF  DC    CL8'REPFILE'                                                     
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        GET YEARHLD FOR REPORT                                                 
*****************************************************************               
GETYRHLD NTR1                                                                   
         LA    R2,RNRQTRH          VALIDATE YEAR                                
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
*                                                                               
         LA    R3,RNRQTR                                                        
         LA    R4,L'RNRQTR                                                      
*                                                                               
GETYR10  DS    0H                                                               
         CLI   0(R3),0                                                          
         JE    ERREND                                                           
*                                                                               
         CLI   0(R3),C'/'                                                       
         BE    GETYR20                                                          
         LA    R3,1(R3)                                                         
         BCT   R4,GETYR10                                                       
         J     ERREND                                                           
*                                                                               
GETYR20  DS    0H                                                               
         XC    DUB,DUB                                                          
*                                                                               
         MVC   DUB(2),1(R3)        MOVE IN YEAR YY                              
         MVC   DUB+2(4),=C'0101'   MM/DD                                        
         GOTO1 DATCON,DMCB,(0,DUB),(3,TMPDTEE)                                  
         MVC   YEARHLD,TMPDTEE     YEAR IN BINARY                               
*                                                                               
GETYRX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        GET QUARTERS REQUESTED FOR REPORT                                      
*****************************************************************               
GQTRRFLT NTR1                                                                   
         MVI   ERROR,MISSING                                                    
*                                                                               
         LA    R2,RNRQTRH                                                       
         CLI   5(R2),0                                                          
         JE    ERREND                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         XC    QTRRFILT,QTRRFILT                                                
         XC    #QTRFLTS,#QTRFLTS                                                
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R3,QTRRFILT         STORE QUARTER FILTERS                        
*                                                                               
GQTRR10  DS    0H                                                               
         CLI   0(R2),C'/'          NO MORE QTR FILTERS                          
         BE    GQTRR40                                                          
         CLI   0(R2),C','          SHOULD SEPERATE QUARTERS                     
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
*                                                                               
         LA    RE,QTRRTAB                                                       
         LA    RF,4                                                             
*                                                                               
GQTRR20  DS    0H                                                               
         CLC   0(2,R2),0(RE)       QUARTER IN TABLE?                            
         BE    GQTRR30             MUST BE NUMERIC                              
*                                                                               
         LA    RE,4(RE)                                                         
         BCT   RF,GQTRR20                                                       
*                                                                               
         LA    R2,RNRQTRH                                                       
         J     ERREND                                                           
*                                                                               
GQTRR30  DS    0H                                                               
         MVC   0(1,R3),3(RE)       GET QTR BIT                                  
         MVC   1(1,R3),2(RE)       QTRHLD BYTE                                  
*                                                                               
         ZIC   R5,#QTRFLTS                                                      
         LA    R5,1(R5)                                                         
         STC   R5,#QTRFLTS                                                      
*                                                                               
         LA    R2,2(R2)            CHECK NEXT QTR REQUESTED                     
         LA    R3,6(R3)                                                         
         B     GQTRR10                                                          
*                                                                               
GQTRR40  DS    0H                                                               
         ZIC   R0,#QTRFLTS                                                      
         LA    R5,6                L'RECORD AND L'KEY                           
         LA    R6,1                DISPLACEMENT OF KEY INTO REC                 
         GOTO1 =V(XSORT),DMCB,(0,QTRRFILT),(R0),(R5),(R6),(R6),RR=RELO          
*                                                                               
         MVI   0(R3),X'FF'         DENOTE END OF QTR FILTERS                    
*                                                                               
         MVC   HALF,1(R2)          CHECK OUT THE YEAR                           
         LA    RE,HALF                                                          
         LA    RF,2                                                             
*                                                                               
GQTRR50  CLI   0(RE),C'0'          MUST BE NUMERIC                              
         JL    ERREND                                                           
         CLI   0(RE),C'9'                                                       
         JH    ERREND                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,GQTRR50                                                       
*                                                                               
         XC    DUB,DUB                                                          
         XC    TMPDTEE,TMPDTEE                                                  
*                                                                               
         MVC   DUB(2),1(R2)        MOVE IN YEAR YY                              
         MVC   DUB+2(4),=C'0101'   MM/DD                                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,DUB),(3,TMPDTEE)                                  
         MVC   YEARHLD,TMPDTEE     YEAR IN BINARY                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),YEARHLD     MOVE YEAR INTO DATE RANGE                    
         MVC   WORK+1(2),=X'010F'                                               
         MVC   WORK+3(1),YEARHLD                                                
         MVC   WORK+4(2),=X'0C0F'                                               
*                                                                               
*--GET THE DATE RANGE FOR BROADCAST YEAR (COMPRESSED)                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(2,STBRDYRC)                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(2,ENBRDYRC)                           
*                                                                               
         BAS   RE,GQTRSTEN         GET QTRS BROADCAST START AND END             
*                                                                               
GQTRRX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
QTRRTAB  DC    CL2'Q1',XL1'01',XL1'80'                                          
         DC    CL2'Q2',XL1'02',XL1'40'                                          
         DC    CL2'Q3',XL1'03',XL1'20'                                          
         DC    CL2'Q4',XL1'04',XL1'10'                                          
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        GET QUARTERS BROADCAST START AND END                                   
*****************************************************************               
GQTRSTEN NTR1                                                                   
         LA    RE,DNLTABA                                                       
         LA    RF,4                                                             
         LA    R3,QTRRFILT                                                      
*                                                                               
GQSE10   DS    0H                  CHECK IF QUARTER IS IN TABLE                 
         CLI   0(R3),X'FF'                                                      
         BE    GQSEX                                                            
*                                                                               
         CLC   1(1,R3),0(RE)       IN TABLE?                                    
         BE    GQSE20                                                           
*                                                                               
         LA    RE,DNLTABAL(RE)     BUMP TO NEXT ENTRY                           
         BCT   RF,GQSE10                                                        
         J     ERREND                                                           
*                                                                               
GQSE20   DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+1(2),1(RE)     START BRD DATE RANGE                         
         MVC   WORK+4(2),3(RE)     END BRD DATE RANGE                           
*                                                                               
         XC    DUB,DUB                                                          
*                                                                               
         MVC   WORK(1),YEARHLD     MOVE YEAR INTO DATE RANGE                    
         MVC   WORK+3(1),YEARHLD                                                
*                                                                               
*--GET THE BROADCAST DATE RANGE FOR THIS QTR                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(2,STENDQTR)                             
         MVC   2(2,R3),STENDQTR                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(2,STENDQTR+2)                         
         MVC   4(2,R3),STENDQTR+2                                               
*                                                                               
         LA    R3,6(R3)            POINT TO NEXT ENTRY IN QTR FILTERS           
         B     GQSE10                                                           
*                                                                               
GQSEX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
DNLTABA  DC    XL1'01',XL5'010F030F80'                                          
         DC    XL1'02',XL5'040F060F40'                                          
         DC    XL1'03',XL5'070F090F20'                                          
         DC    XL1'04',XL5'0A0F0C0F10'                                          
DNLTABAL EQU   6                                                                
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        UPDATE RATE CODE KEY ENTERED (3E)                                      
*****************************************************************               
UPDRTCD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,DUB),(19,TODAYJ)                                  
*                                                                               
         LA    R6,KEY                                                           
         USING RARTREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,AGENCY                                                  
         MVC   RARTKCOD,CODHLD                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     WAS RECORD FOUND                             
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   RTCDKEY,KEY         SAVE AWAY RATE RECORD KEY                    
*                                                                               
         LA    R2,RNMLENH          SET UP HELLO LOOKUP INFO                     
         XC    DUB,DUB                                                          
         MVC   DUB(1),YEARHLD                                                   
         MVC   DUB+1(2),LENHLD                                                  
         GOTO1 HELLO,DMCB,(C'G',UPDREPF),(X'02',AIO),(3,DUB)                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R6,12(R1)                                                        
         USING RALQELEM,R6                                                      
*                                                                               
         LA    R6,RALQLST1         ENTER LAST DATE CHANGED                      
         ZIC   R5,QTRHLD                                                        
         B     *+8                                                              
*                                                                               
UPDR10   DS    0H                                                               
         LA    R6,3(R6)            BUMP TO NEXT QTR                             
         BCT   R5,UPDR10                                                        
*                                                                               
         MVC   0(3,R6),TODAYJ                                                   
         DROP  R6                                                               
*                                                                               
UPDR20   DS    0H                  CHECK/ADD NEW X'03' ELEMENT                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING RASTELEM,R6                                                      
*                                                                               
UPDR30   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   UPDR100             DOESN'T EXIST - ADD NEW ONE                  
*                                                                               
         CLC   RASTSTA,STAHLD      SAME SATION?                                 
         BNE   UPDR30                                                           
*                                                                               
         XC    ELEM,ELEM           FOUND ELEMENT, UPDATE AND PUT BACK           
         ZIC   R1,RASTLEN                                                       
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)       SAVE AWAY ELEMENT                            
*                                                                               
         MVI   0(R6),X'FF'         MARK THIS ELEMENT FOR DELETION               
         GOTO1 HELLO,DMCB,(C'D',UPDREPF),(X'FF',AIO),0                          
*                                                                               
         LA    R6,ELEM                                                          
         ZIC   R1,RASTLEN                                                       
         LA    RF,RASTLENQ                                                      
         SR    R1,RF               # OF YEARS                                   
         BP    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R6,RASTYR                                                        
*                                                                               
UPDR40   DS    0H                                                               
         CLC   0(1,R6),YEARHLD     YEAR ALREADY HERE ?                          
         BE    UPDR130             YES - PUT BACK ELEMENT                       
*                                                                               
         LA    R6,1(R6)                                                         
         BCT   R1,UPDR40                                                        
*                                                                               
         MVC   0(1,R6),YEARHLD     ADD YEAR IN ELEMENT                          
*                                                                               
         LA    R6,ELEM                                                          
         ZIC   R1,RASTLEN                                                       
         LA    R1,1(R1)            INCREMENT LEN BY 1 FOR NEW YEAR              
         STC   R1,RASTLEN                                                       
         B     UPDR130             UPDATE RECORD WITH ELEMENT                   
*                                                                               
UPDR100  DS    0H                  ADD NEW X'03' ELEMENT FOR THIS STA           
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
*                                                                               
         MVI   RASTCODE,X'03'                                                   
*                                                                               
         LA    R1,RASTLENQ                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RASTLEN                                                       
*                                                                               
         MVC   RASTSTA,STAHLD      STATION                                      
         MVC   RASTCDTE,TODAYJ     ELEMENT CREATE DATE (TODAY)                  
*                                                                               
         LA    R6,RASTYR                                                        
         MVC   0(1,R6),YEARHLD                                                  
*                                                                               
UPDR130  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',UPDREPF),(0,AIO),ELEM,0                         
*                                                                               
UPDRTCDX DS    0H                                                               
         GOTO1 PUTREC                                                           
         XIT1                                                                   
*                                                                               
UPDREPF  DC    CL8'REPFILE'                                                     
*                                                                               
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD MODULE                                                    *          
*          PARM1 - RC                                                *          
*          PARM2 - ACTION                                            *          
**********************************************************************          
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         LA    R5,DWNBUF                                                        
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,P                PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'P)                                                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 VDLFLD,(R5)                                                      
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         BAS   RE,DLPRINT                                                       
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
*                                  TURN OFF DOWN-LOAD ROW FLDS AS C' '          
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVC   DLCBLEN,PRTSIZE                                                  
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL50   GOTO1 VDLFLD,(R5)          DOWN-LOAD FIELD                             
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
         SPACE 1                                                                
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         BAS   RE,DLPRINT                                                       
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT DOWNLOAD LINE                                                           
***********************************************************************         
DLPRINT  NTR1                                                                   
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* REGAVFFD                                                                      
* DDGENTWA                                                                      
* REGAVWTWA                                                                     
* REGENINVA                                                                     
* REGENARTE                                                                     
* REGAVWORKD                                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE REGAVFFD                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE REGAVD1D          KEY FIELDS                                   
         EJECT                                                                  
         ORG   RNMHEREH                                                         
       ++INCLUDE REGAVD2D          RDETAIL/WEEKLY SCREEN                        
         EJECT                                                                  
         ORG   RNMHEREH                                                         
       ++INCLUDE REGAVD3D          RDETAIL/CHANGE (LIST) SCREEN                 
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REGAVD4D          RDETAIL/REPORT SCREEN                        
         EJECT                                                                  
       ++INCLUDE REGAVWTWA                                                      
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
*                                                                               
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE REGENARTE                                                      
       ++INCLUDE REGAVWORKD                                                     
*                                                                               
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
*****************************************************                           
*              WORK AREA                                                        
*****************************************************                           
RELO     DS    A                                                                
*                                                                               
SAVEKEY  DS    CL27                                                             
HDRKEY   DS    CL27                HEADER KEY                                   
LASTKEY  DS    CL27                CURRENT KEY TO START LIST WITH               
RTCDKEY  DS    CL27                RATE CODE KEY (3E)                           
*                                                                               
STAHLD   DS    CL5                 STATION                                      
REPHLD   DS    CL2                 REP                                          
CODHLD   DS    CL8                 RATE CODE                                    
LENHLD   DS    XL2                 SPOT LENGTH                                  
YEARHLD  DS    XL1                 YEAR IN BINARY                               
QTRHLD   DS    CL1                 QTR (BINARY)                                 
QTRBIT   DS    XL1                 QUARTER BIT                                  
*                                                                               
TODAYJ   DS    XL3                 TODAY'S DATE (JULIAN)                        
*                                                                               
STENDQTR DS    XL4                 DATE RANGE FOR QTR (COMPRESSED)              
STBRDYRJ DS    XL3                 FIRST WEEK OF BRD YEAR (JULIAN)              
ENBRDYRJ DS    XL3                 LAST WEEK OF BRD YEAR (JULIAN)               
STBRDYRC DS    XL2                 FIRST WEEK OF BRD YEAR (COMPRESSED)          
ENBRDYRC DS    XL2                 LAST WEEK OF BRD YEAR (COMPRESSED)           
*                                                                               
EQUNUM   DS    XL1                 EQUATE NUMBER FOR 'Z' RECORD                 
DEFCOST  DS    F                   RATE                                         
PRVCOST  DS    F                   PREVIOUS COST                                
*                                                                               
*                                                                               
KEYTAB   DS    15CL18              KEY HOLD AREA 15 KEYS 18 BYTES EACH          
KEYTABLN EQU   *-KEYTAB                                                         
         DC    X'FF'                                                            
*                                                                               
BRDTAB   DS    4XL7                BROADCAST STR AND END DATES                  
BRDTABLN EQU   *-BRDTAB            FOR 4 QTRS IN YEAR                           
         DC    X'FF'                                                            
*                                                                               
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
*                                                                               
THISLINE DS    A                   CURRENT LINE ADDRESS                         
*                                                                               
CURSCRN  DS    XL1                 CURRENT SCREEN                               
LISTSCRN EQU   X'D3'               LISTING SCREEN                               
REPSCRN  EQU   X'D4'               REPORT SCREEN                                
WEEKSCRN EQU   X'D2'               WEEKLY SCREEN                                
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
VALSCRN  EQU   X'01'               VALIDATE SCREEN AGAIN                        
GOTEQU#  EQU   X'02'               GOT EQUATE # FROM RECORD                     
GOWKSCRN EQU   X'04'               GO TO WEEKLY SCREEN                          
DISPSEL  EQU   X'10'               DISPLAYED SELECTED HEADER IN WK SCRN         
QTHASCST EQU   X'20'               THIS QUARTER HAS A COST                      
NOWEEKS  EQU   X'40'               NO WEEKS FOR THIS QTR                        
*                                                                               
TMPDTEE  DS    CL6                 TEMP DATE (YYMMDD)                           
TMPDTEE2 DS    CL6                 TEMP DATE (YYMMDD)                           
TMPDTEJ  DS    XL3                 TEMP DATE (JULIAN)                           
TMPBRD   DS    CL12                TEMP BROADCASE DATE RANGE (YYMMDD)           
TMPBRDST DS    XL3                 TEMP START BROADCAST WEEK (JULIAN)           
TMPBRDEN DS    XL3                 TEMP END BROADCAST WEEK (JULIAN)             
TMPBRDCU DS    XL3                 TEMP CURRENT BROADCAST WEEK (JULIAN)         
*                                                                               
TMPDTEC  DS    XL2                 TEMP DATE COMPRESSED                         
*                                                                               
BRDDTEE  DS    CL12                BROADCAST DATE RANGE (YYMMDD)                
BRDQTRST DS    XL3                 BROADCAST START DATE (JULIAN)                
BRDQTREN DS    XL3                 BROADCAST END DATE (JULIAN)                  
*                                                                               
HDRSTRTJ DS    XL3                 HEADER START (JULIAN)                        
HDRENDJ  DS    XL3                 HEADER END (JULIAN)                          
*                                                                               
DYPTFILT DS    CL1                 DAYPART FILTER                               
WEEKFILT DS    XL3                 WEEKLY FILTER (JULIAN)                       
WEEKFLTC DS    XL2                 WEEKFILT COMPRESSED                          
INV#FILT DS    CL4                 INV# START AT FILTER                         
*                                                                               
DYPTRFLT DS    CL6                 DAYPART FILTER FOR REPORT                    
*                                                                               
QTRRFILT DS    XL25                QUARTERS REQUESTED FOR REPORT                
*                                  (1) QUARTER                                  
*                                  (1) QUARTER BIT                              
*                                  (2) BROADCAST QTR START - COMPR.             
*                                  (2) BROADCAST QTR END - COMPR.               
*                                                                               
#QTRFLTS DS    XL1                 # OF QUARTERS REQ. FOR REPORT                
INVRFILT DS    CL8                 INV # FILTERS FOR REPORT                     
CURQTFLT DS    XL1                 CURRENT QTR BEING FILTERED                   
*                                                                               
ANXTQTR  DS    A                   A(NEXT QTR IN QTR FILTERS REPORT)            
ACURHDR  DS    A                   A(CURRENT HEADER IN KEYTAB)                  
KTABDISP DS    H                   DISPLACEMENT INTO KEYTAB                     
*                                                                               
*                                                                               
DWNFLD   DS    CL36                SAVED AREA FOR FIELD TO BE DWNLOADED         
PRTSIZE  DS    CL1                 DOWNLOAD FLD PRINT SIZE FOR PADDING          
*                                                                               
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNINTZ  EQU   X'80'               DOWNLOAD INITIALIZED                         
DWNHDLN  EQU   X'40'               DOWNLOAD HEADLINES                           
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                      DOWN-LOAD INITIALIZATION                  
DWNEOL   EQU   2                      MARK END OF LINE                          
DWNEOR   EQU   3                      MARK END OF REPORT                        
DWNTEXT  EQU   4                      DOWN-LOAD TEXT                            
DWNNUM   EQU   5                      DOWN-LOAD NUMBER                          
DWNPACK  EQU   6                      DOWN-LOAD NUMBER (PACKED)                 
*                                                                               
VDLFLD   DS    A                                                                
ADWNL    DS    A                                                                
*                                                                               
WKCOUNT  DS    XL1                 # OF VALID BRD WEEKS FOR REQUEST             
*                                                                               
SVINV    DS    CL8                 SAVE OFF INV#                                
SVPGM    DS    CL27                SAVED PROGRAM NAME                           
SVDYTM   DS    CL18                SAVED DAY/TIME                               
SVEFF    DS    CL17                SAVED EFFECTIVE DATES                        
SVDPT    DS    CL8                 SAVED DAYPARTS                               
*                                                                               
DWNBUF   DS    XL250                                                            
*                                                                               
DWNTMP   DS    XL500                                                            
*                                                                               
MAXLST#  EQU   15                  MAX NUMBER OF LIST ENTRIES                   
MAXDPT#  EQU   6                   MAX NUMBER OF DPTS FOR REPORT                
*                                                                               
*****************************************************                           
*      ERROR EQUATES                                                            
*****************************************************                           
RATENFND EQU   814                 RATE RECORD NOT FOUND FOR COMBO              
INITQTR  EQU   815                 INITIALIZE QTR W/ COST                       
INVRQTR  EQU   825                 INVALID QUARTER FOR THIS RATE REC            
NOTACTIV EQU   845                 STA NOT ACTIVE FOR RATE CODE                 
*                                                                               
         EJECT                                                                  
*****************************************************                           
*      DSECT FOR KEYTAB                                                         
*****************************************************                           
KEYTABD  DSECT                                                                  
KTREP    DS    CL2                 REP                                          
KTSTA    DS    CL5                 STATION                                      
KTINV    DS    CL4                 INV #                                        
KTSTD    DS    XL3                 EFF DATE                                     
KTSRC    DS    CL1                 SOURCE                                       
KTEQU    DS    XL1                 EQUATE NUMBER                                
KTYR     DS    XL1                 YEAR (BINARY)                                
KTFLAG   DS    XL1                 FLAG BIT                                     
KTSEL    EQU   X'01'               SELECTED THIS HEADER                         
*                                                                               
*****************************************************                           
*      DSECT FOR BRDTAB                                                         
*****************************************************                           
BROADD   DSECT                                                                  
BRDQTR   DS    XL1                 QUARTER #                                    
BRDSTART DS    XL3                 START BROAD. MO (JULIAN)                     
BRDEND   DS    XL3                 END BROAD. MO (JULIAN)                       
BROADDLN EQU   *-BRDQTR                                                         
*                                                                               
*****************************************************                           
*      DSECT FOR LIST LINES IN RDETAIL/CHANGE                                   
*****************************************************                           
LLINED   DSECT                                                                  
LLINV    DS    CL4                                                              
         DS    CL1                                                              
LLEFF    DS    CL17                                                             
         DS    CL1                                                              
LLDYTIM  DS    CL18                                                             
         DS    CL2                                                              
LLPGM    DS    CL18                                                             
*                                                                               
*****************************************************                           
*      DSECT FOR REPORT HEADLINES                                               
*****************************************************                           
HEADD    DSECT                                                                  
HEADINV  DS    CL8                 INV #                                        
         DS    CL1                                                              
HEADPGM  DS    CL27                PROGRAM NAME                                 
         DS    CL1                                                              
HEADDYTM DS    CL18                DAY/TIME                                     
         DS    CL1                                                              
HEADEFF  DS    CL17                EFF DATE                                     
         DS    CL1                                                              
HEADDPT  DS    CL8                 DAYPARTS                                     
*                                                                               
*****************************************************                           
*      DSECT FOR REPORT WEEK BREAKOUTS                                          
*****************************************************                           
WEEKD    DSECT                                                                  
WEEK1    DS    CL8                 WEEKLY DATE AND RATE                         
         DS    CL1                                                              
WEEKDLQ  EQU   *-WEEK1                                                          
*                                                                               
*****************************************************                           
*      DSECT FOR REPORT FOOTNOTE BREAKOUTS                                      
*****************************************************                           
FTNTD    DSECT                                                                  
FTNTSTRT DS    CL8                                                              
         DS    CL1                                                              
FTNTEFF  DS    CL8                 WEEKLY EFF DATE                              
         DS    CL1                                                              
FTNTPGM  DS    CL25                PROGRAM NAME                                 
FTNTPGML EQU   *-FTNTSTRT                                                       
*                                                                               
*****************************************************                           
*      DSECT FOR REPORT (TABLE FOR THE QTR)                                     
*****************************************************                           
WKRATED  DSECT                                                                  
WKDATE   DS    XL3                 WEEK DATE (JULIAN)                           
WKRATE   DS    XL4                 WEEKLY RATE                                  
WKFTNT   DS    XL25                FOOTNOTE FOR THIS WEEK                       
WKRATEDL EQU   *-WKDATE                                                         
*                                                                               
         PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
T813FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T813FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013REGAV18S  05/01/02'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
