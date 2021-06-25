*          DATA SET RERMP28X   AT LEVEL 109 AS OF 02/07/00                      
*PHASE T81028B                                                                  
*INCLUDE REGETKSRC                                                              
*INCLUDE RECUP                                                                  
T81028   TITLE 'RERMP28 - INVENTORY Track COPY REPORT'                          
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* Jan10/97 001 SCHT - New program for Inventory Track Copy Report     *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
RMP28    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81028**,R9,RR=RE                                              
                                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
                                                                                
         ST    RE,RELO                                                          
         ST    RB,MYBASE1                                                       
         ST    R9,MYBASE2                                                       
*                                                                               
         BAS   RE,MYINIT           INITIALIZATION                               
*                                                                               
         DS    0H                  CHECK GENCON MODES                           
         CLI   MODE,VALREC                                                      
         BE    VREC                                                             
         CLI   MODE,PRINTREP                                                    
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                          MY INITIALIZATION                          *         
*---------------------------------------------------------------------*         
MYINIT   NTR1                                                                   
*                                                                               
         DS    0H                  SET UP ADCONS OF TABLES & ROUTINES           
         LH    R2,=Y(DISPTAB-RMP28)                                             
         LA    R2,RMP28(R2)                                                     
         LA    R0,DISPTABQ                                                      
*                                                                               
MI10     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         LA    RE,RMP28            RE = BASE OF TABLE/ROUTINE                   
         ZICM  RF,2(R2),(3)                                                     
         BZ    *+12                                                             
         LA    RF,GEND(RF)                                                      
         L     RE,0(RF)                                                         
         AR    R1,RE               R1 = A(TABLE OR ROUTINE)                     
         ZICM  RF,4(R2),(3)                                                     
         A     RF,ASYSD            RF-->PLACE TO STORE ADDRESS                  
         ST    R1,0(RF)                                                         
         LA    R2,L'DISPTAB(R2)                                                 
         BCT   R0,MI10                                                          
*                                                                               
         DS    0H                  SET UP LABELS IN BIGAREA                     
         LH    R2,=Y(LBLTAB-RMP28)                                              
         LA    R2,RMP28(R2)                                                     
         LA    R0,LBLTABQ                                                       
                                                                                
MI20     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         A     R1,ATIA             R1 = A(TO PUT LABEL)                         
         MVC   0(8,R1),2(R2)        AND MOVE LABEL IN                           
         LA    R2,L'LBLTAB(R2)                                                  
         BCT   R0,MI20                                                          
*                                                                               
MI40     DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
         OI    CONSERVH+6,X80+X01  FOR PFKEYS TO WORK                           
         OI    GENSTAT1,RDUPAPPL   WE CONTROL READ FOR UPDATE                   
*                                                                               
         DS    0H                  MOVE PROFILE TO LOCAL WORKNG STORAGE         
         LR    RF,RA                                                            
         AH    RF,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,RF                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
         DROP  RF                                                               
*                                                                               
         DS    0H                  TRANSLATE DATA DICT TERMS                    
         XC    DMCB(6*4),DMCB                                                   
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNL                                                   
         MVI   DDRETN,DDCASEL                                                   
         MVI   DDRETN,C'M'         (NO IDEA WHY THIS YIELDS LOWER CASE)         
         MVI   DDSYS,8                                                          
         MVI   DDLANG,C' '                                                      
         LH    RE,=Y(DCLIST-RMP28)                                              
         A     RE,MYBASE1                                                       
         STCM  RE,7,DDIADR                                                      
         LA    RE,DSLIST                                                        
         STCM  RE,7,DDOADR                                                      
         GOTO1 DICTATE,(R1)                                                     
         DROP  R1                                                               
*                                                                               
         NI    RE@OPTS,XFF-X40     CHANGE FROM C'O' TO C'o'                     
*                                                                               
         XC    ACURFORC,ACURFORC                                                
*                                                                               
MIX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                       VALIDATE RECORD ROUTINE                       *         
*---------------------------------------------------------------------*         
VREC     DS    0H                                                               
*                                                                               
         NI    CHNGFLG1,XFF-CF1KEY                                              
         NI    MISCFLG1,XFF-MF1GDPLY  DON'T FORCE TO DISPLAY, YET               
         NI    RECDFLG1,XFF-RF1DEL    ASSUME RECORD NOT DELETED                 
*                                                                               
         OI    MISCFLG1,MF1RDDEL   READ DELETED RECORDS                         
*                                                                               
         DS    0H                  CLEAR AREA FOR FROM/TO DETAILS               
         XC    FROMDETL(FRMDETLX-FROMDETL),FROMDETL                             
         XC    TODETAIL(TODTAILX-TODETAIL),TODETAIL                             
         XC    MYFLAG,MYFLAG                                                    
*                                                                               
*--------------------- VALIDATE "FROM" DETAILS -----------------------*         
*                                                                               
VR10     LA    R2,ITCFSTAH         FROM STATION FIELD                           
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         GOTO1 ANY                  MUST HAVE A STATION INPUT                   
         GOTO1 VALISTA              VALIDATE THE STATION INPUTTED               
         OI    4(R2),X20                                                        
*                                                                               
         MVC   FRSTTN,WORK         HOLD ONTO KEY FIELD INPUT                    
         CLI   FRSTTN+4,C' '                                                    
         BNE   *+8                                                              
         MVI   FRSTTN+4,C'T'                                                    
         CLI   WORK+40,C' '        CHECK FOR SATELLITES                         
         BE    *+10                                                             
         MVC   FRSTTN+4(1),WORK+40                                              
*                                                                               
         MVC   TMPSTTN,FRSTTN      MOVE IN FROM STATION                         
         XC    TMPINVN,TMPINVN                                                  
         XC    TMPBDATE,TMPBDATE                                                
         XC    TMPKSRC,TMPKSRC                                                  
         XC    TMPBOOK,TMPBOOK                                                  
         MVI   GOSUBN,BIK#         BUILD KEY OF AN INVENTORY RECORD             
         GOTO1 AGOSUB                                                           
*                                                                               
         MVI   MYRDUPDT,C'N'                                                    
         GOTO1 HIGH                SEE IF STATION EXIST IN INV FILE             
         CLC   KEY(IKYSTAL),KEYSAVE                                             
         BNE   RCDNTFND             NO IT DOESN'T                               
*                                                                               
VR20     LA    R2,ITCFINVH         INVENTORY NUMBER                             
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
*                                                                               
         CLI   5(R2),0             ANY INVENTORY NUMBER INPUT?                  
         BE    MISSFLD              NOPE                                        
*                                                                               
         MVC   TMPINVN,8(R2)       MOVE INV # INTO KEY                          
         OC    TMPINVN,SPACES                                                   
         OI    4(R2),X20                                                        
*                                                                               
VR30     LA    R2,ITCFEFDH         EFFECTIVE DATE (RANGE)                       
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
*                                                                               
         XC    FREFFDB,FREFFDB     ASSUME NO EFFECTIVE DATES INPUT              
         CLI   5(R2),0             ANY EFFECTIVE DATE INPUT?                    
         BE    MISSFLD              NOPE                                        
*                                                                               
         BAS   RE,GETEFFD          GET EFFECTIVE DATE (PERVAL)                  
*                                                                               
         MVC   TMPBDATE,PERBSTA    MOVE EFFECTIVE DATE INTO KEY                 
*                                                                               
         MVC   TMPSTTN,FRSTTN      MOVE IN FROM STATION                         
         XC    TMPKSRC,TMPKSRC                                                  
         XC    TMPBOOK,TMPBOOK                                                  
         MVI   GOSUBN,BIK#         BUILD KEY OF AN INVENTORY RECORD             
         GOTO1 AGOSUB                                                           
*                                                                               
         GOTO1 HIGH                SEE IF STATION EXIST IN INV FILE             
         LA    R2,ITCFSTAH                                                      
         CLC   KEY(24),KEYSAVE                                                  
         BNE   RCDNTFND             NO IT DOESN'T                               
*                                                                               
         MVC   FRSEFFDB,PERBSTA    GET START AND                                
         MVC   FREEFFDB,PERBEND     END EFFECTIVE DATES                         
*                                                                               
         OI    4(R2),X20                                                        
*                                                                               
VR32     LA    R2,ITCSRCEH         TRACK SOURCE                                 
         CLI   5(R2),0             ANY TRACK SOURCE?                            
         BNE   VR35                                                             
         MVI   8(R2),C'N'          DEFAULT TO NSI                               
         OI    6(R2),X'80'                                                      
*                                                                               
VR35     DS    0H                                                               
*                                                                               
         CLI   8(R2),C'N'          NSI?                                         
         BE    VR36                                                             
         CLI   8(R2),C'S'          SRC?                                         
         BE    VR36                                                             
         CLI   8(R2),C'M'          MFX?                                         
         BNE   INVLFLD                                                          
VR36     MVC   TRKSRCF,8(R2)       TRACK SOURCE FILTER                          
*                                                                               
VR45     LA    R2,ITCTRKSH         TRACKS                                       
         CLI   5(R2),0             ANY TRACKS                                   
         BNE   VR47                YES - THERE ARE TRACKS                       
         MVC   8(3,R2),=C'ALL'     DEFAULT TO ALL                               
         OI    6(R2),X'80'                                                      
*                                                                               
VR47     LA    R2,ITCTRKSH         TRACKS                                       
         CLC   =C'ALL',8(R2)       COPY ALL TRACKS?                             
         BNE   VR48                                                             
         OI    MYFLAG,ALLTRKS      YES - COPY ALL TRACKS                        
         B     VR50                                                             
*                                                                               
VR48     DS    0H                                                               
         CLC   =C'NONE',8(R2)      DO NOT COPY ANY TRACKS?                      
         BNE   VR49                                                             
         OI    MYFLAG,NOTRKS       NO - DON'T COPY ANY TRACKS                   
         B     VR50                                                             
*                                                                               
VR49     DS    0H                                                               
         GOTO1 SCANNER,DMCB,(R2),(12,BUFF),C',=,-'                              
         CLI   DMCB+4,0            SCANNER WORKED?                              
         BE    INVLFLD                                                          
         MVC   TRCKCNT,DMCB+4      # OF TRACKS LINES                            
*                                                                               
         BAS   RE,TRACKS           BUILD TRACK FILTER TABLE                     
*                                                                               
VR50     DS    0H                                                               
         LA    R2,ITCFTXTH         COPY TEXT RECORDS?                           
*                                                                               
         MVI   FRCPYTXT,C'Y'       DEFAULT TO YES                               
         CLI   5(R2),0             IF NO INPUT                                  
         BE    VR52                 DEFAULT TO YES                              
         CLI   8(R2),C'Y'          IF INPUT = C'Y'                              
         BE    VR60                 WE'RE OKAY                                  
         MVI   FRCPYTXT,C'N'                                                    
         CLI   8(R2),C'N'          IF INPUT = C'N'                              
         BE    VR55                 SET "COPY TRACKS" TO NO                     
         B     INVLFLD                                                          
*                                                                               
VR52     DS    0H                                                               
         MVI   8(R2),C'Y'          MOVE IN YES                                  
         OI    6(R2),X'80'                                                      
         B     VR60                                                             
*                                                                               
VR55     DS    0H                                                               
         LA    R2,ITCTRKSH                                                      
         TM    MYFLAG,NOTRKS       DO NOT COPY BOTH TRACK AND TEXT?             
         BO    NORECSEL            MUST COPY AT LEAST A TRACK OR TEXT           
*                                                                               
         EJECT                                                                  
*---------------------- VALIDATE "TO" DETAILS ------------------------*         
*                                                                               
VR60     DS    0H                                                               
         LA    R2,ITC1STAH         TO STATION FIELD                             
*                                                                               
         CLI   5(R2),0             ANY 'TO' STATION?                            
         BE    MISSFLD             NO                                           
*                                                                               
         LA    R1,TOTABLE          'TO' STATION TABLE                           
         USING TOTABLED,R1                                                      
         LA    R4,5                MAX # OF 'TO' INPUTS                         
         MVI   TOCOUNT,0           # OF 'TO' ENTRIES                            
*                                                                               
VR70     DS    0H                                                               
*                                                                               
         CLI   5(R2),0             ANY 'TO' STATION                             
         BNE   VR75                                                             
*                                                                               
         ST    R2,SCRPTR           A(EMPTY 'TO' STATION ON SCREEN)              
         BAS   RE,BMPFLD           NO - 'TO' STATION,                           
         BAS   RE,BMPFLD                                                        
*                                                                               
         CLI   5(R2),0             ANY INV # ?                                  
         BE    *+12                                                             
         L     R2,SCRPTR           YES - THEN THERE MUST BE A                   
         B     MISSFLD                   'TO' STATION HERE                      
*                                                                               
         BAS   RE,BMPFLD                                                        
         BAS   RE,BMPFLD                                                        
*                                                                               
         CLI   5(R2),0             ANY EFFECTIVE DATE?                          
         BE    *+12                                                             
         L     R2,SCRPTR           YES - THEN THERE MUST BE A                   
         B     MISSFLD                   'TO' STATION HERE                      
*                                                                               
         B     VR90                                                             
*                                                                               
VR75     BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         ST    R2,TOSTAH            A(TO STATION HEADER)                        
         GOTO1 VALISTA              VALIDATE THE STATION INPUTTED               
         OI    4(R2),X20                                                        
*                                                                               
         MVC   TOSTA,WORK          HOLD ONTO KEY FIELD INPUT                    
         CLI   TOSTA+4,C' '                                                     
         BNE   *+8                                                              
         MVI   TOSTA+4,C'T'                                                     
         CLI   WORK+40,C' '        CHECK FOR SATELLITES                         
         BE    *+10                                                             
         MVC   TOSTA+4(1),WORK+40                                               
*                                                                               
         BAS   RE,BMPFLD           INV. NUMBER                                  
         BAS   RE,BMPFLD                                                        
*                                                                               
         CLI   5(R2),0             ANY INV #?                                   
         BNE   VR79                YES                                          
         MVC   TOINV,ITCFINV       'TO' INV # = 'FROM' INV #                    
         OC    TOINV,SPACES                                                     
         MVC   8(L'ITCFINV,R2),ITCFINV                                          
         OI    6(R2),X'80'                                                      
         B     VR80                                                             
*                                                                               
VR79     MVC   TOINV,8(R2)         INVENTORY NUMBER                             
         OC    TOINV,SPACES                                                     
*                                                                               
VR80     BAS   RE,BMPFLD           EFFECTIVE DATE                               
         BAS   RE,BMPFLD                                                        
*                                                                               
         CLI   5(R2),0             ANY EFFECTIVE DATE?                          
         BE    VR85                NO                                           
*                                                                               
         OI    TOFLAG1,TOF1EFFD    DIFFERENT "TO" EFFECTIVE DATE                
         BAS   RE,GETEFFD          GET EFFECTIVE DATE (PERVAL)                  
*                                                                               
         MVC   TOSTD,PERBSTA       EFFECTIVE DATE                               
         OI    4(R2),X20                                                        
         B     VR87                                                             
*                                                                               
VR85     MVC   TOSTD,FRSEFFDB      EFFECTIVE DATE                               
         MVC   8(L'ITCFEFD,R2),ITCFEFD                                          
         OI    6(R2),X'80'                                                      
*                                                                               
VR87     XC    KEY,KEY             SEE IF 'TO' HEADER EXISTS                    
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
*                                                                               
         MVI   RINVKTYP,X'12'      RECORD TYPE                                  
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,TOSTA      STATION                                      
         MVC   RINVKINV,TOINV      INVENTORY #                                  
         MVC   RINVKSTD,TOSTD      EFFECTIVE DATE                               
*                                                                               
         GOTO1 HIGH                HEADER EXIST?                                
         CLC   KEY(RINVKBK-RINVKEY),KEYSAVE                                     
         BE    VR90                                                             
         L     R2,TOSTAH                                                        
         B     NOHEAD              'TO' HEADER DOESN'T EXIST                    
*                                                                               
VR90     LA    R1,L'TOTABLE(R1)    NEXT ENTRY IN 'TO' STATION TABLE             
*                                                                               
         ZIC   RF,TOCOUNT          INCREMENT 'TO' COUNTER                       
         LA    RF,1(RF)                                                         
         STC   RF,TOCOUNT                                                       
*                                                                               
         BAS   RE,BMPFLD           NEXT 'TO' STATION                            
         BAS   RE,BMPFLD                                                        
         BCT   R4,VR70                                                          
         DROP  R1                                                               
*                                                                               
*======== AT THIS POINT, TOTABLE LISTS ALL OF THE 'TO' DETAILS                  
*                                                                               
         EJECT                                                                  
*                                                                               
VR100    DS    0H                                                               
*                                                                               
         MVC   TMPSTTN,FRSTTN       MOVE IN "FROM" STATION                      
         XC    TMPINVN,TMPINVN                                                  
         XC    TMPBDATE,TMPBDATE                                                
         XC    TMPKSRC,TMPKSRC                                                  
         XC    TMPBOOK,TMPBOOK                                                  
         MVI   GOSUBN,BIK#          BUILD START KEY TO INVENTORY RECORD         
         GOTO1 AGOSUB                                                           
         MVC   STARTKEY,KEY         AND HOLD ONTO IT                            
*                                                                               
VRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                        PRINT REPORT ROUTINE                         *         
*---------------------------------------------------------------------*         
* Will copy inventory items from one station to another in the offline          
*  environment.  Reports successes and failures of copying.                     
*                                                                               
PREP     DS    0H                                                               
         CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   PRX                                                              
*                                                                               
         DS    0H                  REPORT INITIALIZATION                        
         LA    R0,HEDSPECS                                                      
         ST    R0,SPECS                                                         
         LA    R0,HDHOOK                                                        
         ST    R0,HEADHOOK                                                      
*                                                                               
         DS    0H                  INITIALIZE COUNTERS                          
         SR    R0,R0                                                            
         ST    R0,CNTCOPY                                                       
         ST    R0,CNTNCPY                                                       
*                                                                               
         XC    PRVFTKEY,PRVFTKEY   NO PREVIOUS "FROM-TABLE" KEY YET             
*                                                                               
         MVI   ACTELOPT,C'N'       DON'T MONITOR ACTIVITY                       
*                                                                               
*--------------------- BUILD 'FROM' TABLE --------------------------*           
*                                                                               
PR050    DS    0H                                                               
         CLI   STARTKEY,XFF        IF NO MORE "FROM" RECORDS,                   
         BE    PR600               WE'RE DONE                                   
*                                                                               
         L     R4,AFROMTAB                                                      
         USING FROMTABD,R4                                                      
         MVI   COUNTER,0                                                        
*                                                                               
         LR    R0,R4                                                            
         LA    R1,FROMTABX-FROMTAB                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR "FROM" TABLE                           
*                                                                               
         DS    0H                                                               
         MVC   KEY,STARTKEY                                                     
         GOTO1 HIGH                GET FIRST 'FROM' STATION RECORD              
         B     PR060                                                            
*                                                                               
PR055    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PR060    DS    0H                                                               
         CLC   KEY(IKYSTAL),KEYSAVE        SAME 'FROM' STATION?                 
         BE    *+12                         YES                                 
         MVI   STARTKEY,XFF                                                     
         B     PR200                                                            
*                                                                               
*------------- RUN INVENTORY RECORD THROUGH FILTERS -----------------*          
*                                                                               
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
*                                                                               
         MVC   TMPREP,RINVKREP     REP CODE                                     
*                                                                               
         CLI   RINVKSRC,C'M'       MASTER RECORD?                               
         BE    PR055                                                            
         CLI   RINVKSRC,C'S'       STATION RECORD?                              
         BE    PR055                                                            
         CLI   RINVKSRC,C'Z'       RATE RECORD?                                 
         BE    PR055                                                            
*                                                                               
PR070    DS    0H                  INVENTORY NUMBER FILTER                      
         LA    RF,FRINVTB                                                       
*                                                                               
         LA    R2,ITCFINVH                                                      
         CLC   RINVKINV,ITCFINV    SAME INVENTORY NUMBER?                       
         BNE   PR055                                                            
*                                                                               
PR072    DS    0H                  EFFECTIVE DATE FILTER                        
         OC    FREFFDB,FREFFDB      IF NO EFFECTIVE DATE FILTER,                
         BZ    PR090                 LET THIS PASS                              
*                                                                               
         CLC   RINVKSTD,FRSEFFDB    CHECK IF LESS THAN START                    
         BL    PR055                 YES, FAILED FILTER                         
         CLC   RINVKSTD,FREEFFDB    CHECK IF GREATER THAN END                   
         BH    PR055                 YES, FAILED FILTER                         
*                                                                               
PR090    DS    0H                                                               
         CLI   RINVKSRC,X00        HEADER RECORD?                               
         BE    PR055                                                            
*                                                                               
         CLI   FRCPYTXT,C'N'        COPY TEXT RECORDS?                          
         BNE   PR092                                                            
         CLI   RINVKSRC,XFF         NO                                          
         BE    PR055                                                            
         B     PR095                                                            
*                                                                               
PR092    CLI   RINVKSRC,XFF         YES - ADD RECORD IN FROM TABLE              
         BE    PR100                                                            
*                                                                               
PR095    DS    0H                  TRACK FILTERS                                
*                                                                               
         TM    MYFLAG,NOTRKS       DON'T COPY ANY TRACKS?                       
         BO    PR055               DON'T                                        
*                                                                               
         TM    MYFLAG,ALLTRKS      COPY ALL TRACKS?                             
         BZ    PR095A              NO                                           
*                                                                               
         XC    INBLOCK,INBLOCK     CALL GETKSRC TO FIND MATCH                   
         XC    OTBLOCK,OTBLOCK     FOR ALL OF THE SAME SOURCE                   
         MVC   INBLOCK+2(1),RINVKSRC                                            
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'K',INBLOCK),OTBLOCK                             
         CLI   DMCB+4,0                                                         
         BNE   INVLFLD                                                          
*                                                                               
         CLC   TRKSRCF,OTBLOCK      SAME SOURCE?                                
         BNE   PR055                                                            
         B     PR100                                                            
*                                                                               
PR095A   CLI   ITCTRKSH+5,0        FILTER BY TRACKS?                            
         BE    PR100               NO                                           
*                                                                               
         LA    R5,TRCKTBL          TABLE OF TRACK FILTERS                       
         ZIC   R3,TRCKCNT          # OF TRACKS ENTERED                          
*                                                                               
PR095B   DS    0H                                                               
*                                                                               
         XC    INBLOCK,INBLOCK     CALL GETKSRC TO FIND MATCH                   
         XC    OTBLOCK,OTBLOCK     FOR ALL OF THE SAME SOURCE                   
         MVC   INBLOCK+2(1),RINVKSRC                                            
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'K',INBLOCK),OTBLOCK                             
         CLI   DMCB+4,0                                                         
         BNE   INVLFLD                                                          
*                                                                               
         CLC   TRKSRCF,OTBLOCK     SAME SOURCE AS FILTER?                       
         BNE   PR096                                                            
         CLI   6(R5),ONEBOOK       ONLY ONE BOOK FILTER HERE?                   
         BNE   *+14                                                             
         CLC   5(1,R5),OTBLOCK+1                                                
         BNE   PR096                                                            
*                                                                               
         CLC   RINVKBK,1(R5)       < START DATE?                                
         BL    PR096                YES                                         
         CLC   RINVKBK,3(R5)       > END DATE?                                  
         BH    PR096                YES                                         
         B     PR100                                                            
*                                                                               
PR096    LA    R5,L'TRCKTBL(R5)                                                 
         BCT   R3,PR095B                                                        
         B     PR055                                                            
         DROP  R6                                                               
*                                                                               
PR100    DS    0H                  ADD "FROM" RECD TO FROM-TABLE                
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
         MVC   FTKEY(FTKEYL),RINVKINV                                           
         DROP  R6                                                               
*                                                                               
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER          INCREMENT # OF ENTRIES COUNTER               
                                                                                
         LA    R4,FROMTABL(R4)     BUMP TO NEXT AVAILABLE SLOT                  
                                                                                
         B     PR055               GET THE NEXT "FROM" RECORD                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*---------------- PROCESS ENTRIES IN "FROM" TABLE -------------------*          
*                                                                               
* Process "FROM" TABLE by going through its entries and note the status         
*  of their "TO" counterparts on the file, i.e. whether they already            
*  exist or not, or if they are marked for deletion, etc.                       
                                                                                
PR200    DS    0H                                                               
         MVC   NFTNTRYS,COUNTER    NFTNTRYS = # OF "FROM" ENTRIES               
         MVI   COUNTER,0           RESET COUNTER                                
*                                                                               
         SR    RF,RF                                                            
         LA    RF,5                MAX # OF 'TO' STATION ENTRIES                
         LA    R1,TOTABLE          'TO' STATION TABLE                           
         ST    R1,TOADDR           A(TOTABLE) ENTRY                             
*                                                                               
PR210    DS    0H                                                               
         STC   RF,TOMAX            # OF REMAINING 'TO' STATIONS                 
         L     R1,TOADDR           'TO' STATION TABLE ENTRY                     
         USING TOTABLED,R1                                                      
*                                                                               
         BAS   RE,CLRFRFLG         CLEAR ALL FLAGS IN FROMTABLE                 
*                                                                               
         OC    0(TOTABLEL,R1),0(R1)  ANY 'TO' DETAILS IN THIS ENTRY?            
         BZ    PRX                    NO                                        
*                                                                               
         MVC   TOSTTN,TOSTA        GLOBAL 'TO' STATION VARIABLE                 
         MVC   TMPINV,TOINV        GLOBAL 'TO' INV #                            
         MVC   TMPSTD,TOSTD        GLOBAL 'TO' EFFECTIVE DATE                   
         EJECT                                                                  
*                                                                               
*-------------- CREATE "TO" RECORDS AND ADD TO FILE -----------------*          
*                                                                               
PR300    DS    0H                                                               
         L     R4,AFROMTAB                                                      
         USING FROMTABD,R4                                                      
*                                                                               
         BAS   RE,COPYREC          ADD RECORDS FOR "TO" STATION                 
*                                                                               
         L     R4,AFROMTAB                                                      
         LA    R5,P                                                             
         USING PRTLINED,R5                                                      
*                                                                               
         BAS   RE,PRNTHEAD                                                      
*                                                                               
         MVI   COUNTER,0                                                        
*                                                                               
PR320    DS    0H                  MOVE STUFF ONTO PRINT LINE                   
         CLC   COUNTER,NFTNTRYS                                                 
         BNL   PR500                                                            
*                                                                               
         DS    0H                  SEPARATE DIFFERENT STTN/INV#/EFFDATE         
         OC    PRVFTKEY,PRVFTKEY    IS THIS THE FIRST ONE?                      
         BZ    PR325X                YES, NO SEPARATION NEEDED                  
         CLC   FTINV,PRVFTKEY+(FTINV-FROMTABD)                                  
         BNE   PR325P                                                           
         CLC   FTSTD,PRVFTKEY+(FTSTD-FROMTABD)                                  
         BNE   PR325P                                                           
         B     PR325X                                                           
PR325P   GOTO1 SPOOL,DMCB,SPOOLD    PRINT BLANK LINE                            
PR325X   EQU   *                                                                
*                                                                               
         DS    0H                  MOVE KEY OF "TO" RECORD                      
         LA    R2,PLKEY                                                         
*&&DO                                                                           
*^^TEST                                                                         
         GOTO1 HEXOUT,DMCB,FTKEY,PRTLINED,FTKEYL,=C'TOG'                        
         GOTO1 SPOOL,DMCB,SPOOLD                                                
*^^EOTEST                                                                       
*&&                                                                             
*                                                                               
         L     R1,TOADDR                                                        
         USING TOTABLED,R1                                                      
*                                                                               
         MVC   0(4,R2),TOSTTN       STATION                                     
         LA    R2,4(R2)                                                         
*                                                                               
         CLI   TOSTTN+4,C'T'                                                    
         BE    *+18                                                             
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),TOSTTN+4     STATION'S SATELLITE                         
         LA    R2,2(R2)                                                         
*                                                                               
         BAS   RE,INSCOMMA                                                      
*                                                                               
         LA    RF,TMPINV           INVENTORY NUMBER                             
         LA    R1,4                                                             
*                                                                               
PR330    CLI   0(RF),X'80'         VALID EBCDIC CHARACTER?                      
         BL    PR335                                                            
         MVC   0(1,R2),0(RF)        YES                                         
         B     *+8                                                              
PR335    MVI   0(R2),C' '           NO - MOVE IN A SPACE                        
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,PR330                                                         
         BAS   RE,INSCOMMA                                                      
*                                                                               
PR340    MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(X'83',TMPSTD),(5,WORK),0                            
         ZIC   R1,DMCB+4                                                        
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK        EFFECTIVE DATE                              
         MVC   TOEFFDE,WORK         EBCDIC EFFEC. DATE                          
         LA    R2,1(R2,R1)                                                      
*                                                                               
PR345    CLI   FTKSRC,0            CHECK IF "TO" RECORD IS A HEADER,            
         BE    PR360                                                            
         CLI   FTKSRC,XFF           TEXT RECORD,                                
         BE    PR365                                                            
         CLI   FTKSRC,C'M'          MARKET FACT,                                
         BE    PR370                                                            
         CLI   FTKSRC,C'S'          OR A STATION FACT                           
         BE    PR375                                                            
*                                                                               
         DS    0H                  ELSE, IT IS A TRACK RECORD                   
         BAS   RE,INSCOMMA                                                      
*                                                                               
         L     R3,ASVCTAB                                                       
PR355    CLI   0(R3),XFF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   2(1,R3),FTKSRC                                                   
         BE    *+12                                                             
         LA    R3,L'SVCLST(R3)                                                  
         B     PR355                                                            
*                                                                               
         MVI   OURBYTE,1                                                        
         LA    R0,0(R3)                                                         
         MVI   GOSUBN,VSRC#                                                     
         GOTO1 AGOSUB              TMPSRC = 3-CHAR RTG SVCE ON RETURN           
         MVC   0(3,R2),TMPSRC                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         BAS   RE,INSCOMMA                                                      
         CLI   1(R3),C' '                                                       
         BE    *+14                                                             
         MVC   0(1,R2),1(R3)                                                    
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   TMPBOOK,FTBK_TXT                                                 
         MVI   TMPBTYP,0                                                        
         MVI   TMPWKN,0                                                         
         MVI   GOSUBN,TBK#                                                      
         GOTO1 AGOSUB                                                           
         ZIC   R1,OURBYTE                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK                                                    
         B     PR379                                                            
*                                                                               
PR360    DS    0H                  "TO" RECORD IS AN INV HEADER                 
         BAS   RE,INSCOMMA                                                      
         MVC   0(8,R2),=C'(HEADER)'                                             
         LA    R2,8(R2)                                                         
         B     PR379                                                            
*                                                                               
PR365    DS    0H                  "TO" RECORD IS A TEXT RECORD                 
         BAS   RE,INSCOMMA                                                      
         MVC   0(9,R2),=C'TXT LIN# '                                            
         LA    R2,9(R2)                                                         
         ZICM  R1,FTBK_TXT,(3)                                                  
         EDIT  (R1),(6,(R2)),ALIGN=LEFT,ZERO=NOBLANK,COMMAS=YES                 
         AR    R2,R0                                                            
         B     PR379                                                            
*                                                                               
PR370    DS    0H                  "TO" RECORD IS A MARKET FACT                 
         BAS   RE,INSCOMMA                                                      
         MVC   0(10,R2),=C'(MKT TEXT)'                                          
         LA    R2,11(R2)                                                        
         ZICM  R1,FTBK_TXT,(3)                                                  
         EDIT  (R1),(6,(R2)),ALIGN=LEFT,ZERO=NOBLANK,COMMAS=YES                 
         AR    R2,R0                                                            
         B     PR379                                                            
*                                                                               
PR375    DS    0H                  "TO" RECORD IS A STATION FACT                
         BAS   RE,INSCOMMA                                                      
         MVC   0(11,R2),=C'(STTN TEXT)'                                         
         LA    R2,12(R2)                                                        
         ZICM  R1,FTBK_TXT,(3)                                                  
         EDIT  (R1),(6,(R2)),ALIGN=LEFT,ZERO=NOBLANK,COMMAS=YES                 
         AR    R2,R0                                                            
         B     PR379                                                            
*                                                                               
PR379    DS    0H                                                               
         MVC   PLSIGN,=CL3'==>'                                                 
*                                                                               
         TM    FTFLAG1,FTF1HEAD    HEADER RECORD ALREADY EXIST?                 
         BO    PR420               YES                                          
         TM    FTFLAG1,FTF1NOHD    HEADER RECORD DOESN'T EXIST?                 
         BO    PR430               YES                                          
         TM    FTFLAG1,FTF1RDEL+FTF1RXST                                        
         BO    PR400                                                            
         TM    FTFLAG1,FTF1RXST                                                 
         BO    PR410                                                            
*                                                                               
         MVC   PLSTATUS,=CL16'RECORD  ADDED - '                                 
         MVC   PLCOMMNT,=CL25'COPY SUCCESSFUL'                                  
         LA    R1,1                                                             
         A     R1,CNTCOPY                                                       
         ST    R1,CNTCOPY                                                       
         B     PR450                                                            
*                                                                               
PR400    DS    0H                                                               
         MVC   PLSTATUS,=CL16'NOT COPIED    - '                                 
         MVC   PLCOMMNT,=CL25'RECORD CURRENTLY DELETED'                         
         LA    R1,1                                                             
         A     R1,CNTCOPY                                                       
         ST    R1,CNTCOPY                                                       
         B     PR450                                                            
*                                                                               
PR410    DS    0H                                                               
         MVC   PLSTATUS,=CL16'NOT  COPIED   - '                                 
         MVC   PLCOMMNT,=CL25'"TO" RECORD ALREADY EXIST'                        
         LA    R1,1                                                             
         A     R1,CNTNCPY                                                       
         ST    R1,CNTNCPY                                                       
         B     PR450                                                            
*                                                                               
PR420    DS    0H                                                               
         MVC   PLSTATUS,=CL16'NOT  COPIED   - '                                 
         MVC   PLCOMMNT,=CL25'HEADER ALREADY EXISTS'                            
         LA    R1,1                                                             
         A     R1,CNTNCPY                                                       
         ST    R1,CNTNCPY                                                       
         B     PR450                                                            
*                                                                               
PR430    DS    0H                                                               
         MVC   PLSTATUS,=CL16'NOT  COPIED   - '                                 
         MVC   PLCOMMNT,=CL25'HEADER DOES NOT EXIST'                            
         LA    R1,1                                                             
         A     R1,CNTNCPY                                                       
         ST    R1,CNTNCPY                                                       
         B     PR450                                                            
*                                                                               
PR450    DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         DROP  R5                                                               
*                                                                               
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER          INCREMENT COUNTER                            
*                                                                               
         MVC   PRVFTKEY,FTKEY      SAVE KEY OF THIS "FROM-TABLE" ENTRY          
         LA    R4,FROMTABL(R4)     BUMP "FROM" TABLE POINTER                    
         B     PR320                                                            
*                                                                               
PR500    DS    0H                                                               
         B     PR050                                                            
*                                                                               
PR600    DS    0H                                                               
*                                                                               
* WERE ANY RECORDS ACTUALLY SELECTED TO BE COPIED?                              
         OC    CNTCOPY,CNTCOPY                                                  
         BNZ   PR650               YES                                          
         OC    CNTNCPY,CNTNCPY                                                  
         BNZ   PR650               YES                                          
*                                                                               
         CLI   FRCPYTXT,C'Y'       COPY TEXT RECORDS MARKED YES?                
         BNE   PR625               NO                                           
         MVC   P2(33),=C'THERE ARE NO TEXT RECORDS TO COPY'                     
         MVI   P3,0                                                             
         B     PR660                                                            
*                                                                               
PR625    MVC   P2(38),=C'NO RECORDS WERE SELECTED TO GET COPIED'                
         MVI   P3,0                                                             
         B     PR660                                                            
*                                                                               
PR650    DS    0H                                                               
         MVC   P2(37),=C'NUMBER OF RECORDS COPIED SUCCESSFULLY'                 
         L     R1,CNTCOPY                                                       
         EDIT  (R1),(8,P2+40),ZERO=NOBLANK,COMMAS=YES                           
*                                                                               
         MVC   P3(37),=C'NUMBER OF UNSUCCESSFUL COPYS         '                 
         L     R1,CNTNCPY                                                       
         EDIT  (R1),(8,P3+40),ZERO=NOBLANK,COMMAS=YES                           
*                                                                               
PR660    MVI   P1,0                                                             
         MVI   P4,0                                                             
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         GOTO1 SPOOL,DMCB,SPOOLD                                                
*                                                                               
         XC    CNTCOPY,CNTCOPY     CLEAR COUNTERS                               
         XC    CNTNCPY,CNTNCPY                                                  
*                                                                               
         L     R1,TOADDR                                                        
         USING TOTABLED,R1                                                      
         LA    R1,TOTABLEL(R1)     NEXT 'TO' STATION ENTRY                      
         ST    R1,TOADDR                                                        
*                                                                               
         ZIC   RF,TOMAX            MAX # OF 'TO' STATION ENTRIES                
         MVI   COUNTER,0                                                        
         BCT   RF,PR210                                                         
*                                                                               
PRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                      BUMP TO NEXT FIELD ON SCREEN                   *         
*---------------------------------------------------------------------*         
BMPFLD   DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
*---------------------------------------------------------------------*         
*                      CLEAR ALL FLAGS IN FROMTABLE                   *         
*---------------------------------------------------------------------*         
CLRFRFLG NTR1                                                                   
*                                                                               
         L     R4,AFROMTAB                                                      
         USING FROMTABD,R4                                                      
*                                                                               
         CLI   NFTNTRYS,0          ANYTHING IN FROM TABLE?                      
         BE    CLRFRX              NO                                           
         ZIC   RF,NFTNTRYS         # OF 'FROM' RECORDS                          
*                                                                               
CLRFR10  XC    FTFLAG1,FTFLAG1     CLEAR FLAGS                                  
*        TM    TOFLAG1,TOF1EFFD    DIFFERENT EFFECTIVE DATES?                   
*        BZ    *+8                 NO                                           
*        OI    FTFLAG1,FTF1EFFD                                                 
*                                                                               
         LA    R4,FROMTABL(R4)     NEXT 'FROM' RECORD                           
         BCT   RF,CLRFR10                                                       
*                                                                               
CLRFRX   B     XIT                                                              
         DROP  R4                                                               
*---------------------------------------------------------------------*         
*                      COPY RECORD                                    *         
*---------------------------------------------------------------------*         
COPYREC  NTR1                                                                   
         L     R4,AFROMTAB                                                      
         USING FROMTABD,R4                                                      
*                                                                               
         CLI   NFTNTRYS,0          ANY RECORDS IN FROM TABLE                    
         BE    COPYRECX            NO RECORDS TO COPY                           
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    COUNTER,COUNTER                                                  
*                                                                               
CR5      DS    0H                                                               
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
         XC    KEY,KEY                                                          
         XC    TEMPKEY,TEMPKEY                                                  
*                                                                               
         MVI   RINVKTYP,X'12'      RECORD TYPE                                  
         MVC   RINVKREP,TMPREP     'FROM' REP                                   
         MVC   RINVKSTA,TOSTTN     "TO" STATION                                 
         MVC   RINVKINV,TMPINV     'TO' INVENTORY                               
         MVC   RINVKSTD,TMPSTD     'TO' EFFEC. DATE                             
         MVC   RINVKSRC,FTKSRC     'FROM' KEY SOURCE                            
         MVC   RINVKBK,FTBK_TXT    'FROM' BOOK OR TEXT NUMBER                   
         MVC   TEMPKEY,KEY                                                      
*                                                                               
         OI    DMINBTS,X08                                                      
         GOTO1 HIGH                DOES 'TO' RECORD ALREADY EXIST?              
         NI    DMINBTS,XFF-X08                                                  
*                                                                               
         CLC   KEY(L'RINVKEY),KEYSAVE                                           
         BNE   CR10                                                             
         CLI   RINVKEY+L'RINVKEY,X80    RECORD MARKED FOR DELETION?             
         BNE   *+12                                                             
*                                                                               
         OI    FTFLAG1,FTF1RDEL         YES                                     
         B     CR30                                                             
         OI    FTFLAG1,FTF1RXST         DON'T ADD RECORD                        
         B     CR30                                                             
*                                                                               
CR10     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'TEMPKEY),TEMPKEY  RESTORE BUILT KEY                        
         MVC   RINVKSTA,FRSTTN     'FROM' STATION                               
         MVC   RINVKINV,FTINV      'FROM' INVENTORY NUMBER                      
         MVC   RINVKSTD,FTSTD      'FROM' EFFEC. DATE                           
*                                                                               
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         XC    FROMKEY,FROMKEY                                                  
         MVC   FROMKEY,KEY         SAVE 'FROM' KEY                              
*                                                                               
         L     R6,AIO                                                           
*                                                                               
*=================UPDATE ELEMENTS===========================                    
*                                                                               
         BAS   RE,BUILD75          BUILD THE X'75' ELEMENT                      
         BAS   RE,CHNGEF           CHANGE THE X'EF' ELEMENT                     
*                                                                               
         CLI   RINVKSRC,XFF        TEXT RECORD?                                 
         BE    CR15                                                             
*                                                                               
         BAS   RE,UPELEM03         MARK THE X'03' ELEMENT AS COPIED             
*                                                                               
*===========================================================                    
CR15     MVC   RINVKSTA,TOSTTN     "TO" STATION                                 
         MVC   RINVKINV,TMPINV     'TO' INVENTORY                               
         MVC   RINVKSTD,TMPSTD     'TO' EFFEC. DATE                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RINVKEY),0(R6)   'TO' RECORD KEY                           
         LA    R6,KEY                                                           
*                                                                               
*======================== CHECK IF HEADER OF 'TO' REC EXISTS                    
*                                                                               
         MVI   RINVKSRC,X00        HEADER RECORD                                
         XC    RINVKBK,RINVKBK                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RINVKEY),KEYSAVE                                           
         BE    CR18                                                             
         OI    FTFLAG1,FTF1NOHD    HEADER DOES NOT EXIST                        
         B     CR30                                                             
*                                                                               
CR18     DS    0H                                                               
         L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'RINVKEY),0(R6)   'TO' RECORD KEY                           
         NI    DMINBTS,X'FF'-X'40' AIO NOT A 31 BIT ADDRESS                     
*                                                                               
*!!      GOTO1 ADDREC              GO AND ADD NEW "TO" STATION RECORD           
*                                                                               
CR30     LA    R4,FROMTABL(R4)      BUMP TO NEXT "FROM" ENTRY                   
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER           INCREMENT LOOP COUNTER                      
         CLC   COUNTER,NFTNTRYS     WENT THROUGH ALL "FROM" ENTRIES?            
         BL    CR5                   YES, GO ADD "TO" RECORDS                   
*                                                                               
COPYRECX DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                     BUILD THE X'75' ELEMENT                         *         
*---------------------------------------------------------------------*         
BUILD75  NTR1                                                                   
*                                                                               
         L     R6,AIO              'TO' RECORD TO BE ADDED                      
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'75'                                                     
         BAS   RE,GETEL            GET X'75' ELEMENT                            
         BNE   BLD7505             X'75' ELEM DOESN'T EXIST - GO ADD            
*                                                                               
         GOTO1 =V(RECUP),DMCB,(X'02',(R6)),(R3)   REMOVE X'75' ELEM             
*                                                                               
BLD7505  DS    0H                  GO AHEAD AND BUILD THE X'75' ELEM            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING RICPELEM,R3                                                      
*                                                                               
         MVI   RICPCODE,X'75'      ELEMENT CODE X'75'                           
         MVI   RICPLEN,EL75LEN     ELEMENT LENGTH (32)                          
         MVI   RICPFRPR,C'T'       T=TCOPY                                      
         MVC   RICPFRKY,FROMKEY    'FROM' KEY                                   
*                                                                               
         LH    R3,L'RINVKEY(R6)    RECORD LENGTH                                
         AR    R3,R6               A(1ST AVAIL. ELEMENT POSITION)               
*                                                                               
         GOTO1 =V(RECUP),DMCB,(X'02',(R6)),ELEM,(C'R',(R3))  ADD ELEM           
*                                                                               
BLD75X   DS    0H                                                               
         B     XIT                                                              
         GETEL R3,DATADISP,ELCODE                                               
         DROP  R3                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                     CHANGE THE X'EF' ELEMENT                        *         
*---------------------------------------------------------------------*         
CHNGEF   NTR1                                                                   
*                                                                               
         L     R6,AIO                                                           
         USING RINVREC,R6                                                       
         L     R3,AIO                                                           
*                                                                               
         MVI   ELCODE,X'EF'                                                     
         BAS   RE,GETEL            GET THE X'EF' ELEMENT                        
         BNE   RCDNTFND            X'EF' ELEM DOESN'T EXIST - ERROR             
         USING RINVAEL,R3          ACTIVITY ELEMENT                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,RINVAFST) TODAY'S DATE                   
*                                                                               
         MVC   RINVALST,RINVAFST   SAME FIRST AND LAST ACT. DATE                
*                                                                               
CHGEFX   DS    0H                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*             MARK THE X'03' TRACK ELEMENT COPIED                     *         
*---------------------------------------------------------------------*         
UPELEM03 NTR1                                                                   
*                                                                               
         L     R6,AIO              'TO' RECORD TO BE ADDED                      
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL            GET X'03' ELEMENT                            
         BNE   RCDNTFND            X'03' ELEM DOESN'T EXIST - ERROR             
         USING RINVFREL,R3         TRANSFER FROM ELEMENT                        
*                                                                               
         MVI   RINVFRPR,C'K'       MARK AS COPIED BY TCOPY                      
*                                                                               
UPEL03X  DS    0H                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                     PERVAL CALL FOR EFF. DATE                       *         
*---------------------------------------------------------------------*         
GETEFFD  NTR1                                                                   
         XC    PERVALB,PERVALB                                                  
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),PERVALB                                
         CLI   DMCB+4,0                                                         
         BE    GETEFF5                                                          
         TM    DMCB+4,X04          "ONLY ONE DATE INPUT" IS OKAY                
         BO    GETEFF5                                                          
         B     INVLFLD                                                          
*                                                                               
GETEFF5  DS    0H                                                               
         LA    RF,PERVALB                                                       
         USING PERVALD,RF                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY                                 
         BNZ   INVLFLD                                                          
         TM    PVALASSM,PVALAED+PVALAEM+PVALAEY                                 
         BM    INVLFLD                                                          
*                                                                               
         LA    RF,PERVALB                                                       
         USING PERVALD,RF                                                       
         MVC   PERBSTA,PVALBSTA    BIN YYMMDD START OF PERIOD                   
         MVC   PERBEND,PVALBEND    BIN YYMMDD END OF PERIOD                     
*                                                                               
GETEFFX  B     XIT                                                              
         DROP  RF                                                               
*---------------------------------------------------------------------*         
*                      INSERT COMMA ONTO PRINT LINE                   *         
*---------------------------------------------------------------------*         
INSCOMMA DS    0H                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BR    RE                                                               
*---------------------------------------------------------------------*         
*                      TEST IF KEY CHANGED                            *         
*---------------------------------------------------------------------*         
KYCHNGED DS    0H                                                               
         TM    4(R2),X20                                                        
         BZ    KYCH10                                                           
         TM    4(R2),X80                                                        
         BZR   RE                                                               
KYCH10   OI    CHNGFLG1,CF1KEY                                                  
         BR    RE                                                               
*--------------------------------------------------------------------*          
*                  PRINT FROM AND TO INFO                            *          
*--------------------------------------------------------------------*          
PRNTHEAD NTR1                                                                   
*                                                                               
         LA    R2,P                                                             
         USING PRTHEAD,R2                                                       
         L     R1,TOADDR                                                        
         USING TOTABLED,R1                                                      
*                                                                               
         MVC   PHSOURCE,=C'FROM RECORD '                                        
         LA    R2,L'PHSOURCE(R2)                                                
*                                                                               
         MVC   0(4,R2),FRSTTN       STATION                                     
         LA    R2,4(R2)                                                         
*                                                                               
         CLI   FRSTTN+4,C'T'                                                    
         BE    *+18                                                             
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),FRSTTN+4     STATION'S SATELLITE                         
         LA    R2,2(R2)                                                         
         BAS   RE,INSCOMMA                                                      
*                                                                               
         MVC   PHKINVN,ITCFINV     'FROM' INV. #                                
         LA    R2,L'PHKINVN(R2)                                                 
         BAS   RE,INSCOMMA                                                      
*                                                                               
         MVC   PHKEFFD,ITCFEFD     'FROM' EFFECTIVE DATE                        
         GOTO1 SPOOL,DMCB,SPOOLD    PRINT LINE                                  
*                                                                               
         LA    R2,P                                                             
         MVC   PHSOURCE,=C'TO RECORD   '                                        
         LA    R2,L'PHSOURCE(R2)                                                
*                                                                               
         MVC   0(4,R2),TOSTTN       STATION                                     
         LA    R2,4(R2)                                                         
*                                                                               
         CLI   TOSTTN+4,C'T'                                                    
         BE    *+18                                                             
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),TOSTTN+4     STATION'S SATELLITE                         
         LA    R2,2(R2)                                                         
         BAS   RE,INSCOMMA                                                      
*                                                                               
         MVC   PHKINVN,TMPINV      'TO' INV. #                                  
         LA    R2,L'PHKINVN(R2)                                                 
         BAS   RE,INSCOMMA                                                      
*                                                                               
PRNTH10  MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(X'83',TMPSTD),(5,WORK),0                            
         ZIC   R1,DMCB+4                                                        
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK        EFFECTIVE DATE                              
         GOTO1 SPOOL,DMCB,SPOOLD    PRINT LINE                                  
         DROP  R1,R2                                                            
*                                                                               
PRNTHEDX DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD    PRINT BLANK LINE                            
         B     XIT                                                              
*---------------------------------------------------------------------*         
*                   BUILD TRACK TABLE                                 *         
*---------------------------------------------------------------------*         
TRACKS   NTR1                                                                   
*                                                                               
         LA    R2,ITCTRKSH         TRACKS                                       
         LA    R3,BUFF             TABLE OF TRACKS FROM SCREEN                  
         LA    R4,TRCKTBL          TABLE FOR TRACKS AFTER PERVAL                
         ZIC   R6,TRCKCNT          # OF TRACKS                                  
*                                                                               
TRCK10   DS    0H                                                               
         CLI   0(R3),0             ANY MORE BOOK FILTERS?                       
         BE    TRCKX               NO                                           
*                                                                               
         XC    BKVLHEAD,BKVLHEAD                                                
         XC    BTLST,BTLST                                                      
         XC    BKLST,BKLST                                                      
*                                                                               
         MVI   BKHEAD,BKVLHLQ      FIELD LENGTH = 31                            
*                                                                               
         ZIC   R1,0(R3)            LENGTH OF FIRST BOOK                         
         CLI   1(R3),0             BOOK RANGE?                                  
         BNE   *+14                                                             
         AR    R1,R1               R1 - LENGTH OF BOOKS FOR HEADER              
         MVI   6(R4),ONEBOOK        ONLY ONE BOOK FILTER HERE                   
         B     TRCK12                                                           
*                                                                               
         ZIC   RF,1(R3)            LENGTH OF 2ND BOOK                           
         AR    R1,RF               R1 CONTAINS LENGTH OF BOTH BOOKS             
*                                                                               
TRCK12   DS    0H                                                               
         LA    R1,3(R1)            ADD 2 FOR SOURCE AND 2 COMMAS                
         STC   R1,BKHEAD+5         INPUT DATA LENGTH                            
*                                                                               
         LA    R5,BKDATA                                                        
         MVC   0(1,R5),ITCSRCE     BOOK SOURCE                                  
         MVI   1(R5),C','          INSERT COMMA                                 
         LA    R5,2(R5)                                                         
*                                                                               
         ZIC   RF,0(R3)            LENGTH OF 1ST BOOK                           
         BCTR  RF,0                                                             
         EX    RF,*+8              MOVE 1ST BOOK INTO BOOKVAL HEADER            
         B     *+10                                                             
         MVC   0(0,R5),12(R3)                                                   
*                                                                               
         LA    RF,1(RF)                                                         
         AR    R5,RF               BUMP TO NEXT BOOK IN HEADER                  
         MVI   0(R5),C','          INSERT COMMA                                 
         LA    R5,1(R5)                                                         
*                                                                               
         CLI   1(R3),0             ANY 2ND BOOK INPUT?                          
         BNE   TRCK15                                                           
         ZIC   RF,0(R3)            NO - 1ST BOOK = 2ND BOOK                     
         BCTR  RF,0                                                             
         EX    RF,*+8              MOVE 1ST BOOK INTO BOOKVAL HEADER            
         B     *+10                                                             
         MVC   0(0,R5),12(R3)                                                   
         B     TRCK20                                                           
*                                                                               
TRCK15   DS    0H                                                               
         ZIC   RF,1(R3)            LENGTH OF 2ND BOOK                           
         BCTR  RF,0                                                             
         EX    RF,*+8              MOVE 1ST BOOK INTO BOOKVAL HEADER            
         B     *+10                                                             
         MVC   0(0,R5),22(R3)                                                   
*                                                                               
TRCK20   DS    0H                                                               
         GOTO1 BOOKVAL,DMCB,BKVLHEAD,(2,BKLST),(C'B',SCANNER),BTLST             
         CLI   DMCB+4,0            VALID BOOKVAL FIELDS?                        
         BE    INVLFLD                                                          
*                                                                               
         CLC   BKLST(1),BKLST+3    NO MIXING OF BOOK RANGES                     
         BNE   INVLFLD                                                          
         CLC   BTLST(1),BTLST+1    NO MIXING OF BOOK RANGES                     
         BNE   INVLFLD                                                          
*                                                                               
         XC    INBLOCK,INBLOCK                                                  
         XC    OTBLOCK,OTBLOCK                                                  
*                                                                               
         MVC   INBLOCK(1),BKDATA   BOOK SOURCE (RTG SERVICE)                    
         MVI   INBLOCK+1,C' '      INITIALLY A SPACE FOR NO QUALIFIER           
*                                                                               
         TM    BKLST,X'20'         ESTIMATED BOOK?                              
         BZ    *+12                                                             
         MVI   INBLOCK+1,C'E'      YES                                          
         B     TRCK25                                                           
*                                                                               
         TM    BKLST,X'04'         PROJECTED BOOK?                              
         BZ    *+12                                                             
         MVI   INBLOCK+1,C'P'      YES                                          
         B     TRCK25                                                           
*                                                                               
         TM    BKLST,X'08'         TIME PERIOD?                                 
         BZ    *+12                                                             
         MVI   INBLOCK+1,C'T'      YES                                          
         B     TRCK25                                                           
*                                                                               
         TM    BKLST,X'02'         SPECIAL SURVEY?                              
         BZ    *+12                                                             
         MVI   INBLOCK+1,C'S'      YES                                          
         B     TRCK25                                                           
*                                                                               
TRCK25   DS    0H                                                               
         MVC   INBLOCK+4(1),BTLST  BOOKTYPE (NULLS IF NO BOOKTYPE)              
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'Q',INBLOCK),OTBLOCK                             
         CLI   DMCB+4,0                                                         
         BNE   INVLFLD                                                          
*                                                                               
         MVC   0(1,R4),OTBLOCK+2   SOURCE IN RECORD                             
         MVC   1(2,R4),BKLST+1     FIRST BOOK                                   
         MVC   3(2,R4),BKLST+4     2ND BOOK                                     
         MVC   5(1,R4),OTBLOCK+1   BOOK QUALIFIER                               
*                                                                               
TRCK50   DS    0H                                                               
         LA    R4,L'TRCKTBL(R4)                                                 
         XC    0(L'TRCKTBL,R4),0(R4)                                            
         LA    R3,32(R3)                                                        
         BCT   R6,TRCK10                                                        
*                                                                               
TRCKX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                      ERROR MESSAGES                                 *         
*---------------------------------------------------------------------*         
MISSFLD  DS    0H                  MISSING INPUT FIELD ERROR                    
         MVI   OURERRCD,MFLDQ                                                   
         B     OURERROR                                                         
                                                                                
INVLFLD  DS    0H                  INVALID FIELD ERROR                          
         MVI   OURERRCD,IFLDQ                                                   
         B     OURERROR                                                         
                                                                                
RCDNTFND DS    0H                  RECORD NOT FOUND                             
         MVI   OURERRCD,RNFQ                                                    
         B     OURERROR                                                         
                                                                                
NSAMESTA DS    0H                  NO SAME STATION                              
         MVI   OURERRCD,NSSQ                                                    
         B     OURERROR                                                         
                                                                                
INVLBK   DS    0H                  INVALID BOOK                                 
         MVI   OURERRCD,IBKQ                                                    
         B     OURERROR                                                         
                                                                                
INVLFIL  DS    0H                  INVALID FILE                                 
         MVI   OURERRCD,IFILQ                                                   
         B     OURERROR                                                         
                                                                                
INVLSTA  DS    0H                  INVALID STATION                              
         MVI   OURERRCD,ISTAQ                                                   
         B     OURERROR                                                         
                                                                                
INVLDAY  DS    0H                  INVALID DAY                                  
         MVI   OURERRCD,IDAYQ                                                   
         B     OURERROR                                                         
                                                                                
INVLTIM  DS    0H                  INVALID DAY                                  
         MVI   OURERRCD,ITIMQ                                                   
         B     OURERROR                                                         
                                                                                
INVLDAT  DS    0H                  INVALID DATE                                 
         MVI   OURERRCD,IDATQ                                                   
         B     OURERROR                                                         
                                                                                
NORECSEL DS    0H                  NO RECORDS SELECTED FOR OCPY                 
         MVI   OURERRCD,NORECS                                                  
         B     OURERROR                                                         
                                                                                
NOHEAD   DS    0H                  NO HEADER RECORD FOR TO STATION              
         MVI   OURERRCD,NOTHEAD                                                 
         B     OURERROR                                                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                             REPORT SPECS                            *         
*---------------------------------------------------------------------*         
HEDSPECS SSPEC H1,1,C'INVENTORY TRACK-COPY'                                     
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'TCOPY REPORT'                                            
         SSPEC H2,52,C'------------'                                            
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         DC    X'00'                                                            
*---------------------------------------------------------------------*         
*                           HEADLINE ROUTINE                          *         
*---------------------------------------------------------------------*         
HDHOOK   NTR1                                                                   
         DS    0H                                                               
HDHOOKX  B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                      SUBROUTINE POOL INTERFACE                      *         
*---------------------------------------------------------------------*         
         DS    0H                                                               
GOSUB    NTR1  BASE=MYBASE1,LABEL=N                                             
         L     R9,MYBASE2                                                       
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'+GO'                                                  
         MVC   3(1,RE),GOSUBN                                                   
         SR    RE,RE                AND CLEAR  RE  JUST TO BE SAFE              
                                                                                
         MVC   ASUBRTN,ASUBR01                                                  
         CLI   GOSUBN,R01#                                                      
         BNH   GOSUBGO                                                          
         DC    H'0'                                                             
*                                                                               
GOSUBGO  DS    0H                                                               
         GOTO1 ASUBRTN,(RC)                                                     
*                                                                               
         DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
                                                                                
OURERROR DS    0H                                                               
         MVI   BYTE,C'E'                                                        
         B     XMSGGO                                                           
                                                                                
OURWARN  DS    0H                                                               
         MVI   BYTE,C'W'                                                        
         B     XMSGGO                                                           
                                                                                
OURINFO  DS    0H                                                               
         MVI   BYTE,C'I'                                                        
         B     XMSGGO                                                           
                                                                                
XMSGGO   DS    0H                                                               
         GOTO1 AXMSGRTN,DMCB,(BYTE,(RC))                                        
         B     XIT                                                              
                                                                                
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
                                                                                
                                                                                
DEMLSTDC DC    X'00D90000E20000D700FF'   DEMO LIST W/ DELIMITER                 
                                                                                
                                                                                
MAINL    EQU   *-RMP28                                                          
         DS    0CL(X'2000'-MAINL+1)                                             
***********************************************************************         
                                                                                
                                                                                
         DROP  R7,R8,R9,RA,RB,RC                                                
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
         ORG   RMP28+X'2000'                                                    
         ORG                                                                    
SUBR01Q  EQU   (((*-RMP28+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP28+SUBR01Q                                                    
SUBR01   NMOD1 0,**1801**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R8             R8=A(SYSD)                                   
         USING SPOOLD,R7           R7=A(SPOOL WORK AREA)                        
                                                                                
         ZIC   R1,GOSUBN                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
                                                                                
BIK#     EQU   (R01_01-*)/4+1      BUILD KEY TO INVENTORY RECORD                
GIR#     EQU   (R01_02-*)/4+1      GET INVENTORY RECORD                         
VSRC#    EQU   (R01_03-*)/4+1      VALIDATE SOURCE                              
TBK#     EQU   (R01_04-*)/4+1      TRANSLATE BOOK                               
                                                                                
R01_00   DS    0H                                                               
R01_01   B     BINVKEY             BUILD KEY TO INVENTORY RECORD                
R01_02   B     GINVRCD             GET INVENTORY RECORD                         
R01_03   B     VALSRC              VALIDATE SOURCE                              
R01_04   B     TRSLTBK             TRANSLATE BOOK                               
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    RC,RC                                                            
NO_01    LTR   RC,RC                                                            
XIT_01   XIT1                                                                   
*------------------------ BUILD INVENTORY KEY ------------------------*         
*                                                                               
BINVKEY  DS    0H                                                               
         XC    KEY,KEY                                                          
IVK      USING REINVRCD,KEY                                                     
         MVI   IVK.RINVKTYP,X'12'                                               
         MVC   IVK.RINVKREP,AGENCY                                              
         MVC   IVK.RINVKSTA,TMPSTTN                                             
         MVC   IVK.RINVKINV,TMPINVN                                             
         MVC   IVK.RINVKSTD,TMPBDATE                                            
         MVC   IVK.RINVKSRC,TMPKSRC                                             
         MVC   IVK.RINVKBK,TMPBOOK                                              
         DROP  IVK                                                              
         B     XIT_01                                                           
*------------------------ GET INVENTORY RECORD -----------------------*         
                                                                                
* Gets an inventory record.  An effective date may or may not be                
*  supplied.  If it is given, routine will look for the record with the         
*  latest effective date not after the given one.  If it is not given,          
*  routine will just look for the record with the latest effective              
*  date.  In either case, the rest of the key must match the given              
*  input.  Otherwise, "record not found" situation is returned.                 
                                                                                
GINVRCD  DS    0H                                                               
         PRINT OFF                                                              
*&&DO                                                                           
         XC    SAVEKEY,SAVEKEY                                                  
         ZICM  R1,IKEFFDB,(7)                                                   
         BNZ   *+6                                                              
         BCTR  R1,0                                                             
         STCM  R1,7,TMPBDATE       EFFECTIVE DATE FOR THIS ROUTINE              
                                                                                
         MVI   GOSUBN,BIK#         BUILD KEY                                    
         GOTO1 AGOSUB                                                           
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         XC    RINVKSTD,RINVKSTD   START W/ EARLIEST POSSIBLE EFF DATE          
*                                                                               
GIR010   DS    0H                                                               
         ZICM  R1,RINVKSTD,(7)     WANT LATEST EFFECTIVE DATE                   
         LA    R1,1(R1)                                                         
         CLM   R1,7,TMPBDATE        W/O SURPASSING INPUTTED DATE                
         BH    GIR050                                                           
         STCM  R1,7,RINVKSTD                                                    
         MVC   RINVKSRC,IKKSRC                                                  
         MVC   RINVKBK,IKBOOK                                                   
                                                                                
GIR015   DS    0H                                                               
         TM    MISCFLG1,MF1RDDEL   IF WE CAN READ DELETED RECORDS,              
         BZ    *+8                                                              
         OI    DMINBTS,X08          GO AHEAD AND DO IT                          
         GOTO1 HIGH                                                             
         NI    DMINBTS,XFF-X08      AND ALWAYS CLEAR FLAG AFTER I/O             
                                                                                
         CLC   RINVKEY(IKYINVL),KEYSAVE                                         
         BNE   GIR050                                                           
                                                                                
         CLC   RINVKSTD,TMPBDATE                                                
         BH    GIR050                                                           
         B     GIR020                                                           
*                                                                               
GIR020   DS    0H                  CHECK KEY SOURCE                             
         CLC   RINVKSRC,IKKSRC                                                  
         BH    GIR010                                                           
         BE    GIR030                                                           
                                                                                
         DS    0H                   RINVKSRC < IKKSRC                           
         MVC   RINVKSRC,IKKSRC                                                  
         MVC   RINVKBK,IKBOOK                                                   
         B     GIR015                                                           
*                                                                               
GIR030   DS    0H                  CHECK BOOK                                   
         CLC   RINVKBK,IKBOOK                                                   
         BH    GIR010                                                           
         BE    GIR040                                                           
                                                                                
         DS    0H                   RINVKBK < IKBOOK                            
         MVC   RINVKBK,IKBOOK                                                   
         B     GIR015                                                           
*                                                                               
GIR040   DS    0H                  KEY LOOKS GOOD                               
         MVC   SAVEKEY,RINVKEY      HOLD ONTO IT                                
         B     GIR010              GO BACK LOOK FOR LATER EFF DATE              
*                                                                               
GIR050   DS    0H                                                               
         OC    SAVEKEY,SAVEKEY     DID ANY KEY QUALIFY?                         
         BZ    GIRXN                NOPE, COULDN'T MATCH INPUT                  
                                                                                
         MVC   RINVKEY,SAVEKEY     USE THE LAST QUALIFIED KEY                   
         MVC   RDUPDATE,MYRDUPDT                                                
         TM    MISCFLG1,MF1RDDEL   IF WE CAN READ DELETED RECORDS,              
         BZ    *+8                                                              
         OI    DMINBTS,X08          GO AHEAD AND DO IT                          
         GOTO1 READ                                                             
         NI    DMINBTS,XFF-X08      AND ALWAYS CLEAR FLAG AFTER I/O             
                                                                                
         CLC   RINVKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    MISCFLG1,MF1RDDEL   IF WE CAN READ DELETED RECORDS,              
         BZ    *+8                                                              
         OI    DMINBTS,X08          GO AHEAD AND DO IT                          
         GOTO1 GETREC                                                           
         NI    DMINBTS,XFF-X08      AND ALWAYS CLEAR FLAG AFTER I/O             
                                                                                
         B     GIRXY                                                            
         DROP  R6                                                               
                                                                                
                                                                                
*&&                                                                             
         PRINT ON                                                               
GIRXN    DS    0H                                                               
         B     NO_01                                                            
GIRXY    DS    0H                                                               
         B     YES_01                                                           
*------------------- VALIDATE SOURCE (RATING SERVICE) ----------------*         
                                                                                
* Validates a field containing a variable-length source (rating srvce).         
* At entry,                                                                     
*   OURBYTE = length of data to validate,                                       
*   R0      = A(test specimen).                                                 
* At exit,                                                                      
*   TMPSRC = 3-char rating service code,                                        
*   CC set to equal if valid source,                                            
*   CC set to not equal if invalid source.                                      
                                                                                
VALSRC   DS    0H                                                               
         CLI   OURBYTE,3           IF LENGTH > MAX,                             
         BNH   *+6                                                              
         DC    H'0'                 SOMETHING'S INTERNALLY WRONG                
*                                                                               
         ZIC   R1,OURBYTE                                                       
         BCTR  R1,0                R1 = LEN FOR EX COMPARE                      
         LR    RE,R0               RE = A(TEST SPECIMEN)                        
*                                                                               
         MVC   TMPSRC,=C'NSI'      ASSUME NIELSEN                               
         EX    R1,VSRCCLC                                                       
         BE    VSRCXY                                                           
         MVC   TMPSRC,=C'MFX'      ASSUME MEDIAFAX                              
         EX    R1,VSRCCLC                                                       
         BE    VSRCXY                                                           
         MVC   TMPSRC,=C'SRC'      ASSUME STRATEGY                              
         EX    R1,VSRCCLC                                                       
         BE    VSRCXY                                                           
         MVC   TMPSRC,=C'ARB'      ASSUME ARBITRON                              
         EX    R1,VSRCCLC                                                       
         BE    VSRCXY                                                           
*                                                                               
         XC    TMPSRC,TMPSRC       NONE OF THE ABOVE                            
         B     VSRCXN               RETURN INVALID FLAG                         
                                                                                
*                                                                               
VSRCXY   DS    0H                                                               
         B     YES_01                                                           
                                                                                
VSRCXN   DS    0H                                                               
         B     NO_01                                                            
                                                                                
                                                                                
* Should not fall through and execute this                                      
                                                                                
VSRCCLC  CLC   0(0,RE),TMPSRC                                                   
*--------------------------- TRANSLATE BOOK --------------------------*         
* At entry,                                                                     
*   TMPBOOK = book to translate.                                                
*   TMPBTYP = book type, if applicable.                                         
*   TMPWKN  = week number, if applicable.                                       
* At exit,                                                                      
*   OURBYTE = L(converted book),                                                
*   WORK    = converted book.                                                   
*                                                                               
TRSLTBK  DS    0H                                                               
*                                                                               
         MVC   WORK,SPACES                                                      
         OC    TMPBOOK,TMPBOOK     IF BOOK IS NULLS,                            
         BZ    TBKX                 EXIT NOW                                    
*                                                                               
         LA    R2,WORK                                                          
         XC    DUB,DUB                                                          
         MVC   DUB(L'TMPBOOK),TMPBOOK                                           
*                                                                               
         CLI   FTBK_TXT+1,X00      EST BOOK?                                    
         BNE   TBK10                                                            
         EDIT  (1,FTBK_TXT),(2,2(R2))                                           
         MVC   0(2,R2),=C'ST'                                                   
         LA    R2,4(R2)                                                         
         B     TBK15                                                            
*                                                                               
TBK10    GOTO1 DATCON,DMCB,(X'83',DUB),(6,(R2))                                 
         MVC   3(2,R2),4(R2)       REMOVE SLASH                                 
         LA    R2,5(R2)                                                         
         MVI   0(R2),C' '                                                       
*                                                                               
TBK15    XC    INBLOCK,INBLOCK     GET BOOK TYPE                                
         XC    OTBLOCK,OTBLOCK                                                  
         MVC   INBLOCK+2(1),FTKSRC                                              
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'K',INBLOCK),OTBLOCK                             
*                                                                               
         OC    OTBLOCK+4(1),OTBLOCK+4      ANY BOOKTYPE?                        
         BZ    TBK029                                                           
         MVI   0(R2),C'('           TACK IT ON                                  
         MVC   1(1,R2),OTBLOCK+4                                                
         MVI   2(R2),C')'                                                       
         LA    R2,3(R2)                                                         
*                                                                               
TBK029   EQU   *                                                                
*                                                                               
         DS    0H                  FORMAT WEEK NUMBER                           
         CLI   TMPWKN,0                                                         
         BE    TBK039                                                           
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),TMPWKN                                                   
         OI    1(R2),X'F0'                                                      
         LA    R2,2(R2)                                                         
TBK039   EQU   *                                                                
*                                                                               
         LA    R0,WORK                                                          
         SR    R2,R0                                                            
         STC   R2,OURBYTE                                                       
*                                                                               
TBKX     DS    0H                                                               
         B     XIT_01                                                           
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
***********************************************************************         
*==================== EXIT WITH MESSAGE ROUTINES =====================*         
                                                                                
* Exits which leave RMP28 entirely and displays a message go through            
*  this routine.                                                                
* At entry,                                                                     
*   MYTEXT has length and text of text-replace.                                 
                                                                                
SUBRXMQ  EQU   (((*-RMP28+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP28+SUBRXMQ                                                    
XMSGRTN  NMOD1 0,**18XM**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
                                                                                
         MVC   MSGTYPE,0(R1)       GET MESSAGE TYPE                             
         XC    CONHEAD,CONHEAD     CLEAR THE WAY FOR THE MESSAGE                
                                                                                
         CLI   MSGTYPE,C'E'        EXIT W/ AN ERROR MSG                         
         BE    XMERR                                                            
         CLI   MSGTYPE,C'W'        EXIT W/ A WARNING MSG                        
         BE    XMWRN                                                            
         CLI   MSGTYPE,C'I'        EXIT W/ AN INFO  MSG                         
         BE    XMINF                                                            
         DC    H'0'                                                             
                                                                                
                                                                                
ALLMSGX  DS    0H                                                               
         DS    0H                                                               
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
                                                                                
                                                                                
XIT_XM   XIT1                                                                   
*--------------------------- ERROR MESSAGES --------------------------*         
                                                                                
* These messages require the user to re-input (the correct) data for a          
*  response.  Previous values are restored, if the key did not change,          
*  and fields modified this time around will appear modified in the             
*  next transaction.                                                            
* At entry, R2-->field to put cursor.                                           
                                                                                
XMERR    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         OI    MISCFLG1,MF1ERRQ                                                 
                                                                                
         DS    0H                                                               
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   OURERRCD,ERRX#                                                   
         BNH   XMERRGO                                                          
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         MVC   CONHEAD(9),=C'**ERROR**'                                         
         LA    R1,CONHEAD+10                                                    
         CLI   OURERRCD,ERRX2#                                                  
         BNH   XMERRGO                                                          
         DC    H'0'                                                             
                                                                                
XMERRGO  DS    0H                                                               
         CLI   OURERRCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURERRCD,XMERRQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,OURERRCD         BRANCH OFF TO SET ERROR MESSAGE              
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMERR00(RF)                                                      
                                                                                
MIXDQ    EQU   633                                                              
MFLDQ    EQU   ((XMERR01-XMERR00)/4)+1                                          
IFLDQ    EQU   ((XMERR02-XMERR00)/4)+1                                          
RNFQ     EQU   ((XMERR03-XMERR00)/4)+1                                          
ISELQ    EQU   ((XMERR04-XMERR00)/4)+1                                          
NSSQ     EQU   ((XMERR05-XMERR00)/4)+1                                          
IBKQ     EQU   ((XMERR06-XMERR00)/4)+1                                          
IFILQ    EQU   ((XMERR07-XMERR00)/4)+1                                          
ISTAQ    EQU   ((XMERR08-XMERR00)/4)+1                                          
IDAYQ    EQU   ((XMERR09-XMERR00)/4)+1                                          
ITIMQ    EQU   ((XMERR10-XMERR00)/4)+1                                          
INVOQ    EQU   ((XMERR11-XMERR00)/4)+1                                          
IOCBQ    EQU   ((XMERR12-XMERR00)/4)+1                                          
TMODQ    EQU   ((XMERR13-XMERR00)/4)+1                                          
MODQ     EQU   ((XMERR14-XMERR00)/4)+1                                          
IODVQ    EQU   ((XMERR15-XMERR00)/4)+1                                          
IUPGQ    EQU   ((XMERR16-XMERR00)/4)+1                                          
ROMQ     EQU   ((XMERR17-XMERR00)/4)+1                                          
DUPOQ    EQU   ((XMERR18-XMERR00)/4)+1                                          
IPFKQ    EQU   ((XMERR19-XMERR00)/4)+1                                          
OOVFQ    EQU   ((XMERR20-XMERR00)/4)+1                                          
UNAQ     EQU   ((XMERR21-XMERR00)/4)+1                                          
OSKQ     EQU   ((XMERR22-XMERR00)/4)+1                                          
IDATQ    EQU   ((XMERR23-XMERR00)/4)+1                                          
NORECS   EQU   ((XMERR24-XMERR00)/4)+1                                          
NOTHEAD  EQU   ((XMERR25-XMERR00)/4)+1                                          
                                                                                
*                                                                               
XMERR00  DS    0H                                                               
XMERR01  B     MFLD                MISSING INPUT FIELD                          
XMERR02  B     IFLD                INVALID INPUT FIELD                          
XMERR03  B     RNF                 RECORD NOT FOUND                             
XMERR04  B     ISEL                INVALID SELECT CODE                          
XMERR05  B     NSS                 NO SAME STATION                              
XMERR06  B     IBOOK               INVALID BOOK                                 
XMERR07  B     IFILE               INVALID FILE                                 
XMERR08  B     ISTA                INVALID STATION                              
XMERR09  B     IDAY                INVALID DAY                                  
XMERR10  B     ITIME               INVALID TIME                                 
XMERR11  B     INVO                INVALID OPTION KEYWORD                       
XMERR12  B     IOCB                INVALID OPTION COMBINATION                   
XMERR13  B     TMOD                TOO MANY OPTION DATA VALUES                  
XMERR14  B     MOD                 MISSING OPTION DATA                          
XMERR15  B     IODV                INVALID OPTION DATA VALUE                    
XMERR16  B     IUPG                INVALID UPGRADE EXPRESSION                   
XMERR17  B     ROPTMISS            REQUIRED OPTION MISSING                      
XMERR18  B     DUPOPT              DUPLICATED OPTION KEYWORD                    
XMERR19  B     IPFK                INVALID PF KEY                               
XMERR20  B     OOVF                OPTION ONLY VALID FOR (&T)                   
XMERR21  B     UNA                 UPGRADE NOT ALLOWED (&T)                     
XMERR22  B     OSK                 OPTION SPECIFIED BY KEYWORD ONLY             
XMERR23  B     IDAT                INVALID DATE EXPRESSION                      
XMERR24  B     NORECSE             NO RECORD SELECTED FOR COPY                  
XMERR25  B     NOTHEADS            NO RECORD HEADER FOR TO STATION              
ERRX#    EQU   ((*-XMERR00)/4)+1                                                
                                                                                
ERRX2#   EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERRQ   EQU   ((*-XMERR00)/4)+1                                                
         EJECT                                                                  
                                                                                
MFLD     DS    0H                  MISSING INPUT FIELD                          
         MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
IFLD     DS    0H                  INVALID INPUT FIELD                          
         MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
RNF      DS    0H                  RECORD NOT FOUND                             
         MVI   ERROR,NOTFOUND                                                   
         B     ERREXIT                                                          
*                                                                               
ISEL     DS    0H                  INVALID SELECT CODE                          
         MVC   MSGNUM2,=H'221'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
NSS      DS    0H                  NO SAME STATION                              
         MVC   MSGNUM2,=AL2(RR#CPYST)                                           
         B     ERRGTXT                                                          
*                                                                               
IBOOK    DS    0H                  INVALID BOOK                                 
         MVC   MSGNUM2,=H'91'                                                   
         MVI   MSGSYS,15                                                        
         B     ERRGTXT                                                          
*                                                                               
IFILE    DS    0H                  INVALID FILE                                 
         MVC   MSGNUM2,=H'80'                                                   
         MVI   MSGSYS,15                                                        
         B     ERRGTXT                                                          
*                                                                               
ISTA     DS    0H                  INVALID STATION                              
         MVC   MSGNUM2,=H'92'                                                   
         MVI   MSGSYS,15                                                        
         B     ERRGTXT                                                          
*                                                                               
IDAY     DS    0H                  INVALID DAY                                  
         MVC   MSGNUM2,=H'100'                                                  
         MVI   MSGSYS,15                                                        
         B     ERRGTXT                                                          
*                                                                               
ITIME    DS    0H                  INVALID TIME                                 
         MVC   MSGNUM2,=H'101'                                                  
         MVI   MSGSYS,15                                                        
         B     ERRGTXT                                                          
*                                                                               
INVO     DS    0H                  INVALID OPTION KEYWORD                       
         MVC   MSGNUM2,=H'206'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
IOCB     DS    0H                  INVALID OPTION COMBINATION                   
         MVC   MSGNUM2,=AL2(RR#IOCMB)                                           
         B     ERRGTXT                                                          
*                                                                               
TMOD     DS    0H                  TOO MANY OPTION DATA VALUES                  
         MVC   MSGNUM2,=AL2(RR#TMOD)                                            
         B     ERRGTXT                                                          
*                                                                               
MOD      DS    0H                  MISSING OPTION DATA                          
         MVC   MSGNUM2,=AL2(RR#MOPTD)                                           
         B     ERRGTXT                                                          
*                                                                               
IODV     DS    0H                  INVALID OPTION DATA VALUE                    
         MVC   MSGNUM2,=H'209'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
IUPG     DS    0H                  INVALID UPGRADE EXPRESSION                   
         MVC   MSGNUM2,=AL2(RR#IUPGX)                                           
         B     ERRGTXT                                                          
*                                                                               
ROPTMISS DS    0H                  REQUIRE OPTION MISSING                       
         MVC   MSGNUM2,=AL2(RR#ROPTM)                                           
         B     ERRGTXT                                                          
*                                                                               
DUPOPT   DS    0H                  DUPLICATED OPTION KEYWORD                    
         MVC   MSGNUM2,=H'208'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
IPFK     DS    0H                  INVALID PF KEY                               
         MVC   MSGNUM2,=AL2(RR#IPFKY)                                           
         MVC   ACURFORC,AFRSTKEY                                                
         B     ERRGTXT                                                          
*                                                                               
OOVF     DS    0H                  OPTION ONLY VALID FOR (&T)                   
         MVC   MSGNUM2,=AL2(RR#OVLDF)                                           
         B     ERRGTXT                                                          
*                                                                               
UNA      DS    0H                  UPGRADE NOT ALLOWED (&T)                     
         MVC   MSGNUM2,=AL2(RR#UPNA)                                            
         B     ERRGTXT                                                          
*                                                                               
OSK      DS    0H                  OPTION SPECIFIED BY KEYWORD ONLY             
         MVC   MSGNUM2,=AL2(RR#OPSKW)                                           
         B     ERRGTXT                                                          
*                                                                               
IDAT     DS    0H                  INVALID DATE EXPRESSION                      
         MVC   MSGNUM2,=H'6'                                                    
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
NORECSE  DS    0H                  NO RECORD SELECTED FOR COPY                  
         MVC   MSGNUM2,=AL2(672)                                                
         B     ERRGTXT                                                          
*                                                                               
NOTHEADS DS    0H                  NO RECORD HEADER FOR 'TO' STATION            
         MVC   MSGNUM2,=AL2(673)                                                
         B     ERRGTXT                                                          
*                                                                               
         EJECT                                                                  
ERRGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMERR        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         MVI   ERROR,0                                                          
         B     ERREXIT                                                          
                                                                                
                                                                                
ERREXIT  DS    0H                                                               
         LR    R0,R2               SAVE DISPL OF FIELD                          
         S     R0,ATWA                                                          
         STH   R0,PRVFDSP                                                       
         OC    ACURFORC,ACURFORC   WANT TO FORCE CURSOR ELSEWHERE?              
         BZ    *+8                                                              
         L     R2,ACURFORC          YEP                                         
                                                                                
         B     ALLMSGX                                                          
*-------------------------- WARNING MESSAGES -------------------------*         
                                                                                
* These are messages where the user needs to hit <Enter> only for a             
*  response (for acknowledgment).  Previous values are restored,                
*  except in the case when the key changed in the same transaction.             
                                                                                
XMWRN    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         MVC   AERREX,ERREX                                                     
         LA    R2,ITCFSTAH         FORCE CURSOR TO KEY                          
                                                                                
         DS    0H                                                               
         CLI   OURWRNCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURWRNCD,XMWRNQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,OURWRNCD         BRANCH OFF TO SET WARNING MESSAGE            
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMWRN00(RF)                                                      
                                                                                
WTOPQ    EQU   ((XMWRN01-XMWRN00)/4)+1                                          
WBOTQ    EQU   ((XMWRN02-XMWRN00)/4)+1                                          
                                                                                
XMWRN00  DS    0H                                                               
XMWRN01  B     WTOP                TOP OF LIST                                  
XMWRN02  B     WBOT                BOTTOM OF LIST                               
WRNX#    EQU   ((*-XMWRN00)/4)+1                                                
                                                                                
XMWRNQ   EQU   ((*-XMWRN00)/4)+1                                                
         EJECT                                                                  
WTOP     DS    0H                       TOP OF LIST                             
         MVC   MSGNUM2,=AL2(RW#TOPLI)                                           
         B     WRNGTXT                                                          
*                                                                               
WBOT     DS    0H                       BOTTOM OF LIST                          
         MVC   MSGNUM2,=AL2(RW#BOTLI)                                           
         B     WRNGTXT                                                          
         EJECT                                                                  
WRNGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMWRN        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         B     WRNEXIT                                                          
                                                                                
                                                                                
WRNEXIT  DS    0H                                                               
         MVI   ERROR,0                                                          
         ICM   R0,15,ACURFORC      OVERRIDE CURSOR POSITION                     
         BZ    *+6                                                              
         LR    R2,R0                YEP                                         
                                                                                
         B     ALLMSGX                                                          
*---------------------------- INFO MESSAGES --------------------------*         
                                                                                
* At entry, R2-->appropriate field to put cursor on.                            
                                                                                
XMINF    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         XC    ACURFORC,ACURFORC                                                
         LA    R1,CONHEAD                                                       
                                                                                
         DS    0H                                                               
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   OURINFCD,INFX#                                                   
         BL    XMINFGO                                                          
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         CLI   OURINFCD,INFX2#                                                  
         BL    XMINFGO                                                          
         DC    H'0'                                                             
                                                                                
XMINFGO  DS    0H                                                               
         CLI   OURINFCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURINFCD,XMINFQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,OURINFCD         BRANCH OFF TO SET INFO MESSAGE               
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMINF00(RF)                                                      
                                                                                
NDPLYQ   EQU   ((XMINF01-XMINF00)/4)+1  NO DATA TO DISPLAY                      
RECSVQ   EQU   ((XMINF02-XMINF00)/4)+1  RECORD SAVED - ENTER NEXT               
NTRDELQ  EQU   ((XMINF03-XMINF00)/4)+1  REC DSPLYED - PRESS ENTR TO DEL         
RDELQ    EQU   ((XMINF04-XMINF00)/4)+1  REC DISPLAYED IS DELETED                
NTRRSTQ  EQU   ((XMINF05-XMINF00)/4)+1  REC DSPLYED - PRESS ENTR TO RST         
RRSTQ    EQU   ((XMINF06-XMINF00)/4)+1  REC RESTORED - ENTER NXT RQST           
                                                                                
XMINF00  DS    0H                                                               
XMINF01  B     NDPLY                                                            
XMINF02  B     RECSV               RECORD SAVED - ENTER NEXT                    
XMINF03  B     NTRDEL              RCRD DISPLAYED - PRESS ENTER TO DEL          
XMINF04  B     RDEL                RECORD DISPLAYED IS DELETED                  
XMINF05  B     NTRRST              RCRD DISPLAYED - PRESS ENTER TO RSTR         
XMINF06  B     RRST                RECORD RESTORED - ENTER NEXT REQUEST         
INFX#    EQU   ((*-XMINF00)/4)+1                                                
                                                                                
INFX2#   EQU   ((*-XMINF00)/4)+1                                                
                                                                                
XMINFQ   EQU   ((*-XMINF00)/4)+1                                                
         EJECT                                                                  
NDPLY    DS    0H                       NO DATA TO DISPLAY                      
         MVC   MSGNUM2,=AL2(RI#NDPLY)                                           
         MVC   ACURFORC,AFRSTKEY                                                
         B     INFGTXT                                                          
*                                                                               
RECSV    DS    0H                       RECORD SAVED - ENTER NEXT               
         MVC   MSGNUM2,=AL2(2045)                                               
         MVI   MSGSYS,GTGENSYS                                                  
         B     INFGTXT                                                          
*                                                                               
NTRDEL   DS    0H                       REC DSPLYED - PRESS NTR TO DEL          
         MVC   MSGNUM2,=AL2(24)                                                 
         MVC   ACURFORC,AFRSTKEY                                                
         MVI   MSGSYS,GTGENSYS                                                  
         B     INFGTXT                                                          
*                                                                               
RDEL     DS    0H                       RECORD DISPLAYED IS DELETED             
         MVC   MSGNUM2,=AL2(RI#RDIDL)                                           
         MVC   ACURFORC,AFRSTKEY                                                
         B     INFGTXT                                                          
*                                                                               
NTRRST   DS    0H                       REC DSPLYED - PRESS NTR TO RSTR         
         MVC   MSGNUM2,=AL2(25)                                                 
         MVC   ACURFORC,AFRSTKEY                                                
         MVI   MSGSYS,GTGENSYS                                                  
         B     INFGTXT                                                          
*                                                                               
RRST     DS    0H                       RECORD RSTORED - ENTER NXT RQST         
         MVC   MSGNUM2,=AL2(8)                                                  
         MVC   ACURFORC,AFRSTKEY                                                
         MVI   MSGSYS,GTGENSYS                                                  
         B     INFGTXT                                                          
*                                                                               
         EJECT                                                                  
INFGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMINF        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         B     INFEXIT                                                          
                                                                                
                                                                                
INFEXIT  DS    0H                                                               
         OC    ACURFORC,ACURFORC   NEED TO SET CURSOR?                          
         BNZ   INFEXITX             NOPE                                        
         MVC   ACURFORC,AFRSTKEY   PLACE CURSOR ON 1ST KEY FIELD,               
                                                                                
INFEXITX DS    0H                                                               
         MVI   ERROR,0                                                          
         B     ALLMSGX                                                          
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
*--------------------- SUBRXM MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBRXML  EQU   *-XMSGRTN                                                        
         DS    0CL(X'1000'-SUBRXML+1)                                           
***********************************************************************         
         DROP  R7,R8,RA,RB,RC                                                   
*========================== RMP28's EQUATES ==========================*         
                                                                                
EOT      EQU   X'00'               END OF TABLE MARKER                          
                                                                                
PHINVSCO EQU   X'27'               PHASE NUMBER FOR INV/SCOPY                   
SCINVSCO EQU   X'CF'               SCREEN NUMBER FOR INV/SCOPY                  
                                                                                
PAGEQ    EQU   2                   TWA PAGE # FOR TEMPSTR                       
NUMINVQ  EQU   (L'ITCFINV)/(3+1)   MAX # OF INVENTORY NUMBER ENTRIES            
MXFTNTRY EQU   200                 MAX # OF FROM TABLE ENTRIES                  
IKYSTAL  EQU   RINVKSTA-RINVREC+L'RINVKSTA                                      
IKYINVL  EQU   RINVKINV-RINVREC+L'RINVKINV                                      
DMKIVRQ  EQU   (RINVPEL-RINVREC)-(DRFRSTEL-DRKEY)                               
TIASVLEN EQU   BIGENDSV-BIGAREA                                                 
                                                                                
*                                 ********* BIT MANIPULATIONS *********         
XFF      EQU   X'FF'                                                            
X80      EQU   X'80'                                                            
X40      EQU   X'40'                                                            
X20      EQU   X'20'                                                            
X10      EQU   X'10'                                                            
X08      EQU   X'08'                                                            
X04      EQU   X'04'                                                            
X02      EQU   X'02'                                                            
X01      EQU   X'01'                                                            
X00      EQU   X'00'                                                            
                                                                                
*                                 ************* BOOK BITS *************         
BBARB    EQU   X00                 ARBITRON                                     
BBMFX    EQU   BBARB               MEDIAFAX                                     
BBNSI    EQU   X40                 NIELSEN                                      
BBSRC    EQU   X40+X01             STRATEGY                                     
                                                                                
BBPBK    EQU   X04                 PROJECTED BOOK                               
BBTP     EQU   X08                 TIME PERIOD                                  
BBSS     EQU   X02                 SPECIAL SURVEY                               
BBEBK    EQU   X20                 ESTIMATED BOOK                               
         EJECT                                                                  
*                                 *********** OPTION EQUATES **********         
OPNUPGD  EQU   1                   UPGRADE                                      
OPBUPGD  EQU   X'00000001'                                                      
OPNCUME  EQU   2                   CUMULATIVE VALUE TYPE                        
OPBCUME  EQU   X'00000002'                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0XL(2+2+2)                                                       
         DC    AL2(GOSUB-RMP28,0,AGOSUB-SYSD)                                   
         DC    AL2(SUBR01-RMP28,0,ASUBR01-SYSD)                                 
         DC    AL2(XMSGRTN-RMP28,0,AXMSGRTN-SYSD)                               
         DC    AL2(SVCLST-RMP28,0,ASVCTAB-SYSD)                                 
         DC    AL2(TABELCPY-RMP28,0,ATBELCPY-SYSD)                              
         DC    AL2(FROMTAB-BIGAREA,ATIA-GEND,AFROMTAB-SYSD)                     
         DC    AL2(OPTVALS-SYSD,ASYSD-GEND,AOPTVALS-SYSD)                       
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
LBLTAB   DS    0XL(2+8)            TABLE OF LABELS FOR BIG STORAGE              
         DC    AL2(FRMTBLBL-BIGAREA),CL8'*FROMTB*'                              
LBLTABQ  EQU   (*-LBLTAB)/(L'LBLTAB)                                            
                                                                                
                                                                                
TABCLR   DS    0XL(1+2+2)          STORAGES TO CLEAR                            
         DC    C'S',AL2(MYTEXT-SYSD),AL2(MYTEXTX-MYTEXT)                        
TABCLRQ  EQU   (*-TABCLR)/(L'TABCLR)                                            
         EJECT                                                                  
DCLIST   DS    0C                  DATA DICTIONARY ESCAPE SEQUENCES             
         DCDDL RE#10PF,L'RE@10PF,L                                              
         DCDDL RE#PF10S,L'RE@PF10S,L                                            
         DCDDL RE#PF12R,L'RE@PF12R,L                                            
         DCDDL RE#PF12N,L'RE@PF12N,L                                            
         DCDDL RE#CHA,L'RE@CHA,L                                                
         DCDDL RE#OPTS,L'RE@OPTS,L                                              
         DCDDL RE#SEL,L'RE@SEL,L                                                
         DCDDL RE#LP,L'RE@LP,L                                                  
DCLISTX  EQU   *                                                                
                                                                                
                                                                                
TABELCPY DS    0X                  TABLE OF ELEMENTS TO COPY                    
*^^GYL   DC     XL1'01'             TEXT ELEMENT                                
         DC     XL1'02'             TEXT FILTER ELEMENT                         
         DC     XL1'03'             TRANSFER FROM ELEMENT                       
         DC     XL1'CD'             CODE ELEMENT                                
         DC     XL1'CE'             DAY/TIME ELEMENT                            
         DC     XL1'EF'             ACTIVITY ELEMENT                            
         DC     AL1(EOT)                                                        
         EJECT                                                                  
*---------------------------- SOURCE TABLE ---------------------------*         
                                                                                
       ++INCLUDE RESVCTABN                                                      
***********************************************************************         
*============================= RERMPWORKD ============================*         
       ++INCLUDE RERMPWORKD                                                     
***********************************************************************         
*========================== SYSD'S SYSSPARE ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*----------------------- OWNED BY RERMP28 ONLY -----------------------*         
                                                                                
*                                 ************** WORK AREA ************         
DEMODUB  DS    D                   EXTRA STORAGE FOR DEMUP                      
DIVIDEND DS    D                                                                
DIVISOR  DS    F                                                                
REMAINDR DS    F                                                                
QUOTIENT DS    F                                                                
RELO     DS    F                                                                
MYDMCB   DS    6F                                                               
                                                                                
*                                 ************** ADDRESSES ************         
MYBASE1  DS    A                   A(1ST 4096 BYTES OF THIS PROGRAM)            
MYBASE2  DS    A                   A(2ND 4096 BYTES OF THIS PROGRAM)            
AIUNWRK  DS    A                   A(IUN WORK AREA)                             
ASUBRTN  DS    A                   A(SUBROUTINE POOL TO USE)                    
AERREX   DS    A                   A(ERREX ROUTINE) TO USE                      
A1SELFLD DS    A                   A(1ST SELECT FIELD)                          
AOPTNTRY DS    A                   A(OPTTABLE ENTRY)                            
ANLTNTRY DS    A                   A(NEXT AVAILABLE SLOT IN LIST TABLE)         
AMNISTRT DS    A                   A(START FIELD) FOR MNI# ROUTINE              
AMNIEND  DS    A                   A(END FIELD) FOR MNI# ROUTINE                
A1SELCH  DS    A                   A(1ST SELECT FIELD CHANGED)                  
A1LPCH   DS    A                   A(1ST LIST PARM FIELD CHANGED)               
PRVFDSP  DS    H                   FIELD DISP FROM PREV TRANSACTION             
         DS    H                   SPARE TO KEEP FULL-WORD ALIGNMENT            
                                                                                
         DS    0A                  ADDRESSES OF TABLES/ROUTINES                 
AGOSUB   DS    A                    A(SUBROUTINE POOL INTERFACE)                
ASUBR01  DS    A                    A(SUBROUTINE POOL #1)                       
AXMSGRTN DS    A                    A(EXIT W/ MSG ROUTINE)                      
AFROMTAB DS    A                    A(FROMTAB)                                  
ASVCTAB  DS    A                    A(SVCLST)                                   
ATBELCPY DS    A                    A(TABELCPY)                                 
AMLSTDIR DS    A                    A(MY LIST DIRECTORY)                        
ALISTTAB DS    A                    A(LIST TABLE)                               
AINVLIST DS    A                    A(INVENTORY LIST)                           
ASVLP    DS    A                    A(SAVE LP AREA)                             
ALISTPRM DS    A                    A(LIST PARAMETERS)                          
AOPTVALS DS    A                    A(OPTION VALUES)                            
                                                                                
*                                 ************** COUNTERS *************         
CNTCOPY  DS    F                   COUNT # OF COPY SUCCESSFULS                  
CNTNCPY  DS    F                   COUNT # OF "NOT-COPIED"S                     
                                                                                
*                                 ************ FROM DETAILS ***********         
FROMDETL DS    0C                  INPUT FOR "COPY FROM" DETAILS                
FRSTTN   DS     CL5                 STATION                                     
                                                                                
FRINVTB  DS     0C                  INVENTORY NUMBER TABLE                      
FRINVRGE DS      (NUMINVQ)CL(4+4)    ARRAY OF START-END INV # RANGE             
         DS      XL1                 EOT MARKER                                 
FRINVTBX EQU    *                                                               
                                                                                
FREFFDB  DS     0XL(3+3)            EFFECTTIVE DATE (BINARY) RANGE              
FRSEFFDB DS      XL3                 START EFFECTIVE DATE (BINARY)              
FREEFFDB DS      XL3                 END   EFFECTIVE DATE (BINARY,OPTL)         
                                                                                
FRCPYTRK DS     CL1                 COPY TRACK RECORDS (Y/N)                    
                                                                                
FRCPYTXT DS     CL1                 COPY TEXT  RECORDS (Y/N)                    
FRMDETLX EQU   *                                                                
                                                                                
*                                 ************* TO DETAILS ************         
TODETAIL DS    0C                  INPUT FOR "COPY TO" DETAILS                  
TOSTTN   DS     CL5                 STATIONS                                    
TODTAILX EQU   *                                                                
                                                                                
*                                 ************ HEADER INFO ************         
HDRINFO  DS    0X                                                               
HDRIDAY  DS     XL1                 (INTERNAL) DAY                              
HDRTIME  DS     0XL4                TIMES                                       
HDRSTIM  DS      XL2                 START                                      
HDRETIM  DS      XL2                 END                                        
HDRPRGNA DS     CL27                PROGRAM NAME                                
HDRINFOX EQU   *                                                                
HDRINFOL EQU   HDRINFOX-HDRINFO                                                 
                                                                                
*                                 ************* TRACK INFO ************         
TRKINFO  DS    0X                                                               
TRKFTNT  DS     CL16                FOOTNOTE                                    
TRKINFOX EQU   *                                                                
TRKINFOL EQU   TRKINFOX-TRKINFO                                                 
                                                                                
*                                 ************ TEMP STORAGE ***********         
MYDATE   DS    0CL8                DATE FIELD FOR DATVAL                        
MYMNTH   DS    CL3                                                              
MYDAY    DS    CL3                                                              
MYYEAR   DS    CL2                                                              
ODATVAL  DS    CL6                 DATVAL OUTPUT AREA                           
*                                                                               
TMPREP   DS    CL2                 REP CODE                                     
TMPS     DS   0X                                                                
TMPSRC   DS    CL3                 TEMP STORAGE FOR SOURCE                      
TMPKSRC  DS    CL1                  "      "     "  KEY SOURCE                  
TEMPSRC  DS    CL1                  "      "     "  KEY SOURCE                  
TRKSRCF  DS    CL1                 TRACK SOURCE FILTER                          
TRKQUALF DS    CL1                 TRACK QUALIFYING FILTER                      
TMPFRBK  DS    0XL3                 "      "     "  FROM BOOK                   
TMPFRBTS DS     XL1                 "      "     "   BITS                       
TMPBOOK  DS     XL2                 "      "     "   BOOK                       
TMPDFIL  DS    CL3                  "      "     "  FILE                        
TMPCODE  DS    CL2                  "      "     "  TRANSFER CODE               
TMPSTTN  DS    CL5                  "      "     "  STATION                     
TMPINVN  DS    CL4                  "      "     "  INVENTORY NUMBER            
TMPINV   DS    CL4                  "      "     "  INVENTORY NUMBER            
TMPSTD   DS    CL8                  "      "     "  EFFECTIVE DATE              
TOEFFDE  DS    CL8                  "      "     "  EFFEC.DATE (EBCDIC)         
TMPPURE  DS    CL4                  "      "     "  PURE NUMBER                 
TMPBTYP  DS    CL1                  "      "     "  BOOK TYPE                   
TMPWKN   DS    XL1                  "      "     "  WEEK NUMBER (1-4)           
TMPIDAY  DS    XL1                  "      "     "  INTERNAL DAY                
TMPKDAY  DS    XL1                  "      "     "  KEY DAY                     
TMPSETM  DS    0XL4                 "      "     "  TIMES                       
TMPSTIM  DS     XL2                 "      "     "   START TIME                 
TMPETIM  DS     XL2                 "      "     "   END TIME                   
TMPNOR   DS    CL1                  "      "     "  NOR PROGRAM FLAG            
TMPNQHR  DS    XL1                  "      "     "  # OF QUARTER HOURS          
TMPWKS   DS    XL1                  "      "     "  WEEKS                       
TMPBEST  DS    CL1                  "      "     "  BEST/ALL                    
TMPPGNAM DS    CL13                 "      "     "  PROGRAM NAME                
TMPDEMC  DS    XL1                  "      "     "  DEMO CATEGORY               
TMPMTIM  DS    XL2                  "      "     "  MILITARY TIME               
TMPMED   DS    CL1                  "      "     "  MEDIA                       
TMPQHR   DS    XL1                  "      "     "  QUARTER HOUR                
TMPBDATE DS    XL3                  "      "     "  BINARY DATE                 
TMPCDATE DS    XL2                  "      "     "  COMPRESSED DATE             
TMPWTOV  DS    XL1                  "      "     "  OVERRIDE WEIGHT             
TMPFLAG  DS    XL1                  "      "     "  FLAGS                       
TMPSX    EQU  *                                                                 
TMPSL    EQU  TMPSX-TMPS                                                        
*                                                                               
BKLST    DS    XL6                 2 - 3 BYTE CODES AFTER BOOKVAL               
BTLST    DS    XL2                 2 - 1 BYTE CODES AFTER BOOKVAL               
BKVLHEAD DS    0CL31               DUMMY BOOKVAL HEADER                         
BKHEAD   DS     CL8                HEADER PART                                  
BKDATA   DS     CL23               BOOK DATES W/ SOURCE                         
BKVLHLQ  EQU    31                                                              
*                                                                               
TRCKTBL  DS    12CL7               TABLE FOR TRACKS AFTER PERVAL                
*        DS    CL1                 RINVKSRC FOR KEY                             
*        DS    CL2                 START DATE                                   
*        DS    CL2                 END DATE                                     
*        DS    CL1                 TRKQUALF(TRACK QUALIFYING FILTER)            
*        DS    CL1                 TRKTBL FLAGS                                 
ONEBOOK  EQU   X'80'               ONLY ONE BOOK FILTER HERE                    
*                                                                               
FLTSRC   DS    CL1                 RINVKSRC FOR KEY (GETKSRC)                   
INBLOCK  DS    CL5                 INPUT BLOCK FOR GETKSRC                      
OTBLOCK  DS    CL5                 OUTPUT BLOCK FOR GEKSKRC                     
CUMFCTR  DS    F                   CUMLTV FACTOR FOR MULT. DEMAND CALLS         
CUMNQH   DS    H                   CUMULATIVE # OF QUARTER HOURS                
DEMNQH   DS    H                                                                
OURBYTE  DS    XL1                 ANOTHER 1-BYTE WORKING STORAGE               
NLNDSPLY DS    XL1                 # OF LINES DISPLAYED                         
GOSUBN   DS    XL1                 SUBROUTINE NUMBER                            
MYRDUPDT DS    XL1                 MY VERSION OF RDUPDATE                       
MSGNUM2  DS    XL2                                                              
MSGSYS   DS    XL1                                                              
MSGTYPE  DS    CL1                 (E)RROR, (W)ARNING, OR (I)NFO                
OURERRCD DS    XL1                 MY ERROR   CODE                              
OURWRNCD DS    XL1                 MY WARNING CODE                              
OURINFCD DS    XL1                 MY INFO    CODE                              
NFTNTRYS DS    XL1                 # OF "FROM" TABLE ENTRIES                    
NTIMES   DS    XL1                                                              
COUNTER  DS    XL1                                                              
TRCKCNT  DS    XL1                 # OF TRACKS TO BE FILTERED                   
TOCOUNT  DS    XL1                 # OF 'TO' ENTRIES                            
FLDDSPL  DS    XL1                 DISPL OF SUB-FIELD INTO FIELD                
SCANLNTH DS    XL1                 L'RHS OF SCANNER BLOCK ENTRY                 
NOPTN    DS    XL1                 NUMBER OF OPTIONS INPUTTED                   
OPTI     DS    AL4                 THE OPTIONS INPUTTED                         
OPTR     DS    AL4                 OPTIONS REQUIRED                             
OPTX     DS    AL4                 OPTIONS NOT ALLOWED                          
EL75LEN  EQU   32                  LENGTH OF X'75' ELEMENT                      
                                                                                
*                                 *************** FLAGS ***************         
MISCFLG1 DS    XL1                 MISC FLAG #1                                 
MF1GDPLY EQU    X80                 GO TO DISPLAY LOGIC                         
MF1ADCMB EQU    X40                 ADD DATA UP FOR COMBOS                      
MF1RDDEL EQU    X20                 CAN READ DELETED RECORDS AS WELL            
MF1ERRQ  EQU    X01                                                             
MF1RSTKC EQU   0                   RESET THESE ON KEY CHANGE                    
                                                                                
RECDFLG1 DS    XL1                 FLAGS ABOUT RECORD (TRACK)                   
RF1DEL   EQU    X80                 RECORD MARKED FOR DELETION                  
                                                                                
DPLYFLG1 DS    XL1                 DISPLAY FLAG #1                              
DF1PFSAV EQU    X80                 DISPLAYED PFKEY FOR SAVING                  
                                                                                
CHNGFLG1 DS    XL1                 CHANGE FLAG #1                               
CF1KEY   EQU    X80                 A KEY FIELD HAS CHANGED                     
CF1OPT   EQU    X40                 OPTION FIELD HAS BEEN INPUTTED              
CF1IP    EQU    X20                 LIST PARAMETER HAS BEEN INPUTTED            
CF1IPDC  EQU    X10                 LIST PARAMETER DEMO CATGORY MODFYD          
CF1KO    EQU   CF1KEY+CF1OPT                                                    
                                                                                
INPMFLG1 DS    XL1                 INPUT PARAMETER FLAG #1                      
IPF1SC   EQU    X80                 THERE IS SOURCE  INPUT                      
IPF1FL   EQU    X40                   "   "  FILE      "                        
IPF1BK   EQU    X20                   "   "  BOOK      "                        
IPF1ST   EQU    X10                   "   "  STATION   "                        
IPF1PI   EQU    X08                   "   "  PUR/IV#   "                        
IPF1DY   EQU    X04                   "   "  DAY       "                        
IPF1TE   EQU    X02                   "   "  TM/EFFD   "                        
IPF1ALL  EQU   IPF1SC+IPF1FL+IPF1BK+IPF1ST+IPF1PI+IPF1DY+IPF1TE                 
                                                                                
SELFLAG1 DS    XL1                 SELECT FLAG #1                               
SF1AL1ES EQU    X80                 AT LEAST 1 ITEM SELECTED IN LIST            
SF1SLECT EQU    X40                 AN ENTRY GOT SELECTED                       
SF1DSLCT EQU    X20                 AN ENTRY GOT DE-SELECTED                    
SF1RSTKC EQU   SF1AL1ES+SF1SLECT+SF1DSLCT   RESET THESE ON KEY CHANGE           
                                                                                
VRFLAG1  DS    XL1                 VALREC FLAG #1                               
VRF1RLT  EQU    X80                 WENT TO REFRESH LIST TABLE                  
VRF1BLT  EQU    X40                 WENT TO BUILD LIST TABLE                    
VRF1BLD  EQU    X20                 WENT TO RE-BUILD LIST DIRECTORY             
VRF1SDV  EQU    X10                 WENT TO SEED DEMO VALUES                    
VRF1CUM  EQU    X08                 RECALCULATED CUMULATIVE VALUES              
VRF1TSKS EQU   VRF1RLT+VRF1BLT+VRF1BLD+VRF1SDV+VRF1CUM                          
                                                                                
DEMFLAG1 DS    XL1                 DEMO FLAG #1                                 
DF1ADCMB EQU    X80                 ADD DATA UP FOR COMBOS                      
DF1WGTOV EQU    X40                 WEIGHT OVERRIDE                             
DF1RSTKC EQU   DF1ADCMB+DF1WGTOV   RESET THESE ON KEY CHANGE                    
                                                                                
TYPELP   DS    XL1                 TYPE OF LIST PARAMETERS                      
TLPIDEF  EQU    C'I'                INVENTORY RECORD "DEFINITION"               
TLPHIST  EQU    C'H'                HISTORY OF DATA IN INVENTORY RECD           
TLPUINP  EQU    C'U'                USER INPUTTED LIST PARAMS                   
TLPXFER  EQU    C'F'                "TRANSFER FROM" DATA                        
                                                                                
CODECTL  DS    XL1                 CODE CONTROL BITS                            
CDPAV    EQU    X80                 CODE "IMPLIES" PAV FILE                     
CDINV    EQU    X40                  "       "     INV  "                       
CDTP     EQU    X20                  "       "     TP   "                       
CDMIXPT  EQU   CDPAV+CDTP            "       "     PAV & TP FILE                
                                                                                
*                                 ************** PROFILES *************         
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
                                                                                
*                                 ******* DATA DICTIONARY TERMS *******         
DSLIST   DS    0C                                                               
RE@10PF  DS     CL49               PF5=Top  6=Bottom  7=Up  8=Down              
RE@PF10S DS     CL7                10=Save                                      
RE@PF12R DS     CL7                12=Rtrn                                      
RE@PF12N DS     CL7                12=Next                                      
RE@CHA   DS     CL6                change                                       
RE@OPTS  DS     CL7                options                                      
RE@SEL   DS     CL6                select                                       
RE@LP    DS     CL10               list parms                                   
DSLISTX  EQU   *                                                                
                                                                                
         DS    0XL(L'RE@PF12R-L'RE@PF12N+1)                                     
         DS    0XL(L'RE@PF12N-L'RE@PF12R+1)                                     
                                                                                
*                                 ************** BUFFERS **************         
TOTABLE  DS    5CL14               'TO' DETAILS                                 
TOADDR   DS    F                   A(TOTABLE) ENTRY                             
TOMAX    DS    CL1                 MAX # OF 'TO' STATION ENTRIES                
TOKEY    DS    XL(L'RINVKEY)                                                    
FROMKEY  DS    XL(L'RINVKEY)                                                    
PREVKEY  DS    XL21                RECORD TYPE,REP CODE,STATION,INV.            
STARTKEY DS    XL(L'RINVKEY)                                                    
TEMPKEY  DS    XL(L'RINVKEY)                                                    
IBLOCK   DS    CL5                 INPUT BLOCK FOR VGETKSRC                     
OBLOCK   DS    CL5                 OUTPUT BLOCK FOR VGETKSRC                    
SCRPTR   DS    A                   A(EMPTY 'TO' STATION ON SCREEN)              
TOSTAH   DS    A                   A('TO' STATION ON SCREEN)                    
                                                                                
PRVFTKEY DS    XL(FTKEYL)          KEY OF PREVIOUS "FROM-TABLE" ENTRY           
                                                                                
SVINVDT  DS    CL7                 HEADER INV. AND DATE                         
MYFLAG   DS    XL1                 FLAGS                                        
YESMSTA  EQU   X'01'               MASTER/STATION RECORDS EXIST                 
TRCKSRC  EQU   X'02'               THERE IS A TRACK SOURCE FILTER               
TRCKQUAL EQU   X'04'               THERE IS A TRACK QUALIFYING FILTER           
TRCKFLT  EQU   X'08'               THERE ARE TRACKS FOR FILTERING               
ALLTRKS  EQU   X'10'               COPY ALL TRACKS                              
BKTPFLT  EQU   X'20'               THERE IS A BOOKTYPE FILTER                   
NOTRKS   EQU   X'40'               DO NOT COPY ANY TRACKS                       
                                                                                
MYTEXT   DS    0X                  MISCELLANEOUS TEXT FIELD                     
         DS    XL1                  L'TEXT                                      
         DS    CL20                 THE TEXT ITSELF                             
MYTEXTX  EQU   *                                                                
MYTEXTL  EQU   MYTEXTX-MYTEXT                                                   
                                                                                
DUMRINVZ DS    XL(10+1)            AREA FOR DUMMY RINVZEL ELEMENT               
         DS    0XL((L'DUMRINVZ-1)-(RINVZDAY-RINVZEL+L'RINVZDAY))                
         DS    0X                   ENSURE ENOUGH ROOM TO HOLD DAY              
         DS    0XL((L'DUMRINVZ-1)-(RINVZTIM-RINVZEL+L'RINVZTIM))                
         DS    0X                   ENSURE ENOUGH ROOM TO HOLD TIME             
                                                                                
MYFLD    DS    0X                  MY TWA FIELD WORK AREA                       
MYFLDH   DS     XL8                 MY WORKING FIELD HEADER                     
MYFLDD   DS     XL80                MY WORKING FIELD DATA                       
MYFLDL   EQU   *-MYFLD                                                          
                                                                                
PERVALB  DS    XL(L'PVALOUTB)      PERVAL BLOCK                                 
PERBSTA  DS    XL3                 BIN YYMMDD START OF PERIOD                   
PERBEND  DS    XL3                 BIN YYMMDD END OF PERIOD                     
         EJECT                                                                  
*                                 *********** OPTION VALUES ***********         
OPTVALS  DS    0C                                                               
                                                                                
OPVCUM   DS    0X                  OPTION VALUES FOR CUM VALUE TYPE             
OPVCUMMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVCUMNM DS     XL1                     # OF VALUES SO FAR                      
OPVCUMTB DS     (OPVCUMMX)CL1           TABLE TO HOLD VALUES                    
OPVCUMQ  EQU   *-OPVCUM                                                         
                                                                                
OPTVALSQ EQU   *-OPTVALS                                                        
         EJECT                                                                  
                                                                                
*                                                                               
MYSSPREL EQU   *-SYSSPARE                                                       
SYSPREMN EQU   L'SYSSPARE-MYSSPREL   AMOUNT LEFT IN SYSSPARE                    
         DS    0XL(SYSPREMN+1)       CHECK AGAINST SYSSPARE LIMIT               
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
*-------------------------- INV/SCOPY SCREEN -------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPCED                                                       
         EJECT                                                                  
*------------------------- SAVED STORAGE AREA ------------------------*         
         ORG                                                                    
* WARNING: This is not a good place to save information around.  This           
*           area may coincide with the field  SVLIST  defined in                
*           RERMP00 and may eventually clobber what was there.                  
*                                                                               
MYTWAL   EQU   *-CONHEADH                                                       
         DS    0CL(3520-L'SFMPROFS-MYTWAL)  GENCON & RERMP'S TWA LIMIT          
                                                                                
                                                                                
*------------------------------ DDGENTWA -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
*----------------------------- RERMPWTWA -----------------------------*         
       ++INCLUDE RERMPWTWA                                                      
***********************************************************************         
*============================ OTHER DSECTS ===========================*         
                                                                                
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* DDCOREQUS                                                                     
* DDDICTATED                                                                    
* DDPERVALD                                                                     
* REMSGEQUS                                                                     
* REDDEQUS                                                                      
* DEDEMFILE                                                                     
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDDICTATED                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE REMSGEQUS                                                      
       ++INCLUDE REDDEQUS                                                       
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
***********************************************************************         
*============================ REGEN DSECTS ===========================*         
                                                                                
*------------------------------ REGENINV -----------------------------*         
REINVRCD DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
*------------------------------ REGENSTA -----------------------------*         
RSTARECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
*------------------------ ENTRY IN FROM TABLE ------------------------*         
                                                                                
FROMTABD DSECT                                                                  
FTKEY    DS    0X                  START OF KEY FOR BINSRCH                     
FTINV    DS     CL(L'RINVKINV)      INVENTORY NUMBER                            
FTSTD    DS     CL(L'RINVKSTD)      EFFECTIVE DATE                              
FTKSRC   DS     CL(L'RINVKSRC)      KEY SOURCE                                  
FTBK_TXT DS     CL(L'RINVKBK)       BOOK OR TEXT NUMBER                         
FTKEYL   EQU   *-FTKEY             LENGTH OF KEY                                
                                                                                
FTFLAG1  DS    XL1                 FLAG #1                                      
FTF1RDEL EQU    X80                 "TO" RECORD DELETED                         
FTF1RXST EQU    X40                 "TO" RECORD EXISTS                          
FTF1HEAD EQU    X20                 "TO' HEADER RECORD EXISTS                   
FTF1EFFD EQU    X10                 "TO" EFFECTIVE DATE DIFFERENT               
FTF1NOHD EQU    X08                 "TO' HEADER RECORD DOESN'T EXIST            
                                                                                
FROMTABL EQU   *-FROMTABD                                                       
                                                                                
                                                                                
*------------------------ ENTRY IN TO TABLE --------------------------*         
                                                                                
TOTABLED DSECT                                                                  
TOSTA    DS     CL(L'RINVKSTA)      STATION                                     
TOINV    DS     CL(L'RINVKINV)      INVENTORY NUMBER                            
TOSTD    DS     CL(L'RINVKSTD)      EFFECTIVE DATE                              
TOSRC    DS     CL(L'RINVKSRC)      KEY SOURCE                                  
TOKEYL   EQU   *-FTKEY             LENGTH OF KEY                                
*                                                                               
TOFLAG1  DS    XL1                 FLAG #1                                      
TOF1RDEL EQU    X80                 "TO" RECORD DELETED                         
TOF1RXST EQU    X40                 "TO" RECORD EXISTS                          
TOF1EFFD EQU    X20                 "TO" EFFECTIVE DATE DIFFERENT               
*                                                                               
TOTABLEL EQU   *-TOTABLED                                                       
                                                                                
                                                                                
*----------------------------- PRINT LINE ----------------------------*         
                                                                                
PRTLINED DSECT                                                                  
PLKEY    DS    XL36                                                             
PLKSTTN  DS     0CL6               STATION CALL LETTERS                         
         DS     0CL1                                                            
PLKINVN  DS     0CL4               INVENTORY NUMBER                             
         DS     0CL1                                                            
PLKEFFD  DS     0CL8               EFFECTIVE DATE (MMMDD/YY)                    
         DS     0CL1                                                            
PLKRSVC  DS     0CL3               RATING SERVICE                               
         DS     0CL1                                                            
PLKBKTXT DS     0CL14              BOOK/TRACK                                   
                                                                                
         DS    CL1                                                              
PLSIGN   DS    CL3                 '==>'                                        
PLSTATUS DS    CL16                                                             
PLCOMMNT DS    CL25                                                             
                                                                                
PRTLINEL EQU   *-PRTLINED                                                       
         DS    0XL(L'P-PRTLINEL+1)                                              
         EJECT                                                                  
***********************************************************************         
PRTHEAD  DSECT                                                                  
PHSOURCE DS     0CL12                                                           
         DS     0CL3                                                            
PHKSTTN  DS     0CL6               STATION CALL LETTERS                         
         DS     0CL1                                                            
PHKINVN  DS     0CL4               INVENTORY NUMBER                             
         DS     0CL1                                                            
PHKEFFD  DS     0CL8               EFFECTIVE DATE (MMMDD/YY)                    
PRTHEADL EQU   *-PRTHEAD                                                        
         DS    0XL(L'P-PRTHEADL+1)                                              
         EJECT                                                                  
***********************************************************************         
*========================== BIG STORAGE AREA =========================*         
                                                                                
BIGAREA  DSECT                                                                  
                                                                                
                                                                                
BIGENDSV EQU   *                   SAVE UP TO HERE INTO TEMPSTR                 
                                                                                
                                                                                
FRMTBLBL DS    D                   *FROMTB*                                     
FROMTAB  DS    (MXFTNTRY)XL(FROMTABL)                                           
FROMTABX EQU   *                                                                
                                                                                
MYTIALEN EQU   *-BIGAREA                                                        
         DS    0CL((X'4800'-MYTIALEN)+1)                                        
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109RERMP28X  02/07/00'                                      
         END                                                                    
