*          DATA SET RERMP27    AT LEVEL 249 AS OF 03/03/11                      
*PHASE T81027A                                                                  
*INCLUDE REGETKSRC                                                              
*INCLUDE RECUP                                                                  
T81027   TITLE 'RERMP27 - S(TATION) COPY REPORT'                                
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* Feb25/97 001 SCHT - New program for Station COPY REPORT             *         
*                                                                     *         
* FEB11/05     BU   - RELINK WITH NEW VERSION OF RESVCTABN            *         
*                                                                     *         
* APR16/09     KUI    NEW INVENTORY KEY SUPPORT                       *         
*                     COPYREC: CHECK ZERO CONDITION                   *         
*                                                                     *         
* JAN11/10     KUI    FIX GETKSRC K MODE INPUT PARAMETER              *         
*                                                                     *         
* MAR03/11     SMY    FIX COMPARE LOGIC IN PR200                      *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
RMP27    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81027**,R9,RR=RE                                              
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
*                                                                               
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
*========================= MY INITIALIZATION =========================*         
MYINIT   NTR1                                                                   
*                                                                               
         DS    0H                  SET UP ADCONS OF TABLES & ROUTINES           
         LH    R2,=Y(DISPTAB-RMP27)                                             
         LA    R2,RMP27(R2)                                                     
         LA    R0,DISPTABQ                                                      
*                                                                               
MI10     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         LA    RE,RMP27            RE = BASE OF TABLE/ROUTINE                   
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
         LH    R2,=Y(LBLTAB-RMP27)                                              
         LA    R2,RMP27(R2)                                                     
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
         LH    RE,=Y(DCLIST-RMP27)                                              
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
*====================== VALIDATE RECORD ROUTINE ======================*         
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
*                                                                               
*--------------------- VALIDATE "FROM" DETAILS -----------------------*         
*                                                                               
** "FROM" STATION **                                                            
*                                                                               
         LA    R2,ISCFSTAH         FROM STATION FIELD                           
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
         XC    TMPRSR,TMPRSR                                                    
         XC    TMPDSR,TMPDSR                                                    
         XC    TMPQLF,TMPQLF                                                    
         XC    TMPBTP,TMPBTP                                                    
         XC    TMPBOOK,TMPBOOK                                                  
         MVI   GOSUBN,BIK#         BUILD KEY OF AN INVENTORY RECORD             
         GOTO1 AGOSUB                                                           
*                                                                               
         MVI   MYRDUPDT,C'N'                                                    
         GOTO1 HIGH                SEE IF STATION EXIST IN INV FILE             
         CLC   KEY(IKYSTAL),KEYSAVE                                             
         BNE   RCDNTFND             NO IT DOESN'T                               
*                                                                               
** VALIDATE INVENTORY NUMBER **                                                 
* Some valid input are "1042-3600,563,..."                                      
*                                                                               
         LA    R2,ISCFINVH         INVENTORY NUMBER                             
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
*                                                                               
         MVI   FRINVTB,EOT         ASSUME NO INVENTORY NUMBER INPUT             
         CLI   5(R2),0             ANY INVENTORY NUMBER INPUT?                  
         BE    VR059                NOPE                                        
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),('NUMINVQ',BUFF),C',=,-'                       
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
         MVC   NTIMES,DMCB+4                                                    
*                                                                               
         MVI   COUNTER,0                                                        
         LA    R3,BUFF                                                          
         LA    R4,FRINVTB                                                       
*                                                                               
VR044    DS    0H                                                               
         CLC   COUNTER,NTIMES      LOOPED THRU ENOUGH TIMES YET?                
         BNL   VR048                YES                                         
*                                                                               
         CLI   0(R3),0             MUST HAVE A START INVENTORY NUMBER           
         BE    INVLFLD                                                          
         CLI   0(R3),4             MAX L(INVENTORY #) = 4                       
         BH    INVLFLD                                                          
         CLI   1(R3),4             MAX L(INVENTORY #) = 4                       
         BH    INVLFLD                                                          
*                                                                               
         MVC   0(4,R4),12(R3)      MOVE INPUT INTO TABLE                        
         MVC   4(4,R4),22(R3)                                                   
         CLI   1(R3),0             IF IT ISN'T A RANGE,                         
         BNE   *+10                                                             
         MVC   4(4,R4),0(R4)        COPY START INV NUMBER TO END                
*                                                                               
         LA    R3,32(R3)           BUMP TO NEXT SCANNER ENTRY                   
         LA    R4,L'FRINVRGE(R4)   BUMP TO NEXT SLOT IN TABLE                   
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
         B     VR044                                                            
*                                                                               
VR048    DS    0H                                                               
         MVI   0(R4),EOT                                                        
         OI    4(R2),X20                                                        
*                                                                               
VR059    DS    0H                                                               
*                                                                               
** VALIDATE EFFECTIVE DATE **                                                   
*                                                                               
         LA    R2,ISCFEFDH         EFFECTIVE DATE (RANGE)                       
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
*                                                                               
         XC    FREFFDB,FREFFDB     ASSUME NO EFFECTIVE DATES INPUT              
         CLI   5(R2),0             ANY INVENTORY NUMBER INPUT?                  
         BE    VR079                NOPE                                        
*                                                                               
         XC    PERVALB,PERVALB                                                  
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),PERVALB                                
         CLI   DMCB+4,0                                                         
         BE    VR074                                                            
         TM    DMCB+4,X04          "ONLY ONE DATE INPUT" IS OKAY                
         BO    VR074                                                            
         B     INVLFLD                                                          
*                                                                               
VR074    DS    0H                                                               
         LA    RF,PERVALB                                                       
         USING PERVALD,RF                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY                                 
         BNZ   INVLFLD                                                          
         TM    PVALASSM,PVALAED+PVALAEM+PVALAEY                                 
         BM    INVLFLD                                                          
*                                                                               
         MVC   FRSEFFDB,PVALBSTA   GET START AND                                
         MVC   FREEFFDB,PVALBEND    END EFFECTIVE DATES                         
         DROP  RF                                                               
*                                                                               
         OI    4(R2),X20                                                        
*                                                                               
VR079    DS    0H                                                               
*                                                                               
** OPTIONAL FROM DETAILS **                                                     
*                                                                               
         LA    R2,ISCFTRKH                                                      
*                                                                               
         MVI   FRCPYTRK,C'Y'       DEFAULT TO YES                               
         CLI   5(R2),0             IF NO INPUT                                  
         BE    VR084                DEFAULT TO YES                              
         CLI   8(R2),C'Y'          IF INPUT = C'Y'                              
         BE    VR084                WE'RE OKAY                                  
         MVI   FRCPYTRK,C'N'                                                    
         CLI   8(R2),C'N'          IF INPUT = C'N'                              
         BE    VR084                SET "COPY TRACKS" TO NO                     
         B     INVLFLD                                                          
VR084    EQU   *                                                                
         DS    0H                  COPY TEXT RECORDS                            
         LA    R2,ISCFTXTH                                                      
*                                                                               
         MVI   FRCPYTXT,C'Y'       DEFAULT TO YES                               
         CLI   5(R2),0             IF NO INPUT                                  
         BE    VR089                DEFAULT TO YES                              
         CLI   8(R2),C'Y'          IF INPUT = C'Y'                              
         BE    VR089                WE'RE OKAY                                  
         MVI   FRCPYTXT,C'N'                                                    
         CLI   8(R2),C'N'          IF INPUT = C'N'                              
         BE    VR089                SET "COPY TRACKS" TO NO                     
         B     INVLFLD                                                          
VR089    EQU   *                                                                
         EJECT                                                                  
*                                                                               
*------------------------ VALIDATE TO DETAILS ------------------------*         
*                                                                               
** STATION **                                                                   
*                                                                               
         LA    R2,ISCTSTAH         TO STATION FIELD                             
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         GOTO1 ANY                  MUST HAVE A STATION INPUT                   
         GOTO1 VALISTA              VALIDATE THE STATION INPUTTED               
         OI    4(R2),X20                                                        
*                                                                               
         MVC   TOSTTN,WORK         HOLD ONTO KEY FIELD INPUT                    
         CLI   TOSTTN+4,C' '                                                    
         BNE   *+8                                                              
         MVI   TOSTTN+4,C'T'                                                    
         CLI   WORK+40,C' '        CHECK FOR SATELLITES                         
         BE    *+10                                                             
         MVC   TOSTTN+4(1),WORK+40                                              
*                                                                               
VR90     DS    0H                                                               
         CLC   FRSTTN,TOSTTN                                                    
         BE    NSAMESTA                                                         
         EJECT                                                                  
*                                                                               
*--------------------- MISCELLANEOUS VALKEY TASKS --------------------*         
*                                                                               
VR100    DS    0H                                                               
                                                                                
         MVC   TMPSTTN,FRSTTN      MOVE IN FROM STATION                         
         XC    TMPINVN,TMPINVN                                                  
         XC    TMPBDATE,TMPBDATE                                                
         XC    TMPRSR,TMPRSR                                                    
         XC    TMPDSR,TMPDSR                                                    
         XC    TMPQLF,TMPQLF                                                    
         XC    TMPBTP,TMPBTP                                                    
         XC    TMPBOOK,TMPBOOK                                                  
         MVI   GOSUBN,BIK#         BUILD START KEY TO INVENTORY RECORD          
         GOTO1 AGOSUB                                                           
         MVC   STARTKEY,KEY         AND HOLD ONTO IT                            
*                                                                               
VRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*======================== PRINT REPORT ROUTINE =======================*         
                                                                                
* Will copy inventory items from one station to another in the offline          
*  environment.  Reports successes and failures of copying.                     
                                                                                
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
** BUILD FROM-TABLE **                                                          
*                                                                               
PR050    DS    0H                                                               
         CLI   STARTKEY,XFF        IF NO MORE "FROM" RECORDS,                   
         BE    PR600                WE'RE DONE                                  
*                                                                               
         MVI   NTIMES,MXFTNTRY     NTIMES = (MAX) # OF TIMES TO LOOP            
         MVI   COUNTER,0           COUNTER = LOOP COUNTER                       
         L     R4,AFROMTAB                                                      
         USING FROMTABD,R4                                                      
*                                                                               
         LR    R0,R4                                                            
         LA    R1,FROMTABX-FROMTAB                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR "FROM" TABLE                           
*                                                                               
         DS    0H                                                               
         MVC   KEY,STARTKEY                                                     
         GOTO1 HIGH                                                             
         B     PR060                                                            
*                                                                               
PR055    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
         CLC   COUNTER,NTIMES      IF FROM-TABLE FILLED TO CAPACITY,            
         BL    *+14                                                             
         MVC   STARTKEY,KEY         REMEMBER WHERE WE LEFT OFF                  
         B     PR200                AND ADD "TO" RECORDS TO FILE                
*                                                                               
         B     PR060               CONTINUE FILLING-UP "FROM" TABLE             
*                                                                               
PR060    DS    0H                                                               
         CLC   KEY(IKYSTAL),KEYSAVE                                             
         BE    *+12                                                             
         MVI   STARTKEY,XFF                                                     
         B     PR200                                                            
*                                                                               
*** RUN INVENTORY RECORD THROUGH FILTERS ***                                    
*                                                                               
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
*                                                                               
         MVC   TMPREP,RINVKREP     REP CODE                                     
*                                                                               
         CLI   RINVKRTP,C'M'       MASTER RECORD?                               
         BE    PR096                                                            
         CLI   RINVKRTP,C'S'       STATION RECORD?                              
         BE    PR096                                                            
         CLI   RINVKRTP,X'00'      HEADER RECORD?                               
         BE    PR070                                                            
         CLI   RINVKRTP,X'FF'      RATIONALE RECORD?                            
         BE    PR070                                                            
         CLI   RINVKRTP,C'Z'       RATE RECORD?                                 
         BE    PR070                                                            
*                                                                               
         CLI   ISCFBKTH+5,0        ANY BOOKTYPE FILTER?                         
         BE    PR070                                                            
*                                                                               
         ZIC   R1,ISCFBKTH+5       NUMBER OF BOOKTYPES                          
         LA    RF,ISCFBKT          BOOKTYPE(S)                                  
*                                                                               
PR065    CLC   0(1,RF),RINVKBTP    SAME BOOKTYPE?                               
         BE    PR070                                                            
         LA    RF,1(RF)            NEXT BOOKTYPE                                
         BCT   R1,PR065                                                         
         B     PR055                                                            
*                                                                               
PR070    DS    0H                  INVENTORY NUMBER FILTER                      
         CLI   FRINVTB,EOT          IF NO INVENTORY NUMBER FILTER               
         BE    PR074                 LET THIS PASS                              
*                                                                               
         LA    RF,FRINVTB                                                       
PR072    DS    0H                                                               
         CLI   0(RF),EOT                                                        
         BE    PR055                                                            
         CLC   RINVKINV,0(RF)                                                   
         BL    *+14                                                             
         CLC   RINVKINV,4(RF)                                                   
         BNH   PR074                                                            
         LA    RF,L'FRINVRGE(RF)                                                
         B     PR072                                                            
*                                                                               
PR074    DS    0H                                                               
         CLI   RINVKINV,X'F0'      LOWEST POSSIBLE INV #                        
         BL    PR055                                                            
*                                                                               
PR079    EQU   *                                                                
         DS    0H                  EFFECTIVE DATE FILTER                        
         OC    FREFFDB,FREFFDB      IF NO EFFECTIVE DATE FILTER,                
         BZ    PR089                 LET THIS PASS                              
*                                                                               
         CLC   RINVKSTD,FRSEFFDB    CHECK IF LESS THAN START                    
         BL    PR055                 YES, FAILED FILTER                         
         CLC   RINVKSTD,FREEFFDB    CHECK IF GREATER THAN END                   
         BH    PR055                 YES, FAILED FILTER                         
PR089    EQU   *                                                                
         CLI   RINVKRTP,C'Z'       RATE RECORD?                                 
         BE    PR095                                                            
*                                   COPY TRACKS (Y/N) FILTER                    
         CLI   FRCPYTRK,C'N'        IF TRACKS ARE TO BE COPIED,                 
         BNE   PR092X                LET THIS PASS                              
*                                                                               
         XC    IBLOCK,IBLOCK     GET BOOK TYPE                                  
         XC    OBLOCK,OBLOCK                                                    
         MVC   IBLOCK(1),RINVKRSR                                               
         MVC   IBLOCK+1(1),RINVKQLF                                             
         MVC   IBLOCK+3(1),RINVKBTP                                             
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'K',IBLOCK),OBLOCK,ACOMFACS                      
*                                                                               
         CLI   DMCB+4,0            ERRORS?                                      
         BE    PR055                                                            
*                                                                               
PR092X   EQU   *                                                                
*                                                                               
         DS    0H                  COPY TEXT (Y/N) FILTER                       
         CLI   FRCPYTXT,C'N'        IF TEXT ARE TO BE COPIED,                   
         BNE   PR095                 LET THIS PASS                              
         CLI   RINVKRTP,XFF         TEST IF TEXT/RATIONALE RECORD               
         BE    PR055                 IT IS--FAIL                                
*                                                                               
PR095    DS    0H                  INVENTORY RECORD PASSED FILTER               
         B     PR100                                                            
*                                                                               
PR096    LA    R2,ISCFMSTH                                                      
         CLI   ISCFMSTH+5,0                                                     
         BNE   PR097                                                            
         MVI   ISCFMST,C'N'        DON'T COPY MASTER/STATION RECORDS            
         OI    ISCFMSTH+6,X'80'                                                 
         B     PR055                                                            
*                                                                               
PR097    CLI   ISCFMST,C'Y'        COPY MASTER/STATION RECORDS?                 
         BE    PR099               YES                                          
         CLI   ISCFMST,C'N'                                                     
         BNE   INVLFLD                                                          
         B     PR055                                                            
*                                                                               
PR099    OI    MYFLAG,YESMSTA      THERE ARE MASTER/STATION RECORDS             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
PR100    DS    0H                  ADD "FROM" RECD TO FROM-TABLE                
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
         MVC   FTINV,RINVKINV                                                   
         MVC   FTSTD,RINVKSTD                                                   
         MVC   FTRSR,RINVKRSR                                                   
         MVC   FTDSR,RINVKDSR                                                   
         MVC   FTQLF,RINVKQLF                                                   
         MVC   FTBTP,RINVKBTP                                                   
         MVC   FTBK_TXT,RINVKBK                                                 
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
** PROCESS ENTRIES IN "FROM" TABLE **                                           
*                                                                               
* Process "FROM" TABLE by going through its entries and note the status         
*  of their "TO" counterparts on the file, i.e. whether they already            
*  exist or not, or if they are marked for deletion, etc.                       
                                                                                
PR200    DS    0H                                                               
         MVC   NFTNTRYS,COUNTER    NFTNTRYS = # OF "FROM" ENTRIES               
         MVI   COUNTER,0           RESET COUNTER                                
                                                                                
         L     R4,AFROMTAB                                                      
         USING FROMTABD,R4                                                      
         MVC   TMPSTTN,TOSTTN                                                   
         MVC   TMPINVN,FTINV                                                    
         MVC   TMPBDATE,FTSTD                                                   
         MVC   TMPRSR,FTRSR                                                     
         MVC   TMPDSR,FTDSR                                                     
         MVC   TMPQLF,FTQLF                                                     
         MVC   TMPBTP,FTBTP                                                     
         MVC   TMPBOOK,FTBK_TXT                                                 
         MVI   GOSUBN,BIK#                                                      
         GOTO1 AGOSUB                                                           
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
*                                                                               
         DS    0H                  READ FILE SEQUENTIALLY FOR "TO" RECS         
         OI    DMINBTS,X08          (READ DELETED AS WELL)                      
         GOTO1 HIGH                                                             
         NI    DMINBTS,XFF-X08                                                  
         B     PR220                                                            
*                                                                               
PR215    DS    0H                                                               
         OI    DMINBTS,X08          (READ SEQ FOR DELETED AS WELL)              
         GOTO1 SEQ                                                              
         NI    DMINBTS,XFF-X08                                                  
*                                                                               
PR220    DS    0H                                                               
         CLC   KEY(IKYSTAL),KEYSAVE  ANY MORE "TO" RECORDS ON FILE?             
         BNE   PR300                  NO, GO ADD THEM NOW                       
*                         SET UP "KEY" TO TEST AGAINST "FROM" TABLE             
         MVC   RITSTKEY(RINVKSPR-RINVKINV),RINVKINV                             
         MVC   RITSTKEY+RINVKSPR-RINVKINV(FTKEYL2),RINVKRTP                     
*                                                                               
PR230    DS    0H                                                               
         CLC   FTINV(FTKEYL),RITSTKEY                                           
         BE    PR250               RECORD ALREADY EXISTS                        
*                                  "FROM" ENTRY GT "TO" RECORD                  
         BH    PR215               GET NEXT RECORD                              
*                                  "FROM" ENTRY LT "TO" RECORD                  
         LA    R4,FROMTABL(R4)      BUMP TO NEXT "FROM" ENTRY                   
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER           INCREMENT LOOP COUNTER                      
         CLM   R1,1,NFTNTRYS        WENT THROUGH ALL "FROM" ENTRIES?            
         BNL   PR300                 YES, GO ADD "TO" RECORDS                   
         B     PR230                 NO, GO CHECK FOR NEXT ACTION               
*                                                                               
PR250    DS    0H                  "FROM" ENTRY = "TO" RECORD                   
         OI    FTFLAG1,FTF1RXST     FLAG THAT "TO" RECORD EXISTS                
*===================================================================            
         CLI   FTRSR,X'00'         HEADER RECORD?                               
         BNE   PR255               NO                                           
         BAS   RE,YESHEAD          "TO" HEADER RECORD ALREADY EXISTS            
         CLM   R1,1,NFTNTRYS        WENT THROUGH ALL "FROM" ENTRIES?            
         BNL   PR300               YES                                          
         B     PR215                                                            
*===================================================================            
PR255    DS    0H                                                               
         TM    RINVKEY+L'RINVKEY,X80                                            
         BZ    *+8                                                              
         OI    FTFLAG1,FTF1RDEL     FLAG THAT "TO" RECD MARKED DELETED          
         B     PR215                GET NEXT RECORD FROM FILE                   
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
** CREATE "TO" RECORDS AND ADD TO FILE **                                       
*                                                                               
PR300    DS    0H                                                               
         L     R4,AFROMTAB                                                      
         USING FROMTABD,R4                                                      
*=================================================================              
         BAS   RE,COPYREC          ADD RECORDS FOR "TO" STATION                 
*==================================================================             
         L     R4,AFROMTAB                                                      
         LA    R5,P                                                             
         USING PRTLINED,R5                                                      
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
         LA    R2,PLKEYA                                                        
*                                                                               
         MVC   0(4,R2),TOSTTN       STATION                                     
         LA    R2,4(R2)                                                         
                                                                                
         CLI   TOSTTN+4,C'T'                                                    
         BE    *+18                                                             
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),TOSTTN+4     STATION'S SATELLITE                         
         LA    R2,2(R2)                                                         
*                                                                               
         BAS   RE,INSCOMMA                                                      
*                                                                               
         LA    RF,FTINV            INVENTORY NUMBER                             
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
*                                                                               
         BAS   RE,INSCOMMA                                                      
         MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(X'83',FTSTD),(5,WORK),0                             
         ZIC   R1,DMCB+4                                                        
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK        EFFECTIVE DATE                              
         LA    R2,1(R2,R1)                                                      
*                                                                               
         CLI   FTRSR,0             CHECK IF "TO" RECORD IS A HEADER,            
         BE    PR360                                                            
         CLI   FTRSR,XFF            TEXT RECORD,                                
         BE    PR365                                                            
         CLI   FTRSR,C'M'           MARKET FACT,                                
         BE    PR370                                                            
         CLI   FTRSR,C'S'           OR A STATION FACT                           
         BE    PR375                                                            
         CLI   FTRSR,C'Z'          RATE RECORD                                  
         BE    PR376                                                            
*                                                                               
         DS    0H                  ELSE, IT IS A TRACK RECORD                   
         BAS   RE,INSCOMMA                                                      
                                                                                
         MVI   OURBYTE,1                                                        
         MVC   TMPSRC,=C'NSI'                                                   
         MVC   0(3,R2),TMPSRC                                                   
         LA    R2,3(R2)                                                         
                                                                                
         BAS   RE,INSCOMMA                                                      
         MVC   0(1,R2),1(R3)                                                    
         LA    R2,1(R2)                                                         
                                                                                
         MVC   TMPBOOK,FTBK_TXT                                                 
         MVI   TMPBTYP,0                                                        
         MVI   TMPWKN,0                                                         
         MVI   GOSUBN,TBK#                                                      
         GOTO1 AGOSUB                                                           
         ZIC   R1,OURBYTE                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK                                                    
                                                                                
         B     PR379                                                            
                                                                                
PR360    DS    0H                  "TO" RECORD IS AN INV HEADER                 
         BAS   RE,INSCOMMA                                                      
         MVC   0(8,R2),=C'(HEADER)'                                             
         LA    R2,8(R2)                                                         
         B     PR379                                                            
                                                                                
PR365    DS    0H                  "TO" RECORD IS A TEXT RECORD                 
         BAS   RE,INSCOMMA                                                      
         MVC   0(9,R2),=C'TXT LIN# '                                            
         LA    R2,9(R2)                                                         
         ZICM  R1,FTBK_TXT,(3)                                                  
         EDIT  (R1),(6,(R2)),ALIGN=LEFT,ZERO=NOBLANK,COMMAS=YES                 
         AR    R2,R0                                                            
         B     PR379                                                            
                                                                                
PR370    DS    0H                  "TO" RECORD IS A MARKET FACT                 
         BAS   RE,INSCOMMA                                                      
         MVC   0(10,R2),=C'(MKT TEXT)'                                          
         LA    R2,11(R2)                                                        
         ZICM  R1,FTBK_TXT,(3)                                                  
         EDIT  (R1),(6,(R2)),ALIGN=LEFT,ZERO=NOBLANK,COMMAS=YES                 
         AR    R2,R0                                                            
         B     PR379                                                            
                                                                                
PR375    DS    0H                  "TO" RECORD IS A STATION FACT                
         BAS   RE,INSCOMMA                                                      
         MVC   0(11,R2),=C'(STTN TEXT)'                                         
         LA    R2,12(R2)                                                        
         ZICM  R1,FTBK_TXT,(3)                                                  
         EDIT  (R1),(6,(R2)),ALIGN=LEFT,ZERO=NOBLANK,COMMAS=YES                 
         AR    R2,R0                                                            
         B     PR379                                                            
                                                                                
PR376    DS    0H                  "TO" RECORD IS A RATE RECORD                 
         BAS   RE,INSCOMMA                                                      
         MVC   0(10,R2),=C'RATE, YR: '                                          
         LA    R2,10(R2)                                                        
         ZICM  R1,FTBK_TXT,(3)                                                  
         EDIT  (R1),(4,(R2)),ALIGN=LEFT,ZERO=BLANK,COMMAS=YES                   
         AR    R2,R0                                                            
         B     PR379                                                            
                                                                                
PR379    EQU   *                                                                
*                                                                               
         MVC   PLSIGN,=CL3'==>'                                                 
                                                                                
         TM    FTFLAG1,HEADXST     HEADER RECORD ALREADY EXIST?                 
         BO    PR420               YES                                          
         TM    FTFLAG1,FTF1OVER                                                 
         BO    PR430                                                            
         TM    FTFLAG1,FTF1RDEL+FTF1RXST                                        
         BO    PR400                                                            
         TM    FTFLAG1,FTF1RXST                                                 
         BO    PR410                                                            
                                                                                
         MVC   PLSTATUS,=CL16'RECORD  ADDED - '                                 
         MVC   PLCOMMNT,=CL25'COPY SUCCESSFUL'                                  
         LA    R1,1                                                             
         A     R1,CNTCOPY                                                       
         ST    R1,CNTCOPY                                                       
         B     PR450                                                            
                                                                                
PR400    DS    0H                                                               
         MVC   PLSTATUS,=CL16'NOT COPIED    - '                                 
         MVC   PLCOMMNT,=CL25'RECORD CURRENTLY DELETED'                         
         LA    R1,1                                                             
         A     R1,CNTCOPY                                                       
         ST    R1,CNTCOPY                                                       
         B     PR450                                                            
                                                                                
PR410    DS    0H                                                               
         MVC   PLSTATUS,=CL16'NOT  COPIED   - '                                 
         MVC   PLCOMMNT,=CL25'"TO" RECORD ALREADY EXIST'                        
         LA    R1,1                                                             
         A     R1,CNTNCPY                                                       
         ST    R1,CNTNCPY                                                       
         B     PR450                                                            
                                                                                
PR420    DS    0H                                                               
         MVC   PLSTATUS,=CL16'NOT  COPIED   - '                                 
         MVC   PLCOMMNT,=CL25'HEADER ALREADY EXISTS'                            
         LA    R1,1                                                             
         A     R1,CNTNCPY                                                       
         ST    R1,CNTNCPY                                                       
         B     PR450                                                            
                                                                                
PR430    DS    0H                                                               
         MVC   PLSTATUS,=CL16'NOT  COPIED   - '                                 
         MVC   PLCOMMNT,=CL25'OVERLAPPING EFFEC. DATES'                         
         LA    R1,1                                                             
         A     R1,CNTNCPY                                                       
         ST    R1,CNTNCPY                                                       
         B     PR450                                                            
                                                                                
PR450    DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         DROP  R5                                                               
*                                                                               
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER          INCREMENT COUNTER                            
                                                                                
         MVC   PRVFTKEY,FTKEY      SAVE KEY OF THIS "FROM-TABLE" ENTRY          
         LA    R4,FROMTABL(R4)     BUMP "FROM" TABLE POINTER                    
         B     PR320                                                            
*                                                                               
PR500    DS    0H                                                               
         B     PR050                                                            
                                                                                
*                                                                               
PR600    DS    0H                                                               
         MVC   P3(37),=C'NUMBER OF RECORDS COPIED SUCCESSFULLY'                 
         L     R1,CNTCOPY                                                       
         EDIT  (R1),(8,P3+40),ZERO=NOBLANK,COMMAS=YES                           
                                                                                
         MVC   P4(37),=C'NUMBER OF UNSUCCESSFUL COPYS         '                 
         L     R1,CNTNCPY                                                       
         EDIT  (R1),(8,P4+40),ZERO=NOBLANK,COMMAS=YES                           
                                                                                
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     PRX                                                              
*                                                                               
PRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*===================================================================            
YESHEAD  DS    0H                                                               
*                                                                               
         USING FROMTABD,R4                                                      
         MVC   SVINVDT,0(R4)       SAVE HEADER INV. AND DATE                    
         B     *+8                                                              
*                                                                               
YESHEAD5 OI    FTFLAG1,HEADXST      FTFLAG1 HEADER RECORD EXISTS                
         LA    R4,FROMTABL(R4)     NEXT INV. RECORD                             
*                                                                               
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER           INCREMENT LOOP COUNTER                      
         CLM   R1,1,NFTNTRYS        WENT THROUGH ALL "FROM" ENTRIES?            
         BNL   YESHEADX                                                         
*                                                                               
         CLC   SVINVDT,0(R4)       SAME INV. NUM AND DATE AS HEADER?            
         BE    YESHEAD5                                                         
*                                                                               
YESHEADX BR    RE                                                               
         EJECT                                                                  
*==================================================================             
COPYREC  NTR1                                                                   
*                                                                               
         MVI   ADDCOUNT,0          # OF ADDED RECORDS                           
*                                                                               
         OC    PREVINV,PREVINV                                                  
         BZ    CR010                                                            
*                                                                               
         L     R4,AFROMTAB         MARK LEFT OVER RECORDS FROM                  
         USING FROMTABD,R4         PREVIOUS 'FROM' TABLE FOR                    
         ZIC   RF,NFTNTRYS         OVERLAPPING DATES                            
         LTR   RF,RF                                                            
         BZ    COPYRECX                                                         
*                                                                               
CR02     DS    0H                                                               
         CLI   FTRSR,C'M'           MASTER RECORD?                              
         BE    CR05                                                             
         CLI   FTRSR,C'S'           STATION RECORD?                             
         BE    CR05                                                             
         CLC   PREVINV,0(R4)       PREVIOUS 'FROM' INFO FOR THIS TABLE          
         BNE   *+8                                                              
         OI    FTFLAG1,FTF1OVER                                                 
*                                                                               
CR05     LA    R4,FROMTABL(R4)                                                  
         BCT   RF,CR02                                                          
*                                                                               
CR010    XC    PREVINV,PREVINV                                                  
         L     R4,AFROMTAB                                                      
         MVC   AIO,AIO2                                                         
         XC    COUNTER,COUNTER                                                  
         CLI   NFTNTRYS,0                                                       
         BE    COPYRECX                                                         
*                                                                               
COPYREC2 TM    FTFLAG1,HEADXST     HEADER RECORD ALREADY EXIST?                 
         BO    COPYREC5            YES - DON'T ADD "TO" RECORD                  
         TM    FTFLAG1,FTF1RDEL+FTF1RXST                                        
         BO    COPYREC5            DON'T ADD "TO" RECORD                        
         TM    FTFLAG1,FTF1RXST                                                 
         BO    COPYREC5            DON'T ADD "TO" RECORD                        
         TM    FTFLAG1,FTF1OVER                                                 
         BO    COPYREC5            DON'T ADD "TO" RECORD                        
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,RINVKTYQ   RECORD TYPE                                  
         MVC   RINVKREP,TMPREP     'FROM' REP                                   
         MVC   RINVKSTA,FRSTTN     'FROM' STATION                               
         MVC   RINVKINV,FTINV      'FROM' INVENTORY NUMBER                      
         MVC   RINVKSTD,FTSTD      'FROM' EFFEC. DATE                           
         MVC   RINVKRSR,FTRSR      'FROM' RTG SOURCE                            
         MVC   RINVKDSR,FTDSR      'FROM' DATA SOURCE                           
         MVC   RINVKQLF,FTQLF      'FROM' QUALIFIER                             
         MVC   RINVKBTP,FTBTP      'FROM' BOOKTYPE                              
         MVC   RINVKBK,FTBK_TXT    'FROM' BOOK OR TEXT NUMBER                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLI   RINVKRTP,X00        HEADER RECORD?                               
         BNE   COPYR3A                                                          
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   INVLFLD                                                          
         USING RINVPEL,R3                                                       
*                                                                               
         MVC   STRTCMPR,RINVPEFF   START 'FROM' DATE COMPRESSED                 
         MVC   ENDCMPR,RINVPEFF+2  END 'FROM' DATE COMPRESSED                   
         DROP  R3                                                               
*                                                                               
         XC    OVERKEY,OVERKEY                                                  
         MVC   OVERKEY,KEY                                                      
*                                                                               
         BAS   RE,DATECHK          CHECK FOR OVERLAPPING EFFEC DATES            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,OVERKEY                                                      
         TM    MYFLAG,FTF1OVER     OVERLAPPING DATES?                           
         BO    COPYREC5                                                         
*                                                                               
COPYR3A  XC    FROMKEY,FROMKEY                                                  
         MVC   FROMKEY,KEY         SAVE 'FROM' RECORD KEY                       
         L     R6,AIO                                                           
*                                                                               
*=================UPDATE ELEMENTS===========================                    
*                                                                               
         BAS   RE,BUILD75          BUILD THE X'75' ELEMENT                      
         BAS   RE,CHNGEF           CHANGE THE X'EF' ELEMENT                     
*                                                                               
*===========================================================                    
         MVC   RINVKSTA,TOSTTN     "TO" STATION                                 
*                                                                               
         CLI   RINVKRTP,C'M'       MASTER RECORD?                               
         BE    COPYREC3                                                         
         CLI   RINVKRTP,C'S'       STATION RECORD                               
         BE    COPYREC3                                                         
         CLI   RINVKRTP,X00        HEADER RECORD?                               
         BE    COPYREC3                                                         
         CLI   RINVKRTP,XFF        TEXT RECORD?                                 
         BE    COPYREC3                                                         
         CLI   RINVKRTP,C'Z'       RATE RECORD?                                 
         BE    COPYREC3                                                         
*                                                                               
         BAS   RE,UPELEM03         MARK THE X'03' ELEMENT AS COPIED             
*                                                                               
COPYREC3 DS    0H                                                               
         L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'RINVKEY),0(R6)                                             
*                                                                               
         NI    DMINBTS,XFF-X40     TURN OF  31 BIT ADD BYTE                     
*                                                                               
         GOTO1 ADDREC              GO AND ADD NEW "TO" STATION RECORD           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RINVKEY),0(R6)                                             
*                                                                               
*================= ADD PASSIVE DAYPART POINTERS ===================             
*                                                                               
         CLI   RINVKRTP,X00        HEADER RECORD?                               
         BNE   COPYREC5                                                         
*                                                                               
         GOTO1 HIGH                                                             
         MVC   BSVDA,KEY+28        SAVED RECORD ADDRESS                         
*                                                                               
         BAS   RE,INVPTR           BUILD PASSIVE KEYS                           
         BAS   RE,NWPT             ADD PASSIVE POINTERS                         
*                                                                               
*==================================================================             
*                                                                               
COPYREC5 LA    R4,FROMTABL(R4)      BUMP TO NEXT "FROM" ENTRY                   
         NI    MYFLAG,XFF-FTF1OVER                                              
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER           INCREMENT LOOP COUNTER                      
         CLC   COUNTER,NFTNTRYS     WENT THROUGH ALL "FROM" ENTRIES?            
         BL    COPYREC2              YES, GO ADD "TO" RECORDS                   
*                                                                               
COPYRECX DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                CHECK FOR OVERLAPPING EFFEC DATES                    *         
*---------------------------------------------------------------------*         
DATECHK  NTR1                                                                   
*                                                                               
         MVC   AIO,AIO3                                                         
*                                                                               
         NI    MYFLAG,XFF-FTF1OVER                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,RINVKTYQ   RECORD TYPE                                  
         MVC   RINVKREP,TMPREP     'FROM' REP                                   
         MVC   RINVKSTA,TOSTTN     'TO' STATION                                 
         MVC   RINVKINV,FTINV      'FROM' INVENTORY NUMBER                      
*                                                                               
DCHK05   GOTO1 HIGH                                                             
         B     DCHK10                                                           
*                                                                               
DCHK07   GOTO1 SEQ                                                              
*                                                                               
DCHK10   DS    0H                                                               
         CLC   KEY(IKYINVL),KEYSAVE                                             
         BNE   DCHKX                                                            
*                                                                               
         CLI   RINVKRTP,X00        HEADER RECORD?                               
         BNE   DCHK07                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   INVLFLD                                                          
         USING RINVPEL,R3                                                       
*                                                                               
         OC    ENDCMPR,ENDCMPR     ANY END 'FROM' DATE?                         
         BZ    DCHK30              NO                                           
         OC    RINVPEFF+2(2),RINVPEFF+2    ANY END 'TO' DATE?                   
         BZ    DCHK40              NO                                           
*                                                                               
DCHK20   CLC   STRTCMPR,RINVPEFF+2 'FROM' STARTS AFTER 'TO' ENDS?               
         BH    DCHK07                                                           
         CLC   ENDCMPR,RINVPEFF    'FROM' ENDS BEFORE 'TO' BEGINS?              
         BL    DCHK07                                                           
         B     DCHK50              OVERLAPPING DATES                            
*                                                                               
DCHK30   OC    RINVPEFF+2(2),RINVPEFF+2     ANY END 'TO' DATE?                  
         BZ    DCHK50              OVERLAPPING DATES                            
*                                                                               
         CLC   STRTCMPR,RINVPEFF+2 'FROM' STARTS AFTER 'TO' ENDS?               
         BNH   DCHK50              OVERLAPPING DATES                            
         B     DCHK07                                                           
*                                                                               
DCHK40   CLC   RINVPEFF(2),ENDCMPR 'FROM' STARTS AFTER 'TO' ENDS?               
         BNH   DCHK50              OVERLAPPING DATES                            
         B     DCHK07                                                           
*                                                                               
DCHK50   OI    MYFLAG,FTF1OVER                                                  
*                                                                               
DCHK60   DS    0H                  OVERLAPPING DATES                            
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY,OVERKEY         RESTORE KEY                                  
         L     R4,AFROMTAB                                                      
         USING FROMTABD,R4                                                      
         ZIC   RF,NFTNTRYS                                                      
*                                                                               
DCHK65   DS    0H                                                               
         CLC   FTKEY(L'RINVKINV+L'RINVKSTD),RINVKINV                            
         BNE   *+14                                                             
         OI    FTFLAG1,FTF1OVER    OVERLAPPING DATES                            
         MVC   PREVINV,0(R4)       SAVE 'FROM' INFO FOR NEXT TABLE              
*                                                                               
         LA    R4,FROMTABL(R4)                                                  
         BCT   RF,DCHK65                                                        
*                                                                               
DCHKX    DS    0H                                                               
         MVC   AIO,AIO2                                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
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
         MVI   RICPFRPR,C'S'       S=SCOPY                                      
         MVC   RICPFRKY,FROMKEY    'FROM' KEY                                   
*                                                                               
         LH    R3,L'RINVKEY(R6)    RECORD LENGTH                                
         AR    R3,R6               A(1ST AVAIL. ELEMENT POSITION)               
*                                                                               
         GOTO1 =V(RECUP),DMCB,(X'02',(R6)),ELEM,(R3)  ADD X'75' ELEM            
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
         MVI   RINVFRPR,C'C'       MARK AS COPIED                               
*                                                                               
UPEL03X  DS    0H                                                               
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              CREATE NEW PASSIVE POINTER                             *         
*---------------------------------------------------------------------*         
INVPTR   NTR1                                                                   
*                                                                               
         L     R2,AIO                                                           
         USING RINVREC,R2                                                       
         LA    R5,BLOCK                                                         
         USING RIDPKEY,R5                                                       
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R6,6                                                             
         LA    R3,RINVDP                                                        
*                                                                               
INVPTR1  MVI   RIDPKTYP,RIDPKTYQ                                                
         MVC   RIDPKREP,RINVKREP                                                
         MVC   RIDPKSTA,RINVKSTA                                                
         MVC   RIDPKDPT,0(R3)                                                   
         MVC   RIDPKINV,RINVKINV                                                
         MVC   RIDPKSTD,RINVKSTD                                                
*                                                                               
*  IF SELF ASSIGNED GET NEXT DAYPART                                            
*  ONLY COMPUTER GENERATED NUMBERS GET THE DAY,QTR HOUR                         
*  AND THE LENGTH FILLED IN.                                                    
*                                                                               
         TM    RINVSTAT,X'80'                                                   
         BO    INVPTR20            BIT ON SELF ASSIGNED                         
*                                                                               
         MVC   RIDPKDAY,RINVOINV+1   MOVE DAY CODE,                             
         MVC   RIDPKQTR,RINVOINV     QUARTER HOUR,                              
         MVC   RIDPKLEN,RINVOINV+2   AND PROGRAM LENGTH TO KEY                  
*                                                                               
         LA    RE,EFFDAT           SPECIAL DAYPARTS                             
INVPTR10 CLI   0(RE),X'FF'                                                      
         BE    INVPTR20                                                         
         CLC   0(1,R3),0(RE)                                                    
         BE    INVPTR15                                                         
         LA    RE,1(RE)                                                         
         B     INVPTR10                                                         
*                                                                               
INVPTR15 XC    RIDPKDAY,RIDPKDAY                                                
         MVC   RIDPKDTE,RINVPEFF                                                
*                                                                               
INVPTR20 LA    R3,1(R3)            NEXT DAYPART CODE                            
         CLI   0(R3),X'40'                                                      
         BNH   INVPTX                                                           
         LA    R5,32(R5)                                                        
         BCT   R6,INVPTR1          DO NEXT POINTER                              
*                                                                               
INVPTX   DS    0H                                                               
         B     XIT                                                              
         DROP  R2,R5                                                            
*                                                                               
*  THESE DAYPARTS GET A DAY CODE, QUARTER HOUR, AND PROGRAM LENGTH              
DAYCOD   DC    C'MDKNPOUXYWZ',X'FF'                                             
*                                                                               
*  THESE DAYPARTS GET EFFECTIVE DATE, QUARTER HOUR, AND PROGRAM LENGTH          
EFFDAT   DC    C'VSJ',X'FF'                                                     
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              ROUTINE TO ADD PASSIVE POINTERS                        *         
*---------------------------------------------------------------------*         
NWPT     NTR1                                                                   
         LA    R2,BLOCK                                                         
*                                                                               
NWPT1    CLI   0(R2),0             END OF PASSIVE POINTERS                      
         BE    XIT                 END OF LIST                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RIDPKEY),0(R2)                                             
*                                                                               
         OI    DMINBTS,X08                                                      
         GOTO1 HIGH                                                             
         NI    DMINBTS,XFF-X08                                                  
*                                                                               
         CLC   KEYSAVE(L'RIDPKEY),KEY                                           
         BE    NWPT3                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         GOTO1 ADD                                                              
         B     NWPT4                                                            
*                                                                               
NWPT3    XC    KEY,KEY                                                          
         MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         GOTO1 WRITE                                                            
*                                                                               
NWPT4    LA    R2,32(R2)                                                        
         B     NWPT1                                                            
*                                                                               
*---------------------------------------------------------------------*         
                                                                                
* Little routine to insert a comma into the print line.                         
*  R2-->next output location in the print line.                                 
                                                                                
INSCOMMA DS    0H                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BR    RE                                                               
***********************************************************************         
         TITLE 'RERMP27 - S(TATION) COPY REPORT (REPORT SPECS)'                 
***********************************************************************         
*============================ REPORT SPECS ===========================*         
                                                                                
HEDSPECS SSPEC H1,1,C'INVENTORY STATION-COPY'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'SCOPY REPORT'                                            
         SSPEC H2,52,C'------------'                                            
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,01,C'FROM STATION'                                            
         SSPEC H4,23,C'TO STATION'                                              
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         DC    X'00'                                                            
***********************************************************************         
         TITLE 'RERMP27 - S(TATION) COPY REPORT (REPORT HOOK)'                  
***********************************************************************         
*========================== HEADLINE ROUTINE =========================*         
                                                                                
HDHOOK   NTR1                                                                   
         DS    0H                  PRINT "FROM" STATION                         
         MVC   H4+14(4),FRSTTN                                                  
         CLI   FRSTTN+4,C'T'                                                    
         BE    *+14                                                             
         MVI   H4+18,C'-'                                                       
         MVC   H4+19(1),FRSTTN+4                                                
                                                                                
         DS    0H                  PRINT "TO" STATION                           
         MVC   H4+34(4),TOSTTN                                                  
         CLI   TOSTTN+4,C'T'                                                    
         BE    *+14                                                             
         MVI   H4+38,C'-'                                                       
         MVC   H4+39(1),TOSTTN+4                                                
                                                                                
         LA    R5,H5                                                            
                                                                                
         DS    0H                  PRINT INVENTORY # FILTER                     
         CLI   FRINVTB,EOT                                                      
         BE    HDHK045X                                                         
         MVC   0(12,R5),=C'INVENTORY #S'                                        
         ZICM  R1,ISCFINVH+5,(1)                                                
         BZ    HDHK045T                                                         
         BCTR  R1,0                                                             
         EXMVC R1,14(R5),ISCFINV                                                
HDHK045T LA    R5,L'H1(R5)                                                      
HDHK045X EQU   *                                                                
                                                                                
         DS    0H                  PRINT EFFECTIVE DATE FILTER                  
         OC    FREFFDB,FREFFDB                                                  
         BZ    HDHK055X                                                         
         MVC   0(12,R5),=C'EFFCTV DATES'                                        
         ZICM  R1,ISCFEFDH+5,(1)                                                
         BZ    HDHK055T                                                         
         BCTR  R1,0                                                             
         EXMVC R1,14(R5),ISCFEFD                                                
HDHK055T LA    R5,L'H1(R5)                                                      
HDHK055X EQU   *                                                                
                                                                                
         DS    0H                  PRINT BOOKTYPE FILTER                        
         OC    ISCFBKT,ISCFBKT                                                  
         BZ    HDHK065X                                                         
         MVC   0(12,R5),=C'BOOKTYPE(S) '                                        
         ZICM  R1,ISCFBKTH+5,(1)                                                
         BZ    HDHK065T                                                         
         BCTR  R1,0                                                             
         EXMVC R1,14(R5),ISCFBKT                                                
HDHK065T LA    R5,L'H1(R5)                                                      
HDHK065X EQU   *                                                                
                                                                                
         MVI   0(R5),0             PRINT EXTRA LINE BETW/ HDR AND DATA          
                                                                                
HDHOOKX  B     XIT                                                              
***********************************************************************         
         TITLE 'RERMP27 - S(TATION) COPY REPORT (MISCELLANEOUS)'                
***********************************************************************         
*===================== SUBROUTINE POOL INTERFACE =====================*         
                                                                                
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
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== MISCELLANEOUS STUFF ========================*         
*------------------------- TEST CHANGE OF KEY ------------------------*         
KYCHNGED DS    0H                                                               
         TM    4(R2),X20                                                        
         BZ    KYCH10                                                           
         TM    4(R2),X80                                                        
         BZR   RE                                                               
KYCH10   OI    CHNGFLG1,CF1KEY                                                  
         BR    RE                                                               
*                                                                               
*========================= VALIDATION ERRORS =========================*         
                                                                                
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
                                                                                
*----------------- EXIT AND DISPLAY MESSAGE ROUTINES -----------------*         
                                                                                
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (LTORG && CONSTANTS)'           
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
                                                                                
                                                                                
DEMLSTDC DC    X'00D90000E20000D700FF'   DEMO LIST W/ DELIMITER                 
                                                                                
                                                                                
MAINL    EQU   *-RMP27                                                          
         DS    0CL(X'2000'-MAINL+1)                                             
***********************************************************************         
                                                                                
                                                                                
         DROP  R7,R8,R9,RA,RB,RC                                                
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBR01)'                       
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
         ORG   RMP27+X'2000'                                                    
         ORG                                                                    
SUBR01Q  EQU   (((*-RMP27+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP27+SUBR01Q                                                    
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBR01--BIK#)'                 
*------------------------ BUILD INVENTORY KEY ------------------------*         
*                                                                               
BINVKEY  DS    0H                                                               
         XC    KEY,KEY                                                          
IVK      USING REINVRCD,KEY                                                     
         MVI   IVK.RINVKTYP,RINVKTYQ                                            
         MVC   IVK.RINVKREP,AGENCY                                              
         MVC   IVK.RINVKSTA,TMPSTTN                                             
         MVC   IVK.RINVKINV,TMPINVN                                             
         MVC   IVK.RINVKSTD,TMPBDATE                                            
         MVC   IVK.RINVKRSR,TMPRSR                                              
         MVC   IVK.RINVKDSR,TMPDSR                                              
         MVC   IVK.RINVKQLF,TMPQLF                                              
         MVC   IVK.RINVKBTP,TMPBTP                                              
         MVC   IVK.RINVKBK,TMPBOOK                                              
         DROP  IVK                                                              
         B     XIT_01                                                           
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBR01--GIR#)'                 
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBR01--VSRC#)'                
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBR01--TBK#)'                 
*--------------------------- TRANSLATE BOOK --------------------------*         
                                                                                
* At entry,                                                                     
*   TMPBOOK = book to translate.                                                
*   TMPBTYP = book type, if applicable.                                         
*   TMPWKN  = week number, if applicable.                                       
* At exit,                                                                      
*   OURBYTE = L(converted book),                                                
*   WORK    = converted book.                                                   
                                                                                
TRSLTBK  DS    0H                                                               
                                                                                
         MVC   WORK,SPACES                                                      
         OC    TMPBOOK,TMPBOOK     IF BOOK IS NULLS,                            
         BZ    TBKX                 EXIT NOW                                    
                                                                                
*                                                                               
         LA    R2,WORK                                                          
         XC    DUB,DUB                                                          
         MVC   DUB(L'TMPBOOK),TMPBOOK                                           
                                                                                
         GOTO1 DATCON,DMCB,(X'83',DUB),(6,(R2))                                 
         MVC   3(2,R2),4(R2)       REMOVE SLASH                                 
         LA    R2,5(R2)                                                         
         MVI   0(R2),C' '                                                       
*                                                                               
         CLI   FTBTP,0                                                          
         BE    TBK029                                                           
*                                                                               
         GOTO1 GETBTYPE,DMCB,(FTBTP,0)                                          
         CLI   DMCB,0                                                           
         BE    TBK029                                                           
                                                                                
         MVI   0(R2),C'('                                                       
                                                                                
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),DMCB+2                                                   
                                                                                
         LA    R2,2(R1,R2)                                                      
         MVI   0(R2),C')'                                                       
         LA    R2,1(R2)                                                         
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBR01--LTORG && CONST+        
               ANTS)'                                                           
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBR01--MISC STUFF)'           
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
         DS    0H                                                               
                                                                                
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBRXM)'                       
***********************************************************************         
*==================== EXIT WITH MESSAGE ROUTINES =====================*         
                                                                                
* Exits which leave RMP27 entirely and displays a message go through            
*  this routine.                                                                
* At entry,                                                                     
*   MYTEXT has length and text of text-replace.                                 
                                                                                
SUBRXMQ  EQU   (((*-RMP27+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP27+SUBRXMQ                                                    
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBRXM--ERR MSGS)'             
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBRXM--WRN MSGS)'             
*-------------------------- WARNING MESSAGES -------------------------*         
                                                                                
* These are messages where the user needs to hit <Enter> only for a             
*  response (for acknowledgment).  Previous values are restored,                
*  except in the case when the key changed in the same transaction.             
                                                                                
XMWRN    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         MVC   AERREX,ERREX                                                     
         LA    R2,ISCFSTAH         FORCE CURSOR TO KEY                          
                                                                                
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBRXM--INF MSGS)'             
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBRXM--LTORG && CONST+        
               ANTS)'                                                           
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SUBRXM--MISC STUFF)'           
*--------------------- SUBRXM MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBRXML  EQU   *-XMSGRTN                                                        
         DS    0CL(X'1000'-SUBRXML+1)                                           
***********************************************************************         
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP27 - S(TATION) COPY REPORT'                                
***********************************************************************         
*========================== RMP27's EQUATES ==========================*         
                                                                                
EOT      EQU   X'00'               END OF TABLE MARKER                          
                                                                                
PHINVSCO EQU   X'27'               PHASE NUMBER FOR INV/SCOPY                   
SCINVSCO EQU   X'CF'               SCREEN NUMBER FOR INV/SCOPY                  
                                                                                
PAGEQ    EQU   2                   TWA PAGE # FOR TEMPSTR                       
NUMINVQ  EQU   (L'ISCFINV)/(3+1)   MAX # OF INVENTORY NUMBER ENTRIES            
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
         DC    AL2(GOSUB-RMP27,0,AGOSUB-SYSD)                                   
         DC    AL2(SUBR01-RMP27,0,ASUBR01-SYSD)                                 
         DC    AL2(XMSGRTN-RMP27,0,AXMSGRTN-SYSD)                               
         DC    AL2(SVCLST-RMP27,0,ASVCTAB-SYSD)                                 
         DC    AL2(TABELCPY-RMP27,0,ATBELCPY-SYSD)                              
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (RERMPWORKD)'                   
***********************************************************************         
*============================= RERMPWORKD ============================*         
       ++INCLUDE RERMPWORKD                                                     
***********************************************************************         
         TITLE 'RERMP27 - S(TATION) COPY REPORT (SYSD)'                         
***********************************************************************         
*========================== SYSD'S SYSSPARE ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*----------------------- OWNED BY RERMP27 ONLY -----------------------*         
                                                                                
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
TOSTTN   DS     CL5                 STATION                                     
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
TMPREP   DS    CL2                 REP CODE                                     
TMPS     DS   0X                                                                
TMPSRC   DS    CL3                 TEMP STORAGE FOR SOURCE                      
*TMPKSRC  DS    CL1                  "      "     "  KEY SOURCE                 
TMPRSR   DS    CL1                  "      "     "  RATING SOURCE               
TMPDSR   DS    CL1                  "      "     "  DATA SOURCE                 
TMPQLF   DS    CL1                  "      "     "  QUALIFIER                   
TMPBTP   DS    CL1                  "      "     "  BOOKTYPE                    
TMPFRBK  DS    0XL3                 "      "     "  FROM BOOK                   
TMPFRBTS DS     XL1                 "      "     "   BITS                       
TMPBOOK  DS     XL2                 "      "     "   BOOK                       
TMPDFIL  DS    CL3                  "      "     "  FILE                        
TMPCODE  DS    CL2                  "      "     "  TRANSFER CODE               
TMPSTTN  DS    CL5                  "      "     "  STATION                     
TMPINVN  DS    CL4                  "      "     "  INVENTORY NUMBER            
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
                                                                                
EL75LEN  EQU   32                  LENGTH OF X'75' ELEMENT                      
BSVDA    DS    A                   ADD TO ADD PASSIVE POINTER                   
STRTCMPR DS    XL2                 START OF 'FROM' PERIOD (CMPRESSED)           
ENDCMPR  DS    XL2                 END OF 'FROM' PERIOD (CMPRESSED)             
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
ADDCOUNT DS    XL1                                                              
FLDDSPL  DS    XL1                 DISPL OF SUB-FIELD INTO FIELD                
SCANLNTH DS    XL1                 L'RHS OF SCANNER BLOCK ENTRY                 
NOPTN    DS    XL1                 NUMBER OF OPTIONS INPUTTED                   
OPTI     DS    AL4                 THE OPTIONS INPUTTED                         
OPTR     DS    AL4                 OPTIONS REQUIRED                             
OPTX     DS    AL4                 OPTIONS NOT ALLOWED                          
                                                                                
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
TOKEY    DS    XL(L'RINVKEY)                                                    
FROMKEY  DS    XL(L'RINVKEY)                                                    
PREVKEY  DS    XL21                RECORD TYPE,REP CODE,STATION,INV.            
STARTKEY DS    XL(L'RINVKEY)                                                    
OVERKEY  DS    XL(L'RINVKEY)                                                    
IBLOCK   DS    CL5                 INPUT BLOCK FOR VGETKSRC                     
OBLOCK   DS    CL5                 OUTPUT BLOCK FOR VGETKSRC                    
                                                                                
PRVFTKEY DS    XL(FTKEYL)          KEY OF PREVIOUS "FROM-TABLE" ENTRY           
                                                                                
RITSTKEY DS    XL(FTKEYL)          CONCATENATED FIELDS FROM RINVKEY             
                                                                                
PREVINV  DS    CL7                 SAVED PREV 'FROM' INFO FOR NEXT TBL          
SVINVDT  DS    CL7                 HEADER INV. AND DATE                         
MYFLAG   DS    XL1                 FLAGS                                        
YESMSTA  EQU   X'01'               MASTER/STATION RECORDS EXIST                 
                                                                                
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (TWA DSECTS)'                   
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
*-------------------------- INV/SCOPY SCREEN -------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPCFD                                                       
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (OTHER DSECTS)'                 
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
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
***********************************************************************         
         TITLE 'RERMP27 - S(TATION) COPY REPORT (REGEN DSECTS)'                 
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
         TITLE 'RERMP27 - S(TATION) COPY REPORT (MISC DSECTS)'                  
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
*------------------------ ENTRY IN FROM TABLE ------------------------*         
                                                                                
FROMTABD DSECT                                                                  
FTKEY    DS    0X                  START OF KEY FOR BINSRCH                     
FTINV    DS     CL(L'RINVKINV)      INVENTORY NUMBER                            
FTSTD    DS     CL(L'RINVKSTD)      EFFECTIVE DATE                              
*FTKSRC   DS     CL(L'RINVKSRC)      KEY SOURCE                                 
FTRSR    DS     CL(L'RINVKRSR)      RATING SOURCE                               
FTDSR    DS     CL(L'RINVKDSR)      DATA SOURCE                                 
FTQLF    DS     CL(L'RINVKQLF)      QUALIFIER                                   
FTBTP    DS     CL(L'RINVKBTP)      BOOKTYPE                                    
FTBK_TXT DS     CL(L'RINVKBK)       BOOK OR TEXT NUMBER                         
FTKEYL2  EQU   *-FTRSR             LENGTH OF FTRSR THROUGH "END"                
FTKEYL   EQU   *-FTKEY             TOTAL LENGTH OF KEY                          
                                                                                
FTFLAG1  DS    XL1                 FLAG #1                                      
FTF1RDEL EQU    X80                 "TO" RECORD DELETED                         
FTF1RXST EQU    X40                 "TO" RECORD EXISTS                          
HEADXST  EQU    X20                 "TO' HEADER RECORD EXISTS                   
FTF1OVER EQU    X10                 OVERLAPPING DATES                           
                                                                                
FROMTABL EQU   *-FROMTABD                                                       
                                                                                
                                                                                
*----------------------------- PRINT LINE ----------------------------*         
                                                                                
PRTLINED DSECT                                                                  
PLKEYA   DS    XL36                                                             
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
***********************************************************************         
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
**PAN#1  DC    CL21'249RERMP27   03/03/11'                                      
         END                                                                    
