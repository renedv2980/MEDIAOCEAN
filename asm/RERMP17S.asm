*          DATA SET RERMP17S   AT LEVEL 017 AS OF 05/01/02                      
*PHASE T81017A                                                                  
*INCLUDE BINSRCH2                                                               
T81017   TITLE 'RERMP17 - INVENTORY TRACKS'                                     
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* Aug10/99 002 GLEE - New field on screen to help determine 1st time  *         
*                                                                     *         
* Jul15/97 012 GLEE - Support LTRANS request for all updates to file  *         
*                                                                     *         
* Apr15/97 007 GLEE - New select code to go to INV/DEMOS screen       *         
*                                                                     *         
* Feb26/97 006 GLEE - Fix bug in TBK# routine                         *         
*                                                                     *         
* Feb13/97 005 GLEE - Ftnote is last 16 bytes of x'01' elem, per Brian*         
*                                                                     *         
* Jan22/97 004 GLEE - Support for book types                          *         
*                                                                     *         
* Jan22/97 003 GLEE - Handle "EST/YR" tracks                          *         
*                                                                     *         
* Dec12/96 002 GLEE - Bug fix in displaying inv hdr day/time & program*         
*                                                                     *         
* Oct16/96 001 GLEE - New program for INVentory TRACKS listing        *         
*                                                                     *         
* Jun20/00 017 FDOY - Changed to support 'DTRACKS' listing/restore    *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
RMP17    CSECT                                                                  
         PRINT NOGEN                                                            
*!!!     NMOD1 0,T81017**,R9,RR=RE                                              
         NMOD1 FTCHIOLN,T81017**,R9,RR=RE,CLEAR=YES                             
*                                                                               
         LR    R0,RC               SAVE FETCH WORK AREA                         
*                                                                               
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
         ST    R0,AFTCHBLK         A(FETCH WORK AREA)                           
*                                                                               
         BAS   RE,MYINIT           INITIALIZATION                               
*                                                                               
         TM    RMPPROFS+RMPTDEMB,RMPTDEMA    DISP. W/ DEMOS?                    
         BO    MAIN2               YES                                          
         OC    ITKOPTN,ITKOPTN                                                  
         BZ    MAIN5                                                            
         CLC   =C'DEMOS',ITKOPTN   DISP W/DEMOS?                                
         BE    *+14                NO                                           
*                                                                               
MAIN2    CLC   =C'NODEMOS',ITKOPTN DISP. W/ DEMOS?                              
         BE    MAIN5               NO                                           
*                                                                               
         CLC   =C'HOMES',IT2DEM1   DEMOS SCREEN ALREADY LOADED?                 
         BE    MAIN20                                                           
*                                                                               
         GOTO1 CALLOV,DMCB,(X'E9',ITKPGMH),0   GET ALT. TRACK SCREEN            
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   IT2DEM2,=CL6'W1849' DEFAULT DEMOS                                
         MVI   IT2DEM2H+5,5                                                     
         OI    IT2DEM2H+6,X'80'                                                 
         MVC   IT2DEM3,=CL6'W2554'                                              
         MVI   IT2DEM3H+5,5                                                     
         OI    IT2DEM3H+6,X'80'                                                 
         MVC   IT2DEM4,=CL6'M1849'                                              
         MVI   IT2DEM4H+5,5                                                     
         OI    IT2DEM4H+6,X'80'                                                 
*                                                                               
         CLI   MTSCRN,X'E8'        CAME FROM ORIG TRACK SCREEN?                 
         BNE   *+14                                                             
         OI    MYFLAG,FROMORIG     YES                                          
         MVC   ITKSTTNH+5(1),MTSTALN                                            
*                                                                               
         MVI   MTSCRN,X'E9'                                                     
*                                                                               
         MVC   ITKOPTN(5),=C'DEMOS'                                             
         OI    ITKOPTNH+6,X'80'                                                 
         B     MAIN20              <<<<<<<<<<<<<<<                              
*                                                                               
MAIN5    DS    0H                                                               
         CLI   FIRSTIME,C'Y'       IF THIS IS THE 1ST TIME,                     
         BNE   *+8                                                              
         MVI   MTSCRN,X'E8'         THIS IS THE SCREEN THAT'S LOADED            
*                                                                               
         CLI   MTSCRN,X'E8' SCREEN ALREADY LOADED?                              
         BE    MAIN20                                                           
*                                                                               
         OC    MTSTA,MTSTA         FIRST TIME THROUGH W/ PROFILE OFF?           
         BZ    MAIN10              YES                                          
*                                                                               
         GOTO1 CALLOV,DMCB,(X'E8',CONTAGH),0  GET ORIG TRACK SCREEN             
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVC   ITKOPTN(7),=C'NODEMOS'                                           
         OI    ITKOPTNH+6,X'80'                                                 
*                                                                               
         OC    ITKSTTN,ITKSTTN     STATION ALREADY HERE?                        
         BNZ   MAIN7                                                            
*                                                                               
         LA    R2,3                                                             
         CLI   CCONKSTA+3,C' '     ANY 4TH CALL LETTER?                         
         BNE   *+6                                                              
         BCTR  R2,0                                                             
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ITKSTTN(0),CCONKSTA   MOVE IN CALL LETTERS                       
         LA    R2,1(R2)                                                         
         LA    RF,ITKSTTN                                                       
         AR    RF,R2               POINT TO END OF CALL LETTERS                 
*                                                                               
         CLI   CCONKSTA+4,C'T'     CHECK FOR SATELLITES                         
         BE    *+14                                                             
         MVI   0(RF),C'-'                                                       
         MVC   1(1,RF),CCONKSTA+4  MOVE IN SATELLITE                            
*                                                                               
         MVC   ITKSTTNH+5(1),MTSTALN                                            
         OI    ITKSTTNH+6,X'80'                                                 
*&&DO                                                                           
         MVC   ITKSTTN(L'MTSTA),MTSTA                                           
         OI    ITKSTTNH+4,X'80'                                                 
*&&                                                                             
*                                                                               
MAIN7    MVC   ITKINV,CCONINV                                                   
         MVC   ITKINVH+5(1),MTINVLN                                             
         OI    ITKINVH+6,X'80'                                                  
         MVC   ITKEFFD(L'CCONEFF),CCONEFF                                       
         MVC   ITKEFFDH+5(1),MTEFFLN                                            
         OI    ITKEFFDH+6,X'80'                                                 
*                                                                               
MAIN10   MVI   MTSCRN,X'E8'                                                     
*                                                                               
MAIN20   CLI   MODE,VALKEY         CHECK GENCON MODES                           
         BE    VKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    VREC                                                             
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*========================= MY INITIALIZATION =========================*         
         DS    0H                                                               
MYINIT   NTR1                                                                   
*                                                                               
         DS    0H                                                               
         MVI   FIRSTIME,C'N'                                                    
         CLC   ITK1TIM,=C'HI'      <===THIS IS A ZERO-INTENSFIED FIELD          
         BE    MI04X                                                            
         MVI   FIRSTIME,C'Y'                                                    
         MVC   ITK1TIM,=C'HI'                                                   
         OI    ITK1TIMH+6,X80                                                   
MI04X    EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  SET UP ADCONS OF TABLES & ROUTINES           
         LH    R2,=Y(DISPTAB-RMP17)                                             
         LA    R2,RMP17(R2)                                                     
         LA    R0,DISPTABQ                                                      
                                                                                
MI10     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         LA    RE,RMP17            RE = BASE OF TABLE/ROUTINE                   
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
         L     RF,=V(BINSRCH)                                                   
         A     RF,RELO                                                          
         ST    RF,VBINSRCH                                                      
*                                                                               
         DS    0H                  SET UP LABELS IN BIGAREA                     
         LH    R2,=Y(LBLTAB-RMP17)                                              
         LA    R2,RMP17(R2)                                                     
         LA    R0,LBLTABQ                                                       
                                                                                
MI20     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         A     R1,ATIA             R1 = A(TO PUT LABEL)                         
         MVC   0(8,R1),2(R2)        AND MOVE LABEL IN                           
         LA    R2,L'LBLTAB(R2)                                                  
         BCT   R0,MI20                                                          
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
         MVI   ACTELOPT,C'N'       DON'T MONITOR ACTIVITY                       
         MVC   AIO,AIO1                                                         
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
*                                                                               
         DS    0H                  MOVE PROFILE TO LOCAL WORKNG STORAGE         
         LR    RF,RA                                                            
         AH    RF,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,RF                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
         DROP  RF                                                               
*                                                                               
         OI    CONSERVH+6,X80+X01  FOR PFKEYS TO WORK                           
*                                                                               
         DS    0H                  TRANSLATE DATA DICT TERMS                    
         XC    DMCB(6*4),DMCB                                                   
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNL                                                   
         MVI   DDRETN,DDCASEL                                                   
         MVI   DDSYS,8                                                          
         MVI   DDLANG,C' '                                                      
         LH    RE,=Y(DCLIST-RMP17)                                              
         A     RE,MYBASE1                                                       
         STCM  RE,7,DDIADR                                                      
         LA    RE,DSLIST                                                        
         STCM  RE,7,DDOADR                                                      
         GOTO1 DICTATE,(R1)                                                     
         DROP  R1                                                               
*                                                                               
         TM    RMPPROFS+RMPTDEMB,RMPTDEMA    DISP. W/ DEMOS?                    
         BO    MI40                YES                                          
         OC    ITKOPTN,ITKOPTN                                                  
         BZ    MI50                                                             
         CLC   =C'DEMOS',ITKOPTN   DISP W/DEMOS?                                
         BE    MI45                                                             
*                                                                               
MI40     CLC   =C'NODEMOS',ITKOPTN DISP. W/ DEMOS?                              
         BE    MI50                NO                                           
MI45     LA    RF,IT2PFLN                                                       
         OI    IT2PFLNH+6,X80                                                   
         B     MI55                                                             
*                                                                               
MI50     DS    0H                  DISPLAY PF KEYS                              
         OI    ITKPFLNH+6,X80                                                   
         LA    RF,ITKPFLN                                                       
         USING PFLINED,RF                                                       
MI55     MVC   PFLSCRLL,RE@10PF                                                 
         MVC   PFLRTRN,SPACES                                                   
         CLI   CALLSP,0           DON'T DISPLAY 12=RTRN IF OVLAY STACK          
         BE    MI70                   IS EMPTY                                  
         CLI   ACTNUM,24          IF DTRACKS REQUEST                            
         BE    MI70                  EXCLUDE 12=RTRN                            
*                                                                               
MI60     MVC   PFLRTRN,RE@PF12R                                                 
         DROP  RF                                                               
*                                                                               
MI70     NI    CHNGFLG1,XFF-CF1ACTEQ                                            
         CLC   ITITSVAE,ACTEQU                                                  
         BE    *+14                                                             
         OI    CHNGFLG1,CF1ACTEQ                                                
         MVC   ITITSVAE,ACTEQU                                                  
*                                                                               
         XC    CCONRSVC,CCONRSVC                                                
         XC    CCONKBK,CCONKBK                                                  
*                                                                               
         MVC   DSP1EL,=Y(RINVPEL-RINVREC)   DISPL TO 1ST ELEMENT                
*                                                                               
MIX      DS    0H                                                               
         B     XIT                                                              
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY TRACKS (VALKEY)'                            
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
VKEY     DS    0H                                                               
         NI    CHNGFLG1,XFF-CF1KEY                                              
*                                                                               
*-------------------------- VALIDATE STATION -------------------------*         
*                                                                               
         XC    CCONKSTA,CCONKSTA                                                
         LA    R2,ITKSTTNH         STATION                                      
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         GOTO1 ANY                  MUST HAVE A STATION INPUT                   
         GOTO1 VALISTA              VALIDATE THE STATION INPUTTED               
         OI    4(R2),X20                                                        
*                                                                               
         MVC   MTSTA,8(R2)         STATION                                      
         MVC   MTSTALN,5(R2)       L'STATION                                    
*                                                                               
         MVC   IKSTTN,WORK         HOLD ONTO KEY FIELD INPUT                    
         CLI   IKSTTN+4,C' '                                                    
         BNE   *+8                                                              
         MVI   IKSTTN+4,C'T'                                                    
         CLI   WORK+40,C' '        CHECK FOR SATELLITES                         
         BE    *+10                                                             
         MVC   IKSTTN+4(1),WORK+40                                              
         MVC   CCONKSTA,WORK       (RIGHT PADDED W/ SPACES)                     
         MVC   CCOSCRST,8(R2)      TO PASS TO SUBSEQUENT SCREENS                
*                                                                               
*--------------------- VALIDATE INVENTORY NUMBER ---------------------*         
*                                                                               
         XC    CCONINV,CCONINV                                                  
         LA    R2,ITKINVH          INVOICE NUMBER                               
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         GOTO1 ANY                  MUST HAVE AN INVOICE # INPUT                
         OI    4(R2),X20                                                        
*                                                                               
         MVC   MTINVLN,5(R2)       L'INV #                                      
*                                                                               
         MVC   IKINV,WORK          HOLD ONTO KEY FIELD INPUT                    
         MVC   CCONINV,WORK                                                     
*                                                                               
*--------------------------- EFFECTIVE DATE --------------------------*         
*                                                                               
         XC    CCONEFF,CCONEFF                                                  
         XC    IKEFFDB,IKEFFDB                                                  
         XC    IKEFFDC,IKEFFDC                                                  
         LA    R2,ITKEFFDH         EFFECTIVE DATE                               
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         CLI   5(R2),0                                                          
         BE    VK035                                                            
*                                                                               
         MVC   MTEFFLN,5(R2)       L'EFF                                        
*                                                                               
         DS    0H                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    INVLFLD                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,IKEFFDB)                                 
         GOTO1 DATCON,DMCB,(0,WORK),(2,IKEFFDC)                                 
         GOTO1 DATCON,DMCB,(0,WORK),(5,CCONEFF)                                 
*                                                                               
VK035    DS    0H                                                               
         OI    4(R2),X20                                                        
         B     VK040                                                            
*                                                                               
*-------------------------- VALIDATE OPTIONS -------------------------*         
*                                                                               
VK040    DS    0H                                                               
         NI    CHNGFLG1,XFF-CF1OPT                                              
         B     VK100                                                            
*                                                                               
*--------------------- MISCELLANEOUS VALKEY TASKS --------------------*         
*                                                                               
VK100    DS    0H                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         TM    CHNGFLG1,CF1AKO     ACTEQU, KEY, OR OPT CHANGE?                  
         BZ    VK120                NO, DON'T RE-INITIALIZE                     
         NI    SELFLAG1,XFF-SF1TSLOK                                            
         MVI   GOSUBN,ISL#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
VK120    DS    0H                                                               
*&&DO                                                                           
         MVI   GOSUBN,TSL#         TEST SELECT FIELDS                           
         GOTO1 AGOSUB                                                           
         ICM   R2,15,FULL           A(FLD W/ ERROR) RETURNED IN FULL            
         BNZ   OURERROR                                                         
                                                                                
         MVI   GOSUBN,GSI#         GO TO THE SELECTED ITEMS                     
         GOTO1 AGOSUB                                                           
*&&                                                                             
*                                                                               
VKX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------- SETUP PROCEDURE --------------------------*         
                                                                                
         DS    0H                                                               
SETUP    NTR1                                                                   
*                                                                               
** GET INVENTORY HEADER **                                                      
*                                                                               
         DS    0H                  GET INVENTORY HEADER RECORD                  
         LA    R2,ITKSTTNH          PLACE CURSOR HERE IN CASE OF ERROR          
         MVI   MYRDUPDT,C'N'                                                    
         MVI   TMPKSRC,0            GOING FOR INVENTORY HEADER RECORD           
         XC    TMPKBK,TMPKBK                                                    
         MVI   GOSUBN,GIR#                                                      
         GOTO1 AGOSUB               KEY & AIO CONTAINS INV KEY & RECD           
         BNE   RCDNTFND                                                         
*                                                                               
         TM    CHNGFLG1,CF1AK      IF ACTEQU OR KEY DIDN'T CHANGE,              
         BZ    SU020                THEN EFF DATE COULDN'T HAVE CHANGED         
                                                                                
         DS    0H                  FORMAT EFF DATE (IF NECESSARY)               
         L     R6,AIO                                                           
         USING REINVRCD,R6                                                      
*                                                                               
         MVC   MTEFF,RINVPEFF      START AND END EFFECTIVE DATES                
*                                                                               
         CLC   RINVKSTD,IKEFFDB     IF THE INPUT IS DIFFERENT FROM RCD          
         BE    SU020                                                            
         MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,WORK)                                
         MVC   ITKEFFD,WORK         PUT EFFECTIVE DATE ON SCREEN                
         OI    ITKEFFDH+6,X80        AND TRANSMIT IT                            
         MVC   CCONEFF,WORK        PUT IT IN GLOBAL AREA AS WELL                
         DROP  R6                                                               
*                                                                               
** BUILD / RESTORE TABLES **                                                    
*                                                                               
SU020    DS    0H                                                               
         TM    CHNGFLG1,CF1AKO      IF ACTEQU, KEY, OR OPTN CHANGED,            
         BZ    SU030                                                            
                                                                                
         MVI   PFKEY,0               IGNORE ANY PFKEYS HIT,                     
                                                                                
         MVI   GOSUBN,CSA#           CLEAR (SYSSPARE) STORAGE AREAS,            
         GOTO1 AGOSUB                                                           
                                                                                
         DS    0H                    HOLD ONTO INVENTORY HEADER RECORD          
         L     RE,AIO                                                           
         USING REINVRCD,RE                                                      
         ZICM  RF,RINVLEN,(3)         GET LENGTH OF INV HEADER RECD             
         DROP  RE                                                               
         L     R0,AINVHDR                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                   AND MOVE IT TO STORAGE AREA              
                                                                                
         MVI   GOSUBN,ILT#           INITIALIZE LIST TABLE                      
         GOTO1 AGOSUB                                                           
         MVI   ITITSNLD,1             START DISPLAY FROM 1ST ENTRY              
         CLI   ITITNLE,0                                                        
         BH    *+8                                                              
         MVI   ITITSNLD,0              UNLESS THERE ARE NO ENTRIES              
                                                                                
         B     SUX                                                              
*                                                                               
SU030    DS    0H                  RESTORE TABLES                               
         MVI   GOSUBN,RTI#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         B     SUX                                                              
*                                                                               
** EXIT SET-UP **                                                               
*                                                                               
SUX      DS    0H                                                               
         B     XIT                                                              
                                                                                
         EJECT                                                                  
*------------------------- TEST CHANGE OF KEY ------------------------*         
*                                                                               
KYCHNGED DS    0H                                                               
         TM    4(R2),X20                                                        
         BZ    KYCH10                                                           
         TM    4(R2),X80                                                        
         BZR   RE                                                               
KYCH10   OI    CHNGFLG1,CF1KEY                                                  
         BR    RE                                                               
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY TRACKS (VALREC)'                            
***********************************************************************         
*========================== VALIDATE RECORD ==========================*         
VREC     DS    0H                                                               
         TM    MYFLAG,FROMORIG     CAME FROM ORIG SCREEN?                       
         BZ    *+12                                                             
         BAS   RE,DREC                                                          
         NI    MYFLAG,X'FF'-FROMORIG                                            
*                                                                               
         XC    ACURFORC,ACURFORC                                                
*                                                                               
VR02     DS    0H                                                               
         TM    CHNGFLG1,CF1KO      DID KEY OR OPTION CHANGE?                    
         BZ    VR02X                NO, DO NORMAL STUFF                         
         MVI   PFKEY,0                                                          
         BAS   RE,DREC              YES, DISPLAY RECD & IGNORE PFKEY            
         B     XIT                                                              
VR02X    EQU   *                                                                
                                                                                
*                                                                               
** TEST SELECT FIELDS **                                                        
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,TSL#         TEST SELECT FIELDS                           
         GOTO1 AGOSUB                                                           
         ICM   R2,15,FULL           A(FLD W/ ERROR) RETURNED IN FULL            
         BNZ   OURERROR                                                         
                                                                                
*                                                                               
** PROCESS SELECTED ENTRIES **                                                  
*                                                                               
         CLI   ITITNLE,0           IF NONE,                                     
         BE    VRX                  THEN EXIT NOW                               
*                                                                               
         CLI   MTSCRN,X'E9'        TRACK/DEMO SCREEN?                           
         BE    VR100               YES                                          
*                                                                               
*** INITIALIZE POINTERS AND COUNTERS ***                                        
*                                                                               
         LA    R2,ITKSELH          R2-->LIST LINES ON SCREEN                    
         USING DLINED,R2                                                        
*                                                                               
         ZIC   R4,ITITSNLD                                                      
         BCTR  R4,0                                                             
         MH    R4,=Y(LISTTABQ)                                                  
         A     R4,ALISTTAB         R4-->LIST TABLE ENTRIES                      
         USING LISTTABD,R4                                                      
*                                                                               
         MVC   NTIMES,ITITNLD                                                   
         MVI   COUNTER,0                                                        
                                                                                
*                                                                               
*** START OF LOOP ***                                                           
*                                                                               
VR040    DS    0H                                                               
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
                                                                                
         ZICM  R0,DLSELH+5,(1)     ANY INPUT IN THIS SELECT FIELD?              
         BZ    VR070                NO, PROCESS NEXT ITEM                       
                                                                                
         OI    DLSELH+1,X20         YES, PROTECT SELECT FIELD TO                
         OI    DLSELH+6,X80          AVOID ANY MODIFICATIONS                    
*                                                                               
VR042    DS    0H                                                               
         L     R3,ASELPFTB                                                      
         USING SELPFTAB,R3                                                      
VR045A   CLI   SELPFSCD,EOT        IF END OF SEL CODE TABLE REACHED,            
         BNE   *+6                                                              
         DC    H'0'                 SOMETHING'S AMISS                           
         CLC   DLSEL(1),SELPFSCD   MATCH ON 1ST CHAR OF SEL FLD ONLY            
         BE    *+12                                                             
         LA    R3,L'SELPFTAB(R3)                                                
         B     VR045A                                                           
                                                                                
         MVC   GOSUBN,SELPFVRT     GO TO ROUTINE OF SELECT CODE                 
         MVI   OURINFCD,0                                                       
         GOTO1 AGOSUB                                                           
         DROP  R3                                                               
                                                                                
         CLI   OURINFCD,0          ANY INFO MESSAGE TO DISPLAY?                 
         BNE   OURINFO              YES, GO DISPLAY IT                          
*                                                                               
         BCT   R0,VR042                                                         
                                                                                
*                                                                               
VR070    DS    0H                                                               
         NI    DLSELH+1,XFF-X20    UNPROTECT SELECT FIELD TO                    
         OI    DLSELH+6,X80         ALLOW MODIFICATIONS                         
                                                                                
         LA    R2,DLINEQ(R2)       BUMP LIST LINE POINTER                       
         LA    R4,LISTTABQ(R4)     BUMP LIST TABLE POINTER                      
         CLC   COUNTER,NTIMES      LOOPED THRU ALL LINES W/ DATA YET?           
         BL    VR040                NO, GO BACK & CHECK THIS LIST LINE          
         B     VRX                                                              
         DROP  R2,R4                                                            
                                                                                
*                                                                               
*** INITIALIZE POINTERS AND COUNTERS ***                                        
*                                                                               
VR100    DS    0H                                                               
         BAS   RE,VALDEMOS         VALIDATE DEMO CATEGORIES                     
*                                                                               
         LA    R2,IT2SELH          R2-->LIST LINES ON SCREEN                    
         USING DEMLINED,R2                                                      
*                                                                               
         ZIC   R4,ITITSNLD                                                      
         BCTR  R4,0                                                             
         MH    R4,=Y(LISTTABQ)                                                  
         A     R4,ALISTTAB         R4-->LIST TABLE ENTRIES                      
         USING LISTTABD,R4                                                      
*                                                                               
         MVC   NTIMES,ITITNLD                                                   
         MVI   COUNTER,0                                                        
*                                                                               
*** START OF LOOP ***                                                           
*                                                                               
VR140    DS    0H                                                               
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
                                                                                
         ZICM  R0,DEMSELH+5,(1)     ANY INPUT IN THIS SELECT FIELD?             
         BZ    VR170                NO, PROCESS NEXT ITEM                       
                                                                                
         OI    DEMSELH+1,X20         YES, PROTECT SELECT FIELD TO               
         OI    DEMSELH+6,X80         AVOID ANY MODIFICATIONS                    
*                                                                               
VR142    DS    0H                                                               
         L     R3,ASELPFTB                                                      
         USING SELPFTAB,R3                                                      
VR145A   CLI   SELPFSCD,EOT        IF END OF SEL CODE TABLE REACHED,            
         BNE   *+6                                                              
         DC    H'0'                 SOMETHING'S AMISS                           
         CLC   DEMSEL(1),SELPFSCD   MATCH ON 1ST CHAR OF SEL FLD ONLY           
         BE    *+12                                                             
         LA    R3,L'SELPFTAB(R3)                                                
         B     VR145A                                                           
                                                                                
         MVC   GOSUBN,SELPFVRT     GO TO ROUTINE OF SELECT CODE                 
         MVI   OURINFCD,0                                                       
         GOTO1 AGOSUB                                                           
         DROP  R3                                                               
                                                                                
         CLI   OURINFCD,0          ANY INFO MESSAGE TO DISPLAY?                 
         BNE   OURINFO              YES, GO DISPLAY IT                          
*                                                                               
         BCT   R0,VR142                                                         
                                                                                
*                                                                               
VR170    DS    0H                                                               
         NI    DEMSELH+1,XFF-X20    UNPROTECT SELECT FIELD TO                   
         OI    DEMSELH+6,X80         ALLOW MODIFICATIONS                        
                                                                                
         LA    R2,DEMLINEQ(R2)       BUMP LIST LINE POINTER                     
         LA    R2,DEMLINEQ(R2)                                                  
         LA    R4,LISTTABQ(R4)     BUMP LIST TABLE POINTER                      
         CLC   COUNTER,NTIMES      LOOPED THRU ALL LINES W/ DATA YET?           
         BL    VR140                NO, GO BACK & CHECK THIS LIST LINE          
         DROP  R2,R4                                                            
*                                                                               
VRX      DS    0H                                                               
         MVI   GOSUBN,STI#         SAVE OFF STUFF IN TIA                        
         GOTO1 AGOSUB                                                           
                                                                                
         LA    R0,ITKSELH                                                       
         CLI   MTSCRN,X'E9'        TRACK/DEMO SCREEN?                           
         BNE   *+8                 NO                                           
         LA    R0,IT2SELH                                                       
         ST    R0,ACURFORC                                                      
                                                                                
         BAS   RE,DREC                                                          
         B     XIT                                                              
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY TRACKS (GET DEMO VALUES)'                   
***********************************************************************         
GETDEMOS NTR1                                                                   
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         LR    R0,R5                                                            
         LHI   R1,DBLOCK1X-DBLOCK1                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   DBFILE,=C'INV'      SET TO INVENTORY FILE                        
         L     R3,AIO              TRACK RECORDS                                
         ST    R3,DBAREC                                                        
         LA    R0,(RINVPEL-RINVREC)(R3) A(1ST ELEMENT)                          
         ST    R0,DBAQUART                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',DEMOLIST),DBLOCKD,DOUT                         
         MVC   MTDEMOLS,DEMOLIST                                                
*                                                                               
GDX      DS    0H                                                               
         B     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY TRACKS (VALIDATE DEMOS)'                    
***********************************************************************         
VALDEMOS NTR1                                                                   
         LA    R5,0                L'FIELD                                      
         LA    R5,8                8 BYTES FOR HEADER                           
         XC    DEMCOUNT,DEMCOUNT                                                
         XC    TEMPHEAD,TEMPHEAD                                                
*                                                                               
         LA    R4,TEMPHEAD                                                      
         MVC   8(20,R4),=C'RHOMES,SHOMES,PHOMES'                                
         LA    R5,20(R5)                                                        
         AR    R4,R5               POSITION AT END OF TEMPHEAD                  
*                                                                               
         ZIC   RF,DEMCOUNT                                                      
         LA    RF,1(RF)                                                         
         STC   RF,DEMCOUNT                                                      
*                                                                               
         LA    R2,IT2DEM2H         2ND DEMO CATEGORY                            
VD10     DS    0H                                                               
         LA    RF,IT2DEM4H                                                      
         CR    R2,RF                                                            
         BH    VD50                                                             
*                                                                               
         CLI   5(R2),0             ANY MORE DEMOS?                              
         BE    VD50                NO - CALL DEMOVAL                            
         ZIC   RF,DEMCOUNT                                                      
         LA    RF,1(RF)                                                         
         STC   RF,DEMCOUNT                                                      
*                                                                               
         MVI   0(R4),C','          INSERT COMMA                                 
         LA    R4,1(R4)            INCREMENT POSITION IN TEMPHEAD               
         LA    R5,1(R5)            LENGTH                                       
*                                                                               
         MVI   0(R4),C'R'          RATINGS                                      
         LA    R5,1(R5)                                                         
*                                                                               
         ZIC   R3,5(R2)            MOVE IN DEMO NAME                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),8(R2)                                                    
*                                                                               
         LA    R4,1(R4)            INCREMENT FOR 'R'                            
         LA    R3,1(R3)            RESTORE ORIGINAL LENGTH                      
         AR    R4,R3                                                            
         AR    R5,R3               INCREMENT LENGTH                             
*                                                                               
         MVI   0(R4),C','          INSERT COMMA                                 
         LA    R4,1(R4)            INCREMENT POSITION IN TEMPHEAD               
         LA    R5,1(R5)            LENGTH                                       
*                                                                               
         MVI   0(R4),C'S'          SHARE                                        
         LA    R5,1(R5)                                                         
*                                                                               
         ZIC   R3,5(R2)            MOVE IN DEMO NAME                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),8(R2)                                                    
*                                                                               
         LA    R4,1(R4)            INCREMENT FOR 'S'                            
         LA    R3,1(R3)            RESTORE ORIGINAL LENGTH                      
         AR    R4,R3                                                            
         AR    R5,R3               INCREMENT LENGTH                             
*                                                                               
         MVI   0(R4),C','          INSERT COMMA                                 
         LA    R4,1(R4)            INCREMENT POSITION IN TEMPHEAD               
         LA    R5,1(R5)            LENGTH                                       
*                                                                               
         MVI   0(R4),C'P'          PUT                                          
         LA    R5,1(R5)                                                         
*                                                                               
         ZIC   R3,5(R2)            MOVE IN DEMO NAME                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),8(R2)                                                    
*                                                                               
         LA    R4,1(R4)            INCREMENT FOR 'P'                            
         LA    R3,1(R3)            RESTORE ORIGINAL LENGTH                      
         AR    R4,R3                                                            
         AR    R5,R3               INCREMENT LENGTH                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     VD10                                                             
*                                                                               
VD50     DS    0H                                                               
         STC   R5,TEMPHEAD                                                      
         SH    R5,=H'8'                                                         
         STC   R5,TEMPHEAD+5                                                    
*                                                                               
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         LR    R0,R5                                                            
         LA    R1,DBLOCK1X-DBLOCK1                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELAGY,TWAAGY                                                  
         MVI   BYTE,C'S'                                                        
         LA    R2,TEMPHEAD                                                      
         ZIC   R3,DEMCOUNT                                                      
         MH    R3,=H'4'                                                         
*                                                                               
         GOTO1 DEMOVAL,DMCB,(1,(R2)),((R3),DEMOLIST),(BYTE,DBLOCKD),0           
         CLI   0(R1),0                                                          
         BE    VDX                                                              
         LA    R2,IT2DEM2H                                                      
         MVC   RERROR(2),=AL2(INVDEMO)                                          
         B     ERREND2                                                          
*                                                                               
VDX      DS    0H                                                               
         B     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY TRACKS (VALIDATION ERRORS)'                 
***********************************************************************         
*========================= VALIDATION ERRORS =========================*         
                                                                                
INVLFLD  DS    0H                  INVALID FIELD ERROR                          
         MVI   OURERRCD,IFLDQ                                                   
         B     OURERROR                                                         
                                                                                
RCDNTFND DS    0H                  RECORD NOT FOUND                             
         MVI   OURERRCD,RNFQ                                                    
         B     OURERROR                                                         
                                                                                
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY TRACKS (DISPREC)'                           
***********************************************************************         
*=========================== DISPLAY RECORD ==========================*         
DREC     NTR1                                                                   
         CLI   RETURNED,12                                                      
         BNE   DR005X                                                           
         MVI   GOSUBN,ILT#          INITIALIZE LIST TABLE                       
         GOTO1 AGOSUB                                                           
         MVI   ITITSNLD,1            START DISPLAY FROM 1ST ENTRY               
         CLI   ITITNLE,0                                                        
         BH    *+8                                                              
         MVI   ITITSNLD,0             UNLESS THERE ARE NO ENTRIES               
DR005X   EQU   *                                                                
*                                                                               
** HANDLE PF KEYS **                                                            
*                                                                               
         CLI   PFKEY,0             IF A PF KEY WAS PRESSED,                     
         BE    DR010                                                            
         BAS   RE,DOPFKEY           PROCESS IT                                  
         BE    DR010                                                            
         LA    R2,ITKSTTNH                                                      
         CLI   OURERRCD,0                                                       
         BH    OURERROR             DISPLAY ERROR MESSAGE                       
         CLI   OURWRNCD,0                                                       
         BH    OURWARN               OR WARNING MESSAGE IF NOT GOOD             
         DC    H'0'                                                             
*                                                                               
** DISPLAY HEADER INFO **                                                       
*                                                                               
DR010    DS    0H                                                               
         TM    CHNGFLG1,CF1AK      IF ACTEQU OR KEY DIDN'T CHANGE,              
         BZ    DR015                THEN DON'T RE-DISPLAY HDR STUFF             
*                                                                               
         XC    ITKIHDT,ITKIHDT                                                  
         XC    ITKIHPG,ITKIHPG                                                  
*                                                                               
         NI    ITKIHDTH+6,XFF-X80  DAY/TIME HASN'T BEEN FORMATTED YET           
         NI    ITKIHPGH+6,XFF-X80  PGM NAME HASN'T BEEN FORMATTED YET           
         L     R3,AINVHDR                                                       
         AH    R3,DSP1EL           R3-->1ST ELEMENT OF INV HEADER RECD          
         SR    R0,R0                                                            
                                                                                
DR011    DS    0H                                                               
         CLI   0(R3),0             IF AT END OF RECORD,                         
         BE    DR015                DONE DISPLAYING HEADER INFO                 
                                                                                
         CLI   0(R3),X'02'         '02' ELEM HAS DAY/TIME                       
         BNE   DR012                                                            
         TM    ITKIHDTH+6,X80      DAY/TIME FORMATTED YET?                      
         BNZ   DR014                YEP, DON'T OVERWRITE 1ST DAY/TIME           
         LA    R2,ITKIHDT                                                       
         OI    ITKIHDTH+6,X80                                                   
                                                                                
         MVC   TMPIDAY,(RIDTDAY-RIDTELEM)(R3)                                   
         MVI   GOSUBN,TID#                                                      
         GOTO1 AGOSUB                                                           
         ZIC   R1,BYTE               R1=L(CONVERTED DAY)                        
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK                                                    
         AR    R2,R1                                                            
         MVI   1(R2),C'/'                                                       
         LA    R2,2(R2)                                                         
                                                                                
         MVC   DUB(4),(RIDTTIME-RIDTELEM)(R3)                                   
         MVI   GOSUBN,TMT#                                                      
         GOTO1 AGOSUB                                                           
         ZIC   R1,BYTE               R1=L(CONVERTED TIMES)                      
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK                                                    
                                                                                
         B     DR014                                                            
                                                                                
DR012    DS    0H                                                               
         CLI   0(R3),X'03'         '03' ELEMENT HAS PROGRAM NAME                
         BNE   DR014                                                            
         TM    ITKIHPGH+6,X80      PGM NAME FORMATTED YET?                      
         BNZ   DR014                YEP, DON'T OVERWRITE 1ST PGM NAME           
         OI    ITKIHPGH+6,X80                                                   
                                                                                
         LA    RE,L'ITKIHPG                                                     
         ZIC   RF,1(R3)                                                         
         SH    RF,=Y(RIPGNAME-RIPGELEM)                                         
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EXMVC RE,ITKIHPG,(RIPGNAME-RIPGELEM)(R3)                               
                                                                                
         B     DR014                                                            
                                                                                
DR014    DS    0H                                                               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR011                                                            
                                                                                
DR015    EQU   *                                                                
                                                                                
*                                                                               
** PROTECT FIELDS **                                                            
*                                                                               
         DS    0H                                                               
         TM    CHNGFLG1,CF1AK                                                   
         BZ    DR016X                                                           
*                                                                               
         CLI   MTSCRN,X'E9'        TRACK/DEMOS SCREEN?                          
         BE    DR016C                                                           
*                                                                               
         LA    R0,MXLSTLNS                                                      
         LA    R2,ITKSELH                                                       
         USING DLINED,R2                                                        
DR016B   OI    DLFTNTH+1,X20       PROTECT FOOTNOTE FIELD                       
         OI    DLFTNTH+6,X80        AND TRANSMIT IT                             
         LA    R2,DLINEQ(R2)       BUMP TO NEXT LINE                            
         BCT   R0,DR016B                                                        
         B     DR016X                                                           
         DROP  R2                                                               
*                                                                               
DR016C   LA    R0,MXDEMLNS                                                      
         LA    R2,IT2SELH                                                       
         USING DEMLINED,R2                                                      
DR016D   OI    DEMFTNTH+1,X20      PROTECT FOOTNOTE FIELD                       
         OI    DEMFTNTH+6,X80      AND TRANSMIT IT                              
         LA    R2,DEMLINEQ(R2)     BUMP TO NEXT LINE                            
         BCT   R0,DR016D                                                        
         DROP  R2                                                               
*                                                                               
DR016X   EQU   *                                                                
                                                                                
*                                                                               
** DISPLAY LISTTAB ENTRIES **                                                   
*                                                                               
DR020    DS    0H                                                               
         MVI   GOSUBN,CLA#         CLEAR LIST AREA ON SCREEN                    
         GOTO1 AGOSUB                                                           
                                                                                
         MVI   ITITNLD,0           NO LINES DISPLAYED THUS FAR                  
*                                                                               
         CLI   ITITNLE,0           IF NOTHING TO LIST,                          
         BH    *+12                                                             
         MVI   OURINFCD,NDPLYQ      DISPLAY INFO MESSAGE NOW                    
         B     OURINFO                                                          
*                                                                               
         ZICM  R4,ITITSNLD,(1)     START W/ 1ST ENTRY TO BE DISPLAYED           
         BZ    DRX                                                              
         BCTR  R4,0                                                             
         MH    R4,=Y(LISTTABQ)                                                  
         A     R4,ALISTTAB                                                      
         USING LISTTABD,R4                                                      
                                                                                
         CLC   LTNTRY,ITITSNLD     CHECK IF WE'RE STILL OKAY                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   MTSCRN,X'E9'        TRACK/DEMOS SCREEN?                          
         BE    DR040                                                            
*                                                                               
         LA    R2,ITKSELH          POINT TO FIRST DISPLAY LINE                  
*                                                                               
DR030    DS    0H                                                               
         CLI   LTNTRY,0            SEE IF ANY MORE ENTRIES                      
         BE    DR100                                                            
         CLI   ITITNLD,MXLSTLNS    MAKE SURE WE'RE W/IN BOUNDS                  
         BNL   DR100                                                            
*                                                                               
         DS    0H                  MOVE DATA TO LIST LINE                       
         LR    R3,R2                                                            
         USING DLINED,R3                                                        
                                                                                
         TM    LTFLAG,LTTRKRES     WAS THIS TRACK RESTORED?                     
         BZ    DR035                  NO                                        
         XC    DLSEL,DLSEL            YES, CLEAR SELECT FIELD                   
         MVI   DLSEL,C'*'                  MARK IT WITH AN '*'                  
         OI    DLSELH+6,X'80'               AND XMIT                            
*                                                                               
DR035    MVC   TMPKSRC,LTKSRC                                                   
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DLLSRC,TMPSRC        RATING SERVICE                              
                                                                                
         MVC   DLLCODE,LTCODE       CODE                                        
                                                                                
         MVC   TMPKSRC,LTKSRC                                                   
         MVC   TMPBOOK,LTXKBK                                                   
         XC    TMPBOOK,=X'FFFF'                                                 
         MVI   GOSUBN,TBK#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DLLBOOK,WORK         BOOK                                        
                                                                                
         MVC   DLLFDTL,LTFRDETL     FROM DETAILS                                
                                                                                
*&&DO                                                                           
         MVC   TMPIDAY,LTIDAY                                                   
         MVI   GOSUBN,TID#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DLDAY,WORK           DAYS                                        
                                                                                
         MVC   DUB(4),LTTIME                                                    
         MVI   GOSUBN,TMT#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DLSETIM,WORK         TIMES                                       
                                                                                
         MVC   DLPTITL,LTPGNAM      PROGRAM NAME                                
*&&                                                                             
         OI    DLLDTAH+6,X80                                                    
                                                                                
         MVC   DLFTNT,LTFTNOTE     FOOTNOTE                                     
         OI    DLFTNTH+6,X80                                                    
                                                                                
         ZIC   R1,ITITNLD                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ITITNLD          UPDATE # OF LINES DISPLAYED                  
                                                                                
         LA    R2,SZLSTLIN(R2)                                                  
         LA    R4,LISTTABQ(R4)                                                  
         B     DR030                                                            
*                                                                               
DR040    LA    R2,IT2SELH          POINT TO FIRST DISPLAY LINE                  
         BAS   RE,VALDEMOS         VALIDATE DEMOS                               
*                                                                               
DR050    DS    0H                                                               
         CLI   LTNTRY,0            SEE IF ANY MORE ENTRIES                      
         BE    DR100                                                            
         CLI   ITITNLD,7           MAKE SURE WE'RE W/IN BOUNDS                  
*  !!!   CLI   ITITNLD,MXDEMLNS    MAKE SURE WE'RE W/IN BOUNDS                  
         BNL   DR100                                                            
*                                                                               
         DS    0H                  MOVE DATA TO LIST LINE                       
         LR    R3,R2                                                            
         USING DEMLINED,R3                                                      
*                                                                               
         TM    LTFLAG,LTTRKRES     WAS THIS TRACK RESTORED?                     
         BZ    DR055                  NO                                        
         XC    DEMSEL,DEMSEL          YES, CLEAR SELECT FIELD                   
         MVI   DEMSEL,C'*'                 MARK IT WITH AN '*'                  
         OI    DEMSELH+6,X'80'              AND XMIT                            
*                                                                               
DR055    MVC   TMPKSRC,LTKSRC                                                   
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DEMSRC,TMPSRC        RATING SERVICE                              
         MVC   DEMCODE,LTCODE       CODE                                        
*                                                                               
         NI    MYFLAG2,X'FF'-PJORES                                             
         CLI   TMPQLFY,C'E'        ESTIMATED BOOK?                              
         BE    *+12                                                             
         CLI   TMPQLFY,C'P'        PROJECTED BOOK?                              
         BNE   *+8                                                              
         OI    MYFLAG2,PJORES                                                   
*                                                                               
         MVC   TMPKSRC,LTKSRC                                                   
         MVC   TMPBOOK,LTXKBK                                                   
         XC    TMPBOOK,=X'FFFF'                                                 
         MVI   GOSUBN,TBK#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DEMBOOK,WORK         BOOK                                        
*                                                                               
         TM    IT2SELH+1,X'20'     TP TRACK?                                    
         BO    DR070                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),KEYSAVE                                                  
         MVC   KEY+24(1),LTKSRC                                                 
         MVC   KEY+25(2),TMPBOOK                                                
         CLI   TMPBOOK+1,ESTMNTHQ    YEARLY ESTIMATE?                           
         BNE   *+8                                                              
         MVI   KEY+26,0                                                         
*                                                                               
         MVC   SVDMINBT,DMINBTS    SAVE DATAMANAGER INPUT BITS                  
         CLI   ACTNUM,24           IF DTRACKS REQUEST,                          
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       RETURN DELETED TRACKS                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         MVC   DMINBTS,SVDMINBT    RESTORE DATAMANAGER INPUT BITS               
*                                                                               
         BAS   RE,GETDEMOS         GET DEMO VALUES                              
*                                                                               
         LA    R6,DEMR1                                                         
         LA    R5,DOUT             DEMO OUTPUT                                  
*                                                                               
DR060    DS    0H                                                               
         OC    0(4,R5),0(R5)        ANY DEMOS?                                  
         BNZ   DR062                                                            
         OC    4(4,R5),4(R5)       ANY MORE DEMOS                               
         BZ    DR061                                                            
         LA    R5,4(R5)            BUMP UP TO NEXT ENTRIES                      
         LA    R6,4(R6)                                                         
         B     DR062                                                            
*                                                                               
DR061    DS    0H                                                               
         OC    8(4,R5),8(R5)                                                    
         BZ    DR070                                                            
         LA    R5,8(R5)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
DR062    L     RF,0(R5)                                                         
         LA    RF,5(RF)                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
*                                                                               
         EDIT  (RF),DUB                                                         
         MVC   0(2,R6),DUB+6                                                    
         LA    R5,4(R5)                                                         
*                                                                               
         LA    RF,DEMH1            END OF 1ST CATEGORY?                         
         CR    R6,RF                                                            
         BE    DR065                                                            
         LA    RF,DEMP2            END OF 2ND CATEGORY?                         
         CR    R6,RF                                                            
         BE    DR065                                                            
         LA    RF,DEMP3            END OF 3RD CATEGORY?                         
         CR    R6,RF                                                            
         BE    DR065                                                            
         LA    RF,DEMP4            END OF 4TH CATEGORY?                         
         CR    R6,RF                                                            
         BE    DR065                                                            
         LA    R6,3(R6)                                                         
         B     *+8                                                              
*                                                                               
DR065    LA    R6,4(R6)            END OF CATEGORY, MUST SKIP 1 MORE            
         B     DR060                                                            
*                                                                               
*&&DO                                                                           
         MVC   TMPIDAY,LTIDAY                                                   
         MVI   GOSUBN,TID#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DLDAY,WORK           DAYS                                        
                                                                                
         MVC   DUB(4),LTTIME                                                    
         MVI   GOSUBN,TMT#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DLSETIM,WORK         TIMES                                       
                                                                                
         MVC   DLPTITL,LTPGNAM      PROGRAM NAME                                
*&&                                                                             
DR070    OI    DEMDATAH+6,X80                                                   
*                                                                               
         MVC   DEMFTNT,LTFTNOTE     FOOTNOTE                                    
         OI    DEMFTNTH+6,X80                                                   
*                                                                               
         ZIC   R1,ITITNLD                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ITITNLD          UPDATE # OF LINES DISPLAYED                  
*                                                                               
         LA    R2,DEMLINEQ(R2)     GO TO NEXT LINE TO DISPLAY TP                
         LR    R3,R2                                                            
*                                                                               
         TM    MYFLAG2,PJORES      PROJECTED OR ESTIMATED BOOK?                 
         BO    DR90                                                             
*                                                                               
         MVC   TMPKSRC,LTKSRC                                                   
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DEMSRC,TMPSRC        RATING SERVICE                              
         MVC   DEMCODE,=C'TP'       TP CODE                                     
*                                                                               
         MVC   TMPKSRC,LTKSRC                                                   
         MVC   TMPBOOK,LTXKBK                                                   
         XC    TMPBOOK,=X'FFFF'                                                 
         MVI   GOSUBN,TBK#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DEMBOOK,WORK         BOOK                                        
         OI    DEMDATAH+6,X80                                                   
*                                                                               
         MVI   OVFLGO,TPD#         GET AND DISP TP DATA                         
         GOTO1 AOVFL,(RC)                                                       
*                                                                               
         MVC   DEMFTNT,ORIGFTNT    ORIGINAL FOOTNOTE                            
         OI    DEMFTNTH+6,X80                                                   
*                                                                               
DR90     LA    R2,DEMLINEQ(R2)                                                  
         LA    R4,LISTTABQ(R4)                                                  
         B     DR050                                                            
         DROP  R4                                                               
*                                                                               
DR100    DS    0H                  DONE LISTING THIS TIME AROUND                
         ZIC   R1,ITITSNLD                                                      
         ZIC   R0,ITITNLD                                                       
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         STC   R1,ITITENLD        REMEMBER LAST ENTRY DISPLAYED                 
*                                                                               
DRX      DS    0H                                                               
         BAS   RE,CHKFLDS         ENSURE SPECIAL SELECT FLDS PROTECTED          
         MVI   GOSUBN,STI#        SAVE OFF STUFF IN TIA                         
         GOTO1 AGOSUB                                                           
                                                                                
         LA    R0,ITKSELH                                                       
         CLI   MTSCRN,X'E9'       TRACK/DEMO SCREEN?                            
         BNE   *+8                NO                                            
         LA    R0,IT2SELH                                                       
         ST    R0,ACURFORC                                                      
                                                                                
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*============================ DO PFKEY TASKS =========================*         
DOPFKEY  NTR1                                                                   
                                                                                
         MVI   OURERRCD,0                                                       
         MVI   OURWRNCD,0                                                       
*                                                                               
         CLI   PFKEY,5                                                          
         BE    PF05                                                             
         CLI   PFKEY,6                                                          
         BE    PF06                                                             
         CLI   PFKEY,7                                                          
         BE    PF07                                                             
         CLI   PFKEY,8                                                          
         BE    PF08                                                             
         MVI   PFKEY,0                                                          
         B     DOPFKEYX                                                         
                                                                                
*                                                                               
** TOP **                                                                       
*                                                                               
PF05     DS    0H                                                               
         CLI   ITITSNLD,0          IF NOTHING LISTED,                           
         BE    PFINVLD              HITTING THIS PFKEY IS AN ERROR              
         CLI   ITITSNLD,1          IF AT TOP ALREADY,                           
         BNE   *+12                                                             
         MVI   OURWRNCD,WTOPQ       GIVE WARNING MESSAGE                        
         B     NO                                                               
                                                                                
         MVI   ITITSNLD,1                                                       
         B     DOPFKEYX                                                         
                                                                                
*                                                                               
** BOTTOM **                                                                    
*                                                                               
PF06     DS    0H                                                               
         CLI   ITITSNLD,0          IF NOTHING LISTED,                           
         BE    PFINVLD              HITTING THIS PFKEY IS AN ERROR              
         CLC   ITITENLD,ITITNLE    IF AT BOTTOM ALREADY,                        
         BNE   *+12                                                             
         MVI   OURWRNCD,WBOTQ       IF SO, DISPLAY WARNING MSG                  
         B     NO                                                               
                                                                                
         ZIC   RF,ITITNLE          ELSE, SET 1ST ENTRY                          
                                                                                
         CLI   MTSCRN,X'E9'        DEMOS SCREEN?                                
         BNE   PF06A                                                            
         LA    RE,MXDEMLNS/2                                                    
         CLI   ITITNLE,MXDEMLNS/2                                               
         BH    *+6                                                              
         LR    RE,RF                                                            
         B     PF06B                                                            
                                                                                
PF06A    LA    RE,MXLSTLNS                                                      
         CLI   ITITNLE,MXLSTLNS                                                 
         BH    *+6                                                              
         LR    RE,RF                                                            
                                                                                
PF06B    SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,ITITSNLD          TO BE DISPLAYED ON LIST                     
         B     DOPFKEYX                                                         
                                                                                
*                                                                               
** SCROLL UP **                                                                 
*                                                                               
PF07     DS    0H                                                               
         CLI   ITITSNLD,0          IF NOTHING LISTED,                           
         BE    PFINVLD              HITTING THIS PFKEY IS AN ERROR              
         CLI   ITITSNLD,1          AT TOP ALREADY?                              
         BH    *+12                                                             
         MVI   OURWRNCD,WTOPQ       YES, DISPLAY "WARNING" MSG                  
         B     NO                                                               
                                                                                
         ZIC   RF,ITITSNLD                                                      
         BCTR  RF,0                                                             
         STC   RF,ITITSNLD                                                      
         B     DOPFKEYX                                                         
                                                                                
*                                                                               
** SCROLL DOWN **                                                               
*                                                                               
PF08     DS    0H                                                               
         CLI   ITITSNLD,0          IF NOTHING LISTED,                           
         BE    PFINVLD              HITTING THIS PFKEY IS AN ERROR              
         CLC   ITITSNLD,ITITNLE    AT BOTTOM ALREADY?                           
         BL    *+12                                                             
         MVI   OURWRNCD,WBOTQ       YES, DISPLAY "WARNING" MSG                  
         B     NO                                                               
                                                                                
         ZIC   RF,ITITSNLD                                                      
         LA    RF,1(RF)                                                         
         STC   RF,ITITSNLD                                                      
         B     DOPFKEYX                                                         
                                                                                
*                                                                               
PFINVLD  DS    0H                  INVALID PFKEY ERROR                          
         MVI   OURERRCD,IPFKQ                                                   
         B     NO                                                               
                                                                                
*                                                                               
DOPFKEYX DS    0H                                                               
         B     YES                                                              
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY TRACKS (MISCELLANEOUS)'                     
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
                                                                                
*----------------- EXIT AND DISPLAY MESSAGE ROUTINES -----------------*         
                                                                                
OURERROR DS    0H                                                               
         MVI   BYTE,C'E'                                                        
         BAS   RE,CHKFLDS           ENSURE RESTORED MARKER PROTECTED            
         B     XMSGGO                                                           
                                                                                
OURWARN  DS    0H                                                               
         MVI   BYTE,C'W'                                                        
         BAS   RE,CHKFLDS           ENSURE RESTORED MARKER PROTECTED            
         B     XMSGGO                                                           
                                                                                
OURINFO  DS    0H                                                               
         MVI   BYTE,C'I'                                                        
         B     XMSGGO                                                           
                                                                                
XMSGGO   DS    0H                                                               
         GOTO1 AXMSGRTN,DMCB,(BYTE,(RC))                                        
         B     XIT                                                              
                                                                                
ERREND2  DS    0H                                                               
         GOTO1 MYERROR                                                          
                                                                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*   ENSURE ANY SELECT FIELDS MARKED AS RESTORED ("*") ARE PROTECTED   *         
*                                                                     *         
***********************************************************************         
CHKFLDS  NTR1                                                                   
         CLI   ACTNUM,24                                                        
         BNE   CHKX              ROUTINE ONLY APPLIES TO DTRACKS REQ            
*                                                                               
         CLI   MTSCRN,X'E9'      TRACK/DEMO SCREEN?                             
         BE    CHKF100                                                          
*                                                                               
*           FOR NO-DEMOS SCREEN                                                 
*                                                                               
         LA    R2,ITKSELH                                                       
         LA    R3,ITKSELZH                                                      
*                                                                               
CHKF10   CR    R2,R3             IF PAST LAST SELECT FIELD,                     
         BH    CHKX                 THEN EXIT                                   
         USING DLINED,R2                                                        
         CLI   DLSEL,C'*'        CONTAINS THE SPECIAL MARKER?                   
         BNE   CHKF50                                                           
         OI    DLSELH+1,X'20'       YES, KEEP PROTECTED                         
*                                                                               
CHKF50   LA    R2,DLINEQ(R2)     BUMP SCREEN FIELD                              
         B     CHKF10                                                           
         DROP  R2                                                               
*                                                                               
*           FOR DEMOS SCREEN                                                    
*                                                                               
CHKF100  LA    R2,IT2SELH                                                       
         LA    R3,IT2SELZH                                                      
*                                                                               
CHKF110  CR    R2,R3             IF PAST LAST SELECT FIELD,                     
         BH    CHKX                 THEN EXIT                                   
         USING DEMLINED,R2                                                      
         CLI   DEMSEL,C'*'        CONTAINS THE SPECIAL MARKER?                  
         BNE   CHKF150                                                          
         OI    DEMSELH+1,X'20'       YES, KEEP PROTECTED                        
*                                                                               
CHKF150  LA    R2,DEMLINEQ(R2)   BUMP SCREEN FIELD                              
         LA    R2,DEMLINEQ(R2)                                                  
         B     CHKF110                                                          
         DROP  R2                                                               
*                                                                               
CHKX     XIT1                                                                   
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY TRACKS (LTORG && CONSTANTS)'                
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
                                                                                
                                                                                
PF12DC   DC    C'12=Rtrn'                                                       
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
FROMORIG EQU   X'01'               CAME FROM ORIG TRACK SCREEN                  
INVDEMO  EQU   779                 ONE OR MORE INVALID DEMOS                    
                                                                                
DEMCOUNT DS    XL1                 # OF DEMOS                                   
TEMPHEAD DS    XL91                MAX # OF DEMOS TO LOOK UP IN DEMOVAL         
DEMOLIST DS    XL50                OUTPUT FROM DEMOVAL                          
DOUT     DS    XL50                OUTPUT FROM DEMOUT                           
*                                                                               
***********************************************************************         
         DROP  R7,R8,R9,RA,RB,RC                                                
         TITLE 'RERMP17 - INVENTORY TRACKS (OVFL)'                              
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   OVFLGO = EQUATED SUB-ROUTINE NUMBER.                                        
                                                                                
OVFLQ    EQU   (((*-RMP17+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP17+OVFLQ                                                      
OVFL     NMOD1 0,**OVFL**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R8             R8=A(SYSD)                                   
         USING SPOOLD,R7           R7=A(SPOOL WORK AREA)                        
                                                                                
         ZIC   R1,OVFLGO                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     OVFL00(R1)                                                       
                                                                                
FTN#     EQU   (OVFL01-*)/4+1      BUILD KEY TO INVENTORY RECORD                
TPD#     EQU   (OVFL02-*)/4+1      GET AND DISPLAY TP DATE                      
GID2#    EQU   (OVFL03-*)/4+1      GO TO INV DEM FROM TRACK/DEMOS               
*                                                                               
OVFL00   DS    0H                                                               
OVFL01   B     FTNTDEM             CHANGE FOOTNOTE ON TRACK/DEMOS               
OVFL02   B     DISPTPD             DISPLAY TP DATA                              
OVFL03   B     GINVDEM2            GO TO INV DEM FROM TRACK/DEMOS               
*                                                                               
OVXIT    XIT1                                                                   
***********************************************************************         
*   GO TO INV DEM FROM TRACK/DEMOS                                              
***********************************************************************         
GINVDEM2 DS    0H                                                               
         USING DEMLINED,R2                                                      
         USING SELPFTAB,R3                                                      
         USING LISTTABD,R4                                                      
*                                                                               
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         MVC   TIOBAID,SELPFPFK    FUDGE PF KEY INTO TIO                        
         MVC   PFKEY,SELPFPFK      FUDGE IT INTO PFKEY AS WELL                  
         DROP  RF                                                               
*                                                                               
** SET PARAMETERS FOR CPROG **                                                  
*                                                                               
         DS    0H                                                               
         XC    CCONTRKS,CCONTRKS                                                
         LA    R5,CCONTRKS                                                      
         MVI   PRVSRC,0            INITIALIZE PREVIOUS RATING SERVICE           
         MVI   CNTIDMTK,0          COUNTS # OF TRCKS GOING TO INV/DEMOS         
*                                                                               
GIDM232  DS    0H                  FORMAT RATING SERVICE                        
         MVC   TMPKSRC,LTKSRC                                                   
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         CLC   PRVSRC,TMPSRC        IF PREV RSVC = CURR RSVC                    
         BE    GIDM232X              DON'T FORMAT IT INTO FIELD                 
         MVC   PRVSRC,TMPSRC                                                    
         MVC   0(1,R5),TMPSRC                                                   
         LA    R5,1(R5)                                                         
         BAS   RE,GIDMCMM2                                                      
GIDM232X EQU   *                                                                
*                                                                               
         DS    0H                  FORMAT TRACK                                 
         MVC   TMPKSRC,LTKSRC                                                   
         MVC   TMPBOOK,LTXKBK                                                   
         XC    TMPBOOK,=X'FFFF'                                                 
         MVI   GOSUBN,TBK#                                                      
         GOTO1 AGOSUB                                                           
         ZICM  R1,HALF,(3)                                                      
         BZ    GIDM234X                                                         
         BCTR  R1,0                                                             
         EXMVC R1,0(R5),WORK                                                    
         LA    R5,1(R1,R5)                                                      
GIDM234X EQU   *                                                                
*                                                                               
         DS    0H                  FUDGE SEL FLD TO PREVENT LOOPING             
         ZIC   R1,DEMSELH+5                                                     
         BCTR  R1,0                                                             
         STC   R1,DEMSELH+5          DECREMENT L(INPUT) BY ONE                  
         EXMVC R1,WORK,DEMSEL                                                   
         XC    DEMSEL,DEMSEL                                                    
         LTR   R1,R1                                                            
         BZ    GIDM238X                                                         
         BCTR  R1,0                                                             
         EXMVC R1,DEMSEL,WORK+1      SHIFT DATA TO LEFT BY ONE                  
GIDM238X EQU   *                                                                
*                                                                               
         ZIC   R1,CNTIDMTK         UPDATE THE NUMBER OF TRACKS                  
         LA    R1,1(R1)                                                         
         STC   R1,CNTIDMTK          GOING TO INV/DEMOS                          
         CLI   CNTIDMTK,MXTKLNTY   REACHED MAX YET?                             
         BNL   GIDM300              YES, GO TO INV/DEMOS NOW                    
*                                                                               
         LA    RF,IT2SELZH         RF-->LAST LIST LINE                          
GIDM242  DS    0H                                                               
         LA    R2,DEMLINEQ(R2)       BUMP TO NEXT LIST LINE                     
         LA    R2,DEMLINEQ(R2)                                                  
         CR    R2,RF               DID WE PAST LAST LIST LINE?                  
         BH    GIDM249              YES, NO MORE TRACKS TO GET                  
         LA    R4,LISTTABQ(R4)                                                  
         CLI   DEMSEL+0,C'M'        GO TO INV/DEMOS FOR THIS TRACK?             
         BNE   GIDM242              NOPE, BUMP TO NEXT LIST LINE                
         BAS   RE,GIDMCMM2                                                      
         ZIC   R1,ITSHNSEL         DECREMENT # OF SELECTIONS MADE               
         BCTR  R1,0                 (LUMP ENTRIES GOING TO INV/DEMOS            
         STC   R1,ITSHNSEL           TOGETHER AS ONE SELECTION)                 
         B     GIDM232                                                          
GIDM249  EQU   *                                                                
*                                                                               
GIDM300  DS    0H                                                               
         MVI   GOSUBN,STI#         SAVE TABLES BEFORE EXITING PHASE             
         GOTO1 AGOSUB                                                           
         MVI   GOAGAIN,C'Y'                                                     
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
*                                                                               
         B     OVXIT                                                            
         DROP  R2,R3,R4                                                         
GIDMCMM2 DS    0H                                                               
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         BR    RE                                                               
***********************************************************************         
*   GET AND DISPLAY TP DATA                                                     
***********************************************************************         
DISPTPD  DS    0H                                                               
         USING DEMLINED,R2                                                      
*                                                                               
*!!!     LA    R3,FETCHIO                                                       
         L     R3,AFTCHBLK                                                      
         USING FETCHIOD,R3                                                      
*                                                                               
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
*                                                                               
DTP10    DS    0H                                                               
         MVC   RFTACOM,ACOMFACS                                                 
*                                                                               
         L     RF,AIO2                                                          
         ST    RF,RFTAIO1                                                       
         L     RF,AIO3                                                          
         ST    RF,RFTAIO2                                                       
         LA    RF,FETCHWRK                                                      
         ST    RF,RFTAWRK                                                       
         LA    RF,TPDHOOK          HOOK ROUTINE                                 
         ST    RF,RFTHOOKA                                                      
*                                                                               
         MVI   RFTAMODE,C'M'       FETCH VIA MASTER KEY                         
         MVI   RFTCNTL,RFTCDEMQ+RFTCSLVQ+RFTCFTNQ                               
*                                                                               
         L     RF,AIO                                                           
         USING REINVRCD,RF                                                      
         MVC   RFTCREP,RINVKREP    REP ID                                       
         MVC   RFTCSTAT,RINVKSTA   STATION                                      
         MVI   RFTCSRC,C'N'        NSI SOURCE                                   
         MVC   RFTCINV,RINVKINV    INV #                                        
         MVC   RFTCEFST(4),MTEFF   START AND END EFF(COMP)                      
*                                                                               
         MVC   RFTCDEMS(L'MTDEMOLS),MTDEMOLS   DEMOS                            
*                                                                               
         GOTO1 BOOKVAL,DMCB,(C'N',DEMBOOK),(1,DUB),SCANNER,0                    
*                                                                               
         MVI   RFTCBKVL,X'40'      NSI                                          
         MVC   RFTCBKYR,DUB        YEAR                                         
         MVC   RFTCBKMN,DUB+1      MONTH                                        
         MVI   RFTCBKFL,RFTCBKF4   4 WEEK PERIOD                                
*                                                                               
         MVC   RFTCBKSV,TMPBTYP    BOOK TYPE                                    
*                                                                               
         GOTO1 VFETCH,DMCB,FETCHBLK                                             
*                                                                               
DISPTPX  DS    0H                                                               
         B     OVXIT                                                            
***********************************************************************         
*        HOOK ROUTINE FOR FETCH                                                 
***********************************************************************         
TPDHOOK  NTR1                                                                   
         L     RF,RFTFFTNA         FOOTNOTE                                     
         MVC   ORIGFTNT,3(RF)                                                   
*                                                                               
         LA    R4,RFTFDEMS         DEMO VALUES                                  
         LA    R6,DEMR1            1ST RATING FIELD                             
*                                                                               
TPD10    OC    0(4,R4),0(R4)       ANY MORE DEMOS?                              
         BZ    TPDHOOKX                                                         
*                                                                               
         L     RF,0(R4)                                                         
         LA    RF,5(RF)                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
*                                                                               
         EDIT  (RF),DUB                                                         
         MVC   0(2,R6),DUB+6                                                    
         LA    R4,4(R4)                                                         
*                                                                               
         LA    RF,DEMH1            END OF 1ST CATEGORY?                         
         CR    R6,RF                                                            
         BE    TPD50                                                            
         LA    RF,DEMP2            END OF 2ND CATEGORY?                         
         CR    R6,RF                                                            
         BE    TPD50                                                            
         LA    RF,DEMP3            END OF 3RD CATEGORY?                         
         CR    R6,RF                                                            
         BE    TPD50                                                            
         LA    RF,DEMP4            END OF 4TH CATEGORY?                         
         CR    R6,RF                                                            
         BE    TPD50                                                            
         LA    R6,3(R6)                                                         
         B     *+8                                                              
*                                                                               
TPD50    LA    R6,4(R6)            END OF CATEGORY, MUST SKIP 1 MORE            
         B     TPD10                                                            
*                                                                               
TPDHOOKX DS    0H                                                               
         B     OVXIT                                                            
         DROP  R2,R3,R4,RF                                                      
***********************************************************************         
*   CHANGE FOOTNOTE FOR TRACK/DEMOS SCREEN                                      
***********************************************************************         
FTNTDEM  DS    0H                                                               
         USING DEMLINED,R2                                                      
         USING LISTTABD,R4                                                      
*                                                                               
         TM    LTFLAG,LTFPRMPT     HAS THE USER BEEN PROMPTED YET?              
         BNZ   FTD050               YES, GO VALIDATE CHANGE                     
*                                                                               
** PROMPT USER **                                                               
*                                                                               
         OI    LTFLAG,LTFPRMPT                                                  
         MVI   OURINFCD,INCHGQ                                                  
         NI    DEMFTNTH+1,XFF-X20   UNPROTECT FIELD                             
         OI    DEMFTNTH+6,X80                                                   
         LA    R0,DEMFTNTH                                                      
         ST    R0,FULL                                                          
         B     FTDX                                                             
                                                                                
*                                                                               
** VALIDATE AND UPDATE FOOTNOTE **                                              
*                                                                               
FTD050   DS    0H                                                               
         NI    LTFLAG,XFF-LTFPRMPT USER WAS PROMPTED ALREADY                    
*                                                                               
         TM    DEMFTNTH+4,X80       WAS ANYTHING INPUTTED?                      
         BZ    FTD069               NO, NOTHING TO UPDATE                       
*                                                                               
         DS    0H                   UPDATE LIST TABLE                           
         MVC   LTFTNOTE,DEMFTNT                                                 
         OI    LTFLAG,LTFFTNOV                                                  
*                                                                               
         DS    0H                   GET INVENTORY TRACK RECORD                  
         MVC   TMPKSRC,LTKSRC                                                   
         MVC   TMPKBK,LTXKBK                                                    
         XC    TMPKBK,=X'FFFF'                                                  
         CLI   TMPKBK+1,ESTMNTHQ                                                
         BNE   *+8                                                              
         MVI   TMPKBK+1,0                                                       
         MVI   GOSUBN,GIR#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                   UPDATE INVENTORY TRACK RECORD               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING RPGMELM,R3                                                       
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(X'01',AIO),0,0                         
         CLI   DMCB+12,0                                                        
         BE    FTD062                                                           
         CLI   DMCB+12,6                                                        
         BE    FTD063                                                           
         DC    H'0'                                                             
                                                                                
FTD062   DS    0H                    GET ELEM FROM RECD INTO WORK AREA          
         ICM   RF,15,DMCB+12                                                    
         ZIC   R1,1(RF)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,RPGMELM,0(RF)                                                 
         B     FTD065                                                           
                                                                                
FTD063   DS    0H                    BUILD A NEW ELEM IN WORK AREA              
         MVI   RPGMELM,X'01'                                                    
         MVI   RPGMELLN,RPGMELML                                                
         MVI   RPGMLIN,1                                                        
         B     FTD065                                                           
                                                                                
FTD065   DS    0H                    DELETE OLD ELEM FROM RECORD                
         MVI   BYTE,RPGMELML-2                                                  
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(X'01',AIO),(BYTE,RPGMLIN),0            
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DS    0H                   BUILD ELEM W/ OVERRIDING FOOTNOTE           
         MVC   RPGMNAME,SPACES                                                  
         ZIC   R1,DEMFTNTH+5                                                    
         BCTR  R1,0                                                             
         EXMVC R1,RPGMNAME,LTFTNOTE                                             
*                                                                               
         DS    0H                   PUT NEW ELEMENT INTO RECORD                 
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,RPGMELM,0                           
         MVI   GOSUBN,UAE#           ALONG W/ ACTIVITY ELEMENT                  
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                  PUT RECORD BACK TO FILE                      
         GOTO1 PUTREC                                                           
                                                                                
         DS    0H                                                               
         MVI   GOSUBN,PLR#         GO PROCESS LTRANS REQUEST                    
         GOTO1 AGOSUB                                                           
FTD069   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  FUDGE SEL FLD TO PREVENT LOOPING             
         OI    DEMSELH+6,X80         TRANSMIT SEL FIELD                         
         ZIC   R1,DEMSELH+5                                                     
         BCTR  R1,0                                                             
         STC   R1,DEMSELH+5          DECREMENT L(INPUT) BY ONE                  
         EXMVC R1,WORK,DEMSEL                                                   
         XC    DEMSEL,DEMSEL                                                    
         LTR   R1,R1                                                            
         BZ    FTD085X                                                          
         BCTR  R1,0                                                             
         EXMVC R1,DEMSEL,WORK+1      SHIFT DATA TO LEFT BY ONE                  
FTD085X  EQU   *                                                                
*                                                                               
         OI    DEMFTNTH+1,X20       PROTECT FOOTNOTE FIELD                      
         OI    DEMFTNTH+6,X80                                                   
*                                                                               
FTDX     DS    0H                                                               
         B     OVXIT                                                            
***********************************************************************         
         LTORG                                                                  
*                                                                               
*!!! BKHEAD   DS    XL18                BOOK HEADER FOR BOOKVAL                 
*!!! FETCHIO  DS    XL8000              IO NEEDED FOR FETCH CALL                
                                                                                
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01)'                            
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
SUBR01Q  EQU   (((*-RMP17+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP17+SUBR01Q                                                    
SUBR01   NMOD1 0,**1701**,R9                                                    
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
CSA#     EQU   (R01_03-*)/4+1      CLEAR (SYSSPARE) STORAGE AREAS               
STI#     EQU   (R01_04-*)/4+1      SAVE (TABLES IN) TIA AREA                    
RTI#     EQU   (R01_05-*)/4+1      RESTORE (TABLES IN) TIA AREA                 
ILT#     EQU   (R01_06-*)/4+1      INITIALIZE LIST TABLE                        
RLT#     EQU   (R01_07-*)/4+1      REFRESH LIST TABLE                           
XFD#     EQU   (R01_08-*)/4+1      EXTRACT FROM DATA                            
PLT#     EQU   (R01_09-*)/4+1      PUT ENTRY INTO LIST TABLE                    
QTM#     EQU   (R01_10-*)/4+1      CONVERT QH TO MILITARY TIME                  
KID#     EQU   (R01_11-*)/4+1      CONVERT KEY TO INTERNAL DAY                  
TBK#     EQU   (R01_12-*)/4+1      TRANSLATE BOOK                               
TID#     EQU   (R01_13-*)/4+1      TRANSLATE INTERNAL DAY                       
TMT#     EQU   (R01_14-*)/4+1      TRANSLATE MILITARY TIME                      
TWK#     EQU   (R01_15-*)/4+1      TRANSLATE WEEKS                              
CLA#     EQU   (R01_16-*)/4+1      CLEAR LIST AREA                              
ISL#     EQU   (R01_17-*)/4+1      INITIALIZE SELECT FIELDS                     
TSL#     EQU   (R01_18-*)/4+1      TEST SELECT FIELDS                           
GSI#     EQU   (R01_19-*)/4+1      GO TO SELECTED ITEM                          
VSI#     EQU   (R01_20-*)/4+1      VALIDATE SELECTED ITEM                       
UAE#     EQU   (R01_21-*)/4+1      UPDATE ACTIVITY ELEMENT                      
RKS#     EQU   (R01_22-*)/4+1      REVERSE KEY SOURCE                           
GIDM#    EQU   (R01_23-*)/4+1      GO TO INV/DEMOS SCREEN                       
PLR#     EQU   (R01_24-*)/4+1      PROCESS LTRANS REQUEST                       
RSI#     EQU   (R01_25-*)/4+1      RESTORE SELECTED ITEM                        
SKP#     EQU   (R01_26-*)/4+1      SKIP FUNCTION                                
                                                                                
R01_00   DS    0H                                                               
R01_01   B     BINVKEY             BUILD KEY TO INVENTORY RECORD                
R01_02   B     GINVRCD             GET INVENTORY RECORD                         
R01_03   B     CLRAREAS            CLEAR (SYSSPARE) STORAGE AREAS               
R01_04   B     SAVETIA             SAVE (TABLES IN) TIA AREA                    
R01_05   B     RSTRTIA             RESTORE (TABLES IN) TIA AREA                 
R01_06   B     INITLSTB            INITIALIZE LIST TABLE                        
R01_07   B     RFRSHLTB            REFRESH LIST TABLE                           
R01_08   B     XFROMDTA            EXTRACT FROM DATA                            
R01_09   B     PUTLSTAB            PUT AN ENTRY INTO LIST TABLE                 
R01_10   B     QHRTOMIL            CONVERT QH TO MILITARY TIME                  
R01_11   B     KIDAY               CONVERT KEY TO INTERNAL DAY                  
R01_12   B     TRSLTBK             TRANSLATE BOOK                               
R01_13   B     TRSLTIDY            TRANSLATE INTERNAL DAY                       
R01_14   B     TRSLTMTM            TRANSLATE MILITARY TIME                      
R01_15   B     TRSLTWK             TRANSLATE WEEKS                              
R01_16   B     CLRLSTAR            CLEAR LIST AREA                              
R01_17   B     INITSEL             INITIALIZE SELECT FIELDS                     
R01_18   B     TESTSEL             TEST SELECT FIELDS                           
R01_19   B     GOSELITM            GO TO SELECTED ITEM                          
R01_20   B     VLSELITM            VALIDATE SELECTED ITEM                       
R01_21   B     UPACTVEL            UPDATE ACTIVITY ELEMENT                      
R01_22   B     REVKSRC             REVERSE KEY SOURCE                           
R01_23   B     GOINVDEM            GO TO INV/DEMOS SCREEN                       
R01_24   B     PRCLTRNS            PROCESS LTRANS REQUEST                       
R01_25   B     RESELITM            RESTORE SELECTED ITEM                        
R01_26   B     SKIPITM             SKIP ITEM                                    
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    RC,RC                                                            
NO_01    LTR   RC,RC                                                            
XIT_01   XIT1                                                                   
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--BIK#)'                      
*------------------------ BUILD INVENTORY KEY ------------------------*         
*                                                                               
BINVKEY  DS    0H                                                               
         XC    KEY,KEY                                                          
IVK      USING REINVRCD,KEY                                                     
         MVI   IVK.RINVKTYP,X'12'                                               
         MVC   IVK.RINVKREP,AGENCY                                              
         MVC   IVK.RINVKSTA,IKSTTN                                              
         MVC   IVK.RINVKINV,IKINV                                               
         MVC   IVK.RINVKSTD,IKEFFDB                                             
         MVC   IVK.RINVKSRC,TMPKSRC                                             
         MVC   IVK.RINVKBK,TMPKBK                                               
         DROP  IVK                                                              
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--GIR#)'                      
*------------------------ GET INVENTORY HEADER -----------------------*         
                                                                                
* Gets the inventory header record.                                             
* At entry,                                                                     
*   TMPKSRC = key source to look for,                                           
*   TMPKBK  = book to look for.                                                 
                                                                                
GINVRCD  DS    0H                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         ZICM  R1,IKEFFDB,(7)                                                   
         BNZ   *+6                                                              
         BCTR  R1,0                                                             
         STCM  R1,7,TMPBDATE       EFFECTIVE DATE FOR THIS ROUTINE              
                                                                                
         MVI   GOSUBN,BIK#         BUILD KEY OF INVENTORY HEADER                
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
         MVC   RINVKSRC,TMPKSRC                                                 
         MVC   RINVKBK,TMPKBK                                                   
                                                                                
GIR015   DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   RINVKEY(IKYINVL),KEYSAVE                                         
         BNE   GIR050                                                           
                                                                                
         CLC   RINVKSTD,TMPBDATE                                                
         BH    GIR050                                                           
*                                                                               
GIR020   DS    0H                  CHECK KEY SOURCE                             
         CLC   RINVKSRC,TMPKSRC                                                 
         BH    GIR010                                                           
         BE    GIR030                                                           
                                                                                
         DS    0H                   RINVKSRC < IKKSRC                           
         MVC   RINVKSRC,TMPKSRC                                                 
         MVC   RINVKBK,TMPKBK                                                   
         B     GIR015                                                           
*                                                                               
GIR030   DS    0H                  CHECK BOOK                                   
         CLC   RINVKBK,TMPKBK                                                   
         BH    GIR010                                                           
         BE    GIR040                                                           
                                                                                
         DS    0H                   RINVKBK < IKBOOK                            
         MVC   RINVKBK,TMPKBK                                                   
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
         GOTO1 READ                                                             
         CLC   RINVKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 GETREC                                                           
         B     GIRXY                                                            
         DROP  R6                                                               
                                                                                
                                                                                
GIRXN    DS    0H                                                               
         B     NO_01                                                            
GIRXY    DS    0H                                                               
         B     YES_01                                                           
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--CSA#)'                      
*------------------------ CLEAR STORAGE AREAS ------------------------*         
*                                                                               
CLRAREAS DS    0H                                                               
         LA    RF,TABCLRQ          RF = COUNTER                                 
         LH    R4,=Y(TABCLR-RMP17)                                              
         A     R4,MYBASE1                                                       
                                                                                
CSA10    DS    0H                  SET FULL TO BASE OF STORAGE AREA             
         MVC   FULL,ATIA                                                        
         CLI   0(R4),C'I'                                                       
         BE    CSA20                                                            
         MVC   FULL,ASYSD                                                       
         CLI   0(R4),C'S'                                                       
         BE    CSA20                                                            
         DC    H'0'                                                             
CSA20    DS    0H                                                               
         ZICM  R0,1(R4),(3)                                                     
         A     R0,FULL             R0-->AREA TO CLEAR                           
         ZICM  R1,3(R4),(3)        R1=LENGTH TO CLEAR                           
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R0,R2               GO CLEAR AREA                                
         LA    R4,L'TABCLR(R4)                                                  
         BCT   RF,CSA10                                                         
*                                                                               
CSAX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--ILT#)'                      
*----------------------- INITIALIZE LIST TABLE -----------------------*         
                                                                                
* Initializes LISTTAB to the data that will be listed on the screen.            
                                                                                
INITLSTB DS    0H                                                               
         MVI   ITITNLE,0           NO LIST TABLE ENTRIES YET                    
                                                                                
         L     R0,ALISTTAB                                                      
         LH    R1,=Y(LISTTABX-LISTTAB)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR LIST TABLE                             
*                                                                               
         LA    R4,TEMPLT                                                        
         USING LISTTABD,R4                                                      
*                                                                               
         DS    0H                  INITIALIZE BINSRCH PARAMETERS                
         XC    BSPARMS(6*4),BSPARMS                                             
         MVC   BSP2,ALISTTAB        A(TABLE)                                    
         LA    R0,LISTTABQ                                                      
         ST    R0,BSP4              L(RECORD)                                   
         LA    R0,LTKEYL                                                        
         ST    R0,BSP5              L(KEY)                                      
         MVI   BSP5,LTKEY-LISTTABD  DISPLACEMENT OF KEY                         
         LA    R0,MXLTNTRY                                                      
         ST    R0,BSP6              MAXIMUM # OF ENTRIES                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   DATADISP,=AL2(RINVPEL-RINVREC)                                   
                                                                                
         L     R5,AINVHDR                                                       
         USING RINVKEY,R5                                                       
         MVC   KEY(IKYSTDL),RINVKEY          SET KEY UP FOR READS               
         MVI   KEY+(RINVKSRC-RINVKEY),X01    SKIP READ OF INV HDR               
         DROP  R5                                                               
*                                                                               
         MVC   SVDMINBT,DMINBTS    SAVE CURRENT DATAMANAGER INPUT               
         CLI   ACTNUM,24           IS THIS A DTRACK REQUEST?                    
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       YES, RETURN DELETED TRACKS                   
*                                                                               
         DS    0H                                                               
         GOTO1 HIGH                                                             
         B     ILT024                                                           
                                                                                
ILT022   DS    0H                                                               
         CLI   ACTNUM,24           IS THIS A DTRACK REQUEST?                    
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       YES, RETURN DELETED TRACKS                   
         GOTO1 SEQ                                                              
                                                                                
ILT024   DS    0H                                                               
         CLC   KEY(IKYSTDL),KEYSAVE                                             
         BNE   ILT100                                                           
         CLI   KEY+(RINVKSRC-RINVKEY),XFF                                       
         BE    ILT100                                                           
*                                                                               
         CLI   ACTNUM,24                   IS THIS A DTRACK REQUEST?            
         BNE   ILT026                       NO                                  
         TM    KEY+(RINVLEN-RINVKEY),X'80' YES, USE ONLY IF TRACK               
         BNO   ILT022                         MARKED AS DELETED                 
*                                                                               
ILT026   XC    TEMPLT,TEMPLT       CLEAR AREA HOLDING ENTRY                     
*                                                                               
         MVC   TMPKSRC,KEY+(RINVKSRC-RINVKEY)                                   
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB                                                           
         BNE   ILT022                                                           
*                                                                               
         DS    0H                                                               
         GOTO1 GETREC                                                           
         MVC   DMINBTS,SVDMINBT    RESTORE DATAMANAGER INPUT BITS               
                                                                                
         DS    0H                  GET SOURCE AND BOOK                          
         L     R6,AIO                                                           
         USING REINVRCD,R6                                                      
         MVC   LTKSRC,RINVKSRC                                                  
         MVC   LTXKBK,RINVKBK                                                   
         CLI   RINVKBK+1,0          FOR EST/YR TRACKS,                          
         BNE   *+8                                                              
         MVI   LTXKBK+1,ESTMNTHQ     FUDGE IN A DUMMY MONTH                     
         XC    LTXKBK,=X'FFFF'                                                  
         DROP  R6                                                               
                                                                                
         MVC   LTBTK,TMPQLFK       (FROM RKS# ROUTINE ABOVE)                    
                                                                                
         DS    0H                  GET DATA FROM CODE ELEMENT                   
         MVI   ELCODE,X'CD'                                                     
         LR    R3,R6                                                            
         BAS   RE,GETEL                                                         
         BNE   ILT035                                                           
         USING RINVCEL,R3                                                       
         MVC   LTCODE,RINVCODE                                                  
         MVC   LTCSET,RINVCSET     EST/PRJ/SPCL-SURVEY BOOK                     
         DROP  R3                                                               
                                                                                
ILT035   DS    0H                  GET FOOTNOTE (PROGRAM NAME)                  
         MVI   ELCODE,X'01'         PROGRAM NAME TEXT ELEMENT                   
         LR    R3,R6                                                            
         BAS   RE,GETEL                                                         
         B     ILT035C                                                          
ILT035B  BAS   RE,NEXTEL                                                        
ILT035C  BNE   ILT040                                                           
         USING RPGMELM,R3                                                       
         CLI   RPGMLIN,1            LOOK FOR LINE # 1                           
         BNE   ILT035B                                                          
*&&DO                                                                           
         MVC   LTFTNOTE,RPGMNAME                                                
*&&                                                                             
         ZIC   RF,1(R3)             AS PER BRIAN,                               
         AR    RF,R3                                                            
         SH    RF,=H'16'                                                        
         MVC   LTFTNOTE,0(RF)        FOOTNOTE IS LAST 16 CHARS OF ELEM          
         DROP  R3                                                               
                                                                                
ILT040   DS    0H                                                               
         MVI   ELCODE,X'03'         TRANSFER FROM ELEMENT                       
         LR    R3,R6                                                            
         BAS   RE,GETEL                                                         
         BNE   ILT040X                                                          
         USING RINVFREL,R3                                                      
         CLI   RINVFRLN,RINVFRDT-RINVFREL     CHECK IF ANY FROM DETAIL          
         BE    ILT040X                                                          
         CLI   RINVFRLN,RINVFRDT-RINVFREL+1   THIS IS JUST                      
         BH    ILT040B                                                          
         CLI   RINVFRDT,0                      TO PROTECT MYSELF                
         BE    ILT040X                                                          
ILT040B  DS    0H                                                               
         ZIC   R0,RINVFRLN                                                      
         SH    R0,=Y(RINVFRDT-RINVFREL)       R0 = L(FROM DETAILS)              
         LA    R1,L'LTFRDETL                                                    
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,LTFRDETL,RINVFRDT                                             
         DROP  R3                                                               
ILT040X  EQU   *                                                                
                                                                                
ILT045   DS    0H                                                               
         ST    R4,BSP1             A(RECORD)                                    
         MVI   BSP1,X01            INSERT RECORD IF NOT FOUND                   
         GOTO1 VBINSRCH,BSPARMS                                                 
                                                                                
         B     ILT022                                                           
                                                                                
*                                                                               
** ASSIGN LIST TABLE ENTRY NUMBERS **                                           
*                                                                               
ILT100   DS    0H                                                               
         L     R0,BSP3             R0 = # OF ENTRIES IN LIST TABLE              
         STC   R0,ITITNLE           REMEMBER IT                                 
         SR    R1,R1               R1 USED AS COUNTER                           
         L     R4,ALISTTAB                                                      
                                                                                
ILT102   DS    0H                                                               
         LA    R1,1(R1)            INCREMENT COUNTER                            
         STC   R1,LTNTRY            AND ASSIGN IT AS ENTRY NUMBER               
         LA    R4,LISTTABQ(R4)                                                  
                                                                                
         CR    R1,R0               REACHED END YET?                             
         BL    ILT102               NOPE                                        
         CLI   LTNTRY,0             YES, MAKE SURE NO MORE ENTRIES              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
** EXIT **                                                                      
*                                                                               
ILTX     DS    0H                                                               
         B     XIT_01                                                           
                                                                                
                                                                                
         DROP  R4                                                               
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBR01--RLT#)'            
*------------------------- REFRESH LIST TABLE ------------------------*         
                                                                                
RFRSHLTB DS    0H                                                               
         PRINT OFF                                                              
*&&DO                                                                           
         SR    R1,R1               COUNTS # OF ENTRIES SELECTED                 
         L     RE,ALISTTAB                                                      
         USING LISTTABD,RE                                                      
                                                                                
RLT10    DS    0H                  FIND THE 1ST NON-SELECTED ENTRY              
         CLI   0(RE),EOT                                                        
         BE    RLT30                                                            
         TM    LTFLAG,LTFSEL                                                    
         BZ    RLT20                                                            
         LA    RE,LISTTABQ(RE)                                                  
         LA    R1,1(R1)            UPDATE COUNT OF SELECTED ENTRIES             
         B     RLT10                                                            
         DROP  RE                                                               
*                                                                               
RLT20    DS    0H                                                               
         LA    RF,LISTTABQ(RE)                                                  
         USING LISTTABD,RF                                                      
                                                                                
RLT22    DS    0H                                                               
         CLI   0(RF),EOT                                                        
         BE    RLT30                                                            
         TM    LTFLAG,LTFSEL       IF ENTRY WAS SELECTED,                       
         BZ    RLT24                                                            
         MVC   0(LISTTABQ,RE),0(RF)  MOVE IT UP TO NEXT AVAILABLE SLOT,         
         LA    R1,1(R1)                                                         
         STC   R1,(LTNTRY-LISTTABD)(RE)  AND ASSIGN A NEW ENTRY #               
         LA    RE,LISTTABQ(RE)                                                  
                                                                                
RLT24    DS    0H                  BUMP PAST NON-SELECTED ENTRY                 
         LA    RF,LISTTABQ(RF)                                                  
         B     RLT22                                                            
*                                                                               
RLT30    DS    0H                  REACHED LAST ENTRY                           
         ST    RE,AITITNLE          STORE A(NEXT AVAILABLE SLOT)                
         LA    RF,MXLTNTRY          MAX # OF ENTRIES IN TABLE,                  
         STC   R1,ITITNLE            LESS # OF ENTRIES IN TABLE,                
         SR    RF,R1                 YIELDS # OF SLOTS LEFT                     
         BZ    RLTX                                                             
         BP    *+6                                                              
         DC    H'0'                                                             
         MH    RF,=Y(LISTTABQ)                                                  
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                CLEAR REMAINING PORTION OF TABLE            
         DROP  RF                                                               
*                                                                               
RLTX     DS    0H                                                               
         B     XIT_01                                                           
*&&                                                                             
         PRINT ON                                                               
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBR01--STI# && R+        
               TI#)'                                                            
*--------------------------- SAVE TIA TABLES -------------------------*         
                                                                                
* saves TIA tables into TEMPSTR                                                 
                                                                                
SAVETIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ                                                     
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=Y(TIASVLEN)                                          
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ATIA,0                       
         B     XIT_01                                                           
                                                                                
                                                                                
*-------------------------- RESTORE TIA TABLES -----------------------*         
                                                                                
* restores TIA tables from TEMPSTR                                              
                                                                                
RSTRTIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ        3RD PARAMETER                                
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=Y(TIASVLEN)                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ATIA,0                      
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBR01--XFD#)'            
*------------------------- EXTRACT FROM DATA -------------------------*         
                                                                                
* At entry,                                                                     
*   AIO = A(inventory record)                                                   
                                                                                
XFROMDTA DS    0H                                                               
         L     R6,AIO                                                           
         MVC   DATADISP,=Y(RINVPEL-REINVRCD)                                    
*                                                                               
         DS    0H                  GET SOURCE                                   
*^^      MVC   LPSRC,IKRSVC                                                     
*                                                                               
         DS    0H                  GET FROM BOOK, STATION, & BOOK TYPE          
         LR    R3,R6                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   XFD20                                                            
         USING RINVFREL,R3                                                      
         MVC   LPFRBK,RINVFRBK                                                  
         MVC   LPSTTN,RINVFRST                                                  
         MVC   LPBTYP,RINVFRBT                                                  
         DROP  R3                                                               
*                                                                               
XFD20    DS    0H                  GET FILE                                     
         LR    R3,R6                                                            
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         BNE   XFD30                                                            
         USING RINVCEL,R3                                                       
         MVC   LPFILE,=C'TP '                                                   
         CLC   RINVCODE,SPACES                                                  
         BE    XFD22                                                            
         MVC   LPFILE,=C'PAV'                                                   
         DROP  R3                                                               
                                                                                
XFD22    DS    0H                                                               
*                                                                               
XFD30    DS    0H                  GET DEMO CATEGORY NUMBER                     
         LR    R3,R6                                                            
         MVI   ELCODE,X'DE'                                                     
         MVI   LPDEMC,DNHOMES      SET HOMES AS DEFAULT                         
                                                                                
         BAS   RE,GETEL                                                         
         B     XFD32A                                                           
XFD32    BAS   RE,NEXTEL                                                        
XFD32A   BNE   XFD40                                                            
         LA    R1,RINVODEM-RINVOEL   ASSUME OLD FORMAT                          
         CLI   (RINVOCOD-RINVOEL)(R3),6                                         
         BE    XFD35                                                            
         LA    R1,RINVNODM-RINVNOEL  ASSUME NEW FORMAT                          
         CLI   (RINVNOCD-RINVNOEL)(R3),12                                       
         BE    XFD35                                                            
         B     XFD32                 NEITHER FORMAT, TRY AGAIN                  
                                                                                
XFD35    DS    0H                                                               
         AR    R1,R3               R1-->DEMO CATEGORY NUMBER                    
         MVC   LPDEMC,0(R1)                                                     
*                                                                               
XFD40    DS    0H                                                               
*                                                                               
XFDX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBR01--PLT#)'            
*------------------------ PUT INTO LIST TABLE ------------------------*         
                                                                                
* Given the list parameters, this routine puts together a LISTTAB               
*  entry (by calling DEMAND), which will get enterred into the table.           
* At entry,                                                                     
*   AITITNLE = A(next available slot in LISTTAB),                               
*   ITITNLE  = # of entries in LISTTAB so far,                                  
*   List Params (fields w/ prefix 'LP') are set                                 
* At exit,                                                                      
                                                                                
PUTLSTAB DS    0H                                                               
         PRINT OFF                                                              
*&&DO                                                                           
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
*                                                                               
** CLEAR DBLOCK AREA **                                                         
*                                                                               
         LR    R0,R5                                                            
         LA    R1,DBLOCK1X-DBLOCK1                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
** SET UP DBLOCK **                                                             
*                                                                               
         MVC   DBFILE,LPFILE                                                    
         MVC   DBAREC,AIO2                                                      
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBBTYPE,LPBTYP                                                   
         MVC   DBSELSRC,LPSRC                                                   
         MVC   DBSELBK,LPBOOK                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSTA,LPSTTN                                                  
         MVC   DBSELAGY,TWAAGY                                                  
         MVC   DBSELDAY,LPIDAY                                                  
         MVC   DBSELTIM,LPTIME                                                  
                                                                                
         DS    0H                  FILE SPECIFIC PARAMETERS                     
         CLC   DBFILE,=C'TP '                                                   
         BE    PLT100                                                           
         B     PLT300                                                           
*                                                                               
PLT100   DS    0H                  TIME PERIOD SPECIFIC PARAMETERS              
*                                                                               
** CALL DEMAND **                                                               
*                                                                               
PLT300   DS    0H                                                               
         GOTO1 DEMAND,DMCB,DBLOCKD,DEMHOOK                                      
*                                                                               
PLT400   DS    0H                                                               
*                                                                               
PLTX     DS    0H                                                               
         B     XIT_01                                                           
         EJECT                                                                  
*                                                                               
** DEMAND HOOK **                                                               
*                                                                               
* At entry,                                                                     
*   AITITNLE = A(next available slot in LISTTAB),                               
*   ITITNLE  = # of entries in LISTTAB so far,                                  
*   TYPELP   = type of list parms in DEMAND call,                               
*   R5-->DBLOCK.                                                                
                                                                                
DEMHOOK  DS    0H                                                               
DMHK     NTR1                                                                   
*                                                                               
** EXTRACT PERTINENT INFORMATION **                                             
*                                                                               
         USING DBLOCKD,R5                                                       
*                                                                               
*** NON FILE-SPECIFIC INFORMATION ***                                           
*                                                                               
         DS    0H                  FILE                                         
         MVC   TMPDFIL,DBFILE                                                   
*                                                                               
         DS    0H                  MEDIA                                        
         MVC   TMPMED,DBACTMED                                                  
*                                                                               
         DS    0H                  STATION                                      
         MVC   TMPSTTN,DBACTSTA                                                 
*                                                                               
         DS    0H                  BOOK                                         
         MVC   TMPBOOK,DBACTBK                                                  
*                                                                               
         DS    0H                  BOOK TYPE                                    
         MVC   TMPBTYP,DBBTYPE                                                  
*                                                                               
         DS    0H                  DAY(S)                                       
         MVC   WORK,SPACES                                                      
         GOTO1 DEFINE,MYDMCB,=C'DAY',DBLOCKD,WORK                               
         MVC   TMPIDAY,WORK                                                     
*                                                                               
         DS    0H                  TIMES & # OF QUARTER HOURS                   
         MVC   WORK,SPACES                                                      
         GOTO1 DEFINE,MYDMCB,=C'TIME',DBLOCKD,WORK                              
         MVC   TMPSETM,WORK+2                                                   
         MVC   TMPNQHR,WORK+6                                                   
*                                                                               
         DS    0H                  PROGRAM NAME                                 
         MVC   WORK,SPACES                                                      
         GOTO1 DEFINE,MYDMCB,=C'PROGRA',DBLOCKD,WORK                            
         MVC   TMPPGNAM,WORK                                                    
*                                                                               
         DS    0H                  SOURCE                                       
         MVC   TMPSRC,=C'NSI'       ASSUME NIELSEN                              
         CLI   DBACTSRC,C'N'                                                    
         BE    DMHK020                                                          
         MVC   TMPSRC,=C'SRC'       ASSUME STRATEGY                             
         CLI   DBACTSRC,C'S'                                                    
         BE    DMHK020                                                          
         MVC   TMPSRC,=C'ARB'       ASSUME ARBITRON                             
         CLI   DBACTSRC,C'A'                                                    
         BE    DMHK020                                                          
         DC    H'0'                                                             
*                                                                               
*** FILE-SPECIFIC INFORMATION ***                                               
*                                                                               
DMHK020  DS    0H                                                               
         CLC   DBFILE,=C'PAV'                                                   
         BE    DMHK030                                                          
         CLC   DBFILE,=C'TP '                                                   
         BE    DMHK040                                                          
         CLC   DBFILE,=C'IUN'                                                   
         BE    DMHK050                                                          
         DC    H'0'                                                             
*                                                                               
DMHK030  DS    0H                  PAV FILE                                     
*                                                                               
DMHK040  DS    0H                  TIME PERIOD FILE                             
*                                                                               
DMHK050  DS    0H                  INVENTORY FILE                               
         DROP  R5                                                               
*                                                                               
** UPDATE TABLE W/ CURRENT INFORMATION **                                       
*                                                                               
DMHK100  DS    0H                  BUILD ENTRY IN  WORK                         
         XC    WORK,WORK                                                        
LTW      USING LISTTABD,WORK                                                    
         MVC   LTW.LTKSRC,TMPSRC                                                
         MVC   LTW.LTXKBK,TMPBOOK                                               
         MVC   LTW.LTBTYP,TMPBTYP                                               
         MVC   LTW.LTIDAY,TMPIDAY                                               
                                                                                
         MVC   LTW.LTTIME,TMPSETM                                               
         MVC   LTW.LTPGNAM,TMPPGNAM                                             
*                                                                               
         DS    0H                  LOCATE ENTRY IN LISTTAB                      
         L     R4,AITITNLE          R4-->PLACE TO START SEARCH                  
         USING LISTTABD,R4                                                      
         LA    R0,MXLTNTRY                                                      
                                                                                
DMHK112  DS    0H                                                               
         OC    0(LISTTABQ,R4),0(R4)                                             
         BZ    DMHK114                                                          
         CLC   LTKEY(LTKEYL),LTW.LTKEY                                          
         BE    DMHK120                                                          
         LA    R4,LISTTABQ(R4)                                                  
         BCT   R0,DMHK112                                                       
         DC    H'0'                TOO MANY LIST ENTRIES                        
                                                                                
DMHK114  DS    0H                  NEED TO ADD ENTRY TO LIST TABLE              
         MVC   LISTTABD(LISTTABQ),LTW.LISTTABD                                  
         ZIC   R1,ITITNLE                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ITITNLE          UPDATE # OF ENTRIES IN LISTTAB               
         STC   R1,LTNTRY           ASSIGN # TO ENTRY                            
                                                                                
         CLI   TYPELP,TLPHIST      IF THIS IS PART OF INV'S HISTORY             
         BNE   DMHK114X                                                         
         OI    LTFLAG,LTFSEL        FLAG IT SELECTED,                           
         LA    R0,LISTTABD+LISTTABQ                                             
         ST    R0,AITITNLE          AND RESET A(NEXT ENTRY)                     
                                                                                
DMHK114X EQU   *                                                                
         DROP  LTW                                                              
*                                                                               
DMHK120  DS    0H                  R4-->LIST TABLE ENTRY TO UPDATE              
                                                                                
         DS    0H                  UPDATE START/END TIMES                       
         CLC   LTSTIME,TMPSTIM                                                  
         BNH   *+10                                                             
         MVC   LTSTIME,TMPSTIM      START TO EARLIER START                      
         CLC   LTETIME,TMPETIM                                                  
         BNL   *+10                                                             
         MVC   LTETIME,TMPETIM      END TO LATER END                            
                                                                                
         DS    0H                  UPDATE DEMO VALUES                           
                                                                                
*                                                                               
DMHK140  DS    0H                                                               
         DROP  R4                                                               
         B     XIT_01                                                           
*&&                                                                             
         PRINT ON                                                               
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBR01--QTM#)'            
*---------- ROUTINE TO CONVERT QUARTER HOUR TO MILITARY TIME ---------*         
                                                                                
* At entry,                                                                     
*   TMPQHR  = input quarter hour,                                               
*   TMPMED  = media.                                                            
* At exit,                                                                      
*   TMPMTIM = output military time.                                             
                                                                                
QHRTOMIL DS    0H                                                               
         SR    R0,R0                                                            
         ZIC   R1,TMPQHR                                                        
         D     R0,=F'4'                                                         
         LA    R1,5(R1)            BASE = 5AM IF RADIO,                         
         CLI   TMPMED,C'R'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)             ELSE, BASE = 6AM                            
         CH    R1,=H'24'           TEST AFTER MIDNIGHT                          
         BL    *+8                                                              
         SH    R1,=H'24'           YES - GO BACK ONE DAY                        
         MH    R1,=H'100'                                                       
         MH    R0,=H'15'                                                        
         AR    R1,R0               R1 CONTAINS MILITARY TIME                    
         STCM  R1,3,TMPMTIM                                                     
*                                                                               
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBR01--KID#)'            
*-------------------- CONVERT KEY TO INTERNAL DAY --------------------*         
                                                                                
* At entry,                                                                     
*   TMPDFIL = demo file,                                                        
*   TMPKDAY = key day to convert.                                               
* At exit,                                                                      
*   TMPIDAY = converted internal day.                                           
                                                                                
KIDAY    DS    0H                                                               
         L     RF,ADAYTAB                                                       
         USING DAYTABD,RF                                                       
                                                                                
KID10    DS    0H                                                               
         CLI   0(RF),XFF           IF NO MATCH FOUND,                           
         BE    KID20                USE THIS TO INDICATE DAY                    
         CLI   0(RF),0             IF ENTRY APPLY TO ALL FILES,                 
         BE    *+14                 SKIP FILE-MATCHING CHECK                    
         CLC   DYTFILE,TMPDFIL                                                  
         BNE   KID15                                                            
         CLC   DYTKDAY,TMPKDAY                                                  
         BE    KID20                                                            
                                                                                
KID15    DS    0H                                                               
         LA    RF,DYTLEN(RF)                                                    
         B     KID10                                                            
*                                                                               
KID20    DS    0H                                                               
         MVC   TMPIDAY,DYTIDAY                                                  
         DROP  RF                                                               
*                                                                               
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBR01--TBK#)'            
*--------------------------- TRANSLATE BOOK --------------------------*         
                                                                                
* At entry,                                                                     
*   TMPBOOK = book to translate,                                                
*   TMPKSRC = source from key of inventory record.                              
* At exit,                                                                      
*   WORK    = converted book.                                                   
*   HALF    = L(converted book)                                                 
                                                                                
TRSLTBK  DS    0H                                                               
                                                                                
         DS    0H                  TRANSLATE KEY SOURCE FIRST                   
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         MVC   WORK,SPACES                                                      
*^^TEMPFIX 4/08/96                                                              
         OC    TMPBOOK,TMPBOOK                                                  
         BZ    TBKX                                                             
*^^TEMPFIX                                                                      
         LA    R2,WORK                                                          
         XC    DUB,DUB                                                          
         MVC   DUB(L'TMPBOOK),TMPBOOK                                           
         CLI   TMPBOOK+1,ESTMNTHQ  FOR EST/YR BOOKS                             
         BNE   *+8                                                              
         MVI   DUB+1,0                                                          
*                                                                               
         DS    0H                  CHECK FOR PROJ/EST/SPCL SURVEYS              
         CLI   TMPQLFY,C' '        SKIP IF ACTUAL                               
         BE    TBK020                                                           
         MVC   0(1,R2),TMPQLFY                                                  
         LA    R2,1(R2)                                                         
                                                                                
*                                                                               
TBK020   DS    0H                  FORMAT BOOK VALUE                            
         CLI   DUB+1,0                                                          
         BNE   TBK025                                                           
*                                                                               
         DS    0H                   BOOK IS EST/YR                              
         MVC   0(2,R2),=C'ST'                                                   
         ZIC   R1,DUB                                                           
         EDIT  (R1),(2,2(R2)),DUB=MYDUB,WRK=MYWORK,ZERO=NOBLANK,FILL=0          
         LA    R2,2+2(R2)                                                       
         B     TBK029                                                           
*                                                                               
TBK025   DS    0H                   BOOK IS MON/YR                              
         GOTO1 DATCON,DMCB,(X'83',DUB),(6,(R2))                                 
         LA    R2,6(R2)                                                         
*                                                                               
TBK029   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  GET BOOKTYPE                                 
         CLI   TMPBTYP,0           (FROM RKS# ROUTINE ABOVE)                    
         BE    TBK039                                                           
         MVI   0(R2),C'('                                                       
         MVC   1(1,R2),TMPBTYP                                                  
         MVI   2(R2),C')'                                                       
         LA    R2,3(R2)                                                         
TBK039   EQU   *                                                                
                                                                                
*                                                                               
** DONE FORMATTING **                                                           
*                                                                               
         DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R2,R0                                                            
         STH   R2,HALF             RETURN L(FORMATTED BOOK) IN HALF             
*                                                                               
** EXIT **                                                                      
*                                                                               
TBKX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBR01--TID#)'            
*----------------------- TRANSLATE INTERNAL DAY ----------------------*         
                                                                                
* At entry,                                                                     
*   TMPIDAY = day code to translate.                                            
* At exit,                                                                      
*   WORK    = converted day in compressed format,                               
*   BYTE    = length of converted day output.                                   
                                                                                
TRSLTIDY DS    0H                                                               
                                                                                
         MVC   WORK,SPACES                                                      
         XC    DUB,DUB                                                          
         MVC   DUB(L'TMPIDAY),TMPIDAY                                           
*^^TEMPFIX 3/26/96                                                              
         CLI   DUB,0                                                            
         BNE   *+12                                                             
         MVI   BYTE,1                                                           
         B     TIDX                                                             
*^^TEMPFIX                                                                      
                                                                                
         GOTO1 UNDAY,DMCB,DUB,WORK                                              
*                                                                               
         CLI   WORK+7,0            IF L(OUTPUT) IS GREATER THAN 7,              
         BE    TID020                                                           
         MVC   WORK,SPACES          GET IT IN MTWTFSS FORMAT                    
         GOTO1 UNDAY,DMCB,DUB,(X'07',WORK)                                      
*                                                                               
TID020   DS    0H                  DETERMINE LENGTH OF OUTPUT                   
         LA    R0,WORK                                                          
         LA    R1,WORK+6            MAX OUTPUT LEN IS 7                         
         CLI   0(R1),0              FIND FIRST NON-NULL CHARACTER               
         BNE   *+8                                                              
         BCT   R1,*-8                WHILE BUMPING BACKWARDS                    
         SR    R1,R0                SOMETHING'S AMISS IF NEGATIVE               
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    R1,1(R1)             ADJUST FOR OFF-BY-ONE ERROR                 
         STC   R1,BYTE                                                          
*                                                                               
TIDX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBR01--TMT#)'            
*---------------------- TRANSLATE MILITARY TIME ----------------------*         
                                                                                
* At entry,                                                                     
*   DUB(4) = start(/end) time(s) to translate.                                  
* At exit,                                                                      
*   WORK   = converted time in AM/PM mode,                                      
*   BYTE    = length of converted time.                                         
                                                                                
TRSLTMTM DS    0H                                                               
                                                                                
         MVC   WORK,SPACES                                                      
                                                                                
         GOTO1 UNTIME,DMCB,DUB,WORK                                             
*                                                                               
         DS    0H                  DETERMINE LENGTH OF OUTPUT                   
         CLI   WORK,0                                                           
         BNE   *+8                                                              
         MVI   WORK,C' '                                                        
         LA    R0,WORK                                                          
         LA    R1,WORK+10           MAX OUTPUT LEN IS 11                        
         CLI   0(R1),0              FIND FIRST NON-NULL CHARACTER               
         BNE   *+8                                                              
         BCT   R1,*-8                WHILE BUMPING BACKWARDS                    
         SR    R1,R0                SOMETHING'S AMISS IF NEGATIVE               
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    R1,1(R1)             ADJUST FOR OFF-BY-ONE ERROR                 
         STC   R1,BYTE                                                          
*                                                                               
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBR01--TWK#)'            
*-------------------------- TRANSLATE WEEKS --------------------------*         
                                                                                
* At entry,                                                                     
*   TMPMTIM = time to translate.                                                
* At exit,                                                                      
*   WORK    = converted time in AM/PM mode.                                     
                                                                                
TRSLTWK  DS    0H                                                               
*                                                                               
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--CLA#)'                      
*-------------------------- CLEAR LIST AREA --------------------------*         
                                                                                
* Clears and transmits the area on the screen used for list display.            
                                                                                
CLRLSTAR DS    0H                                                               
         LA    R2,ITKSELH          START                                        
         LA    RF,ITKPFLNH         FINISH                                       
*                                                                               
         CLI   MTSCRN,X'E9'        TRACK/DEMOS SCREEN?                          
         BE    CLA050                                                           
*                                                                               
CLA010   DS    0H                                                               
         CR    R2,RF               IF UP TO FINISHING POINT,                    
         BNL   CLAXIT               DON'T GO ANY FURTHER                        
                                                                                
         ZIC   R0,0(R2)            R0 = L(TWA HEADER & FIELD)                   
                                                                                
         DS    0H                  CALCULATE LENGTH FOR EX INSTRUCTION          
         LR    R1,R0                                                            
         LA    RE,8+1                                                           
         TM    1(R2),X02            CHECK FOR EXTENDED FLD HEADER               
         BZ    *+8                                                              
         LA    RE,8(RE)                                                         
         SR    R1,RE                R1 = EXECUTED LENGTH                        
                                                                                
         DS    0H                  CLEAR DATA FIELD                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X80            AND TRANSMIT IT                             
                                                                                
         DS    0H                  BUMP TO NEXT TWA FIELD                       
         AR    R2,R0                                                            
         B     CLA010                                                           
*                                                                               
CLA050   DS    0H                                                               
         LA    R2,IT2SELH                                                       
         LA    RF,IT2PFLNH                                                      
*                                                                               
CLA060   DS    0H                                                               
         CR    R2,RF               IF UP TO FINISHING POINT,                    
         BNL   CLAXIT               DON'T GO ANY FURTHER                        
*                                                                               
         USING DEMLINED,R2                                                      
         XC    DEMSEL,DEMSEL                                                    
         OI    DEMSELH+6,X'80'                                                  
*                                                                               
         XC    DEMDATA,DEMDATA                                                  
         OI    DEMDATAH+6,X'80'                                                 
*                                                                               
         XC    DEMFTNT,DEMFTNT                                                  
         OI    DEMFTNTH+6,X'80'                                                 
         LA    R2,DEMLINEQ(R2)                                                  
         B     CLA060                                                           
*                                                                               
CLAXIT   B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--ISL#)'                      
*------------------------- TEST SELECT FIELD -------------------------*         
                                                                                
* Initializes SEL fields.  This routine should only get called when             
*  there is a change of action, key, option.                                    
                                                                                
INITSEL  DS    0H                                                               
         CLI   MTSCRN,X'E9'        TRACK/DEMOS SCREEN?                          
         BE    ISL20                                                            
*                                                                               
         LA    R0,MXLSTLNS          CLEAR ALL SELECT FIELDS                     
         LA    R2,ITKSELH                                                       
         USING DLINED,R2                                                        
*                                                                               
ISL10    DS    0H                                                               
         MVI   DLSELH+5,0          ZERO OUT THE LENGTH                          
         XC    DLSEL,DLSEL         CLEAR OUT ANY DATA                           
         OI    DLSELH+6,X80        TRANSMIT FIELD                               
         BCT   R0,ISLBUMP                                                       
         B     ISLX                                                             
                                                                                
ISLBUMP  DS    0H                  MAKE SURE ONLY R2 IS CHANGED                 
         LA    R2,DLINEQ(R2)                                                    
         B     ISL10                                                            
         DROP  R2                                                               
*                                                                               
ISL20    LA    R0,MXDEMLNS          CLEAR ALL SELECT FIELDS                     
         LA    R2,IT2SELH                                                       
         USING DEMLINED,R2                                                      
*                                                                               
ISL30    DS    0H                                                               
         MVI   DEMSELH+5,0         ZERO OUT THE LENGTH                          
         XC    DEMSEL,DEMSEL       CLEAR OUT ANY DATA                           
         OI    DEMSELH+6,X80       TRANSMIT FIELD                               
         BCT   R0,ISLBUMP2                                                      
         B     ISLX                                                             
                                                                                
ISLBUMP2 DS    0H                  MAKE SURE ONLY R2 IS CHANGED                 
         LA    R2,DEMLINEQ(R2)                                                  
         B     ISL30                                                            
*                                                                               
ISLX     DS    0H                                                               
         B     XIT_01                                                           
         DROP  R2                                                               
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--TSL#)'                      
*------------------------- TEST SELECT FIELD -------------------------*         
                                                                                
* Validate select code.                                                         
* At exit, FULL = 0 if select fields were ok, else                              
*          FULL = A(field w/ error).                                            
                                                                                
TESTSEL  DS    0H                                                               
         CLI   MTSCRN,X'E9'        TRACK/DEMOS SCREEN?                          
         BE    TESTSEL2                                                         
*                                                                               
         LA    R2,ITKSELH          R2-->FIRST SELECT FIELD.                     
         USING DLINED,R2                                                        
                                                                                
         MVI   ITSHNSEL,0          TELL SUBSEQUENT OVERLAYS # ITEMS SEL         
                                                                                
         CLI   ITITNLD,0           ANYTHING ON LIST SCREEN YET?                 
         BE    TSL050               NOPE                                        
*                                                                               
** CHECK LISTED LINES **                                                        
*                                                                               
         MVC   NTIMES,ITITNLD      CHECK SEL FLD OF LISTED LINES FIRST          
         MVI   COUNTER,0           COUNTS # OF SELECTED ITEMS                   
*                                                                               
TSL010   DS    0H                  START OF LOOP                                
*                                                                               
TSL011   ZICM  R0,DLSELH+5,(1)     ANY INPUT IN SELECT FIELD?                   
         BZ    TSL020               NOPE                                        
                                                                                
         MVI   OURERRCD,ISELQ      ASSUME INVALID SEL CODE ERROR                
         LA    RF,DLSEL                                                         
*                                                                               
TSL012   DS    0H                                                               
         MVC   WORK(1),0(RF)       CHECK SELECT CODES ONE AT A TIME             
         OI    WORK,C' '                                                        
                                                                                
         L     RE,ASELPFTB         MATCH INPUT SEL CODE TO SELPFTAB             
         USING SELPFTAB,RE                                                      
TSL014   CLI   0(RE),EOT                                                        
         BE    TSLXNO              THIS SEL FIELD IS NOT OK                     
         CLC   SELPFSCD,WORK                                                    
         BE    TSL016                                                           
         LA    RE,L'SELPFTAB(RE)   BUMP TO NEXT ENTRY PFTABLE ENTRY             
         B     TSL014                                                           
*                                                                               
TSL016   CLI   ACTNUM,24           IS THIS A DTRACKS ACTION?                    
         BNE   TSL017                                                           
         TM    SELPFLAG,X'40'      YES,VALID FOR DTRACKS ACTION?                
         BZ    TSLXNO                   NO                                      
         B     TSL018                   YES                                     
*                                                                               
TSL017   TM    SELPFLAG,X'80'      NO,VALID FOR TRACKS ACTION?                  
         BZ    TSLXNO                                                           
         DROP  RE                                                               
*                                                                               
TSL018   DS    0H                  BUMP TO NEXT CHARACTER IN SEL FLD            
         LA    RF,1(RF)                                                         
         BCT   R0,TSL012                                                        
*                                                                               
         DS    0H                  THIS SELECT FIELD OK                         
         ZIC   R1,COUNTER                                                       
         ZIC   R0,DLSELH+5                                                      
         AR    R1,R0                                                            
         STC   R1,COUNTER           INCREMENT # OF SELECTIONS MADE              
                                                                                
TSL020   DS    0H                  SEL CODE OKAY, CHECK NEXT ONE                
         B     TSL030                                                           
*                                                                               
TSL030   DS    0H                  SEL CODE OKAY, CHECK NEXT ONE                
         BAS   RE,TSLBUMP          BUMP R2 TO NEXT SEL FIELD                    
         ZIC   R1,NTIMES                                                        
         BCTR  R1,0                                                             
         STC   R1,NTIMES           DECREMENT # OF LINES LEFT TO CHECK           
         LTR   R1,R1               LOOPED THRU ALL LINES W/ DATA YET?           
         BP    TSL010               NO, GO BACK & CHECK THIS LIST LINE          
         BZ    TSL050               YEP, GO CHECK LINES W/O DATA                
         DC    H'0'                                                             
*                                                                               
** CHECK LINES W/O DATA **                                                      
*                                                                               
TSL050   DS    0H                                                               
         MVI   OURERRCD,IFLDQ      ASSUME INVALID INPUT ERROR                   
         LA    RF,ITKSELZH         RF-->LAST LIST LINE ON SCREEN                
TSL050A  CR    R2,RF               IF REACHED, THEN EXIT OKAY-LY                
         BH    TSLXOK                                                           
                                                                                
         CLI   DLSELH+5,0          IF THERE IS INPUT HERE,                      
         BNE   TSLXNO               THERE SHOULDN'T BE                          
                                                                                
TSL050B  DS    0H                                                               
         BAS   RE,TSLBUMP                                                       
         B     TSL050A                                                          
                                                                                
                                                                                
TSLXNO   DS    0H                  AN INVALID SELECT CODE FOUND                 
         B     TSLX                 R2-->FIELD W/ ERROR                         
*                                                                               
TSLXOK   DS    0H                  NO INVALID SELECT CODE FOUND                 
         OI    SELFLAG1,SF1TSLOK    SET FLAG FOR NO INVALID SEL CODE            
         MVC   ITSHNSEL,COUNTER     TELL SUBSEQUENT OVRLAYS # ITEMS SEL         
         SR    R2,R2                CLEAR R2 FOR RETURN CODE                    
*                                                                               
TSLX     DS    0H                                                               
         ST    R2,FULL             RETURN "ERROR CODE" IN FULL                  
         B     XIT_01                                                           
                                                                                
                                                                                
TSLBUMP  DS    0H                  DO NOT CLOBBER RE HERE                       
         LA    R2,DLINEQ(R2)       BUMP TO NEXT LIST LINE                       
         BR    RE                                                               
         DROP  R2                                                               
                                                                                
TESTSEL2 DS    0H                                                               
         LA    R2,IT2SELH          R2-->FIRST SELECT FIELD.                     
         USING DEMLINED,R2                                                      
                                                                                
         MVI   ITSHNSEL,0          TELL SUBSEQUENT OVERLAYS # ITEMS SEL         
                                                                                
         CLI   ITITNLD,0           ANYTHING ON LIST SCREEN YET?                 
         BE    TS2050               NOPE                                        
*                                                                               
** CHECK LISTED LINES **                                                        
*                                                                               
         MVC   NTIMES,ITITNLD      CHECK SEL FLD OF LISTED LINES FIRST          
         MVI   COUNTER,0           COUNTS # OF SELECTED ITEMS                   
*                                                                               
TS2010   DS    0H                  START OF LOOP                                
*                                                                               
                                                                                
TS2011   ZICM  R0,DEMSELH+5,(1)    ANY INPUT IN SELECT FIELD?                   
         BZ    TS2020               NOPE                                        
                                                                                
         MVI   OURERRCD,ISELQ      ASSUME INVALID SEL CODE ERROR                
         LA    RF,DEMSEL                                                        
*                                                                               
TS2012   DS    0H                                                               
         MVC   WORK(1),0(RF)       CHECK SELECT CODES ONE AT A TIME             
         OI    WORK,C' '                                                        
                                                                                
         L     RE,ASELPFTB         MATCH INPUT SEL CODE TO SELPFTAB             
         USING SELPFTAB,RE                                                      
TS2014   CLI   0(RE),EOT                                                        
         BE    TS2XNO              THIS SEL FIELD IS NOT OK                     
         CLC   SELPFSCD,WORK                                                    
         BE    TS2016                                                           
         LA    RE,L'SELPFTAB(RE)   BUMP TO NEXT ENTRY PFTABLE ENTRY             
         B     TS2014                                                           
*                                                                               
TS2016   CLI   ACTNUM,24           IS THIS A DTRACKS ACTION?                    
         BNE   TS2017                                                           
         TM    SELPFLAG,X'40'      YES,VALID FOR DTRACKS ACTION?                
         BZ    TS2XNO                   NO                                      
         B     TS2018                   YES                                     
TS2017   TM    SELPFLAG,X'80'      NO,VALID FOR TRACKS  ACTION?                 
         BZ    TS2XNO                   NO                                      
         DROP  RE                                                               
*                                                                               
                                                                                
TS2018   DS    0H                  BUMP TO NEXT CHARACTER IN SEL FLD            
         LA    RF,1(RF)                                                         
         BCT   R0,TS2012                                                        
*                                                                               
         DS    0H                  THIS SELECT FIELD OK                         
         ZIC   R1,COUNTER                                                       
         ZIC   R0,DEMSELH+5                                                     
         AR    R1,R0                                                            
         STC   R1,COUNTER           INCREMENT # OF SELECTIONS MADE              
                                                                                
TS2020   DS    0H                  SEL CODE OKAY, CHECK NEXT ONE                
         B     TS2030                                                           
*                                                                               
TS2030   DS    0H                  SEL CODE OKAY, CHECK NEXT ONE                
         BAS   RE,TS2BUMP          BUMP R2 TO NEXT SEL FIELD                    
         ZIC   R1,NTIMES                                                        
         BCTR  R1,0                                                             
         STC   R1,NTIMES           DECREMENT # OF LINES LEFT TO CHECK           
         LTR   R1,R1               LOOPED THRU ALL LINES W/ DATA YET?           
         BP    TS2010               NO, GO BACK & CHECK THIS LIST LINE          
         BZ    TS2050               YEP, GO CHECK LINES W/O DATA                
         DC    H'0'                                                             
*                                                                               
** CHECK LINES W/O DATA **                                                      
*                                                                               
TS2050   DS    0H                                                               
         MVI   OURERRCD,IFLDQ      ASSUME INVALID INPUT ERROR                   
         LA    RF,ITKSELZH         RF-->LAST LIST LINE ON SCREEN                
TS2050A  CR    R2,RF               IF REACHED, THEN EXIT OKAY-LY                
         BH    TS2XOK                                                           
                                                                                
         CLI   DEMSELH+5,0         IF THERE IS INPUT HERE,                      
         BNE   TS2XNO               THERE SHOULDN'T BE                          
                                                                                
TS2050B  DS    0H                                                               
         BAS   RE,TS2BUMP                                                       
         B     TS2050A                                                          
*                                                                               
TS2XNO   DS    0H                  AN INVALID SELECT CODE FOUND                 
         B     TS2X                 R2-->FIELD W/ ERROR                         
*                                                                               
TS2XOK   DS    0H                  NO INVALID SELECT CODE FOUND                 
         OI    SELFLAG1,SF1TSLOK    SET FLAG FOR NO INVALID SEL CODE            
         MVC   ITSHNSEL,COUNTER     TELL SUBSEQUENT OVRLAYS # ITEMS SEL         
         SR    R2,R2                CLEAR R2 FOR RETURN CODE                    
*                                                                               
TS2X     DS    0H                                                               
         ST    R2,FULL             RETURN "ERROR CODE" IN FULL                  
         B     XIT_01                                                           
*                                                                               
TS2BUMP  DS    0H                  DO NOT CLOBBER RE HERE                       
         LA    R2,DEMLINEQ(R2)     BUMP TO NEXT LIST LINE                       
         LA    R2,DEMLINEQ(R2)                                                  
         BR    RE                                                               
                                                                                
         DROP  R2                                                               
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--GSI#)'                      
*------------------------ GO TO SELECTED ITEMS -----------------------*         
                                                                                
* Goes to selected items on screen.                                             
* At entry,                                                                     
*   R2-->start of list line,                                                    
*   R3-->SELPFTAB entry,                                                        
*   R4-->list table entry,                                                      
*   COUNTER = nth list line we're on.                                           
                                                                                
GOSELITM DS    0H                                                               
         USING SELPFTAB,R3                                                      
         USING LISTTABD,R4                                                      
*                                                                               
         CLI   MTSCRN,X'E9'        TRACKS/DEMO SCREEN?                          
         BNE   GSIO10X             NO                                           
         USING DEMLINED,R2                                                      
         ZIC   R1,DEMSELH+5                                                     
         BCTR  R1,0                                                             
         STC   R1,DEMSELH+5        DECREMENT L(INPUT) BY ONE                    
         EXMVC R1,WORK,DEMSEL                                                   
         XC    DEMSEL,DEMSEL                                                    
         LTR   R1,R1                                                            
         BZ    GSI015X                                                          
         BCTR  R1,0                                                             
         EXMVC R1,DEMSEL,WORK+1     SHIFT DATA TO LEFT BY ONE                   
         B     GSI015X                                                          
         DROP  R2                                                               
*                                                                               
GSIO10X  DS    0H                  FUDGE SEL FLD TO PREVENT LOOPING             
         USING DLINED,R2                                                        
         ZIC   R1,DLSELH+5                                                      
         BCTR  R1,0                                                             
         STC   R1,DLSELH+5          DECREMENT L(INPUT) BY ONE                   
         EXMVC R1,WORK,DLSEL                                                    
         XC    DLSEL,DLSEL                                                      
         LTR   R1,R1                                                            
         BZ    GSI015X                                                          
         BCTR  R1,0                                                             
         EXMVC R1,DLSEL,WORK+1      SHIFT DATA TO LEFT BY ONE                   
*                                                                               
GSI015X  EQU   *                                                                
*                                                                               
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         MVC   TIOBAID,SELPFPFK    FUDGE PF KEY INTO TIO                        
         MVC   PFKEY,SELPFPFK      FUDGE IT INTO PFKEY AS WELL                  
         DROP  RF                                                               
*                                                                               
         DS    0H                  SET PARMS FOR CPROG                          
         MVC   TMPKSRC,LTKSRC                                                   
         MVC   TMPBOOK,LTXKBK                                                   
         XC    TMPBOOK,=X'FFFF'                                                 
         MVI   GOSUBN,TBK#                                                      
         GOTO1 AGOSUB                                                           
         MVC   CCONKBK,WORK         KEY FIELD: BOOK                             
*                                                                               
         XC    CCONRSVC,CCONRSVC                                                
         MVC   TMPKSRC,LTKSRC                                                   
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB                                                           
         MVC   CCONRSVC(1),TMPSRC                                               
*                                                                               
         MVI   GOSUBN,STI#         SAVE TABLES BEFORE EXITING PHASE             
         GOTO1 AGOSUB                                                           
         MVI   GOAGAIN,C'Y'                                                     
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
*                                                                               
GSIX     DS    0H                                                               
         B     XIT_01                                                           
                                                                                
                                                                                
         DROP  R2,R3,R4                                                         
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--VSI#)'                      
*---------------------- VALIDATE SELECTED ITEMS ----------------------*         
                                                                                
* Allows user to override a footnote for the selected track.                    
* At entry,                                                                     
*   R2-->start of list line,                                                    
*   R3-->SELPFTAB entry,                                                        
*   R4-->list table entry,                                                      
*   COUNTER = nth list line we're on.                                           
                                                                                
VLSELITM DS    0H                                                               
         CLI   MTSCRN,X'E9'        TRACK DEMOS SCREEN?                          
         BNE   VSI001                                                           
*                                                                               
         MVI   OVFLGO,FTN#                                                      
         GOTO1 AOVFL,(RC)                                                       
         L     R2,FULL                                                          
         B     VSIX                                                             
*                                                                               
         USING DLINED,R2                                                        
         USING LISTTABD,R4                                                      
*                                                                               
VSI001   TM    LTFLAG,LTFPRMPT     HAS THE USER BEEN PROMPTED YET?              
         BNZ   VSI050               YES, GO VALIDATE CHANGE                     
*                                                                               
** PROMPT USER **                                                               
*                                                                               
         OI    LTFLAG,LTFPRMPT                                                  
         MVI   OURINFCD,INCHGQ                                                  
         NI    DLFTNTH+1,XFF-X20   UNPROTECT FIELD                              
         OI    DLFTNTH+6,X80                                                    
         LA    R0,DLFTNTH                                                       
         ST    R0,FULL                                                          
         B     VSIX                                                             
                                                                                
*                                                                               
** VALIDATE AND UPDATE FOOTNOTE **                                              
*                                                                               
VSI050   DS    0H                                                               
         NI    LTFLAG,XFF-LTFPRMPT USER WAS PROMPTED ALREADY                    
*                                                                               
         TM    DLFTNTH+4,X80       WAS ANYTHING INPUTTED?                       
         BZ    VSI069               NO, NOTHING TO UPDATE                       
*                                                                               
         DS    0H                   UPDATE LIST TABLE                           
         MVC   LTFTNOTE,DLFTNT                                                  
         OI    LTFLAG,LTFFTNOV                                                  
*                                                                               
         DS    0H                   GET INVENTORY TRACK RECORD                  
         MVC   TMPKSRC,LTKSRC                                                   
         MVC   TMPKBK,LTXKBK                                                    
         XC    TMPKBK,=X'FFFF'                                                  
         CLI   TMPKBK+1,ESTMNTHQ                                                
         BNE   *+8                                                              
         MVI   TMPKBK+1,0                                                       
         MVI   GOSUBN,GIR#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                   UPDATE INVENTORY TRACK RECORD               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING RPGMELM,R3                                                       
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(X'01',AIO),0,0                         
         CLI   DMCB+12,0                                                        
         BE    VSI062                                                           
         CLI   DMCB+12,6                                                        
         BE    VSI063                                                           
         DC    H'0'                                                             
                                                                                
VSI062   DS    0H                    GET ELEM FROM RECD INTO WORK AREA          
         ICM   RF,15,DMCB+12                                                    
         ZIC   R1,1(RF)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,RPGMELM,0(RF)                                                 
         B     VSI065                                                           
                                                                                
VSI063   DS    0H                    BUILD A NEW ELEM IN WORK AREA              
         MVI   RPGMELM,X'01'                                                    
         MVI   RPGMELLN,RPGMELML                                                
         MVI   RPGMLIN,1                                                        
         B     VSI065                                                           
                                                                                
VSI065   DS    0H                    DELETE OLD ELEM FROM RECORD                
         MVI   BYTE,RPGMELML-2                                                  
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(X'01',AIO),(BYTE,RPGMLIN),0            
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DS    0H                   BUILD ELEM W/ OVERRIDING FOOTNOTE           
         MVC   RPGMNAME,SPACES                                                  
         ZIC   R1,DLFTNTH+5                                                     
         BCTR  R1,0                                                             
         EXMVC R1,RPGMNAME,LTFTNOTE                                             
*                                                                               
         DS    0H                   PUT NEW ELEMENT INTO RECORD                 
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,RPGMELM,0                           
         MVI   GOSUBN,UAE#           ALONG W/ ACTIVITY ELEMENT                  
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                  PUT RECORD BACK TO FILE                      
         GOTO1 PUTREC                                                           
                                                                                
         DS    0H                                                               
         MVI   GOSUBN,PLR#         GO PROCESS LTRANS REQUEST                    
         GOTO1 AGOSUB                                                           
VSI069   EQU   *                                                                
*                                                                               
         DS    0H                  FUDGE SEL FLD TO PREVENT LOOPING             
         OI    DLSELH+6,X80         TRANSMIT SEL FIELD                          
         ZIC   R1,DLSELH+5                                                      
         BCTR  R1,0                                                             
         STC   R1,DLSELH+5          DECREMENT L(INPUT) BY ONE                   
         EXMVC R1,WORK,DLSEL                                                    
         XC    DLSEL,DLSEL                                                      
         LTR   R1,R1                                                            
         BZ    VSI085X                                                          
         BCTR  R1,0                                                             
         EXMVC R1,DLSEL,WORK+1      SHIFT DATA TO LEFT BY ONE                   
VSI085X  EQU   *                                                                
*                                                                               
         OI    DLFTNTH+1,X20       PROTECT FOOTNOTE FIELD                       
         OI    DLFTNTH+6,X80                                                    
*                                                                               
VSIX     DS    0H                                                               
         B     XIT_01                                                           
                                                                                
                                                                                
         DROP  R2,R4                                                            
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--UAE#)'                      
*---------------------- UPDATE ACTIVITY ELEMENT ----------------------*         
                                                                                
* Updates the activity element in the inventory track addressed by R6.          
*  If no activity element is found, then one will be created and added          
*  to the record.                                                               
* At entry,                                                                     
*   AIO = A(inventory track).                                                   
                                                                                
UPACTVEL DS    0H                                                               
                                                                                
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(X'EF',AIO),0,0                         
         CLI   12(R1),0                                                         
         BE    UAE014                                                           
         CLI   12(R1),X'06'                                                     
         BE    UAE016                                                           
         DC    H'0'                                                             
*                                                                               
UAE014   DS    0H                  ELEMENT FOUND                                
         ZICM  R3,12+1(R1),(7)                                                  
         USING RINVAEL,R3                                                       
         MVC   RINVALST,BTODAY                                                  
         MVI   RINVAWHY,C'C'                                                    
         DROP  R3                                                               
         B     UAE019                                                           
*                                                                               
UAE016   DS    0H                  ELEMENT NOT FOUND                            
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM              BUILD ACTIVITY ELEMENT                      
         USING RINVAEL,R3                                                       
         MVI   RINVACOD,X'EF'        ELEMENT CODE                               
         MVI   RINVALEN,12              "    LENGTH                             
         MVC   RINVAFST,BTODAY       DATE OF FIRST ACTIVITY                     
         MVC   RINVALST,BTODAY        "   "  LAST     "                         
         MVI   RINVAWHY,C'C'         REASON IS CHANGE                           
                                                                                
         DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,(R3),0                              
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
         B     UAE019                                                           
*                                                                               
UAE019   EQU   *                                                                
                                                                                
*                                                                               
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--RKS#)'                      
*------------------------- REVERSE KEY SOURCE ------------------------*         
                                                                                
* Reverses "key source" to get the book bits and book type.                     
* At entry,                                                                     
*   TMPKSRC   = "key source".                                                   
* At exit,                                                                      
*   TMPSRC(1) = rating service,                                                 
*   TMPFRBTS  = book bits,                                                      
*   TMPBTYP   = book type,                                                      
*   TMPQLFY   = internal key code for qualifier,                                
*   TMPQLFK   = internal key code for qualifier.                                
*   CC set to equal if translation okay,                                        
*   CC set to not equal if translation not okay.                                
                                                                                
REVKSRC  DS    0H                                                               
         XC    TMPSRC,TMPSRC                                                    
         MVI   TMPFRBTS,0                                                       
         MVI   TMPBTYP,0                                                        
         MVI   TMPQLFY,0                                                        
         MVI   TMPQLFK,0                                                        
*                                                                               
         XC    GKSIPARM,GKSIPARM                                                
         XC    GKSOPARM,GKSOPARM                                                
*                                                                               
         DS    0H                  GET RTG SVCE, QLFYR, & BKTYPE                
         LA    R3,GKSIPARM                                                      
         USING GKSPARMD,R3                                                      
         MVC   GKSPKSRC,TMPKSRC                                                 
         DROP  R3                                                               
         GOTO1 VGETKSRC,DMCB,(C'K',GKSIPARM),GKSOPARM                           
         CLI   DMCB+4,0                                                         
         BNE   RKSXN                                                            
                                                                                
         LA    R3,GKSOPARM                                                      
         USING GKSPARMD,R3                                                      
         MVC   TMPSRC(1),GKSPRSVC   RETURN: RATING SERVICE                      
         MVC   TMPQLFY,GKSPQLFY     RETURN: TRACK QUALIFIER                     
         MVC   TMPBTYP,GKSPBTYP     RETURN: BOOK TYPE                           
         DROP  R3                                                               
*                                                                               
RKS020   DS    0H                  GET QUALIFIER KEY CODE                       
         L     RF,AQLFKYTB                                                      
         ZIC   R0,QLFKYTBQ                                                      
RKS022   CLC   0(1,RF),GKSOPARM+(GKSPQLFY-GKSPARMD)                             
         BE    RKS025                                                           
         LA    RF,L'QLFKYTAB(RF)                                                
         BCT   R0,RKS022                                                        
         B     RKSXN                                                            
                                                                                
RKS025   DS    0H                                                               
         MVC   TMPQLFK,1(RF)        RETURN: TRACK QUALIFIER KEY CODE            
                                                                                
*                                                                               
         DS    0H                  GET BOOKVAL BITS                             
         XC    MYFLD(MYFLDL),MYFLD                                              
         LA    R3,GKSOPARM                                                      
         USING GKSPARMD,R3                                                      
         LA    RE,MYFLDD                                                        
         LR    RF,RE                                                            
         CLI   GKSPQLFY,C' '                                                    
         BE    *+14                                                             
         MVC   0(1,RF),GKSPQLFY                                                 
         LA    RF,1(RF)                                                         
         MVC   0(5,RF),=C'NOV96'                                                
         LA    RF,5(RF)                                                         
         SR    RF,RE                                                            
         STC   RF,MYFLDH+5                                                      
         LA    RF,8(RF)                                                         
         STC   RF,MYFLDH+0                                                      
         GOTO1 BOOKVAL,DMCB,(GKSPRSVC,MYFLD),(1,DUB),SCANNER,0                  
         CLI   DMCB+4,1                                                         
         BNE   RKSXN                                                            
                                                                                
*                                                                               
         DS    0H                  SET RETURN VALUES                            
         MVC   TMPFRBTS,DUB         RETURN: BOOKVAL BITS                        
         DROP  R3                                                               
*                                                                               
         B     RKSXY                                                            
                                                                                
                                                                                
RKSXN    DS    0H                                                               
         B     NO_01                                                            
                                                                                
RKSXY    DS    0H                                                               
         B     YES_01                                                           
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--GIDM#)'                     
*----------------------- GO TO INV/DEMOS SCREEN ----------------------*         
                                                                                
* Goes to the INV/DEMOS screen.  Bumps through subsequent select                
*  fields to see if any other tracks were selected (in the 1st byte             
*  of the select field) to go to the same screen.  If there is, then            
*  they will be processed as well.                                              
* At entry,                                                                     
*   R2-->start of list line,                                                    
*   R3-->SELPFTAB entry,                                                        
*   R4-->list table entry,                                                      
*   COUNTER = nth list line we're on.                                           
                                                                                
GOINVDEM DS    0H                                                               
         CLI   MTSCRN,X'E9'        TRACK/DEMOS SCREEN?                          
         BNE   GIDM010                                                          
         MVI   OVFLGO,GID2#        GO TO INV/DEMOS                              
         GOTO1 AOVFL,(RC)                                                       
         B     GIDMX                                                            
*                                                                               
GIDM010  DS    0H                                                               
         USING DLINED,R2                                                        
         USING SELPFTAB,R3                                                      
         USING LISTTABD,R4                                                      
                                                                                
*                                                                               
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         MVC   TIOBAID,SELPFPFK    FUDGE PF KEY INTO TIO                        
         MVC   PFKEY,SELPFPFK      FUDGE IT INTO PFKEY AS WELL                  
         DROP  RF                                                               
                                                                                
*                                                                               
** SET PARAMETERS FOR CPROG **                                                  
*                                                                               
         DS    0H                                                               
         XC    CCONTRKS,CCONTRKS                                                
         LA    R5,CCONTRKS                                                      
         MVI   PRVSRC,0            INITIALIZE PREVIOUS RATING SERVICE           
         MVI   CNTIDMTK,0          COUNTS # OF TRCKS GOING TO INV/DEMOS         
                                                                                
*                                                                               
GIDM032  DS    0H                  FORMAT RATING SERVICE                        
         MVC   TMPKSRC,LTKSRC                                                   
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
         CLC   PRVSRC,TMPSRC        IF PREV RSVC = CURR RSVC                    
         BE    GIDM032X              DON'T FORMAT IT INTO FIELD                 
         MVC   PRVSRC,TMPSRC                                                    
         MVC   0(1,R5),TMPSRC                                                   
         LA    R5,1(R5)                                                         
         BAS   RE,GIDMCMMA                                                      
GIDM032X EQU   *                                                                
*                                                                               
         DS    0H                  FORMAT TRACK                                 
         MVC   TMPKSRC,LTKSRC                                                   
         MVC   TMPBOOK,LTXKBK                                                   
         XC    TMPBOOK,=X'FFFF'                                                 
         MVI   GOSUBN,TBK#                                                      
         GOTO1 AGOSUB                                                           
         ZICM  R1,HALF,(3)                                                      
         BZ    GIDM034X                                                         
         BCTR  R1,0                                                             
         EXMVC R1,0(R5),WORK                                                    
         LA    R5,1(R1,R5)                                                      
GIDM034X EQU   *                                                                
*                                                                               
* I would put these instructions at the beginning of this routine as            
*  they are critical to prevent the same entry being selected.                  
*  However, since they need to be included in the loop, I am forced to          
*  put them here.                                                               
         DS    0H                  FUDGE SEL FLD TO PREVENT LOOPING             
         ZIC   R1,DLSELH+5                                                      
         BCTR  R1,0                                                             
         STC   R1,DLSELH+5          DECREMENT L(INPUT) BY ONE                   
         EXMVC R1,WORK,DLSEL                                                    
         XC    DLSEL,DLSEL                                                      
         LTR   R1,R1                                                            
         BZ    GIDM038X                                                         
         BCTR  R1,0                                                             
         EXMVC R1,DLSEL,WORK+1      SHIFT DATA TO LEFT BY ONE                   
GIDM038X EQU   *                                                                
                                                                                
         DS    0H                                                               
         ZIC   R1,CNTIDMTK         UPDATE THE NUMBER OF TRACKS                  
         LA    R1,1(R1)                                                         
         STC   R1,CNTIDMTK          GOING TO INV/DEMOS                          
         CLI   CNTIDMTK,MXTKLNTY   REACHED MAX YET?                             
         BNL   GIDM100              YES, GO TO INV/DEMOS NOW                    
*                                                                               
         DS    0H                                                               
         LA    RF,ITKSELZH         RF-->LAST LIST LINE                          
GIDM042  DS    0H                                                               
         LA    R2,DLINEQ(R2)       BUMP TO NEXT LIST LINE                       
         CR    R2,RF               DID WE PAST LAST LIST LINE?                  
         BH    GIDM049              YES, NO MORE TRACKS TO GET                  
         LA    R4,LISTTABQ(R4)                                                  
         CLI   DLSEL+0,C'M'        GO TO INV/DEMOS FOR THIS TRACK?              
         BNE   GIDM042              NOPE, BUMP TO NEXT LIST LINE                
         BAS   RE,GIDMCMMA                                                      
         ZIC   R1,ITSHNSEL         DECREMENT # OF SELECTIONS MADE               
         BCTR  R1,0                 (LUMP ENTRIES GOING TO INV/DEMOS            
         STC   R1,ITSHNSEL           TOGETHER AS ONE SELECTION)                 
         B     GIDM032                                                          
GIDM049  EQU   *                                                                
*                                                                               
GIDM100  DS    0H                                                               
         MVI   GOSUBN,STI#         SAVE TABLES BEFORE EXITING PHASE             
         GOTO1 AGOSUB                                                           
         MVI   GOAGAIN,C'Y'                                                     
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         B     GIDMX                                                            
*                                                                               
GIDMX    DS    0H                                                               
         B     XIT_01                                                           
         DROP  R2,R3,R4                                                         
                                                                                
                                                                                
* Little helper routine to insert a comma into space pointed to by R5           
                                                                                
GIDMCMMA DS    0H                                                               
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         BR    RE                                                               
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--PLR#)'                      
*----------------------- PROCESS LTRANS REQUEST ----------------------*         
*                                                                               
PRCLTRNS DS    0H                                                               
         MVC   MY_CSTAT,CSTAT      REMEMBER STATION  CSTAT                      
         CLC   CSTAT,IKSTTN        MAKE SURE CORRECT STATION IN  CSTAT          
         BNE   *+10                                                             
         MVC   CSTAT,IKSTTN                                                     
         GOTO1 VLTRANS                                                          
         MVC   CSTAT,MY_CSTAT      RESTORE STATION IN  CSTAT                    
                                                                                
*                                                                               
         DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--LTORG && CONSTANTS)+        
               '                                                                
*--------------------- RESTORE SELECTED ITEMS ------------------------*         
* Restores selected items on screen.                                            
* At entry,                                                                     
*   R2-->start of list line,                                                    
*   R3-->SELPFTAB entry,                                                        
*   R4-->list table entry,                                                      
*   COUNTER = nth list line we're on.                                           
*                                                                               
RESELITM DS    0H                                                               
                                                                                
         USING SELPFTAB,R3                                                      
         USING LISTTABD,R4                                                      
         USING DEMLINED,R2                                                      
*                                                                               
         MVC   SVDMINBT,DMINBTS    SAVE DATAMANAGER INPUT BITS                  
         OI    DMINBTS,X'08'       RETURN DELETED TRACKS                        
         MVC   TMPBOOK,LTXKBK                                                   
         XC    TMPBOOK,=X'FFFF'                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),KEYSAVE                                                  
         MVC   KEY+24(1),LTKSRC                                                 
         MVC   KEY+25(2),TMPBOOK                                                
         CLI   TMPBOOK+1,ESTMNTHQ  YEARLY ESTIMATE?                             
         BNE   *+8                                                              
         MVI   KEY+26,0                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RINVREC,R6                                                       
         NI    RINVCNTL,X'7F'    TURNDELETE BIT OFF                             
         GOTO1 PUTREC                            IN RECORD, & SAVE              
         NI    KEY+(RINVLEN-RINVKEY),X'7F'  TURN DELETE BIT OFF                 
*                                                IN DIRECTORY                   
         GOTO1 WRITE                        UPDATE DIRECTORY PNTR               
*                                                                               
         OI    LTFLAG,LTTRKRES      MARK AS RESTORED IN LISTTAB                 
*                                                                               
         MVC   DMINBTS,SVDMINBT     RESTORE DATAMANAGER INPUT BITS              
*                                                                               
         DROP  R2,R3,R4,R6                                                      
RSIX     DS    0H                                                               
         B     XIT_01                                                           
*---------------------  SKIP SELECTED ITEMS ------------------------*           
*                                                                               
* Skips item on screen (no-op for special marker '*').                          
*   R2-->start of list line,                                                    
*   R3-->SELPFTAB entry,                                                        
*   R4-->list table entry,                                                      
*   COUNTER = nth list line we're on.                                           
*                                                                               
*                                                                               
SKIPITM  DS    0H                                                               
         B     XIT_01         EXIT                                              
                                                                                
*------------------------- LTORG & CONSTANTS -------------------------*         
         TITLE 'RERMP17 - INVENTORY TRACKS (SUBR01--MISC STUFF)'                
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
*                                                                               
         DS    0H                                                               
         GETEL R3,DATADISP,ELCODE                                               
         LTORG                                                                  
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'2000'-SUBR01L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBRXM)'                  
***********************************************************************         
*==================== EXIT WITH MESSAGE ROUTINES =====================*         
                                                                                
* Exits which leave RMP17 entirely and displays a message go through            
*  this routine.                                                                
                                                                                
SUBRXMQ  EQU   (((*-RMP17+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP17+SUBRXMQ                                                    
XMSGRTN  NMOD1 0,**17XM**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
                                                                                
         XC    CONHEAD,CONHEAD     CLEAR THE WAY FOR THE MESSAGE                
                                                                                
         CLI   0(R1),C'E'          EXIT W/ AN ERROR MSG                         
         BE    XMERR                                                            
         CLI   0(R1),C'W'          EXIT W/ A WARNING MSG                        
         BE    XMWRN                                                            
         CLI   0(R1),C'I'          EXIT W/ AN INFO  MSG                         
         BE    XMINF                                                            
         DC    H'0'                                                             
                                                                                
                                                                                
ALLMSGX  DS    0H                                                               
         MVI   GOSUBN,STI#         SAVE OFF STUFF IN TIA                        
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                                                               
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
                                                                                
                                                                                
XIT_XM   XIT1                                                                   
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBRXM--ERR MSGS)+        
               '                                                                
*--------------------------- ERROR MESSAGES --------------------------*         
                                                                                
* At entry, R2-->field in error.                                                
                                                                                
XMERR    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         OI    MISCFLG1,MF1ERRQ                                                 
         OI    6(R2),X'81'         FIELD IN ERROR IS MODIFIED FOR NEXT          
                                                                                
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
IPFKQ    EQU   ((XMERR05-XMERR00)/4)+1                                          
                                                                                
*                                                                               
XMERR00  DS    0H                                                               
XMERR01  B     MFLD                MISSING INPUT FIELD                          
XMERR02  B     IFLD                INVALID INPUT FIELD                          
XMERR03  B     RNF                 RECORD NOT FIELD                             
XMERR04  B     ISEL                INVALID SELECT CODE                          
XMERR05  B     IPFK                INVALID PFKEY                                
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
IPFK     DS    0H                  INVALID PFKEY                                
         MVC   MSGNUM2,=AL2(RR#IPFKY)                                           
         MVC   ACURFORC,AFRSTKEY                                                
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
*&&DO                                                                           
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
*&&                                                                             
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBRXM--WRN MSGS)+        
               '                                                                
*-------------------------- WARNING MESSAGES -------------------------*         
                                                                                
* At entry, R2-->PWMMEDH.                                                       
                                                                                
XMWRN    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         MVC   AERREX,ERREX                                                     
                                                                                
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
         DROP  R1                                                               
         B     WRNEXIT                                                          
                                                                                
                                                                                
WRNEXIT  DS    0H                                                               
         B     ALLMSGX                                                          
*&&DO                                                                           
         GOTO1 ERREX                                                            
         B     XIT_XM                                                           
*&&                                                                             
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBRXM--INF MSGS)+        
               '                                                                
*---------------------------- INFO MESSAGES --------------------------*         
                                                                                
* At entry, R2-->appropriate field to put cursor on.                            
                                                                                
XMINF    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         MVC   AERREX,ERREX                                                     
         XC    ACURFORC,ACURFORC                                                
                                                                                
         CLI   OURINFCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURINFCD,XMINFQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,OURINFCD         BRANCH OFF TO SET WARNING MESSAGE            
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMINF00(RF)                                                      
                                                                                
NDPLYQ   EQU   ((XMINF01-XMINF00)/4)+1                                          
INCHGQ   EQU   ((XMINF02-XMINF00)/4)+1                                          
                                                                                
XMINF00  DS    0H                                                               
XMINF01  B     NDPLY                                                            
XMINF02  B     INCHG                    ENTER CHANGE DETAILS                    
INFX#    EQU   ((*-XMINF00)/4)+1                                                
                                                                                
XMINFQ   EQU   ((*-XMINF00)/4)+1                                                
         EJECT                                                                  
NDPLY    DS    0H                       NO DATA TO DISPLAY                      
         MVC   MSGNUM2,=AL2(RI#NDPLY)                                           
         MVC   ACURFORC,AFRSTKEY                                                
         B     INFGTXT                                                          
*                                                                               
INCHG    DS    0H                       ENTER CHANGES                           
         MVC   MSGNUM2,=AL2(RI#CHDTL)                                           
         MVC   ACURFORC,FULL                                                    
         B     INFGTXT                                                          
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
         DROP  R1                                                               
         B     INFEXIT                                                          
                                                                                
                                                                                
INFEXIT  DS    0H                                                               
         OC    ACURFORC,ACURFORC   NEED TO SET CURSOR?                          
         BNZ   INFEXITX             NOPE                                        
         MVC   ACURFORC,AFRSTKEY   PLACE CURSOR ON 1ST KEY FIELD,               
                                                                                
INFEXITX DS    0H                                                               
         B     ALLMSGX                                                          
*&&DO                                                                           
         GOTO1 ERREX                                                            
         B     XIT_XM                                                           
*&&                                                                             
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBRXM--LTORG && +        
               CONSTANTS)'                                                      
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SUBRXM--MISC STUF+        
               F)'                                                              
*--------------------- SUBRXM MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBRXML  EQU   *-XMSGRTN                                                        
         DS    0CL(X'1000'-SUBRXML+1)                                           
***********************************************************************         
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE'                           
***********************************************************************         
*========================== RMP17's EQUATES ==========================*         
EOT      EQU   X'00'               END OF TABLE MARKER                          
XFF      EQU   X'FF'                                                            
X80      EQU   X'80'                                                            
X40      EQU   X'40'                                                            
X20      EQU   X'20'                                                            
X10      EQU   X'10'                                                            
X08      EQU   X'08'                                                            
X04      EQU   X'04'                                                            
X02      EQU   X'02'                                                            
X01      EQU   X'01'                                                            
                                                                                
DNHOMES  EQU   1                   DEMO CATEGORY # FOR HOMES                    
MXLTNTRY EQU   9*MXLSTLNS          MAX ENTRIES FOR LIST TABLE (10 PP.)          
PAGEQ    EQU   2                   TWA PAGE # FOR TEMPSTR                       
IKYINVL  EQU   RINVKINV-RINVREC+L'RINVKINV                                      
IKYSTDL  EQU   RINVKSTD-RINVREC+L'RINVKSTD                                      
TIASVLEN EQU   BIGENDSV-BIGAREA                                                 
ESTMNTHQ EQU   13                  TREAT YEARLY ESTIMATES AS MONTH 13           
                                                                                
SZLSTSPC EQU   ITKPFLNH-ITKSELH    TOTAL SIZE OF LIST AREA                      
SZLSTLIN EQU   ITKSEL2H-ITKSELH    LENGTH OF ONE LIST LINE                      
MXLSTLNS EQU   SZLSTSPC/SZLSTLIN   NUMBER OF LIST LINES                         
                                                                                
SZDEMSPC EQU   IT2PFLNH-IT2SELH    TOTAL SIZE OF LIST AREA                      
SZDEMLIN EQU   IT2SEL2H-IT2SELH    LENGTH OF ONE LIST LINE                      
MXDEMLNS EQU   SZDEMSPC/SZDEMLIN   NUMBER OF LIST LINES                         
                                                                                
SZDEMROW EQU   IDMRW2HH-IDMROWHH   SIZE OF ONE INV/DEMOS SCREEN ROW             
WDTHCOLS EQU   SZDEMROW-(L'IDMROWHH+L'IDMROWH)                                  
WDTHCOL  EQU   (L'IDMROWDH+L'IDMROWD+L'IDMROWOH+L'IDMROWO)                      
MXTKLNTY EQU   WDTHCOLS/WDTHCOL    MAX TRACKLIST ENTRIES                        
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0XL(2+2+2)                                                       
         DC    AL2(GOSUB-RMP17,0,AGOSUB-SYSD)                                   
         DC    AL2(OVFL-RMP17,0,AOVFL-SYSD)                                     
         DC    AL2(SUBR01-RMP17,0,ASUBR01-SYSD)                                 
         DC    AL2(XMSGRTN-RMP17,0,AXMSGRTN-SYSD)                               
         DC    AL2(SRCTAB-RMP17,0,ASRCTAB-SYSD)                                 
         DC    AL2(DAYTAB-RMP17,0,ADAYTAB-SYSD)                                 
         DC    AL2(QLFKYTAB-RMP17,0,AQLFKYTB-SYSD)                              
         DC    AL2(SELPFTAB-RMP17,0,ASELPFTB-SYSD)                              
         DC    AL2(LISTTAB-BIGAREA,ATIA-GEND,ALISTTAB-SYSD)                     
         DC    AL2(IVHDRECD-BIGAREA,ATIA-GEND,AINVHDR-SYSD)                     
         DC    AL2(DBLOCK1-BIGAREA,ATIA-GEND,ADBLOCK-SYSD)                      
         DC    AL2(DBXTND1-BIGAREA,ATIA-GEND,ADBXTND-SYSD)                      
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
LBLTAB   DS    0XL(2+8)            TABLE OF LABELS FOR BIG STORAGE              
         DC    AL2(LSTBLABL-BIGAREA),CL8'*LSTTAB*'                              
         DC    AL2(IVHDLABL-BIGAREA),CL8'*INVHDR*'                              
         DC    AL2(DBLKLABL-BIGAREA),CL8'*DBLOCK*'                              
         DC    AL2(DBXTLABL-BIGAREA),CL8'*DBXTND*'                              
LBLTABQ  EQU   (*-LBLTAB)/(L'LBLTAB)                                            
                                                                                
                                                                                
TABCLR   DS    0XL(1+2+2)          STORAGES TO CLEAR                            
         DC    C'I',AL2(LISTTAB-BIGAREA),AL2(LISTTABX-LISTTAB)                  
         DC    C'I',AL2(IVHDRECD-BIGAREA),AL2(IVHDRECX-IVHDRECD)                
         DC    C'I',AL2(DBLOCK1-BIGAREA),AL2(DBLOCK1X-DBLOCK1)                  
         DC    C'I',AL2(DBXTND1-BIGAREA),AL2(DBXTND1X-DBXTND1)                  
         DC    C'S',AL2(LISTPARM-SYSD),AL2(LISTPRMX-LISTPARM)                   
TABCLRQ  EQU   (*-TABCLR)/(L'TABCLR)                                            
                                                                                
                                                                                
DAYTAB   DS    0X                  DAY TABLE (SEE DAYTABD)                      
         DC    X'0',X'40',X'10',CL3'MON'                                        
         DC    X'0',X'20',X'20',CL3'TUE'                                        
         DC    X'0',X'10',X'30',CL3'WED'                                        
         DC    X'0',X'08',X'40',CL3'THU'                                        
         DC    X'0',X'04',X'50',CL3'FRI'                                        
         DC    X'0',X'02',X'60',CL3'SAT'                                        
         DC    X'0',X'01',X'70',CL3'SUN'                                        
         DC    C'T',X'7C',X'95',CL3'M-F'                                        
         DC    C'P',X'7C',X'00',CL3'M-F'                                        
         DC    C'P',X'7F',X'80',CL3'M-S'                                        
         DC    X'FF',X'FF',X'FF',CL3'???'                                       
DAYTABQ  EQU   (*-DAYTAB)/(DYTLEN)                                              
                                                                                
                                                                                
QLFKYTAB DS    0XL(1+1)            QUALIFIER KEY CODE TABLE                     
         DC    CL1' ',AL1(BTKACT)   ACTUAL BOOK                                 
         DC    CL1'P',AL1(BTKPRJ)   PROJECTED                                   
         DC    CL1'T',AL1(BTKTP)    TIME PERIOD                                 
         DC    CL1'S',AL1(BTKSS)    SPCL SURVEY                                 
         DC    CL1'E',AL1(BTKEST)   ESTIMATED                                   
QLFKYTBQ EQU   (*-QLFKYTAB)/(L'QLFKYTAB)                                        
                                                                                
                                                                                
SELPFTAB DS    0XL(L'SELPFSCD+L'SELPFVRT+L'SELPFPFK+L'SELPFLAG)                 
SELPFSCD DS     CL1                                                             
SELPFVRT DS     AL1                                                             
SELPFPFK DS     AL1                                                             
SELPFLAG DS     XL1   FLAGS FOR SELECT (X'80'=TRACKS,X'40'=DTRACKS)             
         ORG   SELPFTAB                                                         
         DC    CL1'R',AL1(GSI#),AL1(02),XL1'C0'     INV/ROVER                   
         DC    CL1'O',AL1(GSI#),AL1(03),XL1'80'     INV/DOVER                   
         DC    CL1'F',AL1(VSI#),AL1(00),XL1'80'     CHANGE FOOTNOTE             
         DC    CL1'D',AL1(GSI#),AL1(04),XL1'80'     INV/TRKDEL                  
         DC    CL1'M',AL1(GIDM#),AL1(09),XL1'80'    INV/DEMOS                   
         DC    CL1'T',AL1(GSI#),AL1(10),XL1'80'     TEXT/ADD <<<<<              
         DC    CL1'U',AL1(RSI#),AL1(00),XL1'40'     INV/TRACK RESTORE           
         DC    CL1'*',AL1(SKP#),AL1(00),XL1'40'     MARKER CHAR (SKIP)          
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
DCLIST   DS    0C                  DATA DICTIONARY ESCAPE SEQUENCES             
         DCDDL RE#10PF,L'RE@10PF,L                                              
         DCDDL RE#PF12R,L'RE@PF12R,L                                            
DCLISTX  EQU   *                                                                
         EJECT                                                                  
*---------------------------- SOURCE TABLE ---------------------------*         
                                                                                
* This table was imported from RERMP0B.  The difference is that                 
*  RERMP0B has a branch address in bytes 2-4, while this overlay has            
*  a 3-byte code of the rating service.  Another difference is that             
*  the end of this table is marked w/ EOT.                                      
* A new source, Mediafax, is also added to this table.  It is equated           
*  to Arbitron because that is what BOOKVAL does.                               
*    Byte 1      RINVKSRC value                                                 
*    Byte 2-4    3-byte rating service                                          
*    Byte 5      BOOKVAL book-type bits                                         
*    Byte 6      book-type character for printing                               
*    Byte 7      book-type key for sorting                                      
                                                                                
SRCTAB   DS    0XL7                                                             
         DC    CL1'A',CL3'MFX',XL1'00',CL1' ',AL1(BTKACT) MFX                   
         DC    CL1'B',CL3'MFX',XL1'04',CL1'P',AL1(BTKPRJ)  PROJECTED            
         DC    CL1'C',CL3'MFX',XL1'08',CL1'T',AL1(BTKTP)   TIME PERIOD          
         DC    CL1'D',CL3'MFX',XL1'02',CL1'S',AL1(BTKSS)   SPCL SURVEY          
         DC    CL1'E',CL3'MFX',XL1'20',CL1'E',AL1(BTKEST)  ESTIMATED            
                                                                                
         DC    CL1'A',CL3'ARB',XL1'00',CL1' ',AL1(BTKACT) ARB                   
         DC    CL1'B',CL3'ARB',XL1'04',CL1'P',AL1(BTKPRJ)  PROJECTED            
         DC    CL1'C',CL3'ARB',XL1'08',CL1'T',AL1(BTKTP)   TIME PERIOD          
         DC    CL1'D',CL3'ARB',XL1'02',CL1'S',AL1(BTKSS)   SPCL SURVEY          
         DC    CL1'E',CL3'ARB',XL1'20',CL1'E',AL1(BTKEST)  ESTIMATED            
                                                                                
         DC    CL1'N',CL3'NSI',XL1'40',CL1' ',AL1(BTKACT) NSI                   
         DC    CL1'O',CL3'NSI',XL1'44',CL1'P',AL1(BTKPRJ)  PROJECTED            
         DC    CL1'P',CL3'NSI',XL1'48',CL1'T',AL1(BTKTP)   TIME PERIOD          
         DC    CL1'Q',CL3'NSI',XL1'42',CL1'S',AL1(BTKSS)   SPCL SURVEY          
         DC    CL1'R',CL3'NSI',XL1'60',CL1'E',AL1(BTKEST)  ESTIMATED            
                                                                                
         DC    CL1'T',CL3'SRC',XL1'41',CL1' ',AL1(BTKACT) SRC                   
         DC    CL1'U',CL3'SRC',XL1'45',CL1'P',AL1(BTKPRJ)  PROJECTED            
         DC    CL1'X',CL3'SRC',XL1'61',CL1'E',AL1(BTKEST)  ESTIMATED            
                                                                                
         DC    XL1'FF',CL3'   ',XL1'00',CL1' '                                  
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
BTKEST   EQU   1                   ESTIMATE BOOK                                
BTKPRJ   EQU   2                   PROJECTED BOOK                               
BTKTP    EQU   3                   TIME PERIOD BOOK                             
BTKSS    EQU   4                   SPECIAL SURVEY BOOK                          
BTKACT   EQU   5                   ACTUAL BOOK                                  
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (RERMPWORKD)'              
***********************************************************************         
*============================= RERMPWORKD ============================*         
       ++INCLUDE RERMPWORKD                                                     
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (SYSD)'                    
***********************************************************************         
*========================== SYSD'S SYSSPARE ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*---------------------------- COMMON AREA ----------------------------*         
                                                                                
       ++INCLUDE RERMPITSYP                                                     
                                                                                
                                                                                
*----------------------- OWNED BY RERMP17 ONLY -----------------------*         
*                                 ************** WORK AREA ************         
RELO     DS    F                                                                
MYDMCB   DS    6F                                                               
MYDUB    DS    D                                                                
MYWORK   DS    XL(L'WORK)                                                       
                                                                                
*                                 ************** ADDRESSES ************         
MYBASE1  DS    A                   A(1ST 4096 BYTES OF THIS PROGRAM)            
MYBASE2  DS    A                   A(2ND 4096 BYTES OF THIS PROGRAM)            
ASUBRTN  DS    A                   A(SUBROUTINE POOL TO USE)                    
AERREX   DS    A                   A(ERREX ROUTINE) TO USE                      
A1SELFLD DS    A                   A(1ST SELECT FIELD)                          
ASRCNTRY DS    A                   A(CORRESPONDING SRCTAB ENTRY)                
AITITNLE DS    A                   A(NEXT AVAILABLE SLOT IN LIST TABLE)         
PRVFDSP  DS    H                   FIELD DISP FROM PREV TRANSACTION             
DSP1EL   DS    H                   DISPL TO 1ST ELEMENT (IN REPFILE)            
                                                                                
         DS    0A                  ADDRESSES OF TABLES/ROUTINES                 
AGOSUB   DS    A                    A(SUBROUTINE POOL INTERFACE)                
AOVFL    DS    A                    A(SUBROUTINE POOL INTERFACE)                
ASUBR01  DS    A                    A(SUBROUTINE POOL #1)                       
AXMSGRTN DS    A                    A(EXIT W/ MSG ROUTINE)                      
ASRCTAB  DS    A                    A(SRCTAB)                                   
ADAYTAB  DS    A                    A(DAYTAB)                                   
AQLFKYTB DS    A                    A(QLFKYTAB)                                 
ASELPFTB DS    A                    A(SELPFTAB)                                 
ALISTTAB DS    A                    A(LIST TABLE)                               
AINVHDR  DS    A                    A(INVENTORY HEADER RECORD)                  
ADBLOCK  DS    A                    A(DBLOCK)                                   
ADBXTND  DS    A                    A(DBEXTEND)                                 
VBINSRCH DS    A                    V(BINSRCH)                                  
                                                                                
*                                 ********* BINSRCH PARAMETERS ********         
BSPARMS  DS    0F                                                               
BSP1     DS     F                  A(RECORD)                                    
BSP2     DS     F                  A(TABLE)                                     
BSP3     DS     F                  NUMBER OF RECORDS IN TABLE SO FAR            
BSP4     DS     F                  L(RECORD)                                    
BSP5     DS     F                  DSPL AND LENGTH OF KEY                       
BSP6     DS     F                  MAX NUMBER OF RECORDS IN TABLE               
                                                                                
*                                 ************* INPUT DATA ************         
INPUTKEY DS    0C                  INPUT TO KEY FIELDS                          
IKSTTN   DS    CL5                  STATION                                     
IKINV    DS    CL4                  INVENTORY NUMBER                            
IKEFFDB  DS    XL3                  EFFECTIVE DATE (BINARY)                     
IKEFFDC  DS    XL2                  EFFECTIVE DATE (COMPRESSED)                 
                                                                                
LISTPARM DS    0X                  PARAMETERS TO PRODUCE LIST                   
LPSRC    DS    CL1                  SOURCE                                      
LPFILE   DS    CL3                  DEMO FILE                                   
LPSTTN   DS    CL5                  STATION                                     
LPFRBK   DS    0XL3                 FROM BOOK                                   
LPFRBTS  DS    XL1                   BITS                                       
LPBOOK   DS    XL2                   BOOK                                       
LPBTYP   DS    CL1                  BOOKTYPE                                    
LPIDAY   DS    XL1                  INTERNAL DAY                                
LPTIME   DS    0XL4                 MILITARY START & END TIMES                  
LPSTIM   DS    XL2                   START TIME                                 
LPETIM   DS    XL2                   END   TIME                                 
LPDEMC   DS    XL1                  DEMO CATEGORY NUMBER                        
LPDEMLST DS    XL(3+1)              DEMO LIST + TERMNATR (1 DEMO ONLY)          
LISTPRMX EQU   *                                                                
                                                                                
*                                 ************ TEMP STORAGE ***********         
TMPKSRC  DS    CL1                 TEMP STORAGE FOR KEY FIELD--SOURCE           
TMPKBK   DS    XL2                  "      "     "  KEY FIELD--BOOK             
TMPSRC   DS    CL3                  "      "     "  SOURCE                      
TMPFRBK  DS    0XL3                 "      "     "  FROM BOOK                   
TMPFRBTS DS    XL1                  "      "     "   BITS                       
TMPBOOK  DS    XL2                  "      "     "   BOOK                       
TMPDFIL  DS    CL3                  "      "     "  FILE                        
TMPSTTN  DS    CL5                  "      "     "  STATION                     
TMPBTYP  DS    CL1                  "      "     "  BOOK TYPE                   
TMPIDAY  DS    XL1                  "      "     "  INTERNAL DAY                
TMPKDAY  DS    XL1                  "      "     "  KEY DAY                     
TMPSETM  DS    0XL4                 "      "     "  TIMES                       
TMPSTIM  DS    XL2                  "      "     "   START TIME                 
TMPETIM  DS    XL2                  "      "     "   END TIME                   
TMPNQHR  DS    XL1                  "      "     "  # OF QUARTER HOURS          
TMPPGNAM DS    CL13                 "      "     "  PROGRAM NAME                
TMPMTIM  DS    XL2                  "      "     "  MILITARY TIME               
TMPMED   DS    CL1                  "      "     "  MEDIA                       
TMPQHR   DS    XL1                  "      "     "  QUARTER HOUR                
TMPBDATE DS    XL3                  "      "     "  BINARY DATE                 
TMPQLFY  DS    CL1                  "      "     "  QUALIFIER                   
TMPQLFK  DS    XL1                  "      "     "  QUALIFIER KEY CODE          
                                                                                
GOSUBN   DS    XL1                 SUBROUTINE NUMBER                            
OVFLGO   DS    XL1                 SUBROUTINE NUMBER                            
MYRDUPDT DS    XL1                 MY VERSION OF RDUPDATE                       
MSGNUM2  DS    XL2                                                              
MSGSYS   DS    XL1                                                              
OURERRCD DS    XL1                 MY ERROR   CODE                              
OURWRNCD DS    XL1                 MY WARNING CODE                              
OURINFCD DS    XL1                 MY INFO    CODE                              
COUNTER  DS    XL1                                                              
CNTIDMTK DS    XL1                 COUNT # OF TRACKS TO INV/DEMOS SCRN          
NTIMES   DS    XL1                 NUMBER OF TIMES TO LOOP                      
PRVSRC   DS    CL1                 PREVIOUS RATING SERVICE                      
                                                                                
*                                 *************** FLAGS ***************         
FIRSTIME DS    CL1                 1st TIME OVLY CALLED SNCE SCR LOADED         
                                                                                
MISCFLG1 DS    XL1                 MISC FLAG #1                                 
MF1ERRQ  EQU    X01                                                             
                                                                                
SELFLAG1 DS    XL1                 SELECT FLAG #1                               
SF1TSLOK EQU    X80                 TESTSEL RETURNED W/ OK                      
                                                                                
CHNGFLG1 DS    XL1                 CHANGED (FROM PREV TRNSCTN) FLAG #1          
CF1ACTEQ EQU    X80                 ACTION EQUATE CHANGED                       
CF1KEY   EQU    X40                 A KEY FIELD CHANGED                         
CF1OPT   EQU    X20                 AN OPTION CHANGED                           
CF1AK    EQU   CF1ACTEQ+CF1KEY                                                  
CF1AKO   EQU   CF1ACTEQ+CF1KEY+CF1OPT                                           
CF1KO    EQU   CF1KEY+CF1OPT                                                    
                                                                                
*                                 *********** MISCELLANEOUS ***********         
GKSIPARM DS    CL5                 REGETKSRC INPUT  BLOCK                       
GKSOPARM DS    CL5                 REGETKSRC OUTPUT BLOCK                       
                                                                                
MY_CSTAT DS    CL(L'CSTAT)         SAVE FIELD FOR CSTAT                         
                                                                                
SVDMINBT DS    XL1                SAVED DATAMANAGER INPUT BITS                  
                                                                                
*                                 ************** PROFILES *************         
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
                                                                                
*                                 ******* DATA DICTIONARY TERMS *******         
DSLIST   DS    0C                                                               
RE@10PF  DS     CL31               PF5=Top  6=Bottom  7=Up  8=Down              
RE@PF12R DS     CL7                12=Rtrn                                      
DSLISTX  EQU   *                                                                
                                                                                
*                                 ************** BUFFERS **************         
SAVEKEY  DS    XL(L'RINVKEY)                                                    
                                                                                
TEMPLT   DS    XL(LISTTABQ)                                                     
                                                                                
AFTCHBLK DS    A                   A(FETCH WORK AREA)                           
                                                                                
MYDMWORK DS    12D                                                              
                                                                                
MYFLD    DS    0X                  MY TWA FIELD WORK AREA                       
MYFLDH   DS     XL8                 MY WORKING FIELD HEADER                     
MYFLDD   DS     XL80                MY WORKING FIELD DATA                       
MYFLDL   EQU   *-MYFLD                                                          
                                                                                
MYFLAG2  DS    XL1                                                              
PJORES   EQU   X'01'               PROJECTED OR ESTIMATE BOOK                   
*                                                                               
MYSSPREL EQU   *-SYSSPARE                                                       
SYSPREMN EQU   L'SYSSPARE-MYSSPREL   AMOUNT LEFT IN SYSSPARE                    
         DS    0XL(SYSPREMN+1)       CHECK AGAINST SYSSPARE LIMIT               
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (TWA DSECTS)'              
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
*------------------------- INV/TRACKS SCREEN -------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPE8D                                                       
*                                                                               
         ORG   ITKPGMH                                                          
       ++INCLUDE RERMPE9D          TRACK/DEMOS SCREEN                           
*                                                                               
*&&DO                                                                           
         ORG   IT2WORK+400                                                      
*&&                                                                             
         ORG   IT2WORK+300                                                      
MTSCRN   DS    XL1                 CURRENT SCREEN                               
MTSTA    DS    CL5                 STATION                                      
MTSTALN  DS    XL1                 L'STATION                                    
MTINVLN  DS    XL1                 L'INV #                                      
MTEFFLN  DS    XL1                 L'EFF                                        
MTEFF    DS    XL4                 START AND END EFF DATES                      
MTDEMOLS DS    XL50                DEMO LIST                                    
*                                                                               
ORIGFTNT DS    CL16                ORIGINAL FOOTNOTE                            
*                                                                               
*-------------------------- INV/ROVER SCREEN -------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPE7D                                                       
                                                                                
                                                                                
*-------------------------- INV/DOVER SCREEN -------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPE6D                                                       
                                                                                
                                                                                
*-------------------------- INV/DEMOS SCREEN -------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPBED                                                       
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
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (DBLOCKD)'                 
***********************************************************************         
*============================= DEMO BLOCK ============================*         
                                                                                
* DEDBLOCK *                                                                    
DBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
                                                                                
                                                                                
* DEDBEXTRAD *                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBEXTRAD                                                     
         PRINT ON                                                               
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (OTHER DSECTS)'            
***********************************************************************         
*============================ OTHER DSECTS ===========================*         
                                                                                
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* DDPERVALD                                                                     
* DDCOREQUS                                                                     
* DDDICTATED                                                                    
* REMSGEQUS                                                                     
* REDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDDICTATED                                                     
       ++INCLUDE REMSGEQUS                                                      
       ++INCLUDE REDDEQUS                                                       
       ++INCLUDE RERMPPROF                                                      
       ++INCLUDE REFETCHD                                                       
         PRINT ON                                                               
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (REGEN DSECTS)'            
***********************************************************************         
*============================ REGEN DSECTS ===========================*         
                                                                                
*------------------------------ REGENINV -----------------------------*         
REINVRCD DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
***********************************************************************         
         TITLE 'RERMP17 - INVENTORY ROAM && OVERRIDE (MISC DSECTS)'             
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
*------------------------- ENTRY IN LIST TABLE -----------------------*         
                                                                                
LISTTABD DSECT                                                                  
LTNTRY   DS    XL1                 NTH ENTRY                                    
                                                                                
LTKEY    DS    0X                  KEY                                          
LTXKBK   DS     XL2                 (INVERSED) BOOK                             
LTBTK    DS     XL1                 KEY FOR TYPE OF BOOK (EST,PRJ,...)          
LTKSRC   DS     CL1                 SOURCE                                      
LTKEYL   EQU   *-LTKEY             L(KEY)                                       
                                                                                
LTDATA   DS    0X                  DATA                                         
LTCODE   DS     CL2                 PROGRAM/BREAK CODE (RINVCODE)               
LTCSET   DS     CL1                 RINVCSET FIELD IN RECORD                    
LTFRDETL DS     CL27                FROM DETAILS                                
LTFTNOTE DS     CL16                FOOTNOTE (PROGRAM NAME)                     
LTFLAG   DS     XL1                 FLAG                                        
LTFSEL   EQU     X80                 ENTRY IS SELECTED                          
LTFPRMPT EQU     X40                 USER PROMPTED TO CHANGE ENTRY              
LTTRKRES EQU     X20                 TRACK RESTORED                             
LTFFTNOV EQU     X01                 FOOTNOTE OVERRIDED                         
LTDATAL  EQU   *-LTDATA            L(DATA)                                      
                                                                                
LISTTABQ EQU   *-LISTTABD                                                       
                                                                                
                                                                                
*----------------------------- DAY TABLE -----------------------------*         
                                                                                
DAYTABD  DSECT                                                                  
DYTFILE  DS    CL1                 FILE (X'00'==>ALL FILES)                     
DYTIDAY  DS    XL1                 INTERNAL DAY                                 
DYTKDAY  DS    XL1                 KEY DAY                                      
DYTNAME  DS    CL3                 NAME OF DAY(S)                               
DYTLEN   EQU   *-DAYTABD                                                        
                                                                                
                                                                                
*-------------------------- REGETKSRC PARMS --------------------------*         
                                                                                
GKSPARMD DSECT                                                                  
GKSPRSVC DS    CL1                 RATING SERVICE                               
GKSPQLFY DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GKSPKSRC DS    XL1                 RINVKSRC FOR KEY                             
GKSPBKBT DS    XL1                 BOOKVAL BITS                                 
GKSPBTYP DS    CL1                 BOOK TYPE                                    
GKSPARMX EQU   *                                                                
GKSPARML EQU   GKSPARMX-GKSPARMD                                                
         DS    0XL(L'GKSIPARM-GKSPARML+1)                                       
         DS    0XL(GKSPARML-L'GKSIPARM+1)                                       
         DS    0XL(L'GKSOPARM-GKSPARML+1)                                       
         DS    0XL(GKSPARML-L'GKSOPARM+1)                                       
         EJECT                                                                  
*---------------------------- DISPLAY LINE ---------------------------*         
                                                                                
DLINED   DSECT                                                                  
DLSELH   DS    XL8                 SEL FIELD HEADER                             
DLSEL    DS    CL3                 SELECT FIELD                                 
DLSELX   DS    XL8                 SEL EXTENDED FIELD HEADER                    
                                                                                
                                                                                
DLLDTAH  DS    XL8                 LIST DATA FIELD HEADER                       
DLLDTA   DS    0CL(L'ITKLDTA)      LIST DATA                                    
         DS     XL1                                                             
DLLSRC   DS     CL1                 SOURCE                                      
         DS     XL2                                                             
DLLBOOK  DS     CL10                BOOK                                        
         DS     XL2                                                             
DLLCODE  DS     CL2                 CODE                                        
         DS     XL2                                                             
DLLFDTL  DS     CL27                FROM DETAILS                                
                                                                                
                                                                                
DLFTNTH  DS    XL8                 FOOTNOTE FIELD HEADER                        
DLFTNT   DS    XL16                FOOTNOTE                                     
DLFTNTX  DS    XL8                 FOOTNOTE EXTENDED FIELD HEADER               
                                                                                
                                                                                
DLINEQ   EQU   *-DLINED                                                         
         DS    0XL(SZLSTLIN-DLINEQ+1)                                           
         DS    0XL(DLINEQ-SZLSTLIN+1)                                           
*---------------------------------------------------------------                
DEMLINED DSECT                                                                  
DEMSELH  DS    XL8                 SEL FIELD HEADER                             
DEMSEL   DS    CL3                 SELECT FIELD                                 
DEMSELX  DS    XL8                 SEL EXTENDED FIELD HEADER                    
*                                                                               
DEMDATAH DS    XL8                 LIST DATA FIELD HEADER                       
DEMDATA  DS    0CL(L'IT2DATA)      LIST DATA                                    
         DS     XL1                                                             
DEMSRC   DS     CL1                 SOURCE                                      
         DS     XL2                                                             
DEMBOOK  DS     CL10                BOOK                                        
         DS     XL2                                                             
DEMR1    DS     CL2                 RATING                                      
         DS     XL1                                                             
DEMS1    DS     CL2                 SHARE                                       
         DS     XL1                                                             
DEMH1    DS     CL2                 HUT                                         
         DS     XL2                                                             
DEMR2    DS     CL2                 RATING                                      
         DS     XL1                                                             
DEMS2    DS     CL2                 SHARE                                       
         DS     XL1                                                             
DEMP2    DS     CL2                 PUT                                         
         DS     XL2                                                             
DEMR3    DS     CL2                 RATING                                      
         DS     XL1                                                             
DEMS3    DS     CL2                 SHARE                                       
         DS     XL1                                                             
DEMP3    DS     CL2                 PUT                                         
         DS     XL2                                                             
DEMR4    DS     CL2                 RATING                                      
         DS     XL1                                                             
DEMS4    DS     CL2                 SHARE                                       
         DS     XL1                                                             
DEMP4    DS     CL2                 PUT                                         
         DS     XL2                                                             
DEMCODE  DS     CL2                 CODE HEADER                                 
*                                                                               
DEMFTNTH DS    XL8                 FOOTNOTE FIELD HEADER                        
DEMFTNT  DS    XL14                FOOTNOTE                                     
DEMFTNTX DS    XL8                 FOOTNOTE EXTENDED FIELD HEADER               
*                                                                               
DEMLINEQ EQU   *-DEMLINED                                                       
         DS    0XL(SZDEMLIN-DEMLINEQ+1)                                         
         DS    0XL(DEMLINEQ-SZDEMLIN+1)                                         
*---------------------------------------------------------------                
FETCHIOD DSECT                                                                  
FETCHBLK DS    XL2000              USED TO BUILD FETCH BLOCK                    
FETCHWRK DS    XL6000              WORK SPACE FOR FETCH                         
FTCHIOLN EQU   *-FETCHIOD                                                       
                                                                                
*----------------------------- PFKEY LINE ----------------------------*         
                                                                                
PFLINED  DSECT                                                                  
PFLSCRLL DS    CL(L'RE@10PF)       PF5=Top  6=Bottom  7=Up  8=Down              
                                                                                
         ORG   (PFLINED+L'ITKPFLN)-L'RE@PF12R                                   
PFLRTRN  DS    CL(L'RE@PF12R)      12=Rtrn                                      
                                                                                
                                                                                
PFLINEX  EQU   *                                                                
PFLINEL  EQU   PFLINEX-PFLINED                                                  
         DS    0XL(L'ITKPFLN-PFLINEL+1)                                         
         DS    0XL(PFLINEL-L'ITKPFLN+1)                                         
                                                                                
                                                                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== BIG STORAGE AREA =========================*         
                                                                                
BIGAREA  DSECT                                                                  
LSTBLABL DS    D                   *LSTTAB*                                     
LISTTAB  DS    (MXLTNTRY)XL(LISTTABQ)                                           
         DS    XL1                                                              
LISTTABX EQU   *                                                                
                                                                                
IVHDLABL DS    D                   *INVHDR*                                     
IVHDRECD DS    XL2000                                                           
IVHDRECX EQU   *                                                                
                                                                                
BIGENDSV EQU   *                                                                
                                                                                
DBLKLABL DS    D                   *DBLOCK*                                     
DBLOCK1  DS    XL(L'DBLOCK)                                                     
         DS    XL14                (RESERVED)                                   
DBLOCK1X DS    0X                                                               
                                                                                
DBXTLABL DS    D                   *DBXTND*                                     
DBXTND1  DS    XL256                                                            
DBXTND1X DS    0X                                                               
                                                                                
MYTIALEN EQU   *-BIGAREA                                                        
         DS    0CL((X'2400'-MYTIALEN)+1)  2ND HALF FOR OVRLYS UNDRNEATH         
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017RERMP17S  05/01/02'                                      
         END                                                                    
