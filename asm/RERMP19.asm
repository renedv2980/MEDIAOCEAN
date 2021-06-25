*          DATA SET RERMP19    AT LEVEL 032 AS OF 06/30/09                      
*PHASE T81019C                                                                  
         TITLE 'DOVER - << DOVER >>  INV DEMO DISPLAY AND OVERRIDE'             
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* MAR00/09 025 BOBY - NEW INVENTORY RECORD KEY                        *         
* OCT23/00 121 BPOO - CHANGE IMPRESSION WHEN RATINGS CHANGE ON SCREEN *         
* JUL16/96 120 MAYA - PROFILES SUPPORT: DISALLOW OVRDS ON CERTAIN BKS *         
* JUL15/96 072 MAYA - MEDIAFAX/H-SHRS AND PUTS/FOOTNOTE/DAYS/TIMES    *         
* APR11/96 017 MAYA - SADLY DELETED VERT PF2 'RIPPLE'/ CORE DEMO SEED *         
* MAR27/96 003 MAYA - SUPPORT DEMOLIST OPTION                         *         
* MAR13/96 002 MAYA - HANDLE HORIZ RIPPLING                           *         
* DEC26/95 001 MAYA - INVENTORY DEMO DISPLAY AND OVERIDE HANDLING     *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
RMP19    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81019**,R9,RR=RE                                              
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
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    VREC                                                             
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
XIT_RTRN DS    0H                  EXIT PROGRAM AND RETURN TO CALLER            
         CLI   CALLSP,0             MAKE SURE THERE WAS A CALLER                
         BH    *+6                                                              
         DC    H'0'                                                             
         SR    R2,R2                LET GENCON DECIDE WHERE CURSOR              
         MVI   PFKEY,12             SIMULATE THE HIT OF PF12                    
         MVI   GOAGAIN,C'Y'                                                     
         GOTO1 ERREX2                                                           
         EJECT                                                                  
***********************************************************************         
*========================= MY INITIALIZATION =========================*         
         DS    0H                  SET UP ADCONS OF TABLES & ROUTINES           
MYINIT   NTR1                                                                   
         LH    R2,=Y(DISPTAB-RMP19)                                             
         LA    R2,RMP19(R2)                                                     
         LA    R0,DISPTABQ                                                      
                                                                                
MI10     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         LA    RE,RMP19            RE = BASE OF TABLE/ROUTINE                   
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
         LH    R2,=Y(LBLTAB-RMP19)                                              
         LA    R2,RMP19(R2)                                                     
         LA    R0,LBLTABQ                                                       
                                                                                
MI20     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         A     R1,ATIA             R1 = A(TO PUT LABEL)                         
         MVC   0(8,R1),2(R2)        AND MOVE LABEL IN                           
         LA    R2,L'LBLTAB(R2)                                                  
         BCT   R0,MI20                                                          
*                                                                               
         L     RF,ACOMFACS         ACOMFACS                                     
         MVC   ADEMAINT,CDEMAINT-COMFACSD(RF)                                   
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
         MVC   AIO,AIO1                                                         
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
*                                                                               
         DS    0H                  MOVE PROFILE TO LOCAL WORKNG STORAGE         
         LR    RF,RA                                                            
         AH    RF,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,RF                                                       
         MVC   RMPPROFS,SVPGPBIT   PROFILE BITS                                 
         DROP  RF                                                               
*                                                                               
         OI    CONSERVH+6,X80+X01  FOR PFKEYS TO WORK                           
*                                                                               
         LA    R2,DOVPFLNH         MOVE IN PFKEY INFO                           
         OI    DOVPFLNH+6,X80                                                   
         MVC   DOVPFLN(L'DOVPFLN),SPACES                                        
         MVC   DOVPFLN(6),=C'PF7=Up'                                            
         MVC   DOVPFLN+8(6),=C'8=Down'                                          
         MVC   DOVPFLN+16(7),=C'10=Save'                                        
         CLI   CALLSP,0                                                         
         BE    MI68                                                             
         MVC   DOVPFLN+50(7),=C'12=Next'                                        
         CLI   ITSHNSEL,1                                                       
         BH    MI68                                                             
         MVC   DOVPFLN+50(7),=C'12=Rtrn'                                        
*                                                                               
MI68     DS    0H                                                               
*                                                                               
         B     XIT                                                              
***********************************************************************         
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (VALKEY)'                       
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
VKEY     DS    0H                                                               
         NI    MISCFLG1,XFF-MF1KYCHG                                            
                                                                                
         XC    OPTR,OPTR           NO OPTIONS REQUIRED                          
         XC    OPTX,OPTX            NOR DIS-ALLOWED                             
*                                                                               
         DS    0H                  FILL IN STUFF FROM INEFFTAB                  
         OC    INEFFTAB,INEFFTAB                                                
         BZ    VK005X                                                           
         MVC   DOVINV,INEFFTAB+1                                                
         MVI   DOVINVH+5,4                                                      
         OI    DOVINVH+6,X80                                                    
         MVC   DOVEFFD,(INEFFTAB+1)+4                                           
         MVI   DOVEFFDH+5,8                                                     
         OI    DOVEFFDH+6,X80                                                   
         MVI   DOVSRC,C'N'                                                      
         MVI   DOVSRCH+5,1                                                      
         OI    DOVSRCH+6,X80                                                    
VK005X   EQU   *                                                                
*                                                                               
*-------------------------- VALIDATE STATION -------------------------*         
*                                                                               
         LA    R2,DOVSTTNH         STATION                                      
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         GOTO1 ANY                  MUST HAVE A STATION INPUT                   
         GOTO1 VALISTA              VALIDATE THE STATION INPUTTED               
         OI    4(R2),X20                                                        
         MVC   IKSTTN,WORK                                                      
         MVI   IKSTTN+4,C'T'                                                    
         CLI   WORK+4,C' '                                                      
         BE    *+10                                                             
         MVC   IKSTTN+4(1),WORK+4                                               
         CLI   WORK+40,C' '                                                     
         BE    *+10                                                             
         MVC   IKSTTN+4(1),WORK+40 CHECK SATTELITE                              
                                                                                
*        MVC   IKSTTN,WORK         HOLD ONTO KEY FIELD INPUT                    
*        CLI   IKSTTN+4,C' '                                                    
*        BNE   *+8                                                              
*        MVI   IKSTTN+4,C'T'                                                    
*                                                                               
*---------------------- VALIDATE INVOICE NUMBER ----------------------*         
*                                                                               
         LA    R2,DOVINVH          INVOICE NUMBER                               
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         GOTO1 ANY                  MUST HAVE AN INVOICE # INPUT                
         OI    4(R2),X20                                                        
                                                                                
         MVC   IKINV,WORK          HOLD ONTO KEY FIELD INPUT                    
*                                                                               
*--------------------------- EFFECTIVE DATE --------------------------*         
*                                                                               
         XC    IKEFFDB,IKEFFDB                                                  
         XC    IKEFFDC,IKEFFDC                                                  
         LA    R2,DOVEFFDH         EFFECTIVE DATE                               
         BAS   RE,KYCHNGED        SEE IF THIS KEY FIELD CHANGED >>>>            
         CLI   5(R2),0                                                          
         BE    VK040                                                            
         DS    0H                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    INVLFLD                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,IKEFFDB)                                 
         GOTO1 DATCON,DMCB,(0,WORK),(2,IKEFFDC)                                 
         OI    4(R2),X20                                                        
*                                                                               
*-------------------------- VALIDATE SOURCE --------------------------*         
*                                                                               
VK040    DS    0H                                                               
         LA    R2,DOVSRCH          SOURCE (RATING SERVICE)                      
         CLI   CALLSP,0            IF CALLED FROM ANOTHER SCREEN,               
         BE    VK042                                                            
         CLI   5(R2),0              THERE MAY BE NOTHING IN KEY FIELD           
         BH    VK042                                                            
         B     VKX_RTRN                                                         
*                                                                               
VK042    CLI   5(R2),0                                                          
         BH    *+20                                                             
         OI    4(R2),X80                                                        
         MVI   5(R2),1             DEFAULT=NSI                                  
         OI    6(R2),X80                                                        
         MVI   8(R2),C'N'                                                       
*                                                                               
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   IKRSVC,=C'NSI'      ASSUME NIELSEN                               
         EXCLC R1,WORK,=C'NSI'                                                  
         BE    VK045                                                            
         MVC   IKRSVC,=C'ARB'      ASSUME ARBITRON                              
         EXCLC R1,WORK,=C'ARB'                                                  
         BE    VK045                                                            
         MVC   IKRSVC,=C'SRC'      ASSUME STRATEGY                              
         EXCLC R1,WORK,=C'SRC'                                                  
         BE    VK045                                                            
         MVC   IKRSVC,=C'MFX'      ASSUME MEDIAFAX                              
         EXCLC R1,WORK,=C'MFX'                                                  
         BNE   INVLFLD                                                          
                                                                                
VK045    DS    0H                                                               
         OI    4(R2),X20                                                        
*                                                                               
*--------------------------- VALIDATE BOOK ---------------------------*         
*                                                                               
         LA    R2,DOVBOOKH         BOOK                                         
         OC    INEFFTAB,INEFFTAB   CAME FROM INV/LIST?                          
         BZ    VK050                                                            
         XC    INEFFTAB,INEFFTAB   YES - NEED BOOK                              
         CLI   5(R2),0             MUST HAVE SOMETHING IN BOOK FIELD            
         BE    MISSFLD                                                          
         B     VK052                                                            
*                                                                               
VK050    CLI   CALLSP,0            IF CALLED FROM ANOTHER SCREEN,               
         BE    VK052                                                            
         CLI   5(R2),0             THERE MAY BE NOTHING IN KEY FIELD            
         BH    VK052                                                            
         B     VKX_RTRN            IF SO, RETURN DIRECTLY TO CALLER             
*                                                                               
VK052    BAS   RE,KYCHNGED         SEE IF THIS KEY FIELD CHANGED                
         GOTO1 ANY                                                              
*                                                                               
         XC    DUB,DUB                                                          
         GOTO1 BOOKVAL,DMCB,(IKRSVC,(R2)),(1,WORK),                    X        
               (C'B',SCANNER),DUB,(C'C',ACOMFACS)                               
         CLI   4(R1),1                                                          
         BNE   INVLFLD                                                          
         MVC   IKFRBK,WORK         SET BITS AND BOOK                            
         MVC   IKBTYPE,DUB                                                      
         MVC   IKQLFK,IKFRBTS      SET QUALIFIER                                
         XC    WORK(5),WORK        OUTPUT BLOCK GOES INTO WORK                  
         XC    IBLK,IBLK           INPUT BLOCK:                                 
         MVC   IBLK+2(1),IKFRBTS     BOOKVAL BITS                               
         MVC   IBLK+3(1),IKBTYPE     BOOKTYPE                                   
         GOTO1 VGETKSRC,DMCB,(C'B',IBLK),WORK,ACOMFACS                          
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BNE   INVLFLD             INVALID FIELD                                
         MVC   IKTYPBK,WORK+1       & TRK QLFYR (E/P/S/T/' ')                   
         OI    4(R2),X20           FIELD HAS BEEN VALIDATED                     
*                                                                               
         MVI   GOSUBN,PRF#         SET PRFOVR FLAG                              
         GOTO1 AGOSUB                                                           
         DS    0H                  RESTORE SAVED AREAS                          
         MVI   GOSUBN,RTI#         RESTORE TIA AREA                             
         GOTO1 AGOSUB                                                           
*                                                                               
         TM    MISCFLG1,MF1KYCHG   DID A KEY FIELD CHANGE?                      
         BZ    VK063                                                            
         MVI   GOSUBN,CLA#         CLEAR SYSPARE STGR AREAS                     
         GOTO1 AGOSUB                                                           
                                                                                
*----------------------- VALIDATE OPTIONS FIELD ----------------------*         
*                                                                               
VK063    DS    0H                                                               
         NI    MISCFLG1,XFF-MF1OPCHG                                            
         LH    R3,=Y(MGFOPT-RMP19) NEED TO MERGE OPTION FIELDS                  
         A     R3,MYBASE1                                                       
         MVI   GOSUBN,MGF#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         LA    R2,BLOCK            BLOCK CONTAINS MERGED FIELDS                 
         TM    4(R2),X80           WAS FIELD INPUTTED THIS TIME?                
         BZ    *+12                                                             
         OI    MISCFLG1,MF1OPCHG    YES, AN OPTION CHANGED                      
         B     VK064                                                            
         TM    MISCFLG1,MF1KYCHG   DID A KEY FIELD CHANGE?                      
         BO    VK064                YES, VALIDATE OPTIONS                       
         B     VK068               DON'T VALIDATE OPTIONS FIELD                 
                                                                                
VK064    DS    0H                                                               
         BAS   RE,VALOPT            GO VALIDATE OPTIONS                         
         BE    VK068                                                            
*                                                                               
         DS    0H                  INVALID OPTION ENCOUNTERED                   
         LH    R3,=Y(MGFOPT-RMP19)  DETERMINE WHERE TO PUT CURSOR               
         A     R3,MYBASE1                                                       
                                                                                
VK066A   DS    0H                                                               
         ZICM  R2,0(R3),(3)        IF END OF TABLE REACHED,                     
         BZ    VK066B                                                           
         LA    R2,T810FFD(R2)      R2-->AN OPTION FLD THAT WAS MERGED           
         ZIC   RE,5(R2)            RE = L(INPUT IN THIS OPTN FLD)               
         ZIC   RF,FLDDSPL          RF = DISPL INTO OPTN FLD                     
         CR    RE,RF               IF L(INPUT) > DSPL INTO FIELD,               
         BH    VK066B               THIS IS THE FLD TO PUT CURSOR ON            
         SR    RF,RE               ELSE, KEEP REDUCING FLDDSPL                  
         STC   RF,FLDDSPL                                                       
         LA    R3,2(R3)                                                         
         B     VK066A                                                           
                                                                                
VK066B   DS    0H                  SET TIOB FOR CURSOR POSITION                 
         OR    R2,R2                DOES R2 POINT TO AN OPTIONS FIELD?          
         BNZ   *+8                                                              
         LA    R2,DOVOPT1H           NO, POINT IT TO 1ST OPTION FIELD           
         L     R1,SYSPARMS                                                      
         L     RF,0(R1)             RF-->TIOB                                   
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         S     R0,ATWA                                                          
         STH   R0,TIOBCURD                                                      
         MVC   TIOBCURI,FLDDSPL                                                 
         DROP  RF                                                               
                                                                                
         B     OURERROR                                                         
*                                                                               
VK068    DS    0H                  OPTIONS INPUTTED ARE VALID                   
         ZICM  RE,0(R3),(3)                                                     
         BZ    VK070                                                            
         A     RE,ATWA                                                          
         OI    4(RE),X20            TURN ON "FIELD HAS BEEN VALIDATED"          
         LA    R3,2(R3)                                                         
         B     VK068                                                            
                                                                                
                                                                                
VK070    DS    0H                                                               
*                                                                               
*--------------------- MISCELLANEOUS VALKEY TASKS --------------------*         
*                                                                               
VK100    DS    0H                  GET INV RECD                                 
         MVI   GOSUBN,BIK#          BUILD KEY TO INVENTORY RECORD               
         GOTO1 AGOSUB                                                           
         MVC   SAVEKEY,KEY           AND HOLD ONTO IT                           
                                                                                
         LA    R2,DOVSTTNH          PLACE CURSOR HERE IN CASE OF ERROR          
         MVI   MYRDUPDT,C'N'                                                    
         MVI   GOSUBN,GIR#         GET INVENTORY RECORD                         
         GOTO1 AGOSUB               KEY & AIO CONTAINS INV KEY & RECD           
         BNE   RCDNTFND                                                         
*                                                                               
         DS    0H                  DISPLAY EFFECTIVE DATE KEY FIELD             
         L     R6,AIO                                                           
         USING REINVRCD,R6                                                      
         CLC   RINVKSTD,IKEFFDB     IF THE INPUT IS DIFFERENT FROM RCD          
         BE    VK120                                                            
         MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,WORK)                                
         MVC   DOVEFFD,WORK         PUT EFFECTIVE DATE ON SCREEN                
         OI    DOVEFFDH+6,X80        AND TRANSMIT IT                            
         OI    DOVEFFDH+4,X20      MARK AS VALIDATED                            
         DROP  R6                                                               
*                                                                               
VK120    DS    0H                                                               
*        MVI   GOSUBN,GHI#         GET INV HDR INFO                             
*        GOTO1 AGOSUB                                                           
*                                                                               
VK140    BAS   RE,SETUP            <-- SETUP NOW CALLS GETHDR                   
         B     VKX                                                              
*                                                                               
VKX_RTRN DS    0H                                                               
         MVI   GOSUBN,STI#                                                      
         GOTO1 AGOSUB                                                           
         B     XIT_RTRN            RETURN TO CALLER                             
*                                                                               
VKX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------- SETUP PROCEDURE --------------------------*         
                                                                                
* At entry,                                                                     
*   KEY = key of inventory record,                                              
*   AIO = A(inventory record)                                                   
                                                                                
         DS    0H                                                               
SETUP    NTR1                                                                   
         L     R6,AIO                                                           
         USING REINVRCD,R6                                                      
         CLC   RINVKSTD,IKEFFDB     IF THE INPUT IS DIFFERENT FROM RCD          
         BE    SU010                                                            
         MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,WORK)                                
         MVC   DOVEFFD,WORK         PUT EFFECTIVE DATE ON SCREEN                
         OI    DOVEFFDH+6,X80       AND TRANSMIT IT                             
         OI    DOVEFFDH+4,X20      MARK AS VALIDATED                            
         DROP  R6                                                               
*                                                                               
SU010    DS    0H                                                               
         MVI   GOSUBN,GHI#         GET HEADER INFORMATION                       
         GOTO1 AGOSUB                                                           
*                                                                               
         TM    MISCFLG1,MF1KYCHG    IF KEY CHANGED,                             
         BZ    SU020                                                            
         MVI   GOSUBN,CLA#          CLEAR TWA & SYSPARE STRG AREAS              
         GOTO1 AGOSUB                                                           
                                                                                
SU020    DS    0H                                                               
         B     XIT                                                              
                                                                                
         EJECT                                                                  
*------------------------- TEST CHANGE OF KEY ------------------------*         
*                                                                               
KYCHNGED DS    0H                                                               
         TM    4(R2),X20           VALIDATED PREVIOUSLY                         
         BZ    KYCH10                                                           
         TM    4(R2),X80           FIELD INPUT THIS TIME                        
         BZR   RE                                                               
KYCH10   OI    MISCFLG1,MF1KYCHG                                                
         MVI   FRST,C'Y'           RE-INIT SAVED STRG & SCRN FLDS               
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (VALREC)'                       
***********************************************************************         
*========================== VALIDATE RECORD ==========================*         
VREC     DS    0H                                                               
**       CLI   FRST,C'Y'           FIRST TIME IN?                               
**       BE    VR02                                                             
**       MVI   GOSUBN,STI#         SAVE TIA                                     
**       GOTO1 AGOSUB                                                           
VR02     XC    ACURFORC,ACURFORC                                                
         MVI   OURINFCD,0                                                       
         MVI   OURERRCD,0                                                       
         MVC   STDEM,TOPDEM                                                     
*                                                                               
         CLI   PFKEY,10            SAVE REQUEST?                                
         BNE   VR05                                                             
         MVI   RDUPDATE,C'Y'       READ RECD FOR UPDATE                         
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
VR05     OC    OPVMINTB,OPVMINTB                                                
         BZ    VR07                                                             
         MVI   GOSUBN,MIN#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
VR07     TM    MISCFLG1,MF1KYCHG   DID KEY CHANGE?                              
         BZ    *+8                                                              
         MVI   FRST,C'Y'           SET TO FIRST TIME IN                         
         CLI   FRST,C'Y'           FIRST TIME IN?                               
         BNE   VR10                                                             
         MVI   EDT,C'C'                                                         
         MVI   GOSUBN,EDT#         COPY OVRS & DEM ELEMS FROM ORIG RECD         
         GOTO1 AGOSUB                                                           
         XC    STDEM,STDEM         START AT TOP OF TABLE                        
         MVI   COL,CMALE                                                        
         MVI   COL+1,CFEM                                                       
         MVI   COL+2,CTOT                                                       
         LH    R1,STDEM                                                         
         MVI   FRST,C'N'                                                        
         MVI   GOSUBN,PRF#         SET PROFILE ACTION FLAGS                     
         GOTO1 AGOSUB                                                           
         B     VR30                                                             
*                                                                               
VR10     DS    0H                                                               
         MVI   FRST,C'N'                                                        
*                                                                               
VR11     MVI   GOSUBN,LST#         BLD DEMOLIST & GET DEMOS                     
         GOTO1 AGOSUB                                                           
         BE    VR12                                                             
         MVI   GOSUBN,CLS#         CLEAR SCREEN                                 
         GOTO1 (RF)                                                             
         B     VRECX                                                            
*                                                                               
VR12     CLI   PRFOVR,0            PROFILES SAY OVERRIDES OKAY TO DO?           
         BNE   VR15                NO, THEY DON'T ALLOW THEM                    
         MVI   GOSUBN,VSC#         VALIDATE CHANGES ON SCREEN                   
         GOTO1 (RF)                                                             
         BE    VR13                                                             
         L     R2,DMCB                                                          
         CLI   OURINFCD,TOOVRQ     SEND MSG AND DISPLAY WHAT i HAVE             
         BNE   VRECX                                                            
         B     VR30                                                             
*                                                                               
VR13     CLI   PFKEY,10            PF10= SAVE RECD?                             
         BNE   VR15                                                             
         MVI   GOSUBN,WRT#         VALIDATE CHANGES ON SCREEN                   
         GOTO1 AGOSUB                                                           
         BNE   VR30                REDISPLAY RECD & GIVE MESSAGE                
         MVI   OURINFCD,CHGDQ      RECORD CHANGED                               
         B     VRECX                                                            
*                                                                               
VR15     LH    R1,STDEM                                                         
         CLI   PFKEY,7             PF7 = PAGE UP?                               
         BNE   VR20                                                             
         SH    R1,=Y(SCRLNQ)       BACK UP TO PREV PAGE                         
         BP    *+6                                                              
         SR    R1,R1               PAST TOP OF LIST-> START AT TOP TABL         
         STH   R1,STDEM                                                         
         B     VR30                                                             
*                                                                               
VR20     CLI   PFKEY,8             PF8 = PAGE DOWN?                             
         BNE   VR25                                                             
         AH    R1,=Y(SCRLNQ)                                                    
         CH    R1,=Y(DEMQ)         PAST LAST DEMO IN TABLE?                     
         BNL   VR30                END OF TABLE, JUST TREAT AS <ENTER>          
         STH   R1,STDEM                                                         
         B     VR30                                                             
*                                                                               
VR25     CLI   PFKEY,12            PF12= RETURN                                 
         BNE   VR30                                                             
         MVI   GOSUBN,STI#         SAVE TIA                                     
         GOTO1 AGOSUB                                                           
         CLI   CALLSP,0                                                         
         BNE   XIT_RTRN                                                         
*                                                                               
VR30     BAS   RE,SETTAB           IF OPTN OR KEY CHG, RE-BUILD DEMTAB          
         MVI   GOSUBN,CLS#         CLEAR SCREEN AND SET XMIT BITS ON            
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,DSI#         DISP info about this inv recd                
         GOTO1 (RF)                                                             
         MVI   GOSUBN,LST#         BLD DEMOLIST                                 
         GOTO1 (RF)                                                             
         BNE   VRECX               NO DEMOS ON RECD?                            
         MVI   GOSUBN,BLD#         GET DEMOS FROM DEMOUT W/SVOVR                
         GOTO1 (RF)                                                             
         BNE   VRECX               ERROR GETTING DEMS                           
         MVI   GOSUBN,DSD#         DISP DEMOS IN DEMOVAL ON SCREEN              
         GOTO1 (RF)                                                             
*                                                                               
VRECX    DS    0H                                                               
         MVI   GOSUBN,STI#         SAVE TIA                                     
         GOTO1 AGOSUB                                                           
         CLI   OURERRCD,0          ANY ERRORS?                                  
         BNE   OURERROR                                                         
         CLI   OURINFCD,0          DISPLAY INFO MSG?                            
         BNE   OURINFO                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*SETTAB - CREATES DEMOTAB FROM DEMOS REQUESTED IN OPTIONS LIST AND/OR           
*         FROM ORIGDEMS TABLE                                                   
***********************************************************************         
SETTAB   DS    0H                                                               
         NTR1                                                                   
         TM    MISCFLG1,MF1KYCHG+MF1OPCHG                                       
         BZ    SETTABX             NEITHER KEY NOR OPTIONS CHANGED              
         OC    OPVDEMTB,OPVDEMTB   ANY RQSTD DEMOS?                             
         BNZ   SETB10              YES, GO GET THEM FIRST                       
         L     R0,ADEMTAB          NO BUILD DEMTAB FROM ORIG DEMO LIST          
         LA    R1,ORIGDEML                                                      
         L     RE,AORGDEM          FROM ORIGDEMS TABLE                          
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY DEMWRK DEMOS TO SVDEM AREA              
         B     SETTABX                                                          
*                                                                               
SETB10   XC    STDEM,STDEM         IF SPEC DEMOS REQSTD, DISP THEM 1ST          
         L     R0,ADEMTAB          CLEAR DEMTAB                                 
         LA    R1,ORIGDEML                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R6,OPVDEMTB         PT TO DEMOLIST ENTRIES RQSTD                 
         ZIC   R0,OPVDEMNM         # RQSTD DEMOS                                
         L     RF,ADEMTAB          RF=DEMO TABLE TO BE BUILT                    
*                                                                               
SETB12   L     RE,AORGDEM          RE=PT TO TOP OF ORIG DEMO TBL                
SETB15   CLC   0(L'OPVDEMTB,R6),DEMTNAME-DEMTABD(RE)                            
         BE    *+18                                                             
         LA    RE,DEMTABQ(RE)                                                   
         CLI   0(RE),XFF           END OF ORIG DEMO TABLE?                      
         BNE   SETB15                                                           
         DC    H'0'                                                             
         MVC   0(DEMTABQ,RF),0(RE)  MOVE ENTRY FROM ORIGTBL TO DEMTAB           
         LA    RF,DEMTABQ(RF)      PT TO NEXT FREE BUCKET IN DEMTAB             
         LA    R6,L'OPVDEMTB(R6)   NEXT RQSTD DEMO                              
         BCT   R0,SETB12                                                        
*                                                                               
         LA    R4,ORIGDMQ          TOTAL # ENTRIES ORIG DEMS                    
         L     RE,AORGDEM          RE=PT TO TOP OF ORIG DEMO TBL                
*                                                                               
SETB20   LA    R6,OPVDEMTB         PT TO DEMOLIST ENTRIES RQSTD                 
         ZIC   R0,OPVDEMNM         # RQSTD DEMOS                                
         CLC   0(L'OPVDEMTB,R6),DEMTNAME-DEMTABD(RE)                            
         BE    SETB25              DON'T COPY ENTRY AGAIN                       
         LA    R6,L'OPVDEMTB(R6)  NEXT RQSTD DEMO                               
         BCT   R0,*-14                                                          
         MVC   0(DEMTABQ,RF),0(RE)  NOT FOUND IN RQST LIST-> COPY ENTY          
         LA    RF,DEMTABQ(RF)      PT TO NEXT FREE BUCKET IN DEMTAB             
*                                                                               
SETB25   LA    RE,DEMTABQ(RE)      NEXT ENTY IN ORGDEM TBL                      
         BCT   R4,SETB20           BUMP CTR # ENTRYS COPIED SO FAR              
         MVI   0(RF),XFF           MARK EOT ON DEMTAB                           
*                                                                               
SETTABX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*VALOPT - VALIDATE OPITONS ON OPTION LINES                                      
* At entry,                                                                     
*   R2-->options field.                                                         
* On a clean exit,                                                              
*   CC set to equal.                                                            
* On an error exit,                                                             
*   CC set to not equal,                                                        
*   MYTEXT(1) = l(text replace in error msg) or 0,                              
*   MYTEXT+1  = text replace if MYTEXT=0,                                       
*   FLDDSPL   = displacement to sub-field w/ error,                             
*   OURERRCD  = error code.                                                     
***********************************************************************         
         DS    0H                                                               
VALOPT   NTR1                                                                   
         MVI   FLDDSPL,0                                                        
         MVI   GOSUBN,IOV#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         CLI   5(R2),0                                                          
         BE    VOPT200                                                          
*                                                                               
         MVI   OURERRCD,IFLDQ                                                   
         MVI   SCANLNTH,70                                                      
         GOTO1 SCANNER,DMCB,(SCANLNTH,(R2)),(X'8A',BUFF),0                      
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         BZ    VOPTXN              ERROR                                        
         STC   R0,NOPTN                                                         
*                                                                               
         MVI   COUNTER,0                                                        
         LA    R3,BUFF             R3-->SCANNER BLOCK                           
*                                                                               
** VALIDATE OPTION KEYWORD **                                                   
*                                                                               
VOPT020  DS    0H                  RUN THRU OPTION KEYWORDS TABLE               
         L     R4,AOPTTAB                                                       
         USING OPTDSECT,R4                                                      
                                                                                
         MVC   FLDDSPL,4(R3)       HOLD ONTO DISPLACEMENT INTO FIELD            
                                                                                
VOPT022  DS    0H                                                               
         CLI   0(R4),EOT           IF END OF OPTION KEYWORD TABLE,              
         BNE   VOPT024                                                          
         MVI   OURERRCD,INVOQ                                                   
         B     VOPTXN               THEN INPUT IS INVALID                       
                                                                                
VOPT024  DS    0H                                                               
         ZICM  RF,OPDKYTB,(3)                                                   
         LA    RF,OPTDSECT(RF)     RF-->TABLE OF NAMES FOR ENTRY                
                                                                                
VOPT030  DS    0H                                                               
         CLI   0(RF),EOT           IF AT END OF NAME TABLE,                     
         BE    VOPT036              IT'S NOT THIS OPTION ENTRY                  
         ZIC   R1,0(RF)                                                         
         ZIC   RE,0(R3)            RE = LENGTH OF INPUTTED KEYWORD              
         CR    R1,RE               L(KYWD IN TABLE) VS L(KYWD INPUTTED)         
         BL    VOPT033              LOW: TRY ANOTHER NAME                       
         BCTR  RE,0                                                             
         EXCLC RE,12(R3),1(RF)     IF THEY MATCH,                               
         BE    VOPT040              THEN THIS IS THE OPTION ENTRY               
                                                                                
VOPT033  DS    0H                                                               
         LA    RF,1(R1,RF)         ELSE, CHECK NEXT NAME IN TABLE               
         B     VOPT030                                                          
                                                                                
VOPT036  DS    0H                  BUMP TO NEXT OPTION ENTRY                    
         ZIC   R1,OPDNTRYL                                                      
         AR    R4,R1                                                            
         B     VOPT022                                                          
*                                                                               
VOPT040  DS    0H                  OPTION ENTRY LOCATED                         
         MVC   DUB(4),OPTI                                                      
         NC    DUB(4),OPDOPTB       IS THIS OPTION DUPLICATED?                  
         BZ    VOPT043                                                          
         MVI   OURERRCD,DUPOQ        YES, DUPLICATED OPTION KEYWORD             
         B     VOPTXN                                                           
                                                                                
VOPT043  DS    0H                                                               
         MVC   DUB(4),OPTX                                                      
         NC    DUB(4),OPDOPTB       IS THIS OPTION COMPATIBLE?                  
         BZ    VOPT046                                                          
         MVI   OURERRCD,IOCBQ        NO, INVALID OPTN COMBO                     
         B     VOPTXN                                                           
                                                                                
VOPT046  DS    0H                                                               
         ST    R4,AOPTNTRY                                                      
         OC    OPTR,OPDRQOPT       OTHER OPTIONS REQUIRED                       
         OC    OPTX,OPDNAOPT       OTHER OPTIONS NOT ALLOWED                    
         B     VOPT050                                                          
*                                                                               
** VALIDATE OPTION VALUE **                                                     
*                                                                               
VOPT050  DS    0H                                                               
         MVC   FLDDSPL,8(R3)       HOLD ONTO DISPLACEMENT INTO FIELD            
                                                                                
         TM    OPDFLAG,OPDFKYWD                                                 
         BO    VOPT060                                                          
                                                                                
         CLI   1(R3),0             THERE S/B DATA INPUT TO VALIDATE             
         BH    *+12                                                             
         MVI   OURERRCD,MODQ                                                    
         B     VOPTXN                                                           
                                                                                
         TM    OPDFLAG,OPDFVRTN                                                 
         BO    VOPT080                                                          
         TM    OPDFLAG,OPDFVTBL                                                 
         BO    VOPT100                                                          
         DC    H'0'                                                             
*                                                                               
*** OPTION VALUE VALID VIA KEYWORD ***                                          
*                                                                               
VOPT060  DS    0H                                                               
         ZICM  R5,OPDODSPL,(3)                                                  
         LA    R5,SYSD(R5)         R5-->OUTPUT AREA FOR OPTION DATA             
         ZIC   R1,0(R5)            R1 = # OF VALUES FOR OPTION SO FAR           
         CLM   R1,1,OPDMXNDV       DO WE HAVE MAX AMOUNT ALREADY                
         BL    VOPT062                                                          
         MVI   OURERRCD,TMODQ       YES, TOO MANY OPTN DATA ERROR               
         B     VOPTXN                                                           
                                                                                
VOPT062  DS    0H                                                               
         LA    R0,1(R1)                                                         
         STC   R0,0(R5)            UPDATE # OF VALUES FOR OPTION                
         ZIC   RF,OPDOLEN          RF = L(OPTION OUTPUT)                        
         SR    R0,R0                                                            
         MR    R0,RF                                                            
         LA    R0,1(R1,R5)         SET DESTINATION ADDRESS                      
         LR    R1,RF               SET DESTINATION LENGTH                       
*                                                                               
         ZICM  R5,OPDVLTB,(3)                                                   
         LA    R5,OPTDSECT(R5)     R5-->VALUE(S) FOR KEYWORD                    
         LA    RE,1(R5)            SET SOURCE ADDRESS                           
         ZIC   RF,0(R5)            SET SOURCE LENGTH                            
                                                                                
         MVCL  R0,RE               USE MVCL IN CASE L'DEST <> L'SOURCE          
                                                                                
         B     VOPT150                                                          
*                                                                               
*** OPTION VALUE VALIDATED VIA ROUTINE ***                                      
*                                                                               
VOPT080  DS    0H                                                               
         LA    R0,BLOCK                                                         
         LA    R1,480              L'BLOCK                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR  BLOCK  FOR OUTPUT FROM ROUTN          
                                                                                
         MVC   GOSUBN,OPDRTNUM                                                  
         GOTO1 AGOSUB                                                           
         BNE   VOPTXN              ROUTINE SHOULD SET ALL ERROR INFO            
*                                                                               
         ZIC   R0,OPDOLEN          R0 = OUTPUT LENGTH                           
         ZICM  R5,OPDODSPL,(3)                                                  
         LA    R5,SYSD(R5)         R5-->OUTPUT AREA FOR OPTION DATA             
         ZIC   RE,0(R5)            RE = # OF OUTPUT VALUES SO FAR               
         STH   R0,HALF                                                          
         MH    RE,HALF                                                          
         LA    RE,1(RE,R5)         RE-->DESTINATION FOR OPTION VALUE            
         LA    RF,BLOCK            RF-->OPTION VALUES RETURNED FROM RTN         
                                                                                
VOPT083  DS    0H                                                               
         LR    R1,R0               R1 = OUTPUT LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),0(RF)       ANY MORE OUTPUT?                             
         BZ    VOPT090              NOPE                                        
                                                                                
         CLC   0(1,R5),OPDMXNDV    DO WE HAVE MAX # DATA VALUE ALREADY?         
         BL    VOPT086                                                          
         MVI   OURERRCD,TMODQ       YES, TOO MANY OPTN DATA ERROR               
         B     VOPTXN                                                           
                                                                                
VOPT086  DS    0H                                                               
         EXMVC R1,0(RE),0(RF)      MOVE OPTION DATA TO STORAGE                  
         AR    RE,R0               BUMP TO NEXT STORAGE LOCATION                
         AR    RF,R0               BUMP TO NEXT DATA VALUE SOURCE               
         ZIC   R1,0(R5)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,0(R5)            UPDATE # OF DATA VALUES                      
         B     VOPT083                                                          
*                                                                               
VOPT090  DS    0H                                                               
         B     VOPT150                                                          
*                                                                               
*** OPTION VALUE VALIDATED VIA TABLE ***                                        
*                                                                               
VOPT100  DS    0H                                                               
         ZICM  R2,OPDTBDSP,(3)                                                  
         A     R2,MYBASE1          R2-->TABLE OF VALID OPTION VALUES            
*                                                                               
         SR    R1,R1                                                            
VOPT112  DS    0H                                                               
         ZICM  R0,0(R2),(1)        IF L(TABLE DATA) = 0,                        
         BNZ   VOPT114                                                          
         MVI   OURERRCD,IODVQ       EOT REACHED AND INPUT IS INVALID            
         B     VOPTXN                                                           
                                                                                
VOPT114  DS    0H                                                               
         IC    R1,1(R3)            R1 = L(INPUT DATA)                           
         CR    R0,R1               L(TABLE DATA) VS L(INPUT DATA)               
         BL    VOPT116              LOW: INSUFFICIENT FOR COMPARISON            
         BCTR  R1,0                                                             
         EXCLC R1,22(R3),2(R2)                                                  
         BE    VOPT120                                                          
                                                                                
VOPT116  DS    0H                  BUMP TO NEXT TABLE ENTRY                     
         ZIC   RF,1(R2)             RF = L(VALUE TO STORE)                      
         AR    RF,R0                R0 = L(TABLE DATA)                          
         LA    R2,2(RF,R2)          R2 BUMPED TO NEXT TABLE ENTRY               
         B     VOPT112                                                          
*                                                                               
VOPT120  DS    0H                  OPTION DATA INPUTTED IS VALID                
         LR    RF,R0                RF = L(TABLE DATA)                          
         LA    RF,2(RF,R2)          RF-->VALUE TO STORE FOR OPTION              
                                                                                
         ZICM  R5,OPDODSPL,(3)                                                  
         LA    R5,SYSD(R5)         R5-->PLACE TO STORE OPTION VALUE             
         CLC   0(1,R5),OPDMXNDV    DO WE HAVE MAX # DATA VALUE ALREADY?         
         BL    VOPT123                                                          
         MVI   OURERRCD,TMODQ                                                   
         B     VOPTXN                                                           
                                                                                
VOPT123  DS    0H                                                               
         ZIC   RE,0(R5)            RE = # OF DATA VALUES SO FAR                 
         LA    R1,1(RE)                                                         
         STC   R1,0(R5)             UPDATE IT                                   
         ZIC   R1,OPDOLEN          R1 = L(OPTION DATA VALUE)                    
         STH   R1,HALF                                                          
         MH    RE,HALF                                                          
         LA    RE,1(RE,R5)         RE-->DESTINATION FOR OPTION VALUE            
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),0(RF)                                                   
*                                                                               
         B     VOPT150                                                          
*                                                                               
** OPTION INPUTTED IS VALID **                                                  
*                                                                               
VOPT150  DS    0H                                                               
         OC    OPTI,OPDOPTB        REMEMBER THAT IT WAS INPUTTED                
                                                                                
         ZIC   R1,COUNTER          ANY MORE SCANNER ENTRIES?                    
         LA    R1,1(R1)                                                         
         CLM   R1,1,NOPTN                                                       
         BNL   VOPT200              NOPE, FINISHED W/ OPTIONS                   
         STC   R1,COUNTER                                                       
         ZIC   R1,SCANLNTH                                                      
         LA    R3,22(R3,R1)         YEP, BUMP TO NEXT SCANNER ENTRY             
         B     VOPT020                                                          
*                                                                               
VOPT200  DS    0H                                                               
         MVC   DUB(4),OPTR         TEST ALL REQUIRED OPTIONS INPUTTED           
         NC    DUB(4),OPTI                                                      
         CLC   DUB(4),OPTR                                                      
         BE    VOPT220              YEP, THEY'RE ALL HERE                       
                                                                                
         L     R4,AOPTTAB          LOCATE MISSING OPTION                        
VOPT212  DS    0H                                                               
         CLI   0(R4),EOT           IF END OF OPTIONS TABLE REACHED,             
         BNE   *+6                                                              
         DC    H'0'                 SOMETHING'S AMISS                           
         MVC   DUB(4),OPTI                                                      
         NC    DUB(4),OPTR                                                      
         XC    DUB(4),OPTR         DUB(4) CONTAINS OPTIONS MISSING              
         NC    DUB(4),OPDOPTB      IS THIS AN OPTION MISSING?                   
         BNZ   VOPT214              YES                                         
         ZIC   R0,OPDNTRYL          NO, TRY NEXT ENTRY                          
         AR    R4,R0                                                            
         B     VOPT212                                                          
                                                                                
VOPT214  DS    0H                  MOVE KEYWORD LEN & NAME INTO MYTEXT          
         ZICM  RF,OPDKYTB,(3)                                                   
         LA    RF,OPTDSECT(RF)                                                  
         ZIC   RE,0(RF)                                                         
         EXMVC RE,MYTEXT,0(RF)                                                  
         MVI   OURERRCD,ROMQ                                                    
         B     VOPTXN10                                                         
*                                                                               
VOPT220  DS    0H                                                               
         B     VOPTXY                                                           
         DROP  R4                                                               
                                                                                
                                                                                
VOPTXN   DS    0H                                                               
         MVI   MYTEXT,0            NO TEXT REPLACE W/IN ERROR MESSAGE           
                                                                                
VOPTXN10 DS    0H                                                               
         B     NO                                                               
*                                                                               
VOPTXY   DS    0H                                                               
         B     YES                                                              
                                                                                
*========================= VALIDATION ERRORS =========================*         
                                                                                
INVLFLD  DS    0H                  INVALID FIELD ERROR                          
         MVI   OURERRCD,IFLDQ                                                   
         B     OURERROR                                                         
                                                                                
MISSFLD  DS    0H                  MISSING FIELD ERROR                          
         MVI   OURERRCD,MFLDQ                                                   
         B     OURERROR                                                         
                                                                                
RCDNTFND DS    0H                  RECORD NOT FOUND                             
         MVI   OURERRCD,RNFQ                                                    
         B     OURERROR                                                         
                                                                                
***********************************************************************         
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (MISCELLANEOUS)'                
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
         MVC   ASUBRTN,ASUBR02                                                  
         CLI   GOSUBN,R02#                                                      
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR03                                                  
         CLI   GOSUBN,R03#                                                      
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR04                                                  
         CLI   GOSUBN,R04#                                                      
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
         SPACE 2                                                                
*LTORG & CONSTANTS                                                              
         LTORG                                                                  
         DROP  R7,R8,R9,RA,RB,RC                                                
         EJECT                                                                  
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
***********************************************************************         
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
         ORG   RMP19+X'2000'                                                    
         ORG                                                                    
SUBR01Q  EQU   (((*-RMP19+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP19+SUBR01Q                                                    
SUBR01   NMOD1 0,**1901**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R8             R8=A(SYSD)                                   
         USING SPOOLD,R7           R7=A(SPOOL WORK AREA)                        
                                                                                
         ZIC   R1,GOSUBN                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
                                                                                
LST#     EQU   (R01_01-*)/4+1      BUILD DEMO LIST                              
BLD#     EQU   (R01_02-*)/4+1      GOTO DEMOUT WITH DEMO LIST                   
UNV#     EQU   (R01_03-*)/4+1      GET UNIVS                                    
EDT#     EQU   (R01_04-*)/4+1      EDIT OVERIDE ELEMS FROM RECD TO LIST         
ADD#     EQU   (R01_05-*)/4+1      ADD OVR -> R6 TO RECD IN AIO                 
WRT#     EQU   (R01_06-*)/4+1      WRITE BACK RECD W/UPDATES TO FILE            
MIN#     EQU   (R01_07-*)/4+1      MIN VALUE/UPGRADE HANDLING                   
GHI#     EQU   (R01_08-*)/4+1      GET INV HEADER RECD IN AIO2                  
SDBX#    EQU   (R01_09-*)/4+1      SET UP EXTENDED DBLOCK                       
CLS#     EQU   (R01_10-*)/4+1       CLR SCREEN AND SET XMIT BITS                
DSI#     EQU   (R01_11-*)/4+1       DISP INFO OF INV RECD                       
DSD#     EQU   (R01_12-*)/4+1       DISP DEMO LIST ON SCREEN                    
TID#     EQU   (R01_13-*)/4+1       TRANSLATE INTERNAL DAY                      
TTM#     EQU   (R01_14-*)/4+1       TRANSLATE INTERNAL TIME                     
*                                                                               
R01_00   DS    0H                                                               
R01_01   B     LSTDEM              BLD DEMOLIST & CLEAR DEMOVALS                
R01_02   B     BLDDEM              GET DEMOS FROM DEMOUT                        
R01_03   B     UNVDEM              GET UNIVERSES                                
R01_04   B     EDTOVR              EDIT OVERIDES ON RECD                        
R01_05   B     ADDOVR              ADD OVR TO RECD                              
R01_06   B     WRTOVR              WRITE BACK RECD W/OVERIDES TO FILE           
R01_07   B     MINVAL              ADD MIN VALUE ELEM                           
R01_08   B     GETHDR              GET INV HDR RECD -> AIO2                     
R01_09   B     STDBXLNK            SET UP EXTENDED DBLOCK                       
R01_10   B     CLSCR               CLEAR SCREEN                                 
R01_11   B     DSPINFO             DISPLAY INFO ABOUT INV RECD                  
R01_12   B     DSPDEM              DISPLAY RECORD                               
R01_13   B     TRSLDAY             TRANSLATE DAY                                
R01_14   B     TRSLTIM             TRANSLATE TIME                               
*                                                                               
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    RC,RC                                                            
*                                                                               
NO_01    LTR   RC,RC                                                            
*                                                                               
XIT_01   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*LSTDEM  - BUILD DEMO LIST FROM DEMTAB                                          
***********************************************************************         
LSTDEM   DS    0H                                                               
         L     R3,AIO              PT TO INV RECD                               
         MVC   DATADISP,=Y(RINVPEL-REINVRCD)                                    
         MVI   ELCODE,X'5E'        ANY DEMOS ON RECD?                           
         BAS   RE,GETEL                                                         
         BNE   LST_ERRX                                                         
*                                  CLEAR DEMOLST AND DEMOVLS                    
         LA    R0,DEMOLST            R0=A(DEMOLIST)                             
         LA    R1,DEMOLSTX-DEMOLST   R1=L'AREA TO CLEAR                         
         SR    RE,RE                 RE= 0'S TO MOVE IN                         
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R0,DEMOVLS            R0=A(DEMOVALS)                             
         LA    R1,DEMOVALX-DEMOVLS   R1=L'AREA TO CLEAR                         
         SR    RE,RE                 RE= 0'S TO MOVE IN                         
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LH    R1,STDEM            SAVE STARTING PT IN TABLE                    
         MH    R1,=Y(DEMTABQ)      BUMP TO CORRECT LINE IN TBL                  
         A     R1,ADEMTAB          ADDRESS THE TABLE                            
         LA    R0,SCRLNQ           NUMBER OF LINES ON THE SCREEN                
         L     RE,ADEMHOM                                                       
         MVC   DEMOLST(L'DEMHOMES),0(RE)                                        
         LA    RE,DEMOLIST                                                      
         USING DEMTABD,R1                                                       
LSTD30   MVI   1(RE),C'R'                                                       
         MVC   2(1,RE),DEMTMALE          MALE RTG                               
         MVI   4(RE),C'T'                                                       
         MVC   5(1,RE),DEMTMALE          MALE IMP                               
         MVI   7(RE),C'R'                                                       
         MVC   8(1,RE),DEMTFEM           FEMALE RTG                             
         MVI   10(RE),C'T'                                                      
         MVC   11(1,RE),DEMTFEM          FEMALE IMP                             
         MVI   13(RE),C'R'                                                      
         MVC   14(1,RE),DEMTTOT          TOTAL RTG                              
         MVI   16(RE),C'T'                                                      
         MVC   17(1,RE),DEMTTOT          TOTAL IMP                              
         LA    R1,DEMTABQ(R1)      NEXT ENTRY IN DEMTAB                         
         LA    RE,18(RE)           NEXT LINE OF DEMOS IN DEMLIST                
         CLI   0(R1),X'FF'         END OF DEMTAB?                               
         BE    *+8                                                              
         BCT   R0,LSTD30           LOOP FOR SCRLNQ=15 DEMO LINES                
         MVI   0(RE),X'FF'         MARK END OF DEMOLIST                         
         MVI   6(RE),X'FF'         FOR EACH COLUMN  (CTGY)                      
         MVI   12(RE),X'FF'                                                     
*                                                                               
LSTDEMX  DS    0H                                                               
         B     YES_01                                                           
*                                                                               
LST_ERRX DS    0H                                                               
         B     NO_01                                                            
         DROP  R1                                                               
***********************************************************************         
*BLDDEM  - GOTO DEMOUT TO GET DEMOS IN DEMLIST, PLACE VALUES IN DEMOVAL         
***********************************************************************         
BLDDEM   DS    0H                  CLEAR DBLOCK                                 
         MVI   EDT,C'D'            DELETE ALL PREV OVR FROM RECD                
         MVI   GOSUBN,EDT#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
BLD10    LA    R6,SVOVR            ADD OVR ELEMS TO RECD                        
         USING OVRLSTD,R6                                                       
BLD15    CLI   0(R6),X'FF'         END OF LIST?                                 
         BE    BLD40                                                            
         CLI   0(R6),0                                                          
         BE    BLD30               OVR WAS DELETED IN LIST                      
         MVI   GOSUBN,ADD#         BLD OVR ELEM ->R6 & ADD TO RECD              
         GOTO1 AGOSUB                                                           
         BE    BLD30               RECD OVERFLOW (OR SOMETHING)                 
         MVI   OURINFCD,TOOVRQ     TOO MANY OVERIDES                            
         B     BLD_ERRX                                                         
*                                                                               
BLD30    LA    R6,OVRNTY(R6)       NEXT OVERIDE IN SVOVR                        
         B     BLD15               GET NEXT OVR ELEM                            
*                                                                               
BLD40    L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         LR    R0,R5                  R0=A(AREA TO CLEAR)                       
         LA    R1,DBLOCK1X-DBLOCK1    R1=L'AREA TO CLEAR                        
         SR    RE,RE               RE= 0'S TO MOVE IN                           
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     RE,AIO              PT TO INV RECD                               
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBAREC,AIO                                                       
         MVC   DBCOMFCS,ACOMFACS                                                
         LA    RE,RINVPEL-REINVRCD(RE)                                          
         ST    RE,DBAQUART                                                      
         GOTO1 DEMOUT,DMCB,(C'L',DEMOLST),DBLOCKD,DEMOVLS                       
         CLI   DBERROR,0                                                        
         BNE   BLD_ERR                                                          
         DROP  R5,R6                                                            
*                                                                               
BLD50    DS    0H                  PUT AN '*' IN DEMOLIST IF OVERRIDEN          
         LA    R1,SVOVR            OVERIDES                                     
         USING OVRLSTD,R1                                                       
BLD55    CLI   0(R1),X'FF'         DO WE HAVE ANY OVERIDES?                     
         BE    BLDDEMX             NO                                           
         LA    RE,DEMOLST          DEMO MODIFIER AND NUMBER                     
*                                                                               
BLD60    CLI   0(RE),X'FF'         END OF DEMOLIST                              
         BE    BLD70                                                            
         CLC   OVRIDEM,1(RE)       DEMOLIST DEMO = OVR DEMO TYPE                
         BE    *+12                                                             
         LA    RE,3(RE)            NO MATCH, BUMP DEMO IN DEMOLIST              
         B     BLD60                                                            
         MVI   0(RE),C'*'          '*' IN DEMOLIST TO SHOW OVERIDDEN            
*                                                                               
BLD70    LA    R1,OVRNTY(R1)       SEARCH FOR NEXT OVERIDE                      
         B     BLD55                                                            
         DROP  R1                                                               
*                                                                               
BLD_ERR  MVI   OURINFCD,NODEMQ     NO DEMOS ON RECD NO OVR ALLOWED              
BLD_ERRX DS    0H                                                               
         B     NO_01                                                            
*                                                                               
BLDDEMX  B     YES_01              DEMO VALUES IN DEMOVALS                      
         EJECT                                                                  
***********************************************************************         
*UNVDEM  - GET UNIV DEMOS FROM RECD PUT VALUES IN DEMOVAL                       
*        DON'T NEED TO LOOK UP HH-SHR & PUT UNIVS                               
***********************************************************************         
UNVDEM   DS    0H                                                               
         L     R3,AIO              PT TO INV RECD                               
         LA    R0,BUFF             R0=A(DEMOLIST) IN BUFF                       
         LA    R1,DEMOLSTX-DEMOLIST   R1=L'AREA TO CLEAR                        
         SR    RE,RE               RE= 0'S TO MOVE IN                           
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,DEMOVALS            R0=A(DEMOVALS)                            
         LA    R1,DEMOVALX-DEMOVALS   R1=L'AREA TO CLEAR                        
         SR    RE,RE               RE= 0'S TO MOVE IN                           
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LH    R1,STDEM            STARTING PT IN TABLE                         
         MH    R1,=Y(DEMTABQ)      BUMP TO CORRECT LINE IN TBL                  
         A     R1,ADEMTAB          ADDRESS THE TABLE                            
         LA    R0,SCRLNQ           NUMBER OF LINES ON THE SCREEN                
         LA    RE,BUFF                                                          
         USING DEMTABD,R1                                                       
UNV30    MVI   1(RE),C'U'                                                       
         MVC   2(1,RE),DEMTMALE    MALE                                         
         MVI   4(RE),C'U'                                                       
         MVC   5(1,RE),DEMTFEM     FEMALE                                       
         MVI   7(RE),C'U'                                                       
         MVC   8(1,RE),DEMTTOT     TOTAL                                        
         LA    R1,DEMTABQ(R1)      NEXT ENTRY IN DEMTAB                         
         LA    RE,9(RE)            NEXT LINE OF DEMOS IN DEMLIST                
         CLI   0(R1),X'FF'         END OF DEMTAB?                               
         BE    *+8                                                              
         BCT   R0,UNV30            LOOP FOR SCRLNQ=12 DEMO LINES                
         MVI   0(RE),X'FF'         MARK END OF DEMOLIST                         
         MVI   3(RE),X'FF'         FOR EACH COLUMN  (CTGY)                      
         MVI   6(RE),X'FF'                                                      
         DROP  R1                                                               
*                                                                               
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         LR    R0,R5                  R0=A(AREA TO CLEAR)                       
         LA    R1,DBLOCK1X-DBLOCK1    R1=L'AREA TO CLEAR                        
         SR    RE,RE               RE= 0'S TO MOVE IN                           
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     RE,AIO              PT TO INV RECD                               
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBAREC,AIO                                                       
         MVC   DBCOMFCS,ACOMFACS                                                
         LA    RE,RINVPEL-REINVRCD(RE)                                          
         ST    RE,DBAQUART                                                      
         GOTO1 DEMOUT,DMCB,(C'L',BUFF),DBLOCKD,DEMOVALS                         
         CLI   DBERROR,0                                                        
         BNE   UNV_ERRX                                                         
         DROP  R5                                                               
*                                                                               
UNV_ERRX MVI   OURINFCD,NODEMQ     NO DEMOS ON RECD NO OVR ALLOWED              
         B     NO_01                                                            
*                                                                               
UNVX     B     YES_01              DEMO VALUES IN DEMOVALS                      
         EJECT                                                                  
***********************************************************************         
*EDTOVR - PERFORMS EDITS TO OVERIDE ELEMS ON RECD                               
*         EDT  ='C' = COPY OVR FROM RECD IN AIO TO SVOVR LIST                   
*               'D' = DELETE ALL NEW FMT OVR FROM RECD IN AIO                   
*               'I' = DELETE ALL INDEX ELEMS (X'DE' LN=IN AIO                   
***********************************************************************         
EDTOVR   DS    0H                                                               
         USING RINVNOEL,R3                                                      
         LA    R6,SVOVR            SAVE OVER ELEM LIST                          
         CLI   EDT,C'C'            COPY FROM RECD, INIT LIST                    
         BNE   EDTOV10                                                          
         XC    SVOVR,SVOVR                                                      
         MVI   0(R6),X'FF'                                                      
*                                                                               
EDTOV10  L     R3,AIO              PT TO INV RECD                               
         MVC   DATADISP,=Y(RINVPEL-REINVRCD)                                    
         MVI   ELCODE,X'DE'        NEW DEMO FORMAT ELEMENT                      
         BAS   RE,GETEL                                                         
EDTOV15  BNE   EDTOVRX             NO (MORE) OVR ELEM(S)                        
         CLI   EDT,C'I'            REQ TO DELETE INDEX ELEMS?                   
         BNE   EDTOV15A                                                         
         CLC   2(2,R3),=X'50FF'    INDEX ELEM                                   
         BE    EDTOV30                                                          
         CLC   2(2,R3),=X'50FE'    INDEX ELEM                                   
         BE    EDTOV30                                                          
         B     EDTOV16                                                          
*                                                                               
EDTOV15A CLI   1(R3),12            NEW FMT?                                     
         BNE   EDTOV16             YES, ELSE BYPASS                             
         CLI   RINVNOTP,C'R'                                                    
         BE    EDTOV18                                                          
         CLI   RINVNOTP,C'T'                                                    
         BE    EDTOV18                                                          
         CLI   RINVNOTP,C'S'                                                    
         BE    EDTOV18                                                          
         CLI   RINVNOTP,C'P'                                                    
         BE    EDTOV18                                                          
*                                                                               
EDTOV16  BAS   RE,NEXTEL                                                        
         B     EDTOV15                                                          
*                                                                               
EDTOV18  DS    0H                                                               
         CLI   EDT,C'C'           COPY RQST?                                    
         BE    EDTOV20             YES                                          
         CLI   EDT,C'D'            DELETE REQST?                                
         BE    EDTOV30             YES                                          
*                                                                               
EDTOV20  DS    0H                  COPY ELEM ->R3  TO SVOVR LST- > R6           
         USING OVRLSTD,R6                                                       
         USING RINVNOEL,R3                                                      
         MVC   OVRID,RINVNOTP      IDENTIFIER                                   
         MVC   OVRDEM,RINVNODM     DEMO CATGY                                   
         MVC   OVRVAL,RINVNOVL      OVERIDE VALUE                               
         LA    R6,OVRNTY(R6)                                                    
         LA    R0,SVOVRLN+SVOVR    A(END OF OVR LIST)                           
         MVI   0(R6),X'FF'         END OF LIST                                  
         B     EDTOV16             GET NEXT ELEM                                
*                                                                               
EDTOV30  DS    0H                  DELETE ELEM ->R3                             
         L     RE,AIO                                                           
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,C'R'                                                      
         LR    R4,R3                                                            
         GOTO1 VRECUP,DMCB,(X'02',(RE)),(R4),,0                                 
         CLI   DMCB+8,0                                                         
         BNE   EDTOV10                                                          
*                                                                               
EDTOVNX  MVI   OURINFCD,TOOVRQ     TOO MANY OVERIDES                            
         B     NO_01                                                            
*                                                                               
EDTOVRX  B     YES_01                                                           
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*ADDOVR - ADD OVR ELEM PTD TO BY (R6) TO RECD IN AIO                            
***********************************************************************         
ADDOVR   DS    0H                                                               
         USING OVRLSTD,R6                                                       
* 8/22/00  bpoo  add impressions #                                              
*                                                                               
         L     R3,AIO              FIND A(INSERTN) IN RECD                      
         LA    R3,(RINVPEL-RINVREC)(R3)                                         
         SR    R0,R0                                                            
ADDOV05  CLI   0(R3),0             END OF RECD?                                 
         BE    ADDOV10                                                          
         CLI   0(R3),X'DE'         FIND A(INSERTION) LOCATION                   
         BNL   ADDOV10                                                          
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     ADDOV05                                                          
*                                                                               
ADDOV10  ST    R3,MYDMCB           SAVE A(INSERTION)                            
ADDOV15  CLI   0(R6),X'FF'         END OF OVR LIST?                             
         BE    ADDOVRX                                                          
         CLI   0(R6),0                                                          
         BE    ADDOVRX             OVR WAS DELETED IN LIST                      
         LA    R3,WORK                                                          
         USING RINVNOEL,R3                                                      
         XC    RINVNOEL(12),RINVNOEL                                            
         MVI   RINVNOCD,X'DE'                                                   
         MVI   RINVNOLN,12                                                      
         MVC   RINVNODM,OVRDEM                                                  
         MVC   RINVNOVL,OVRVAL                                                  
         CLI   OVRID,C'T'                                                       
         BNE   *+16                                                             
         MVI   RINVNOTP,C'T'                                                    
         MVI   RINVNOPR,X'42'      HUNDREDS FOR IMPS                            
         B     ADDOV20                                                          
*                                                                               
         CLI   OVRID,C'R'                                                       
         BNE   *+16                                                             
         MVI   RINVNOTP,C'R'                                                    
         MVI   RINVNOPR,X'81'      1 DEC FOR RTG                                
         B     ADDOV20                                                          
*                                                                               
         CLI   OVRID,C'P'                                                       
         BNE   *+16                                                             
         MVI   RINVNOTP,C'P'                                                    
         MVI   RINVNOPR,X'81'      1 DEC FOR PUTS (?)                           
         B     ADDOV20                                                          
*                                                                               
         CLI   OVRID,C'S'                                                       
         BNE   *+16                                                             
         MVI   RINVNOTP,C'S'                                                    
         MVI   RINVNOPR,X'81'      1 DEC FOR SHR                                
         B     ADDOV20                                                          
         DC    H'0'                                                             
*                                                                               
ADDOV20  L     RE,AIO                                                           
         SR    R1,R1                                                            
         ICM   R1,3,RINVLEN-RINVREC(RE) RECD LENGTH SO FAR                      
         AH    R1,=H'12'           ENOUGH ROOM FOR ANOTHER ELEM?                
         C     R1,=F'3972'         WILL ELEM FIT?                               
******   C     R1,=F'2000'         WILL ELEM FIT?                               
         BL    ADDOV25                                                          
         MVI   OURINFCD,TOOVRQ     NO, TOO MANY OVERIDES                        
         MVI   0(R6),X'FF'         MAKE THIS NEW END OF SVOVR LIST              
         B     ADDOVRNX                                                         
*                                                                               
ADDOV25  L     RE,AIO                                                           
         MVC   DMCB+8(4),MYDMCB    A(INSERTION)                                 
         MVI   DMCB+8,C'R'                                                      
         GOTO1 VRECUP,DMCB,(X'02',(RE)),WORK,,0                                 
         CLI   DMCB+8,0                                                         
         BNE   ADDOVRX                                                          
         MVI   OURINFCD,TOOVRQ     TOO MANY OVERIDES                            
         B     ADDOVRNX                                                         
*                                                                               
ADDOVRNX B     NO_01               TOO MANY OVR                                 
*                                                                               
ADDOVRX  B     YES_01                                                           
         DROP  R3,R6                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*WRTOVR - 1. READ RECD FOR UPDATE FROM FILE.                                    
*         2. DELETE ALL NEW FORMAT OVR ELEMS                                    
*         3. BUILD NEW OVRIDE ELEMS FROM SVOVR LIST                             
*         4. ADD OVR ELEMS TO RECD.                                             
***********************************************************************         
WRTOVR   DS    0H                                                               
         MVI   EDT,C'D'            REMOVE ALL NEW FMT OVR FROM RECD             
         MVI   GOSUBN,EDT#                                                      
         GOTO1 AGOSUB                                                           
         BE    WRTOV10                                                          
         MVI   OURINFCD,NODEMQ     SET TO INVALID FIELD                         
         B     WRTOVRNX                                                         
*                                                                               
WRTOV10  LA    R6,SVOVR            PT TO OVRIDE LIST                            
         USING OVRLSTD,R6                                                       
WRTOV15  CLI   0(R6),X'FF'         END OF LIST?                                 
         BE    WRTOV40                                                          
         CLI   0(R6),0                                                          
         BE    WRTOV30             OVR WAS DELETED IN LIST                      
         MVI   GOSUBN,ADD#         BLD OVR ELEM ->R6 & ADD TO RECD              
         GOTO1 AGOSUB                                                           
         BE    WRTOV30             RECD OVERFLOW (OR SOMETHING)                 
         MVI   OURINFCD,TOOVRQ     TOO MANY OVERIDES                            
         B     WRTOVRNX                                                         
*                                                                               
WRTOV30  LA    R6,OVRNTY(R6)       NEXT OVERIDE IN SVOVR                        
         B     WRTOV15             GET NEXT OVR ELEM                            
*                                                                               
WRTOV40  DS    0H                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'EF'        DELETE PREV ACTIVITY ELEM                    
         BAS   RE,GETEL                                                         
         BNE   WRTOV50                                                          
         MVC   WORK(12),0(R3)      COPY OLD ELEM INTO WORK                      
         MVI   DMCB+8,C'R'                                                      
         LR    R4,R3                                                            
         GOTO1 VRECUP,DMCB,(X'02',AIO),(R4),,0                                  
         LA    R6,WORK                                                          
         USING RINVAEL,R6                                                       
         MVI   RINVAWHY,C'C'                                                    
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
*                                                                               
WRTOV47  ST    R3,DMCB+8           A(INSERTION)                                 
         MVI   DMCB+8,C'R'                                                      
         GOTO1 VRECUP,DMCB,(X'02',AIO),WORK,,0                                  
         CLI   DMCB+8,0                                                         
         BNE   *+6                 ERROR ADDING MIN ELEM                        
         DC    H'0'                                                             
*                                                                               
WRTOV50  DS    0H                  WRITE OUT RECORD                             
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 VLTRANS             UPDATE REQ FILE EVERYTIME INV CHGS           
*                                                                               
WRTOVRX  B     YES_01                                                           
*                                                                               
WRTOVRNX B     NO_01                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*MINVAL - OPVMINTB CONTAINS NEW UPGRADE ELEM. IF RINVOP1=0, DELETES             
* ANY PREV EXISTING X'05' ELEMS W/MIN VALUE. IF ENTIRE UPGEL IS EMPTY           
* THEN THE EXISTING MIN VAL WILL BE IN EFFECT: THE RECD WONT BE CHGD            
***********************************************************************         
MINVAL   DS    0H                                                               
         OC    OPVMINTB,OPVMINTB   DO ANYTHING TO RECD?                         
         BZ    MINVALX             NO                                           
MINV05   L     R3,AIO              PT TO RECD                                   
         MVC   DATADISP,=Y(RINVPEL-REINVRCD)                                    
         MVI   ELCODE,X'05'        NEW DEMO FORMAT ELEMENT                      
         BAS   RE,GETEL                                                         
MINV10   BNE   MINV30              NO MORE UPGDS ON RECD                        
         USING RAVLNEL,R3                                                       
         CLI   RAVLNTYP,X'07'      ONLY PROCESS MIN VAL ELEM                    
         BE    MINV20                                                           
MINV15   BAS   RE,NEXTEL           NEXT UPG ELEM                                
         B     MINV10                                                           
         DROP  R3                                                               
*                                                                               
MINV20   L     RE,AIO              DELETE UPGD/MIN VAL ELEM                     
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,C'R'                                                      
         LR    R4,R3                                                            
         GOTO1 VRECUP,DMCB,(X'02',(RE)),(R4),,0                                 
         CLI   DMCB+8,0                                                         
         BNE   MINV05              SEARCH FOR ANOTHER ELEM FROM THE TOP         
         DC    H'0'                ERROR DELETING ELEM                          
*                                                                               
MINV30   DS    0H                  ADD NEW UPGRADE                              
         LA    R1,OPVMINTB         PT TO 05 ELEM                                
         USING RAVLNEL,R1                                                       
         OC    RAVLNOP1,RAVLNOP1                                                
         BZ    MINVALX                                                          
*                                                                               
         CLI   DECIMAL,C'D'                                                     
         BNE   MINV40                                                           
         LH    RF,RAVLNOP1                                                      
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         STH   RF,RAVLNOP1                                                      
         XC    DECIMAL,DECIMAL                                                  
*                                                                               
MINV40   DS    0H                  ADD NEW UPGRADE TO RECD                      
         L     R3,AIO              PT TO RECD                                   
         SR    R0,R0                                                            
         LA    R3,(RINVPEL-REINVRCD)(R3)                                        
MINV45   CLI   0(R3),X'05'         UPGRADE ELEM?                                
         BH    MINV50                                                           
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     MINV45                                                           
*                                                                               
MINV50   L     RE,AIO                                                           
         ST    R3,DMCB+8           A(INSERTION)                                 
         MVI   DMCB+8,C'R'                                                      
         GOTO1 VRECUP,DMCB,(X'02',(RE)),OPVMINTB,,0                             
         CLI   DMCB+8,0                                                         
         BNE   MINVALX             ERROR ADDING MIN ELEM                        
         MVI   OURINFCD,TOLNGQ     RECORD TOO LONG TO ADD MIN ELEM              
         B     NO_01                                                            
*                                                                               
MINVALX  B     YES_01                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*GETHDR - GET INV HEADER RECD INFO - DAY/TIMES AND PRG NAME                     
***********************************************************************         
GETHDR   DS    0H                                                               
         XC    HDRINFO(HDRINFOL),HDRINFO                                        
         GOTO1 DATVAL,DMCB,(0,DOVEFFD),WORK                                     
         ICM   R0,15,DMCB          USE 'ICM' AND NOT 'OC' SAVES 2 BYTES         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(0,WORK),(2,IKEFFDC)                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,IKEFFDB)                                 
*                                                                               
GHI009   L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         LR    R0,R5                  R0=A(AREA TO CLEAR)                       
         LA    R1,DBLOCK1X-DBLOCK1    R1=L'AREA TO CLEAR                        
         SR    RE,RE               RE= 0'S TO MOVE IN                           
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,AIO1             PT TO INV RECD                               
         MVC   DBFILE,=C'IUN'                                                   
         MVC   DBAREC,AIO2         DEMAND USES AIO2                             
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELSRC,IKRSVC                                                  
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELAGY,TWAAGY                                                  
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELSTA,IKSTTN                                                  
         MVC   DBSELBK,IKBOOK                                                   
         MVC   DBBTYPE,IKBTYPE                                                  
         MVI   DBSELMED,C'U'        MEDIA = (U)pgrade                           
         MVC   DBSELINV,IKINV       INVENTORY NUMBER                            
         MVC   DBSELDAT,IKEFFDC                                                 
*                                                                               
         DS    0H                  INVENTORY FILE SPECIFIC PARAMETERS           
         LA    RF,RINVBLK                                                       
         XC    RINVBLK,RINVBLK                                                  
         USING DBXINVWK,RF         ESTABLISH EXTENSION AREA                     
         MVC   0(4,RF),=C'RINV'    TYPE OF EXTENSION                            
         ST    RF,DBEXTEND                                                      
         MVI   DBSTYPE,X'FF'       NEW FORMAT                                   
         MVC   DBXIKRSR,IKRSVC     RATING SOURCE                                
         MVC   DBXIKQLF,IKQLFK     QUALIFIER CODE                               
         MVC   DBXIKBTP,IKBTYPE    BOKTYPE                                      
         GOTO1 DEMAND,DMCB,DBLOCKD,DEMHOOK2,0,0,0,0                             
         B     YES_01                                                           
         DROP  R5,RF                                                            
         EJECT                                                                  
*---------------------------------------------------------                      
*  DEMAND HOOK #2  - TO GET DAY/TIME/PRG NAME FROM INV HDR                      
*---------------------------------------------------------                      
* INPUT:  R5-->DBLOCK.                                                          
                                                                                
DEMHOOK2 DS    0H                                                               
DHK2     NTR1                                                                   
         USING DBLOCKD,R5                                                       
         ICM   R6,15,DBEXTEND                                                   
         BZ    DHK2X                                                            
*                                                                               
DHK2012  DS    0H                                                               
         CLC   0(4,R6),=C'RINV'                                                 
         BE    DHK2020                                                          
         ICM   R6,15,4(R6)                                                      
         BNZ   DHK2012                                                          
         B     DHK2X                                                            
*                                                                               
DHK2020  DS    0H                  R6-->EXTENSION LINK                          
         USING DBXINVWK,R6                                                      
         MVC   HDRIDAY,DBXIDAY                                                  
         MVC   HDRTIME,DBXISTIM                                                 
         MVC   HDRPRGNA,DBXIPROG                                                
         MVC   HDRFOOT,DBXIFOOT    SAVE FOOTNOTE                                
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 DEFINE,MYDMCB,=C'TRAK',DBLOCKD,WORK                              
         MVC   HDRFOOT,WORK                                                     
*                                                                               
         B     DHK2X                                                            
         DROP  R6                                                               
*                                                                               
DHK2X    DS    0H                                                               
         B     YES_01                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* STDBXLNK - SETS UP A LINK IN THE DBEXTEND AREA                                
* At entry,                                                                     
*   DUB(4)   = link identification,                                             
*   DUB+4(4) = length of link,                                                  
*   R5       = A(DBLOCK).                                                       
* At exit,                                                                      
*   FULL     = address of link, zeroes if no link set up.                       
***********************************************************************         
                                                                                
STDBXLNK DS    0H                                                               
         XC    FULL,FULL                                                        
*                                                                               
         DS    0H                  GET R2 TO START OF EXTENSION AREA            
         USING DBLOCKD,R5                                                       
         ICM   R2,15,DBEXTEND                                                   
         BNZ   *+12                                                             
         L     R2,ADBXTND                                                       
         STCM  R2,15,DBEXTEND                                                   
         LA    RF,(DBXTND1X-1)-DBXTND1(R2)  RF-->LAST BYTE IN XTND AREA         
         DROP  R5                                                               
*                                                                               
SDBX020  DS    0H                  BUMP TO APPROPRIATE LINK                     
         CR    R2,RF                                                            
         BH    SDBXX                                                            
         OC    0(4,R2),0(R2)        IF LINK IS AVAILABLE,                       
         BZ    SDBX030               IT'S APPROPRIATE                           
         CLC   0(4,R2),DUB          IF LINK'S ID MATCH,                         
         BE    SDBX040               IT'S APPROPRIATE                           
         ICM   R2,15,4(R2)                                                      
         BNZ   SDBX020                                                          
         B     SDBXX                                                            
*                                                                               
SDBX030  DS    0H                  SET UP NEW LINK                              
         LR    R1,R2                START OF LINK                               
         A     R1,DUB+4              PLUS LINK'S LENGTH                         
         BCTR  R1,0                  MINUS ONE EQUALS LINK'S END                
         CR    R1,RF                CAN LINK FIT INTO EXTENDED AREA?            
         BH    SDBXX                 NOPE                                       
                                                                                
         MVC   0(4,R2),DUB          MOVE IN LINK IDENTIFICATION                 
         LA    R1,1(R1)             R1 = A(NEXT AVAILABLE LINK)                 
         CR    R1,RF                IS IT W/IN EXTENSION AREA?                  
         BNH   *+6                                                              
         SR    R1,R1                 NOPE, END LINKAGE                          
         STCM  R1,15,4(R2)                                                      
*                                                                               
SDBX040  DS    0H                                                               
         ST    R2,FULL                                                          
         B     SDBXX                                                            
*                                                                               
SDBXX    DS    0H                                                               
         B     YES_01                                                           
         EJECT                                                                  
***********************************************************************         
*CLSCR  - CLEAR SCREEN FIELDS                                                   
***********************************************************************         
CLSCR    DS    0H                                                               
         LA    R0,DOVPFLNH           LAST FIELD ON SCREEN                       
         LA    R2,DOVHRTGH           FIRST DEMO FLD                             
         LA    RE,DOVCAT1H           DON'T ERASE COL TITLES                     
*                                                                               
CLS10    CR    R2,RE               SKIP COL TITLES                              
         BNE   *+8                                                              
         LA    R2,DOVDEMOH         PT TO 1ST DEMO LINE                          
         ZIC   R1,0(R2)              LENGTH OF FIELD + HEADER                   
*****    SH    R1,=H'09'             MINUS HEADER(8), AND 1 FOR EXMVC           
         AHI   R1,-9                 MINUS HEADER(8), AND 1 FOR EXMVC           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES        BLANK OUT FIELD                            
         OI    6(R2),X'80'           TRANSMIT                                   
*                                                                               
CLS20    ZIC   R1,0(R2)              RESTORE LENGTH                             
         AR    R2,R1                 NEXT SCREEN FIELD                          
         CR    R2,R0                 END OF SCREEN?                             
         BL    CLS10                                                            
*                                                                               
CLSCRX   B     XIT_01                                                           
         EJECT                                                                  
***********************************************************************         
*DSPINFO-  DISPLAY HEADER FLDS FOR INV RECD                                     
***********************************************************************         
*                                                                               
DSPINFO  DS    0H                  DSP SOME INFO ABOUT THIS INV RECD            
         XC    DOVCODE,DOVCODE     GET XFER CODE FROM 'CD' ELEM                 
         L     R3,AIO                                                           
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING RINVCEL,R3                                                       
         MVC   DOVCODE,RINVCODE                                                 
         OC    DOVCODE,SPACES                                                   
         OI    DOVCODEH+6,X80                                                   
*                                                                               
         XC    DOVIHDT,DOVIHDT     DISPLAY DAY/TIME OF INVENTORY HDR            
         LA    R3,DOVIHDT          FORMAT DAY & TIME INTO FIELD                 
         MVC   TMPIDAY,HDRIDAY                                                  
         MVI   GOSUBN,TID#           TRANSLATE DAY                              
         GOTO1 AGOSUB                                                           
         ZIC   R1,BYTE               R1=L(CONVERTED DAY)                        
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),WORK                                                    
         AR    R3,R1                                                            
         MVI   1(R3),C'/'                                                       
         LA    R3,2(R3)                                                         
         MVC   TMPSETM,HDRTIME                                                  
         MVI   GOSUBN,TTM#         TRANSLATE TIMES                              
         GOTO1 AGOSUB                                                           
         ZIC   R1,BYTE             R1=L(CONVERTED TIMES)                        
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),WORK                                                    
         OI    DOVIHDTH+6,X80                                                   
*                                                                               
         DS    0H                  DISPLAY PROGRAM TITLE OF INV HEADER          
         MVC   DOVIHPG,HDRPRGNA                                                 
         OI    DOVIHPGH+6,X80                                                   
         DS    0H                  DISPLAY FOOTNOTE                             
         XC    DOVIFT,DOVIFT                                                    
*        CLI   PRFFTNT,0           SUPPRESS ACCORDING TO PROFILE                
*        BNE   *+10                                                             
         MVC   DOVIFT(L'HDRFOOT),HDRFOOT                                        
         OC    DOVIFT,SPACES                                                    
         OI    DOVIFTH+6,X80                                                    
*                                                                               
DSPINFX  B     XIT_01                                                           
         EJECT                                                                  
***********************************************************************         
*DSPDEM -  DISPLAY DEMOS ON THE SCREEN FROM DEMOVAL LIST                        
***********************************************************************         
*                                                                               
DSPDEM   DS    0H                                                               
         L     R3,AIO              DISPLAY MIN VALUE IF PRESENT                 
         MVI   ELCODE,X'05'        NEW DEMO FORMAT ELEMENT                      
         BAS   RE,GETEL                                                         
DSP04    BNE   DSP08               NO UPGD ON RECD                              
         USING RAVLNEL,R3                                                       
         CLI   RAVLNTYP,X'07'      ONLY PROCESS MIN VAL ELEM                    
         BE    *+12                FOUND MIN VAL UPGD                           
         BAS   RE,NEXTEL           NEXT UPG ELEM                                
         B     DSP04                                                            
         MVC   DOVMIN(4),=C'MIN='                                               
         SR    RE,RE                                                            
         ZICM  RF,RAVLNOP1,(3)                                                  
*        BZ    *+8                                                              
*        D     RE,=F'10'                                                        
         EDIT  (RF),(3,DOVMIN+4),1,ZERO=NOBLANK,ALIGN=LEFT                      
         DROP  R3                                                               
*                                                                               
DSP08    DS    0H                                                               
         LA    R0,SCRLNQ           MAX # DEMOS ON SCREEN                        
         LA    R1,DOVDEMO          1ST SCREEN FIELD FOR DEMO NAME               
         LH    RE,STDEM            DSP TO NAME IN TABLE                         
         MH    RE,=Y(DEMTABQ)                                                   
         A     RE,ADEMTAB          ADDRESS THE TABLE                            
         USING DEMTABD,RE                                                       
*                                  FILL DEMO NAME COLUMN                        
DSPD10   MVC   0(L'DOVDEMO,R1),DEMTNAME    GET DEMO NAME                        
         CLC   DEMTCORE,SPACES     ANY CORE CELLS?                              
         BE    DSPD20                                                           
         ZIC   RF,COL                                                           
         AR    RF,RE                                                            
         MVC   DOVOVR1-DOVDEMO(1,R1),DEMTCORE-DEMTABD(RF)                       
         MVC   DOVOVI1-DOVDEMO(1,R1),DEMTCORE-DEMTABD(RF)                       
         CLI   COL+1,XFF           ANY MORE COLS DISPLAYED?                     
         BE    DSPD20                                                           
         ZIC   RF,COL+1                                                         
         AR    RF,RE                                                            
         MVC   DOVOVR2-DOVDEMO(1,R1),DEMTCORE-DEMTABD(RF)                       
         MVC   DOVOVI2-DOVDEMO(1,R1),DEMTCORE-DEMTABD(RF)                       
         CLI   COL+2,XFF           ANY MORE COLS DISPLAYED?                     
         BE    DSPD20                                                           
         ZIC   RF,COL+2                                                         
         AR    RF,RE                                                            
         MVC   DOVOVR3-DOVDEMO(1,R1),DEMTCORE-DEMTABD(RF)                       
         MVC   DOVOVI3-DOVDEMO(1,R1),DEMTCORE-DEMTABD(RF)                       
*                                                                               
DSPD20   LA    R1,DOVDEM2-DOVDEMO(R1) NEXT LINE ON SCREEN                       
         LA    RE,DEMTABQ(RE)      NEXT ENTRY IN TBL                            
         CLI   0(RE),X'FF'         EOT?                                         
         BE    *+8                                                              
         BCT   R0,DSPD10           FILL THE SCREEN                              
         DROP  RE                                                               
*                                                                               
         LA    RE,DOVRTG1H-DOVDEMOH FILL COLUMN 1 DEMOS                         
         STH   RE,MYDMCB                                                        
         MVC   MYDMCB+2(1),COL       MALE CTGY                                  
         BAS   RE,DCOL             DISPLAY 1ST COLUMN OF DEMOS                  
*                                                                               
         LA    RE,DOVRTG2H-DOVDEMOH FILL 2ND COLUMN DEMOS                       
         STH   RE,MYDMCB                                                        
         MVC   MYDMCB+2(1),COL+1   FEMALE CTGY                                  
         BAS   RE,DCOL             DISPLAY 1ST COLUMN OF DEMOS                  
*                                                                               
         LA    RE,DOVRTG3H-DOVDEMOH FILL 3RD COLUMN DEMOS                       
         STH   RE,MYDMCB                                                        
         MVC   MYDMCB+2(1),COL+2   TOTAL CTGY                                   
         BAS   RE,DCOL             DISPLAY 1ST COLUMN OF DEMOS                  
*                                                                               
         XC    MYDMCB(2),MYDMCB                                                 
         MVI   MYDMCB+2,CHUT       HOMES LINE                                   
         BAS   RE,DCOL             DISPLAY 1ST COLUMN OF DEMOS                  
*                                                                               
DSPDX    MVC   TOPDEM,STDEM                                                     
         CLI   OURINFCD,0                                                       
         BNE   *+16                                                             
         CLI   PRFOVR,0                                                         
         BE    *+8                                                              
         MVI   OURINFCD,NOOVRQ                                                  
         L     R0,ASCRNTAB                                                      
         LA    R1,SCRNTABQ                                                      
***      LA    RE,DOVDEMOH                                                      
         LA    RE,DOVHRTGH                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     XIT_01                                                           
         EJECT                                                                  
***********************************************************************         
*DCOL - DISPLAY COLUMN OF DEMOS FROM DEMOVAL                                    
*       INPUT: MYDMCB (2) = DISP TO COLUMN FROM DEMO NAME FIELD                 
*              MYDMCB+2(1) = DISP TO CATAGORY IN DEMOVAL                        
*       EXIT:  (RE)                                                             
***********************************************************************         
*                                                                               
DCOL     DS    0H                                                               
         CLI   MYDMCB+2,CHUT       DISPLAY HUT LINE?                            
         BNE   DCOL09                                                           
         LA    RF,DEMOVLS                                                       
         LA    R4,DEMOLST                                                       
         LA    R3,DOVHRTGH         1ST HOMES FLD ON SCRN                        
*                                                                               
DCOL05   DS    0H                                                               
         EDIT  (B4,0(RF)),(7,8(R3)),1,ZERO=NOBLANK    DISP HH DEMO              
         ZIC   R1,0(R3)            LENGTH OF FLD                                
         AR    R3,R1               BUMP TO OVR INDID FLD                        
         MVI   8(R3),C'c'          ALL HOMES ARE CORE                           
         CLI   0(R4),C'*'          WAS DEMO OVERRIDE PRESENT?                   
         BNE   *+8                                                              
         MVI   8(R3),C'!'          YES                                          
*                                                                               
         LA    R4,3(R4)            NEXT DEMO IN DEMOLST                         
         CLI   1(R4),C'U'          UNVS=END OF HOMES DEMO LST                   
         BE    DCOLX                                                            
         LA    RF,4(RF)            NEXT HOMES DEMO VALUE                        
         ZIC   R1,0(R3)            LENGTH OF SCREEN OVR FLD                     
         AR    R3,R1               BUMP TO NEXT UNPROTECTED HOMES FLD           
         TM    1(R3),X'20'         IS THIS A PROTECTED FLD?                     
         BNO   DCOL05              YES --WE'RE GOOD                             
         DC    H'0'                SHOULD BE UNPROT FLD?????                    
*                                                                               
DCOL09   LA    RF,DEMOVALS         RF=VALUES                                    
         ZIC   R1,MYDMCB+2         CTGY (0=MALE, 1=FEMALE, 2=TOTAL)             
         SLL   R1,3                CTGY*8 = DISP IN DEMOVAL                     
         AR    RF,R1               RF=DEMOVAL                                   
         LA    R4,DEMOLIST                                                      
         LA    R2,DOVDEMOH         R2-> 1ST LINE ON SCREEN                      
*                                                                               
DCOL10   LR    R3,R2                                                            
         AH    R3,MYDMCB           R3 -> COL POSN ON SCREEN                     
         CLI   0(R4),X'FF'         END OF DEMOLIST?                             
         BE    DCOLX               YES                                          
         ZIC   R1,MYDMCB+2         PT TO DEMLIST FOR THIS CTGY                  
         MH    R1,=H'6'                                                         
         AR    R1,R4                                                            
         CLI   2(R1),0             DEMO# NOT DEFINED                            
         BNE   DCOL20              YES                                          
         MVC   8(L'DOVRTG1,R3),=C'     . '                                      
         LA    R3,DOVIMP1H-DOVRTG1H(R3)                                         
         MVC   8(L'DOVIMP1,R3),=C'     . '                                      
         B     DCOL30                                                           
*                                                                               
DCOL20   DS    0H                                                               
         EDIT  (B4,0(RF)),(7,8(R3)),1,ZERO=NOBLANK    DISP RTG                  
         LA    R3,DOVOVR1H-DOVRTG1H(R3)                                         
         CLI   0(R1),C'*'                                                       
         BNE   DCOL25                                                           
         CLI   8(R3),C'c'                                                       
         BNE   *+12                                                             
         MVI   8(R3),C'!'          OVERIDE ON CORE CELL                         
         B     *+8                                                              
         MVI   8(R3),C'*'          INDICATE OVERIDE                             
DCOL25   LA    R3,DOVIMP1H-DOVOVR1H(R3)                                         
         EDIT  (B4,4(RF)),(7,8(R3)),1,ZERO=NOBLANK     DISP IMP                 
         LA    R3,DOVOVI1H-DOVIMP1H(R3)                                         
         ZIC   R1,MYDMCB+2                                                      
         MH    R1,=H'6'                                                         
         LA    R1,3(R1,R4)         DISP INTO DEMLIST TO IMP                     
         CLI   0(R1),C'*'                                                       
         BNE   DCOL30                                                           
         CLI   8(R3),C'c'                                                       
         BNE   *+12                                                             
         MVI   8(R3),C'!'          OVERIDE ON CORE CELL                         
         B     *+8                                                              
         MVI   8(R3),C'*'          INDICATE OVERIDE                             
*                                                                               
DCOL30   LA    RF,6*4(RF)          6- 4BYTE DEMOS TO SAME CATGY                 
         LA    R4,6*3(R4)          NEXT DEMO FOR (1ST) CATGY IN LIST            
         LA    R2,DOVDEM2H-DOVDEMOH(R2) NEXT LINE ON SCREEN                     
         LA    R0,DOVDEMXH                                                      
         CR    R2,R0               LAST DEMO ON SCREEN?                         
         BNH   DCOL10              NOT YET                                      
*                                                                               
DCOLX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*TRSLDAY -TRANSLATE INTERNAL DAY                                                
* At entry,                                                                     
*   TMPIDAY = day code to translate.                                            
* At exit,                                                                      
*   WORK    = converted day in compressed format,                               
*   BYTE    = length of converted day output.                                   
***********************************************************************         
TRSLDAY  DS    0H                                                               
         MVC   WORK,SPACES                                                      
         XC    DUB,DUB                                                          
         MVC   DUB(L'TMPIDAY),TMPIDAY                                           
         CLI   DUB,0              << TEMPFIX IN ROVER  >>                       
         BNE   *+12                                                             
         MVI   BYTE,1                                                           
         B     TIDX                                                             
         GOTO1 UNDAY,DMCB,DUB,WORK                                              
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
         B     TIDX                                                             
*                                                                               
TIDX     DS    0H                                                               
         B     YES_01                                                           
                                                                                
***********************************************************************         
*TRSLTIM - TRANSLATE MILITARY TIME                                              
* At entry,                                                                     
*   TMPSETM = time(s) to translate.                                             
* At exit,                                                                      
*   WORK    = converted time in AM/PM mode,                                     
*   BYTE    = length of converted time.                                         
***********************************************************************         
TRSLTIM  DS    0H                                                               
         MVC   WORK,SPACES                                                      
         XC    DUB,DUB                                                          
         MVC   DUB(L'TMPSETM),TMPSETM                                           
         GOTO1 UNTIME,DMCB,DUB,WORK                                             
         DS    0H                  DETERMINE LENGTH OF OUTPUT                   
         CLI   WORK,0              << TEMPFIX IN ROVER >>                       
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
         B     YES_01                                                           
         EJECT                                                                  
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SUBR01--MISC STUFF'            
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
         DS    0H                                                               
         GETEL R3,DATADISP,ELCODE                                               
                                                                                
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SUBR02)'                       
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
SUBR02Q  EQU   (((*-RMP19+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP19+SUBR02Q                                                    
SUBR02   NMOD1 0,**1902**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R8             R8=A(SYSD)                                   
         USING SPOOLD,R7           R7=A(SPOOL WORK AREA)                        
                                                                                
         ZIC   R1,GOSUBN                                                        
         SH    R1,=Y(R01#)         SUBTRACT FOR SUB-RTN # 2                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R02_00(R1)                                                       
                                                                                
MGF#     EQU   (R02_01-*)/4+1+R01#  MERGE TWA FIELDS                            
IOV#     EQU   (R02_02-*)/4+1+R01#  INITIALIZE OPTION VALUES                    
VUPG#    EQU   (R02_03-*)/4+1+R01#  VALIDATE UPGRADE EXPRESSION                 
VMIN#    EQU   (R02_04-*)/4+1+R01#  VALIDATE MINIMUM EXPRESSION                 
VDEM#    EQU   (R02_05-*)/4+1+R01#  VALIDATE DEMOS                              
*                                                                               
                                                                                
R02_00   DS    0H                                                               
R02_01   B     MERGFLDS             MERGE TWA FIELDS                            
R02_02   B     INITOPTV             INITIALIZE OPTION VALUES                    
R02_03   B     VALUPGD              VALIDATE UPGRADE EXPRESSION                 
R02_04   B     VALMIN               VALIDATE MINIMUM EXPRESSION                 
R02_05   B     VALDEM               VALIDATE DEMOS                              
*                                                                               
R02#     EQU   ((*-R02_00)/4+1)+R01#                                            
         DC    H'0'                                                             
                                                                                
YES_02   SR    RC,RC                                                            
*                                                                               
NO_02    LTR   RC,RC                                                            
*                                                                               
XIT_02   XIT1                                                                   
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SUBR02--MGF#)'                 
*-------------------------- MERGE TWA FIELDS -------------------------*         
                                                                                
* Merges those TWA fields specified in a table to form a single field.          
*  The resulting field will not have an extended field header.                  
* At entry,                                                                     
*   R3-->table of TWA fields.                                                   
* At exit,                                                                      
*   BLOCK contains the combined field.                                          
                                                                                
MERGFLDS DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
*                                                                               
         DS    0H                  INITIALIZE FIELD HEADER                      
         XC    DUB,DUB                                                          
         MVI   DUB+4,X'0F'-X01      ASSUME THESE INPUT INDICATORS ON            
                                                                                
         SR    R4,R4               USE R4 FOR LENGTH OF NEW FIELD               
*                                                                               
MGF20    DS    0H                                                               
         ZICM  RE,0(R3),(3)        GET DISPL OF FIELD HEADER                    
         BZ    MGF50                NULLS MEAN END OF TABLE                     
         A     RE,ATWA                                                          
                                                                                
         DS    0H                  SET INPUT INDICATORS                         
         MVC   BYTE,4(RE)                                                       
         NI    BYTE,X'F0'           TURN THESE                                  
         OC    DUB+4(1),BYTE         ON IN HEADER OF MERGED FIELD               
         MVC   BYTE,4(RE)                                                       
         OI    BYTE,X'F0'                                                       
         NC    DUB+4(1),BYTE         OFF IN HEADER OF MERGED FIELD              
                                                                                
         DS    0H                  APPEND DATA TO NEW FIELD                     
         ZICM  R1,5(RE),(1)                                                     
         BZ    MGF40                                                            
         LA    RF,BLOCK+8(R4)       RF-->LOCATION TO APPEND NEW DATA            
         AR    R4,R1                UPDATE L(NEW FIELD)                         
         CH    R4,=Y(255-8)          IF GREATER THAN MAX ALLOWED,               
         BNH   *+6                                                              
         DC    H'0'                   THEN WE SHOULDN'T GO ON                   
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),8(RE)        ELSE, APPEND DATA ONTO NEW FIELD           
*                                                                               
MGF40    DS    0H                                                               
         LA    R3,2(R3)            BUMP TO NEXT FLD HEADER IN TABLE             
         B     MGF20                                                            
*                                                                               
MGF50    DS    0H                  SET NEW FIELD HEADER                         
         MVC   BLOCK(8),DUB                                                     
         STC   R4,BLOCK+5                                                       
         LA    R4,8(R4)                                                         
         STC   R4,BLOCK                                                         
*                                                                               
         B     XIT_02                                                           
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SUBR02--IOV#)'                 
*-------------------------- MERGE TWA FIELDS -------------------------*         
                                                                                
INITOPTV DS    0H                                                               
         MVI   NOPTN,0             CLEAR # OF OPTIONS                           
         XC    OPTI,OPTI           CLEAR OPTIONS INPUTTED                       
                                                                                
         LA    R0,OPTVALS                                                       
         LA    R1,OPTVALSQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         B     XIT_02                                                           
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SUBR02--VUPG#)'                
*-------------------- VALIDATE UPGRADE EXPRESSION --------------------*         
                                                                                
* Validates an upgrade expression specified in the options field.               
* At entry,                                                                     
*   R3-->SCANNER entry w/ upgrade expression in 2nd half,                       
*   R4-->entry of option keyword for upgrade.                                   
* On exit when upgrade expression valid,                                        
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when upgrade expression invalid,                                      
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALUPGD  DS    0H                                                               
*                                                                               
         LA    R0,BLOCK                                                         
         LA    R1,480              L'BLOCK                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR  BLOCK  FOR OUTPUT                     
                                                                                
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         DS    0H                  BUILD DUMMY TWA FIELD IN WORK                
         XC    WORK,WORK                                                        
         ZIC   R0,1(R3)                                                         
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,WORK+8,22(R3)     FIELD DATA                                  
         CLC   =C'MIN',WORK+8      ONLY UPT=MIN ALLOWED IN DOVER                
         BE    *+12                                                             
         MVI   OURERRCD,OPTNAQ                                                  
         B     VUPGXN                                                           
         XC    WORK(8),WORK                                                     
         STC   R0,WORK+5                                                        
         LA    R1,8+1(R1)                                                       
         STC   R1,WORK              FIELD HEADER                                
                                                                                
         GOTO1 UPVAL,DMCB,WORK,BLOCK,(C'/',ACOMFACS)                            
         CLI   0(R1),0                                                          
         BNE   VUPGXY                                                           
         MVI   OURERRCD,IUPGQ                                                   
         B     VUPGXN                                                           
*                                                                               
VUPGXY   DS    0H                                                               
         B     YES_02                                                           
*                                                                               
VUPGXN   DS    0H                                                               
         B     NO_02                                                            
**********************************************************************          
*VALMIN - KEYWORD IS MIN                                                        
*   R3-->SCANNER entry w/ upgrade expression in 2nd half,                       
*   R4-->entry of option keyword for upgrade.                                   
**********************************************************************          
*                                                                               
VALMIN   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         TM    3(R3),X80           NUMERIC INPUT?                               
         BO    VALMIN20            INVALID                                      
*        CLI   3(R3),0             SCANNER CAN'T READ DECIMAL                   
*        BE    VALMIN1                                                          
*        MVI   OURERRCD,IODVQ                                                   
*        B     VALMINXN                                                         
*                                                                               
         MVI   OURERRCD,IODVQ      VALIDATE POSSIBLE DECIMAL 2ND FLD            
         MVI   BYTE,2                                                           
         LA    RF,22(R3)           POINT TO 2ND FIELD                           
         MVC   TEMPMIN,0(RF)                                                    
*                                                                               
VALMIN1  CLI   0(RF),C'.'                                                       
         BNE   *+18                                                             
         CLC   1(1,R3),BYTE        LENGTH OF 2ND FIELD                          
         BNE   VALMINXN                                                         
         B     VALMIN5                                                          
*                                                                               
         ZIC   R1,BYTE             BY POSITION OF DECI PT. LENGTH CAN           
         LA    R1,1(R1)            ONLY BE 2 OR 3 BYTES                         
         STC   R1,BYTE                                                          
         CLI   BYTE,4                                                           
         BE    VALMINXN                                                         
*                                                                               
         CLI   0(RF),C'0'          NUMERIC?                                     
         BL    VALMINXN                                                         
         CLI   0(RF),C'9'                                                       
         BH    VALMINXN                                                         
         LA    RF,1(RF)                                                         
         B     VALMIN1             GO ON TO NEXT FIELD.                         
*                                                                               
VALMIN5  CLI   1(RF),C'0'                                                       
         BL    VALMINXN                                                         
         CLI   1(RF),C'9'                                                       
         BH    VALMINXN                                                         
*                                                                               
         MVC   0(1,RF),1(RF)       TWEAKING                                     
         MVI   1(RF),0                                                          
         ZIC   RF,1(R3)            6.5 -> 65                                    
         SHI   RF,1                2ND FIELD LENGTH - 1                         
         STC   RF,1(R3)                                                         
         OI    3(R3),X'A0'         TURN ON NUMBER/ALPHA VALID BIT               
         MVI   DECIMAL,C'D'        FLAG IT                                      
         MVI   OURERRCD,0                                                       
         B     VALMIN30                                                         
*                                                                               
VALMIN20 SR    RF,RF                                                            
         ICM   RF,7,9(R3)                                                       
         C     RF,=F'10'           MIN HAS TO BE < 100                          
         BL    *+12                                                             
         MVI   OURERRCD,IODVQ                                                   
         B     VALMINXN                                                         
                                                                                
VALMIN30 LA    R0,BLOCK                                                         
         LA    R1,480              L'BLOCK                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR  BLOCK  FOR OUTPUT                     
                                                                                
         DS    0H                  BUILD DUMMY TWA FIELD IN WORK                
         ZIC   R0,1(R3)                                                         
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         MVC   WORK+8(4),=C'MIN/'                                               
         EXMVC R1,WORK+12,22(R3)   FIELD DATA                                   
         XC    WORK(8),WORK                                                     
         AH    R0,=H'4'                                                         
         STC   R0,WORK+5                                                        
         LA    R1,8+1+4(R1)                                                     
         STC   R1,WORK             FIELD HEADER                                 
*                                                                               
         CLI   DECIMAL,C'D'        DECIMAL?                                     
         BE    VALMIN40                                                         
         GOTO1 UPVAL,DMCB,WORK,BLOCK,(C'/',ACOMFACS)                            
         B     VALMIN50                                                         
VALMIN40 GOTO1 UPVAL,DMCB,WORK,(C'Y',BLOCK),(C'/',ACOMFACS)                     
*                                                                               
VALMIN50 CLI   0(R1),0                                                          
         BNE   VALMINXY                                                         
         MVI   OURERRCD,IUPGQ                                                   
         B     VALMINXN                                                         
*                                                                               
VALMINXY DS    0H                                                               
         B     YES_02                                                           
VALMINXN DS    0H                                                               
         B     NO_02                                                            
         EJECT                                                                  
***********************************************************************         
*VALDEM - VALIDATE DEMO (MALE,FEMALE OR TOTAL = M/F/T)                          
***********************************************************************         
VALDEM   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
*        CLC   22(4,R3),=C'CORE'                                                
*        BNE   VALD01                                                           
*        XC    ELEM,ELEM                                                        
*        MVC   ELEM(COREQ),CORES                                                
*        OC    ELEM,SPACES                                                      
*        MVI   1(R3),COREQ                                                      
*        MVI   COREFLAG,1          CORE OPTION ACTIVATED                        
*        B     VALD05                                                           
*                                                                               
VALD01   XC    ELEM,ELEM                                                        
         ZIC   R1,1(R3)            SET UP A CARD FOR SCANNER                    
         BCTR  R1,0                                                             
         EXMVC R1,ELEM,22(R3)                                                   
         OC    ELEM,SPACES                                                      
*                                                                               
                                                                                
VALD05   MVI   OURERRCD,IFLDQ                                                   
         MVI   SCANLNTH,70                                                      
         MVC   DMCB+8(4),=C',=/='                                               
         LA    R4,BUFF                                                          
         AH    R4,=H'1000'                                                      
         ST    R4,DMCB+4                                                        
         MVI   DMCB+4,X'8A'                                                     
         GOTO1 SCANNER,DMCB,(C'C',ELEM)                                         
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         BZ    VALD_NX             ERROR                                        
         XC    DUB,DUB             DUB=ENTRIES PROCESSED COUNTER                
         STC   R0,DUB+1            TOTAL NUMBER OF ENTRIES IN BUFF              
         LA    R4,BUFF             R4-->MY SCANNER BLOCK                        
         AH    R4,=H'1000'                                                      
*                                                                               
VALD10   XC    ELEM,ELEM                                                        
         MVI   OURERRCD,INVDQ      INVALID DEMO EXPRESSION                      
         SR    R1,R1                                                            
         CLI   0(R4),10            MAX # CHARS FOR DEMO NAME                    
         BH    VALD_NX                                                          
         CLI   0(R4),2             AT LEAST 2 CHARS                             
         BL    VALD_NX                                                          
         IC    R1,0(R4)            L'FIELD                                      
         BCTR  R1,0                                                             
         EXMVC R1,ELEM,12(R4)                                                   
         OC    ELEM,SPACES                                                      
         MVC   FLDDSPL,4(R4)       DISP TO THIS FIELD                           
*                                                                               
         CLC   =C'HOMES',ELEM      DON'T DEL 1ST CHAR FOR THESE DEMS:           
         BE    VALD14                                                           
         CLC   =C'WWRK',ELEM                                                    
         BE    VALD14                                                           
         CLC   =C'MET-A',ELEM                                                   
         BE    VALD14                                                           
         CLC   =C'MET-B',ELEM                                                   
         BE    VALD14                                                           
*                                                                               
         CLC   =C'META ',ELEM      MANUALLY INSERT '-' FOR THESE:               
         BNE   *+14                                                             
         MVC   ELEM(5),=C'MET-A'                                                
         B     VALD14                                                           
         CLC   =C'METB ',ELEM                                                   
         BNE   *+14                                                             
         MVC   ELEM(5),=C'MET-B'                                                
         B     VALD14                                                           
*                                  DE-SEX DEMO NAME IF INPUT EG. M/F/A          
         MVC   ELEM,SPACES                                                      
         ZIC   R1,0(R4)            R1 -> L'FIELD                                
         LA    RF,12(R4)           RF -> DEMO NAME                              
         LR    R0,R1                                                            
         TM    0(RF),X'F0'         IF IT'S A NUMBER, WE'RE OKAY                 
         BO    *+10                                                             
         BCTR  R1,0                DECR LENGTH BY 1                             
         LA    RF,1(RF)            BUMP TO NEXT FLD OF DEMO NAME                
         BCT   R0,*-14             DELETE ANY # LETTERS SEX PREFIX              
         CH    R1,=H'2'            MIN LENGHT OF INPUT = 2                      
         BL    VALD_NX                                                          
*                                  IF NO '-' IN FLD, INSERT IT                  
         LR    RE,RF               SAVE RF (1ST CHAR OF DEMO NAME)              
         LR    R0,R1               SAVE R1 (L'FLD)                              
         CLI   0(RE),C'-'                                                       
         BE    VALD11              DASH FOUND                                   
         LA    RE,1(RE)                                                         
         BCT   R0,*-12             CHECK ALL FIELDS                             
         B     VALD12                                                           
*                                                                               
VALD11   BCTR  R1,0                                                             
         EXMVC R1,ELEM,0(RF)                                                    
         B     VALD14                                                           
*                                                                               
VALD12   CH    R1,=H'3'            IF 3BYTE FLD, SEE IF LST CHAR= '+'           
         BNE   VALD13                                                           
         CLI   2(RF),C'+'          IF LAST CHAR '+' DON'T INSERT '-'            
         BE    VALD11                                                           
         MVC   ELEM(1),0(RF)                                                    
         MVI   ELEM+1,C'-'                                                      
         MVC   ELEM+2(2),1(RF)                                                  
         B     VALD14                                                           
*                                                                               
VALD13   CH    R1,=H'4'                                                         
         BNE   VALD11              JUST MOVE IN FLD W/OUT DASH                  
         MVC   ELEM(2),0(RF)       MAKE XX - XX                                 
         MVI   ELEM+2,C'-'                                                      
         MVC   ELEM+3(2),2(RF)                                                  
*                                                                               
VALD14   L     RE,AORGDEM                                                       
         USING DEMTABD,RE                                                       
VALD15   CLI   DEMTENTY,X'FF'      END OF TABLE?                                
         BE    VALD_NX             INVALID DEMO NAME                            
         CLC   DEMTNAME,ELEM       MATCH ON ENTRY?                              
         BE    VALD20                                                           
         LA    RE,DEMTABQ(RE)      NEXT ENTRY IN TABLE                          
         B     VALD15                                                           
*                                                                               
VALD20   DS    0H                                                               
         ZIC   R0,DUB              # ENTRIES IN BLOCK SO FAR                    
         LA    RF,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    VALD25                                                           
         CLC   DEMTNAME,0(RF)                                                   
         BE    VALD_NX             DUPLICATE DEMO REQSTD                        
         LA    RF,L'OPVDEMTB(RF)                                                
         BCT   R0,*-14                                                          
VALD25   MVC   0(L'OPVDEMTB,RF),DEMTNAME                                        
         DROP  RE                                                               
         ZIC   R1,DUB              BUMP TO NEXT SCANNER ENTRY                   
         LA    R1,1(R1)                                                         
         CLM   R1,1,DUB+1                                                       
         BNL   VALD_YX             DONE W/LIST                                  
         STC   R1,DUB              SAVE NEW COUNTER                             
         LA    R4,32(R4)           NEXT ENTRY                                   
         B     VALD10                                                           
*                                                                               
VALD_NX  DS    0H                                                               
         ZIC   R1,8(R3)            DISP IN ORIG SCAN BLK TO DEMO LINE           
         ZIC   R0,FLDDSPL                                                       
         AR    R1,R0                                                            
         STC   R1,FLDDSPL                                                       
         B     NO_02                                                            
*                                                                               
VALD_YX  DS    0H                                                               
         MVI   OURERRCD,0          GOOD FIELD                                   
         B     YES_02                                                           
*                                                                               
*--------------- SUBR02 ->  LTORG & CONSTANTS  -----------------------*         
         LTORG                                                                  
         DS    0H                                                               
         GETEL2 R3,DATADISP,ELCODE                                              
*                                                                               
*ORES    DC    C'2-5/6-11/12-17/18-20/21-24/25-34/35-49/50-54/55-64/'           
*        DC    C'65+'                                                           
*OREQ    EQU   *-CORES                                                          
*                                                                               
CORETAB  DS    0H                                                               
*        DC    C'                                                               
*                                                                               
SUBR02L  EQU   *-SUBR02                                                         
         DS    0CL(X'1000'-SUBR02L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SUBR03)'                       
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
SUBR03Q  EQU   (((*-RMP19+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP19+SUBR03Q                                                    
SUBR03   NMOD1 0,**1903**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R8             R8=A(SYSD)                                   
         USING SPOOLD,R7           R7=A(SPOOL WORK AREA)                        
                                                                                
         ZIC   R1,GOSUBN                                                        
         SH    R1,=Y(R02#)         SUBTRACT FOR SUB-RTN # 2                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R03_00(R1)                                                       
                                                                                
BIK#     EQU   (R03_01-*)/4+1+R02#  BUILD KEY TO INVENTORY RECORD               
GIR#     EQU   (R03_02-*)/4+1+R02#  GET INVENTORY RECORD                        
CLA#     EQU   (R03_03-*)/4+1+R02#  CLEAR (SYSSPARE) STORAGE AREAS              
*TI#     EQU   (R03_04-*)/4+1+R02#  SAVE (TABLES IN) TIA AREA                   
*TI#     EQU   (R03_05-*)/4+1+R02#  RESTORE (TABLES IN) TIA AREA                
VSC#     EQU   (R03_06-*)/4+1+R02#  VALIDATE SCREEN                             
VCO#     EQU   (R03_07-*)/4+1+R02#  VALIDATE COLUMN OF DEMOS                    
BLK#     EQU   (R03_08-*)/4+1+R02#  PROCESS BLANKED OUT FIELDS                  
HRZ#     EQU   (R03_09-*)/4+1+R02#  DO HORIZ RIPPLING                           
PRF#     EQU   (R03_10-*)/4+1+R02#  IF PROFILES APPLICABLE, SET FLAGS           
*                                                                               
R03_00   DS    0H                                                               
R03_01   B     BINVKEY             BUILD KEY TO INVENTORY RECORD                
R03_02   B     GINVRCD             GET INVENTORY RECORD                         
R03_03   B     CLRAREAS            CLEAR (SYSSPARE) STORAGE AREAS               
*03_04   B     SAVETIA             SAVE (TABLES IN) TIA AREA                    
*03_05   B     RSTRTIA             RESTORE (TABLES IN) TIA AREA                 
R03_06   B     VSCRN               VALIDATE OVR ON SCREEN- UPDATE SVOVR         
R03_07   B     VCOL                VALIDATE COLUMN ON SCREEN                    
R03_08   B     BLNK                PROCESS BLANKED OUT FIELDS                   
R03_09   B     HORIZ               HORIZONTAL RIPPLING                          
R03_10   B     PROFLS              SET ACTION FLAGS IF PROFILES APPLY           
                                                                                
R03#     EQU   ((*-R03_00)/4+1)+R02#                                            
         DC    H'0'                                                             
                                                                                
YES_03   SR    RC,RC                                                            
*                                                                               
NO_03    LTR   RC,RC                                                            
*                                                                               
XIT_03   XIT1                                                                   
                                                                                
***********************************************************************         
*BINVKEY  - BUILD INV KEY                                                       
***********************************************************************         
BINVKEY  DS    0H                                                               
         XC    KEY,KEY                                                          
IVK      USING REINVRCD,KEY                                                     
         MVI   IVK.RINVKTYP,RINVKTYQ                                            
         MVC   IVK.RINVKREP,AGENCY                                              
         MVC   IVK.RINVKSTA,IKSTTN                                              
         MVC   IVK.RINVKINV,IKINV                                               
         MVC   IVK.RINVKSTD,IKEFFDB                                             
         MVC   IVK.RINVKRSR,IKRSVC                                              
         MVC   IVK.RINVKQLF,IKQLFK                                              
         MVC   IVK.RINVKBTP,IKBTYPE                                             
         MVC   IVK.RINVKBK,IKBOOK                                               
         DROP  IVK                                                              
         B     XIT_03                                                           
         EJECT                                                                  
***********************************************************************         
*GINVRCD  - GET AN INV RECD   - (code is from rermp18)                          
* Gets an inventory record.  An effective date may or may not be                
*  supplied.  If it is given, routine will look for the record with the         
*  exact or next effective date as the given one.  If it is not given,          
*  routine will just find the record with the latest effective date.            
* At exit,                                                                      
*  CC equal  ==> record was found                                               
*  CC nequal ==> "record not found" situation.  OURERRCD may                    
*                 or may not be set                                             
*  SAVEKEY = key of inventory record attempted to get                           
         PRINT OFF                                                              
* Gets an inventory record.  An effective date may or may not be                
*  supplied.  If it is given, routine will look for the record with the         
*  latest effective date not after the given one.  If it is not given,          
*  routine will just look for the record with the latest effective              
*  date.  In either case, the rest of the key must match the given              
*  input.  Otherwise, "record not found" situation is returned.                 
         PRINT ON                                                               
                                                                                
GINVRCD  DS    0H                                                               
         MVI   OURERRCD,0                                                       
         DS    0H                                                               
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
                                                                                
*                                                                               
** MATCH ON STATION, INVENTORY # **                                             
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,BIK#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
         ZICM  R0,IKEFFDB,(3)                                                   
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STCM  R0,3,RINVKSTD       (FORCE IN A NON-ZERO DATE)                   
         XC    RINVKRTP,RINVKRTP                                                
         XC    RINVKBK,RINVKBK                                                  
         GOTO1 HIGH                                                             
         CLC   RINVKEY(IKYINVL),KEYSAVE                                         
         BE    *+12                                                             
         MVI   OURERRCD,NHDRQ       NO INVENTORY HEADER                         
         B     GIRXN                 EXIT W/ ERROR                              
                                                                                
*                                                                               
** MATCH ON STATION, INVENTORY #, EFF DATE **                                   
*                                                                               
         DS    0H                  KEY HAS CORRECT STTN & INV#                  
         MVC   SAVEKEY,RINVKEY     HOLD ONTO THE KEY                            
*                                                                               
         OC    IKEFFDB,IKEFFDB     IF EFFECTIVE DATE GIVEN,                     
         BZ    *+10                                                             
         MVC   RINVKSTD,IKEFFDB     MOVE IT INTO KEY                            
*                                                                               
         DS    0H                  CHECK FOR MATCHING EFFECTIVE DATE            
         XC    RINVKRTP,RINVKRTP                                                
         XC    RINVKBK,RINVKBK                                                  
                                                                                
*                                                                               
GIR054   DS    0H                  CHECK NXT REC FOR EQL OR LATER EFFDT         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(IKYINVL),KEYSAVE                                             
         BNH   *+14                                                             
         MVC   RINVKEY,SAVEKEY                                                  
         B     GIR059                                                           
                                                                                
         OC    IKEFFDB,IKEFFDB                                                  
         BZ    *+14                                                             
         CLC   RINVKSTD,IKEFFDB                                                 
         BNL   GIR059                                                           
                                                                                
         MVC   SAVEKEY,RINVKEY                                                  
         MVC   RINVKRTP,=X'FFFFFF'                                              
         MVC   RINVKBK,=X'FFFF'                                                 
         B     GIR054                                                           
GIR059   EQU   *                                                                
*                                                                               
         CLC   RINVKSTD,IKEFFDB     MAKE SURE EFF DATE IN KEY                   
         BNL   *+12                  IS NOT LOWER THAN THE ONE GIVEN            
         MVI   OURERRCD,NHDRQ                                                   
         B     GIRXN                                                            
                                                                                
*                                                                               
** MATCH ON ENTIRE INVENTORY KEY **                                             
*                                                                               
         DS    0H                  KEY HAS CORRECT STTN, INV#, & EFFDT          
         MVC   RINVKRSR,IKRSVC                                                  
         MVC   RINVKQLF,IKQLFK                                                  
         MVC   RINVKBTP,IKBTYPE                                                 
         MVC   RINVKBK,IKBOOK                                                   
                                                                                
         MVC   SAVEKEY,RINVKEY                                                  
                                                                                
*                                                                               
         DS    0H                  READ HIGH FOR RECORD                         
         MVC   RDUPDATE,MYRDUPDT                                                
         TM    MISCFLG1,MF1RDDEL                                                
         BZ    *+8                                                              
         OI    DMINBTS,X08                                                      
         GOTO1 HIGH                                                             
         NI    DMINBTS,XFF-X08                                                  
         CLC   RINVKEY,KEYSAVE                                                  
         BNE   GIRXN               EXIT W/O SETTING ERROR CODE                  
                                                                                
*                                                                               
         DS    0H                  GO AND GET THE RECORD                        
         TM    MISCFLG1,MF1RDDEL                                                
         BZ    *+8                                                              
         OI    DMINBTS,X08                                                      
         GOTO1 GETREC                                                           
         NI    DMINBTS,XFF-X08                                                  
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
         MVC   RINVKRSR,IKRSVC                                                  
         MVC   RINVKQLF,IKQLFK                                                  
         MVC   RINVKBTP,IKBTYPE                                                 
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
         CLC   RINVKRSR,IKRSVC                                                  
         BH    GIR010                                                           
         BL    GIR025                                                           
         CLC   RINVKQLF,IKQLFK     QUALIFIER                                    
         BH    GIR010                                                           
         BL    GIR025                                                           
         CLC   RINVKBTP,IKBTYPE    BOOK TYPE                                    
         BH    GIR010                                                           
         BE    GIR030                                                           
                                                                                
GIR025   DS    0H                   RINVKRTP < IKRTP                            
         MVC   RINVKRSR,IKRSVC                                                  
         MVC   RINVKQLF,IKQLFK                                                  
         MVC   RINVKBTP,IKBTYPE                                                 
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
*&&                                                                             
         PRINT ON                                                               
                                                                                
         B     GIRXY                                                            
         DROP  R6                                                               
                                                                                
                                                                                
GIRXN    DS    0H                                                               
         B     NO_03                                                            
GIRXY    DS    0H                                                               
         B     YES_03                                                           
         TITLE 'RERMP18 - INVENTORY ROAM && OVERRIDE (SUBR01--XFD#)'            
***********************************************************************         
*CLRAREAS - CLEAR STORAGE AREAS                                                 
***********************************************************************         
*                                                                               
CLRAREAS DS    0H                                                               
         LA    RF,TABCLRQ          RF = COUNTER                                 
         LH    R4,=Y(TABCLR-RMP19)                                              
         A     R4,MYBASE1                                                       
                                                                                
CLA10    DS    0H                  SET FULL TO BASE OF STORAGE AREA             
         MVC   FULL,ATIA                                                        
         CLI   0(R4),C'I'                                                       
         BE    CLA20                                                            
         MVC   FULL,ASYSD                                                       
         CLI   0(R4),C'S'                                                       
         BE    CLA20                                                            
         DC    H'0'                                                             
CLA20    DS    0H                                                               
         ZICM  R0,1(R4),(3)                                                     
         A     R0,FULL             R0-->AREA TO CLEAR                           
         ZICM  R1,3(R4),(3)        R1=LENGTH TO CLEAR                           
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R0,R2               GO CLEAR AREA                                
         LA    R4,L'TABCLR(R4)                                                  
         BCT   RF,CLA10                                                         
                                                                                
         MVI   NLTNTRY,0           NO LIST TABLE ENTRIES YET                    
*                                                                               
CLAX     DS    0H                                                               
         B     XIT_03                                                           
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
*SAVETIA AND RSTRTIA - COPY IN AND OUT SAVED AREAS FROM PG2                     
***********************************************************************         
                                                                                
* Saves TIA tables into TEMPSTR                                                 
                                                                                
SAVETIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ                                                     
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=Y(TIASVLEN)                                          
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ATIA,0                       
         B     XIT_03                                                           
         SPACE 3                                                                
                                                                                
***********************************************************************         
*RSTRTIA -  Restores TIA tables from TEMPSTR                                    
***********************************************************************         
                                                                                
RSTRTIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ        3RD PARAMETER                                
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=Y(TIASVLEN)                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ATIA,0                      
         B     XIT_03                                                           
         EJECT                                                                  
                                                                                
*&&                                                                             
***********************************************************************         
*VSCRN  -  GO DOWN EACH COLUMN OF SCREEN VALIDATE INPUT AND ADD OVR             
*        TO SVOVR LIST.                                                         
*        EXIT - DMCB=A(FLD W/ERROR)                                             
***********************************************************************         
*                                                                               
VSCRN    DS    0H                  GO DOWN A COLUMN AT A TIME                   
         MVI   GOSUBN,BLK#         PROCESS BLANKED OUT FIELDS                   
         GOTO1 AGOSUB                                                           
         BE    *+14                                                             
         MVC   DMCB(4),MYDMCB+4    MOVE A(ERROR) INTO DMCB P1                   
         B     VSC40                                                            
*                                                                               
         MVI   GOSUBN,HRZ#         IF REQSTED, DO HORIZ RIPPLING                
         GOTO1 AGOSUB                                                           
         BNE   VSC40                                                            
*                                  PROCESS OVERRIDES                            
         XC    MYDMCB(4),MYDMCB                                                 
         MVI   MYDMCB+2,CHUT       SHR/PUT ROW                                  
         MVI   GOSUBN,VCO#         VALIDATE 1ST COLUMN OF DEMOS                 
         GOTO1 AGOSUB                                                           
         BNE   VSC40                                                            
*                                                                               
         LA    RE,DOVRTG1H-DOVDEMOH                                             
         STH   RE,MYDMCB                                                        
         MVC   MYDMCB+2(1),COL     SET CTGY   MALE                              
         MVI   GOSUBN,VCO#         VALIDATE 1ST COLUMN OF DEMOS                 
         GOTO1 AGOSUB                                                           
         BNE   VSC40                                                            
*                                                                               
         LA    RE,DOVRTG2H-DOVDEMOH FILL 2ND COLUMN DEMOS                       
         STH   RE,MYDMCB                                                        
         MVC   MYDMCB+2(1),COL+1   SET CTGY   FEMALE                            
         MVI   GOSUBN,VCO#         VALIDATE 2ND COLUMN OF DEMOS                 
         GOTO1 AGOSUB                                                           
         BNE   VSC40                                                            
*                                                                               
         LA    RE,DOVRTG3H-DOVDEMOH FILL 3RD COLUMN DEMOS                       
         STH   RE,MYDMCB                                                        
         MVC   MYDMCB+2(1),COL+2   SET CTGY   ADULT                             
         MVI   GOSUBN,VCO#         VALIDATE 3RD COLUMN OF DEMOS                 
         GOTO1 AGOSUB                                                           
         BNE   VSC40                                                            
VSCR30   XC    DMCB,DMCB                                                        
         B     VSCRNX              EXIT W/NO ERRORS                             
*                                                                               
VSC40    DS    0H                  EROR ON INPUT-MARK FLDS MODIF                
         CLI   OURINFCD,TOOVRQ     IF TOO MANY OVR, DON'T SET MODIF             
         BE    VSCRN_NO                                                         
         LA    R0,DOVPFLNH         LAST FIELD ON SCREEN                         
         LA    R2,DOVRTG1H         BEGIN W/1ST RTG                              
*                                                                               
VSC45    DS    0H                                                               
         TM    2(R2),X20           IS THIS A PROTECTED FLD?                     
         BO    VSC50               YES -> DON'T WANT IT                         
         TM    4(R2),X80           WAS FLD INPUT THIS TIME?                     
         BNO   VSC50               NO, GOT TO NEXT UNPROT FLD                   
         OI    6(R2),X01+X80       SET TO MODIF & TRANSMIT                      
*                                                                               
VSC50    ZIC   R1,0(R2)            BUMP TO NEXT FLD                             
         AR    R2,R1               NEXT SCREEN FIELD                            
         CR    R2,R0               END OF SCREEN?                               
         BL    VSC45                                                            
*                                                                               
VSCRN_NO B     NO_03               FILTER BACK ERROR OCCURED                    
*                                                                               
VSCRNX   B     YES_03                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*VCOL - VALIDATE A COLUMN OF DEMOS ON SCREEN                                    
*       INPUT: MYDMCB (2)  = DISP TO COLUMN FROM DEMO NAME FIELD                
*              MYDMCB+2(1) = DISP TO CATAGORY IN DEMOVAL                        
***********************************************************************         
*                                                                               
VCOL     DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   OURINFCD,0                                                       
         CLI   MYDMCB+2,CHUT      PUT LINE PROCESSING?                          
         BNE   *+16                                                             
         LA    R3,DOVHRTGH        VALIDATE HOMES RTG 1ST                        
         LA    R4,DEMOLST                                                       
         B     VCOL15                                                           
*                                                                               
VCOL05   LA    R2,DOVDEMOH         R2-> 1ST LINE ON SCREEN                      
         LA    R4,DEMOLIST                                                      
         ZIC   RF,MYDMCB+2         CTGY (0=MALE, 1=FEMALE, 2=TOTAL)             
         MH    RF,=H'6'                                                         
         AR    R4,RF               R4 PTS TO ENTRY IN DEMOLIST                  
*                                                                               
VCOL10   LR    R3,R2                                                            
         AH    R3,MYDMCB           R3 -> COL POSN ON SCREEN                     
         CLI   0(R4),X'FF'         END OF DEMOLIST?                             
         BE    VCOLX               YES                                          
*                                                                               
VCOL15   TM    4(R3),X80           ANY INPUT TO THIS FIELD?                     
         BNO   VCOL60              FIELD WASN'T TOUCHED -> NXT FLD              
         CLI   2(R4),0             THIS CTGY IS EMPTY ->BYPASS INPUT            
         BE    VCOL60                                                           
         XC    OVAL,OVAL           OVERIDE VALUE (X'FF'=CLR OVR)                
         MVC   ODEM,1(R4)          OVDEM=DEMO MODIF + NUMBER                    
         CLI   5(R3),0             INPUT LN=0?                                  
         BNE   *+12                NO                                           
         MVI   OVAL,X'FF'          CLEAR ANY EXISTING OVERIDES                  
         B     VCOL30              FND OVR IN SVOVR                             
*                                                                               
         ZIC   R1,5(R3)            L'INPUT                                      
         BCTR  R1,0                                                             
         EXCLC R1,8(R3),SPACES     INPUT CLEARED?                               
         BNE   *+12                                                             
         MVI   OVAL,X'FF'          YES                                          
         B     VCOL30                                                           
*                                                                               
         BAS   RE,PLUS             VALIDATE FIELD FOR '+' AND #                 
         CLI   DMCB,X'FF'          VALID INPUT TO FIELD?                        
         BE    VCOLINVF            NO, SET ALL MODF FLDS TO MODF NXT            
         MVI   PLUSFLAG,PLUSNO                                                  
         CLI   DMCB,X'01'                                                       
         BNE   *+8                                                              
         MVI   PLUSFLAG,PLUSYES                                                 
         TM    DMCB+4,X80          NEG NUMBER?                                  
         BO    VCOLINVF                                                         
         CLI   1(R4),C'R'          MAX RTG=99.9                                 
         BE    *+8                                                              
         CLI   1(R4),C'P'          MAX PUT=99.9                                 
         BE    *+12                                                             
         CLI   1(R4),C'S'                                                       
         BNE   *+14                                                             
         CLC   =F'999',DMCB+4      IS INPUT TOO LARGE                           
         BL    VCOLINVF                                                         
         CLI   1(R4),C'T'                                                       
         BNE   *+14                                                             
         CLC   =F'999999',DMCB+4                                                
         BL    VCOLINVF                                                         
         MVC   OVAL,DMCB+4         SET OVERIDE VALUE                            
*                                                                               
         USING OVRLSTD,RE                                                       
VCOL30   DS    0H                  SEARCH FOR DEMO IN SVOVR LIST                
         LA    RE,SVOVR            PT TO SAVED OVR LIST                         
         XC    DMCB,DMCB                                                        
VCOL35   CLI   0(RE),X'FF'         END OF OVR LIST?                             
         BE    VCOL50              YES                                          
         CLC   OVRIDEM,ODEM        MATCH OF DEMO MODF & NUMBER                  
         BNE   VCOL40                                                           
         CLI   OVAL,X'FF'          DELETE OVR ELEM?                             
         BNE   *+10                                                             
         XC    OVRID(OVRNTY),OVRID CLEAR ENTRY IN SVOVR                         
         MVC   OVRVAL,OVAL         MOVE IN DEMO VALUE                           
         B     VCOL60              BUMP TO IMP OR NEXT ROW'S RTG                
*                                                                               
VCOL40   CLI   OVRID,0             FREE SLOT IN SVOVR LIST?                     
         BNE   *+8                 NO                                           
         ST    RE,DMCB             SAVE IT IN CASE WE NEED TO ADD NTY           
         LA    RE,OVRNTY(RE)       BUMP TO NEXT OVR IN LIST                     
         B     VCOL35                                                           
*                                                                               
VCOL50   CLI   OVAL,X'FF'          WAS THIS A RQST TO CLR OVR?                  
         BE    VCOL60              YES -> OVR DIDN'T EXIST, WE'RE OKAY          
         OC    DMCB,DMCB           NO -> ADD OVR ELEM                           
         BZ    *+8                 DID WE HAVE A(FREE SPOT IN SVOVR)?           
         L     RE,DMCB             IF YES GET IT, ELSE ADD TO EOL               
         LA    R1,SVOVR+SVOVRLN                                                 
         CR    RE,R1                                                            
         BNL   VCOLBF              WE HIT MAX # OVRS FOR BUFFER                 
*                                                                               
         MVC   OVRIDEM,ODEM        MOVE IN MODIF & NUMBER                       
         MVC   OVRVAL,OVAL         MOVE IN OVR VALUE                            
         OC    DMCB,DMCB           DID WE USE A 'FREE' BKT?                     
         BNZ   *+8                 YES DON'T X'FF' EOL                          
         MVI   OVRNTY(RE),X'FF'    MARK END OF LIST                             
         DROP  RE                                                               
*                                                                               
VCOL60   CLI   MYDMCB+2,CHUT       DO ENTIRE HUT LINE DEMOS                     
         BNE   VCOL65                                                           
         CLI   1(R4),C'R'          DID WE JUST PROCESS RTG?                     
         BE    VCOL65                                                           
         LA    R4,3(R4)            NEXT DEMO IN DEMOLST                         
         CLI   1(R4),C'U'          UNVS= LAST DEMO IN HOMES BUFFER              
         BE    VCOLX               END OF HOMES LIST                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         AR    R3,R1                                                            
         TM    1(R3),X'20'         PROT FLD?                                    
         BO    *-10                YES, SKIP IT                                 
         B     VCOL15                                                           
*                                  BUMP TONEXT ROW RTG IN COLUMN                
VCOL65   CLI   1(R4),C'R'          DID WE JUST PROCESS RTG?                     
         BNE   VCOL100                                                          
         LR    RE,R3               A(CURRENT RTG SCREEN FIELD)                  
         LA    R3,DOVIMP1H-DOVRTG1H(R3) GO DO IMPS FOR THIS DEMO                
**********************************************************************          
* STARTING HERE WILL BE OUR IMPRESSION CHANGING CODE ALONG W'RATING  *          
* 1: ADD/SUBTRACT THE SAME % OF CHANGE FOR THE RATING FIELD FROM THE *          
*    IMPRESSION.                                                     *          
* 2: IF IMPRESSION'S 0: -> IMP = RTG * UNV                           *          
**********************************************************************          
         CLI   PLUSFLAG,PLUSYES    IF PLUS IN FIELD PROCEED AS NORMAL           
         BE    VCOL95                                                           
         CLI   HRZBITS,0           IF PLUS IN FIELD PROCEED AS NORMAL           
         BNE   VCOL95                                                           
         TM    4(RE),X'80'         ONLY IF RATINGS CHANGED DO THE IMPS          
         BNO   VCOL95                                                           
         CLI   2(R4),0             ONLY IF RATINGS CHANGED DO THE IMPS          
         BE    VCOL95                                                           
*                                                                               
         LR    R0,R3             SAVE R3  SO WE CAN CALL PLUS ROUTINE           
         LR    R1,RE             R1=A(CURRENT RATINGS SCREEN FIELD)             
         MVC   SVHRZMEN,DMCB+4   DMCB+4 MUST BE NEW RATINGS VALIDATED           
*                                ALREADY                                        
         LA    RF,DOVHRTGH       RE=A(CURRENT RATINGS), RF=A(DOVHRTGH)          
         SR    R1,RF             DISPLACEMENT                                   
         L     RF,ASCRNTAB                                                      
         AR    RF,R1             ADD DISPACEMENT TO SAVED SCREEN TABLE          
*                                TO GET RATINGS FROM SAVED SCREEN TABLE         
         ZIC   RE,0(RF)          TOTAL LENGTH                                   
         AHI   RE,-8             SUBTRACT HEADER LENTH AND STICK IT             
         STC   RE,5(RF)          5 BEYOND HEADER AS INPUT LENGTH                
*                                                                               
         LR    R3,RF             POINT R3 TO OLD RATINGS VALUE BEFORE           
         BAS   RE,PLUS           CHANGE TO SCREEN                               
         CLI   DMCB,X'FF'        OLD RATINGS BETTER BE VALID OR ELSE            
         BNE   *+8                                                              
         B     VCOLINVF                                                         
         LR    R3,R0             LETS RESTORE R3                                
*                                                                               
         ICM   R1,15,SVHRZMEN       NEW RATING                                  
         L     RF,DMCB+4            OLD RATING                                  
         OR    RF,RF                IF OLD RATING IS ZERO JUST REPLACE          
         BZ    VCOL90A              AND PROCEED TO CALCULATE IMP.               
         CR    R1,RF                                                            
         BL    VCOL80                                                           
*                                                                               
* NEW RATING HERE MUST HAVE BEEN HIGHER THAN OLD RATING                         
*                                                                               
         MVI   DIFFFLAG,DIFFPLUS                                                
         SR    R1,RF             CALCULATE DIFFERENCE                           
         MH    R1,=H'100'                                                       
         SR    R0,R0                                                            
         DR    R0,RF             R1 NOW HAS PERCENTAGE CHANGE                   
         B     VCOL90                                                           
*                                                                               
VCOL80   DS    0H                                                               
*                                                                               
* OLD RATING HERE MUST HAVE BEEN HIGHER THAN NEW RATING                         
*                                                                               
         MVI   DIFFFLAG,DIFFMINS                                                
         ICM   RF,15,SVHRZMEN       NEW RATING                                  
         L     R1,DMCB+4         OLD RATING                                     
         SR    R1,RF             CALCULATE DIFFERENCE                           
         MH    R1,=H'100'                                                       
         SR    R0,R0                                                            
         L     RF,DMCB+4                                                        
         DR    R0,RF             R1 NOW HAS PERCENTAGE CHANGE                   
*                                                                               
VCOL90   DS    0H                                                               
         ST    R1,FULL            FULL NOW HAS PERCENTAGE CHANGE                
*    RIGHT NOW R3 HAD ALREADY BEEN RESTORED TO POINT TO IMPRESSION              
*    FIELD,SO WE CAN SAFELY CALL PLUS ROUTINE TO GET BINARY IMPRESSION          
         BAS   RE,PLUS                                                          
         CLI   DMCB,X'FF'         OLD IMPRESSION BETTER BE GOOD OR ELSE         
         BNE   *+6                SOMETHING IS WRONG                            
         DC    H'0'                                                             
         L     R1,FULL            PERCENTAGE CHANGE                             
         OR    R1,R1                                                            
         BZ    VCOL95                                                           
*                                                                               
VCOL90A  L     RF,DMCB+4          IMPRESSION                                    
         OR    RF,RF                                                            
         BNZ   VCOL91X            IF IMPRESSION=0, CALCULATE                    
*    NEW IMPRESSION = NEW RATING * UNIVERSE                                     
*    SKIP THIS CALCULATION IF THE PERCENTAGE OF CHANGE FOR RATING'S 0           
         ICM   R1,15,SVHRZMEN      NEW RATING                                   
         SR    R0,R0                                                            
*                                                                               
         LR    RF,R4               GET THE DISPLACEMENT INTO TABLE              
         LA    RE,DEMOLIST                                                      
         CR    RF,RE               IS R4 POINTED AT THE HOMES VALUE?            
         BNL   *+12                                                             
         L     RF,DEMOVUNV         LOAD THE UNIVERSE FROM HOMES FIELD           
         B     VCOL91U                                                          
*                                                                               
         SR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6'            HOW MANY DEMOS IN? (M+F = 6 BYTES)           
         M     RE,=F'4'            UNIVERSE TABLE'S 4 BYTE PER CELL             
         LA    RE,DEMOVALS                                                      
         L     RF,0(RF,RE)         POINT TO THE DEMO UNIVERSE WE WANT           
*                                                                               
VCOL91U  MR    R0,RF               IMP = RTG * UNIVERSE                         
         LA    RF,100              SET PRECISION                                
         DR    R0,RF                                                            
         ST    R1,FULL                                                          
         B     VCOL93                                                           
*                                                                               
VCOL91X  SR    R0,R0               ADJUST IMPRESSION BY % OF CHANGE             
         MR    R0,RF                                                            
         ST    R1,FULL             R1 NOW HAS TOTAL DIFF OF IMPRESSION          
* NEED TO DIVIDE IMPRESSION DIFFERENCE BY 100 TO COMPENSATE FOR PERCENT         
         LA    RF,100                                                           
         SR    R0,R0                                                            
         DR    R0,RF               R1=IMPRESSION DIFFERENCE                     
         L     RF,DMCB+4           OLD MPRESSION                                
*  COMPARE TO SEE IF NEW IMPRESSION SHOULD ADD OR SUBTRACT DIFFERENCE           
         CLI   DIFFFLAG,DIFFPLUS                                                
         BNE   VCOL92                                                           
*                                                                               
VCOL91   AR    RF,R1                                                            
         ST    RF,FULL                  FULL HAS NEW IMPRESSION                 
         B     VCOL93                                                           
*                                                                               
VCOL92   SR    RF,R1                                                            
         ST    RF,FULL                  FULL HAS NEW IMPRESSION                 
*                                                                               
VCOL93   DS    0H                                                               
         XC    8(L'DOVRTG1,R3),8(R3)           RF HAS IMPRESSION                
                                                                                
         EDIT  (B4,FULL),(7,8(R3)),1,ZERO=NOBLANK     DISP NEW IMP              
         MVI   5(R3),7                                                          
         MVI   4(R3),X'80'                                                      
*                                                                               
*   AT THIS POINT LETS GET OLD RATINGS FROM SAVED SCRNTABLE AND SEE             
*   WHAT THE DIFFERENCE OF THE CHANGE WAS                                       
*                                                                               
VCOL95   DS    0H                                                               
*                                                                               
         LA    R4,3(R4)            GO TO IMP IN DEMOLIST                        
         B     VCOL15                                                           
VCOL100  DS    0H                                                               
*                                  BUMP TO NEXT ROW RTG IN COLUMN               
         LA    R4,5*3(R4)          FROM IMP BUMP TO RTG OF NEXT LINE            
         LA    R2,DOVDEM2H-DOVDEMOH(R2) NEXT LINE ON SCREEN                     
         LA    R0,DOVDEMXH         END OF SCREEN                                
         CR    R2,R0               LAST DEMO ON SCREEN?                         
         BNH   VCOL10              NOT YET                                      
         B     VCOLX                                                            
*                                                                               
VCOLX    B     YES_03                                                           
*                                                                               
VCOLINVF ST    R3,DMCB             RETURN ADDRESS OF FIELD                      
         MVI   OURERRCD,IFLDQ      SET TO INVALID FIELD                         
         B     NO_03                                                            
*                                                                               
VCOLBF   ST    R3,DMCB             RETURN ADDRESS OF FIELD                      
         MVI   OURINFCD,TOOVRQ     SET TO INVALID FIELD                         
         B     NO_03                                                            
         EJECT                                                                  
*                                                                               
PLUSFLAG DS    X                                                                
PLUSYES  EQU   0                                                                
PLUSNO   EQU   1                                                                
*                                                                               
***********************************************************************         
*BLK -   PROCESS BLANKED OUT FIELDS.  DELETE OVR FROM LIST IF EXISTED           
*        GET ORIG VALUE FROM DEMOUT AND REPLACE IT IN TWA FIELD.                
***********************************************************************         
BLNK     DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYDMCB,1            GET RID OF BLANKED FLDS FROM SVOVR           
*                                                                               
BLK02    LA    R3,DOVHRTGH         DO HOMES LINE FIRST                          
         LA    R4,DEMOLST                                                       
         LA    R6,DEMOVLS                                                       
         B     BLK10                                                            
*                                                                               
BLK05    LA    R3,DOVRTG1H                                                      
         LA    R4,DEMOLIST                                                      
         LA    R6,DEMOVALS                                                      
*                                                                               
BLK10    CLI   2(R4),0             UNDEF FLD?                                   
         BE    BLK40               GO TO NEXT FIELD ON SCREEN                   
         TM    4(R3),X80           ANY INPUT TO THIS FIELD?                     
         BNO   BLK40               NO, NXT FLD                                  
         CLI   5(R3),0             REQUEST TO CLR OVR?                          
         BE    BLK12               YES                                          
         ZIC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EXCLC R1,8(R3),SPACES                                                  
         BE    BLK12                                                            
         CLI   MYDMCB,2            ON 2ND ITERATION VALID OTHER FIELDS          
         BE    BLK30                                                            
         B     BLK40               NOT A REQ TO BLANK OUT OVR                   
*                                                                               
BLK12    CLI   MYDMCB,2            REPLACE VALUES INTO BLANKED FLDS?            
         BE    BLK25               YES                                          
         MVC   ODEM,1(R4)          CLEAR FIELD/OVR                              
         MVI   OVAL,XFF                                                         
         DS    0H                  SEARCH FOR DEMO IN SVOVR LIST                
         USING OVRLSTD,RE                                                       
         LA    RE,SVOVR            PT TO SAVED OVR LIST                         
BLK15    CLI   0(RE),X'FF'         END OF OVR LIST?                             
         BE    BLK40               DEMO NOT FOUND                               
         CLC   OVRIDEM,ODEM        MATCH OF DEMO MODF & NUMBER                  
         BNE   BLK20                                                            
         XC    OVRID(OVRNTY),OVRID CLEAR ENTRY IN SVOVR                         
         MVC   OVRVAL,OVAL         MOVE IN DEMO VALUE                           
         B     BLK40               BUMP TO IMP OR NEXT ROW'S RTG                
BLK20    LA    RE,OVRNTY(RE)       BUMP TO NEXT OVR IN LIST                     
         B     BLK15                                                            
         DROP  RE                                                               
*                                                                               
BLK25    DS    0H                  OUTPUT DEMOVALUE IN TO BLK FLD               
         MVC   8(L'DOVRTG1,R3),SPACES   ASSUMES L'RTG=L'IMPS                    
         EDIT  (B4,0(R6)),(L'DOVRTG1,8(R3)),1,ZERO=NOBLANK                      
         MVI   5(R3),7             SET MAX FLDLN                                
         OI    6(R3),X80           TRANSMIT FIELD                               
         NI    4(R3),XFF-X80       TURN OFF INPUT THIS TIME BIT                 
*                                                                               
         ZIC   R1,0(R3)            CLEAR OVR INDIC FLD                          
         AR    R1,R3                                                            
         CLI   8(R1),C'!'                                                       
         BNE   *+12                                                             
         MVI   8(R1),C'c'                                                       
         B     *+8                                                              
         MVI   8(R1),C' '                                                       
         OI    6(R1),X80                                                        
         B     BLK40                                                            
*                                                                               
BLK30    DS    0H                  VALID INPUT IN OTHER FIELDS                  
         CLI   OURERRCD,0          ALREADY GOT AN ERROR                         
         BNE   BLK40                                                            
         BAS   RE,PLUS             VALIDATE FLD FOR '+' AND VALUE               
         CLI   DMCB,X'FF'          VALID INPUT TO FIELD?                        
         BE    BLK35                                                            
         TM    DMCB+4,X80          NEG NUMBER?                                  
         BO    BLK35                                                            
         CLI   1(R4),C'T'                                                       
         BNE   *+18                                                             
         CLC   =F'999999',DMCB+4                                                
         BL    BLK35                                                            
         B     BLK40                                                            
*                                                                               
         CLC   =F'999',DMCB+4      RTG/PUT/SHR MAX IS 99.9                      
         BL    BLK35                                                            
         B     BLK40                                                            
*                                                                               
BLK35    ST    R3,MYDMCB+4                                                      
         MVI   OURERRCD,IFLDQ                                                   
         B     BLK40                                                            
*                                                                               
BLK40    LA    R4,3(R4)            NEXT DEMO IN DEMOLIST                        
         LA    R6,4(R6)            NEXT DEMO IN DEMOVAL                         
         CLI   1(R4),C'U'          DID PUTS?                                    
         BE    BLK05               YES, NOW DO REST OF DEMOS                    
         CLI   0(R4),XFF           END OF LIST?                                 
         BE    BLK50                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)            NEXT FLD ON SCREEN                           
         AR    R3,R1                                                            
         TM    1(R3),X20           PROTECTED FIELD?                             
         BO    *-10                YES, ONLY WANT UNPROT FLDS                   
         B     BLK10                                                            
                                                                                
*GOTO DEMOUT W/UPDATED OVR LIST TO GET VALUES FOR BLANKED FLDS                  
BLK50    DS    0H                  DEMOLIST ALREADY BUILT                       
         CLI   MYDMCB,2            DONE FILLING SCREEN                          
         BE    BLKX                                                             
         MVI   GOSUBN,BLD#         BUILD DEMOVALS                               
         GOTO1 AGOSUB                                                           
         MVI   MYDMCB,2            PUT VALUES ON SCRN FOR BLANK FLDS            
         XC    MYDMCB+4(4),MYDMCB+4     ADDRESS OF ERROR                        
         B     BLK02                                                            
*                                                                               
BLK_NO   DS    0H                  INVALID INPUT IN A FIELD                     
                                                                                
*                                                                               
BLKX     DS    0H                                                               
         CLI   OURERRCD,0          MYDMCB+4=A(FLD W/ERROR)                      
         BNE   NO_03                                                            
         B     YES_03                                                           
         EJECT                                                                  
***********************************************************************         
*HORIZ - PROCESS HORIZ RIPPLE REQUEST (+ BEFORE VALUE)                          
*                                                                               
*  NOTE: HRZTBL : BITS(1),MODF(1),MALE DEM#(1),MALE VALUE(4),                   
*                 FEM DEM#(1), FEM VALUE(4), TOT DEM#(1), TOT VALUE(4)          
***********************************************************************         
HORIZ    DS    0H                                                               
         MVI   GOSUBN,UNV#         READ UNIVS INTO DEMOVAL                      
         GOTO1 AGOSUB                                                           
*                                                                               
HRZ05    LA    R2,DOVDHOMH         PROCESS HOMES ROW                            
         LA    R4,DEMOLST                                                       
         ST    R4,MYDMCB                                                        
         LA    R6,DEMOVLS                                                       
         ST    R6,MYDMCB+4                                                      
         LA    R6,HRZMDEM          PT TO 1ST ENTRY IN TABLE                     
         XC    HRZTBL,HRZTBL                                                    
         MVI   HRZMODF,C'H'       SET MODIF IN TBL TO INDICATE HOMES            
         LA    R3,DOVHRTGH-DOVDHOMH(R2)    DSP TO RTGS                          
         MVC   0(1,R6),2(R4)                                                    
         BAS   RE,PLUS             LK FOR PLUS IN FIELD?                        
         BE    HRZ_NO              ERROR IN FLD                                 
         MVC   1(4,R6),DMCB+4                                                   
         CLI   DMCB,X'01'                                                       
         BNE   *+8                                                              
         OI    HRZBITS,X'01'       '+' IN HOMES RTG FLD                         
*                                                                               
         LA    R3,DOVHIMPH-DOVDHOMH(R2)   + IN IMPS FLD IS INVALID              
         LA    R4,6(R4)            NEXT RTG IN DEMOLIST                         
         MVC   0(1,R6),2(R4)                                                    
         BAS   RE,PLUS             LK FOR PLUS IN FIELD?                        
         CLI   DMCB,X'01'          NOT VALID FOR IMPS                           
         BE    HRZ_NO                                                           
*                                                                               
         LA    R6,HRZFDEM          PT TO NEXT BKT IN TABLE                      
         LA    R3,DOVHSHRH-DOVDHOMH(R2)    DSP TO SHARES                        
         MVC   0(1,R6),2(R4)                                                    
         BAS   RE,PLUS             LK FOR PLUS IN FIELD?                        
         BE    HRZ_NO              ERROR IN FLD                                 
         MVC   1(4,R6),DMCB+4                                                   
         CLI   DMCB,X'01'                                                       
         BNE   *+8                                                              
         OI    HRZBITS,X'02'       '+' IN HOMES SHR FLD                         
*                                                                               
         LA    R6,HRZTDEM          PT TO NEXT BKT IN TABLE                      
         LA    R3,DOVHPUTH-DOVDHOMH(R2)    DSP TO PUTS                          
         MVC   0(1,R6),2(R4)                                                    
         BAS   RE,PLUS             LK FOR PLUS IN FIELD?                        
         BE    HRZ_NO              ERROR IN FLD                                 
         MVC   1(4,R6),DMCB+4                                                   
         CLI   DMCB,X'01'                                                       
         BNE   *+8                                                              
         OI    HRZBITS,X'04'       '+' IN HOMES PUT FLD                         
*                                                                               
HRZ05B   DS    0H                                                               
         CLI   HRZBITS,0                                                        
         BE    HRZ09               NOTHING TO RE-CALC                           
         CLI   HRZBITS,X'06'       INVALID COMBO                                
         BH    HRZ_NO                                                           
         BAS   RE,CALCHOM          RE-CALC FIELD                                
         BAS   RE,RTNVAL           RETURN CALC VALUE TO SCREEN                  
*                                                                               
*-----                                                                          
HRZ09    LA    R2,DOVDEMOH         R2=ROW                                       
         LA    R4,DEMOLIST                                                      
         ST    R4,MYDMCB           MYDMCB =  A(ENTRY IN DEMOLIST)               
         LA    R6,DEMOVALS                                                      
         ST    R6,MYDMCB+4         MYDMCB+4 = A(UNIV ENTRY IN DEMOVAL)          
*                                                                               
HRZ10    DS    0H                                                               
         LA    R6,HRZMDEM          PT TO MALE DEMO# BKT                         
         XC    HRZTBL,HRZTBL                                                    
         MVI   HRZMODF,C'R'       SET MODIF IN TBL TO RTGS                      
         LA    R3,DOVRTG1H-DOVDEMOH(R2)                                         
***      LA    RE,DOVIMP1H-DOVDEMOH(R2)                                         
***      ST    RE,AMALEIMP        SAVE ADDRESS OF MALE IMPRESSION               
*                                 FIELD FOR THIS ROW                            
         CLI   2(R4),0                                                          
         BE    HRZ12                                                            
         MVC   0(1,R6),2(R4)                                                    
         BAS   RE,PLUS             LK FOR PLUS IN FIELD?                        
         MVC   1(4,R6),DMCB+4                                                   
         CLI   DMCB,X'01'                                                       
         BNE   *+8                                                              
         OI    HRZBITS,X'01'        + IN MALE FLD                               
* BPOO   8/24/00                                                                
****     ST    R6,AMDEMLST       SAVE ADDRESS OF MALE PART OF DEMOLIST          
*                                                                               
HRZ12    LA    R3,DOVRTG2H-DOVDEMOH(R2)                                         
         LA    R4,6(R4)            NEXT RTG IN DEMOLIST                         
         LA    R6,HRZFDEM          PT TO FEMALE DEMO# BKT                       
         CLI   2(R4),0                                                          
         BE    HRZ14                                                            
         MVC   0(1,R6),2(R4)                                                    
         BAS   RE,PLUS             LK FOR PLUS IN FIELD?                        
         MVC   1(4,R6),DMCB+4                                                   
         CLI   DMCB,X'01'                                                       
         BNE   *+8                                                              
         OI    HRZBITS,X'02'        + IN FEM                                    
*****    ST    R6,AFDEMLST         SAVE ADDRESS OF FEMALE PART OF               
*                                  DEMOLIST                                     
HRZ14    LA    R3,DOVRTG3H-DOVDEMOH(R2)                                         
         LA    R4,6(R4)            NEXT RTG IN DEMOLIST                         
         LA    R6,HRZTDEM          PT TO TOTAL DEMO# BKT                        
         CLI   2(R4),0                                                          
         BE    HRZ16                                                            
         MVC   0(1,R6),2(R4)                                                    
         BAS   RE,PLUS             LK FOR PLUS IN FIELD?                        
         MVC   1(4,R6),DMCB+4                                                   
         CLI   DMCB,X'01'                                                       
         BNE   *+8                                                              
         OI    HRZBITS,X'04'        + IN TOT                                    
*****    ST    R6,AFDEMLST         SAVE ADDRESS OF TOTAL PART OF                
*                                  DEMOLIST                                     
HRZ16    CLI   HRZBITS,X'07'       ALL FLDS HAVE A '+'                          
         BE    HRZ_NO              INVALID INPUT                                
         CLI   HRZBITS,X'00'       NO +'S                                       
         BE    HRZ20               NO HRZ RIPL FOR RTGS... CHK IMPS             
         BAS   RE,CALC             CALCULATE RTGS                               
         BNE   *+8                                                              
         BAS   RE,RTNVAL           RETURN VALUES TO SCREEN                      
         CLI   OURERRCD,0                                                       
         BNE   HRZ_NO                                                           
*                                                                               
HRZ20    DS    0H                  PROCESS IMPS FOR ROW                         
         XC    HRZTBL,HRZTBL                                                    
         MVI   HRZMODF,C'I'        SET MODIF IN TBL TO IMPS                     
         L     R4,MYDMCB           RESET TO 1ST DEMO ON LN IN DEMOLIST          
         LA    R4,3(R4)            PT TO IMPS                                   
         LA    R6,HRZMDEM          PT TO MALE DEM# BKT                          
         LA    R3,DOVIMP1H-DOVDEMOH(R2)                                         
*                                                                               
         CLI   2(R4),0                                                          
         BE    HRZ22                                                            
         MVC   0(1,R6),2(R4)                                                    
         BAS   RE,PLUS             LK FOR PLUS IN FIELD?                        
         MVC   1(4,R6),DMCB+4                                                   
         CLI   DMCB,X'01'                                                       
         BNE   *+8                                                              
         OI    HRZBITS,X'01'       MALE FLD HAS A +                             
*                                                                               
HRZ22    LA    R3,DOVIMP2H-DOVDEMOH(R2)                                         
         LA    R4,6(R4)            NEXT RTG IN DEMOLIST                         
         LA    R6,HRZFDEM          PT TO FEMALE DEM# BKT                        
         CLI   2(R4),0                                                          
         BE    HRZ24                                                            
         MVC   0(1,R6),2(R4)                                                    
         BAS   RE,PLUS             LK FOR PLUS IN FIELD?                        
         MVC   1(4,R6),DMCB+4                                                   
         CLI   DMCB,X'01'                                                       
         BNE   *+8                                                              
         OI    HRZBITS,X'02'                                                    
*                                                                               
HRZ24    LA    R3,DOVIMP3H-DOVDEMOH(R2)                                         
         LA    R4,6(R4)            NEXT IMP IN DEMOLIST                         
         LA    R6,HRZTDEM          PT TO TOTAL DEM# BKT                         
         CLI   2(R4),0                                                          
         BE    HRZ26                                                            
         MVC   0(1,R6),2(R4)                                                    
         BAS   RE,PLUS             LK FOR PLUS IN FIELD?                        
         MVC   1(4,R6),DMCB+4                                                   
         CLI   DMCB,X'01'                                                       
         BNE   *+8                                                              
         OI    HRZBITS,X'04'        + IN TOTAL FLD                              
*                                                                               
HRZ26    CLI   HRZBITS,X'07'        ALL FLDS HAVE A '+'                         
         BE    HRZ_NO              INVALID INPUT                                
         CLI   HRZBITS,X'00'        NO +'S                                      
         BE    HRZ30               NO HRZ RIPL FOR IMPS ON LINE                 
         BAS   RE,CALC             CALCULATE IMPS                               
         BNE   *+8                                                              
         BAS   RE,RTNVAL           RETURN VALUES TO SCREEN                      
         CLI   OURERRCD,0                                                       
         BNE   HRZ_NO                                                           
*                                                                               
HRZ30    DS    0H                  BUMP TO NEXT ROW ON SCREEN                   
         L     R4,MYDMCB           R4 ->DEMOLIST                                
         LA    R4,18(R4)           NEXT ROW IN DEMOLIST                         
         ST    R4,MYDMCB                                                        
         CLI   0(R4),X'FF'         END OF DEMO LIST?                            
         BE    HRZ_YES             DONE W/ALL ROWS                              
         L     R6,MYDMCB+4                                                      
         LA    R6,12(R6)           NEXT UNIV ROW IN DEMOVALS                    
         ST    R6,MYDMCB+4                                                      
         LA    R2,DOVDEM2H-DOVDEMOH(R2) NEXT ROW ON SCREEN                      
         B     HRZ10                                                            
*                                                                               
HRZ_NO   DS    0H                  ALL FLDS CANNOT HAVE '+' IN IT               
         MVI   OURERRCD,IFLDQ                                                   
         ST    R3,DMCB                                                          
         B     NO_03                                                            
*                                                                               
HRZ_YES  DS    0H                                                               
         B     YES_03                                                           
         EJECT                                                                  
***********************************************************************         
*PLUS -  LOOK FOR '+' IN FIELD                                                  
*        INPUT: R3 -> FIELD ON SCREEN                                           
*        OUTPUT: DMCB(1)= X'FF' INVALID INPUT                                   
*                         X'01' PLUS IN FIELD                                   
*                         X'00' NO PLUS IN FIELD                                
*                                                                               
*              : DMCB+4 = VALUE OF INPUT FIELD                                  
***********************************************************************         
PLUS     DS    0H                                                               
         NTR1                                                                   
         MVI   DUB,0                                                            
         XC    DMCB,DMCB                                                        
         LA    R6,8(R3)                                                         
         ZIC   R1,5(R3)                                                         
         LTR   R1,R1                                                            
         BZ    PLUSNX                                                           
*                                                                               
PLUS5    CLI   0(R6),C'+'                                                       
         BE    PLUS10                                                           
         CLI   0(R6),C'.'          IS THIS A DECIMAL POINT                      
         BE    PLUS12                                                           
         CLI   0(R6),X'F0'         IS THIS A NUMBER                             
         BNL   PLUS12                                                           
         CLI   0(R6),C' '                                                       
         BNE   PLUS12                                                           
         LA    R6,1(R6)            BYPASS EMPTY SPACES                          
         BCT   R1,PLUS5                                                         
         B     PLUSNX                                                           
*                                                                               
PLUS10   MVI   DUB,X'01'                                                        
         LA    R6,1(R6)                                                         
         BCT   R1,PLUS12                                                        
         B     PLUSNX              IF R1=ZERO, INVALID INPUT                    
*                                                                               
PLUS12   ST    R1,DMCB+4           SAVE LENGTH OF FIELD                         
         GOTO1 CASHVAL,DMCB,(1,(R6))                                            
         CLI   DMCB,XFF                                                         
         BE    PLUSNX                                                           
         MVC   DMCB(1),DUB                                                      
         B     PLUSX                                                            
*                                                                               
PLUSNX   MVI   DMCB,XFF                                                         
*                                                                               
PLUSX    DS    0H                                                               
         CLI   DMCB,XFF                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*CALCHOM - CALCULATES HOME LINE DEMOS                                           
*        HRZTBL HOLDS VALUES:  HRZMEN = RTG                                     
*                              HRZFEM = SHR                                     
*                              HRZTOT = PUT                                     
***********************************************************************         
CALCHOM  NTR1                                                                   
         LA    R1,CHOMTBL          DETERMINE WHAT TO RE-CALC FROM TBL           
CLH05    CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   HRZBITS,0(R1)                                                    
         BE    CLH10                                                            
         LA    R1,L'CHOMTBL(R1)                                                 
         B     CLH05                                                            
*                                                                               
CLH10    MVC   HRZMODF,1(R1)       SET MODF TO WHAT WE WANT TO CALC             
         CLI   HRZMODF,C'P'        PUT=RTG/SHR  -> TOT=MEN/FEM                  
         BNE   CLH20                                                            
         OC    HRZFEM,HRZFEM                                                    
         BNZ   *+14                                                             
         XC    HRZTOT,HRZTOT       IF SHR=0, THEN PUT=0                         
         B     CALCHX              DONE                                         
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         ICM   R1,15,HRZMEN        PUT = (RTG *10000)/SHR + 5/10                
         M     R0,=F'10000'                                                     
         ICM   RF,15,HRZFEM        FEM = SHR                                    
         DR    R0,RF               RTG * 10,000 / SHR                           
         SR    R0,R0                                                            
         AH    R1,=H'5'                                                         
         D     R0,=F'10'                                                        
         STCM  R1,15,HRZTOT                                                     
         B     CALCHX                                                           
*                                                                               
CLH20    CLI   HRZMODF,C'S'        CALC SHRS=RTG/PUT  -> FEM=MEN/TOT            
         BNE   CLH30                                                            
         OC    HRZTOT,HRZTOT                                                    
         BNZ   *+14                                                             
         XC    HRZFEM,HRZFEM       IF PUT=0, THEN SHR=0                         
         B     CALCHX              DONE                                         
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         ICM   R1,15,HRZMEN        SHR = (RTG *10000)/PUT + 5/10                
         M     R0,=F'10000'                                                     
         ICM   RF,15,HRZTOT        TOT=PUT                                      
         DR    R0,RF               RTG * 10,000 / PUT                           
         SR    R0,R0                                                            
         AH    R1,=H'5'                                                         
         D     R0,=F'10'                                                        
         STCM  R1,15,HRZFEM                                                     
         B     CALCHX                                                           
                                                                                
CLH30    CLI   HRZMODF,C'R'        CALC RTG=SHR*PUT -> MEN=FEM*TOT              
         BNE   CALCHX                                                           
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         ICM   R1,15,HRZFEM        RTG = (SHR * PUT) + 500/1000                 
         ICM   RF,15,HRZTOT                                                     
         MR    R0,RF                                                            
         A     R1,=F'500'                                                       
         SR    R0,R0                                                            
         D     R0,=F'1000'                                                      
         STCM  R1,15,HRZMEN                                                     
         B     CALCHX                                                           
                                                                                
CALCHX   DS    0H                                                               
         MVI   HRZMODF,C'H'        HOMES                                        
         B     YES_03                                                           
                                                                                
*----------------------------------------------------------------               
CHOMTBL  DS    0XL2                                                             
         DC    X'01',C'S'           RTG CHGD, RE-CALC SHR                       
         DC    X'02',C'R'           SHR CHGD, RE-CALC RTG                       
         DC    X'04',C'R'           PUT CHGD, RE-CALC RTG                       
         DC    X'03',C'P'           RTG & SHR -> RE-CALC PUT                    
         DC    X'05',C'S'           RTG & PUT -> RE-CALC SHR                    
         DC    X'06',C'R'           PUT & SHR -> RE-CALC RTG                    
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*CALC   - CALCULATES FIELDS FOR HORZ RIPPLING FOR RTGS                          
***********************************************************************         
CALC     DS    0H                                                               
         NTR1                                                                   
         CLI   HRZMODF,C'R'        FOR RTGS, MULT BY UNIVS                      
         BNE   CALC15                                                           
         L     R6,MYDMCB+4         A(UNIVS)                                     
         SR    R0,R0                                                            
         ICM   R1,15,0(R6)         MALE UNIV                                    
         BZ    CALCNX                                                           
         ICM   RF,15,HRZMEN        MALE RTG                                     
         MR    R0,RF                                                            
         STCM  R1,15,HRZMEN                                                     
         SR    R0,R0                                                            
         ICM   R1,15,4(R6)         FEMALE UNIV                                  
         BZ    CALCNX                                                           
         ICM   RF,15,HRZFEM        FEMALE RTG                                   
         MR    R0,RF                                                            
         STCM  R1,15,HRZFEM                                                     
         SR    R0,R0                                                            
         ICM   R1,15,8(R6)         TOT UNIV                                     
         BZ    CALCNX                                                           
         ICM   RF,15,HRZTOT        TOT RTG                                      
         MR    R0,RF                                                            
         STCM  R1,15,HRZTOT                                                     
****     MVC   SVHRZFEM,HRZFEM     SAVE OLD FEM 'IMP' FOR LATER USE             
****     MVC   SVHRZTOT,HRZTOT     SAVE OLD TOTAL FOR LATER USE                 
*                                                                               
CALC15   CLI   HRZBITS,X'01'        + MALE ONLY                                 
         BE    *+8                                                              
         CLI   HRZBITS,X'02'        + FEMALE ONLY                               
         BE    *+8                                                              
         CLI   HRZBITS,X'03'        + MALE AND + FEMALE                         
         BNE   CALC20                                                           
         ICM   R1,15,HRZMEN         R1=MEN VALUE                                
         ICM   RF,15,HRZFEM         RF=FEM VALUE                                
         AR    R1,RF                TOT = MEN + FEM                             
         CLI   HRZTDEM,0                                                        
         BE    *+8                                                              
         STCM  R1,15,HRZTOT         SAVE TOT                                    
         B     CALC50                                                           
*                                                                               
CALC20   CLI   HRZBITS,X'04'      + TOT ONLY  -> WGT M + F                      
         BE    CALC40                                                           
         ICM   R1,15,HRZTOT                                                     
         TM    HRZBITS,X'01'       +MEN                                         
         BNO   *+12                                                             
         ICM   R0,15,HRZMEN                                                     
         LA    RF,HRZFDEM          CALC FEMALE BUCKET                           
         TM    HRZBITS,X'02'       +FEMALE                                      
         BNO   *+12                                                             
         ICM   R0,15,HRZFEM                                                     
         LA    RF,HRZMDEM          CALC MALE BUCKET                             
         XC    1(4,RF),1(RF)       CLEAR PREV VALUE                             
         SR    R1,R0               R1=TOT-(MEN OR WOMEN)                        
         BM    CALC_ERX                                                         
         CLI   0(RF),0             UNDEF DEMO                                   
         BE    *+8                                                              
         STCM  R1,15,1(RF)         SAVE CALC MEN/WOMEN                          
         B     CALC50                                                           
*                                                                               
CALC40   DS    0H                  WGT MEN AND WOMEN ON TOTAL                   
         ICM   RF,15,HRZTOT        RF=TOTAL                                     
         BNZ   *+20                IF TOT=0, CLEAR MEN AND WOMEN                
         XC    HRZMEN,HRZMEN                                                    
         XC    HRZFEM,HRZFEM                                                    
         B     CALC50                                                           
*                                                                               
         SR    R1,R1               IF WOMEN UNDEF, SET VAL TO 0                 
         CLI   HRZFDEM,0           UNDEF DEMO?                                  
         BE    CALC45              TRY TO DO MEN                                
         ICM   R1,15,HRZFEM        FIND PREV TOT (MEN+WOMEN)                    
         ICM   RF,15,HRZMEN                                                     
         AR    RF,R1                                                            
         BZ    CALC42                                                           
         ST    RF,WORK             WORK = OLD TOTAL                             
         M     R0,=F'2000'          FEM*2000                                    
         D     R0,WORK                                                          
         ICM   R0,15,HRZTOT                                                     
         MR    R0,R0               NEW TOT * INDEX                              
         D     R0,=F'1000'                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STCM  R1,15,HRZFEM                                                     
         B     CALC45                                                           
*                                                                               
CALC42   DS    0H                  OLD SUM=0, W=NEWTOT/2                        
         ICM   R1,15,HRZTOT                                                     
         M     R0,=F'10'                                                        
         SRL   R1,1                DIVIDE BY 2                                  
         AH    R1,=H'5'                                                         
         D     R0,=F'10'                                                        
         STCM  R1,15,HRZFEM                                                     
*                                                                               
CALC45   XC    HRZMEN,HRZMEN                                                    
         CLI   HRZMDEM,0           MEN NOT DEFINED?                             
         BE    CALC50                                                           
         ICM   RF,15,HRZTOT                                                     
         SR    RF,R1               NEW MEN = TOT - NEW FEM                      
         BP    *+6                                                              
         SR    RF,RF                                                            
         STCM  RF,15,HRZMEN                                                     
*                                                                               
CALC50   CLI   HRZMODF,C'R'        FOR RTGS, MULT BY UNIVS                      
         BNE   CALCX                                                            
*                                                                               
         L     R6,MYDMCB+4         A(UNIVS)                                     
         SR    R0,R0                                                            
         ICM   R1,15,HRZMEN        MALE 'IMP' CALC FROM RTG                     
         ICM   RF,15,0(R6)         MALE UNIV                                    
         BZ    *+10                                                             
         DR    R0,RF               RTG =  'IMP' / UNIV                          
         STCM  R1,15,HRZMEN                                                     
         SR    R0,R0                                                            
         ICM   R1,15,HRZFEM        FEMALE RTG                                   
         ICM   RF,15,4(R6)         FEMALE UNIV                                  
         BZ    *+10                                                             
         DR    R0,RF               RTG = 'IMP' / UNIV                           
         STCM  R1,15,HRZFEM                                                     
         SR    R0,R0                                                            
         ICM   R1,15,HRZTOT        TOT RTG                                      
         MVC   FULL,HRZTOT                                                      
         ICM   RF,15,8(R6)         TOT UNIV                                     
         BZ    *+10                                                             
         DR    R0,RF                                                            
         STCM  R1,15,HRZTOT                                                     
*&&DO                                                                           
* RIPPLING NOT TO BE RELEASED YET                                               
* THIS PART CALCULATES WHAT THE OLD MALE RATINGS WAS                            
* AND ADJUSTS THE MALE IMPRESS ION BY THE PERCETAGE CHANGE                      
*  CALCULATE OLD MALE RATINS TO DETERMINE PERCENTAGE RATINGS CHANGED            
* FORMULA=  ((OLD TOTAL 'IMP')- (OLD FEM 'IMP'))/ (MALE UNIV)    BPOO           
*                                                                               
         ICM   R1,15,SVHRZTOT                                                   
         ICM   RF,15,SVHRZFEM                                                   
         SR    R1,RF                                                            
         SR    R0,R0                                                            
         ICM   RF,15,0(R6)         MALE UNIV                                    
         BZ    *+10                                                             
         DR    R0,RF               R1 HAS OLD MALE RTGS NOW                     
         STCM  R1,15,SVHRZMEN      SAVE OLD MALE RATING                         
         ICM   RF,15,HRZMEN         NEW MEN RTG                                 
*   COMPARE OLD TOTAL WITH NEW TOTAL TO SEE SUBRACT WHICH FROM WHICH            
*  FULL CONTAINS OLD TOTAL                                                      
         CLC   FULL,SVHRZTOT                                                    
         BH    CALC70                                                           
         MVI   DIFFFLAG,DIFFMINS                                                
         SR    R1,RF               OLD TOTAL IS HIGHER SO OLD MALE              
         MH    R1,=H'100'          MUST BE HIGHER                               
         B     CALC80              R1 HAS DIFFERENCE OF OLD AND NEW             
*                                                                               
CALC70   DS    0H                                                               
         MVI   DIFFFLAG,DIFFPLUS                                                
         SR    RF,R1               OLD TOTAL IS HIGHER SO OLD MALE              
         MH    RF,=H'100'          MUST BE HIGHER                               
         LR    R1,RF               R1 NOW HAS THE DIFFERENCE                    
*                                                                               
*  WE CAN POINT R3 TO IMPRESSION SCREEN FIELD AND CALL                          
*  EXISTING PLUS ROUTINE TO GET BINARY NUMBER                                   
*  NEW IMPRESS = (PERCENTAGE CHANGE * OLD IMPRESSION)/100+OLD IMP               
CALC80   DS    0H                                                               
         ICM   RF,15,SVHRZMEN        OLD MEN RTG                                
         SR    R0,R0               PERCENT CHANGE = DIFFERENCE/OLD RTG          
         DR    R0,RF               R1 NOW HAS PERCENTAGE CHANGE                 
*                                                                               
         L     R3,AMALEIMP                                                      
         ST    R1,FULL             FULL HAS PERCENTAGE CHANGE                   
         BAS   RE,PLUS             DMCB+4 HAS MALE IMPRESSION                   
         L     RF,FULL                                                          
*                                  NOW MULTIPLY PERCENTAGE CHANGE WITH          
         L     R1,DMCB+4           OLD MALE IMPRESSION                          
         SR    R0,R0                                                            
         MR    R0,RF               R1 HAS % CHANGE * OLD IMPRESS                
         SR    R0,R0                                                            
         LA    RF,100                                                           
         DR    R0,RF                                                            
         L     RF,DMCB+4                                                        
*  DO WE ADD OR SUBTRACT THE DIFFERENCE                                         
         CLI   DIFFFLAG,DIFFMINS                                                
         BE    CALC110                                                          
         AR    R1,RF                                                            
         B     CALC120                                                          
CALC110  SR    RF,R1                                                            
         LR    R1,RF                                                            
*        DR    R0,RF               R1 NOW HAS THE NEW IMPRESSION                
CALC120  MVC   HRZIMP,HRZMDEM      IMPRESSION# SAME AS MEN                      
         STCM  R1,15,HRZMIMP                                                    
*&&                                                                             
CALCX    B     YES_03                                                           
*                                                                               
CALC_ERX MVI   OURERRCD,IFLDQ                                                   
*                                                                               
CALCNX   B     NO_03                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*RTNVAL - OUTPUTS VALUES IN HRZTBL TO SCREEN FIELDS                             
*        (R2) -> ROW ON SCREEN                                                  
***********************************************************************         
RTNVAL   NTR1                                                                   
         CLI   HRZMODF,C'I'        FILL IMPS FLDS ON SCREEN                     
         BNE   RTNVAL5                                                          
         LA    R6,HRZMDEM                                                       
         LA    R3,DOVIMP1H-DOVDEMOH(R2)                                         
         BAS   RE,RTN10                                                         
         LA    R6,HRZFDEM                                                       
         LA    R3,DOVIMP2H-DOVDEMOH(R2)                                         
         BAS   RE,RTN10                                                         
         LA    R6,HRZTDEM                                                       
         LA    R3,DOVIMP3H-DOVDEMOH(R2)                                         
         BAS   RE,RTN10                                                         
         B     RTNVALX                                                          
*                                                                               
RTNVAL5  CLI   HRZMODF,C'R'        FILL RTGS FIELDS                             
         BNE   RTNVAL10                                                         
         LA    R6,HRZMDEM                                                       
         LA    R3,DOVRTG1H-DOVDEMOH(R2)                                         
         BAS   RE,RTN10                                                         
         LA    R6,HRZFDEM                                                       
         LA    R3,DOVRTG2H-DOVDEMOH(R2)                                         
         BAS   RE,RTN10                                                         
         LA    R6,HRZTDEM                                                       
         LA    R3,DOVRTG3H-DOVDEMOH(R2)                                         
         BAS   RE,RTN10                                                         
*         NEW RIPPLING STUFF DONT RELEASE YET.  BPOO OCT/31/00                  
*&&DO                                                                           
* NEW CHANGE IMPRESSIONS FOR RTG'S CHANGED                                      
         LA    R6,HRZIMP                                                        
         LA    R3,DOVIMP1H-DOVDEMOH(R2)                                         
         BAS   RE,RTN10                                                         
*&&                                                                             
         B     RTNVALX                                                          
*                                                                               
RTNVAL10 CLI   HRZMODF,C'H'        FILL HOMES RTGS FLD                          
         BNE   RTNVALX                                                          
         LA    R6,HRZMDEM                                                       
         LA    R3,DOVHRTGH-DOVDHOMH(R2)                                         
         BAS   RE,RTN10                                                         
         LA    R6,HRZFDEM                                                       
         LA    R3,DOVHSHRH-DOVDHOMH(R2)                                         
         BAS   RE,RTN10                                                         
         LA    R6,HRZTDEM                                                       
         LA    R3,DOVHPUTH-DOVDHOMH(R2)                                         
         BAS   RE,RTN10                                                         
         B     RTNVALX                                                          
*                                                                               
RTNVALX  XIT1                                                                   
                                                                                
*      -------- BAL RE -----------                                              
                                                                                
RTN10    DS    0H                  CALLED W/ BAL, RTN RE                        
         XC    8(L'DOVRTG1,R3),8(R3)  CLEAR FLD                                 
         CLI   0(R6),0             UNDEFINED CELL?                              
         BNE   RTN15                                                            
         MVC   8(L'DOVRTG1,R3),=C'     . '                                      
         NI    4(R3),XFF-X80       TURN OFF INPUT THIS TIME                     
         B     RTN20               NEXT COL                                     
*                                                                               
RTN15    DS    0H                                                               
         EDIT  (B4,1(R6)),(L'DOVRTG1,8(R3)),1,ZERO=NOBLANK                      
         MVI   5(R3),7             SET MAX FLDLN                                
         OI    4(R3),X80           TURN ON INPUT THIS TIME BIT                  
*                                                                               
RTN20    OI    6(R3),X80           TRANSMIT                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*PROFLS - DETERMINE IF PROFILE BITS APPLY TO THIS RECD.                         
*        IF THEY DO, SET SPECIFIC ACTION FLAGS                                  
***********************************************************************         
*                                                                               
PROFLS   DS    0H                                                               
         XC    PRFOVR,PRFOVR       DEFAULT= OVERRIDES ALLOWED                   
*        XC    PRFFTNT,PRFFTNT     DEFAULT=PRINT FOOTNOTES                      
*                                                                               
*ROFOOT  TM    RMPPROFS+RMPFTNTB,RMPFTNTA  SUPRESS FTNOTES ON EST/PRJ?          
*        BNO   PROFACT                                                          
*        CLI   IKTYPBK,C'P'                                                     
*        BE    *+12                                                             
*        CLI   IKTYPBK,C'E'                                                     
*        BNE   PROFACT                                                          
*        MVI   PRFFTNT,1           SET FLAG                                     
*                                                                               
PROFACT  CLI   IKTYPBK,C' '        ACTUAL BK                                    
         BNE   PROFEST                                                          
         TM    RMPPROFS+RMPDABKB,RMPDABKA                                       
         BNO   PROFX                                                            
         B     PROF_YX                                                          
*                                                                               
PROFEST  CLI   IKTYPBK,C'E'        ESTIMATED BK                                 
         BNE   PROFPRJ                                                          
         TM    RMPPROFS+RMPDEBKB,RMPDEBKA                                       
         BNO   PROFX                                                            
         B     PROF_YX                                                          
*                                                                               
PROFPRJ  CLI   IKTYPBK,C'P'        PROJECTED                                    
         BNE   PROFSPC                                                          
         TM    RMPPROFS+RMPDPBKB,RMPDPBKA                                       
         BNO   PROFX                                                            
         B     PROF_YX                                                          
*                                                                               
PROFSPC  CLI   IKTYPBK,C'S'        SPECIAL BK                                   
         BNE   PROFTP                                                           
         TM    RMPPROFS+RMPDSBKB,RMPDSBKA                                       
         BNO   PROFX                                                            
         B     PROF_YX                                                          
*                                                                               
PROFTP   CLI   IKTYPBK,C'T'        TIME PERIOD                                  
         BNE   PROFX                                                            
         TM    RMPPROFS+RMPDTBKB,RMPDTBKA                                       
         BNO   PROFX                                                            
         B     PROF_YX                                                          
*                                                                               
PROF_YX  MVI   PRFOVR,1            DISABLE OVERRIDE CAPABILITY                  
*                                                                               
PROFX    B     XIT_03                                                           
*                                                                               
*----------------------------------------------------------------------         
* LTORG & CONSTANTS FOR SUBR03                                                  
*----------------------------------------------------------------------         
         LTORG                                                                  
         DS    0H                                                               
GETEL3   DS    0H                  "GETEL3  R3,DATADISP,ELCODE"                 
         PRINT OFF                                                              
         AH    R3,DATADISP                                                      
FIRSTEL3 CLI   0(R3),0                                                          
         BNE   *+10                                                             
         CLI   0(R3),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R3)                                                     
         BCR   8,RE                                                             
NEXTEL3  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
         B     FIRSTEL3                                                         
         PRINT ON                                                               
                                                                                
SUBR03L  EQU   *-SUBR03                                                         
         DS    0CL(X'1000'-SUBR03L+1)                                           
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
*======================== SUBROUTINE POOL FOUR =======================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
SUBR04Q  EQU   (((*-RMP19+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP19+SUBR04Q                                                    
SUBR04   NMOD1 0,**1904**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R8             R8=A(SYSD)                                   
         USING SPOOLD,R7           R7=A(SPOOL WORK AREA)                        
                                                                                
         ZIC   R1,GOSUBN                                                        
         SH    R1,=Y(R03#)         SUBTRACT FOR SUB-RTN # 2                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R04_00(R1)                                                       
                                                                                
*CO#     EQU   (R04_01-*)/4+1+R03#  VALIDATE COLUMN OF DEMOS                    
STI#     EQU   (R04_01-*)/4+1+R03#  SAVE (TABLES IN) TIA AREA                   
RTI#     EQU   (R04_02-*)/4+1+R03#  RESTORE (TABLES IN) TIA AREA                
*                                                                               
R04_00   DS    0H                                                               
*04_01   B     VCOL                VALIDATE COLUMN ON SCREEN                    
R04_01   B     SAVETIA             SAVE (TABLES IN) TIA AREA                    
R04_02   B     RSTRTIA             RESTORE (TABLES IN) TIA AREA                 
                                                                                
R04#     EQU   ((*-R04_00)/4+1)+R03#                                            
         DC    H'0'                                                             
                                                                                
YES_04   SR    RC,RC                                                            
*                                                                               
NO_04    LTR   RC,RC                                                            
*                                                                               
XIT_04   XIT1                                                                   
                                                                                
***********************************************************************         
*SAVETIA AND RSTRTIA - COPY IN AND OUT SAVED AREAS FROM PG2                     
***********************************************************************         
                                                                                
* Saves TIA tables into TEMPSTR                                                 
                                                                                
SAVETIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ                                                     
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=Y(TIASVLEN)                                          
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ATIA,0                       
         B     XIT_04                                                           
         SPACE 3                                                                
                                                                                
***********************************************************************         
*RSTRTIA -  Restores TIA tables from TEMPSTR                                    
***********************************************************************         
                                                                                
RSTRTIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ        3RD PARAMETER                                
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=Y(TIASVLEN)                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ATIA,0                      
         B     XIT_04                                                           
         EJECT                                                                  
                                                                                
*&&DO                                                                           
***********************************************************************         
*VCOL - VALIDATE A COLUMN OF DEMOS ON SCREEN                                    
*       INPUT: MYDMCB (2)  = DISP TO COLUMN FROM DEMO NAME FIELD                
*              MYDMCB+2(1) = DISP TO CATAGORY IN DEMOVAL                        
***********************************************************************         
*                                                                               
VCOL     DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   OURINFCD,0                                                       
         CLI   MYDMCB+2,CHUT      PUT LINE PROCESSING?                          
         BNE   *+16                                                             
         LA    R3,DOVHRTGH        VALIDATE HOMES RTG 1ST                        
         LA    R4,DEMOLST                                                       
         B     VCOL15                                                           
*                                                                               
VCOL05   LA    R2,DOVDEMOH         R2-> 1ST LINE ON SCREEN                      
         LA    R4,DEMOLIST                                                      
         ZIC   RF,MYDMCB+2         CTGY (0=MALE, 1=FEMALE, 2=TOTAL)             
         MH    RF,=H'6'                                                         
         AR    R4,RF               R4 PTS TO ENTRY IN DEMOLIST                  
*                                                                               
VCOL10   LR    R3,R2                                                            
         AH    R3,MYDMCB           R3 -> COL POSN ON SCREEN                     
         CLI   0(R4),X'FF'         END OF DEMOLIST?                             
         BE    VCOLX               YES                                          
*                                                                               
VCOL15   TM    4(R3),X80           ANY INPUT TO THIS FIELD?                     
         BNO   VCOL60              FIELD WASN'T TOUCHED -> NXT FLD              
         CLI   2(R4),0             THIS CTGY IS EMPTY ->BYPASS INPUT            
         BE    VCOL60                                                           
         XC    OVAL,OVAL           OVERIDE VALUE (X'FF'=CLR OVR)                
         MVC   ODEM,1(R4)          OVDEM=DEMO MODIF + NUMBER                    
         CLI   5(R3),0             INPUT LN=0?                                  
         BNE   *+12                NO                                           
         MVI   OVAL,X'FF'          CLEAR ANY EXISTING OVERIDES                  
         B     VCOL30              FND OVR IN SVOVR                             
*                                                                               
         ZIC   R1,5(R3)            L'INPUT                                      
         BCTR  R1,0                                                             
         EXCLC R1,8(R3),SPACES     INPUT CLEARED?                               
         BNE   *+12                                                             
         MVI   OVAL,X'FF'          YES                                          
         B     VCOL30                                                           
*                                                                               
         BAS   RE,PLUS             VALIDATE FIELD FOR '+' AND #                 
*        MVI   GOSUBN,RTI#                                                      
*        GOTO1 AGOSUB                                                           
*        DC    H'0'                                                             
*****    MVI   MYTEST,1                                                         
         CLI   DMCB,X'FF'          VALID INPUT TO FIELD?                        
         BE    VCOLINVF            NO, SET ALL MODF FLDS TO MODF NXT            
*****    MVI   MYTEST,2                                                         
         TM    DMCB+4,X80          NEG NUMBER?                                  
         BO    VCOLINVF                                                         
******   MVI   MYTEST,3                                                         
         CLI   1(R4),C'R'          MAX RTG=99.9                                 
         BE    *+8                                                              
         CLI   1(R4),C'P'          MAX PUT=99.9                                 
         BE    *+12                                                             
         CLI   1(R4),C'S'                                                       
         BNE   *+14                                                             
         CLC   =F'999',DMCB+4      IS INPUT TOO LARGE                           
         BL    VCOLINVF                                                         
         CLI   1(R4),C'T'                                                       
         BNE   *+14                                                             
         CLC   =F'999999',DMCB+4                                                
         BL    VCOLINVF                                                         
         MVC   OVAL,DMCB+4         SET OVERIDE VALUE                            
*****    MVI   MYTEST,X'FF'                                                     
*                                                                               
         USING OVRLSTD,RE                                                       
VCOL30   DS    0H                  SEARCH FOR DEMO IN SVOVR LIST                
         LA    RE,SVOVR            PT TO SAVED OVR LIST                         
         XC    DMCB,DMCB                                                        
VCOL35   CLI   0(RE),X'FF'         END OF OVR LIST?                             
         BE    VCOL50              YES                                          
         CLC   OVRIDEM,ODEM        MATCH OF DEMO MODF & NUMBER                  
         BNE   VCOL40                                                           
         CLI   OVAL,X'FF'          DELETE OVR ELEM?                             
         BNE   *+10                                                             
         XC    OVRID(OVRNTY),OVRID CLEAR ENTRY IN SVOVR                         
         MVC   OVRVAL,OVAL         MOVE IN DEMO VALUE                           
         B     VCOL60              BUMP TO IMP OR NEXT ROW'S RTG                
*                                                                               
VCOL40   CLI   OVRID,0             FREE SLOT IN SVOVR LIST?                     
         BNE   *+8                 NO                                           
         ST    RE,DMCB             SAVE IT IN CASE WE NEED TO ADD NTY           
         LA    RE,OVRNTY(RE)       BUMP TO NEXT OVR IN LIST                     
         B     VCOL35                                                           
*                                                                               
VCOL50   CLI   OVAL,X'FF'          WAS THIS A RQST TO CLR OVR?                  
         BE    VCOL60              YES -> OVR DIDN'T EXIST, WE'RE OKAY          
         OC    DMCB,DMCB           NO -> ADD OVR ELEM                           
         BZ    *+8                 DID WE HAVE A(FREE SPOT IN SVOVR)?           
         L     RE,DMCB             IF YES GET IT, ELSE ADD TO EOL               
         LA    R1,SVOVR+SVOVRLN                                                 
         CR    RE,R1                                                            
         BNL   VCOLBF              WE HIT MAX # OVRS FOR BUFFER                 
*                                                                               
         MVC   OVRIDEM,ODEM        MOVE IN MODIF & NUMBER                       
         MVC   OVRVAL,OVAL         MOVE IN OVR VALUE                            
         OC    DMCB,DMCB           DID WE USE A 'FREE' BKT?                     
         BNZ   *+8                 YES DON'T X'FF' EOL                          
         MVI   OVRNTY(RE),X'FF'    MARK END OF LIST                             
         DROP  RE                                                               
*                                                                               
VCOL60   CLI   MYDMCB+2,CHUT       DO ENTIRE HUT LINE DEMOS                     
         BNE   VCOL65                                                           
         LA    R4,3(R4)            NEXT DEMO IN DEMOLST                         
         CLI   1(R4),C'U'          UNVS= LAST DEMO IN HOMES BUFFER              
         BE    VCOLX               END OF HOMES LIST                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         AR    R3,R1                                                            
         TM    1(R3),X'20'         PROT FLD?                                    
         BO    *-10                YES, SKIP IT                                 
         B     VCOL15                                                           
*                                  BUMP TO NEXT ROW RTG IN COLUMN               
VCOL65   CLI   1(R4),C'R'          DID WE JUST PROCESS RTG?                     
****     BNE   *+16                NO, WE DID IMP                               
         BNE   VCOL100                                                          
*        MVI   GOSUBN,RTI#                                                      
*        GOTO1 AGOSUB                                                           
*        DC    H'0'                                                             
         LR    RE,R3               A(CURRENT RTG SCREEN FIELD)                  
         LA    R3,DOVIMP1H-DOVRTG1H(R3) GO DO IMPS FOR THIS DEMO                
         TM    4(RE),X'80'         ONLY IF RATINGS CHANGED DO THE IMPS          
         BNO   VCOL95                                                           
***      CLC   MYDMCB+2(1),COL                                                  
***      BNE   *+8                                                              
***      OI    HRZBITS,X'01'        MALE  CHANGED                               
***      CLC   MYDMCB+2(1),COL+1                                                
***      BNE   *+8                                                              
***      OI    HRZBITS,X'02'        FEMALE CHANGED                              
***      CLC   MYDMCB+2(1),COL+2                                                
***      BNE   *+10                                                             
***      OI    HRZBITS,X'04'        TOT CHANGED                                 
***      DC    H'0'                                                             
*                                                                               
*  NOW CHANGE THE IMPRESSION FIELD ON THE SCREEN                                
         LR    R0,R3             SAVE R3  SO WE CAN CALL PLUS ROUTINE           
         LR    R1,RE             R1=A(CURRENT RATINGS SCREEN FIELD)             
         MVC   SVHRZMEN,DMCB+4   DMCB+4 MUST BE NEW RATINGS VALIDATED           
*                                ALREADY                                        
****     LA    RF,DOVHRTGH       RE=A(CURRENT RATINGS), RF=A(DOVHRTGH)          
*****    LA    RF,DOVRTG1H       RE=A(CURRENT RATINGS), RF=A(DOVHRTGH)          
         LA    RF,DOVDEMOH       RE=A(CURRENT RATINGS), RF=A(DOVHRTGH)          
         SR    R1,RF             DISPLACEMENT                                   
         L     RF,ASCRNTAB                                                      
***      LA    RF,SCRNTAB                                                       
         AR    RF,R1             ADD DISPACEMENT TO SAVED SCREEN TABLE          
*                                TO GET RATINGS FROM SAVED SCREEN TABLE         
         ZIC   RE,0(RF)          TOTAL LENGTH                                   
         AHI   RE,-8             SUBTRACT HEADER LENTH AND STICK IT             
         STC   RE,5(RF)          5 BEYOND HEADER AS INPUT LENGTH                
*                                                                               
         LR    R3,RF             POINT R3 TO OLD RATINGS VALUE BEFORE           
         BAS   RE,PLUS           CHANGE TO SCREEN                               
         CLI   DMCB,X'FF'        OLD RATINGS BETTER BE VALID OR ELSE            
*        BNE   *+6               SOMETHING IS REALLY WRONG                      
*        DC    H'0'                                                             
         BNE   *+8                                                              
         B     VCOLINVF                                                         
         LR    R3,R0             LETS RESTORE R3                                
*                                                                               
         ICM   R1,15,SVHRZMEN       NEW RATING                                  
         L     RF,DMCB+4         OLD RATING                                     
         CR    R1,RF                                                            
         BL    VCOL80                                                           
* NEW RATING HERE MUST HAVE BEEN HIGHER THAN OLD RATING                         
         MVI   DIFFFLAG,DIFFPLUS                                                
         SR    R1,RF             CALCULATE DIFFERENCE                           
         MH    R1,=H'100'                                                       
         SR    R0,R0                                                            
         DR    R0,RF             R1 NOW HAS PERCENTAGE CHANGE                   
         B     VCOL90                                                           
*                                                                               
VCOL80   DS    0H                                                               
* OLD RATING HERE MUST HAVE BEEN HIGHER THAN NEW RATING                         
         MVI   DIFFFLAG,DIFFMINS                                                
         ICM   RF,15,SVHRZMEN       NEW RATING                                  
         L     R1,DMCB+4         OLD RATING                                     
         SR    R1,RF             CALCULATE DIFFERENCE                           
         MH    R1,=H'100'                                                       
         SR    R0,R0                                                            
         DR    R0,RF             R1 NOW HAS PERCENTAGE CHANGE                   
*                                                                               
* MULTIPLY PERCENTAGE CHANGE WITH IMPRESSIONS FIELD                             
* THEN ADD OR SUBTRACT FROM OLD IMPRESSIONS FIELD                               
VCOL90   DS    0H                                                               
         ST    R1,FULL            FULL NOW HAS PERCENTAGE CHANGE                
*    RIGHT NOW R3 HAD ALREADY BEEN RESTORED TO POINT TO IMPRESSION              
*    FIELD,SO WE CALL SAFELY CALL PLUS ROUTINE TO GET BINARY IMPRESSION         
         BAS   RE,PLUS                                                          
         CLI   DMCB,X'FF'         OLD IMPRESSION BETTER BE GOOD OR ELSE         
         BNE   *+6                SOMETHING IS WRONG                            
         DC    H'0'                                                             
         L     R1,FULL            PERCENTAGE CHANGE                             
         L     RF,DMCB+4          IMPRESSION                                    
         SR    R0,R0                                                            
         MR    R0,RF                                                            
         ST    R1,FULL             R1 NOW HAS TOTAL DIFF OF IMPRESSION          
         L     RF,DMCB+4           IMPRESSION                                   
*  COMPARE TO SEE IF NEW IMPRESSION SHOULD ADD OR SUBTRACT DIFFERENCE           
         CLI   DIFFFLAG,DIFFPLUS                                                
         BNE   VCOL92                                                           
*******  DC    H'0'                                                             
*                                                                               
VCOL91   AR    RF,R1                                                            
         B     VCOL93                                                           
*                                                                               
VCOL92   SR    RF,R1                                                            
*                                                                               
VCOL93   DS    0H                                                               
         XC    8(L'DOVRTG1,R3),8(R3)           RF HAS IMPRESSION                
         LR    R1,RF                           NEEDTO DIVIDE BY 100             
         LA    RF,100                                                           
         SR    R0,R0                                                            
         DR    R0,RF                                                            
         ST    R1,FULL                          R1 HAS NEW IMPRESSION           
                                                                                
         EDIT  (B4,FULL),(7,8(R3)),1,ZERO=NOBLANK     DISP NEW IMP              
         MVI   5(R3),7                                                          
*        MVC   8(3,R3),=C'100'                                                  
         MVI   4(R3),X'80'                                                      
******   MVI   MYTEST,0                                                         
*                                                                               
*   AT THIS POINT LETS GET OLD RATINGS FROM SAVED SCRNTABLE AND SEE             
*   WHAT THE DIFFERENCE OF THE CHANGE WAS                                       
*                                                                               
VCOL95   DS    0H                                                               
         LA    R4,3(R4)            GO TO IMP IN DEMOLIST                        
         B     VCOL15                                                           
VCOL100  DS    0H                                                               
*                                  BUMP TO NEXT ROW RTG IN COLUMN               
         LA    R4,5*3(R4)          FROM IMP BUMP TO RTG OF NEXT LINE            
         LA    R2,DOVDEM2H-DOVDEMOH(R2) NEXT LINE ON SCREEN                     
         LA    R0,DOVDEMXH         END OF SCREEN                                
         CR    R2,R0               LAST DEMO ON SCREEN?                         
         BNH   VCOL10              NOT YET                                      
*                                                                               
VCOLX    B     YES_03                                                           
*                                                                               
VCOLINVF ST    R3,DMCB             RETURN ADDRESS OF FIELD                      
         MVI   OURERRCD,IFLDQ      SET TO INVALID FIELD                         
***      DC    H'0'                                                             
         B     NO_03                                                            
*                                                                               
VCOLBF   ST    R3,DMCB             RETURN ADDRESS OF FIELD                      
         MVI   OURINFCD,TOOVRQ     SET TO INVALID FIELD                         
         B     NO_03                                                            
MYTEST   DS    X                                                                
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SUBR04--MISC STUFF'            
*--------------------- SUBR04 MISCELLANEOUS STUFF --------------------*         
                                                                                
         DS    0H                                                               
                                                                                
SUBR04L  EQU   *-SUBR04                                                         
         DS    0CL(X'1000'-SUBR04L+1)                                           
***********************************************************************         
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SUBRXM)'                       
***********************************************************************         
*==================== EXIT WITH MESSAGE ROUTINES =====================*         
                                                                                
* Exits which leave RMP19 entirely and displays a message go through            
*  this routine.                                                                
                                                                                
SUBRXMQ  EQU   (((*-RMP19+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP19+SUBRXMQ                                                    
XMSGRTN  NMOD1 0,**19XM**                                                       
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
                                                                                
XIT_XM   XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
*                                                                               
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SUBRXM--ERRMSGS)'              
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
INVOQ    EQU   ((XMERR04-XMERR00)/4)+1                                          
IOCBQ    EQU   ((XMERR05-XMERR00)/4)+1                                          
TMODQ    EQU   ((XMERR06-XMERR00)/4)+1                                          
MODQ     EQU   ((XMERR07-XMERR00)/4)+1                                          
IODVQ    EQU   ((XMERR08-XMERR00)/4)+1                                          
IUPGQ    EQU   ((XMERR09-XMERR00)/4)+1                                          
ROMQ     EQU   ((XMERR00-XMERR00)/4)+1                                          
DUPOQ    EQU   ((XMERR11-XMERR00)/4)+1                                          
INVDQ    EQU   ((XMERR12-XMERR00)/4)+1                                          
OPTNAQ   EQU   ((XMERR13-XMERR00)/4)+1                                          
NHDRQ    EQU   ((XMERR14-XMERR00)/4)+1                                          
                                                                                
*                                                                               
XMERR00  DS    0H                                                               
XMERR01  B     MFLD                MISSING INPUT FIELD                          
XMERR02  B     IFLD                INVALID INPUT FIELD                          
XMERR03  B     RNF                 RECORD NOT FOUND                             
XMERR04  B     INVO                INVALID OPTION KEYWORD                       
XMERR05  B     IOCB                INVALID OPTION COMBINATION                   
XMERR06  B     TMOD                TOO MANY OPTION DATA VALUES                  
XMERR07  B     MOD                 MISSING OPTION DATA                          
XMERR08  B     IODV                INVALID OPTION DATA VALUE                    
XMERR09  B     IUPG                INVALID UPGRADE EXPRESSION                   
XMERR10  B     ROPTMISS            REQUIRED OPTION MISSING                      
XMERR11  B     DUPOPT              DUPLICATED OPTION KEYWORD                    
XMERR12  B     INVDEMO             INVALID DEMO EXPRESSION                      
XMERR13  B     OPTNAV              OPTION NOT AVAILABLE                         
XMERR14  B     NHDR                INVENTORY HEADER NOT FOUND                   
*                                                                               
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
INVDEMO  DS    0H                  INVALID DEMO NUMBER                          
         MVC   MSGNUM2,=Y(233)                                                  
         B     ERRGTXT                                                          
*                                                                               
OPTNAV   DS    0H                  OPTION NOT AVAIL                             
         MVC   MSGNUM2,=Y(26)                                                   
         B     ERRGTXT                                                          
*                                                                               
NHDR     DS    0H                  INVENTORY HEADER NOT FOUND                   
         MVC   MSGNUM2,=Y(RR#HDRNF)                                             
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
         CLI   MYTEXT,0             ANY REPLACE TEXT?                           
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT         YES, PUT LENGTH IN                         
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT            AS WELL AS THE ADDR OF THE TEXT           
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
                                                                                
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SUBRXM--WRN MSGS'              
*-------------------------- WARNING MESSAGES -------------------------*         
                                                                                
* At entry, R2-->PWMMEDH.                                                       
                                                                                
XMWRN    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
                                                                                
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
XMWRN01  B     WTOP                TOP OF SCHEDULE                              
XMWRN02  B     WBOT                BOTTOM OF SCHEDULE                           
WRNX#    EQU   ((*-XMWRN00)/4)+1                                                
                                                                                
XMWRNQ   EQU   ((*-XMWRN00)/4)+1                                                
         EJECT                                                                  
WTOP     MVC   MSGNUM2,HALF             TOP OF SCHEDULE                         
         B     WRNGTXT                                                          
*                                                                               
WBOT     MVC   MSGNUM2,HALF             BOTTOM OF SCHEDULE                      
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
         CLI   MYTEXT,0             ANY REPLACE TEXT?                           
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT         YES, PUT LENGTH IN                         
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT            AS WELL AS THE ADDR OF THE TEXT           
         DROP  R1                                                               
         B     WRNEXIT                                                          
                                                                                
                                                                                
WRNEXIT  DS    0H                                                               
         GOTO1 ERREX                                                            
         B     XIT_XM                                                           
*---------------------------- INFO MESSAGES --------------------------*         
                                                                                
* At entry, R2-->appropriate field to put cursor on.                            
                                                                                
XMINF    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         XC    ACURFORC,ACURFORC                                                
         LA    R2,CONHEADH                                                      
         LA    R1,CONHEAD                                                       
                                                                                
         DS    0H                                                               
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   OURINFCD,INFX#                                                   
         BL    XMINFGO                                                          
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         CLI   OURINFCD,INFX3#                                                  
         BL    XMINFGO                                                          
         DC    H'0'                                                             
                                                                                
XMINFGO  DS    0H                                                               
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
                                                                                
DUMBQ    EQU   ((XMINF01-XMINF00)/4)+1                                          
TOOVRQ   EQU   ((XMINF02-XMINF00)/4)+1                                          
NODEMQ   EQU   ((XMINF03-XMINF00)/4)+1                                          
CHGDQ    EQU   ((XMINF04-XMINF00)/4)+1                                          
TOLNGQ   EQU   ((XMINF05-XMINF00)/4)+1                                          
NOOVRQ   EQU   ((XMINF06-XMINF00)/4)+1                                          
                                                                                
XMINF00  DS    0H                                                               
*                                                                               
INFX#    EQU   ((*-XMINF00)/4)+1                                                
*                                                                               
XMINF01  B     DUMB                                                             
XMINF02  B     TOVR                                                             
XMINF03  B     NODM                                                             
XMINF04  B     CHGD                                                             
XMINF05  B     TOLG                                                             
XMINF06  B     NOOVR                                                            
INFX3#   EQU   ((*-XMINF00)/4)+1                                                
                                                                                
XMINFQ   EQU   ((*-XMINF00)/4)+1                                                
                                                                                
         EJECT                                                                  
DUMB     DS    0H                       TOO MANY OVERIDES                       
         MVC   0(18,R1),=C'THIS IS A DUMB MSG'                                  
         B     INFGTXT                                                          
                                                                                
TOVR     DS    0H                       TOO MANY OVERIDES                       
         MVC   0(18,R1),=C'Too many overrides'                                  
         B     INFGTXT                                                          
                                                                                
NODM     DS    0H                       NO DEMOS FOUND. NO OVRRIDES             
         MVC   0(36,R1),=C'No demos found. Overrides disallowed'                
         B     INFGTXT                                                          
*                                                                               
CHGD     DS    0H                                                               
         MVC   0(38,R1),=C'Record updated, override changes saved'              
         B     INFGTXT                                                          
*                                                                               
TOLG     DS    0H                                                               
         MVC   0(32,R1),=C'Recd too long, upgrade not done.'                    
         B     INFGTXT                                                          
*                                                                               
NOOVR    DS    0H                                                               
         MVC   0(36,R1),=C'Recd displayed. Changes not allowed.'                
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
         CLI   MYTEXT,0             ANY REPLACE TEXT?                           
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT         YES, PUT LENGTH IN                         
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT            AS WELL AS THE ADDR OF THE TEXT           
         DROP  R1                                                               
         B     INFEXIT                                                          
                                                                                
INFEXIT  DS    0H                                                               
         OC    ACURFORC,ACURFORC   NEED TO SET CURSOR?                          
         BNZ   INFEXITX             NOPE                                        
         MVC   ACURFORC,AFRSTKEY   PLACE CURSOR ON 1ST KEY FIELD,               
                                                                                
INFEXITX DS    0H                                                               
         MVI   ERROR,0                                                          
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
*--------------------- SUBRXM MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBRXML  EQU   *-XMSGRTN                                                        
         DS    0CL(X'1000'-SUBRXML+1)                                           
***********************************************************************         
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE'                                
***********************************************************************         
*========================== RMP19's EQUATES ==========================*         
*                                                                               
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
MXLTNTRY EQU   80                  MAX ENTRIES FOR LIST TABLE                   
PAGEQ    EQU   2                   TWA PAGE # FOR TEMPSTR                       
IKYINVL  EQU   RINVKINV-RINVREC+L'RINVKINV                                      
TIASVLEN EQU   BIGENDSV-BIGAREA                                                 
                                                                                
*                                 *********** OPTION EQUATES **********         
OPNUPGD  EQU   1                   UPGRADE                                      
OPBUPGD  EQU   X'00000001'                                                      
OPNMIN   EQU   2                   MINIMUM VALUE                                
OPBMIN   EQU   X'00000002'                                                      
OPNDEM   EQU   3                   DEM                                          
OPBDEM   EQU   X'00000004'                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0XL(2+2+2)                                                       
         DC    AL2(GOSUB-RMP19,0,AGOSUB-SYSD)                                   
         DC    AL2(SUBR01-RMP19,0,ASUBR01-SYSD)                                 
         DC    AL2(SUBR02-RMP19,0,ASUBR02-SYSD)                                 
         DC    AL2(SUBR03-RMP19,0,ASUBR03-SYSD)                                 
         DC    AL2(SUBR04-RMP19,0,ASUBR04-SYSD)                                 
         DC    AL2(XMSGRTN-RMP19,0,AXMSGRTN-SYSD)                               
         DC    AL2(DAYTAB-RMP19,0,ADAYTAB-SYSD)                                 
         DC    AL2(DEMHOMES-RMP19,0,ADEMHOM-SYSD)                               
         DC    AL2(ORIGDEMS-RMP19,0,AORGDEM-SYSD)                               
         DC    AL2(OPTTABLE-RMP19,0,AOPTTAB-SYSD)                               
         DC    AL2(DEMTAB-BIGAREA,ATIA-GEND,ADEMTAB-SYSD)                       
         DC    AL2(SVDEM-BIGAREA,ATIA-GEND,ASVDEM-SYSD)                         
         DC    AL2(DEMWRK-BIGAREA,ATIA-GEND,ADEMWRK-SYSD)                       
         DC    AL2(DBLOCK1-BIGAREA,ATIA-GEND,ADBLOCK-SYSD)                      
         DC    AL2(DBXTND1-BIGAREA,ATIA-GEND,ADBXTND-SYSD)                      
         DC    AL2(SCRNTAB-BIGAREA,ATIA-GEND,ASCRNTAB-SYSD)                     
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
LBLTAB   DS    0XL(2+8)            TABLE OF LABELS FOR BIG STORAGE              
         DC    AL2(DEMTTBL-BIGAREA),CL8'*DEMTAB*'                               
         DC    AL2(SVDEMTBL-BIGAREA),CL8'*SVDEMT*'                              
         DC    AL2(DEMWRKTB-BIGAREA),CL8'*DEMWRK*'                              
         DC    AL2(DBLKLABL-BIGAREA),CL8'*DBLOCK*'                              
         DC    AL2(DBXTLABL-BIGAREA),CL8'*DBXTND*'                              
         DC    AL2(SCRNTABL-BIGAREA),CL8'*SCRNTA*'                              
LBLTABQ  EQU   (*-LBLTAB)/(L'LBLTAB)                                            
                                                                                
                                                                                
TABCLR   DS    0XL(1+2+2)          STORAGES TO CLEAR                            
         DC    C'I',AL2(SVDEM-BIGAREA),AL2(SVDEMX-SVDEM)                        
         DC    C'I',AL2(DBLOCK1-BIGAREA),AL2(DBLOCK1X-DBLOCK1)                  
         DC    C'I',AL2(DBXTND1-BIGAREA),AL2(DBXTND1X-DBXTND1)                  
         DC    C'S',AL2(DEMOLST-SYSD),AL2(DEMOLSTX-DEMOLST)                     
         DC    C'S',AL2(DEMOVLS-SYSD),AL2(DEMOVALX-DEMOVLS)                     
         DC    C'S',AL2(MYTEXT-SYSD),AL2(MYTEXTX-MYTEXT)                        
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
                                                                                
                                                                                
MGFOPT   DS    0AL2                TABLE OF TWA OPTION FIELDS TO MERGE          
         DC    AL2(DOVOPT1H-T810FFD)                                            
         DC    AL2(DOVOPT2H-T810FFD)                                            
         DC    AL2(0)                                                           
MGFOPTX  EQU   *                                                                
                                                                                
*-HOMES LIST TO PASS TO DEMOUT                                                  
DEMHOMES DS    0CL18                                                            
         DC    AL1(0),CL1'R',AL1(1)    RTG                                      
         DC    AL1(0),CL1'T',AL1(1)    IMPS                                     
         DC    AL1(0),CL1'S',AL1(1)    SHR                                      
         DC    AL1(0),CL1'P',AL1(1)    PUT                                      
         DC    AL1(0),CL1'U',AL1(1)    UNIV                                     
         DC    AL1(0),AL1(0),AL1(0)    BLANK (FILLER)                           
         EJECT                                                                  
***********************************************************************         
*ORIGDEMS - DEMO NAME TO PUT ON SCRN.  ORDER= MALE#, FEMALE#, TOTAL#            
***********************************************************************         
*                                                                               
ORIGDEMS DS    0XL13                                                            
         DC    AL1(02),CL6'18+   ',AL1(95,45,145),CL3'ccc'    1st pg            
         DC    AL1(03),CL6'18-34 ',AL1(91,41,141),CL3'ccc'                      
         DC    AL1(04),CL6'18-49 ',AL1(92,42,142),CL3'ccc'                      
         DC    AL1(05),CL6'21-49 ',AL1(115,71,191),CL3'ccc'                     
         DC    AL1(06),CL6'25-49 ',AL1(97,47,147),CL3'cc '                      
         DC    AL1(07),CL6'25-54 ',AL1(98,48,148),CL3'ccc'                      
         DC    AL1(08),CL6'25-64 ',AL1(99,49,149),CL3'cc '                      
         DC    AL1(09),CL6'2-11  ',AL1(20,119,122),CL3'  c'                     
         DC    AL1(10),CL6'6-11  ',AL1(22,21,123),CL3'  c'                      
         DC    AL1(11),CL6'12-17 ',AL1(75,25,125),CL3'  c'                      
*                                                                               
         DC    AL1(12),CL6'MET-A ',AL1(0,0,2),CL3'  c'        2nd pg            
         DC    AL1(13),CL6'MET-B ',AL1(0,0,3),CL3'  c'                          
         DC    AL1(14),CL6'WWRK  ',AL1(0,65,0),CL3' c '                         
         DC    AL1(15),CL6'2+    ',AL1(77,27,127),CL3'  c'                      
         DC    AL1(16),CL6'12-24 ',AL1(78,28,128),CL3' cc'                      
         DC    AL1(17),CL6'12-34 ',AL1(79,29,129),CL3' cc'                      
ORIGCX   EQU   *-ORIGDEMS                        END OF CORE CELLS              
*                                                       KIDS GROUPS             
         DC    AL1(18),CL6'2-5   ',AL1(217,197,121),CL3'   '                    
         DC    AL1(19),CL6'12-20 ',AL1(111,23,187),CL3'   '                     
         DC    AL1(20),CL6'12-49 ',AL1(080,30,130),CL3'   '                     
         DC    AL1(21),CL6'12-54 ',AL1(081,31,131),CL3'   '                     
         DC    AL1(22),CL6'12-64 ',AL1(082,32,132),CL3'   '                     
         DC    AL1(23),CL6'12+   ',AL1(083,33,133),CL3'   '                     
*                                                      18 - AGE GROUP           
         DC    AL1(24),CL6'18-20 ',AL1(112,68,188),CL3'   '                     
         DC    AL1(25),CL6'18-24 ',AL1(090,40,140),CL3'   '                     
         DC    AL1(26),CL6'18-54 ',AL1(093,43,143),CL3'   '                     
         DC    AL1(27),CL6'18-64 ',AL1(094,44,144),CL3'   '                     
*                                                       21 - AGE GROUP          
         DC    AL1(28),CL6'21-24 ',AL1(113,69,189),CL3'   '                     
         DC    AL1(29),CL6'21-34 ',AL1(114,70,190),CL3'   '                     
         DC    AL1(30),CL6'21-54 ',AL1(116,72,192),CL3'   '                     
         DC    AL1(31),CL6'21-64 ',AL1(117,73,193),CL3'   '                     
         DC    AL1(32),CL6'21+   ',AL1(118,67,194),CL3'   '                     
*                                                       25 - AGE GROUP          
         DC    AL1(33),CL6'25-34 ',AL1(096,46,146),CL3'   '                     
         DC    AL1(34),CL6'25+   ',AL1(100,50,150),CL3'   '                     
*                                                       35 - AGE GROUP          
         DC    AL1(35),CL6'35-49 ',AL1(101,51,151),CL3'   '                     
         DC    AL1(36),CL6'35-54 ',AL1(102,52,152),CL3'   '                     
         DC    AL1(37),CL6'35-64 ',AL1(103,53,153),CL3'   '                     
         DC    AL1(38),CL6'35+   ',AL1(104,54,154),CL3'   '                     
*                                                                               
         DC    AL1(39),CL6'50-54 ',AL1(105,55,155),CL3'   '                     
         DC    AL1(40),CL6'50-64 ',AL1(106,56,156),CL3'   '                     
         DC    AL1(41),CL6'50+   ',AL1(107,57,157),CL3'   '                     
         DC    AL1(42),CL6'55-64 ',AL1(108,58,158),CL3'   '                     
         DC    AL1(43),CL6'55+   ',AL1(109,59,159),CL3'   '                     
         DC    AL1(44),CL6'65+   ',AL1(110,60,160),CL3'   '                     
         DC    AL1(45),CL6'2-17  ',AL1(0,0,172),CL3'   '                        
         DC    AL1(46),CL6'2-24  ',AL1(0,0,173),CL3'   '                        
         DC    AL1(47),CL6'2-34  ',AL1(0,0,174),CL3'   '                        
         DC    AL1(48),CL6'2-49  ',AL1(0,0,175),CL3'   '                        
         DC    AL1(49),CL6'2-54  ',AL1(0,0,176),CL3'   '                        
         DC    AL1(50),CL6'2-64  ',AL1(0,0,177),CL3'   '                        
*                                                    V6 - VIEWERS ONLY          
         DC    AL1(51),CL6'6-17  ',AL1(0,0,179),CL3'   '                        
         DC    AL1(52),CL6'6-24  ',AL1(0,0,180),CL3'   '                        
         DC    AL1(53),CL6'6-34  ',AL1(0,0,181),CL3'   '                        
         DC    AL1(54),CL6'6-49  ',AL1(0,0,182),CL3'   '                        
         DC    AL1(55),CL6'6-54  ',AL1(0,0,183),CL3'   '                        
         DC    AL1(56),CL6'6-64  ',AL1(0,0,184),CL3'   '                        
         DC    AL1(67),CL6'6+    ',AL1(0,0,185),CL3'   '                        
ORIGDEMX DC    X'FF'                                                            
ORIGDEML EQU   *-ORIGDEMS            L'ORIGINAL DEMO TABLE                      
ORIGDMQ  EQU   (ORIGDEML-1)/L'ORIGDEMS                                          
*                                                                               
         EJECT                                                                  
*--------------------------- OPTIONS TABLE ---------------------------*         
                                                                                
OPTTABLE DS    0X                  SEE OPTDSECT                                 
*                                                                               
OP01     DS    0X                  UPT=  (SAME AS MIN)                          
         DC    AL1(OPNUPGD),AL1(OP01X-OP01)                                     
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VUPG#,0)                                                     
         DC    AL4(OPBUPGD)                                                     
         DC    AL4(0)                                                           
         DC    AL4(OPBMIN)                                                      
         DC    AL1(OPVMINMX),AL1(L'OPVMINTB),AL2(OPVMINNM-SYSD)                 
         DC    AL2(OP01NAM1-OP01,0)                                             
OP01NAM1 DC    AL1(OP01NAMX-OP01NAM1-1),C'UPT'                                  
OP01NAMX DC    AL1(EOT)                                                         
OP01X    EQU   *                                                                
*                                                                               
OP02     DS    0X                  MIN=                                         
         DC    AL1(OPNMIN),AL1(OP02X-OP02)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VMIN#,0)                                                     
         DC    AL4(OPBMIN)                                                      
         DC    AL4(0)                                                           
         DC    AL4(OPBUPGD)                                                     
         DC    AL1(OPVMINMX),AL1(L'OPVMINTB),AL2(OPVMINNM-SYSD)                 
         DC    AL2(OP02NAM1-OP02,0)                                             
OP02NAM1 DC    AL1(OP02NAMX-OP02NAM1-1),C'MIN'                                  
OP02NAMX DC    AL1(EOT)                                                         
OP02X    EQU   *                                                                
*                                                                               
OP03     DS    0X                  DEMOS=                                       
         DC    AL1(OPNDEM),AL1(OP03X-OP03)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VDEM#,0)                                                     
         DC    AL4(OPBDEM)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVDEMMX),AL1(L'OPVDEMTB),AL2(OPVDEMNM-SYSD)                 
         DC    AL2(OP03NAM1-OP03,0)                                             
OP03NAM1 DC    AL1(OP03NAMX-OP03NAM1-1),C'DEMOS'                                
OP03NAMX DC    AL1(EOT)                                                         
OP03X    EQU   *                                                                
*                                                                               
OPTZ     DC    AL1(EOT)                                                         
***********************************************************************         
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (RERMPWORKD)'                   
***********************************************************************         
*============================= RERMPWORKD ============================*         
       ++INCLUDE RERMPWORKD                                                     
***********************************************************************         
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (SYSD)'                         
***********************************************************************         
*========================== SYSD'S SYSSPARE ==========================*         
*                                                                               
*                                  RERMPITSYP HAS ORG ONTO SYSSPARE             
SYSD     DSECT                                                                  
       ++INCLUDE RERMPITSYP                                                     
*                                 ************** WORK AREA ************         
RELO     DS    F                                                                
MYDMCB   DS    6F                                                               
SVDMCB   DS    6F                                                               
HRZTBL   DS    0XL17               HORIZONTAL DEMO PROCESSING                   
HRZBITS  DS    X                   '01'=+MALE, '02'=+FEMALE, '04'=+TOT          
HRZMODF  DS    X                   MODIFIER 'R'/'I'                             
HRZMDEM  DS    X                   MALE DEMO#                                   
HRZMEN   DS    XL4                 MALE DEMO VALUE                              
HRZFDEM  DS    X                   FEMALE DEMO#                                 
HRZFEM   DS    XL4                 FEMALE DEMO VALUE                            
HRZTDEM  DS    X                   TOTAL DEMO#                                  
HRZTOT   DS    XL4                 TOTAL DEMO VALUE                             
HRZIMP   DS    X                   MALE DEMO#                                   
HRZMIMP  DS    XL4                 MALE IMPRESSION VALUE                        
SVHRZMEN DS    XL(L'HRZMEN)                                                     
SVHRZFEM DS    XL(L'HRZFEM)                                                     
SVHRZTOT DS    XL(L'HRZTOT)                                                     
DIFFFLAG DS    XL1                                                              
DIFFPLUS EQU   0                                                                
DIFFMINS EQU   1                                                                
*                                 ************** ADDRESSES ************         
MYBASE1  DS    A                   A(1ST 4096 BYTES OF THIS PROGRAM)            
MYBASE2  DS    A                   A(2ND 4096 BYTES OF THIS PROGRAM)            
ASUBRTN  DS    A                   A(SUBROUTINE POOL TO USE)                    
AERREX   DS    A                   A(ERREX ROUTINE) TO USE                      
AM1STREC DS    A                   MY OWN AFRSTREC                              
AOPTNTRY DS    A                   A(OPTTABLE ENTRY)                            
ANLTNTRY DS    A                   A(NEXT AVAILABLE SLOT IN LIST TABLE)         
PRVFDSP  DS    H                   FIELD DISP FROM PREV TRANSACTION             
         DS    H                   SPARE TO KEEP FULL-WORD ALIGNMENT            
                                                                                
         DS    0A                  ADDRESSES OF TABLES/ROUTINES                 
AGOSUB   DS    A                    A(SUBROUTINE POOL INTERFACE)                
ASUBR01  DS    A                    A(SUBROUTINE POOL #1)                       
ASUBR02  DS    A                    A(SUBROUTINE POOL #2)                       
ASUBR03  DS    A                    A(SUBROUTINE POOL #3)                       
ASUBR04  DS    A                    A(SUBROUTINE POOL #3)                       
AXMSGRTN DS    A                    A(EXIT W/ MSG ROUTINE)                      
ADEMHOM  DS    A                    A(DEMHOMES)                                 
AORGDEM  DS    A                    A(ORIGDEMS)                                 
ADAYTAB  DS    A                    A(DAYTAB)                                   
AOPTTAB  DS    A                    A(OPTTABLE)                                 
ADEMTAB  DS    A                    A(DEMTAB)                                   
ASVDEM   DS    A                    A(SAVED DEMO ELEMS)                         
ADEMWRK  DS    A                    A(DEMO WORK AREA)                           
ADBLOCK  DS    A                    A(DBLOCK)                                   
ADBXTND  DS    A                    A(DBEXTEND)                                 
ADEMAINT DS    A                    A(DEMAINT)                                  
AMALEIMP DS    A                                                                
ASCRNTAB DS    A                    A(SCRNTAB)                                  
                                                                                
*                                 ************* INPUT DATA ************         
INPUTKEY DS    0C                  INPUT TO KEY FIELDS                          
IKSTTN   DS    CL5                  STATION                                     
IKINV    DS    CL4                  INVENTORY NUMBER                            
IKEFFDB  DS    XL3                  EFFECTIVE DATE (BINARY)                     
IKEFFDC  DS    XL2                  EFFECTIVE DATE (COMPRESSED)                 
IKRSVC   DS    CL3                  RATING SERVICE                              
IKQLFK   DS    XL1                  QUALIFIER CODE FOR RINVKQLF                 
IKFRBK   DS    0XL3                 FROM BOOK                                   
IKFRBTS  DS    XL1                   BITS                                       
IKBOOK   DS    XL2                   BOOK                                       
IKBTYPE  DS    XL1                 BOOK TYPE (B/H/M/E/T.)                       
IKTYPBK  DS    XL1                  BK PREFIX/ QLFYR (E/P/S/T)                  
*                                 ************ HEADER INFO                      
************                                                                    
HDRINFO  DS    0X                                                               
HDRIDAY  DS    XL1                 (INTERNAL) DAY                               
HDRTIME  DS    0XL4                TIMES                                        
HDRSTIM  DS    XL2                  START                                       
HDRETIM  DS    XL2                  END                                         
HDRPRGNA DS    CL27                PROGRAM NAME                                 
HDRFOOT  DS    CL16                FOOTNOTE                                     
HDRINFOX EQU   *                                                                
HDRINFOL EQU   HDRINFOX-HDRINFO                                                 
TMPBDATE DS    XL3                                                              
TMPIDAY  DS    XL1                                                              
TMPSETM  DS    XL4                                                              
                                                                                
NLTNTRY  DS    XL1                 # OF LIST TABLE ENTRIES                      
GOSUBN   DS    XL1                 SUBROUTINE NUMBER                            
MYRDUPDT DS    XL1                 MY VERSION OF RDUPDATE                       
MSGNUM2  DS    XL2                                                              
MSGSYS   DS    XL1                                                              
OURERRCD DS    XL1                 MY ERROR   CODE                              
OURWRNCD DS    XL1                 MY WARNING CODE                              
OURINFCD DS    XL1                 MY INFO    CODE                              
COUNTER  DS    XL1                                                              
FLDDSPL  DS    XL1                 DISPL OF SUB-FIELD INTO FIELD                
SCANLNTH DS    XL1                 L'RHS OF SCANNER BLOCK ENTRY                 
NOPTN    DS    XL1                 NUMBER OF OPTIONS INPUTTED                   
OPTI     DS    AL4                 THE OPTIONS INPUTTED                         
OPTR     DS    AL4                 OPTIONS REQUIRED                             
OPTX     DS    AL4                 OPTIONS NOT ALLOWED                          
*                                                                               
*          MTA WORK AREA                                                        
STDEM    DS    H                   START DSP TO DEMO IN DEMTAB                  
EDT      DS    X                   PARAM TO ROUTINE                             
OVAL     DS    XL4                 OVERIDE VALUE                                
ODEM     DS    CL2                 OVERIDE MODIF & DEMO NUMBER                  
IBLK     DS    CL5                 WORK BLOCK TO PASS INFO TO GETKSRC           
RIPPLE   DS    X                   UNIVERSE TABLE COUNTER                       
*                                 *************** FLAGS ***************         
MISCFLG1 DS    XL1                 MISC FLAG #1                                 
MF1KYCHG EQU    X80                 KEY FIELDS HAVE BEEN CHANGED                
MF1OPCHG EQU    X40                 OPTIONS FIELD WAS CHANGED                   
MF1RDDEL EQU    X20                 CAN READ DELETED RECORDS AS WELL            
MF1ERRQ  EQU    X01                                                             
MF1KYOPT EQU   MF1KYCHG+MF1OPCHG                                                
*                                 ************** PROFILES *************         
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
                                                                                
         EJECT                                                                  
*                                 *********** OPTION VALUES ***********         
OPTVALS  DS    0C                                                               
                                                                                
OPVUPG   DS    0X                  OPTION VALUES FOR UPGRADES                   
OPVUPGMX EQU    1                   MAX # OF VALUES ALLOWED                     
OPVUPGNM DS     XL1                 # OF VALUES SO FAR                          
OPVUPGTB DS     (OPVUPGMX)XL14      TABLE TO HOLD VALUES                        
OPVUPGQ  EQU   *-OPVUPG                                                         
                                                                                
OPVMIN   DS    0X                  OPTION VALUES FOR MINIMUM VALUES             
OPVMINMX EQU    1                   MAX # OF VALUES ALLOWED                     
OPVMINNM DS     XL1                 # OF VALUES SO FAR                          
OPVMINTB DS     (OPVMINMX)XL14      TABLE TO HOLD VALUES                        
OPVMINQ  EQU   *-OPVMIN                                                         
                                                                                
OPVDEM   DS    0X                  OPTION VALUES FOR DEMOS                      
OPVDEMMX EQU    SCRLNQ              MAX # OF VALUES ALLOWED=#SCRN LINES         
OPVDEMNM DS     XL1                 # OF VALUES SO FAR                          
OPVDEMTB DS     (OPVDEMMX)XL6       TABLE TO HOLD VALUES                        
         DS     XL1                (SPECIAL: LIST DELIMITER)                    
OPVDEMQ  EQU   *-OPVDEM                                                         
                                                                                
OPTVALSQ EQU   *-OPTVALS                                                        
         EJECT                                                                  
*                                                                               
RINVBLK  DS    XL(DBXINVWL)                                                     
*                                 ************** BUFFERS **************         
SAVEKEY  DS    XL(L'RINVKEY)                                                    
                                                                                
DEMOVLS  DS    0F                 HOMES                                         
DEMOVRTG DS    F                   RTG                                          
DEMOVIMP DS    F                   IMP                                          
DEMOVSHR DS    F                   SHR                                          
DEMOVPUT DS    F                   PUT                                          
DEMOVUNV DS    F                   UNV                                          
         DS    F                   BLANK                                        
DEMOVALS DS    61F                 61 DEMO VALUES FROM DEMOUT                   
DEMOVALX EQU   *                                                                
*                                                                               
DEMOLST  DS    0XL3                ENTIRE DEMOLIST TO GOTO DEMOUT WITH          
DEMORTG  DS    XL3                 RTG                                          
DEMOIMP  DS    XL3                 IMP                                          
DEMOSHR  DS    XL3                 SHR                                          
DEMOPUT  DS    XL3                 PUT                                          
DEMOUNV  DS    XL3                 UNV                                          
         DS    XL3                 BLANK                                        
DEMOLIST DS    61XL3               61 DEMO IN LIST FROM DEMOUT                  
         DS    XL1                                                              
DEMOLSTX EQU   *                                                                
                                                                                
MYTEXT   DS    0X                  MISCELLANEOUS TEXT FIELD                     
         DS    XL1                  L'TEXT                                      
         DS    CL20                 THE TEXT ITSELF                             
MYTEXTX  EQU   *                                                                
*                                                                               
MYSSPREL EQU   *-SYSSPARE                                                       
SYSPREMN EQU   L'SYSSPARE-MYSSPREL   AMOUNT LEFT IN SYSSPARE                    
         DS    0XL(SYSPREMN+1)       CHECK AGAINST SYSSPARE LIMIT               
***********************************************************************         
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (TWA DSECTS)'                   
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE RERMPFFD                                                       
                                                                                
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
*-------------------------- INV/DOVER SCREEN -------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPE6D                                                       
SCRLNQ   EQU   1+((DOVDEMXH-DOVDEMOH)/(DOVDEM2H-DOVDEMOH))                      
                                                                                
*                                                                               
*---------------------------------------------------------------------*         
* SAVED STORAGE AT BOTTOM OF TWA - SAVED BETWN TRANSACTIONS                     
*---------------------------------------------------------------------*         
                                                                                
SVSTRG   DS    0H                                                               
DECIMAL  DS    C                                                                
TEMPMIN  DS    CL3                 TEMP STORAGE OF MIN FIELD.                   
FRST     DS    C                   CHANGED TO C'N' NEXT TIME                    
TOPDEM   DS    H                   1ST DEMO ON SCN'S POSN IN TABLE              
PRFOVR   DS    X                   0=OVR OKAY  1=NO OVR ALLOWED                 
*RFFTNT  DS    X                   0=OVR OKAY  1=NO OVR ALLOWED                 
COL      DS    CL3                 COLUMN CTGY ORDER                            
CMALE    EQU   0                                                                
CFEM     EQU   1                                                                
CTOT     EQU   2                                                                
CHUT     EQU   3                                                                
SVSCRDEM DS    CL16                SAVE DEMTAB DEMO NUMBER FROM SCREEN          
*                                                                               
SVOVR    DS    55CL6               SAVED OVERIDES                               
SVOVRLN  EQU   *-SVOVR             LENGTH OF OVERIDE STRG AREA                  
*                                                                               
*CRNTAB  DS    XL(DOVDEM2H-DOVDEMOH)                                            
*CRNTABL EQU   *-SCRNTAB                                                        
*                                                                               
SVSTRGLN EQU   *-SVSTRG                                                         
                                                                                
MYTWAL   EQU   *-CONHEADH                                                       
         DS    0XL(3520-L'SFMPROFS-MYTWAL)  GENCON & RERMP'S TWA LIMIT          
*--------------------------------------------                                   
*RERMPWTWA - (USES LAST 12 BYTES OF TWA AREA)                                   
*--------------------------------------------                                   
       ++INCLUDE RERMPWTWA                                                      
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
*----------------------------------------------------------------------         
*DSECT TO COVER SAVED OVERRIDES LIST                                            
*----------------------------------------------------------------------         
OVRLSTD  DSECT                                                                  
OVRIDEM  DS    0XL2                DEMO MODIFIER AND DEMO NUMBER                
OVRID    DS    C                   DEMO MODIFIER ('R' OR 'I')                   
OVRDEM   DS    X                   DEMO NUMBER                                  
OVRVAL   DS    XL4                 OVERIDE DEMO VALUE                           
OVRNTY   EQU   *-OVRID             L'OVERIDE ENTRY                              
                                                                                
*----------------------------------------------------------------------         
*DSECT TO DEMTAB TABLE                                                          
*----------------------------------------------------------------------         
DEMTABD  DSECT                                                                  
DEMTENTY DS    XL1                 ENTRY NUMBER IN DEMTAB                       
DEMTNAME DS    CL6                 CATAGORY NAME (+ = CORE CELL)                
DEMTMALE DS    XL1                 MALE DEMO NUMBER                             
DEMTFEM  DS    XL1                 FEMALE DEMO NUMBER                           
DEMTTOT  DS    XL1                 TOTAL DEMO NUMBER                            
DEMTCORE DS    0XL3                CORE CELL INDICATORS                         
DEMTCMAL DS    XL1                  +1= MALE CORE CELL INDIC                    
DEMTCFEM DS    XL1                  +2= FEMALE CORE CELL INDIC                  
DEMTCTOT DS    XL1                  +3= TOTAL CORE CELL INDIC                   
DEMTABQ  EQU   *-DEMTABD           L'EACH DEMTAB ENTRY                          
DEMQ     EQU   ORIGDMQ             # ENTRIES IN DEMO TABLE                      
         EJECT                                                                  
***********************************************************************         
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (OTHER DSECTS)'                 
***********************************************************************         
*============================ OTHER DSECTS ===========================*         
                                                                                
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* DDCOREQUS                                                                     
* REMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE REMSGEQUS                                                      
         PRINT ON                                                               
***********************************************************************         
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (REGEN DSECTS)'                 
***********************************************************************         
*============================ REGEN DSECTS ===========================*         
                                                                                
*------------------------------ REGENINV -----------------------------*         
REINVRCD DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE RERMPPROF                                                      
                                                                                
***********************************************************************         
         TITLE 'DOVER - INVENTORY DEMO OVERRIDE (MISC DSECTS)'                  
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
*----------------------- OPTION KEYWORDS DSECT -----------------------*         
                                                                                
OPTDSECT DSECT                                                                  
OPDNUMB  DS    XL1                 INTERNAL OPTION NUMBER                       
OPDNTRYL DS    XL1                 L(OPTION KEYWORD ENTRY)                      
OPDFLAG  DS    XL1                 FLAGS ABOUT KEYWORD                          
OPDFKYWD EQU    X80                 OPTION IS VALID BY KEYWORD                  
OPDFVRTN EQU    X40                 VALIDATE DATA VIA ROUTINE                   
OPDFVTBL EQU    X20                 VALIDATE DATA VIA TABLE                     
*                                    EG. AL1(3,1),CL3'YES',CL1'Y'               
*                                        AL1(2,1),CL3'NO',CL1'N'                
*                                        AL1(EOT)                               
                                                                                
OPDRTNUM DS    XL1                 VALIDATION ROUTINE # (OPDFVRTN)              
         ORG   OPDRTNUM                                                         
OPDTBDSP DS    AL2                 VALIDATION TABLE DSPL (OPDFVTBL)             
         ORG                                                                    
                                                                                
OPDOPTB  DS    AL4                 OPTION'S BIT MASK                            
OPDRQOPT DS    AL4                 OTHER OPTIONS REQUIRED                       
OPDNAOPT DS    AL4                 OPTIONS NOT ALLOWED                          
OPDMXNDV DS    XL1                 MAX # OF DATA VALUES ALLOWED                 
OPDOLEN  DS    XL1                 OUTPUT ENTRY LENGTH                          
OPDODSPL DS    AL2                 DSPL FROM SYSD OF OUTPUT FIELD               
OPDKYTB  DS    AL2                 DSPL OF KEYWORD TABLE                        
OPDVLTB  DS    AL2                 DSPL OF VALUE TABLE                          
OPDFXLNQ EQU   *-OPTDSECT                                                       
         EJECT                                                                  
                                                                                
                                                                                
*----------------------------- DAY TABLE -----------------------------*         
                                                                                
DAYTABD  DSECT                                                                  
DYTFILE  DS    CL1                 FILE (X'00'==>ALL FILES)                     
DYTIDAY  DS    XL1                 INTERNAL DAY                                 
DYTKDAY  DS    XL1                 KEY DAY                                      
DYTNAME  DS    CL3                 NAME OF DAY(S)                               
DYTLEN   EQU   *-DAYTABD                                                        
         EJECT                                                                  
*---------------------------- DISPLAY LINE ---------------------------*         
                                                                                
DLINED   DSECT                                                                  
         DS    0XL1                                                             
DLSRC    DS    CL1                 SOURCE                                       
         DS    XL1                                                              
DLBOOK   DS    CL6                 BOOK                                         
         DS    XL1                                                              
DLFILE   DS    CL3                 FILE                                         
         DS    XL1                                                              
DLSTTN   DS    CL5                                                              
         DS    XL1                                                              
DLPTITL  DS    CL13                PROGRAM TITLE (NAME)                         
         DS    XL1                                                              
DLDAY    DS    CL7                 DAY                                          
         DS    XL1                                                              
DLSTIME  DS    CL5                 START TIME                                   
         DS    XL1                                                              
DLQHRS   DS    CL2                 QUARTER HOURS                                
         DS    XL1                                                              
DLWEEKS  DS    CL4                 WEEKS                                        
         DS    XL3                                                              
DLDEMRTG DS    CL5                 DEMO RATINGS                                 
         DS    XL1                                                              
DLDEMSHR DS    CL5                 DEMO SHARES                                  
         DS    XL1                                                              
DLDEMHUT DS    CL5                 DEMO HUT                                     
*                                                                               
DLINEQ   EQU   *-DLINED                                                         
*        DS    0XL(L'DOVLDTA-DLINEQ+1)                                          
*        DS    0XL(DLINEQ-L'DOVLDTA+1)                                          
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== BIG STORAGE AREA =========================*         
                                                                                
BIGAREA  DSECT                                                                  
         DS    0H                                                               
         ORG   BIGAREA+X'2400'     RESERVED FOR INV/TRACKS                      
                                                                                
DEMTQ    EQU   ORIGDEML                                                         
DEMTTBL  DS    D                   *DEMTAB*                                     
DEMTAB   DS    XL(DEMTQ)                                                        
         DS    XL1                                                              
SVDEMQ   EQU   2000                                                             
SVDEMTBL DS    D                   *SVDEMT*                                     
SVDEM    DS    XL(SVDEMQ)                                                       
SVDEMX   EQU   *                                                                
         DS    XL1                                                              
SCRNTABL DS    D                   *SCRNTA*                                     
SCRNTAB  DS    XL(DOVPFLNH-DOVHRTGH)                                            
SCRNTABQ EQU   *-SCRNTAB                                                        
*                                                                               
*OREFLAG DS    XL1                 TO INDICATE THE CORE OPT. ACTIVATED          
*                                                                               
BIGENDSV EQU   *                --> SAVE STORAGE UP TO HERE <--                 
                                                                                
DEMWRKQ  EQU   2000                                                             
DEMWRKTB DS    D                   *DEMWRK*                                     
DEMWRK   DS    XL(DEMWRKQ)                                                      
         DS    XL1                                                              
                                                                                
                                                                                
DBLKLABL DS    D                   *DBLOCK*                                     
DBLOCK1  DS    XL(L'DBLOCK)                                                     
         DS    XL14                (RESERVED)                                   
DBLOCK1X DS    0X                                                               
                                                                                
DBXTLABL DS    D                   *DBXTND*                                     
DBXTND1  DS    XL256                                                            
DBXTND1X DS    0X                                                               
*                                                                               
                                                                                
MYTIALEN EQU   *-BIGAREA                                                        
         DS    0CL((X'4800'-MYTIALEN)+1)                                        
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032RERMP19   06/30/09'                                      
         END                                                                    
