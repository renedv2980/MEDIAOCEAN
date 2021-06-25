*          DATA SET RERMP41S   AT LEVEL 007 AS OF 02/02/00                      
*PHASE T81041A                                                                  
*INCLUDE BINSRCH2                                                               
T81041   TITLE 'RERMP41 - INVENTORY DEMOS'                                      
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* Feb02/00 007 GLEE - In BTR# rtn, clear tgt area bfor copying into it*         
*                                                                     *         
* Jan21/00 006 GLEE - Handle unsuccessful DEMAND calls                *         
*                                                                     *         
* Aug26/98 005 GLEE - New field SVNDEMS to use in BTR# rtn to fix bug *         
*              GLEE - Mimick GDV# routine in DEMHOOK                  *         
*                                                                     *         
* Jul15/97 004 GLEE - Support LTRANS request for all updates to file  *         
*                                                                     *         
* Jun09/97 003 GLEE - New routine to delete demo override elems       *         
*                   - Code to handle index element (x'DE')            *         
*                                                                     *         
* May27/97 002 GLEE - Set up SUBR03                                   *         
*                                                                     *         
* Apr25/97 001 GLEE - New program for INVentory DEMOS, displaying     *         
*                      demos across tracks                            *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
RMP41    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81041**,R9,RR=RE                                              
                                                                                
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
         EJECT                                                                  
***********************************************************************         
*========================= MY INITIALIZATION =========================*         
         DS    0H                                                               
MYINIT   NTR1                                                                   
*                                                                               
         DS    0H                  CHECK IF PHASE IS CALLED CORRECTLY           
         MVI   FRMINVTK,C'N'        ASSUME NOT CALLED FROM INV/TRACKS           
         ZICM  R1,CALLSP,(1)                                                    
         BZ    MI02X                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,CALLSTCK(R1)                                                  
         CLI   0(R1),SCINVTRA                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FRMINVTK,C'Y'        IT IS CALLED FROM INV/TRACKS                
MI02X    EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  SET UP ADCONS OF TABLES & ROUTINES           
         LH    R2,=Y(DISPTAB-RMP41)                                             
         LA    R2,RMP41(R2)                                                     
         LA    R0,DISPTABQ                                                      
                                                                                
MI10     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         LA    RE,RMP41            RE = BASE OF TABLE/ROUTINE                   
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
                                                                                
         DS    0H                  RESTORE TIA TABLES                           
         MVI   GOSUBN,RTI#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         DS    0H                  SET UP LABELS IN BIGAREA                     
         LH    R2,=Y(LBLTAB-RMP41)                                              
         LA    R2,RMP41(R2)                                                     
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
         LH    RE,=Y(DCLIST-RMP41)                                              
         A     RE,MYBASE1                                                       
         STCM  RE,7,DDIADR                                                      
         LA    RE,DSLIST                                                        
         STCM  RE,7,DDOADR                                                      
         GOTO1 DICTATE,(R1)                                                     
         DROP  R1                                                               
*                                                                               
         PRINT OFF                                                              
*&&DO                                                                           
         NI    CHNGFLG1,XFF-CF1ACTEQ                                            
         CLC   ITITSVAE,ACTEQU                                                  
         BE    *+14                                                             
         OI    CHNGFLG1,CF1ACTEQ                                                
         MVC   ITITSVAE,ACTEQU                                                  
*                                                                               
*&&                                                                             
         PRINT ON                                                               
         XC    CCONRSVC,CCONRSVC                                                
         XC    CCONKBK,CCONKBK                                                  
*                                                                               
         MVC   DSP1EL,=Y(RINVPEL-RINVREC)   DISPL TO 1ST ELEMENT                
*                                                                               
         MVC   DCFILTP,=C'TP '                                                  
         MVC   DCFILPAV,=C'PAV'                                                 
         MVC   DCFILIUN,=C'IUN'                                                 
         MVC   DCFILINV,=C'INV'                                                 
         MVC   DCSRCNSI,=C'NSI'                                                 
         MVC   DCSRCMFX,=C'MFX'                                                 
         MVC   DCSRCSRC,=C'SRC'                                                 
         MVC   DCREPFIL,=C'REPFILE'                                             
*                                                                               
MIX      DS    0H                                                               
         B     XIT                                                              
***********************************************************************         
         TITLE 'RERMP41 - INVENTORY DEMOS (VALKEY)'                             
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
VKEY     DS    0H                                                               
                                                                                
         NI    CHNGFLG1,XFF-CF1KTD                                              
                                                                                
*                                                                               
*-------------------------- VALIDATE STATION -------------------------*         
*                                                                               
         LA    R2,IDMSTTNH         STATION FIELD                                
         MVI   ERROPT,C'Y'         RETURN STATION ERRORS TO ME                  
*                                                                               
         MVI   BYTE,CF1KEY                                                      
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
                                                                                
*                                                                               
         DS    0H                  VALIDATE STATION FIELD                       
         GOTO1 ANY                  MUST HAVE A STATION INPUT                   
         CLI   ERROPT,C'Y'                                                      
         BNE   MISSFLD                                                          
                                                                                
         GOTO1 VALISTA              VALIDATE THE STATION INPUTTED               
         CLI   ERROPT,C'Y'                                                      
         BNE   INVLSTA                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         OI    4(R2),X20                                                        
                                                                                
         MVC   IKSTTN,WORK         HOLD ONTO KEY FIELD INPUT                    
         CLI   IKSTTN+4,C' '                                                    
         BNE   *+8                                                              
         MVI   IKSTTN+4,C'T'                                                    
         CLI   WORK+40,C' '        CHECK FOR SATELLITES                         
         BE    *+10                                                             
         MVC   IKSTTN+4(1),WORK+40                                              
                                                                                
*                                                                               
*--------------------- VALIDATE INVENTORY NUMBER ---------------------*         
*                                                                               
         LA    R2,IDMINVH          INVENTORY NUMBER                             
         MVI   ERROPT,C'Y'         RETURN INVENTORY #  ERRORS TO ME             
*                                                                               
         MVI   BYTE,CF1KEY                                                      
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
                                                                                
*                                                                               
         DS    0H                  VALIDATE INVENTORY NUMBER FIELD              
         GOTO1 ANY                  MUST HAVE AN INVENTORY # INPUT              
         CLI   ERROPT,C'Y'                                                      
         BNE   MISSFLD                                                          
                                                                                
*                                                                               
         DS    0H                  INVENTORY NUMBER VALIDATED                   
         OI    4(R2),X20                                                        
                                                                                
         MVC   IKINV,WORK          HOLD ONTO KEY FIELD INPUT                    
                                                                                
*                                                                               
*--------------------------- EFFECTIVE DATE --------------------------*         
*                                                                               
         LA    R2,IDMEFFDH         EFFECTIVE DATE                               
         MVI   ERROPT,0                                                         
*                                                                               
         XC    IKEFFDB,IKEFFDB                                                  
         XC    IKEFFDC,IKEFFDC                                                  
         XC    TMPBDATE,TMPBDATE                                                
                                                                                
*                                                                               
         TM    4(R2),X20           IF FIELD WAS NOT VALIDATED PREVSLY,          
         BZ    VK033                THEN FIND SUITABLE EFF DATE                 
         TM    4(R2),X80           WAS FIELD INPUTTED THIS TIME?                
         BZ    VK037                NO, THEN JUST ACCEPT DATA IN FIELD          
                                                                                
*                                                                               
** MATCH EFF DATE INPUTTED TO FILE **                                           
*                                                                               
VK033    DS    0H                  GET SUITABLE EFF DATE FROM INPUT             
         CLI   5(R2),0              FIRST CHECK IF THERE IS INPUT               
         BH    VK033B                                                           
*                                                                               
         DS    0H                    NO EFFECTIVE DATE INPUT                    
         BAS   RE,VKSTD1              FIND A SUITABLE EFF DATE                  
         B     VK035                                                            
*                                                                               
VK033B   DS    0H                    EFFECTIVE DATE INPUTTED                    
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    INVLFLD                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,TMPBDATE)                                
         BAS   RE,VKSTD2              TRY TO MATCH EFF DATE INPUTTED            
         B     VK035                                                            
                                                                                
*                                                                               
VK035    DS    0H                   DISPLAY EFF DATE CONTAINED IN KEY           
         MVC   TMPBDATE,KEY+(RINVKSTD-RINVREC)                                  
         MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(X'83',TMPBDATE),(5,WORK),0                          
         MVC   IDMEFFD,WORK         MOVE IN SUITABLE EFF DATE                   
         MVC   IDMEFFDH+5(1),DMCB+4  ITS LENGTH                                 
         OI    IDMEFFDH+6,X80        AND TRANSMIT FIELD                         
                                                                                
*                                                                               
** TRANSFORM EFF DATE DATA TO INTERNAL USE **                                   
*                                                                               
VK037    DS    0H                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(0,WORK),(3,IKEFFDB)                                 
         GOTO1 (RF),(R1),,(2,IKEFFDC)                                           
                                                                                
*                                                                               
VK039    DS    0H                                                               
         MVI   BYTE,CF1KEY                                                      
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         OI    4(R2),X20                                                        
                                                                                
*                                                                               
*--------------------------- VALIDATE BOOKS --------------------------*         
*                                                                               
         LA    R2,IDMBKSH          BOOKS FIELD                                  
         MVI   ERROPT,0                                                         
                                                                                
*                                                                               
         DS    0H                  INITIALIZE FIELDS                            
         XC    IKTKLST(IKTKLSTL),IKTKLST                                        
         XC    IKBTLST(IKBTLSTL),IKBTLST                                        
*                                                                               
         TM    4(R2),X40                                                        
         BO    *+16                                                             
         MVC   SVTKLST(SVTKLSTL),SPACES  DON'T INITIALIZE TO SAME VALUE         
         MVC   SVBTLST(SVBTLSTL),SPACES   AS IKTKLST & IKBTLST                  
                                                                                
*                                                                               
         CLI   5(R2),0             SEE IF ANY INPUT IN FIELD                    
         BNE   VK045                                                            
*                                                                               
         DS    0H                  NO INPUT, SEE IF CAME FROM INV/TRACK         
         ZIC   R1,CALLSP                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,CALLSTCK(R1)                                                  
         CLI   0(R1),SCINVTRA                                                   
         BNE   VK043G                                                           
         LA    RF,CCONTRKS                                                      
         LR    RE,RF                                                            
         CLI   0(RF),C' '                                                       
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         SR    RF,RE               RF=LENGTH OF DATA TO PUT INTO FIELD          
         BZ    VK043G                                                           
         STC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EXMVC RF,8(R2),CCONTRKS        IF YES, FILL IN TRACKS SELECTED         
         OI    6(R2),X80                                                        
         B     VK045                                                            
VK043G   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  NO INPUT, FILL BOOKS IN MYSELF               
         MVI   GOSUBN,BTL#                                                      
         GOTO1 AGOSUB                                                           
         ZICM  R1,MYFLDH+5,(1)                                                  
         BZ    VK059                                                            
         STC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,8(R2),MYFLDD                                                  
         OI    6(R2),X80                                                        
                                                                                
*                                                                               
VK045    DS    0H                                                               
         XC    DUB,DUB                                                          
         GOTO1 BOOKVAL,DMCB,(R2),(MXTKLNTY,WORK),(C'B',SCANNER),DUB             
         CLI   DMCB+4,0            IF INVALID,                                  
         BNE   VK045X                                                           
                                                                                
         DS    0H                   TRY DEFAULT TO NSI                          
         GOTO1 (RF),DMCB,(C'N',(R2)),(MXTKLNTY,WORK),(C'B',SCANNER),DUB         
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
VK045X   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  TRANSFER DATA TO INTERNAL STORAGE            
         ZIC   RF,DMCB+4                                                        
         STC   RF,NTKLNTRY          STORE # NUMBER OF TRACKS                    
         LR    RE,RF                                                            
         MH    RF,=H'3'                                                         
         BCTR  RF,0                                                             
         EXMVC RF,IKTKLST,WORK      STORE LIST OF TRACKS                        
         BCTR  RE,0                                                             
         EXMVC RE,IKBTLST,DUB       STORE LIST OF BOOKTYPES                     
                                                                                
*                                                                               
         DS    0H                  CHECK IF INPUTTED TRACKS EXIST               
         ZIC   R0,NTKLNTRY          R0 = # TIMES TO LOOP                        
         LA    R3,IKTKLST           R3-->LIST OF INPUTTED TRACKS                
         LA    R4,IKBTLST           R4-->LIST OF INPUTTED BOOKTYPES             
                                                                                
         MVC   TMPSTTN,IKSTTN                                                   
         MVC   TMPINVN,IKINV                                                    
         MVC   TMPBDATE,IKEFFDB                                                 
                                                                                
VK052B   DS    0H                                                               
         MVC   TMPFRBTS,0(R3)                                                   
         MVC   TMPBTYP,0(R4)                                                    
         MVI   GOSUBN,GKS#                                                      
         GOTO1 AGOSUB               KEY SOURCE SET IN TMPKSRC                   
                                                                                
         MVC   TMPKBK,1(R3)                                                     
         MVI   GOSUBN,BIK#                                                      
         GOTO1 (RF)                                                             
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RINVKEY),KEYSAVE                                           
         BNE   RCDNTFND                                                         
                                                                                
         LA    R3,L'IKTKLST(R3)                                                 
         LA    R4,L'IKBTLST(R4)                                                 
         BCT   R0,VK052B                                                        
                                                                                
*                                                                               
VK059    EQU   *                                                                
         MVI   GOSUBN,CDUPT#       CHECK FOR DUPLICATE TRACKS                   
         GOTO1 AGOSUB                                                           
         BNE   VKERROR                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   BYTE,CF1TRK                                                      
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         OI    4(R2),X20                                                        
                                                                                
*                                                                               
*--------------------------- VALIDATE DEMOS --------------------------*         
*                                                                               
         LA    R2,IDMDEMOH         DEMOS FIELD                                  
         MVI   ERROPT,0                                                         
                                                                                
*                                                                               
         XC    IKDMLST(IKDMLSTL),IKDMLST                                        
         TM    4(R2),X40                                                        
         BO    *+14                                                             
         XC    SVDMLST(SVDMLSTL),SPACES                                         
         MVI   SVNDEMS,0                                                        
                                                                                
*                                                                               
** PRELIMINARY TASKS FOR DEMOS FIELD **                                         
*                                                                               
         DS    0H                                                               
         CLI   5(R2),0             SEE IF ANY INPUT IN FIELD                    
         BNE   VK065                THERE IS INPUT                              
                                                                                
*                                                                               
         DS    0H                   NO INPUT IN FIELD                           
         CLI   FRMINVTK,C'Y'         SEE IF CAME FROM INV/TRACKS                
         BNE   VK063T                 NOPE, CAN'T USE SHARED STRGE              
         TM    4(R2),X40             SEE IF FIELD INPUT PREVIOUSLY              
         BNZ   VK063T                 YEP, CAN'T USE SHARED STRGE               
         OC    CCONDEMS,CCONDEMS     ANY DATA IN SHARED STORAGE                 
         BZ    VK063T                 NO DATA IN SHARED STORAGE TO USE          
*                                                                               
         DS    0H                   USE DATA IN SHARED STORAGE                  
         LA    RF,CCONDEMS                                                      
         LR    RE,RF                                                            
         CLI   0(RF),C' '                                                       
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         SR    RF,RE                 RF=LENGTH OF DATA TO PUT INTO FLD          
         BZ    VK063T                IF ZERO, DON'T USE DATA                    
         STC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EXMVC RF,8(R2),CCONDEMS                                                
         OI    6(R2),X80                                                        
         B     VK065                                                            
VK063T   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  NO INPUT, FILL DEMOS IN MYSELF               
         MVC   8(DFLTDEML,R2),DFLTDEMS                                          
         OI    6(R2),X80                                                        
         MVI   5(R2),DFLTDEML                                                   
                                                                                
*                                                                               
** VALIDATE DEMO INPUT **                                                       
*                                                                               
VK065    DS    0H                                                               
         CLC   8(2,R2),=C'M='      SEE IF DEMO MENU INPUT                       
         BE    VK080                                                            
         CLI   8(R2),C'/'          SEE IF DEMO EDIT OVERRIDE                    
         BE    VK090                                                            
         B     VK070               ELSE, VALIDATE AS DEMOLIST                   
                                                                                
*                                                                               
*** VALIDATE AS DEMO LIST ***                                                   
*                                                                               
VK070    DS    0H                                                               
         MVI   GOSUBN,CDBK#                                                     
         GOTO1 AGOSUB                                                           
*                                                                               
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         MVC   DBFILE,DCFILINV                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELAGY,TWAAGY                                                  
*                                                                               
         GOTO1 DEMOVAL,DMCB,(1,(R2)),(10,WORK),DBLOCKD,0                        
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
         DROP  R5                                                               
*                                                                               
         ZIC   R1,DMCB+4                                                        
         STC   R1,HALF             # OF DEMOS                                   
         MH    R1,=H'3'                                                         
         LA    R1,1(R1)                                                         
         STC   R1,HALF+1           L(DEMOLIST)                                  
*                                                                               
         B     VK200                                                            
                                                                                
*                                                                               
*** VALIDATE AS DEMO MENU ***                                                   
*                                                                               
VK080    DS    0H                                                               
         CLI   5(R2),4             DEMO MENU INPUT IS 4 BYTES LONG              
         BNE   DMTWOCH                                                          
                                                                                
*                                                                               
         DS    0H                  BUILD KEY TO DEMO MENU RECORD                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDEMKEY,R6                                                       
         MVI   RDEMKTYP,X'23'       RECORD TYPE                                 
         MVC   RDEMKREP,TWAAGY      AGENCY                                      
         MVC   RDEMKDEM,10(R2)      MENU NAME                                   
         DROP  R6                                                               
*                                                                               
         DS    0H                  SEE IF DEMO MENU IS ON FILE                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDEMKEY),KEYSAVE                                           
         BNE   DMNOTFND                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         DS    0H                  GET NUMBER OF DEMOS                          
         L     R3,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL                                                         
         BNE   DMNOTFND                                                         
         MVC   HALF(1),(RDEMNUM-RDEMELEM)(R3)                                   
*                                                                               
         DS    0H                  GET DEMOLIST                                 
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL                                                         
         BNE   DMNOTFND                                                         
                                                                                
* I'm going through all this trouble because the demo menu may be               
*  longer than what my demolist allows.                                         
                                                                                
         USING RDEMDEL,R3                                                       
         ZIC   R1,RDEMDLN                                                       
         SH    R1,=H'2'                                                         
         BCTR  R1,0                 R1 = L(DEMOLIST) - L(DELIMITER)             
         SR    R0,R0                                                            
         ZIC   RF,HALF              RF = # OF DEMO ENTRIES                      
         DR    R0,RF                R1 = L(EACH DEMO ENTRY)                     
                                                                                
         CLM   RF,1,=AL1(MXDEMOS)   COMPARE # ENTRIES AGAINST MAX               
         BNH   *+8                                                              
         LA    RF,MXDEMOS            IF GREATER, USE MAX ALLOWED                
                                                                                
         SR    R0,R0                                                            
         MR    R0,RF                                                            
         LA    R0,1(R1)             R0 = L(DEMOLIST) FOR INV/DEMOS              
         STC   RF,HALF              STORE # OF DEMOS                            
         STC   R0,HALF+1            STORE L(DEMOLIST)                           
                                                                                
         EXMVC R1,WORK,RDEMDEM                                                  
         LA    R1,WORK(R1)                                                      
         MVI   0(R1),XFF            MOVE IN DELIMITER                           
         DROP  R3                                                               
*                                                                               
         B     VK200                                                            
                                                                                
*                                                                               
*** VALIDATE AS DEMO EDIT OVERRIDE ***                                          
*                                                                               
VK090    DS    0H                                                               
         CLI   9(R2),C'0'          TEST FOR INPUT LIKE "/001"                   
         BL    *+12                                                             
         CLI   9(R2),C'9'                                                       
         BNH   VK110                IF SO, VALIDATE "/001" WAY                  
*                                                                               
         CLI   5(R2),3             IF INPUT LENGTH > 3,                         
         BH    VK070                GO VALIDATE AS DEMO LIST                    
                                                                                
*                                                                               
**** VALIDATE FOR "/MC" ****                                                    
*                                                                               
         MVI   HALF,C'T'           ASSUME MODIFIER IS IMPRESSIONS               
         MVC   HALF+1(1),9(R2)     ASSUME THIS IS THE CATEGORY                  
         CLI   5(R2),2             CHECK LENGTH OF INPUT AGAIN                  
         BH    VK093                IF > 2, CHECK MODIFIER & CATEGORY           
         BE    VK095                IF = 2, CHECK CATEGORY ONLY                 
         B     INVLFLD              ERROR FOR THIS CASE                         
                                                                                
*                                                                               
VK093    DS    0H                  VALIDATE MODIFIER                            
         LA    RF,MODLIST                                                       
         LA    R0,MODLISTQ                                                      
         CLC   9(1,R2),0(RF)                                                    
         BE    VK093C                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,*-14                                                          
         B     INVLFLD                                                          
                                                                                
VK093C   DS    0H                                                               
         MVC   HALF(1),0(RF)        REMEMBER MODIFIER                           
         MVC   HALF+1(1),10(R2)     GET DEMO CATEGORY                           
                                                                                
*                                                                               
VK095    DS    0H                  VALIDATE CATEGORY THAT'S IN HALF+1           
         L     R3,ADMLSTTB                                                      
                                                                                
VK095B   DS    0H                                                               
         CLI   0(R3),EOT                                                        
         BE    INVLFLD                                                          
         CLC   0(1,R3),HALF+1                                                   
         BE    *+12                                                             
         LA    R3,L'DMLSTTAB(R3)                                                
         B     VK095B                                                           
                                                                                
         ZICM  RF,1(R3),(3)                                                     
         LA    RF,RMP41(RF)                                                     
         ZIC   R1,4(R3)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,WORK,0(RF)                                                    
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   HALF,C'T'                                                        
         BE    VK096X                                                           
                                                                                
         ZIC   R0,3(R3)                                                         
         LA    RF,WORK                                                          
         MVC   1(1,RF),HALF                                                     
         LA    RF,3(RF)                                                         
         BCT   R0,*-10                                                          
VK096X   EQU   *                                                                
*                                                                               
         MVC   HALF+0(1),3(R3)                                                  
         MVC   HALF+1(1),4(R3)                                                  
         B     VK200                                                            
                                                                                
*                                                                               
**** VALIDATE FOR "/nnn" ****                                                   
*                                                                               
VK110    DS    0H                                                               
         CLI   5(R2),4             TEST FOR 3-DIGIT LIMIT                       
         BH    VK070                                                            
*                                                                               
         DS    0H                  CHECK FOR NUMERIC DATA                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                 DECREMENT FOR SLASH                         
         LR    R0,R1               HOLD ONTO L'DATA                             
         LA    RE,9(R2)                                                         
                                                                                
         CLI   0(RE),C'0'                                                       
         BL    VK070                                                            
         CLI   0(RE),C'9'                                                       
         BH    VK070                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,*-20                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,9(0,R2)                                                      
         CVB   R0,DUB                                                           
*                                                                               
         LA    RE,MODLIST                                                       
         LA    R1,MODLISTQ                                                      
         XC    WORK(3*MODLISTQ),WORK                                            
         LA    RF,WORK                                                          
                                                                                
         MVC   1(1,RF),0(RE)       MODIFIER                                     
         STC   R0,2(RF)            DEMO NUMBER                                  
         LA    RE,1(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R1,*-18                                                          
         MVI   0(RF),XFF                                                        
                                                                                
         LA    RF,1(RF)                                                         
         LA    R0,WORK                                                          
         SR    RF,R0                                                            
         STC   RF,HALF+1                                                        
         MVI   HALF,MODLISTQ                                                    
*                                                                               
         B     VK200                                                            
                                                                                
*                                                                               
** TRANSFER INPUT TO STORAGE **                                                 
*                                                                               
VK200    DS    0H                                                               
         MVC   NDEMS,HALF                                                       
         ZIC   R1,HALF+1                                                        
         BCTR  R1,0                                                             
         EXMVC R1,IKDMLST,WORK                                                  
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,CDUPD#       CHECK FOR DUPLICATE DEMO ENTRY               
         GOTO1 AGOSUB                                                           
         BNE   VKERROR                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   FRMINVTK,C'Y'       DID WE COME FROM INV/TRACKS?                 
         BNE   VK207X               NOPE                                        
                                                                                
         XC    CCONDEMS,CCONDEMS                                                
         ZIC   R1,5(R2)            PUT ACTUAL INPUT                             
         BCTR  R1,0                                                             
         EXMVC R1,CCONDEMS,8(R2)    INTO SHARED STORAGE                         
VK207X   EQU   *                                                                
                                                                                
*                                                                               
         MVI   BYTE,CF1DEM                                                      
         BAS   RE,KYCHNGED          SEE IF THIS KEY FIELD CHANGED               
         OI    4(R2),X20                                                        
         EJECT                                                                  
*                                                                               
*-------------------------- VALIDATE OPTIONS -------------------------*         
*                                                                               
         DS    0H                                                               
         NI    CHNGFLG1,XFF-CF1OPT                                              
         B     VK400                                                            
         EJECT                                                                  
*                                                                               
*--------------------- MISCELLANEOUS VALKEY TASKS --------------------*         
*                                                                               
VK400    DS    0H                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         MVC   SVTKLST(SVTKLSTL),IKTKLST   REMEMBER TRACKS    INPUTTED          
         MVC   SVBTLST(SVBTLSTL),IKBTLST      "     BOOKTYPES    "              
         MVC   SVDMLST(SVDMLSTL),IKDMLST      "     DEMOS        "              
         MVC   SVNDEMS,NDEMS                                                    
                                                                                
*                                                                               
*---------------------------- VALKEY EXIT ----------------------------*         
*                                                                               
VKX      DS    0H                                                               
         MVI   GOSUBN,STI#         SAVE TIA TBLES BEFORE EXITING VALKEY         
         GOTO1 AGOSUB                                                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------- SETUP PROCEDURE --------------------------*         
                                                                                
         DS    0H                                                               
SETUP    NTR1                                                                   
         TM    CHNGFLG1,CF1KO       IF KEY, OR OPTN CHANGED,                    
         BZ    *+8                                                              
         MVI   PFKEY,0               IGNORE ANY PFKEYS HIT                      
                                                                                
*                                                                               
         DS    0H                                                               
         TM    CHNGFLG1,CF1KEY      DID KEY CHANGE?                             
         BNZ   SU013G                YES, FORGET THE SAVED TRKS & DEMOS         
         CLI   PFKEY,PFQRDPLY       HIT PFKEY TO RE-DISPLAY?                    
         BE    SU013G                YES, FORGET THE SAVED TRKS & DEMOS         
         B     SU013X                                                           
                                                                                
SU013G   DS    0H                                                               
         MVC   SVTKLST(SVTKLSTL),SPACES  DON'T INITIALIZE TO SAME VALUE         
         MVC   SVBTLST(SVBTLSTL),SPACES   AS IKTKLST, IKBTLST                   
         MVC   SVDMLST(SVDMLSTL),SPACES   & IKDMLST                             
         MVI   SVNDEMS,0                                                        
SU013X   EQU   *                                                                
                                                                                
*                                                                               
** GET INVENTORY HEADER **                                                      
*                                                                               
         TM    CHNGFLG1,CF1KO       IF KEY, OR OPTN CHANGED,                    
         BZ    SU019                                                            
                                                                                
*                                                                               
         DS    0H                    HOLD ONTO INVENTORY HEADER RECORD          
         L     R0,AINVHDR                                                       
         LA    R1,(IVHDRECX-IVHDRECD)                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                  CLEAR STORAGE AREA                        
*                                                                               
         MVI   TMPKSRC,0                                                        
         XC    TMPKBK,TMPKBK                                                    
         MVI   GOSUBN,BIK#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RINVKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AIO                                                           
         USING REINVRCD,RE                                                      
         ZICM  RF,RINVLEN,(3)         GET LENGTH OF INV HEADER RECD             
         DROP  RE                                                               
         L     R0,AINVHDR                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                   AND MOVE IT TO STORAGE AREA              
SU019    EQU   *                                                                
                                                                                
*                                                                               
** BUILD/RESTORE TABLES **                                                      
*                                                                               
SU020    DS    0H                                                               
         TM    CHNGFLG1,CF1KO       IF KEY, OR OPTN CHANGED,                    
         BNZ   SU025                 BUILD TABLES                               
         CLI   PFKEY,PFQRDPLY       HIT PFKEY TO RE-DISPLAY?                    
         BE    SU025                 YES, BUILD TABLES                          
         B     SU030                                                            
                                                                                
*                                                                               
*** BUILD TABLES ***                                                            
*                                                                               
SU025    DS    0H                                                               
         MVI   GOSUBN,CSA#           CLEAR (SYSSPARE) STORAGE AREAS,            
         GOTO1 AGOSUB                                                           
*                                                                               
         MVI   GOSUBN,BMDT#          BUILD MASTER DISPLAY TABLE                 
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         B     SU100                                                            
                                                                                
*                                                                               
*** RESTORE TABLES ***                                                          
*                                                                               
SU030    DS    0H                  RESTORE TABLES                               
*^^GYL   MVI   GOSUBN,RTI#                                                      
*^^GYL   GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                                                               
         TM    CHNGFLG1,CF1TD      IF TRACKS OR DEMOS WERE CHANGED              
         BZ    SU035X                                                           
         MVI   GOSUBN,UMDT#         UPDATE MASTER DISPLAY TABLE                 
         GOTO1 AGOSUB                                                           
SU035X   EQU   *                                                                
*                                                                               
         B     SU100                                                            
                                                                                
*                                                                               
SU100    DS    0H                                                               
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
KYCH10   OC    CHNGFLG1,BYTE                                                    
         BR    RE                                                               
         EJECT                                                                  
* Routine gets called when there is no effective date input.  Routine           
*  finds the latest effective date for the given station/inv#.  If              
*  none is found, this routine goes to error message handling.                  
                                                                                
VKSTD1   NTR1                                                                   
         MVI   TMPKSRC,0                                                        
         XC    TMPKBK,TMPKBK                                                    
         MVI   GOSUBN,BIK#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
                                                                                
*                                                                               
VKSTD110 DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
         CLC   RINVKEY(IKYINVL),KEYSAVE                                         
         BNE   VKSTD130                                                         
                                                                                
         DS    0H                  KEYS MATCH--FIND NEXT EFF DATE               
         MVI   RINVKSTD+L'RINVKSTD,XFF                                          
         LA    R1,L'RINVKEY-(RINVKSTD+L'RINVKSTD-RINVKEY)-2                     
         EXMVC R1,RINVKSTD+L'RINVKSTD+1,RINVKSTD+L'RINVKSTD                     
         B     VKSTD110                                                         
                                                                                
VKSTD130 DS    0H                  KEYS DOESN'T MATCH                           
         LA    R6,KEYSAVE                                                       
         OC    RINVKSTD,RINVKSTD    DO WE HAVE AN EFF DATE?                     
         BZ    RCDNTFND              NOPE, NO INVENTORY FOUND                   
         MVC   KEY,RINVKEY           YES, RETURN IT IN KEY                      
         B     VKSTD1X                                                          
                                                                                
*                                                                               
         DROP  R6                                                               
                                                                                
*                                                                               
VKSTD1X  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* Routine gets called when there is an effective date input.  Routine           
*  finds the latest effective date not after the inputted one.  If              
*  all the effective dates in the file for the given station/inv# are           
*  greater than the inputted one, this routine goes to error-message            
*  handling for "Record not found".                                             
                                                                                
VKSTD2   NTR1                                                                   
         MVI   TMPKSRC,0                                                        
         XC    TMPKBK,TMPKBK                                                    
         MVC   IKEFFDB,TMPBDATE    SO THAT KEY CAN BE BUILT W/ EFF DATE         
         MVI   GOSUBN,BIK#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
                                                                                
*                                                                               
VKSTD210 DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
         CLC   RINVKEY(IKYINVL),KEYSAVE                                         
         BNE   VKSTD230                                                         
         CLC   RINVKSTD,TMPBDATE                                                
         BH    VKSTD230                                                         
         BE    VKSTD240                                                         
                                                                                
         DS    0H                  (KEY EFF DATE) < (INPUT EFF DATE)            
         MVI   RINVKSTD+L'RINVKSTD,XFF                                          
         LA    R1,L'RINVKEY-(RINVKSTD+L'RINVKSTD-RINVKEY)-2                     
         EXMVC R1,RINVKSTD+L'RINVKSTD+1,RINVKSTD+L'RINVKSTD                     
         B     VKSTD210                                                         
                                                                                
VKSTD230 DS    0H                  FINISHED READING FILE                        
         LA    R6,KEYSAVE                                                       
         OC    RINVKSTD,RINVKSTD    DO WE HAVE AN EFF DATE?                     
         BZ    RCDNTFND              NOPE, NO INVENTORY FOUND                   
         MVC   KEY,RINVKEY           YES, RETURN IT IN KEY                      
                                                                                
VKSTD240 DS    0H                                                               
         B     VKSTD1X                                                          
                                                                                
*                                                                               
         DROP  R6                                                               
                                                                                
*                                                                               
VKSTD2X  DS    0H                                                               
         B     XIT                                                              
***********************************************************************         
         TITLE 'RERMP41 - INVENTORY DEMOS (VALREC)'                             
***********************************************************************         
*========================== VALIDATE RECORD ==========================*         
VREC     DS    0H                                                               
*                                                                               
         DS    0H                  INITIALIZATION                               
         MVI   OURERRCD,0           NO MESSAGES YET                             
         MVI   OURINFCD,0                                                       
         MVI   OURWRNCD,0                                                       
         XC    MYTEXT(MYTEXTL),MYTEXT                                           
                                                                                
         XC    ACURFORC,ACURFORC                                                
         XC    AERRFLD,AERRFLD                                                  
                                                                                
*                                                                               
VR002    DS    0H                                                               
         TM    CHNGFLG1,CF1KOTD    DID KEY,OPTN,TRK,DEM CHANGE?                 
         BNZ   VR002G               YES, GO STRAIGHT TO DISPLAY RECD            
         CLI   PFKEY,PFQRDPLY      HIT PFKEY TO RE-DISPLAY?                     
         BE    VR002G               YES, GO STRAIGHT TO DISPLAY RECD            
         B     VR002X              DO NORMAL STUFF                              
*                                                                               
VR002G   DS    0H                                                               
         MVI   PFKEY,0                                                          
         B     DREC                 YES, DISPLAY RECD & IGNORE PFKEY            
VR002X   EQU   *                                                                
                                                                                
*                                                                               
** CHECK PFKEYS **                                                              
*                                                                               
         CLI   PFKEY,0                                                          
         BE    VR009                                                            
*                                                                               
         BAS   RE,VRPFKEY                                                       
         BNE   VRERROR             ERROR CODE SET BY VRPFKEY                    
VR009    EQU   *                                                                
         EJECT                                                                  
*                                                                               
** LOOP THROUGH AND VALIDATE COLUMN AT A TIME **                                
*                                                                               
         XC    CNTTRK,CNTTRK                                                    
         L     R4,AMDPLYTB                                                      
         USING MDPLYTBD,R4                                                      
                                                                                
*                                                                               
VR052    DS    0H                                                               
         MVI   GOSUBN,VCOL#         GO VALIDATE THE COLUMN                      
         GOTO1 AGOSUB                                                           
         BE    *+14                  CC EQ==>COLUMN OKAY                        
         MVC   ACURFORC,AERRFLD      CC NEQ==>AN ERROR OCCURRED                 
         B     VRERROR                                                          
*                                                                               
         DS    0H                   CHECK IF TOO MANY OVERRIDES                 
         MVI   GOSUBN,BTR#           BUILDING TRACK IS ONLY WAY TO KNOW         
         GOTO1 AGOSUB                                                           
         BE    VR054X                TRACK WAS BUILT FINE                       
                                                                                
         LH    RF,CNTTRK            FIND ADDRESS TO PLACE CURSOR                
         MH    RF,=Y(DDCELLQ)                                                   
         LA    RF,IDMROWDH(RF)                                                  
         USING DDCELLD,RF                                                       
         TM    DDCOVRH+1,X20         IF OVERRIDE FIELD IS PROTECTED,            
         BZ    *+12                                                             
         LA    RF,SZDEMROW(RF)        BUMP TO NEXT ROW                          
         B     *-12                                                             
         LA    R2,DDCOVRH            SET ADDRESS IN R2                          
         DROP  RF                                                               
         B     VRERROR                                                          
VR054X   EQU   *                                                                
*                                                                               
         DS    0H                                                               
         TM    MDTFLAG,MDTFCHNG                                                 
         BZ    *+8                                                              
         OI    MDTFLAG,MDTFDPLY      TRACK NO LONGER CHANGED                    
                                                                                
*                                                                               
         LA    R0,1                                                             
         AH    R0,CNTTRK                                                        
         CLM   R0,1,NTKLNTRY                                                    
         BNL   VR079                                                            
                                                                                
         STH   R0,CNTTRK                                                        
         LA    R4,MDPLYTBQ(R4)                                                  
         B     VR052                                                            
         DROP  R4                                                               
                                                                                
*                                                                               
VR079    EQU   *                                                                
                                                                                
         EJECT                                                                  
*                                                                               
** SAVE INVENTORY TRACK(S) **                                                   
*                                                                               
VR200    DS    0H                                                               
         CLI   PFKEY,PFQSVALL      SAVE ALL TRACKS THAT WERE CHANGED?           
         BE    VR240                YEP                                         
         CLI   PFKEY,PFQSAVC1      SAVE A SINGLE COLUMN (TRACK)?                
         BL    VR299                NOPE                                        
         CLI   PFKEY,PFQSAVC5                                                   
         BH    VR299                NOPE                                        
                                                                                
*                                                                               
*** SAVE A SINGLE TRACK ***                                                     
*                                                                               
         DS    0H                                                               
         ZIC   R4,PFKEY                                                         
         SH    R4,=Y(PFQSVMAP)     MAP PFKEY TO COLUMN NUMBER                   
         BCTR  R4,0                                                             
         MH    R4,=Y(MDPLYTBQ)                                                  
         A     R4,AMDPLYTB         R4-->MDPLYTAB ENTRY FOR COLUMN               
         USING MDPLYTBD,R4                                                      
         BAS   RE,VRBSTRK          GO BUILD AND SAVE TRACK                      
         NI    MDTFLAG,XFF-MDTFCHNG  TRACK NO LONGER CHANGED                    
         DROP  R4                                                               
*                                                                               
         MVI   OURINFCD,TKSVQ      SET MESSAGE TO DISPLAY                       
         B     VR299                                                            
                                                                                
*                                                                               
*** SAVE ALL TRACKS THAT WERE CHANGED ***                                       
*                                                                               
VR240    DS    0H                                                               
         MVI   NTKSV,0             NO TRACKS SAVED YET                          
         ZIC   R0,NTKLNTRY                                                      
         L     R4,AMDPLYTB                                                      
         USING MDPLYTBD,R4                                                      
*                                                                               
VR242    DS    0H                                                               
         TM    MDTFLAG,MDTFCHNG    WAS THIS TRACK CHANGED?                      
         BZ    VR245                                                            
                                                                                
         BAS   RE,VRBSTRK          GO BUILD AND SAVE TRACK                      
         NI    MDTFLAG,XFF-MDTFCHNG  TRACK NO LONGER CHANGED                    
                                                                                
         ZIC   R1,NTKSV            COUNT # OF TRACKS SAVED                      
         LA    R1,1(R1)                                                         
         STC   R1,NTKSV                                                         
         DROP  R4                                                               
                                                                                
VR245    DS    0H                                                               
         LA    R4,MDPLYTBQ(R4)                                                  
         BCT   R0,VR242                                                         
*                                                                               
         MVI   OURINFCD,TKSSVQ     SET MESSAGE TO DISPLAY                       
         CLI   NTKSV,1                                                          
         BH    *+8                                                              
         MVI   OURINFCD,TKSVQ                                                   
         B     VR299                                                            
                                                                                
*                                                                               
VR299    EQU   *                                                                
         B     VRX                                                              
                                                                                
                                                                                
* Helper routine to build and save a track                                      
* At entry,                                                                     
*   R4-->Master Display Table entry for track                                   
                                                                                
VRBSTRK  NTR1                                                                   
         MVI   GOSUBN,BTR#                                                      
         GOTO1 AGOSUB                                                           
         BNE   NO                                                               
                                                                                
         L     RF,AIO1             RF-->NEW TRACK                               
         MVC   KEY(L'RINVKEY),0(RF)                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                                                             
         CLC   KEY(L'RINVKEY),KEYSAVE                                           
         BNE   NO                                                               
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC                                                           
                                                                                
         MVI   GOSUBN,PLR#         GO PROCESS LTRANS REQUEST                    
         GOTO1 AGOSUB                                                           
*                                                                               
         B     YES                                                              
         EJECT                                                                  
*                                                                               
** VALREC EXIT **                                                               
*                                                                               
VRX      DS    0H                                                               
         B     DREC                                                             
         EJECT                                                                  
*========================== VALREC PFKEY TASKS =======================*         
VRPFKEY  NTR1                                                                   
                                                                                
         MVI   OURERRCD,0          ASSUME NO ERRORS                             
*                                                                               
         CLI   PFKEY,PFQSAVC1                                                   
         BE    VRPFSV1C                                                         
         CLI   PFKEY,PFQSAVC2                                                   
         BE    VRPFSV1C                                                         
         CLI   PFKEY,PFQSAVC3                                                   
         BE    VRPFSV1C                                                         
         CLI   PFKEY,PFQSAVC4                                                   
         BE    VRPFSV1C                                                         
         CLI   PFKEY,PFQSAVC5                                                   
         BE    VRPFSV1C                                                         
         CLI   PFKEY,PFQSVALL                                                   
         BE    VRPFSVAC                                                         
         CLI   PFKEY,PFQRDPLY                                                   
         BE    VRPFRDPL                                                         
         B     VRPFINVL                                                         
                                                                                
*                                                                               
** SAVE A SINGLE COLUMN **                                                      
*                                                                               
VRPFSV1C DS    0H                                                               
         ZIC   R0,PFKEY                                                         
         SH    R0,=Y(PFQSVMAP)      MAPPED BACK TO COLUMN NUMBER                
         BAS   RE,VRPFH1            SEE IF TRACK WAS CHANGED                    
         BNE   VRPFINVL              NO, THEN PFKEY WASN'T ACTIVATED            
*                                                                               
         B     VRPFKEYX                                                         
                                                                                
*                                                                               
** SAVE ALL COLUMNS **                                                          
*                                                                               
VRPFSVAC DS    0H                                                               
         ZIC   R0,NTKLNTRY          R0 = # OF COLUMNS DISPLAYED                 
*                                                                               
         BAS   RE,VRPFH1            SEE IF TRACK WAS CHANGED                    
         BE    *+12                  YES, THEN PFKEY IS OKAY                    
         BCT   R0,*-8                NO, CHECK PREVIOUS COLUMN                  
         B     VRPFINVL             PFKEY WASN'T ACTIVATED                      
*                                                                               
         B     VRPFKEYX                                                         
                                                                                
*                                                                               
** RE-DISPLAY **                                                                
*                                                                               
VRPFRDPL DS    0H                                                               
         B     VRPFKEYX                                                         
                                                                                
                                                                                
*                                                                               
VRPFINVL DS    0H                  INVALID PFKEY ERROR                          
         MVI   OURERRCD,IPFKQ                                                   
         B     NO                                                               
                                                                                
*                                                                               
VRPFKEYX DS    0H                                                               
         B     YES                                                              
                                                                                
                                                                                
* Helper routine to check if track was changed.  BYTE will be clobbered         
* At entry,                                                                     
*   R0 = column number                                                          
* At exit,                                                                      
*   CC eq  if track was changed                                                 
*   CC neq if track wasn't changed                                              
* !!WARNING!! Do NOT clobber RE                                                 
                                                                                
VRPFH1   DS    0H                                                               
         LR    R4,R0                                                            
         BCTR  R4,0                                                             
         MH    R4,=Y(MDPLYTBQ)                                                  
         A     R4,AMDPLYTB         R4-->TRACK CORRESPONDING TO COLUMN           
         USING MDPLYTBD,R4                                                      
         MVC   BYTE,MDTFLAG                                                     
         DROP  R4                                                               
*                                                                               
         NI    BYTE,MDTFCHNG        STRIP OFF OTHER FLAGS                       
         CLI   BYTE,MDTFCHNG                                                    
         BR    RE                                                               
                                                                                
***********************************************************************         
         TITLE 'RERMP41 - INVENTORY DEMOS (VALIDATION ERRORS)'                  
***********************************************************************         
*========================= VALIDATION ERRORS =========================*         
                                                                                
VKERROR  DS    0H                  (ERROR CODE MUST BE SET)                     
         B     OURERROR                                                         
                                                                                
MISSFLD  DS    0H                  MISSING FIELD ERROR                          
         MVI   OURERRCD,MFLDQ                                                   
         B     OURERROR                                                         
                                                                                
INVLFLD  DS    0H                  INVALID FIELD ERROR                          
         MVI   OURERRCD,IFLDQ                                                   
         B     OURERROR                                                         
                                                                                
RCDNTFND DS    0H                  RECORD NOT FOUND                             
         MVI   OURERRCD,RNFQ                                                    
         B     OURERROR                                                         
                                                                                
DMNOTFND DS    0H                  DEMO MENU NOT FOUND                          
         MVI   OURERRCD,DMNFQ                                                   
         B     OURERROR                                                         
                                                                                
DMTWOCH  DS    0H                  DEMO MENU MUST BE TWO CHARACTERS             
         MVI   OURERRCD,DM2CQ                                                   
         B     OURERROR                                                         
                                                                                
INVLSTA  DS    0H                  INVALID STATION                              
         MVI   OURERRCD,ISTAQ                                                   
         B     OURERROR                                                         
                                                                                
VRERROR  DS    0H                  (ERROR CODE MUST BE SET)                     
         B     OURERROR                                                         
                                                                                
***********************************************************************         
         TITLE 'RERMP41 - INVENTORY DEMOS (DISPREC)'                            
***********************************************************************         
*=========================== DISPLAY RECORD ==========================*         
DREC     DS    0H                                                               
                                                                                
*                                                                               
** HANDLE PF KEYS **                                                            
*                                                                               
         CLI   PFKEY,0             IF A PF KEY WAS PRESSED,                     
         BE    DR010                                                            
         BAS   RE,DRPFKEY           PROCESS IT                                  
         BE    DR010                                                            
         LA    R2,IDMSTTNH                                                      
         CLI   OURERRCD,0                                                       
         BH    OURERROR             DISPLAY ERROR MESSAGE                       
         CLI   OURWRNCD,0                                                       
         BH    OURWARN               OR WARNING MESSAGE IF NOT GOOD             
         DC    H'0'                                                             
                                                                                
*                                                                               
** DISPLAY HEADER INFO **                                                       
*                                                                               
DR010    DS    0H                                                               
         TM    CHNGFLG1,CF1KEY     IF KEY DIDN'T CHANGE,                        
         BZ    DR015                THEN DON'T RE-DISPLAY HDR STUFF             
                                                                                
*                                                                               
         MVC   IDMIHDT,SPACES      BLANK OUT HEADER DAY/TIME                    
         OI    IDMIHDTH+6,X80                                                   
         MVC   IDMIHPG,SPACES       AND HEADER PROGRAM NAME FIELDS              
         OI    IDMIHPGH+6,X80                                                   
*                                                                               
         L     R3,AINVHDR                                                       
         AH    R3,DSP1EL           R3-->1ST ELEMENT OF INV HEADER RECD          
         SR    R0,R0                                                            
                                                                                
DR011    DS    0H                                                               
         CLI   0(R3),0             IF AT END OF RECORD,                         
         BE    DR015                DONE DISPLAYING HEADER INFO                 
                                                                                
         CLI   0(R3),X'02'         '02' ELEM HAS DAY/TIME                       
         BNE   DR012                                                            
         LA    R2,IDMIHDT                                                       
                                                                                
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
                                                                                
         LA    RE,L'IDMIHPG                                                     
         ZIC   RF,1(R3)                                                         
         SH    RF,=Y(RIPGNAME-RIPGELEM)                                         
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EXMVC RE,IDMIHPG,(RIPGNAME-RIPGELEM)(R3)                               
                                                                                
         B     DR014                                                            
                                                                                
DR014    DS    0H                                                               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR011                                                            
                                                                                
DR015    EQU   *                                                                
                                                                                
*                                                                               
** SET FIELD PROTECTION **                                                      
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,CPRTF#                                                    
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
** DISPLAY DEMO CATEGORY NAMES **                                               
*                                                                               
         TM    CHNGFLG1,CF1DEM     DID DEMOS CHANGE?                            
         BZ    DR049                NO, DON'T RE-DISPLAY DEMO NAMES             
                                                                                
*                                                                               
         DS    0H                  CONVERT DEMOLIST TO DISPLAY-ABLE FMT         
         ZIC   R1,=AL1(MXDEMOS)                                                 
         MH    R1,=H'6'                                                         
         SH    R1,=H'2'                                                         
         MVI   BLOCK,C' '                                                       
         EXMVC R1,BLOCK+1,BLOCK     BLANK OUT OUTPUT AREA FOR DEMOCON           
*                                                                               
         MVI   GOSUBN,CDBK#                                                     
         GOTO1 AGOSUB                                                           
*                                                                               
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         MVC   DBFILE,DCFILTP                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELAGY,TWAAGY                                                  
         GOTO1 DEMOCON,DMCB,(NDEMS,IKDMLST),(6,BLOCK),(C'S',DBLOCKD),0          
         DROP  R5                                                               
                                                                                
*                                                                               
         DS    0H                  DISPLAY CATEGORY NAMES                       
         XC    CNTDEM,CNTDEM                                                    
         LA    R2,IDMROWHH          R2-->TWA FIELD                              
         USING ROWDSECT,R2                                                      
         LA    R3,IKDMLST           R3-->DEMOLIST                               
         LA    R4,BLOCK             R4-->CATEGORY NAMES                         
*                                                                               
DR046    DS    0H                                                               
         MVC   ROWHDG,0(R4)                                                     
         CLC   0(6,R4),=C'PHOMES'                                               
         BNE   *+16                                                             
         MVC   ROWHDG,SPACES                                                    
         MVC   ROWHDG(3),=C'HUT'                                                
         CLC   0(6,R4),=C'SHOMES'                                               
         BNE   *+16                                                             
         MVC   ROWHDG,SPACES                                                    
         MVC   ROWHDG(5),=C'SHARE'                                              
                                                                                
         OI    ROWHDGH+6,X80        TRANSMIT FIELD                              
*                                                                               
         DS    0H                   CHECK IF CORE DEMO                          
         MVI   ROWCORE,C' '          ASSUME IT'S NOT                            
         OI    ROWCOREH+6,X80                                                   
                                                                                
         LA    R0,COREDEMQ                                                      
         L     R1,ACOREDEM                                                      
                                                                                
DR048B   DS    0H                                                               
         CLC   0(L'COREDEMS,R1),2(R3)                                           
         BNE   *+12                                                             
         MVI   ROWCORE,C'c'         INDICATE CORE DEMO                          
         B     DR048X                                                           
         LA    R1,L'COREDEMS(R1)                                                
         BCT   R0,DR048B                                                        
DR048X   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  BUMP TO NEXT DEMO NAME                       
         LA    R0,1                                                             
         AH    R0,CNTDEM                                                        
         CLM   R0,1,=AL1(MXDEMOS)                                               
         BNL   DR049                                                            
                                                                                
         STH   R0,CNTDEM                                                        
         LA    R2,SZDEMROW(R2)                                                  
         LA    R3,L'IKDMLST(R3)                                                 
         LA    R4,6(R4)                                                         
         B     DR046                                                            
         DROP  R2                                                               
DR049    EQU   *                                                                
                                                                                
*                                                                               
** DISPLAY TRACK DATA **                                                        
*                                                                               
         DS    0H                                                               
         XC    CNTTRK,CNTTRK                                                    
         L     R4,AMDPLYTB                                                      
         USING MDPLYTBD,R4                                                      
                                                                                
*                                                                               
DR052    DS    0H                                                               
         MVI   GOSUBN,CCOL#        CLEAR COLUMN                                 
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,DCOL#        DISPLAY COLUMN                               
         GOTO1 (RF)                                                             
                                                                                
         NI    MDTFLAG,XFF-MDTFNEW TURN FLAG OFF AFTER DISPLAYING TRACK         
                                                                                
*                                                                               
         DS    0H                  BUMP TO NEXT TRACK                           
         LA    R0,1                                                             
         AH    R0,CNTTRK                                                        
         CLM   R0,1,=AL1(MXTKLNTY)                                              
         BNL   DR059                                                            
                                                                                
         STH   R0,CNTTRK                                                        
         LA    R4,MDPLYTBQ(R4)      BUMP TO NEXT TRACK ENTRY                    
         B     DR052                                                            
DR059    EQU   *                                                                
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
** DISPLAY PF LINE **                                                           
*                                                                               
DRPFL    DS    0H                                                               
         MVC   IDMPFLN,SPACES                                                   
         OI    IDMPFLNH+6,X80                                                   
*                                                                               
         LA    R2,IDMPFLN                                                       
         USING PFLINED,R2                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   PFLPF,=C'PF'                                                     
                                                                                
*                                                                               
*** "SAVE TRACKS" PFKEY(S) ***                                                  
*                                                                               
         DS    0H                                                               
         LA    R3,PFLPF+L'PFLPF                                                 
         L     R4,AMDPLYTB                                                      
         USING MDPLYTBD,R4                                                      
         XC    CNTTRK,CNTTRK                                                    
         MVI   BYTE,C'N'           USE BYTE AS A TEMPORARY FLAG                 
                                                                                
*                                                                               
DRPFL022 DS    0H                                                               
         LA    R1,1                                                             
         AH    R1,CNTTRK                                                        
         CLM   R1,1,=AL1(MXTKLNTY)                                              
         BH    DRPFL029                                                         
         STH   R1,CNTTRK            R1 = COLUMN NUMBER                          
                                                                                
*                                                                               
         TM    MDTFLAG,MDTFCHNG    WAS THIS TRACK CHANGED?                      
         BZ    DRPFL026             NOPE, JUMP TO NEXT TRACK                    
*                                                                               
         DS    0H                  DISPLAY PFKEY FOR THIS TRACK                 
         LA    R0,PFQSVMAP(R1)      R0 = PFKEY MAPPED TO COLUMN NUMBER          
         STC   R0,0(R3)                                                         
         OI    0(R3),X'F0'                                                      
         MVC   1(4,R3),=C'=SvC'                                                 
         STC   R1,5(R3)                                                         
         OI    5(R3),X'F0'                                                      
                                                                                
         MVI   BYTE,C'Y'           TURN FLAG ON--AT LEAST 1 PFSAVE KEY          
         LA    R3,L'PFLSVCOL(R3)                                                
         B     DRPFL026                                                         
                                                                                
*                                                                               
DRPFL026 DS    0H                                                               
         LA    R4,MDPLYTBQ(R4)                                                  
         B     DRPFL022                                                         
                                                                                
*                                                                               
DRPFL029 EQU   *                                                                
         CLI   BYTE,C'Y'                      IF A PFSAVE KEY IS SHOWN,         
         BNE   *+14                                                             
         MVC   0(L'PFLSVALL,R3),=C'8=SaveAll'  DISPLAY THIS ALSO                
         LA    R3,L'PFLSVALL+2(R3)                                              
                                                                                
*                                                                               
*** "RE-DISPLAY" PFKEY ***                                                      
*                                                                               
         LA    R0,PFLPF+L'PFLPF                                                 
         CR    R0,R3               IF THIS ISN'T 1ST PFKEY SHOWN,               
         BE    *+8                                                              
         LA    R3,1(R3)             SKIP AN EXTRA SPACE                         
         MVC   0(L'PFLRDPLY,R3),=C'10=Rdsply'                                   
                                                                                
*                                                                               
*** "RETURN/NEXT" PFKEY ***                                                     
*                                                                               
         CLI   CALLSP,0                                                         
         BE    DRPFL098                                                         
         MVC   PFLPF12,RE@PF12N                                                 
         CLI   ITSHNSEL,1                                                       
         BH    DRPFL098                                                         
         MVC   PFLPF12,RE@PF12R                                                 
DRPFL098 EQU   *                                                                
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
** EXIT DISPLAY RECORD **                                                       
*                                                                               
DRX      DS    0H                                                               
         MVI   GOSUBN,STI#         SAVE OFF STUFF IN TIA                        
         GOTO1 AGOSUB                                                           
*                                                                               
         CLI   OURERRCD,0          IS THERE AN ERROR MSG TO DISPLAY?            
         BE    *+12                                                             
         L     R2,AERRFLD                                                       
         B     OURERROR             YES, GO DISPLAY IT                          
*                                                                               
         CLI   OURINFCD,0          IS THERE AN INFO MSG TO DISPLAY?             
         BNE   OURINFO              YES, GO DISPLAY IT                          
*                                                                               
         LA    R2,IDMRCODH         PLACE CURSOR HERE                            
         ST    R2,ACURFORC                                                      
                                                                                
         B     XIT                                                              
         EJECT                                                                  
*============================ DO PFKEY TASKS =========================*         
DRPFKEY  NTR1                                                                   
                                                                                
         MVI   OURERRCD,0                                                       
         MVI   OURWRNCD,0                                                       
*                                                                               
         CLI   PFKEY,PFQSAVC1                                                   
         BE    DRPFSAVE                                                         
         CLI   PFKEY,PFQSAVC2                                                   
         BE    DRPFSAVE                                                         
         CLI   PFKEY,PFQSAVC3                                                   
         BE    DRPFSAVE                                                         
         CLI   PFKEY,PFQSAVC4                                                   
         BE    DRPFSAVE                                                         
         CLI   PFKEY,PFQSAVC5                                                   
         BE    DRPFSAVE                                                         
         CLI   PFKEY,PFQSVALL                                                   
         BE    DRPFSAVE                                                         
         CLI   PFKEY,PFQRDPLY                                                   
         BE    DRPFRDPL                                                         
         DC    H'0'                                                             
                                                                                
*                                                                               
** SAVE TRACK(S) **                                                             
*                                                                               
DRPFSAVE DS    0H                                                               
         B     DRPFKEYX                                                         
                                                                                
*                                                                               
** RE-DISPLAY **                                                                
*                                                                               
DRPFRDPL DS    0H                                                               
         B     DRPFKEYX                                                         
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
DRPFKEYX DS    0H                                                               
         B     YES                                                              
***********************************************************************         
         TITLE 'RERMP41 - INVENTORY DEMOS (MISCELLANEOUS)'                      
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
                                                                                
*------------------------------- GETEL -------------------------------*         
                                                                                
         GETEL R3,DATADISP,ELCODE                                               
                                                                                
                                                                                
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
         TITLE 'RERMP41 - INVENTORY DEMOS (LTORG && CONSTANTS)'                 
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
                                                                                
                                                                                
PF12DC   DC    C'12=Rtrn'                                                       
                                                                                
DFLTDEMS DC    C'PHOME,RHOME,SHOME,HOME,RMN18+,RWM18+,RAD18+,RMN1834,RW+        
               M1834,RAD1834'                                                   
DFLTDEML EQU   *-DFLTDEMS                                                       
                                                                                
MODLIST  DC    C'RTSPQXU'          DEMO MODIFIER LIST                           
MODLISTQ EQU   *-MODLIST                                                        
***********************************************************************         
                                                                                
                                                                                
         DROP  R7,R8,R9,RA,RB,RC                                                
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01)'                             
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
         ORG   RMP41+X'2000'                                                    
         ORG                                                                    
SUBR01   NMOD1 0,**4101**                                                       
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
CSA#     EQU   (R01_02-*)/4+1      CLEAR (SYSSPARE) STORAGE AREAS               
STI#     EQU   (R01_03-*)/4+1      SAVE (TABLES IN) TIA AREA                    
RTI#     EQU   (R01_04-*)/4+1      RESTORE (TABLES IN) TIA AREA                 
BMDT#    EQU   (R01_05-*)/4+1      BUILD MASTER DISPLAY TABLE                   
BMDN#    EQU   (R01_06-*)/4+1      BUILD MASTER DISPLAY TABLE ENTRY             
UMDT#    EQU   (R01_07-*)/4+1      UPDATE MASTER DISPLAY TABLE                  
QTM#     EQU   (R01_08-*)/4+1      CONVERT QH TO MILITARY TIME                  
TBK#     EQU   (R01_09-*)/4+1      TRANSLATE BOOK                               
TID#     EQU   (R01_10-*)/4+1      TRANSLATE INTERNAL DAY                       
TMT#     EQU   (R01_11-*)/4+1      TRANSLATE MILITARY TIME                      
CDA#     EQU   (R01_12-*)/4+1      CLEAR DISPLAY AREA                           
BTL#     EQU   (R01_13-*)/4+1      BUILD TRACK LIST                             
UAE#     EQU   (R01_14-*)/4+1      UPDATE ACTIVITY ELEMENT                      
CDBK#    EQU   (R01_15-*)/4+1      CLEAR DBLOCK                                 
CDUPT#   EQU   (R01_16-*)/4+1      CHECK DUPLICATE TRACK                        
CDUPD#   EQU   (R01_17-*)/4+1      CHECK DUPLICATE DEMO                         
                                                                                
R01_00   DS    0H                                                               
R01_01   B     BINVKEY             BUILD KEY TO INVENTORY RECORD                
R01_02   B     CLRAREAS            CLEAR (SYSSPARE) STORAGE AREAS               
R01_03   B     SAVETIA             SAVE (TABLES IN) TIA AREA                    
R01_04   B     RSTRTIA             RESTORE (TABLES IN) TIA AREA                 
R01_05   B     BLDMDTB             BUILD MASTER DISPLAY TABLE                   
R01_06   B     BLDMDTN             BUILD MASTER DISPLAY TABLE ENTRY             
R01_07   B     UPDMDTB             UPDATE MASTER DISPLAY TABLE                  
R01_08   B     QHRTOMIL            CONVERT QH TO MILITARY TIME                  
R01_09   B     TRSLTBK             TRANSLATE BOOK                               
R01_10   B     TRSLTIDY            TRANSLATE INTERNAL DAY                       
R01_11   B     TRSLTMTM            TRANSLATE MILITARY TIME                      
R01_12   B     CLRDSPLY            CLEAR DISPLAY AREA                           
R01_13   B     BLDTKLST            BUILD TRACK LIST                             
R01_14   B     UPACTVEL            UPDATE ACTIVITY ELEMENT                      
R01_15   B     CLRDBLK             CLEAR DBLOCK                                 
R01_16   B     CHKDUPTK            CHECK DUPLICATE TRACK                        
R01_17   B     CHKDUPDM            CHECK DUPLICATE DEMO                         
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    RC,RC                                                            
NO_01    LTR   RC,RC                                                            
XIT_01   XIT1                                                                   
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--BIK#)'                       
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
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--CSA#)'                       
*------------------------ CLEAR STORAGE AREAS ------------------------*         
*                                                                               
CLRAREAS DS    0H                                                               
         LA    RF,TABCLRQ          RF = COUNTER                                 
         LH    R4,=Y(TABCLR-RMP41)                                              
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
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--BMDT#)'                      
*--------------------- BUILD MASTER DISPLAY TABLE --------------------*         
                                                                                
* Builds the Master Display table that stores the data to be displayed          
                                                                                
BLDMDTB  DS    0H                                                               
*                                                                               
** CLEAR MASTER DISPLAY TABLE **                                                
*                                                                               
         L     R0,AMDPLYTB                                                      
         LA    R1,MDPLYTBX-MDPLYTAB                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR MASTER DISPLAY TABLE                   
                                                                                
*                                                                               
** UPDATE MASTER DISPLAY TABLE **                                               
*                                                                               
         MVI   GOSUBN,UMDT#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
BMDTX    DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--BMDN#)'                      
*------------------ BUILD MASTER DISPLAY TABLE ENTRY -----------------*         
* At entry,                                                                     
*   TMPs values are set                                                         
* At exit,                                                                      
*   TMPMDTN = MDPLYTAB entry                                                    
                                                                                
BLDMDTN  DS    0H                                                               
         XC    TMPMDTN,TMPMDTN     CLEAR OUTPUT AREA                            
*                                                                               
         LA    R4,TMPMDTN          POINT R4 TO THE TEMP MDPLYTAB ENTRY          
         USING MDPLYTBD,R4                                                      
                                                                                
*                                                                               
         OC    TMPBOOK,TMPBOOK     IF BOOK IS NOT SET,                          
         BZ    BMDN150              THEN JUST DO MISCELLANEOUS STUFF            
                                                                                
*                                                                               
** BUILD DBLOCK **                                                              
*                                                                               
         MVI   GOSUBN,BDB#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
** CALL DEMAND **                                                               
*                                                                               
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         XC    DMCB(6*4),DMCB                                                   
         GOTO1 DEMAND,DMCB,DBLOCKD,DEMHOOK                                      
*                                                                               
         OC    DBDIVSOR,DBDIVSOR   SHOULD BE NON-ZERO IF DEMAND CALL OK         
         BZ    BMDN130              OTHERWISE, NO DEMOS TO DISPLAY              
         DROP  R5                                                               
                                                                                
*                                                                               
** DEMO OVERRIDE CAPABILITY PROFILE **                                          
*                                                                               
         DS    0H                                                               
         LA    R6,MDTDIIVK                                                      
         MVC   TMPKSRC,(RINVKSRC-RINVKEY)(R6)                                   
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB              TMPQLFY HAS QUALIFIER                        
                                                                                
*                                                                               
         L     RF,AOVPRFTB                                                      
         USING OVPRFTBD,RF                                                      
                                                                                
BMDN072  DS    0H                                                               
         CLI   0(RF),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OVPRFQLF,TMPQLFY                                                 
         BE    *+12                                                             
         LA    RF,OVPRFTBQ(RF)                                                  
         B     BMDN072                                                          
*                                                                               
         DS    0H                                                               
         ZIC   R1,OVPRFDSP                                                      
         LA    R1,RMPPROFS(R1)                                                  
         MVC   HALF+0(1),0(R1)     HALF+0(1) = PROFILE SETTING                  
         MVC   HALF+1(1),OVPRFBIT  HALF+1(1) = OVERRIDE BIT                     
         NC    HALF(1),HALF+1      IF LOGICAL AND YIELDS NON-ZERO,              
         BZ    *+8                                                              
         OI    MDTFLAG,MDTFNOVR     DISALLOW DEMO OVERRIDE FOR TRACK            
         DROP  RF                                                               
                                                                                
*                                                                               
** CHECK INDIVIDUAL DEMOS OVERRIDE-ABLE **                                      
*                                                                               
         LA    R2,IKDMLST                                                       
         LA    R3,MDTDVCLL                                                      
         USING DVCELLD,R3                                                       
                                                                                
*                                                                               
BMDN082  DS    0H                  LOOP THROUGH DEMO LIST                       
         CLI   0(R2),XFF                                                        
         BE    BMDN089                                                          
                                                                                
*                                                                               
         L     RF,AOVDMTAB                                                      
         USING OVDMTABD,RF                                                      
                                                                                
BMDN084  DS    0H                  CHECK IF DEMO MODIFIER IN TABLE              
         CLI   0(RF),EOT           IF END OF TABLE REACHED,                     
         BE    BMDN086              DISALLOW OVERRIDE FOR THIS DEMO             
         CLC   OVDMMOD,1(R2)       IF DEMO MODIFIER IS IN TABLE,                
         BE    BMDN088              CHECK NEXT DEMO                             
         LA    RF,OVDMTABQ(RF)                                                  
         B     BMDN084                                                          
         DROP  RF                                                               
*                                                                               
BMDN086  DS    0H                  DEMO CAN'T BE OVERRIDDEN                     
         OI    DVCFLAG,DVCFNOVR     FLAG IT AS SO                               
         B     BMDN088                                                          
                                                                                
*                                                                               
BMDN088  DS    0H                  BUMP TO NEXT DEMO                            
         LA    R2,L'IKDMLST(R2)     IN DEMO LIST                                
         LA    R3,DVCELLQ(R3)       IN MASTER DISPLAY TABLE ENTRY               
         B     BMDN082                                                          
                                                                                
*                                                                               
BMDN089  EQU   *                                                                
         DROP  R3                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     BMDN150                                                          
                                                                                
*                                                                               
** TRACK NOT FOUND BY DEMAND **                                                 
*                                                                               
BMDN130  DS    0H                                                               
         MVI   GOSUBN,GKS#                                                      
         GOTO1 AGOSUB              KEY SOURCE SET IN TMPKSRC                    
         MVC   TMPKBK,TMPBOOK                                                   
                                                                                
         MVI   GOSUBN,BIK#                                                      
         GOTO1 AGOSUB                                                           
         MVC   MDTDIIVK,KEY        SET KEY                                      
*                                                                               
         OI    MDTFLAG,MDTFNOTF    TRACK NOT FOUND BY DEMAND                    
                                                                                
*                                                                               
         DS    0H                                                               
         B     BMDN150                                                          
                                                                                
*                                                                               
** MISCELLANEOUS STUFF **                                                       
*                                                                               
BMDN150  DS    0H                                                               
                                                                                
*                                                                               
         DROP  R4                                                               
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
         DS    0H                                                               
         B     XIT_01                                                           
         EJECT                                                                  
*                                                                               
** DEMAND HOOK **                                                               
*                                                                               
* At entry,                                                                     
*   CNTTRK = nth track being processed                                          
*   R4   --> area to put a MDPLYTAB entry                                       
*   R5   --> DBLOCK                                                             
                                                                                
DEMHOOK  DS    0H                                                               
DMHK     NTR1                                                                   
         DS    0XL(1-(DMHK-DEMHOOK))                                            
                                                                                
*                                                                               
         USING MDPLYTBD,R4                                                      
         USING DBLOCKD,R5                                                       
*                                                                               
         DS    0H                  SAVE OFF DBLOCK IN DEMAND CALL               
         L     R0,ASVDBLK                                                       
         LA    R1,SVDBLCKX-SVDBLOCK                                             
         LA    RE,DBLOCKD                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,DBAREC                                                        
         USING RINVREC,R6                                                       
*                                                                               
         MVC   DATADISP,DSP1EL                                                  
                                                                                
*                                                                               
** EXTRACT DATA TO PUT INTO MDPLYTAB ENTRY **                                   
*                                                                               
*** INVENTORY KEY AND D/A ***                                                   
*                                                                               
         DS    0H                                                               
         MVC   MDTDIR(L'MDTDIIVK+L'MDTDICTL+L'MDTDIDA),RINVREC                  
                                                                                
*                                                                               
*** PROGRAM TITLE FOOTNOTE ***                                                  
*                                                                               
         DS    0H                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 DEFINE,MYDMCB,=C'TRAK',DBLOCKD,WORK                              
         MVC   MDTFTNT,WORK                                                     
                                                                                
*                                                                               
*** CODE AND TEXT ***                                                           
*                                                                               
         DS    0H                                                               
         LR    R3,R6                                                            
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL1                                                        
         BNE   DMHK049                                                          
                                                                                
         USING RINVCEL,R3                                                       
         MVC   MDTCODE,RINVCODE                                                 
         MVC   MDTTEXT,RINVCTXT                                                 
         DROP  R3                                                               
DMHK049  EQU   *                                                                
                                                                                
*                                                                               
*** DEMO VALUES AND OVERRIDE FLAGS ***                                          
*                                                                               
         DS    0H                                                               
         XC    TMPDMVLS(TMPDMVLL),TMPDMVLS                                      
         MVC   DBFILE,DCFILINV                                                  
         GOTO1 DEMOUT,MYDMCB,(C'L',IKDMLST),DBLOCKD,TMPDMVLS                    
         MVC   DBFILE,DCFILIUN                                                  
                                                                                
*                                                                               
         XC    CNTDEM,CNTDEM                                                    
                                                                                
*                                                                               
DMHK062  DS    0H                  SEED DEMO VALUES                             
         LH    R2,CNTDEM                                                        
         MH    R2,=Y(DVCELLQ)                                                   
         LA    R2,MDTDVCLL(R2)     R2-->APPROPRIATE CELL TO PUT DEM VAL         
         USING DVCELLD,R2                                                       
*                                                                               
         LH    RE,CNTDEM           RE = NTH DEMO BEING PROCESSED                
         SLL   RE,2                 MULTIPLY BY 4                               
         LA    RE,TMPDMVLS(RE)     RE-->DEMO VALUE FROM DEMOUT                  
         MVC   DVCOVRDM,0(RE)      MOVE DEMO VALUE INTO CELL                    
*                                                                               
         DS    0H                  CHECK FOR PRECISION                          
         LH    RE,CNTDEM            RE = NTH DEMO BEING PROCESSED               
         MH    RE,=Y(L'IKDMLST)                                                 
         LA    RE,IKDMLST(RE)       RE-->DEMOLIST ENTRY                         
         OI    DVCFLAG,DVCF1DEC     ASSUME 1 DECIMAL PLACE                      
         CLI   1(RE),C'U'                                                       
         BNE   *+8                                                              
         NI    DVCFLAG,XFF-DVCF1DEC  UNLESS DEMO IS A UNIVERSE                  
*                                                                               
         DS    0H                  CHECK FOR OVERRIDES                          
         LH    RE,CNTDEM            RE = NTH DEMO BEING PROCESSED               
         MH    RE,=Y(L'IKDMLST)                                                 
         LA    RE,IKDMLST(RE)       RE-->DEMOLIST ENTRY                         
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING RINVNOEL,RF                                                      
         MVI   RINVNOEL,X'DE'                                                   
         MVI   RINVNOZR,0                                                       
         MVI   RINVNOCT,0                                                       
         MVC   RINVNOTP,1(RE)       MOVE IN DEMO MODIFIER                       
         MVC   RINVNODM,2(RE)       MOVE IN DEMO CATEGORY NUMBER                
         DROP  RF                                                               
                                                                                
         LA    R0,(RINVNODM+L'RINVNODM-RINVNOEL)-2                              
         GOTO1 HELLO,MYDMCB,(C'G',DBFILNAM),(ELEM,(R6)),               +        
               ((R0),ELEM+2),0                                                  
         CLI   MYDMCB+12,0          IF DEMO OVERRIDE ELEM FOUND,                
         BNE   *+8                                                              
         OI    DVCFLAG,DVCFDOVD      FLAG DEMO VALUE OVERRIDDEN                 
*                                                                               
         DS    0H                  CHECK FOR MINIMUM VALUE OVERRIDES            
         LH    RE,CNTDEM            RE = NTH DEMO BEING PROCESSED               
         MH    RE,=Y(L'IKDMLST)                                                 
         LA    RE,IKDMLST(RE)       RE-->DEMOLIST ENTRY                         
         MVC   MYDEMDFY,1(RE)                                                   
                                                                                
         LR    R3,R6                                                            
         MVI   ELCODE,X'05'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL1                                                        
         B     DMHK076D                                                         
                                                                                
DMHK076B BAS   RE,NEXTEL1                                                       
DMHK076D BNE   DMHK076G                                                         
         USING RAVLNEL,R3                                                       
         CLI   RAVLNTYP,X'07'                                                   
         BNE   DMHK076B                                                         
         CLC   RAVLNCAT,MYDEMDFY                                                
         BNE   DMHK076B                                                         
         OI    DVCFLAG,DVCFDMIN      FLAG DEMO VALUE HAS MINIMUM OVRD           
         DROP  R3                                                               
DMHK076G EQU   *                                                                
         DROP  R2                                                               
*                                                                               
         DS    0H                   PROCESS NEXT DEMO                           
         LA    R1,1                                                             
         AH    R1,CNTDEM                                                        
         CLM   R1,1,NDEMS                                                       
         BNL   *+12                                                             
         STH   R1,CNTDEM                                                        
         B     DMHK062                                                          
                                                                                
*                                                                               
*** DEMO VALUES W/O OVERRIDES ***                                               
*                                                                               
         DS    0H                  GET NON-OVERRIDDEN CALC DEMOS                
         MVC   AINVTRK,DBAREC                                                   
         MVC   TMPFLNAM,DBFILNAM                                                
         MVI   DDOEFLAG,DDOEFCAL                                                
         MVI   GOSUBN,DDOE#                                                     
         GOTO1 AGOSUB               DELETE CORE DEMO OVERRIDE ELEMS             
                                                                                
         XC    TMPDMVLS(TMPDMVLL),TMPDMVLS                                      
         MVC   DBFILE,DCFILINV                                                  
         GOTO1 DEMOUT,MYDMCB,(C'L',IKDMLST),DBLOCKD,TMPDMVLS                    
         MVC   DMVL_NOV(DMVL_NOL),TMPDMVLS                                      
*                                                                               
         DS    0H                  GET NON-OVERRIDDEN CORE DEMOS                
         MVI   DDOEFLAG,DDOEFCOR                                                
         MVI   GOSUBN,DDOE#                                                     
         GOTO1 AGOSUB               DELETE CORE DEMO OVERRIDE ELEMS             
                                                                                
         XC    TMPDMVLS(TMPDMVLL),TMPDMVLS                                      
         GOTO1 DEMOUT,MYDMCB,(C'L',IKDMLST),DBLOCKD,TMPDMVLS                    
         MVC   DBFILE,DCFILIUN                                                  
*                                                                               
         DS    0H                  MERGE NON-OVERRIDDEN CALC & CORE             
         ZIC   R0,NDEMS                                                         
         SR    R2,R2                                                            
         LA    R3,IKDMLST                                                       
         MVI   GOSUBN,ICD#                                                      
                                                                                
DMHK082G DS    0H                                                               
         MVC   TMPDEMCD,0(R3)                                                   
         GOTO1 AGOSUB              IS IT A CORE DEMO?                           
         BNE   *+18                 NOPE                                        
         LA    RE,DMVL_NOV(R2)                                                  
         LA    RF,TMPDMVLS(R2)                                                  
         MVC   0(4,RE),0(RF)        YEP, MOVE IN NON-OVERRIDDEN CORE            
         LA    R2,4(R2)                                                         
         LA    R3,L'IKDMLST(R3)                                                 
         BCT   R0,DMHK082G                                                      
                                                                                
*                                                                               
         XC    CNTDEM,CNTDEM                                                    
                                                                                
*                                                                               
DMHK084  DS    0H                  SEED NON-OVERRIDDEN DEMO VALUES              
         LH    R2,CNTDEM            R2 = NTH DEMO BEING PROCESSED               
         LR    RE,R2                RE = NTH DEMO BEING PROCESSED               
         MH    R2,=Y(DVCELLQ)                                                   
         LA    R2,MDTDVCLL(R2)      R2-->APPROPRIATE CELL TO PUT DM VAL         
         USING DVCELLD,R2                                                       
*                                                                               
         SLL   RE,2                 (MULTIPLY BY 4)                             
         LA    RE,DMVL_NOV(RE)      RE-->DEMO VALUE FROM DEMOUT                 
         MVC   DVCDEMVL,0(RE)       MOVE NON-OVR DEMO VALUE INTO CELL           
         DROP  R2                                                               
*                                                                               
         DS    0H                   PROCESS NEXT DEMO                           
         LA    R1,1                                                             
         AH    R1,CNTDEM                                                        
         CLM   R1,1,NDEMS                                                       
         BNL   *+12                                                             
         STH   R1,CNTDEM                                                        
         B     DMHK084                                                          
                                                                                
*                                                                               
** DONE EXTRACTING DATA **                                                      
*                                                                               
         DS    0H                  RESTORE DBLOCK USED IN DEMAND CALL           
         LA    R0,DBLOCKD                                                       
         LA    R1,DBLOCK1X-DBLOCK1                                              
         L     RE,ASVDBLK                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
         B     DMHKX                                                            
         DROP  R6                                                               
                                                                                
*                                                                               
** DEMHOOK EXIT **                                                              
*                                                                               
DMHKX    DS    0H                                                               
         B     XIT_01                                                           
         DROP  R4,R5                                                            
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--UMDT#)'                      
*-------------------- UPDATE MASTER DISPLAY TABLE --------------------*         
                                                                                
UPDMDTB  DS    0H                                                               
         XC    CNTTRK,CNTTRK                                                    
*                                                                               
         L     R4,AMDPLYTB                                                      
         USING MDPLYTBD,R4                                                      
                                                                                
UMDT022  DS    0H                  SEE IF USER INPUTTED A DIFFERENT TRK         
         LH    RE,CNTTRK                                                        
         MH    RE,=Y(L'IKTKLST)                                                 
         LR    RF,RE                                                            
         LA    RE,IKTKLST(RE)                                                   
         LA    RF,SVTKLST(RF)                                                   
         CLC   0(L'IKTKLST,RE),0(RF)   MATCH SOURCE,BOOK                        
         BNE   UMDT030                                                          
*                                                                               
         LH    RE,CNTTRK                                                        
         MH    RE,=Y(L'IKBTLST)                                                 
         LR    RF,RE                                                            
         LA    RE,IKBTLST(RE)                                                   
         LA    RF,SVBTLST(RF)                                                   
         CLC   0(L'IKBTLST,RE),0(RF)   MATCH BOOKTYPE                           
         BNE   UMDT030                                                          
*                                                                               
         DS    0H                  SAME TRACK AS BEFORE                         
         B     UMDT050              CHECK ANY CHANGE IN DEMO INPUT              
                                                                                
*                                                                               
UMDT030  DS    0H                  REPLACE MDPLYTAB ENTRY W/ NEW TRACK          
         BAS   RE,UMDT_BMDN                BUILD ENTRY                          
         MVC   MDPLYTBD(MDPLYTBQ),TMPMDTN   AND PUT IT IN TABLE                 
         OI    MDTFLAG,MDTFNEW+MDTFCPRT    NEW TRACK--DIFF FROM PREV            
*                                                                               
         DS    0H                  CLR, RESET PRTCTN & DSPLY ALL CELLS          
         ZIC   R0,=AL1(MXDEMOS)                                                 
         LA    RE,MDTDVCLL                                                      
         USING DVCELLD,RE                                                       
                                                                                
UMD035B  DS    0H                                                               
         OI    DVCFLAG,DVCFCPRT+DVCFCCLL                                        
         OC    MDTDIIVK,MDTDIIVK                                                
         BZ    *+8                                                              
         OI    DVCFLAG,DVCFDPLY                                                 
         TM    MDTFLAG,MDTFNOTF                                                 
         BZ    *+8                                                              
         NI    DVCFLAG,XFF-DVCFDPLY                                             
         LA    RE,DVCELLQ(RE)                                                   
         BCT   R0,UMD035B                                                       
         DROP  RE                                                               
*                                                                               
         B     UMDT150                                                          
                                                                                
*                                                                               
UMDT050  DS    0H                  TRACK WAS NOT RE-INPUTTED                    
         CLC   SVDMLST(SVDMLSTL),IKDMLST  ANY DEMOS RE-INPUTTED?                
         BE    UMDT150                     NOPE                                 
                                                                                
*                                                                               
         DS    0H                  AT LEAST ONE DEMO RE-INPUTTED                
         BAS   RE,UMDT_BMDN               BUILD ENTRY                           
*                                                                               
         OC    MDTDIIVK,MDTDIIVK          IS THERE REALLY A TRACK HERE?         
         BZ    UMDT056X                                                         
         TM    MDTFLAG,MDTFNOTF                                                 
         BNZ   UMDT056X                                                         
         MVI   GOSUBN,BTR#                 YES, WE'LL WANT TO USE THE           
         GOTO1 AGOSUB                                                           
         MVC   AINVTRK,AIO1                                                     
         MVI   GOSUBN,GDV#                  DEMO VALUES FROM ENTRY              
         GOTO1 (RF)                                                             
UMDT056X EQU   *                                                                
*                                                                               
         OI    MDTFLAG,MDTFCPRT+MDTFDPLY  RESET PROTECTION & DISPLAY            
         XC    CNTDEM,CNTDEM              INITIALIZE COUNTER                    
                                                                                
*                                                                               
UMDT062  DS    0H                  BUMP THRU TO SEE WHICH DEMO CHANGED          
         LH    RE,CNTDEM                                                        
         MH    RE,=Y(L'IKDMLST)                                                 
         LR    RF,RE                                                            
         LA    RE,IKDMLST(RE)                                                   
         LA    RF,SVDMLST(RF)                                                   
         CLC   0(L'IKDMLST,RE),0(RF)  IF DEMO CATEGORY THE SAME,                
         BE    UMDT080                 BUMP TO NEXT ONE                         
*                                                                               
         DS    0H                  THIS DEMO CATEGORY CHANGED                   
         LH    R2,CNTDEM                                                        
         MH    R2,=Y(DVCELLQ)                                                   
         LA    R2,(MDTDVCLL-MDPLYTBD)(R2)                                       
         LR    R3,R2                                                            
         LA    R2,MDPLYTBD(R2)                                                  
         LA    R3,TMPMDTN(R3)                                                   
         MVC   0(DVCELLQ,R2),0(R3)                                              
         USING DVCELLD,R2                                                       
                                                                                
         OC    MDTDIIVK,MDTDIIVK   IF THERE IS A TRACK IN THIS ENTRY,           
         BZ    UMDT073X                                                         
         TM    MDTFLAG,MDTFNOTF                                                 
         BNZ   UMDT073X                                                         
         LH    RE,CNTDEM                                                        
         SLL   RE,2                                                             
         LR    RF,RE                                                            
         LA    RE,DMVL_NOV(RE)                                                  
         LA    RF,DMVL_OVR(RF)                                                  
         MVC   DVCDEMVL,0(RE)       USE THE DEMO VALUES FROM IT                 
         MVC   DVCOVRDM,0(RF)                                                   
UMDT073X EQU   *                                                                
                                                                                
         OI    DVCFLAG,DVCFCPRT+DVCFCCLL                                        
         LH    R0,CNTDEM                                                        
         CLM   R0,1,NDEMS                                                       
         BNL   *+8                                                              
         OI    DVCFLAG,DVCFDPLY                                                 
                                                                                
         DROP  R2                                                               
*                                                                               
UMDT080  DS    0H                  BUMP TO CHECK NEXT DEMO                      
         LA    R0,1                                                             
         AH    R0,CNTDEM                                                        
         CLM   R0,1,=AL1(MXDEMOS)                                               
         BNL   UMDT150                                                          
         STH   R0,CNTDEM                                                        
         B     UMDT062                                                          
                                                                                
*                                                                               
UMDT150  DS    0H                  BUMP TO CHECK NEXT TRACK                     
         LA    R0,1                                                             
         AH    R0,CNTTRK                                                        
         CLM   R0,1,=AL1(MXTKLNTY)                                              
         BNL   UMDTX                                                            
                                                                                
         STH   R0,CNTTRK                                                        
         LA    R4,MDPLYTBQ(R4)                                                  
         B     UMDT022                                                          
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
UMDTX    DS    0H                                                               
         B     XIT_01                                                           
         DROP  R4                                                               
                                                                                
                                                                                
* Helper routine to build a MASTER DISPLAY TABLE entry in TMPMDTN               
                                                                                
UMDT_BMDN NTR1                                                                  
         MVC   TMPSTTN,IKSTTN      STATION                                      
         MVC   TMPINVN,IKINV       INVENTORY NUMBER                             
         MVC   TMPCDATE,IKEFFDC    EFFECTIVE DATE                               
                                                                                
         LH    RE,CNTTRK                                                        
         LR    RF,RE                                                            
         MH    RE,=Y(L'IKTKLST)                                                 
         LA    RE,IKTKLST(RE)                                                   
         MVC   TMPFRBK,0(RE)       FROM BITS & BOOK                             
                                                                                
         MH    RF,=Y(L'IKBTLST)                                                 
         LA    RF,IKBTLST(RF)                                                   
         MVC   TMPBTYP,0(RF)       BOOKTYPE                                     
                                                                                
*                                                                               
         MVI   GOSUBN,BMDN#                                                     
         GOTO1 AGOSUB              TMPMDTN HAS ENTRY ON RETURN                  
                                                                                
*                                                                               
         B     XIT_01                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--STI# && RTI#)'               
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
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--QTM#)'                       
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
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--TBK#)'                       
*--------------------------- TRANSLATE BOOK --------------------------*         
                                                                                
* At entry,                                                                     
*   TMPBOOK = book to translate                                                 
*   TMPQLFY = track qualifier                                                   
*   TMPBTYP = booktype                                                          
* At exit,                                                                      
*   WORK    = converted book                                                    
*   HALF    = L(converted book)                                                 
                                                                                
TRSLTBK  DS    0H                                                               
                                                                                
         DS    0H                  INITIALIZATION                               
         MVC   WORK,SPACES                                                      
         LA    R2,WORK                                                          
                                                                                
*                                                                               
         DS    0H                  SCOPE OUT BOOK FIRST                         
         OC    TMPBOOK,TMPBOOK      IF BOOK IS NULLS,                           
         BZ    TBK100                DON'T DO ANYTHING                          
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(L'TMPBOOK),TMPBOOK                                           
*                                                                               
         CLI   TMPBOOK+1,ESTMNTHQ   CHECK FOR EST/YR BOOKS                      
         BNE   *+8                                                              
         MVI   DUB+1,0                                                          
                                                                                
*                                                                               
** START FORMATTING **                                                          
*                                                                               
         DS    0H                  CHECK FOR PROJ/EST/SPCL SURVEYS              
         CLI   TMPQLFY,C' '         SKIP IF ACTUAL                              
         BE    TBK029                                                           
         MVC   0(1,R2),TMPQLFY                                                  
         LA    R2,1(R2)                                                         
TBK029   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  FORMAT BOOK VALUE                            
         CLI   DUB+1,0                                                          
         BNE   TBK035                                                           
*                                                                               
         DS    0H                   BOOK IS EST/YR                              
         MVC   0(2,R2),=C'ST'                                                   
         LA    R2,2(R2)                                                         
         ZIC   R1,DUB                                                           
         EDIT  (R1),(2,(R2)),DUB=MYDUB,WRK=MYWORK,ZERO=NOBLANK,FILL=0           
         LA    R2,2(R2)                                                         
         B     TBK039                                                           
*                                                                               
TBK035   DS    0H                   BOOK IS MON/YR                              
         GOTO1 DATCON,DMCB,(X'83',DUB),(6,(R2))                                 
         LA    R2,6(R2)                                                         
*                                                                               
TBK039   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  FORMAT BOOKTYPE                              
         CLI   TMPBTYP,0                                                        
         BE    TBK049                                                           
         MVI   0(R2),C'('                                                       
         MVC   1(1,R2),TMPBTYP                                                  
         MVI   2(R2),C')'                                                       
         LA    R2,3(R2)                                                         
TBK049   EQU   *                                                                
                                                                                
*                                                                               
** DONE FORMATTING **                                                           
*                                                                               
TBK100   DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R2,R0                                                            
         STH   R2,HALF             RETURN L(FORMATTED BOOK) IN HALF             
         B     TBKX                                                             
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
TBKX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--TID#)'                       
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
         B     TIDX                                                             
*                                                                               
TIDX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--TMT#)'                       
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
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--CDA#)'                       
*------------------------- CLEAR DISPLAY AREA ------------------------*         
                                                                                
CLRDSPLY DS    0H                                                               
*                                                                               
** COLUMN HEADINGS **                                                           
*                                                                               
         DS    0H                                                               
         LA    R2,IDMCOLHH                                                      
         MVI   NTIMES,MXTKLNTY                                                  
         BAS   RE,CDAXCLIN                                                      
                                                                                
*                                                                               
** PROGRAM TITLE FOOTNOTES **                                                   
*                                                                               
         DS    0H                                                               
         LA    R2,IDMFTNTH                                                      
         MVI   NTIMES,MXTKLNTY                                                  
         BAS   RE,CDAXCLIN                                                      
                                                                                
*                                                                               
** CODE/TEXT **                                                                 
*                                                                               
         DS    0H                                                               
         LA    R2,IDMRCODH                                                      
         MVI   NTIMES,2*MXTKLNTY                                                
         BAS   RE,CDAXCLIN                                                      
                                                                                
*                                                                               
** DEMOGRAPHIC VALUES **                                                        
*                                                                               
         DS    0H                                                               
         LA    R0,MXDEMOS                                                       
         LA    R3,IDMROWHH                                                      
         USING ROWDSECT,R3                                                      
                                                                                
*                                                                               
CDA042   DS    0H                                                               
         XC    ROWHDG,ROWHDG       CLEAR ROW HEADING                            
         OI    ROWHDGH+6,X80                                                    
*                                                                               
         XC    ROWCORE,ROWCORE     CLEAR CORE DEMO FLAG                         
         OI    ROWCOREH+6,X80                                                   
*                                                                               
         LA    R2,ROWDEMOS         CLEAR DEMO COLUMNS                           
         MVI   NTIMES,2*MXTKLNTY                                                
         BAS   RE,CDAXCLIN                                                      
*                                                                               
         LA    R3,ROWL(R3)                                                      
         BCT   R0,CDA042                                                        
         DROP  R3                                                               
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
         DS    0H                                                               
         B     XIT_01                                                           
                                                                                
                                                                                
* Little helper routine to clear out fields on a single line                    
* At entry,                                                                     
*   NTIMES = # of fields on that single line                                    
*   R2    -->field to start at                                                  
                                                                                
CDAXCLIN NTR1                                                                   
         ZIC   R0,NTIMES           R0 = # OF TIMES TO LOOP                      
                                                                                
CDAXL012 DS    0H                                                               
         ZIC   RF,0(R2)            RF = L(ENTIRE FIELD)                         
         LR    RE,RF                                                            
                                                                                
         SH    RF,=H'8'            ADJUST FOR HEADER                            
         TM    1(R2),X02                                                        
         BZ    *+8                                                              
         SH    RF,=H'8'             AND EXTENDED HEADER (IF ANY)                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
                                                                                
         OI    6(R2),X80           TRANSMIT IT                                  
         AR    R2,RE               BUMP TO NEXT FIELD                           
         BCT   R0,CDAXL012                                                      
                                                                                
*                                                                               
         B     XIT_01                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--BTL#)'                       
*-------------------------- BUILD TRACK LIST -------------------------*         
                                                                                
* Builds a list of the latest tracks for the given inventory header.            
* At exit,                                                                      
*   MYFLD = dummy TWA field containing the list of tracks                       
                                                                                
BLDTKLST DS    0H                                                               
                                                                                
         DS    0H                  INITIALIZE FIELDS                            
         MVI   NTKLNTRY,0                                                       
         XC    TRKLIST(TRKLISTL),TRKLIST                                        
*                                                                               
         DS    0H                  INITIALIZE BINSRCH PARMS                     
         XC    BSPARMS(6*4),BSPARMS                                             
         L     R0,AIO               USE I/O AREA                                
         ST    R0,BSP2               AS BINSRCH TABLE                           
         LA    R0,BINTRKQ                                                       
         ST    R0,BSP4              L(RECORD)                                   
         LA    R0,BINTKKYL                                                      
         ST    R0,BSP5              DISPL TO KEY & L(KEY)                       
         LA    R0,MXTKLNTY+1                                                    
         ST    R0,BSP6              MAX ENTRIES IN TABLE                        
*                                                                               
         DS    0H                  INITIALIZE BINSRCH TABLE                     
         L     R0,AIO                                                           
         LA    R1,L'IO                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
** GET MOST RECENT FIVE TRACKS **                                               
*                                                                               
         MVI   TMPKSRC,0                                                        
         XC    TMPKBK,TMPKBK                                                    
         MVI   GOSUBN,BIK#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         GOTO1 HIGH                                                             
         B     BTL025                                                           
                                                                                
BTL022   DS    0H                                                               
         GOTO1 SEQ                                                              
         B     BTL025                                                           
                                                                                
*                                                                               
BTL025   DS    0H                                                               
         CLC   KEY(IKYSTDL),KEYSAVE                                             
         BNE   BTL200                                                           
                                                                                
         DS    0H                  DECIPHER KEY SOURCE                          
         MVC   TMPKSRC,KEY+(RINVKSRC-RINVKEY)                                   
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB                                                           
         BNE   BTL022               READ NEXT RECD IF NOT A DEMO TRACK          
                                                                                
*                                                                               
*** BUILD BINSRCH RECORD FOR THIS TRACK ***                                     
*                                                                               
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
                                                                                
         DS    0H                   PUT BINSRCH KEY TOGETHER                    
         XC    WORK,WORK             USE WORK FOR BINSRCH RECORD                
         LA    R4,WORK                                                          
         USING BINTRKD,R4                                                       
                                                                                
         MVC   BINTKXBK,RINVKBK                                                 
         CLI   RINVKBK+1,0                                                      
         BNE   *+8                                                              
         MVI   BINTKXBK+1,ESTMNTHQ                                              
         XC    BINTKXBK,=X'FFFF'     BINKEY: REVERSE BOOK YEAR/MONTH            
                                                                                
         MVC   BINTKTQK,TMPQLFK      BINKEY: TRACK QUALIFIER KEY                
                                                                                
         MVC   BINTKSRC,TMPSRC       BINKEY: RATING SERVICE                     
                                                                                
         MVC   BINTKBT,TMPBTYP       BINKEY: BOOKTYPE                           
                                                                                
         MVC   BINTKQLF,TMPQLFY      BINTRK: TRACK QUALIFIER                    
         DROP  R6                                                               
                                                                                
*                                                                               
*** PUT RECORD INTO BINSRCH TABLE ***                                           
*                                                                               
         ST    R4,BSP1              A(RECORD)                                   
         MVI   BSP1,X01             INSERT RECORD IF NOT FOUND                  
         GOTO1 VBINSRCH,BSPARMS                                                 
         DROP  R4                                                               
                                                                                
         CLC   BSP3,BSP6           (# RECS NOW SO FAR) VS. (MAX # RECS)         
         BL    BTL049               LOW: DON'T MANIPULATE TABLE                 
         BE    *+6                  EQL: MANIPULATE TABLE                       
         DC    H'0'                                                             
                                                                                
         DS    0H                  TABLE MAXXED OUT--REMOVE LAST ENTRY          
         L     R1,BSP4              R1 = L(BINSRCH RECORD)                      
         L     RF,BSP6                                                          
         BCTR  RF,0                 MAX ENTRIES MINUS ONE                       
         SR    RE,RE                                                            
         MR    RE,R1                                                            
         A     RF,BSP2              RF-->LAST ENTRY                             
         BCTR  R1,0                 R1 = EX LENGTH                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)        CLEAR LAST ENTRY                            
                                                                                
         L     R1,BSP3                                                          
         BCTR  R1,0                                                             
         ST    R1,BSP3              DECREMENT # ENTRIES SO FAR                  
BTL049   EQU   *                                                                
         B     BTL022                                                           
                                                                                
*                                                                               
** BUILD DUMMY TWA FIELD WITH TRACKS **                                         
*                                                                               
BTL200   DS    0H                                                               
         XC    MYFLD(MYFLDL),MYFLD                                              
         OC    BSP3,BSP3           IF NO TRACKS TO DISPLAY,                     
         BZ    BTLX                 THEN EXIT NOW                               
                                                                                
*                                                                               
         L     R1,BSP3             R1 = # OF BINSRCH RECORDS                    
         C     R1,BSP6             IF NOT LOWER THAN MAX ALLOWED,               
         BL    *+6                                                              
         BCTR  R1,0                 DECREMENT # OF BINSRCH RECDS                
         STC   R1,COUNTER          COUNTER = # OF ENTRIES TO DO                 
*                                                                               
         MVI   PRVSRC,0            INITIALIZE PREVIOUS SOURCE                   
*                                                                               
         LA    R2,MYFLDD           R2-->DUMMY TWA FIELD                         
         L     R4,BSP2             R4-->BINSRCH TABLE                           
         USING BINTRKD,R4                                                       
                                                                                
*                                                                               
BTL210   DS    0H                                                               
         CLC   PRVSRC,BINTKSRC     IF PREV RSVC = CURR RSVC                     
         BE    BTL212X              DON'T FORMAT IT INTO FIELD                  
         MVC   PRVSRC,BINTKSRC                                                  
         MVC   0(1,R2),BINTKSRC                                                 
         LA    R2,1(R2)                                                         
         BAS   RE,BTLCOMMA                                                      
BTL212X  EQU   *                                                                
*                                                                               
         MVC   TMPQLFY,BINTKQLF    TRACK QUALIFIER                              
         MVC   TMPBOOK,BINTKXBK    BOOK YEAR/MONTH                              
         XC    TMPBOOK,=X'FFFF'                                                 
         MVC   TMPBTYP,BINTKBT     BOOKTYPE                                     
         MVI   GOSUBN,TBK#         TRANSLATE BOOK                               
         GOTO1 AGOSUB                                                           
         ZICM  R1,HALF,(3)                                                      
         BZ    BTL214X                                                          
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK                                                    
         LA    R2,1(R1,R2)                                                      
BTL214X  EQU   *                                                                
*                                                                               
         ZIC   R1,COUNTER                                                       
         BCT   R1,*+8              IF WE'RE NOT DONE LOOPING,                   
         B     BTL250                                                           
         STC   R1,COUNTER           UPDATE COUNTER                              
         A     R4,BSP4              BUMP TO NEXT BINSRCH RECORD                 
         BAS   RE,BTLCOMMA          INSERT ANOTHER COMMA NOW                    
         B     BTL210               GO BACK & DO THIS BINSRCH RECD              
         DROP  R4                                                               
                                                                                
*                                                                               
BTL250   DS    0H                                                               
         LA    R0,MYFLDD                                                        
         SR    R2,R0               R2 = L(DATA)                                 
         STC   R2,MYFLDH+5         STICK IT INTO DUMMY TWA HEADER               
         LA    R2,8(R2)                                                         
         STC   R2,MYFLDH+0                                                      
                                                                                
*                                                                               
BTLX     DS    0H                                                               
         B     XIT_01                                                           
                                                                                
                                                                                
* Little helper routine to insert a comma into dummy TWA field.                 
                                                                                
BTLCOMMA DS    0H                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BR    RE                                                               
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--UAE#)'                       
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
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--CDBK#)'                      
*---------------------------- CLEAR DBLOCK ---------------------------*         
                                                                                
* Clears the area pointed to by ADBLOCK.                                        
                                                                                
CLRDBLK  DS    0H                                                               
         L     R0,ADBLOCK                                                       
         LA    R1,DBLOCK1X-DBLOCK1                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         B     XIT_01                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--CDUPT#)'                     
*------------------- CHECK DUPLICATION WITH TRACKS -------------------*         
                                                                                
* Checks to see if any tracks were requested more than once.  If there          
*  were, then it's an error.                                                    
* At exit,                                                                      
*   CC equal  ==> there are no duplication                                      
*   CC nequal ==> a track duplicated                                            
*   OURERRCD   =  error code when CC not equal                                  
*   MYTEXT     =  additional text to display w/ error msg                       
                                                                                
CHKDUPTK DS    0H                                                               
         MVI   OURERRCD,0                                                       
         XC    MYTEXT(MYTEXTL),MYTEXT                                           
                                                                                
*                                                                               
         DS    0H                                                               
         LA    RE,IKTKLST          RE-->LIST OF SOURCE,BOOKS                    
         LA    R2,IKBTLST          R2-->LIST OF BOOKTYPES                       
         ZIC   R1,NTKLNTRY                                                      
         BCTR  R1,0                                                             
         LTR   R1,R1               IF THERE WERE ONLY ONE TRACK,                
         BZ    CDUPTXY              THEN THERE IS NO DUPLICATION                
                                                                                
*                                                                               
CDUPT020 DS    0H                                                               
         LA    RF,L'IKTKLST(RE)                                                 
         LA    R3,L'IKBTLST(R2)                                                 
         LR    R0,R1                                                            
*                                                                               
CDUPT022 DS    0H                                                               
         CLC   0(L'IKTKLST,RE),0(RF)  CHECK SOURCE,BOOK                         
         BNE   *+10                                                             
         CLC   0(L'IKBTLST,R2),0(R3)   AND BOOKTYPE                             
         BNE   *+8                                                              
         B     CDUPT050               THIS TRACK IS A DUPLICATE                 
         LA    RF,L'IKTKLST(RF)                                                 
         LA    R3,L'IKBTLST(R3)                                                 
         BCT   R0,CDUPT022                                                      
*                                                                               
         LA    RE,L'IKTKLST(RE)    CHECK NEXT TRACK AGAINST REST OF LST         
         LA    R2,L'IKBTLST(R2)                                                 
         BCT   R1,CDUPT020                                                      
         B     CDUPTXY                                                          
                                                                                
*                                                                               
** DUPLICATE TRACK ENCOUNTERED **                                               
*                                                                               
CDUPT050 DS    0H                  RF-->DUPL. SRC,BK  R3-->DUPL. BKTYP          
         MVC   TMPFRBK,0(RF)        HOLD ONTO FROM BITS, BOOK                   
         MVC   TMPBTYP,0(R3)         AND BOOKTYPE                               
*                                                                               
         MVI   OURERRCD,DUPQ        SET ERROR CODE                              
                                                                                
*                                                                               
         DS    0H                  SET ADDITIONAL TEXT W/ ERROR MSG             
         MVI   GOSUBN,GKS#          GET KEY SOURCE                              
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,RKS#           TO GET RTG SVC & QUALIFIER                 
         GOTO1 (RF)                                                             
*                                                                               
         MVC   DUB(3),DCSRCNSI                                                  
         CLI   TMPSRC,C'N'                                                      
         BE    CDUPT056X                                                        
         MVC   DUB(3),DCSRCMFX                                                  
         CLI   TMPSRC,C'M'                                                      
         BE    CDUPT056X                                                        
         MVC   DUB(3),DCSRCSRC                                                  
         CLI   TMPSRC,C'S'                                                      
         BE    CDUPT056X                                                        
         DC    H'0'                                                             
CDUPT056X EQU  *                                                                
                                                                                
*                                                                               
         DS    0H                   ADDITIONAL TEXT IS TRACK "LABEL"            
         LA    R2,MYTEXT+1                                                      
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(3,R2),DUB           RATING SERVICE                             
         LA    R2,4(R2)                                                         
                                                                                
         MVI   GOSUBN,TBK#           TRANSLATE BOOK                             
         GOTO1 AGOSUB                                                           
         LH    R1,HALF                                                          
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK          AND MOVE IT IN                            
         LA    R2,1(R1,R2)                                                      
                                                                                
         MVI   0(R2),C'"'                                                       
         LA    R2,2(R2)                                                         
                                                                                
         MVC   0(5,R2),=C'track'                                                
         LA    R2,5(R2)                                                         
                                                                                
         LA    R0,MYTEXT+1                                                      
         SR    R2,R0                                                            
         STC   R2,MYTEXT                                                        
                                                                                
*                                                                               
         B     CDUPTXN                                                          
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
CDUPTXY  DS    0H                                                               
         B     YES_01                                                           
*                                                                               
CDUPTXN  DS    0H                                                               
         B     NO_01                                                            
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--CDUPD#)'                     
*-------------------- CHECK DUPLICATION WITH DEMOS -------------------*         
                                                                                
* Checks the demolist for any demos requested more than once.  If there         
*  are, then it's an error.                                                     
* At exit,                                                                      
*   CC equal  ==> there are no duplication                                      
*   CC nequal ==> a demo duplicated                                             
*   OURERRCD   =  error code when CC not equal                                  
*   MYTEXT     =  additional text to display w/ error msg                       
                                                                                
CHKDUPDM DS    0H                                                               
         MVI   OURERRCD,0                                                       
         XC    MYTEXT(MYTEXTL),MYTEXT                                           
                                                                                
*                                                                               
         DS    0H                                                               
         LA    RE,IKDMLST                                                       
         ZIC   R1,NDEMS                                                         
         BCTR  R1,0                                                             
         LTR   R1,R1               IF THERE WERE ONLY ONE DEMO,                 
         BZ    CDUPDXY              THEN THERE IS NO DUPLICATION                
                                                                                
*                                                                               
CDUPD020 DS    0H                                                               
         LA    RF,L'IKDMLST(RE)                                                 
         LR    R0,R1                                                            
*                                                                               
         CLC   0(L'IKDMLST,RE),0(RF)                                            
         BE    CDUPD050                                                         
         LA    RF,L'IKDMLST(RF)                                                 
         BCT   R0,*-14                                                          
*                                                                               
         LA    RE,L'IKDMLST(RE)    CHECK NEXT DEMO AGAINST REST OF LST          
         BCT   R1,CDUPD020                                                      
         B     CDUPDXY                                                          
                                                                                
*                                                                               
** DUPLICATE DEMO ENCOUNTERED **                                                
*                                                                               
CDUPD050 DS    0H                  RF-->DUPLICATED DEMO                         
         MVC   DUB(L'IKDMLST),0(RF) HOLD ONTO DEMO ENTRY                        
         MVI   DUB+L'IKDMLST,XFF     DELIMITER (JUST IN CASE)                   
*                                                                               
         MVI   OURERRCD,DUPQ        SET ERROR CODE                              
                                                                                
*                                                                               
         DS    0H                  SET ADDITIONAL TEXT W/ ERROR MSG             
         MVI   GOSUBN,CDBK#         CLEAR DBLOCK                                
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                   CALL DEMOCON TO TRANSLATE DEMO              
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         MVC   DBFILE,DCFILTP                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELAGY,TWAAGY                                                  
         GOTO1 DEMOCON,DMCB,(1,DUB),(6,WORK),(C'S',DBLOCKD),0                   
         DROP  R5                                                               
*                                                                               
         DS    0H                   ADDITIONAL TEXT IS DEMO CATEGORY            
         MVI   (MYTEXT+1)+00,C'"'                                               
         MVC   (MYTEXT+1)+01(6),WORK                                            
         MVI   (MYTEXT+1)+07,C'"'                                               
         MVC   (MYTEXT+1)+09(4),=C'demo'                                        
         MVI   MYTEXT,13                                                        
                                                                                
*                                                                               
         B     CDUPDXN                                                          
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
CDUPDXY  DS    0H                                                               
         B     YES_01                                                           
*                                                                               
CDUPDXN  DS    0H                                                               
         B     NO_01                                                            
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--LTORG && CONSTANTS)+0        
               '                                                                
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR01--MISC STUFF)'                 
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
         DS    0H                                                               
GETEL1   DS    0H                  "GETEL1  R3,DATADISP,ELCODE"                 
         PRINT OFF                                                              
         AH    R3,DATADISP                                                      
         PRINT ON                                                               
FIRSTEL1 DS    0H                                                               
         PRINT OFF                                                              
         CLI   0(R3),0                                                          
         BNE   *+10                                                             
         CLI   0(R3),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R3)                                                     
         BCR   8,RE                                                             
         PRINT ON                                                               
NEXTEL1  DS    0H                                                               
         PRINT OFF                                                              
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
         B     FIRSTEL1                                                         
         PRINT ON                                                               
                                                                                
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02)'                             
***********************************************************************         
*======================== SUBROUTINE POOL TWO ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
SUBR02Q  EQU   (((*-RMP41+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP41+SUBR02Q                                                    
SUBR02   NMOD1 0,**4102**                                                       
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
                                                                                
GKS#     EQU   ((R02_01-*)/4+1)+R01#  GET KEY SOURCE                            
RKS#     EQU   ((R02_02-*)/4+1)+R01#  REVERSE KEY SOURCE                        
BDB#     EQU   ((R02_03-*)/4+1)+R01#  BUILD DBLOCK                              
SDBX#    EQU   ((R02_04-*)/4+1)+R01#  SET UP A LINK IN DBEXTEND AREA            
CPRTF#   EQU   ((R02_05-*)/4+1)+R01#  CHANGE PROTECTION OF FIELD                
VCOL#    EQU   ((R02_06-*)/4+1)+R01#  VALIDATE COLUMN                           
CCOL#    EQU   ((R02_07-*)/4+1)+R01#  CLEAR    COLUMN                           
DCOL#    EQU   ((R02_08-*)/4+1)+R01#  DISPLAY  COLUMN                           
BTR#     EQU   ((R02_09-*)/4+1)+R01#  BUILD TRACK RECORD                        
GDV#     EQU   ((R02_10-*)/4+1)+R01#  GET DEMO VALUES                           
XDV#     EQU   ((R02_11-*)/4+1)+R01#  EXTRACT DEMO VALUES                       
MNI#     EQU   ((R02_12-*)/4+1)+R01#  MODIFY FOR NEXT INPUT                     
                                                                                
R02_00   DS    0H                                                               
R02_01   B     GETKSRC                GET KEY SOURCE                            
R02_02   B     REVKSRC                REVERSE KEY SOURCE                        
R02_03   B     BLDBLOCK               BUILD DBLOCK                              
R02_04   B     STDBXLNK               SET UP A LINK IN DBEXTEND AREA            
R02_05   B     CPRTFLD                CHANGE PROTECTION OF FIELD                
R02_06   B     VALCOLMN               VALIDATE COLUMN                           
R02_07   B     CLEARCOL               CLEAR    COLUMN                           
R02_08   B     DSPLYCOL               DISPLAY  COLUMN                           
R02_09   B     BLDTRKRC               BUILD TRACK RECORD                        
R02_10   B     GTDEMVAL               GET DEMO VALUES                           
R02_11   B     XTRCTDMV               EXTRACT DEMO VALUES                       
R02_12   B     MODNXTIN               MODIFY FOR NEXT INPUT                     
R02#     EQU   ((*-R02_00)/4+1)+R01#                                            
         DC    H'0'                                                             
                                                                                
YES_02   SR    RC,RC                                                            
NO_02    LTR   RC,RC                                                            
XIT_02   XIT1                                                                   
         TITLE 'RERMP18 - INVENTORY DEMOS (SUBR02--GKS#)'                       
*--------------------------- GET KEY SOURCE --------------------------*         
                                                                                
* Gets "key source" given the book bits and a rating service.                   
* At entry,                                                                     
*   TMPFRBTS = book bits,                                                       
*   TMPBTYP  = book type.                                                       
* At exit,                                                                      
*   TMPKSRC  = "key source" and CC set to equal if found,                       
*   CC set to not equal if not found.                                           
                                                                                
GETKSRC  DS    0H                                                               
         MVI   TMPKSRC,0                                                        
                                                                                
         XC    GKSIPARM,GKSIPARM                                                
         XC    GKSOPARM,GKSOPARM                                                
                                                                                
         LA    R1,GKSIPARM                                                      
         USING GKSPARMD,R1                                                      
         MVC   GKSPBKBT,TMPFRBTS                                                
         MVC   GKSPBTYP,TMPBTYP                                                 
         DROP  R1                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GKSIPARM),GKSOPARM                           
         CLI   DMCB+4,0                                                         
         BNE   GKSXN                                                            
                                                                                
         MVC   TMPKSRC,GKSOPARM+(GKSPKSRC-GKSPARMD)                             
                                                                                
         B     GKSXY                                                            
                                                                                
                                                                                
GKSXN    DS    0H                                                               
         B     NO_02                                                            
                                                                                
GKSXY    DS    0H                                                               
         B     YES_02                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--RKS#)'                       
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
         DS    0H                  GET QUALIFIER KEY CODE                       
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
         B     NO_02                                                            
                                                                                
RKSXY    DS    0H                                                               
         B     YES_02                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--BDB#)'                       
*---------------------------- BUILD DBLOCK ---------------------------*         
                                                                                
* Builds the demo block (DBLOCK) and extended DBLOCK areas for the              
*  DEMAND calls made in this program.  AIO2 is used for DEMAND i/o,             
*  and the media is always TV (for now).                                        
* At entry,                                                                     
*   TMPS values are set.                                                        
                                                                                
BLDBLOCK DS    0H                                                               
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
                                                                                
*                                                                               
** CLEAR DBLOCK AND EXTENSION AREAS **                                          
*                                                                               
         MVI   GOSUBN,CDBK#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         L     R0,ADBXTND                                                       
         LA    R1,DBXTND1X-DBXTND1                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
** SET UP DBLOCK **                                                             
*                                                                               
         MVC   DBFILE,DCFILIUN                                                  
         MVC   DBAREC,AIO2                                                      
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELBK,TMPBOOK      TRACK                                       
         MVI   DBSELMED,C'U'        MEDIA                                       
         MVC   DBSELSTA,TMPSTTN     STATION                                     
         MVC   DBSELAGY,TWAAGY      REP                                         
         MVC   DBSELINV,TMPINVN     INVENTORY NUMBER                            
         MVC   DBSELDAT,TMPCDATE    EFFECTIVE DATE                              
                                                                                
*                                                                               
         MVI   GOSUBN,GKS#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DBSTYPE,TMPKSRC      KEY SOURCE                                  
                                                                                
*                                                                               
         MVC   DUB(4),=C'RINV'      SET LINK IN DBEXTEND AREA                   
         MVC   DUB+4(4),ADBXTRIN                                                
         MVI   GOSUBN,SDBX#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         ICM   RF,15,FULL           FULL=A(LINK) ON RETURN                      
         BNZ   *+6                                                              
         DC    H'0'                 CAN'T DO MUCH W/O LINK                      
         USING DBXINVWK,RF                                                      
         MVC   DBXIWID,=C'RINV'                                                 
         DROP  RF                                                               
         DROP  R5                                                               
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
         DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--SDBX#)'                      
*------------------------- SET DBEXTEND AREA -------------------------*         
                                                                                
* Sets up a link in the DBEXTEND area.                                          
* At entry,                                                                     
*   DUB(4)   = link identification if want to match to an existing link         
*            = x'00000000' if link must be added,                               
*   DUB+4(4) = A(link),                                                         
*   R5       = A(DBLOCK).                                                       
* At exit,                                                                      
*   FULL     = address of link, zeroes if no link set up.                       
                                                                                
STDBXLNK DS    0H                                                               
         XC    FULL,FULL                                                        
*                                                                               
         DS    0H                  GET R2 TO START OF EXTENSION AREA            
         USING DBLOCKD,R5                                                       
         ICM   R2,15,DBEXTEND                                                   
         BNZ   SDBX020                                                          
                                                                                
         L     R2,DUB+4            USE A(CALLER'S LINK) IF DBEXTEND=0           
         STCM  R2,15,DBEXTEND                                                   
         B     SDBX040              AND MAKE IT THE LAST LINK                   
         DROP  R5                                                               
                                                                                
*                                                                               
SDBX020  DS    0H                  BUMP TO APPROPRIATE LINK                     
         ICM   R0,15,4(R2)          GET ADDRESS OF NEXT LINK                    
         BZ    SDBX030               IF ZERO, ADD CALLER'S LINK                 
                                                                                
         LR    R2,R0                "BUMPED" TO NEXT LINK                       
         OC    DUB(4),DUB           IF LOOKING FOR MATCH,                       
         BZ    SDBX020                                                          
         CLC   DUB(4),0(R2)          AND LINKS' IDS MATCH,                      
         BNE   SDBX020                                                          
         B     SDBX050               PASS BACK ADDR OF CURRENT LINK             
                                                                                
*                                                                               
SDBX030  DS    0H                  ADD CALLER'S LINK TO END                     
         MVC   4(4,R2),DUB+4        SET THE NEXT ADDR INTO LAST LINK            
         ICM   R2,15,4(R2)          BUMP TO THE NEW LAST LINK                   
         B     SDBX040                                                          
                                                                                
*                                                                               
SDBX040  DS    0H                  R2-->LAST LINK                               
         XC    4(4,R2),4(R2)        ZERO OUT THE NEXT ADDRESS                   
         B     SDBX050               NOPE                                       
                                                                                
*                                                                               
SDBX050  DS    0H                                                               
         ST    R2,FULL             RETURN ADDRESS OF MATCHED/ADDED LINK         
         B     SDBXX                                                            
                                                                                
*                                                                               
SDBXX    DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--CPRTF#)'                     
*----------------------- CHANGE FIELD PROTECTION ---------------------*         
                                                                                
*  Changes the protection of those non-key fields where data is                 
*   enterrable.                                                                 
                                                                                
CPRTFLD  DS    0H                                                               
                                                                                
         SR    R0,R0               WANT R0=ZERO IN THIS WHOLE ROUTINE           
         XC    CNTTRK,CNTTRK                                                    
*                                                                               
         L     R4,AMDPLYTB                                                      
         USING MDPLYTBD,R4                                                      
                                                                                
*                                                                               
** LOOP BY COLUMN **                                                            
*                                                                               
CPRT010  DS    0H                                                               
         LA    R1,1                                                             
         AH    R1,CNTTRK                                                        
         CLM   R1,1,=AL1(MXTKLNTY)                                              
         BH    CPRTX                                                            
         STH   R1,CNTTRK           CNTTRK = COLUMN NUMBER                       
                                                                                
*                                                                               
         TM    MDTFLAG,MDTFCPRT    CHANGE PROTECTION FOR COLUMN?                
         BZ    CPRT099              NOPE                                        
         NI    MDTFLAG,XFF-MDTFCPRT                                             
                                                                                
*                                                                               
         DS    0H                 SET PROTECTION FOR COLUMN                     
         MVI   FLDPRCTN,C'P'       ASSUME ENTIRE COLUMN S/B PROTECTED           
*                                                                               
         LH    R1,CNTTRK                                                        
         CLM   R1,1,NTKLNTRY        NO TRACK INPUTTED FOR THIS COLUMN           
         BH    CPRT019               ==>PROTECT ENTIRE COLUMN                   
*                                                                               
         TM    MDTFLAG,MDTFNOVR     PROFILE DISALLOWS DEMO OVERRIDE             
         BO    CPRT019               ==>PROTECT ENTIRE COLUMN                   
*                                                                               
         MVI   FLDPRCTN,C'U'       ELSE, UNPROTECT COLUMN                       
CPRT019  EQU   *                                                                
                                                                                
*                                                                               
*** CODE/TEXT FIELDS ***                                                        
*                                                                               
         DS    0H                                                               
         TM    MDTFLAG,MDTFNEW                                                  
         BZ    CPRT029                                                          
                                                                                
*                                                                               
         DS    0H                  SET UP  R6  FOR  EX  INSTRUCTION             
         LA    R6,PRTFLD                                                        
         CLI   FLDPRCTN,C'P'                                                    
         BE    CPRT022X                                                         
         LA    R6,UPRTFLD                                                       
         CLI   FLDPRCTN,C'U'                                                    
         BE    CPRT022X                                                         
         DC    H'0'                                                             
CPRT022X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         LH    R2,CNTTRK                                                        
         BCTR  R2,0                                                             
         MH    R2,=Y(CTCELLQ)                                                   
         LA    R2,IDMRCODH(R2)      R2-->APPROPRIATE COLUMN FOR TRACK           
         USING CTCELLD,R2                                                       
*                                                                               
         SR    R0,R0                                                            
         LA    R3,CTCCODH           R3-->HEADER FOR CODE FIELD                  
         EX    R0,0(R6)                                                         
         OI    6(R3),X80                                                        
                                                                                
         LA    R3,CTCTXTH           R3-->HEADER FOR TEXT FIELD                  
         EX    R0,0(R6)                                                         
         OI    6(R3),X80                                                        
*                                                                               
         DROP  R2                                                               
CPRT029  EQU   *                                                                
                                                                                
*                                                                               
*** DEMO VALUE DISPLAY FIELDS ***                                               
*                                                                               
         XC    CNTDEM,CNTDEM                                                    
         LA    R2,IDMROWHH                                                      
         USING ROWDSECT,R2                                                      
                                                                                
*                                                                               
CPRT052  DS    0H                                                               
         CLI   FLDPRCTN,C'U'                                                    
         BE    CPRT054                                                          
         CLI   FLDPRCTN,C'P'                                                    
         BE    CPRT056                                                          
         DC    H'0'                                                             
*                                                                               
CPRT054  DS    0H                  PREPARE TO UNPROTECT FIELDS                  
         MVC   NTIMES,NDEMS                                                     
         LA    R6,UPRTFLD                                                       
         B     CPRT059                                                          
*                                                                               
CPRT056  DS    0H                  PREPARE TO PROTECT FIELDS                    
         MVC   NTIMES,=AL1(MXDEMOS)                                             
         LA    R6,PRTFLD                                                        
         B     CPRT059                                                          
*                                                                               
CPRT059  EQU   *                                                                
                                                                                
*                                                                               
CPRT082  DS    0H                  LOOP TO PROCESS CELLS IN A COLUMN            
         LA    R1,1                                                             
         AH    R1,CNTDEM                                                        
         CLM   R1,1,NTIMES                                                      
         BH    CPRT089                                                          
         STH   R1,CNTDEM                                                        
*                                                                               
         DS    0H                   CHECK IF CELL NEEDS PROTECTION              
         LH    RE,CNTDEM                                                        
         BCTR  RE,0                                                             
         MH    RE,=Y(DVCELLQ)                                                   
         LA    RE,MDTDVCLL(RE)      RE-->DEMO VALUE CELL                        
         USING DVCELLD,RE                                                       
         TM    DVCFLAG,DVCFCPRT     CHANGE PROTECTION OF FIELD?                 
         BZ    CPRT088               NO                                         
         NI    DVCFLAG,XFF-DVCFCPRT                                             
*                                                                               
         DS    0H                  SET R3 POINTING TO DEMO DISPLAY CELL         
         LH    RF,CNTTRK                                                        
         BCTR  RF,0                                                             
         MH    RF,=Y(DDCELLQ)                                                   
         LA    RF,ROWDEMOS(RF)                                                  
         LA    R3,(DDCOVRH-DDCELLD)(RF)  R3-->OVERRIDE-ABLE DSPLAY CELL         
                                                                                
         DS    0H                   SET  RF  FOR  EX  INSTRUCTION               
         LR    RF,R6                                                            
         CLI   FLDPRCTN,C'U'         IF IN UN-PROTECT MODE,                     
         BNE   CPRT086P                                                         
         TM    DVCFLAG,DVCFNOVR       BUT CAN'T OVRD DEMO,                      
         BZ    *+8                                                              
         LA    RF,PRTFLD              SET RF FOR PROTECT FIELD                  
CPRT086P EQU   *                                                                
         DROP  RE                                                               
                                                                                
         SR    R0,R0                                                            
         EX    R0,0(RF)             SET FIELD PROTECTION                        
         OI    6(R3),X80             AND TRANSMIT FIELD                         
                                                                                
CPRT088  DS    0H                                                               
         LA    R2,SZDEMROW(R2)      BUMP TO NEXT ROW                            
         B     CPRT082                                                          
CPRT089  EQU   *                                                                
         DROP  R2                                                               
                                                                                
*                                                                               
         CLI   FLDPRCTN,C'P'       IF WE WERE PROTECTING FIELDS,                
         BE    CPRT099              THEN WE'RE DONE W/ COLUMN                   
                                                                                
         CLI   FLDPRCTN,C'U'       IF WE WERE UNPROTECTING FIELDS,              
         BNE   *+12                                                             
         MVI   FLDPRCTN,C'P'        DO PROTECT FIELDS NOW                       
         B     CPRT052                                                          
                                                                                
         DC    H'0'                                                             
CPRT099  EQU   *                                                                
                                                                                
*                                                                               
** DONE PROCESSING COLUMN **                                                    
*                                                                               
         DS    0H                                                               
         LA    R4,MDPLYTBQ(R4)                                                  
         B     CPRT010                                                          
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
CPRTX    DS    0H                                                               
         B     XIT_02                                                           
                                                                                
                                                                                
PRTFLD   OI    1(R3),X20           SHOULD NOT FALL THROUGH AND                  
UPRTFLD  NI    1(R3),XFF-X20        EXECUTE THESE INSTRUCTIONS                  
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--VCOL#)'                      
*-------------------------- VALIDATE COLUMN --------------------------*         
                                                                                
* At entry,                                                                     
*   CNTTRK is set                                                               
*   R4 --> Master Display Table entry for track                                 
                                                                                
VALCOLMN DS    0H                                                               
         USING MDPLYTBD,R4                                                      
*                                                                               
         MVI   OURERRCD,0          ASSUME NO ERRORS                             
                                                                                
*                                                                               
** VALIDATE CODE/TEXT **                                                        
*                                                                               
         LH    R3,CNTTRK                                                        
         MH    R3,=Y(CTCELLQ)                                                   
         LA    R3,IDMRCODH(R3)                                                  
         USING CTCELLD,R3                                                       
                                                                                
*                                                                               
*** CODE ***                                                                    
*                                                                               
         LA    R2,CTCCODH                                                       
         TM    4(R2),X80           WAS CODE FIELD INPUTTED THIS TIME?           
         BZ    VCOLCT49             NOPE                                        
                                                                                
*                                                                               
         MVC   DUB(L'MDTCODE),SPACES                                            
         CLI   5(R2),1                                                          
         BL    VCOLCT40             NO INPUT MEANS CODE IS BLANK                
         BE    VCOLIFLD             ONE CHARACTER INPUT IS INVALID              
                                                                                
*                                                                               
         DS    0H                  VALIDATE CODE AGAINST TABLE                  
         L     RF,ACODETAB                                                      
                                                                                
VCOLCT22 DS    0H                                                               
         CLI   0(RF),EOT                                                        
         BE    VCOLCT30                                                         
         CLC   CTCCOD,0(RF)                                                     
         BE    *+12                                                             
         LA    RF,L'CODETAB(RF)                                                 
         B     VCOLCT22                                                         
                                                                                
         MVC   DUB(L'MDTCODE),CTCCOD                                            
         B     VCOLCT40                                                         
*                                                                               
VCOLCT30 DS    0H                  VALIDATE CODE AS MONTH/YEAR                  
         LA    R0,MONTABL                                                       
         LA    RF,MONTAB                                                        
                                                                                
VCOLCT32 DS    0H                   LOOK UP MONTH                               
         CLC   CTCCOD(1),0(RF)                                                  
         BE    VCOLCT35                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VCOLCT32                                                      
         B     VCOLIFLD                                                         
                                                                                
VCOLCT35 DS    0H                   CHECK YEAR                                  
         CLI   CTCCOD+1,C'0'                                                    
         BL    VCOLIFLD                                                         
         CLI   CTCCOD+1,C'9'                                                    
         BH    VCOLIFLD                                                         
                                                                                
         MVC   DUB(L'MDTCODE),CTCCOD                                            
         B     VCOLCT40                                                         
                                                                                
*                                                                               
VCOLCT40 DS    0H                  CODE IS VALID                                
         MVC   MDTCODE,DUB          UPDATE ENTRY IN MASTER DISPLAY TBL          
*                                                                               
         OI    MDTFLAG,MDTFCHNG     FLAG THIS TRACK WAS CHANGED                 
                                                                                
*                                                                               
VCOLCT49 EQU   *                                                                
                                                                                
*                                                                               
*** TEXT ***                                                                    
*                                                                               
         LA    R2,CTCTXTH                                                       
         TM    4(R2),X80           WAS CODE FIELD INPUTTED THIS TIME?           
         BZ    VCOLCT99             NOPE                                        
                                                                                
*                                                                               
         DS    0H                  VALIDATE TEXT (NUMBER) INPUT                 
         XC    DUB(L'MDTTEXT),DUB                                               
         CLI   5(R2),0              SEE IF ANY INPUT                            
         BE    VCOLCT80              NO INPUT IS OKAY                           
                                                                                
         TM    4(R2),X08            CHECK IF ALL NUMERIC                        
         BZ    VCOLIFLD                                                         
                                                                                
*                                                                               
         DS    0H                  CONVERT INPUT TO BINARY                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  MYDUB,CTCTXT(0)                                                  
         CVB   R0,MYDUB                                                         
         STCM  R0,3,DUB                                                         
                                                                                
*                                                                               
VCOLCT80 DS    0H                  TEXT (NUMBER) IS VALID                       
         MVC   MDTTEXT,DUB          UPDATE ENTRY IN MASTER DISPLAY TBL          
*                                                                               
         OI    MDTFLAG,MDTFCHNG     FLAG THIS TRACK WAS CHANGED                 
                                                                                
*                                                                               
VCOLCT99 EQU   *                                                                
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
** VALIDATE DEMO VALUES **                                                      
*                                                                               
         XC    CNTDEM,CNTDEM                                                    
         LA    R3,IDMROWHH                                                      
         USING ROWDSECT,R3                                                      
         LA    R5,MDTDVCLL                                                      
         USING DVCELLD,R5                                                       
                                                                                
*                                                                               
*** LOCATE APPROPRIATE DEMO DISPLAY CELL ***                                    
*                                                                               
VCOLDM22 DS    0H                                                               
         LH    RF,CNTTRK                                                        
         MH    RF,=Y(DDCELLQ)                                                   
         LA    RF,ROWDEMOS(RF)     RF-->DEMO DISPLAY FIELD                      
         USING DDCELLD,RF                                                       
         LA    R2,DDCOVRH          R2-->FIELD TO ENTER OVERRIDE                 
         DROP  RF                                                               
                                                                                
*                                                                               
*** VALIDATE INPUT ***                                                          
*                                                                               
         TM    4(R2),X80           WAS FIELD INPUTTED THIS TIME?                
         BZ    VCOLDM82             NOPE                                        
                                                                                
*                                                                               
         DS    0H                                                               
         ZICM  R0,5(R2),(1)        IF NO INPUT,                                 
         BNZ   VCOLDM44                                                         
         TM    DVCFLAG,DVCFDOVD     WAS DEMO OVERRIDDEN?                        
         BZ    *+18                                                             
         NI    DVCFLAG,XFF-DVCFDOVD  YES--TURN OFF OVERRIDE FLAG,               
         MVC   DVCOVRDM,DVCDEMVL      MOVE ORIG VALUE TO OVERRIDE VALUE         
         OI    MDTFLAG,MDTFCHNG       FLAG THIS TRACK WAS CHANGED               
                                                                                
         B     VCOLDM78                                                         
                                                                                
*                                                                               
VCOLDM44 DS    0H                  THERE IS INPUT (R0=L'INPUT)                  
         GOTO1 ANY                  GET THE INPUT INTO WORK                     
*                                                                               
         DS    0H                   REMOVE ANY LEFT-PADDED BLANKS               
         LR    R1,R0                                                            
         LA    RE,WORK                                                          
         LR    RF,RE                                                            
                                                                                
         CLI   0(RF),C' '            BUMP TO FIRST REAL CHARACTER               
         BH    *+14                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
                                                                                
         LR    R0,RF                 R0 = RF-->1ST REAL CHARACTER               
         SR    R0,RE                 R0 = DISPLACEMENT TO REAL CHARS            
         SR    R1,R0                 R1 = ACTUAL L(INPUT)                       
                                                                                
         STC   R1,MYFLDH+5          L(INPUT) W/O LEADING BLANKS                 
         BCTR  R1,0                                                             
         EXMVC R1,MYFLDD,0(RF)      MYFLDD=INPUT (FLUSHED TO THE LEFT)          
*                                                                               
         DS    0H                  VALIDATE INPUT                               
         ZIC   R0,MYFLDH+5                                                      
         GOTO1 CASHVAL,DMCB,(1,MYFLDD),(R0),0                                   
         CLI   DMCB+0,0                                                         
         BNE   VCOLIFLD                                                         
*                                                                               
         DS    0H                  UPDATE MASTER DISPLAY TABLE ENTRY            
         OI    DVCFLAG,DVCFDOVD     MAKE SURE OVERRIDE FLAG IS ON,              
         MVC   DVCOVRDM,DMCB+4      AND MOVE IN OVERRIDE VALUE                  
         OI    MDTFLAG,MDTFCHNG     FLAG THIS TRACK WAS CHANGED                 
         B     VCOLDM78                                                         
                                                                                
*                                                                               
VCOLDM78 DS    0H                  INPUT VALIDATED                              
         B     VCOLDM82                                                         
                                                                                
*                                                                               
*** BUMP TO NEXT DEMO ***                                                       
*                                                                               
VCOLDM82 DS    0H                                                               
         LA    R0,1                                                             
         AH    R0,CNTDEM                                                        
         CLM   R0,1,NDEMS                                                       
         BNL   VCOLDM99                                                         
                                                                                
         STH   R0,CNTDEM                                                        
         LA    R3,ROWL(R3)         BUMP TO NEXT ROW ON SCREEN                   
         LA    R5,DVCELLQ(R5)      BUMP TO NEXT DEMO VALUE CELL                 
         B     VCOLDM22                                                         
                                                                                
*                                                                               
VCOLDM99 EQU   *                                                                
         DROP  R3,R5                                                            
                                                                                
         EJECT                                                                  
*                                                                               
** UPDATE MASTER DISPLAY TABLE ENTRY **                                         
*                                                                               
         DS    0H                                                               
         TM    MDTFLAG,MDTFCHNG    DID USER INPUT ANY CHANGE?                   
         BZ    VCOL399              NO                                          
                                                                                
*                                                                               
*** RE-SEED DEMO VALUES TO EFFECTUATE RIPPLING ***                              
*                                                                               
         DS    0H                  RE-BUILD TRACK RECORD                        
         MVI   GOSUBN,BTR#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         DS    0H                  COPY IT INTO I/O AREA #2                     
         L     R0,AIO2                                                          
         LA    R1,L'IO                                                          
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
         DS    0H                  GET DEMO VALUES FROM NEW RECORD              
         MVC   AINVTRK,AIO2                                                     
         MVI   GOSUBN,GDV#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         DS    0H                  SEED DEMO VALUES & FLAGS INTO ENTRY          
         XC    CNTDEM,CNTDEM                                                    
*                                                                               
VCOL252  DS    0H                                                               
         LH    R2,CNTDEM                                                        
         MH    R2,=Y(DVCELLQ)                                                   
         LA    R2,MDTDVCLL(R2)                                                  
         USING DVCELLD,R2                                                       
*                                                                               
         DS    0H                  MOVE DEMO VALUES INTO CELL                   
         LH    RE,CNTDEM                                                        
         SLL   RE,2                 (MULTIPLY BY 4)                             
         LR    RF,RE                                                            
         LA    RE,DMVL_OVR(RE)      RE-->OVERRIDE DEMO VALUE                    
         LA    RF,DMVL_NOV(RF)      RF-->NON-OVERRIDE DEMO VALUE                
         MVC   DVCOVRDM,0(RE)                                                   
         MVC   DVCDEMVL,0(RF)                                                   
         OI    DVCFLAG,DVCFDPLY     FLAG TO RE-DISPLAY DEMO VALUES              
*                                                                               
         DS    0H                  CHECK FOR OVERRIDES                          
         NI    DVCFLAG,XFF-DVCFDOVD ASSUME NOT OVERRIDDEN                       
         LH    RE,CNTDEM            RE = NTH DEMO BEING PROCESSED               
         MH    RE,=Y(L'IKDMLST)                                                 
         LA    RE,IKDMLST(RE)       RE-->DEMOLIST ENTRY                         
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING RINVNOEL,RF                                                      
         MVI   RINVNOEL,X'DE'                                                   
         MVI   RINVNOZR,0                                                       
         MVI   RINVNOCT,0                                                       
         MVC   RINVNOTP,1(RE)       MOVE IN DEMO MODIFIER                       
         MVC   RINVNODM,2(RE)       MOVE IN DEMO CATEGORY NUMBER                
         DROP  RF                                                               
                                                                                
         LA    R0,(RINVNODM+L'RINVNODM-RINVNOEL)-2                              
         GOTO1 HELLO,DMCB,(C'G',DCREPFIL),(ELEM,AIO1),((R0),ELEM+2),0           
         CLI   DMCB+12,0            IF DEMO OVERRIDE ELEM FOUND,                
         BNE   *+8                                                              
         OI    DVCFLAG,DVCFDOVD      FLAG DEMO VALUE OVERRIDDEN                 
*                                                                               
         DS    0H                  CHECK FOR MINIMUM VALUE OVERRIDES            
         NI    DVCFLAG,XFF-DVCFDMIN ASSUME NO MINIMUM VALUE                     
         LH    RE,CNTDEM            RE = NTH DEMO BEING PROCESSED               
         MH    RE,=Y(L'IKDMLST)                                                 
         LA    RE,IKDMLST(RE)       RE-->DEMOLIST ENTRY                         
         MVC   MYDEMDFY,1(RE)                                                   
                                                                                
         L     R3,AIO1                                                          
         MVI   ELCODE,X'05'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL2                                                        
         B     VCOL273B                                                         
                                                                                
VCOL273  BAS   RE,NEXTEL2                                                       
VCOL273B BNE   VCOL273X                                                         
         USING RAVLNEL,R3                                                       
         CLI   RAVLNTYP,X'07'                                                   
         BNE   VCOL273                                                          
         CLC   RAVLNCAT,MYDEMDFY                                                
         BNE   VCOL273                                                          
         OI    DVCFLAG,DVCFDMIN      FLAG DEMO VALUE HAS MINIMUM OVRD           
         DROP  R3                                                               
VCOL273X EQU   *                                                                
         DROP  R2                                                               
*                                                                               
         DS    0H                   PROCESS NEXT DEMO                           
         LA    R1,1                                                             
         AH    R1,CNTDEM                                                        
         CLM   R1,1,NDEMS                                                       
         BNL   *+12                                                             
         STH   R1,CNTDEM                                                        
         B     VCOL252                                                          
                                                                                
*                                                                               
VCOL399  EQU   *                                                                
                                                                                
         EJECT                                                                  
*                                                                               
** DONE VALIDATING COLUMN **                                                    
*                                                                               
         B     VCOLXY                                                           
         DROP  R4                                                               
                                                                                
*                                                                               
** ERRORS **                                                                    
*                                                                               
VCOLIFLD DS    0H                                                               
         MVI   OURERRCD,IFLDQ                                                   
         OI    6(R2),X01           FIELD IN ERROR IS MODIFIED FOR NEXT          
         ST    R2,AERRFLD                                                       
         B     VCOLXN                                                           
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
VCOLXN   DS    0H                                                               
         B     NO_02                                                            
*                                                                               
VCOLXY   DS    0H                                                               
         B     YES_02                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--CCOL#)'                      
*---------------------------- CLEAR COLUMN ---------------------------*         
                                                                                
* At entry,                                                                     
*   CNTTRK is set                                                               
*   R4 --> Master Display Table entry for track                                 
                                                                                
CLEARCOL DS    0H                                                               
         USING MDPLYTBD,R4                                                      
                                                                                
*                                                                               
** CLEAR TRACK LABEL **                                                         
*                                                                               
         DS    0H                                                               
         TM    MDTFLAG,MDTFNEW     DIFFERENT TRACK FROM BEFORE?                 
         BZ    CCOLTLX              NO                                          
                                                                                
*                                                                               
         DS    0H                  DISPLACE TO CORRECT COLUMN HEADER            
         SR    R0,R0                                                            
         LA    R2,IDMCOLHH                                                      
         ZICM  R1,CNTTRK,(3)                                                    
         BZ    CCOLTL02                                                         
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,*-6                                                           
CCOLTL02 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  GET DISPLAYABLE SRC,BOOK                     
         XC    8(L'IDMCOLH,R2),8(R2)                                            
         OI    6(R2),X80                                                        
CCOLTLX  EQU   *                                                                
                                                                                
*                                                                               
** CLEAR FOOTNOTE **                                                            
*                                                                               
         DS    0H                                                               
         TM    MDTFLAG,MDTFNEW     DIFFERENT TRACK FROM BEFORE?                 
         BZ    CCOLFNX              NO                                          
                                                                                
*                                                                               
         DS    0H                  DISPLACE TO CORRECT COLUMN HEADER            
         SR    R0,R0                                                            
         LA    R2,IDMFTNTH                                                      
         ZICM  R1,CNTTRK,(3)                                                    
         BZ    CCOLFN02                                                         
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,*-6                                                           
CCOLFN02 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         XC    8(L'IDMFTNT,R2),8(R2)                                            
         OI    6(R2),X80                                                        
CCOLFNX  EQU   *                                                                
                                                                                
*                                                                               
** CLEAR CODE AND TEXT **                                                       
*                                                                               
         DS    0H                                                               
         TM    MDTFLAG,MDTFNEW     DIFFERENT TRACK FROM BEFORE?                 
         BZ    CCOLCTX              NO                                          
                                                                                
*                                                                               
         DS    0H                  DISPLACE TO CORRECT COLUMN HEADER            
         LH    R2,CNTTRK                                                        
         MH    R2,=Y(CTCELLQ)                                                   
         LA    R2,IDMRCODH(R2)                                                  
         USING CTCELLD,R2                                                       
                                                                                
*                                                                               
         DS    0H                  R2-->CORRESPONDING CODE/TEXT CELL            
         XC    CTCCOD,CTCCOD                                                    
         OI    CTCCODH+6,X80                                                    
*                                                                               
         XC    CTCTXT,CTCTXT                                                    
         OI    CTCTXTH+6,X80                                                    
         DROP  R2                                                               
CCOLCTX  EQU   *                                                                
                                                                                
*                                                                               
** CLEAR DEMO VALUES **                                                         
*                                                                               
         DS    0H                                                               
         XC    CNTDEM,CNTDEM                                                    
         LA    R2,IDMROWHH         R2-->FIRST DEMO ROW                          
         USING ROWDSECT,R2                                                      
         LA    R5,MDTDVCLL         R5-->FIRST DEMO VALUE CELL                   
         USING DVCELLD,R5                                                       
                                                                                
*                                                                               
CCOLDV12 DS    0H                  R2-->A DEMO ROW                              
         TM    DVCFLAG,DVCFCCLL     CLEAR THIS DEMO CELL?                       
         BZ    CCOLDV70              NO                                         
         NI    DVCFLAG,XFF-DVCFCCLL                                             
                                                                                
*                                                                               
         LH    R3,CNTTRK                                                        
         MH    R3,=Y(DDCELLQ)                                                   
         LA    R3,ROWDEMOS(R3)      R3-->DEMO COLUMN W/IN ROW                   
         USING DDCELLD,R3                                                       
                                                                                
*                                                                               
         DS    0H                  R4-->TRK ENTRY CORRESPONDING TO COL          
         XC    DDCNOVD,DDCNOVD                                                  
         OI    DDCNOVH+6,X80                                                    
*                                                                               
         XC    DDCOVRD,DDCOVRD                                                  
         OI    DDCOVRH+6,X80                                                    
         DROP  R3                                                               
                                                                                
*                                                                               
CCOLDV70 DS    0H                  FINISHED W/ THIS DEMO                        
         LA    R0,1                                                             
         AH    R0,CNTDEM                                                        
         CLM   R0,1,=AL1(MXDEMOS)                                               
         BNL   CCOLDV99                                                         
                                                                                
         STH   R0,CNTDEM                                                        
         LA    R2,SZDEMROW(R2)     BUMP TO NEXT DEMO ROW                        
         LA    R5,DVCELLQ(R5)      BUMP TO NEXT DEMO VALUE CELL                 
         B     CCOLDV12                                                         
         DROP  R2,R5                                                            
CCOLDV99 EQU   *                                                                
                                                                                
*                                                                               
         B     XIT_02                                                           
         DROP  R4                                                               
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--DCOL#)'                      
*--------------------------- DISPLAY COLUMN --------------------------*         
                                                                                
* At entry,                                                                     
*   CNTTRK is set                                                               
*   R4 --> Master Display Table entry for track                                 
                                                                                
DSPLYCOL DS    0H                                                               
         USING MDPLYTBD,R4                                                      
                                                                                
*                                                                               
         OC    MDTDIIVK,MDTDIIVK                                                
         BZ    DCOLX                                                            
                                                                                
*                                                                               
*** DISPLAY TRACK LABEL ***                                                     
*                                                                               
         DS    0H                                                               
         TM    MDTFLAG,MDTFNEW     DIFFERENT TRACK FROM BEFORE?                 
         BZ    DCOLTLX              NO                                          
                                                                                
*                                                                               
         DS    0H                  DISPLACE TO CORRECT COLUMN HEADER            
         SR    R0,R0                                                            
         LA    R2,IDMCOLHH                                                      
         ZICM  R1,CNTTRK,(3)                                                    
         BZ    DCOLTL02                                                         
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,*-6                                                           
DCOLTL02 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  GET DISPLAYABLE SRC,BOOK                     
         LA    R6,MDTDIIVK                                                      
         USING RINVKEY,R6                                                       
         MVC   TMPKSRC,RINVKSRC                                                 
         MVC   TMPBOOK,RINVKBK                                                  
         DROP  R6                                                               
                                                                                
         CLI   TMPBOOK+1,0          IF TRACK IS EST/YR,                         
         BNE   *+8                                                              
         MVI   TMPBOOK+1,ESTMNTHQ    CODE IN AN EST-MONTH FOR MY USE            
                                                                                
         MVI   GOSUBN,RKS#          REVERSE KEY-SOURCE TO SET                   
         GOTO1 AGOSUB                TMPQLFY AND TMPBTYP                        
                                                                                
         MVI   GOSUBN,TBK#          TRANSLATE BOOK                              
         GOTO1 AGOSUB                                                           
                                                                                
         MVC   8(1,R2),TMPSRC       SOURCE SET FROM RKS# RTN ABOVE              
         MVI   9(R2),C','                                                       
         MVC   10(L'IDMCOLH-2,R2),WORK                                          
         OI    6(R2),X80                                                        
DCOLTLX  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         TM    MDTFLAG,MDTFNOTF    IF TRACK NOT FOUND BY DEMAND,                
         BZ    DCOLNFX                                                          
         OC    AERRFLD,AERRFLD                                                  
         BNZ   *+12                                                             
         ST    R2,AERRFLD                                                       
         MVI   OURERRCD,NDDFQ       SET ERROR MESSAGE,                          
         B     DCOLX                AND DON'T DISPLAY REST OF STUFF             
DCOLNFX  EQU   *                                                                
                                                                                
*                                                                               
*** DISPLAY FOOTNOTE ***                                                        
*                                                                               
         DS    0H                                                               
         TM    MDTFLAG,MDTFNEW     DIFFERENT TRACK FROM BEFORE?                 
         BZ    DCOLFNX              NO                                          
                                                                                
*                                                                               
         DS    0H                  DISPLACE TO CORRECT COLUMN HEADER            
         SR    R0,R0                                                            
         LA    R2,IDMFTNTH                                                      
         ZICM  R1,CNTTRK,(3)                                                    
         BZ    DCOLFN02                                                         
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,*-6                                                           
DCOLFN02 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   8(L'IDMFTNT,R2),MDTFTNT                                          
         OI    6(R2),X80                                                        
DCOLFNX  EQU   *                                                                
                                                                                
*                                                                               
*** DISPLAY CODE AND TEXT ***                                                   
*                                                                               
         DS    0H                  DISPLACE TO CORRECT COLUMN HEADER            
         LH    R2,CNTTRK                                                        
         MH    R2,=Y(CTCELLQ)                                                   
         LA    R2,IDMRCODH(R2)                                                  
         USING CTCELLD,R2                                                       
                                                                                
*                                                                               
         DS    0H                  R2-->CORRESPONDING CODE/TEXT CELL            
         TM    MDTFLAG,MDTFNEW      DIFFERENT TRACK FROM BEFORE?                
         BNZ   DCOLCT15              YES, DISPLAY CODE                          
         TM    CTCCODH+4,X80        FIELD INPUTTED THIS TIME?                   
         BNZ   DCOLCT15              YES, RE-FORMAT CODE                        
         B     DCOLCT19                                                         
                                                                                
DCOLCT15 DS    0H                                                               
         MVC   CTCCOD,MDTCODE                                                   
         OI    CTCCODH+6,X80                                                    
DCOLCT19 EQU   *                                                                
*                                                                               
         TM    MDTFLAG,MDTFNEW      DIFFERENT TRACK FROM BEFORE?                
         BNZ   DCOLCT25              YES, DISPLAY TEXT                          
         TM    CTCTXTH+4,X80        FIELD INPUTTED THIS TIME?                   
         BNZ   DCOLCT25              YES, RE-FORMAT TEXT                        
         B     DCOLCT29                                                         
                                                                                
DCOLCT25 DS    0H                                                               
         ZICM  R1,MDTTEXT,(3)                                                   
         EDIT  (R1),(L'CTCTXT,CTCTXT),ZERO=BLANK,ALIGN=LEFT                     
         OI    CTCTXTH+6,X80                                                    
DCOLCT29 EQU   *                                                                
         DROP  R2                                                               
                                                                                
*                                                                               
*** DISPLAY DEMO VALUES ***                                                     
*                                                                               
                                                                                
         DS    0H                                                               
         XC    CNTDEM,CNTDEM                                                    
         LA    R2,IDMROWHH         R2-->FIRST DEMO ROW                          
         USING ROWDSECT,R2                                                      
         LA    R5,MDTDVCLL         R5-->FIRST DEMO VALUE CELL                   
         USING DVCELLD,R5                                                       
                                                                                
*                                                                               
DCOLDV12 DS    0H                  R2-->A DEMO ROW                              
         TM    DVCFLAG,DVCFDPLY     DISPLAY THIS DEMO?                          
         BZ    DCOLDV70              NO                                         
         NI    DVCFLAG,XFF-DVCFDPLY                                             
*                                                                               
         LH    R0,CNTDEM                                                        
         CLM   R0,1,NDEMS           IF NOT LOWER THAN # DEMOS INPUTTED,         
         BNL   DCOLDV70              THEN DON'T DISPLAY ANYTHING                
                                                                                
*                                                                               
         LH    R3,CNTTRK                                                        
         MH    R3,=Y(DDCELLQ)                                                   
         LA    R3,ROWDEMOS(R3)      R3-->DEMO COLUMN W/IN ROW                   
         USING DDCELLD,R3                                                       
                                                                                
*                                                                               
         DS    0H                  R4-->TRK ENTRY CORRESPONDING TO COL          
         ICM   R1,15,DVCOVRDM       R1 = "OVERRIDE" DEMO VALUE                  
         TM    DVCFLAG,DVCFDOVD     IF THERE REALLY IS AN OVERRIDE,             
         BZ    *+8                                                              
         ICM   R1,15,DVCDEMVL        USE NON-OVERRIDDEN DEMO VALUE              
         LA    RF,DCOLDVE1                                                      
         TM    DVCFLAG,DVCF1DEC                                                 
         BO    *+8                                                              
         LA    RF,DCOLDVE0                                                      
         BR    RF                                                               
                                                                                
DCOLDVE1 DS    0H                    EDIT W/ 1 DECIMAL PLACE                    
         EDIT  (R1),(L'DDCNOVD,DDCNOVD),1,ZERO=NOBLANK                          
         B     DCOLDV24                                                         
DCOLDVE0 DS    0H                    EDIT W/ NO DECIMAL PLACE                   
         EDIT  (R1),(L'DDCNOVD,DDCNOVD),ZERO=NOBLANK                            
         B     DCOLDV24                                                         
DCOLDV24 EQU   *                                                                
         OI    DDCNOVH+6,X80                                                    
                                                                                
         DS    0H                   CHECK FOR OVERRIDES                         
         TM    DVCFLAG,DVCFDOVD                                                 
         BZ    DCOLDV29                                                         
         ICM   R1,15,DVCOVRDM       R1 = OVERRIDDEN DEMO VALUE                  
         EDIT  (R1),(L'DDCOVRD,DDCOVRD),1,ZERO=NOBLANK                          
         OI    DDCOVRH+6,X80                                                    
DCOLDV29 EQU   *                                                                
         DROP  R3                                                               
                                                                                
*                                                                               
DCOLDV70 DS    0H                  FINISHED W/ THIS DEMO                        
         LA    R0,1                                                             
         AH    R0,CNTDEM                                                        
         CLM   R0,1,=AL1(MXDEMOS)                                               
         BNL   DCOLDV99                                                         
                                                                                
         STH   R0,CNTDEM                                                        
         LA    R2,SZDEMROW(R2)     BUMP TO NEXT DEMO ROW                        
         LA    R5,DVCELLQ(R5)      BUMP TO NEXT DEMO VALUE CELL                 
         B     DCOLDV12                                                         
DCOLDV99 EQU   *                                                                
         DROP  R2,R5                                                            
                                                                                
*                                                                               
DCOLX    DS    0H                                                               
         B     XIT_02                                                           
         DROP  R4                                                               
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--BTR#)'                       
*------------------------- BUILD TRACK RECORD ------------------------*         
                                                                                
* Builds a track record based on a Master Display Table entry                   
* At entry,                                                                     
*   R4-->Master Display Table entry                                             
* At exit,                                                                      
*   AIO = A(built track)                                                        
                                                                                
BLDTRKRC DS    0H                                                               
         USING MDPLYTBD,R4                                                      
                                                                                
*                                                                               
** SET UP A BASE RECORD **                                                      
*                                                                               
*** GET RECORD FROM FILE                                                        
*                                                                               
         DS    0H                                                               
         MVC   KEY(L'RINVKEY),MDTDIIVK                                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RINVKEY),KEYSAVE    DID WE FIND TRACK?                     
         BE    BTR024X                    YES                                   
         MVI   OURERRCD,TNLFQ             NO, SET UP ERROR MESSAGE              
                                                                                
         SR    R0,R0                                                            
         L     RE,AMDPLYTB                                                      
         LA    RF,IDMCOLHH                                                      
                                                                                
         CR    RE,R4                                                            
         BNL   *+18                                                             
         LA    RE,MDPLYTBQ(RE)                                                  
         IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         B     *-16                                                             
                                                                                
         ZIC   R1,7(RF)                                                         
         STC   R1,MYTEXT                                                        
         BCTR  R1,0                                                             
         EXMVC R1,MYTEXT+1,8(RF)           WITH TRACK NAME                      
                                                                                
         B     BTRXN                                                            
BTR024X  EQU   *                                                                
*                                                                               
         GOTO1 GETREC                                                           
                                                                                
*                                                                               
*** SAVE ORIGINAL RECORD AROUND ***                                             
*                                                                               
         DS    0H                  CLEAR TARGET AREA FIRST                      
         L     R0,AIO2                                                          
         LHI   R1,L'IO                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         L     RE,AIO                                                           
         ZICM  RF,(RINVLEN-RINVREC)(RE),(3)                                     
         L     R0,AIO2             COPY ORIGINAL INTO I/O2                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
*** DELETE UPDATE-ABLE ELEMENTS ***                                             
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'CD',AIO),0,0                    
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 (RF),(R1),,(X'DE',AIO),0,0                                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
** UPDATE RECORD **                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RINVREC,R6                                                       
*                                                                               
         MVC   DATADISP,DSP1EL                                                  
                                                                                
*                                                                               
*** CODE/TEXT ***                                                               
*                                                                               
         DS    0H                                                               
         L     R3,AIO2             LOOK FOR X'CD' ELEM IN ORIG RECD             
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL2                                                        
         BNE   BTR066                                                           
                                                                                
*                                                                               
         DS    0H                  FOUND ELEM IN ORIG RECD                      
         ZIC   R1,1(R3)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,ELEM,0(R3)                                                    
         LA    R3,ELEM                                                          
         B     BTR068                                                           
*                                                                               
BTR066   DS    0H                  ELEM NOT FOUND IN ORIG RECD                  
         LA    R3,ELEM                                                          
         USING RINVCEL,R3                                                       
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,RINVCLNQ                                                
                                                                                
         MVC   TMPKSRC,RINVKSRC    GET QUALIFIER FROM KSRC                      
         MVI   GOSUBN,RKS#                                                      
         GOTO1 AGOSUB               TMPQLFY HAS QUALIFIER                       
                                                                                
         MVC   RINVCSET,TMPQLFY    SET RINVCSET FIELD                           
         CLI   TMPQLFY,C'E'                                                     
         BE    BTR066D                                                          
         CLI   TMPQLFY,C'P'                                                     
         BE    BTR066D                                                          
         CLI   TMPQLFY,C'S'                                                     
         BE    BTR066D                                                          
         MVI   RINVCSET,0                                                       
BTR066D  EQU   *                                                                
         DROP  R3                                                               
         B     BTR068                                                           
                                                                                
*                                                                               
BTR068   DS    0H                  R3-->ELEM FIELD                              
         USING RINVCEL,R3                                                       
         MVC   RINVCODE,MDTCODE     UPDATE CODE                                 
         MVC   RINVCTXT,MDTTEXT     UPDATE TEXT                                 
         DROP  R3                                                               
*                                                                               
         BAS   RE,BTRPUTEL         PUT ELEMENT INTO RECORD                      
                                                                                
*                                                                               
*** DEMO VALUE OVERRIDES ***                                                    
*                                                                               
         DS    0H                  FIRST, PUT OVERRIDES FROM MDPLYTAB           
         XC    CNTDEM,CNTDEM                                                    
         LA    R5,MDTDVCLL                                                      
         USING DVCELLD,R5                                                       
                                                                                
*                                                                               
BTR082   DS    0H                                                               
         TM    DVCFLAG,DVCFDOVD     DOES THIS HAVE A DEMO OVERRIDE?             
         BZ    BTR085                NOPE                                       
*                                                                               
         DS    0H                    YES,                                       
         BAS   RE,BTRBOVR             BUILD OVERRIDE ELEMENT                    
                                                                                
         LA    R3,ELEM                                                          
         BAS   RE,BTRPUTEL            AND PUT IT IN THE RECORD                  
         BE    BTR085                 DO NEXT OVRD IF CC RETURNED EQUAL         
         CLI   DMCB+12,5               ELSE, CHECK IF RECD IS TOO LONG          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   OURERRCD,TMDOQ         IF YES, RETURN W/ ERROR CODE              
         B     BTRXN                                                            
*                                                                               
BTR085   DS    0H                   BUMP TO NEXT DEMO VALUE CELL                
         LA    R0,1                                                             
         AH    R0,CNTDEM                                                        
         CLM   R0,1,SVNDEMS                                                     
         BNL   BTR089                                                           
                                                                                
         STH   R0,CNTDEM                                                        
         LA    R5,DVCELLQ(R5)                                                   
         B     BTR082                                                           
         DROP  R5                                                               
*                                                                               
BTR089   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  SECOND, COPY REST OF OVRDS FROM ORIG         
         L     R3,AIO2                                                          
         MVI   ELCODE,X'DE'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL2                                                        
*                                                                               
BTR092   DS    0H                                                               
         BNE   BTR099               NO MORE OVERRIDE ELEMENTS                   
*                                                                               
         CLI   (RINVDXFL-RINVDXEL)(R3),X'50'  ALWAYWS PUT INDEX                 
         BE    BTR095                          ELEMENT INTO RECORD              
*                                                                               
         USING RINVNOEL,R3                                                      
         LA    RF,SVDMLST           SEE IF DEMO IN OVRD ELEM IS DSPLYED         
BTR092D  CLI   0(RF),XFF             IF END OF DEMOLIST,                        
         BE    BTR092F                DEMO NOT DSPLYED--PROCESS OVRD EL         
         CLC   RINVNOTP(2),1(RF)     MATCH AGAINST LIST OF DEMS DSPLYED         
         BE    BTR097                DEMO DISPLAYED--GET NXT OVRD ELEM          
         LA    RF,L'IKDMLST(RF)                                                 
         B     BTR092D                                                          
BTR092F  EQU   *                                                                
*                                                                               
         LA    R0,(RINVNODM+L'RINVNODM-RINVNOEL)-2                              
         GOTO1 HELLO,DMCB,(C'G',=C'REPFILE'),(RINVNOCD,AIO),           +        
               ((R0),RINVNOZR),0                                                
         CLI   DMCB+12,0            IF ELEMENT FOUND,                           
         BE    BTR097                DON'T ADD THIS TO NEW RECORD               
         CLI   DMCB+12,6            CHECK IF ELEMENT NOT FOUND                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BTR095   DS    0H                                                               
         BAS   RE,BTRPUTEL          PUT ELEMENT INTO NEW RECORD                 
         BE    BTR097                DO NEXT OVRD IF CC RETURNED EQUAL          
         CLI   DMCB+12,5             ELSE, CHECK IF RECD IS TOO LONG            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   OURERRCD,TMDOQ        IF YES, RETURN W/ ERROR CODE               
         B     BTRXN                                                            
                                                                                
*                                                                               
BTR097   DS    0H                                                               
         BAS   RE,NEXTEL2           GET NEXT OVERRIDE ELEMENT                   
         B     BTR092                                                           
*                                                                               
BTR099   EQU   *                                                                
                                                                                
*                                                                               
*** ACTIVITY ELEMENT ***                                                        
*                                                                               
         MVI   GOSUBN,UAE#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         B     BTRXY                                                            
                                                                                
         DROP  R4,R6                                                            
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
BTRXN    DS    0H                                                               
         B     NO_02                                                            
*                                                                               
BTRXY    DS    0H                                                               
         B     YES_02                                                           
         EJECT                                                                  
* Little helper routine to put an element into a REPFILE record                 
* At entry,                                                                     
*   AIO = A(record)                                                             
*   R3 -->element                                                               
* At exit,                                                                      
*   CC set to eq if elem successfully put into record                           
*   CC set to neq otherwise                                                     
                                                                                
BTRPUTEL NTR1                                                                   
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO,(R3),0                         
         CLI   DMCB+12,0                                                        
         B     XIT_02                                                           
                                                                                
                                                                                
* Little helper routine build an new format demo override element               
* At entry,                                                                     
*   CNTDEM is set                                                               
*   R4-->Master Display Table entry                                             
* At exit,                                                                      
*   ELEM has new format demo override element                                   
                                                                                
BTRBOVR  NTR1                                                                   
         USING MDPLYTBD,R4                                                      
*                                                                               
         LH    RE,CNTDEM                                                        
         LR    RF,RE                                                            
         MH    RE,=Y(L'IKDMLST)                                                 
         LA    RE,SVDMLST(RE)      RE-->DEMOLIST FOR DEMOS IN ENTRY             
         MH    RF,=Y(DVCELLQ)                                                   
         LA    RF,MDTDVCLL(RF)     RF-->DEMO VALUE CELL                         
         USING DVCELLD,RF                                                       
*                                                                               
         L     R1,AOVDMTAB                                                      
         USING OVDMTABD,R1                                                      
BTRBOVR3 CLI   0(R1),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OVDMMOD,1(RE)       MATCH ON DEMO MODIFIER                       
         BE    *+12                                                             
         LA    R1,OVDMTABQ(R1)                                                  
         B     BTRBOVR3                                                         
*                                                                               
         LA    R3,ELEM                                                          
         USING RINVNOEL,R3                                                      
         XC    RINVNOEL(12),RINVNOEL                                            
         MVI   RINVNOCD,X'DE'                                                   
         MVI   RINVNOLN,12                                                      
         MVC   RINVNOTP,OVDMMOD     DEMO MODIFIER                               
         MVC   RINVNODM,2(RE)       DEMO CATEGORY                               
         MVC   RINVNOPR,OVDMPRC     PRECISION                                   
         MVC   RINVNOVL,DVCOVRDM    OVERRIDING VALUE                            
         DROP  R1,R3,RF                                                         
*                                                                               
         B     XIT_02                                                           
         DROP  R4                                                               
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--GDV#)'                       
*-------------------------- GET DEMO VALUES --------------------------*         
                                                                                
* Gets the overridden and non-overridden demo values of a track.                
*  Overridden values are those that are just pulled off from a record.          
*  The non-overridden values are demo values pulled off from a record           
*  after the demo-override and minimum-value upgrade elements are               
*  stricken off the record.                                                     
* At entry,                                                                     
*   AINVTRK = A(inventory track)                                                
* At exit,                                                                      
*   DMVL_OVR contains the   overridden   values                                 
*   DMVL_NOV    "      "  non-overridden   "                                    
* CAUTION: The integrity of the inventory track is not maintained by            
*           this routine!                                                       
                                                                                
GTDEMVAL DS    0H                                                               
         XC    DMVL_OVR(DMVL_OVL),DMVL_OVR                                      
         XC    DMVL_NOV(DMVL_NOL),DMVL_NOV                                      
                                                                                
*                                                                               
** GET OVERRIDE DEMOS **                                                        
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,XDV#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DMVL_OVR(DMVL_OVL),TMPDMVLS                                      
                                                                                
*                                                                               
** DEMO VALUES W/O OVERRIDES **                                                 
*                                                                               
*** NON-OVERRIDDEN CALCULATED VALUES ***                                        
*                                                                               
         DS    0H                  DELETE OVERRIDE ELEMENTS                     
         MVI   DDOEFLAG,DDOEFCAL                                                
         MVI   GOSUBN,DDOE#                                                     
         MVC   TMPFLNAM,DCREPFIL                                                
         GOTO1 AGOSUB                                                           
*&&DO                                                                           
         GOTO1 HELLO,MYDMCB,(C'D',=C'REPFILE'),(X'DE',AINVTRK),0,0              
         CLI   MYDMCB+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
                                                                                
*                                                                               
         DS    0H                  GET NON-OVERRIDDEN DEMO VALUES               
         MVI   GOSUBN,XDV#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DMVL_NOV(DMVL_NOL),TMPDMVLS                                      
                                                                                
*                                                                               
*** NON-OVERRIDDEN CORE VALUES ***                                              
*                                                                               
         DS    0H                  DELETE (CORE) OVERRIDE ELEMENTS              
         MVI   DDOEFLAG,DDOEFCOR                                                
         MVI   GOSUBN,DDOE#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         DS    0H                  EXTRACT NON-OVERRIDDEN CORE VALUES           
         MVI   GOSUBN,XDV#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         DS    0H                  SEED IN NON-OVERRIDDEN CORE VALUES           
         ZIC   R0,NDEMS                                                         
         LA    R3,IKDMLST                                                       
         LA    R4,DMVL_NOV                                                      
         LA    R5,TMPDMVLS                                                      
         MVI   GOSUBN,ICD#                                                      
                                                                                
GDV034   DS    0H                                                               
         MVC   TMPDEMCD,0(R3)                                                   
         GOTO1 AGOSUB               IS BUCKET A CORE DEMO?                      
         BNE   *+10                                                             
         MVC   0(4,R4),0(R5)         YEP, MOVE CORRESPONDING VALUE IN           
         LA    R3,L'IKDMLST(R3)                                                 
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,GDV034                                                        
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
         DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--XDV#)'                       
*------------------------ EXTRACT DEMO VALUES ------------------------*         
                                                                                
* Extracts demo values from an inventory track record.                          
* At entry,                                                                     
*   AINVTRK  = A(inventory track record).                                       
* At exit,                                                                      
*   TMPDMVLS = extracted demo values.                                           
* WARNING: DBLOCK area addressed by ADBLOCK will get creamed here!              
                                                                                
XTRCTDMV DS    0H                                                               
         XC    TMPDMVLS(TMPDMVLL),TMPDMVLS                                      
*                                                                               
         DS    0H                  CLEAR DBLOCK                                 
         MVI   GOSUBN,CDBK#                                                     
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                  SET UP DBLOCK                                
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
         MVC   DBFILE,DCFILINV                                                  
         L     R6,AINVTRK                                                       
         ST    R6,DBAREC                 A(RECORD)                              
         LA    R0,(RINVPEL-RINVREC)(R6)  A(1ST ELEMENT)                         
         ST    R0,DBAQUART                                                      
         MVC   DBCOMFCS,ACOMFACS         A(COMFACS)                             
*                                                                               
         DS    0H                                                               
         GOTO1 DEMOUT,MYDMCB,(C'L',IKDMLST),DBLOCKD,TMPDMVLS                    
         DROP  R5                                                               
*                                                                               
         B     XIT_02                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--MNI#)'                       
*----------------------- MODIFY FOR NEXT INPUT -----------------------*         
                                                                                
* Bumps through all the tracks columns on the bottom of screen and              
*  turns on the "change to modified field for next input" flag (x'01'           
*  in FLDHDR+6) for those fields which were inputted this time.                 
*  Routine also checks the key fields for modification as well.                 
*  Routine usually called when an error occurred during a validation            
*  so that we know which fields to validate in the next transaction.            
                                                                                
MODNXTIN DS    0H                                                               
*                                                                               
** KEY FIELDS **                                                                
*                                                                               
         DS    0H                  STATION                                      
         TM    IDMSTTNH+4,X80       WAS IT MODIFIED THIS TIME?                  
         BZ    *+8                                                              
         OI    IDMSTTNH+6,X80+X01    YES, KEEP MODIFIED UNTIL VALIDATED         
*                                                                               
         DS    0H                  INVENTORY NUMBER                             
         TM    IDMINVH+4,X80        WAS IT MODIFIED THIS TIME?                  
         BZ    *+8                                                              
         OI    IDMINVH+6,X80+X01     YES, KEEP MODIFIED UNTIL VALIDATED         
*                                                                               
         DS    0H                  EFFECTIVE DATE                               
         TM    IDMEFFDH+4,X80       WAS IT MODIFIED THIS TIME?                  
         BZ    *+8                                                              
         OI    IDMEFFDH+6,X80+X01    YES, KEEP MODIFIED UNTIL VALIDATED         
                                                                                
*                                                                               
         DS    0H                                                               
         XC    CNTTRK,CNTTRK                                                    
                                                                                
*                                                                               
MNI010   DS    0H                                                               
                                                                                
*                                                                               
** CODE AND TEXT FIELDS **                                                      
*                                                                               
         DS    0H                                                               
         LH    R2,CNTTRK                                                        
         MH    R2,=Y(CTCELLQ)                                                   
         LA    R2,IDMRCODH(R2)                                                  
         USING CTCELLD,R2                                                       
                                                                                
*                                                                               
         DS    0H                  CODE FIELD                                   
         TM    CTCCODH+4,X80        WAS IT MODIFIED THIS TIME?                  
         BZ    *+8                                                              
         OI    CTCCODH+6,X80+X01     YES, KEEP MODIFIED UNTIL VALIDATED         
*                                                                               
         DS    0H                  TEXT FIELD                                   
         TM    CTCTXTH+4,X80        WAS IT MODIFIED THIS TIME?                  
         BZ    *+8                                                              
         OI    CTCTXTH+6,X80+X01     YES, KEEP MODIFIED UNTIL VALIDATED         
         DROP  R2                                                               
                                                                                
*                                                                               
** DEMO VALUES **                                                               
*                                                                               
         DS    0H                                                               
         XC    CNTDEM,CNTDEM                                                    
         LA    R2,IDMROWHH         R2-->FIRST DEMO ROW                          
         USING ROWDSECT,R2                                                      
                                                                                
*                                                                               
MNIDV12  DS    0H                  R2-->A DEMO ROW                              
         LH    R3,CNTTRK                                                        
         MH    R3,=Y(DDCELLQ)                                                   
         LA    R3,ROWDEMOS(R3)      R3-->DEMO COLUMN W/IN ROW                   
         USING DDCELLD,R3                                                       
                                                                                
*                                                                               
         DS    0H                  DEMO OVERRIDE FIELD                          
         TM    DDCOVRH+4,X80        WAS IT MODIFIED THIS TIME?                  
         BZ    *+8                                                              
         OI    DDCOVRH+6,X80+X01     YES, KEEP MODIFIED UNTIL VALIDATED         
         DROP  R3                                                               
                                                                                
*                                                                               
         DS    0H                  FINISHED W/ THIS DEMO                        
         LA    R0,1                                                             
         AH    R0,CNTDEM                                                        
         CLM   R0,1,=AL1(MXDEMOS)                                               
         BNL   MNIDV99                                                          
                                                                                
         STH   R0,CNTDEM                                                        
         LA    R2,SZDEMROW(R2)     BUMP TO NEXT DEMO ROW                        
         B     MNIDV12                                                          
MNIDV99  EQU   *                                                                
         DROP  R2                                                               
                                                                                
*                                                                               
         DS    0H                  BUMP TO NEXT COLUMN                          
         LA    R0,1                                                             
         AH    R0,CNTTRK                                                        
         CLM   R0,1,=AL1(MXTKLNTY)                                              
         BNL   MNI099                                                           
                                                                                
         STH   R0,CNTTRK                                                        
         B     MNI010                                                           
MNI099   EQU   *                                                                
                                                                                
*                                                                               
         B     XIT_02                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--MISC STUFF)'                 
*--------------------- SUBR02 MISCELLANEOUS STUFF --------------------*         
                                                                                
         DS    0H                                                               
         GETEL2 R3,DATADISP,ELCODE                                              
                                                                                
                                                                                
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR02--LTORG && CONSTANTS)+0        
               '                                                                
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
MONTAB   DC    C'NFMAYJO'                                                       
MONTABL  EQU   *-MONTAB                                                         
                                                                                
                                                                                
SUBR02L  EQU   *-SUBR02                                                         
         DS    0CL(X'1000'-SUBR02L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR03)'                             
***********************************************************************         
*======================= SUBROUTINE POOL THREE =======================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
SUBR03Q  EQU   (((*-RMP41+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP41+SUBR03Q                                                    
SUBR03   NMOD1 0,**4103**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R8             R8=A(SYSD)                                   
         USING SPOOLD,R7           R7=A(SPOOL WORK AREA)                        
                                                                                
         ZIC   R1,GOSUBN                                                        
         SH    R1,=Y(R02#)         SUBTRACT FOR SUB-RTN # 3                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R03_00(R1)                                                       
                                                                                
DDOE#    EQU   ((R03_01-*)/4+1)+R02#  DELETE DEMO OVERRIDE ELEMENTS             
ICD#     EQU   ((R03_02-*)/4+1)+R02#  IS CORE DEMO?                             
PLR#     EQU   ((R03_03-*)/4+1)+R02#  PROCESS LTRANS REQUEST                    
                                                                                
R03_00   DS    0H                                                               
R03_01   B     DELDEMOV               DELETE DEMO OVERRIDE ELEMENTS             
R03_02   B     ISCORDEM               IS CORE DEMO?                             
R03_03   B     PRCLTRNS               PROCESS LTRANS REQUEST                    
R03#     EQU   ((*-R03_00)/4+1)+R02#                                            
         DC    H'0'                                                             
                                                                                
YES_03   SR    RC,RC                                                            
NO_03    LTR   RC,RC                                                            
XIT_03   XIT1                                                                   
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR03--DDOE#)'                      
*------------------- DELETE DEMO OVERRIDE ELEMENTS -------------------*         
                                                                                
* Deletes the demo override elements (x'DE'), being careful not to              
*  delete the index element.  Caller has option to delete only                  
*  core overrides, calculated overrides, or all demo overrides.                 
* At entry,                                                                     
*   AINVTRK  = A(inventory track)                                               
*   DDOEFLAG = DDOEFCAL: delete calculated overrides only                       
*              DDOEFCOR:   "    core           "      "                         
*              DDOEFALL:   "    all            "                                
*   TMPFLNAM = file name                                                        
                                                                                
DELDEMOV DS    0H                                                               
         MVI   ELCODE,X'DE'                                                     
         MVC   DATADISP,DSP1EL                                                  
                                                                                
*                                                                               
** START OF LOOP **                                                             
*                                                                               
DDOE020  DS    0H                                                               
         L     R3,AINVTRK                                                       
         BAS   RE,GETEL3                                                        
*                                                                               
DDOE024  DS    0H                                                               
         BNE   DDOE099                                                          
                                                                                
         CLI   (RINVDXFL-RINVDXEL)(R3),X'50'   SEE IF ELEM IS AN INDEX          
         BNE   *+12                                                             
         BAS   RE,NEXTEL3                       IT IS, GET NEXT ELEM            
         B     DDOE024                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R2,X'70'            "BRANCH-ON-NOT-EQUAL"                        
         CLI   DDOEFLAG,DDOEFCAL    FOR CALCULATED DEMOS                        
         BE    DDOE027G                                                         
         LA    R2,X'80'            "BRANCH-ON-EQUAL"                            
         CLI   DDOEFLAG,DDOEFCOR    FOR CORE DEMOS                              
         BE    DDOE027G                                                         
         B     DDOE027X            EVERYTHING ELSE GOES THROUGH                 
*                                                                               
DDOE027G DS    0H                                                               
         MVC   TMPDEMCD+1(2),(RINVNOTP-RINVNOEL)(R3)                            
         MVI   GOSUBN,ICD#         GO TEST IF CORE DEMO OR NOT                  
         GOTO1 AGOSUB                                                           
         EX    R2,*+4                                                           
         BC    0,*+12                                                           
         BAS   RE,NEXTEL3                                                       
         B     DDOE024                                                          
DDOE027X EQU   *                                                                
                                                                                
*                                                                               
*** DELETE ELEMENT ***                                                          
*                                                                               
         DS    0H                 DELETE ELEM POINTED TO BY R3                  
         ZIC   R1,1(R3)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,ELEM,0(R3)       ELEM WILL CONTAIN SEARCH ARGUMENT            
                                                                                
         BCTR  R1,0                                                             
         LR    R0,R1               R0 = L(SEARCH ARGUMENT)                      
         GOTO1 HELLO,MYDMCB,(C'D',TMPFLNAM),(X'DE',AINVTRK),           +        
               ((R0),ELEM+2),0                                                  
         CLI   MYDMCB+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     DDOE020                                                          
                                                                                
*                                                                               
DDOE099  EQU   *                                                                
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
         DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR03--ICD#)'                       
*--------------------------- IS CORE DEMO? ---------------------------*         
                                                                                
* Routine determines whether a demo is a core demo or not                       
* At entry,                                                                     
*   TMPDEMCD = demo code to be tested                                           
* At exit,                                                                      
*   CC equal  if demo is a core demo                                            
*   CC nequal if demo is not a core demo                                        
                                                                                
ISCORDEM DS    0H                                                               
         LA    R0,COREDEMQ                                                      
         L     R1,ACOREDEM                                                      
*                                                                               
ICD020   DS    0H                                                               
         CLC   0(L'COREDEMS,R1),TMPDEMCD+2                                      
         BE    YES_03                                                           
         LA    R1,L'COREDEMS(R1)                                                
         BCT   R0,ICD020                                                        
         B     NO_03                                                            
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR03--PLR#)'                       
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
         B     XIT_03                                                           
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR03--MISC STUFF)'                 
*--------------------- SUBR03 MISCELLANEOUS STUFF --------------------*         
                                                                                
GETEL3   DS    0H                  "GETEL3  R3,DATADISP,ELCODE"                 
         PRINT OFF                                                              
         AH    R3,DATADISP                                                      
         PRINT ON                                                               
FIRSTEL3 DS    0H                                                               
         PRINT OFF                                                              
         CLI   0(R3),0                                                          
         BNE   *+10                                                             
         CLI   0(R3),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R3)                                                     
         BCR   8,RE                                                             
         PRINT ON                                                               
NEXTEL3  DS    0H                                                               
         PRINT OFF                                                              
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
         B     FIRSTEL3                                                         
         PRINT ON                                                               
                                                                                
                                                                                
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBR03--LTORG && CONSTANTS)+0        
               '                                                                
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR03L  EQU   *-SUBR03                                                         
         DS    0CL(X'1000'-SUBR03L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBRXM)'                             
***********************************************************************         
*==================== EXIT WITH MESSAGE ROUTINES =====================*         
                                                                                
* Exits which leave RMP41 entirely and displays a message go through            
*  this routine.                                                                
                                                                                
SUBRXMQ  EQU   (((*-RMP41+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP41+SUBRXMQ                                                    
XMSGRTN  NMOD1 0,**41XM**                                                       
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
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBRXM--ERR MSGS)'                   
*--------------------------- ERROR MESSAGES --------------------------*         
                                                                                
* At entry, R2-->field in error.                                                
                                                                                
XMERR    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         OI    MISCFLG1,MF1ERRQ                                                 
                                                                                
         MVI   GOSUBN,MNI#         KEEP OTHER MODIFIED FIELDS                   
         GOTO1 AGOSUB               MODIFIED FOR NEXT TIME                      
*                                                                               
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
IPFKQ    EQU   ((XMERR04-XMERR00)/4)+1                                          
DMNFQ    EQU   ((XMERR05-XMERR00)/4)+1                                          
DM2CQ    EQU   ((XMERR06-XMERR00)/4)+1                                          
TMDOQ    EQU   ((XMERR07-XMERR00)/4)+1                                          
ISTAQ    EQU   ((XMERR08-XMERR00)/4)+1                                          
TNLFQ    EQU   ((XMERR09-XMERR00)/4)+1                                          
DUPQ     EQU   ((XMERR10-XMERR00)/4)+1                                          
NDDFQ    EQU   ((XMERR11-XMERR00)/4)+1                                          
                                                                                
*                                                                               
XMERR00  DS    0H                                                               
XMERR01  B     MFLD                MISSING INPUT FIELD                          
XMERR02  B     IFLD                INVALID INPUT FIELD                          
XMERR03  B     RNF                 RECORD NOT FIELD                             
XMERR04  B     IPFK                INVALID PFKEY                                
XMERR05  B     DMNF                DEMO MENU NOT FOUND                          
XMERR06  B     DM2C                DEMO MENU MUST BE TWO CHARACTERS             
XMERR07  B     TMDO                TOO MANY DEMO OVERRIDES                      
XMERR08  B     ISTA                INVALID STATION                              
XMERR09  B     TNLF                TRACK CAN NO LONGER BE FOUND (&T)            
XMERR10  B     DUP                 &T DUPLICATED                                
XMERR11  B     NDDF                NO DEMOGRAPHIC DATA FOUND                    
ERRX#    EQU   ((*-XMERR00)/4)+1                                                
                                                                                
ERRX2#   EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERRQ   EQU   ((*-XMERR00)/4)+1                                                
         DC    H'0'                                                             
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
IPFK     DS    0H                  INVALID PFKEY                                
         MVC   MSGNUM2,=AL2(RR#IPFKY)                                           
         MVC   ACURFORC,AFRSTKEY                                                
         B     ERRGTXT                                                          
*                                                                               
DMNF     DS    0H                  DEMO MENU NOT FOUND                          
         MVC   MSGNUM2,=AL2(RR#DMTNF)                                           
         B     ERRGTXT                                                          
*                                                                               
DM2C     DS    0H                  DEMO MENU MUST BE TWO CHARACTERS             
         MVC   MSGNUM2,=AL2(RR#DMT2C)                                           
         B     ERRGTXT                                                          
*                                                                               
TMDO     DS    0H                  TOO MANY DEMO OVERRIDES                      
         MVC   MSGNUM2,=AL2(RR#TMDOV)                                           
         B     ERRGTXT                                                          
*                                                                               
ISTA     DS    0H                  INVALID STATION                              
         MVC   MSGNUM2,=AL2(RR#ISTA)                                            
         B     ERRGTXT                                                          
*                                                                               
TNLF     DS    0H                  TRACK CAN NO LONGER BE FOUND (&T)            
         MVC   MSGNUM2,=AL2(RR#TNLF)                                            
         B     ERRGTXT                                                          
*                                                                               
DUP      DS    0H                   &T DUPLICATED                               
         MVC   MSGNUM2,=AL2(RR#DUPLI)                                           
         B     ERRGTXT                                                          
*                                                                               
NDDF     DS    0H                  NO DEMOGRAPHIC DATA FOUND                    
         MVC   MSGNUM2,=AL2(RR#NDDF)                                            
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
         OC    ACURFORC,ACURFORC   WANT TO FORCE CURSOR ELSEWHERE?              
         BZ    *+8                                                              
         L     R2,ACURFORC          YEP                                         
                                                                                
         B     ALLMSGX                                                          
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBRXM--WRN MSGS)'                   
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
                                                                                
                                                                                
XMWRN00  DS    0H                                                               
WRNX#    EQU   ((*-XMWRN00)/4)+1                                                
                                                                                
XMWRNQ   EQU   ((*-XMWRN00)/4)+1                                                
         DC    H'0'                                                             
         EJECT                                                                  
* (Set warning message numbers here)                                            
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
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBRXM--INF MSGS)'                   
*---------------------------- INFO MESSAGES --------------------------*         
                                                                                
* At entry, R2-->appropriate field to put cursor on.                            
                                                                                
XMINF    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         XC    ACURFORC,ACURFORC                                                
*                                                                               
         DS    0H                  SET ADDRESS OF ERREX ROUTINE                 
         MVC   AERREX,ERREX                                                     
         CLI   OURINFCD,INFX#                                                   
         BL    XMINFGO                                                          
         MVC   AERREX,ERREX2                                                    
         CLI   OURINFCD,INFX2#                                                  
         BL    XMINFGO                                                          
         DC    H'0'                                                             
*                                                                               
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
                                                                                
NDPLYQ   EQU   ((XMINF01-XMINF00)/4)+1                                          
TKSVQ    EQU   ((XMINF02-XMINF00)/4)+1                                          
TKSSVQ   EQU   ((XMINF03-XMINF00)/4)+1                                          
                                                                                
XMINF00  DS    0H                                                               
XMINF01  B     NDPLY                                                            
INFX#    EQU   ((*-XMINF00)/4)+1                                                
                                                                                
XMINF02  B     TKSV                                                             
XMINF03  B     TKSSV                                                            
INFX2#   EQU   ((*-XMINF00)/4)+1                                                
                                                                                
XMINFQ   EQU   ((*-XMINF00)/4)+1                                                
         DC    H'0'                                                             
         EJECT                                                                  
NDPLY    DS    0H                       NO DATA TO DISPLAY                      
         MVC   MSGNUM2,=AL2(RI#NDPLY)                                           
         MVC   ACURFORC,AFRSTKEY                                                
         B     INFGTXT                                                          
*                                                                               
TKSV     DS    0H                       TRACK SAVED                             
         LA    R1,CONHEAD                                                       
         MVC   0(5,R1),=C'Track'                                                
         MVC   6(5,R1),=C'saved'                                                
         MVC   ACURFORC,AFRSTKEY                                                
         B     INFEXIT                                                          
*                                                                               
TKSSV    DS    0H                       TRACKS SAVED                            
         LA    R1,CONHEAD                                                       
         MVC   0(5,R1),=C'Track'                                                
         MVI   5(R1),C's'                                                       
         MVC   7(5,R1),=C'saved'                                                
         MVC   ACURFORC,AFRSTKEY                                                
         B     INFEXIT                                                          
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
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBRXM--LTORG && CONSTANTS)'         
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'RERMP41 - INVENTORY DEMOS (SUBRXM--MISC STUFF)'                 
*--------------------- SUBRXM MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBRXML  EQU   *-XMSGRTN                                                        
         DS    0CL(X'1000'-SUBRXML+1)                                           
***********************************************************************         
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP41 - INVENTORY DEMOS'                                      
***********************************************************************         
*========================== RMP41's EQUATES ==========================*         
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
                                                                                
PHINVTRA EQU   X'17'               PHASE NUMBER FOR INV/TRACKS                  
SCINVTRA EQU   X'E8'               SCREEN NUMBER FOR INV/TRACKS                 
                                                                                
PAGEQ    EQU   2                   TWA PAGE # FOR TEMPSTR                       
IKYINVL  EQU   RINVKINV-RINVREC+L'RINVKINV                                      
IKYSTDL  EQU   RINVKSTD-RINVREC+L'RINVKSTD                                      
TIASVLEN EQU   BIGENDSV-BIGAREA                                                 
ESTMNTHQ EQU   13                  TREAT YEARLY ESTIMATES AS MONTH 13           
                                                                                
SZDEMROW EQU   IDMRW2HH-IDMROWHH             SIZE OF ONE ROW                    
SZDEMSPC EQU   (IDMROWXH-IDMROWHH)+SZDEMROW  SIZE OF DEMO DISPLAY AREA          
WDTHCOLS EQU   SZDEMROW-(L'IDMROWHH+L'IDMROWH)                                  
WDTHCOL  EQU   (L'IDMROWDH+L'IDMROWD+L'IDMROWOH+L'IDMROWO)                      
                                                                                
MXTKLNTY EQU   WDTHCOLS/WDTHCOL    MAX TRACKLIST ENTRIES                        
MXDEMOS  EQU   SZDEMSPC/SZDEMROW   MAX NUMBER OF DEMOS                          
                                                                                
PFQSVMAP EQU   2                                                                
PFQSAVC1 EQU   1+PFQSVMAP          SAVE TRACK IN COLUMN 1                       
PFQSAVC2 EQU   2+PFQSVMAP          SAVE TRACK IN COLUMN 2                       
PFQSAVC3 EQU   3+PFQSVMAP          SAVE TRACK IN COLUMN 3                       
PFQSAVC4 EQU   4+PFQSVMAP          SAVE TRACK IN COLUMN 4                       
PFQSAVC5 EQU   5+PFQSVMAP          SAVE TRACK IN COLUMN 5                       
PFQSVALL EQU   8                   SAVE ALL TRACKS (THAT WERE CHANGED)          
PFQRDPLY EQU   10                  REDISPLAY                                    
                                                                                
BTKEST   EQU   1                   ESTIMATE BOOK                                
BTKPRJ   EQU   2                   PROJECTED BOOK                               
BTKTP    EQU   3                   TIME PERIOD BOOK                             
BTKSS    EQU   4                   SPECIAL SURVEY BOOK                          
BTKACT   EQU   5                   ACTUAL BOOK                                  
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0XL(2+2+2)                                                       
         DC    AL2(GOSUB-RMP41,0,AGOSUB-SYSD)                                   
         DC    AL2(SUBR01-RMP41,0,ASUBR01-SYSD)                                 
         DC    AL2(SUBR02-RMP41,0,ASUBR02-SYSD)                                 
         DC    AL2(SUBR03-RMP41,0,ASUBR03-SYSD)                                 
         DC    AL2(XMSGRTN-RMP41,0,AXMSGRTN-SYSD)                               
         DC    AL2(QLFKYTAB-RMP41,0,AQLFKYTB-SYSD)                              
         DC    AL2(OVPRFTAB-RMP41,0,AOVPRFTB-SYSD)                              
         DC    AL2(OVDMTAB-RMP41,0,AOVDMTAB-SYSD)                               
         DC    AL2(CODETAB-RMP41,0,ACODETAB-SYSD)                               
         DC    AL2(COREDEMS-RMP41,0,ACOREDEM-SYSD)                              
         DC    AL2(DMLSTTAB-RMP41,0,ADMLSTTB-SYSD)                              
         DC    AL2(HOMES-RMP41,0,AHOMES-SYSD)                                   
         DC    AL2(ADULTS-RMP41,0,AADULTS-SYSD)                                 
         DC    AL2(VIEWERS-RMP41,0,AVIEWERS-SYSD)                               
         DC    AL2(MEN-RMP41,0,AMEN-SYSD)                                       
         DC    AL2(WOMEN-RMP41,0,AWOMEN-SYSD)                                   
         DC    AL2(MDPLYTAB-BIGAREA,ATIA-GEND,AMDPLYTB-SYSD)                    
         DC    AL2(IVHDRECD-BIGAREA,ATIA-GEND,AINVHDR-SYSD)                     
         DC    AL2(DBLOCK1-BIGAREA,ATIA-GEND,ADBLOCK-SYSD)                      
         DC    AL2(DBXTND1-BIGAREA,ATIA-GEND,ADBXTND-SYSD)                      
         DC    AL2(DBXTRINV-BIGAREA,ATIA-GEND,ADBXTRIN-SYSD)                    
         DC    AL2(SVDBLOCK-BIGAREA,ATIA-GEND,ASVDBLK-SYSD)                     
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
LBLTAB   DS    0XL(2+8)            TABLE OF LABELS FOR BIG STORAGE              
         DC    AL2(MDTBLABL-BIGAREA),CL8'**MDTB**'                              
         DC    AL2(IVHDLABL-BIGAREA),CL8'*INVHDR*'                              
         DC    AL2(DBLKLABL-BIGAREA),CL8'*DBLOCK*'                              
         DC    AL2(DBXTLABL-BIGAREA),CL8'*DBXTND*'                              
         DC    AL2(SVDBLABL-BIGAREA),CL8'*SVDBLK*'                              
LBLTABQ  EQU   (*-LBLTAB)/(L'LBLTAB)                                            
                                                                                
                                                                                
TABCLR   DS    0XL(1+2+2)          STORAGES TO CLEAR                            
*&&DO                                                                           
         DC    C'I',AL2(IVHDRECD-BIGAREA),AL2(IVHDRECX-IVHDRECD)                
*&&                                                                             
         DC    C'I',AL2(DBLOCK1-BIGAREA),AL2(DBLOCK1X-DBLOCK1)                  
         DC    C'I',AL2(DBXTND1-BIGAREA),AL2(DBXTND1X-DBXTND1)                  
TABCLRQ  EQU   (*-TABCLR)/(L'TABCLR)                                            
                                                                                
                                                                                
QLFKYTAB DS    0XL(1+1)            QUALIFIER KEY CODE TABLE                     
         DC    CL1' ',AL1(BTKACT)   ACTUAL BOOK                                 
         DC    CL1'P',AL1(BTKPRJ)   PROJECTED                                   
         DC    CL1'T',AL1(BTKTP)    TIME PERIOD                                 
         DC    CL1'S',AL1(BTKSS)    SPCL SURVEY                                 
         DC    CL1'E',AL1(BTKEST)   ESTIMATED                                   
QLFKYTBQ EQU   (*-QLFKYTAB)/(L'QLFKYTAB)                                        
                                                                                
                                                                                
OVPRFTAB DS    0XL(OVPRFTBQ)       OVERRIDE PROFILE TABLE                       
         DC     CL1'E',AL1(RMPDEBKB),AL1(RMPDEBKA)   ESTIMATED                  
         DC     CL1'P',AL1(RMPDPBKB),AL1(RMPDPBKA)   PROJECTED                  
         DC     CL1' ',AL1(RMPDABKB),AL1(RMPDABKA)   ACTUAL BOOK                
         DC     CL1'T',AL1(RMPDTBKB),AL1(RMPDTBKA)   TIME PERIOD                
         DC     CL1'S',AL1(RMPDSBKB),AL1(RMPDSBKA)   SPCL SURVEY                
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
OVDMTAB  DS    0XL(OVDMTABQ)       OVERRIDE-ABLE DEMO MODIFIER TABLE            
         DC     CL1'R',XL1'81'      RATINGS                                     
         DC     CL1'T',XL1'42'      IMPRESSIONS                                 
         DC     CL1'S',XL1'81'      SHARES                                      
         DC     CL1'P',XL1'81'      PUTS                                        
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
DCLIST   DS    0C                  DATA DICTIONARY ESCAPE SEQUENCES             
         DCDDL RE#PF12R,L'RE@PF12R,L                                            
         DCDDL RE#PF12N,L'RE@PF12N,L                                            
DCLISTX  EQU   *                                                                
         EJECT                                                                  
*----------------------------- CODE TABLE ----------------------------*         
                                                                                
CODETAB  DS    0CL2                                                             
         DC     C'TP'                                                           
         DC     C'TT'                                                           
         DC     C'ES'                                                           
         DC     C'PJ'                                                           
         DC     C'PR'                                                           
         DC     C'PA'                                                           
         DC     C'PT'                                                           
         DC     C'TE'                                                           
         DC     C'PE'                                                           
         DC     C'NT'                                                           
         DC     C'FT'                                                           
         DC     C'MT'                                                           
         DC     C'YT'                                                           
         DC     C'JT'                                                           
         DC     C'OT'                                                           
         DC     C'RT'                                                           
         DC     C'NP'                                                           
         DC     C'FP'                                                           
         DC     C'MP'                                                           
         DC     C'YP'                                                           
         DC     C'OP'                                                           
         DC     C'RP'                                                           
         DC     C'JP'                                                           
         DC     C'  '                                                           
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*----------------------------- CORE DEMOS ----------------------------*         
                                                                                
COREDEMS DS    0XL1                                                             
         DC     AL1(127)     V2+                                                
         DC     AL1(145)     V18+                                               
         DC     AL1(129)     V1234                                              
         DC     AL1(128)     V1224                                              
         DC     AL1(125)     V1217                                              
         DC     AL1(123)     V611                                               
         DC     AL1(122)     V211                                               
         DC     AL1(045)     W18+                                               
         DC     AL1(041)     W1834                                              
         DC     AL1(042)     W1849                                              
         DC     AL1(047)     W2549                                              
         DC     AL1(048)     W2554                                              
         DC     AL1(028)     W1224                                              
         DC     AL1(049)     W2564                                              
         DC     AL1(029)     W1234                                              
         DC     AL1(095)     M18+                                               
         DC     AL1(091)     M1834                                              
         DC     AL1(092)     M1849                                              
         DC     AL1(098)     M2554                                              
         DC     AL1(099)     M2564                                              
         DC     AL1(001)     HOMES                                              
         DC     AL1(002)     META                                               
         DC     AL1(003)     METB                                               
         DC     AL1(065)     WWRK                                               
         DC     AL1(097)     M2549                                              
         DC     AL1(142)     A1849                                              
         DC     AL1(141)     A1834                                              
         DC     AL1(148)     A2554                                              
         DC     AL1(191)     V2149                                              
         DC     AL1(071)     W2149                                              
         DC     AL1(115)     M2149                                              
COREDEMX EQU   *                                                                
COREDEMQ EQU   (COREDEMX-COREDEMS)/L'COREDEMS                                   
         EJECT                                                                  
*--------------------------- DEMOLIST TABLE --------------------------*         
                                                                                
DMLSTTAB DS    0XL(1+2+1+1)                                                     
         DC     C'H',AL2(HOMES-RMP41),AL1((HOMESL-1)/L'HOMES)                   
         DC      AL1(HOMESL)                                                    
         DC     C'A',AL2(ADULTS-RMP41),AL1((ADULTSL-1)/L'ADULTS)                
         DC      AL1(ADULTSL)                                                   
         DC     C'V',AL2(VIEWERS-RMP41),AL1((VIEWERSL-1)/L'VIEWERS)             
         DC      AL1(VIEWERSL)                                                  
         DC     C'M',AL2(MEN-RMP41),AL1((MENL-1)/L'MEN)                         
         DC      AL1(MENL)                                                      
         DC     C'W',AL2(WOMEN-RMP41),AL1((WOMENL-1)/L'WOMEN)                   
         DC      AL1(WOMENL)                                                    
         DC    AL1(EOT)                                                         
                                                                                
HOMES    DS    0XL3                                                             
         DC     X'00',C'T',X'01'    HOMES                                       
         DC     X'00',C'T',X'02'    META                                        
         DC     X'00',C'T',X'03'    METB                                        
         DC     X'FF'                                                           
HOMESX   EQU   *                                                                
HOMESL   EQU   HOMESX-HOMES                                                     
                                                                                
ADULTS   DS    0XL3                                                             
         DC     X'00',C'T',AL1(145) AD18+                                       
         DC     X'00',C'T',AL1(142) AD1849                                      
         DC     X'00',C'T',AL1(141) AD1834                                      
         DC     X'00',C'T',AL1(148) AD2554                                      
         DC     X'00',C'T',AL1(191) AD2149                                      
         DC     X'00',C'T',AL1(153) AD3564                                      
         DC     X'FF'                                                           
ADULTSX  EQU   *                                                                
ADULTSL  EQU   ADULTSX-ADULTS                                                   
                                                                                
VIEWERS  DS    0XL3                                                             
         DC     X'00',C'T',AL1(127) V2+                                         
         DC     X'00',C'T',AL1(129) V1234                                       
         DC     X'00',C'T',AL1(128) V1224                                       
         DC     X'00',C'T',AL1(125) V1217                                       
         DC     X'00',C'T',AL1(123) V6-11                                       
         DC     X'00',C'T',AL1(122) V2-11                                       
         DC     X'FF'                                                           
VIEWERSX EQU   *                                                                
VIEWERSL EQU   VIEWERSX-VIEWERS                                                 
                                                                                
MEN      DS    0XL3                                                             
         DC     X'00',C'T',AL1(095) M18+                                        
         DC     X'00',C'T',AL1(091) M1834                                       
         DC     X'00',C'T',AL1(092) M1849                                       
         DC     X'00',C'T',AL1(098) M2554                                       
         DC     X'00',C'T',AL1(099) M2564                                       
         DC     X'00',C'T',AL1(097) M2549                                       
         DC     X'00',C'T',AL1(115) M2149                                       
         DC     X'FF'                                                           
MENX     EQU   *                                                                
MENL     EQU   MENX-MEN                                                         
                                                                                
WOMEN    DS    0XL3                                                             
         DC     X'00',C'T',AL1(045) W18+                                        
         DC     X'00',C'T',AL1(041) W1834                                       
         DC     X'00',C'T',AL1(042) W1849                                       
         DC     X'00',C'T',AL1(047) M2549                                       
         DC     X'00',C'T',AL1(048) W2554                                       
         DC     X'00',C'T',AL1(028) W1224                                       
         DC     X'00',C'T',AL1(049) W2564                                       
         DC     X'00',C'T',AL1(029) W1234                                       
         DC     X'00',C'T',AL1(065) WWRK                                        
         DC     X'00',C'T',AL1(071) W2149                                       
         DC     X'FF'                                                           
WOMENX   EQU   *                                                                
WOMENL   EQU   WOMENX-WOMEN                                                     
***********************************************************************         
         TITLE 'RERMP41 - INVENTORY DEMOS (RERMPWORKD)'                         
***********************************************************************         
*============================= RERMPWORKD ============================*         
       ++INCLUDE RERMPWORKD                                                     
***********************************************************************         
         TITLE 'RERMP41 - INVENTORY DEMOS (SYSD)'                               
***********************************************************************         
*========================== SYSD'S SYSSPARE ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*---------------------------- COMMON AREA ----------------------------*         
                                                                                
       ++INCLUDE RERMPITSYP                                                     
                                                                                
                                                                                
*----------------------- OWNED BY RERMP41 ONLY -----------------------*         
                                                                                
*                                 ************** WORK AREA ************         
MYDMWORK DS    12D                                                              
MYDUB    DS    D                                                                
MYDMCB   DS    6F                                                               
RELO     DS    F                                                                
MYWORK   DS    XL(L'WORK)                                                       
                                                                                
*                                 ************** ADDRESSES ************         
MYBASE1  DS    A                   A(1ST 4096 BYTES OF THIS PROGRAM)            
MYBASE2  DS    A                   A(2ND 4096 BYTES OF THIS PROGRAM)            
ASUBRTN  DS    A                   A(SUBROUTINE POOL TO USE)                    
AERREX   DS    A                   A(ERREX ROUTINE) TO USE                      
AERRFLD  DS    A                   A(FIELD W/ ERROR)                            
ASRCNTRY DS    A                   A(CORRESPONDING SRCTAB ENTRY)                
AINVTRK  DS    A                   A(INVENTORY TRACK)                           
DSP1EL   DS    H                   DISPL TO 1ST ELEMENT (IN REPFILE)            
         DS    H                   (SPARE TO KEEP FULLWORD-ALIGNMENT)           
                                                                                
         DS    0A                  ADDRESSES OF TABLES/ROUTINES                 
AGOSUB   DS    A                    A(SUBROUTINE POOL INTERFACE)                
ASUBR01  DS    A                    A(SUBROUTINE POOL #1)                       
ASUBR02  DS    A                    A(SUBROUTINE POOL #2)                       
ASUBR03  DS    A                    A(SUBROUTINE POOL #3)                       
AXMSGRTN DS    A                    A(EXIT W/ MSG ROUTINE)                      
AQLFKYTB DS    A                    A(QLFKYTAB)                                 
AOVPRFTB DS    A                    A(OVPRFTAB)                                 
AOVDMTAB DS    A                    A(OVDMTAB)                                  
ACODETAB DS    A                    A(CODETAB)                                  
ACOREDEM DS    A                    A(COREDEMS)                                 
ADMLSTTB DS    A                    A(DMLSTTAB)                                 
AHOMES   DS    A                    A(HOMES)                                    
AADULTS  DS    A                    A(ADULTS)                                   
AVIEWERS DS    A                    A(VIEWERS)                                  
AMEN     DS    A                    A(MEN)                                      
AWOMEN   DS    A                    A(WOMEN)                                    
AMDPLYTB DS    A                    A(MASTER DISPLAY TABLE)                     
AINVHDR  DS    A                    A(INVENTORY HEADER RECORD)                  
ADBLOCK  DS    A                    A(DBLOCK)                                   
ADBXTND  DS    A                    A(DBEXTEND)                                 
ADBXTRIN DS    A                    A(DBEXTEND AREA FOR DBXINVWK)               
ASVDBLK  DS    A                    A(SAVE DBLOCK)                              
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
IKSTTN   DS     CL5                 STATION                                     
IKINV    DS     CL4                 INVENTORY NUMBER                            
IKEFFDB  DS     XL3                 EFFECTIVE DATE (BINARY)                     
IKEFFDC  DS     XL2                 EFFECTIVE DATE (COMPRESSED)                 
IKTKLST  DS     (MXTKLNTY)XL3       LIST OF TRACKS (MAX 5)                      
IKTKLSTL EQU    *-IKTKLST                                                       
IKBTLST  DS     (MXTKLNTY)CL1       LIST OF BOOK TYPES (MAX 5)                  
IKBTLSTL EQU    *-IKBTLST                                                       
IKDMLST  DS     (MXDEMOS)XL3        LIST OF DEMOS                               
         DS     XL1                 + X'FF' END-OF-LIST MARKER                  
IKDMLSTL EQU    *-IKDMLST                                                       
                                                                                
*                                 ************* CONSTANTS *************         
DCFILTP  DS    CL3                 C'TP '                                       
DCFILPAV DS    CL3                 C'PAV'                                       
DCFILIUN DS    CL3                 C'IUN'                                       
DCFILINV DS    CL3                 C'INV'                                       
DCSRCNSI DS    CL3                 C'NSI'                                       
DCSRCMFX DS    CL3                 C'MFX'                                       
DCSRCSRC DS    CL3                 C'SRC'                                       
DCREPFIL DS    CL7                 C'REPFILE'                                   
                                                                                
*                                 ************ TEMP STORAGE ***********         
TMPSTTN  DS    CL5                 TEMP STORAGE FOR STATION                     
TMPINVN  DS    CL4                  "      "     "  INVENTORY NUMBER            
TMPBDATE DS    XL3                  "      "     "  BINARY DATE                 
TMPCDATE DS    XL2                  "      "     "  COMPRESSED DATE             
TMPKSRC  DS    CL1                  "      "     "  KEY FIELD--SOURCE           
TMPSRC   DS    CL3                  "      "     "  SOURCE                      
TMPFRBK  DS    0XL3                 "      "     "  FROM BOOK                   
TMPFRBTS DS     XL1                 "      "     "   BITS                       
TMPBOOK  DS     XL2                 "      "     "   BOOK                       
TMPBTYP  DS    CL1                  "      "     "  BOOK TYPE                   
TMPKBK   DS    XL2                  "      "     "  KEY FIELD--BOOK             
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
TMPQLFY  DS    CL1                  "      "     "  QUALIFIER                   
TMPQLFK  DS    XL1                  "      "     "  QUALIFIER KEY CODE          
TMPDMVLS DS    (MXDEMOS)XL4         "      "     "  DEMO VALUES                 
TMPDMVLL EQU   *-TMPDMVLS                                                       
TMPMDTN  DS    XL(MDPLYTBQ)         "      "     "  MDPLYTAB ENTRY              
TMPDEMCD DS    XL(L'IKDMLST)        "      "     "  1 DEMO CODE                 
TMPFLNAM DS    CL7                  "      "     "  FILE NAME                   
                                                                                
GOSUBN   DS    XL1                 SUBROUTINE NUMBER                            
MYRDUPDT DS    XL1                 MY VERSION OF RDUPDATE                       
MSGNUM2  DS    XL2                                                              
MSGSYS   DS    XL1                                                              
OURERRCD DS    XL1                 MY ERROR   CODE                              
OURWRNCD DS    XL1                 MY WARNING CODE                              
OURINFCD DS    XL1                 MY INFO    CODE                              
NTIMES   DS    XL1                 NUMBER OF TIMES TO LOOP                      
PRVSRC   DS    CL1                 PREVIOUS RATING SERVICE                      
                                                                                
*                                 ************** COUNTERS *************         
CNTTRK   DS    H                   COUNTER FOR TRACKS                           
CNTDEM   DS    H                   COUNTER FOR DEMOS                            
COUNTER  DS    XL1                                                              
                                                                                
*                                 *************** FLAGS ***************         
FRMINVTK DS    CL1                 CAME FROM INV/TRACKS (Y/N)                   
                                                                                
MISCFLG1 DS    XL1                 MISC FLAG #1                                 
MF1ERRQ  EQU    X01                                                             
                                                                                
CHNGFLG1 DS    XL1                 CHANGED (FROM PREV TRNSCTN) FLAG #1          
*&&DO                                                                           
CF1ACTEQ EQU    X80                 ACTION EQUATE CHANGED                       
*&&                                                                             
CF1KEY   EQU    X40                 A KEY FIELD CHANGED                         
CF1OPT   EQU    X20                 AN OPTION CHANGED                           
CF1TRK   EQU    X10                 TRACKS FIELD CHANGED                        
CF1DEM   EQU    X08                 DEMOS FIELD CHANGED                         
CF1KO    EQU   CF1KEY+CF1OPT                                                    
CF1KOTD  EQU   CF1KEY+CF1OPT+CF1TRK+CF1DEM                                      
CF1KTD   EQU   CF1KEY+CF1TRK+CF1DEM                                             
CF1TD    EQU   CF1TRK+CF1DEM                                                    
                                                                                
FLDPRCTN DS    CL1                 (U)NPROTECT OR (P)ROTECT                     
                                                                                
DDOEFLAG DS    CL1                 TYPE OF DEMO OVERRIDE TO DELETE              
DDOEFCAL EQU    C'C'                CALCULATED DEMOS                            
DDOEFCOR EQU    C'O'                CORE DEMOS                                  
DDOEFALL EQU    C'A'                ALL DEMOS                                   
                                                                                
*                                 *********** MISCELLANEOUS ***********         
GKSIPARM DS    CL5                 REGETKSRC INPUT  BLOCK                       
GKSOPARM DS    CL5                 REGETKSRC OUTPUT BLOCK                       
                                                                                
NTKLNTRY DS    XL1                 NUMBER OF TRACKLIST ENTRIES                  
NDEMS    DS    XL1                 NUMBER OF DEMO ENTRIES                       
NTKSV    DS    XL1                 NUMBER OF TRACKS SAVED                       
                                                                                
MYDEMDFY DS    CL1                 DEMO MODIFIER                                
MY_CSTAT DS    CL(L'CSTAT)         SAVE FIELD FOR CSTAT                         
                                                                                
*                                 ************** PROFILES *************         
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
                                                                                
*                                 ******* DATA DICTIONARY TERMS *******         
DSLIST   DS    0C                                                               
RE@PF12R DS     CL7                12=Rtrn                                      
RE@PF12N DS     CL7                12=Next                                      
DSLISTX  EQU   *                                                                
                                                                                
*                                 ********** DATA TO REMEMBER *********         
SVTKLST  DS    (MXTKLNTY)XL(L'IKTKLST)                                          
SVTKLSTL EQU   *-SVTKLST                                                        
         DS    0XL((SVTKLSTL-IKTKLSTL)+1)                                       
         DS    0XL((IKTKLSTL-SVTKLSTL)+1)                                       
                                                                                
SVBTLST  DS    (MXTKLNTY)XL(L'IKBTLST)                                          
SVBTLSTL EQU   *-SVBTLST                                                        
         DS    0XL((SVBTLSTL-IKBTLSTL)+1)                                       
         DS    0XL((IKBTLSTL-SVBTLSTL)+1)                                       
                                                                                
SVNDEMS  DS    XL(L'NDEMS)         NUMBER OF DEMO ENTRIES                       
                                                                                
SVDMLST  DS    (MXDEMOS)XL(L'IKDMLST)                                           
         DS    XL1                                                              
SVDMLSTL EQU   *-SVDMLST                                                        
         DS    0XL((SVDMLSTL-IKDMLSTL)+1)                                       
         DS    0XL((IKDMLSTL-SVDMLSTL)+1)                                       
                                                                                
*                                 ************** BUFFERS **************         
TRKLIST  DS    (MXTKLNTY)XL3       LIST OF TRACKS (MAX 5)                       
         DS    XL1                  + EOT MARKER                                
TRKLISTX EQU   *                                                                
TRKLISTL EQU   TRKLISTX-TRKLIST                                                 
                                                                                
DMVL_OVR DS    (MXDEMOS)XL4        TRACK'S DEMO VALUES W/  OVERRIDES            
DMVL_OVL EQU   *-DMVL_OVR                                                       
DMVL_NOV DS    (MXDEMOS)XL4          "      "     "    W/O     "                
DMVL_NOL EQU   *-DMVL_NOV                                                       
                                                                                
MYFLD    DS    0X                  MY TWA FIELD WORK AREA                       
MYFLDH   DS     XL8                 MY WORKING FIELD HEADER                     
MYFLDD   DS     XL78                MY WORKING FIELD DATA                       
MYFLDL   EQU   *-MYFLD                                                          
                                                                                
MYTEXT   DS    0X                  MISCELLANEOUS TEXT FIELD                     
         DS    XL1                  L'TEXT                                      
         DS    CL20                 THE TEXT ITSELF                             
MYTEXTX  EQU   *                                                                
MYTEXTL  EQU   MYTEXTX-MYTEXT                                                   
                                                                                
*                                                                               
MYSSPREL EQU   *-SYSSPARE                                                       
SYSPREMN EQU   L'SYSSPARE-MYSSPREL   AMOUNT LEFT IN SYSSPARE                    
         DS    0XL(SYSPREMN+1)       CHECK AGAINST SYSSPARE LIMIT               
***********************************************************************         
         TITLE 'RERMP41 - INVENTORY DEMOS (TWA DSECTS)'                         
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
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
         TITLE 'RERMP41 - INVENTORY DEMOS (DBLOCKD)'                            
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
         TITLE 'RERMP41 - INVENTORY DEMOS (OTHER DSECTS)'                       
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
* RERMPPROF                                                                     
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
         PRINT ON                                                               
***********************************************************************         
         TITLE 'RERMP41 - INVENTORY DEMOS (REGEN DSECTS)'                       
***********************************************************************         
*============================ REGEN DSECTS ===========================*         
                                                                                
*------------------------------ REGENINV -----------------------------*         
REINVRCD DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
*------------------------------ REGENDEM -----------------------------*         
REDEMRCD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENDEM                                                       
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ REGENAVL -----------------------------*         
RAVLRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENAVL                                                       
         PRINT ON                                                               
***********************************************************************         
         TITLE 'RERMP41 - INVENTORY DEMOS (MISC DSECTS)'                        
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
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
                                                                                
                                                                                
*-------------------------- BINSEARCH TRACK --------------------------*         
                                                                                
BINTRKD  DSECT                                                                  
BINTKXBK DS    XL2                 (INVERSED) BOOK                              
BINTKTQK DS    XL1                 TRACK QUALIFIER KEY                          
BINTKSRC DS    CL1                 RATING SERVICE                               
BINTKBT  DS    CL1                 BOOKTYPE                                     
BINTKKYL EQU   *-BINTRKD                                                        
                                                                                
BINTKQLF DS    CL1                 QUALIFIER                                    
BINTRKQ  EQU   *-BINTRKD                                                        
                                                                                
                                                                                
*----------------------- OVERRIDE PROFILE TABLE ----------------------*         
                                                                                
OVPRFTBD DSECT                                                                  
OVPRFQLF DS    CL1                 TRACK QUALIFIER                              
OVPRFDSP DS    XL1                 PROFILE'S DISPLACEMENT                       
OVPRFBIT DS    XL1                 PROFILE'S BIT                                
OVPRFTBQ EQU   *-OVPRFTBD                                                       
                                                                                
                                                                                
*----------------- OVERRIDE-ABLE DEMO MODIFIER TABLE -----------------*         
                                                                                
OVDMTABD DSECT                                                                  
OVDMMOD  DS    CL1                 DEMO MODIFIER                                
OVDMPRC  DS    XL1                 DEMO PRECISION                               
OVDMTABQ EQU   *-OVDMTABD                                                       
         EJECT                                                                  
*------------------------ MASTER DISPLAY TABLE -----------------------*         
                                                                                
MDPLYTBD DSECT                                                                  
MDTDIR   DS    0X                    DIRECTORY ITEM                             
MDTDIIVK DS     XL(L'RINVKEY)         INVENTORY KEY                             
MDTDICTL DS     XL1                   CONTROL BYTE                              
MDTDIDA  DS     XL4                   DISK ADDRESS                              
MDTFTNT  DS    CL16                  FOOTNOTE                                   
MDTCODE  DS    CL2                   CODE                                       
MDTTEXT  DS    XL2                   TEXT                                       
MDTFLAG  DS    XL1                   FLAGS                                      
MDTFNOVR EQU    X80                   CAN'T OVERRIDE TRACK'S DEMOS              
MDTFCHNG EQU    X40                   USER HAS MADE CHANGES TO TRACK            
MDTFSAVE EQU    X20                   TRACK HAS BEEN SAVED                      
MDTFNOTF EQU    X10                   TRACK NOT FOUND BY DEMAND                 
MDTFCPRT EQU    X04                   RESET PROTECTION FOR THIS TRACK           
MDTFDPLY EQU    X02                   (RE-)DISPLAY THIS TRACK                   
MDTFNEW  EQU    X01                   NEW TRACK--DIFF FROM PREVIOUS             
MDTDVCLL DS    (MXDEMOS)XL(DVCELLQ)  DEMO VALUE CELLS                           
MDPLYTBQ EQU     *-MDPLYTBD                                                     
                                                                                
                                                                                
*-------------------------- DEMO VALUE CELL --------------------------*         
                                                                                
DVCELLD  DSECT                                                                  
DVCFLAG  DS     XL1                FLAG                                         
DVCFDOVD EQU     X80                HAS A DEMO VALUE OVERRIDE                   
DVCFDMIN EQU     X40                HAS A MINIMUM DEMO VALUE OVERRIDE           
DVCFNOVR EQU     X20                OVERRIDE NOT ALLOWED HERE                   
DVCFCPRT EQU     X10                PROTECT DEMO VALUE DISPLAY FIELD            
DVCFDPLY EQU     X08                (RE-)DISPLAY DEMO VALUE                     
DVCFCCLL EQU     X04                CLEAR CELL                                  
DVCF1DEC EQU     X01                1 DECIMAL PLACE                             
DVCDEMVL DS     XL4                DEMO VALUE                                   
DVCOVRDM DS     XL4                OVERRIDING DEMO VALUE                        
DVCELLQ  EQU    *-DVCELLD                                                       
                                                                                
                                                                                
*-------------------------- DEMO VALUES ROW --------------------------*         
                                                                                
ROWDSECT DSECT                                                                  
ROWHDGH  DS    XL(L'IDMROWHH)      ROW HEADING HEADER                           
ROWHDG   DS    CL(L'IDMROWH)                                                    
ROWCOREH DS    XL(L'IDMROWCH)      ROW CORE FLAG HEADER                         
ROWCORE  DS    CL(L'IDMROWC)                                                    
ROWDEMOS DS    (MXTKLNTY)XL(DDCELLQ)                                            
ROWL     EQU   *-ROWDSECT                                                       
                                                                                
                                                                                
*------------------------- DEMO DISPLAY CELL -------------------------*         
                                                                                
DDCELLD  DSECT                                                                  
DDCNOVH  DS     XL(L'IDMROWDH)     NON-OVERRIDDEN DEMO HEADER                   
DDCNOVD  DS     XL(L'IDMROWD)      NON-OVERRIDDEN DEMO VALUE                    
DDCOVRH  DS     XL(L'IDMROWOH)     OVERRIDDEN DEMO HEADER                       
DDCOVRD  DS     XL(L'IDMROWO)      OVERRIDDEN DEMO VALUE                        
DDCELLQ  EQU    *-DDCELLD                                                       
                                                                                
         DS    0XL((DDCELLQ-WDTHCOL)+1)                                         
         DS    0XL((WDTHCOL-DDCELLQ)+1)                                         
                                                                                
                                                                                
*--------------------------- CODE/TEXT CELL --------------------------*         
                                                                                
CTCELLD  DSECT                                                                  
CTCCODH  DS    XL(L'IDMRCODH)                                                   
CTCCOD   DS    XL(L'IDMRCOD)                                                    
CTCTXTH  DS    XL(L'IDMRTXTH)                                                   
CTCTXT   DS    XL(L'IDMRTXT)                                                    
CTCELLQ  EQU   *-CTCELLD                                                        
                                                                                
         DS    0XL((CTCELLQ-(IDMRCT2H-IDMRCODH))+1)                             
         DS    0XL(((IDMRCT2H-IDMRCODH)-CTCELLQ)+1)                             
         EJECT                                                                  
*----------------------------- PFKEY LINE ----------------------------*         
                                                                                
PFLINED  DSECT                                                                  
PFLPF    DS    CL2                 C'PF'                                        
                                                                                
PFLSVCOL DS    (MXTKLNTY)CL(6+2)   3=SvC1  4=SvC2...                            
PFLSVALL DS    CL9                 8=SaveAll                                    
         DS    XL3                                                              
                                                                                
PFLRDPLY DS    CL9                 10=Rdsply                                    
         DS    XL3                                                              
                                                                                
         ORG   (PFLINED+L'IDMPFLN)-L'RE@PF12R                                   
PFLPF12  DS    CL(L'RE@PF12R)      12=Rtrn OR 12=Next                           
                                                                                
         ORG                                                                    
                                                                                
PFLINEX  EQU   *                                                                
PFLINEL  EQU   PFLINEX-PFLINED                                                  
         DS    0XL(L'IDMPFLN-PFLINEL+1)                                         
         DS    0XL(PFLINEL-L'IDMPFLN+1)                                         
                                                                                
                                                                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== BIG STORAGE AREA =========================*         
                                                                                
BIGAREA  DSECT                                                                  
                                                                                
                                                                                
         DS    0H                  1ST HALF RESERVED FOR INV/TRACKS             
         ORG   BIGAREA+X'2400'                                                  
                                                                                
                                                                                
MDTBLABL DS    D                   **MDTB**                                     
MDPLYTAB DS    (MXTKLNTY)XL(MDPLYTBQ)                                           
MDPLYTBX EQU   *                                                                
                                                                                
IVHDLABL DS    D                   *INVHDR*                                     
IVHDRECD DS    XL2000                                                           
IVHDRECX EQU   *                                                                
                                                                                
BIGENDSV EQU   *                                                                
                                                                                
DBLKLABL DS    D                   *DBLOCK*                                     
DBLOCK1  DS    XL(L'DBLOCK)                                                     
         DS    XL14                (RESERVED)                                   
DBLOCK1X DS    0X                                                               
                                                                                
DBXTLABL DS    D                   *DBXTND*                                     
DBXTND1  DS    0XL256                                                           
DBXTRINV DS     XL(DBXINVWL)        C'RINV' (DBXINVWK)                          
         DS    0XL(L'DBXTND1-(*-DBXTND1))                                       
DBXTND1X DS    0X                                                               
                                                                                
SVDBLABL DS    D                   *SVDBLK*                                     
SVDBLOCK DS    XL(DBLOCK1X-DBLOCK1)                                             
SVDBLCKX DS    0X                                                               
                                                                                
MYTIALEN EQU   *-BIGAREA                                                        
         DS    0CL((X'4800'-MYTIALEN)+1)                                        
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007RERMP41S  02/02/00'                                      
         END                                                                    
