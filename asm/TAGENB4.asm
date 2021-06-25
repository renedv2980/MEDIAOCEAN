*          DATA SET TAGENB4    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T702B4A                                                                  
         TITLE 'T702B4 - DEAL MAINTENANCE'                                      
T702B4   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702B4                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=PROGRAM SAVED STORAGE                     
         USING DEALD,R7                                                         
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     DLX                                                              
         SPACE 1                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     DLX                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         IF VALIDATE RECORD                           
         BNE   *+12                                                             
         BAS   RE,BLDREC           BUILD THE RECORD                             
         B     DLX                                                              
         SPACE 1                                                                
         CLI   MODE,XRECADD        IF RECORD ADDED                              
         BE    DL40                                                             
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BE    DL40                                                             
         CLI   MODE,XRECREST       OR RECORD RESTORED                           
         BE    DL40                                                             
         CLI   MODE,XRECDEL        OR RECORD DELETED                            
         BNE   DL50                                                             
DL40     BAS   RE,DISPLAY          RE-DISPLAY THE RECORD                        
         SPACE 1                                                                
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BE    PGDSPMS2            GIVE MY OWN MESSAGE                          
         B     DLX                                                              
         SPACE 1                                                                
DL50     CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BNE   DLX                                                              
         BAS   RE,DISPLAY          DISPLAY THE RECORD                           
         SPACE 1                                                                
         CLI   ACTNUM,ACTDIS       IF ACTION IS DISPLAY                         
         BE    PGDSPMSG            GIVE MY OWN MESSAGE                          
         SPACE 1                                                                
DLX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BE    *+8                                                              
         NI    SDLAGYH+4,X'DF'     SET AGENCY FIELD CHANGED                     
         SPACE 1                                                                
         TM    SDLAGYH+4,X'20'     IF AGENCY CHANGED                            
         BO    VK10                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SDLAGYH),SDLAGYNH  AGENCY             
         SPACE 1                                                                
VK10     TM    SDLCLIH+4,X'20'     IF CLIENT CHANGED                            
         BO    VK30                                                             
         XC    TGCLI,TGCLI         CLEAR CLIENT-RELATED FIELDS                  
         XC    SDLCLIN,SDLCLIN                                                  
         OI    SDLCLINH+6,X'80'                                                 
         SPACE 1                                                                
         CLI   SDLCLIH+5,0         INPUT IS OPTIONAL                            
         BE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',SDLCLIH),SDLCLINH  CLIENT             
VK20     OI    SDLCLIH+4,X'20'                                                  
         SPACE 1                                                                
         XC    DLS,DLS             CLEAR LAST EL. KEYS                          
         SPACE 1                                                                
VK30     GOTO1 RECVAL,DMCB,TLDLCDQ,(X'40',0)  BUILD KEY FOR DEAL REC.           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY THE KEY                                       
         SPACE 1                                                                
DKEY     NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2            AND I/O AREA                                 
         SPACE 1                                                                
         L     R4,AIO1             R4=A(DEAL RECORD)                            
         USING TLDLD,R4                                                         
         SPACE 1                                                                
         MVC   SDLAGY,TLDLAGY      AGENCY IS IN DEAL RECORD                     
         MVI   SDLAGYH+5,L'TLDLAGY                                              
         OI    SDLAGYH+6,X'80'                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SDLAGYH),SDLAGYNH   AGY NAME          
         SPACE 1                                                                
         MVC   SDLCLI,TLDLCLI      CLIENT IS IN DEAL RECORD                     
         XC    SDLCLIN,SDLCLIN                                                  
         OI    SDLCLINH+6,X'80'                                                 
         OC    SDLCLI,SDLCLI                                                    
         BZ    DK20                                                             
         MVI   SDLCLIH+5,L'TLDLCLI                                              
         OI    SDLCLIH+6,X'80'                                                  
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',SDLCLIH),SDLCLINH   CLI NAME          
         SPACE 1                                                                
DK20     XC    DLS,DLS             CLEAR LAST EL. KEYS                          
         SPACE 1                                                                
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            AND I/O AREA                                 
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         SPACE 1                                                                
         BAS   RE,VALDEAL          VALIDATE DEAL LINES                          
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
         SPACE 1                                                                
         MVC   KEY,SVKEY           RESTORE DEAL KEY                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE DEAL LINES                                   
         SPACE 1                                                                
VALDEAL  NTR1                                                                   
         OC    DLS,DLS             IF SOMETHING ON SCREEN                       
         BZ    VDL30                                                            
         L     R3,AIO              SET TO DELETE CORRES. DEAL ELEMENTS          
         MVI   ELCODE,TADLELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADLD,R3            R3=A(DEAL ELEMENT)                           
         SPACE 1                                                                
VDL10    CLC   TADLAREA(6),DLFRST  IF ELEMENT IS WITHIN DISPLAYED RANGE         
         BL    VDL20                                                            
         CLC   TADLAREA(6),DLLAST                                               
         BH    VDL20                                                            
         MVI   TADLEL,X'FF'        SET TO DELETE                                
         SPACE 1                                                                
VDL20    BAS   RE,NEXTEL                                                        
         BE    VDL10                                                            
         MVI   ELCODE,X'FF'        REMOVE MARKED ELEMENTS                       
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
VDL30    XC    TGAREA,TGAREA       CLEAR GLOBAL AREA CODE                       
         SPACE 1                                                                
         LA    R4,SDLL1H           R4=A(1ST LINE)                               
         USING LINED,R4                                                         
         LA    R0,MXLINES          R0=N'LINES                                   
         LA    R3,ELEMENT                                                       
         USING TADLD,R3            R3=A(DEAL ELEMENT)                           
         SPACE 1                                                                
VDL40    GOTO1 FLDVAL,DMCB,(X'80',LINAREAH),(X'80',LINTERMH) IF BLANK           
         BE    VDL90                                         SKIP LINE          
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     INITIALIZE NEW DEAL ELEMENT                  
         MVI   TADLEL,TADLELQ                                                   
         MVI   TADLLEN,TADLLNQ                                                  
         SPACE 1                                                                
         LA    R2,LINAREAH         AREA                                         
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         OC    TGAREA,TGAREA                                                    
         BNZ   VDL50                                                            
         GOTO1 RECVAL,DMCB,TLARCDQ,LINAREAH                                     
VDL50    MVC   TADLAREA,TGAREA                                                  
         SPACE 1                                                                
         LA    R2,LINUSEH          USE                                          
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLUSCDQ,(R2)                                         
         MVC   TADLUSE,TGUSE                                                    
         SPACE 1                                                                
         MVI   ELCODE,TADLELQ                                                   
         GOTO1 GETL,DMCB,(6,TADLAREA)  TEST IF THIS KEY ON REC ALREADY          
         BE    FLDINV                                                           
         SPACE 1                                                                
         LA    R2,LINTERMH         TERM                                         
         CLI   5(R2),0                                                          
         BE    VDL60                                                            
         CLC   8(2,R2),=C'UN'      IF UNLIMITED                                 
         BNE   VDL52                                                            
         MVI   TADLTERM,TADLTUNL   SET SPECIAL TERM                             
         B     VDL60                                                            
VDL52    CLC   8(2,R2),=C'1X'      IF ONE TIME                                  
         BNE   VDL54                                                            
         MVI   TADLTERM,TADLT1X    SET SPECIAL TERM                             
         B     VDL60                                                            
VDL54    CLI   8(R2),C' '                                                       
         BH    *+8                                                              
         MVI   8(R2),C'0'                                                       
         GOTO1 VALINUM                                                          
         MVC   TADLTERM,ACTUAL     SAVE N'MONTHS                                
         SPACE 1                                                                
VDL60    GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         SPACE 1                                                                
VDL90    LA    R4,LINNEXT          BUMP TO NEXT LINE                            
         BCT   R0,VDL40            AND CONTINUE IF MORE                         
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SDLL1H,SDLLSTH,PROT=Y             CLEAR THE SCREEN               
         SPACE 1                                                                
         BAS   RE,DISDEAL                        DISPLAY DEAL LINES             
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,SDLLCHGH             LAST CHANGED INFO              
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTREC),999  MAKE ALL FLDS VALID            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS DEAL LINES                                      
         SPACE 1                                                                
DISDEAL  NTR1                                                                   
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BNE   DDL10                                                            
         CLI   PFAID,14            AND USER WANTS ROOM FOR MORE                 
         BE    DDL20               CLEAR SAVED KEYS & LEAVE SCRN EMPTY          
         SPACE 1                                                                
DDL10    L     R3,AIO                                                           
         MVI   ELCODE,TADLELQ      LOOK FOR DEAL ELEMENTS                       
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
DDL20    XC    DLS,DLS             NONE FOUND - CLEAR SAVED EL. KEYS            
         B     DDLX                AND GET OUT                                  
         SPACE 1                                                                
         OC    DLS,DLS             IF WE HAVEN'T DISPLAYED ANYTHING YET         
         BZ    DDL50               THEN START WITH FIRST                        
         SPACE 1                                                                
         USING TADLD,R3            R3=A(DEAL ELEMENT)                           
         SPACE 1                                                                
         XR    R0,R0               INITIALIZE START SWITCH                      
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         BE    *+12                                                             
         CLI   MODE,XRECADD        OR JUST ADDED IT                             
         BNE   DDL30                                                            
         GOTO1 FLDVAL,DMCB,(X'40',SDLL1H),SDLLSTH  AND SCREEN CHANGED           
         BE    DDL30                                                            
         LA    R0,1                SET START SWITCH TO RE-DISPLAY               
         SPACE 1                                                                
DDL30    LTR   R0,R0               IF NEED TO DISPLAY CURRENT PAGE              
         BZ    DDL40                                                            
         CLC   TADLAREA(6),DLFRST  SCAN FOR FIRST EL DISPLAYED                  
         BNL   DDL50                                                            
         B     *+14                                                             
DDL40    CLC   TADLAREA(6),DLLAST  ELSE SCAN FOR 1ST EL AFTER LAST DISP         
         BH    DDL50                                                            
         BAS   RE,NEXTEL                                                        
         BE    DDL30                                                            
         XC    DLS,DLS             NONE LEFT - START FROM BEGINNING             
         B     DDL10                                                            
         SPACE 1                                                                
DDL50    LA    R4,SDLL1H           R4=A(1ST LINE)                               
         USING LINED,R4                                                         
         LA    R2,MXLINES          R2=N'LINES                                   
         SPACE 1                                                                
         XC    TGAREA,TGAREA       CLEAR GLOBAL AREA CODE                       
         MVC   DLFRST,TADLAREA     SAVE KEY OF FIRST EL. DISPLAYED              
         SPACE 1                                                                
DDL60    MVC   DLLAST,TADLAREA     SAVE KEY OF LAST EL. DISPLAYED               
         SPACE 1                                                                
         CLC   TADLAREA,TGAREA     IF THIS AREA IS SAME AS PREVIOUS             
         BE    DDL70               DON'T BOTHER DISPLAYING AGAIN                
         SPACE 1                                                                
         MVC   LINAREA,TADLAREA    AREA                                         
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLARCDQ,(X'8C',LINAREA),LINARENH  GET NAME           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
DDL70    MVC   LINUSE,TADLUSE      USE                                          
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLUSCDQ,(X'8C',LINUSE),LINUSENH   GET NAME           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         CLI   TADLTERM,TADLTUNL   IF UNLIMITED                                 
         BNE   *+14                                                             
         MVC   LINTERM,=C'UN'      DISPLAY "UN"                                 
         B     DDL74                                                            
         CLI   TADLTERM,TADLT1X    IF ONE TIME                                  
         BNE   *+14                                                             
         MVC   LINTERM,=C'1X'      DISPLAY "1X"                                 
         B     DDL74                                                            
         EDIT  (1,TADLTERM),(2,LINTERM),ZERO=BLANK    TERM                      
         SPACE 1                                                                
DDL74    MVI   ELCODE,TADLELQ      RESET FOR DEAL ELEMENTS                      
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   DDLX                                                             
         LA    R4,LINNEXT          BUMP TO NEXT LINE                            
         BCT   R2,DDL60                                                         
         SPACE 1                                                                
DDLX     XR    R4,R4               SET TO COUNT TOTAL N'PAGES                   
         BAS   RE,PGIT                                                          
         MVC   NPAGES,BYTE                                                      
         SPACE 1                                                                
         LA    R4,DLLAST           SET TO CALC THIS PAGE NUMBER BASED           
         BAS   RE,PGIT             ON LAST DEAL ELEMENT DISPLAYED               
         MVC   THISPG,BYTE                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES CURRENT PAGE NUMBER                           
         SPACE 1                                                                
*                                  R4=0 (COUNT ALL) OR A(LAST DSPLY'D)          
PGIT     NTR1                                                                   
         XR    R2,R2               R2=COUNT                                     
         L     R3,AIO                                                           
         MVI   ELCODE,TADLELQ      LOOK FOR DEAL ELEMENTS                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PG10     BAS   RE,NEXTEL                                                        
         BNE   PG20                                                             
         AH    R2,=H'1'            ADD 1 TO COUNT                               
         LTR   R4,R4               IF SCANNING FOR LAST EL.                     
         BZ    PG10                                                             
         CLC   DLLAST,TADLAREA     TEST IF WE'VE REACHED IT                     
         BNE   PG10                                                             
         SPACE 1                                                                
PG20     LA    R1,MXLINES-1                                                     
         AR    R1,R2               ADD N'LINES-1 FOR DIVIDE                     
         XR    R0,R0                                                            
         LA    R2,MXLINES                                                       
         DR    R0,R2               DIVIDE BY N'LINES/SCREEN                     
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                MUST HAVE AT LEAST 1 PAGE                    
         STC   R1,BYTE             RETURN BYTE=PAGE NUMBER                      
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
PGDSPMS2 CLI   PFAID,14            TEST DISPLAYED NEW PAGE                      
         BE    PLSENTER                                                         
PGDSPMSG MVI   MYMSGNO1,54         PAGE X OF Y DISPLAYED                        
         L     R2,AFRSTKEY                                                      
         CLI   ACTNUM,ACTDIS                                                    
         BE    PGDSP2                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+12                                                             
         CLI   THISLSEL,C'S'                                                    
         BE    PGDSP2                                                           
         MVI   MYMSGNO1,55          ... - ENTER CHANGES AS DESIRED              
         L     R2,AFRSTREC                                                      
PGDSP2   MVI   BLOCK,2                                                          
         EDIT  THISPG,(1,BLOCK+1)                                               
         MVI   BLOCK+2,2                                                        
         EDIT  NPAGES,(1,BLOCK+3)                                               
         MVI   BLOCK+4,0                                                        
         B     INFEND                                                           
         SPACE 1                                                                
PLSENTER MVI   MYMSGNO1,2          PLEASE ENTER REQUIRED FIELDS                 
         MVI   MYMSYS,X'FF'                                                     
         LA    R2,SDLL1H           CURSOR TO FIRST DEAL LINE                    
         B     INFEND                                                           
         SPACE 1                                                                
INFEND   OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
MXLINES  EQU   (SDLPFKH-SDLL1H)/LINLNQ  MAX N'DEAL LINES/SCREEN                 
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              LOCAL SAVED STORAGE                                              
         SPACE 1                                                                
DEALD    DSECT                                                                  
DLS      DS    0CL12                                                            
DLFRST   DS    CL6                 KEY OF FIRST EL DISPLAYED ON SCREEN          
DLLAST   DS    CL6                 KEY OF LAST EL DISPLAYED ON SCREEN           
THISPG   DS    XL1                 CURRENT PAGE NUMBER                          
NPAGES   DS    XL1                 TOTAL N'PAGES                                
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
         SPACE 3                                                                
*              DSECT TO COVER SCREEN DEAL LINES                                 
         SPACE 1                                                                
LINED    DSECT                                                                  
LINAREAH DS    CL8                                                              
LINAREA  DS    CL3                 AREA                                         
         DS    CL8                                                              
LINARENH DS    CL8                                                              
LINAREN  DS    CL16                AREA NAME                                    
LINUSEH  DS    CL8                                                              
LINUSE   DS    CL3                 USE                                          
         DS    CL8                                                              
LINUSENH DS    CL8                                                              
LINUSEN  DS    CL16                USE NAME                                     
LINTERMH DS    CL8                                                              
LINTERM  DS    CL2                 TERM (MONTHS)                                
         DS    CL8                                                              
LINLNQ   EQU   *-LINED                                                          
LINNEXT  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRB4D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008TAGENB4   05/01/02'                                      
         END                                                                    
