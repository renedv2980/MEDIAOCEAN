*          DATA SET PRSFM25    AT LEVEL 014 AS OF 09/06/06                      
*PHASE T41C25A                                                                  
*                                                                               
*     CHANGE LOG                                                                
*                                                                               
* BOBY  03/06   COLLECT SECURITY MANAGEMENT PIDS                                
*                                                                               
* SMYE  04/04   ALLOW 2 CHARACTER CLIENT GROUP IDS                              
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  TITLE        T41C25 - PUB/CLIENT GROUP DEFINITION MAINT/LIST       *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST  *         
*                                                                     *         
*  INPUTS       SCREEN T41CB1 (MAINTENANCE)                           *         
*               SCREEN T41CB8 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED GROUP DEFINITION RECORDS                      *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- SVSPARE                                         *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- FREE                                            *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - GROUP DEFINITION RECORD                         *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C25 - PUB/CLIENT GROUP DEFINITION RECORDS'                   
T41C25   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C25                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         LH    R3,=Y(SVSPARE-T41CFFD)                                           
         AR    R3,RA               POINT TO TWA0 END FOR SAVES                  
         USING SVSPARED,R3                                                      
*                                                                               
         BRAS  RE,PID              GET USER'S PID AND SECURITY AGENCY           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,XRECADD        AFTER ADD                                    
         BE    DR                                                               
         CLI   MODE,XRECPUT        AFTER PUT                                    
         BE    DR                                                               
*                                                                               
         CLI   MODE,RECDEL         DELETE NOT ALLOWED                           
         BNE   *+16                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
XIT      ANSR                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       DS    0H                                                               
*                                                                               
         TM    CONRECH+4,X'80'     INPUT THIS TIME ?                            
         BO    VK01                YES - CLEAR LIST START FIELDS                
         TM    CONKEYH+4,X'80'     INPUT THIS TIME ?                            
         BO    VK01                YES - CLEAR LIST START FIELDS                
         TM    SFMMEDH+4,X'80'     INPUT THIS TIME ?                            
         BO    VK01                YES - CLEAR LIST START FIELDS                
         TM    SFMIDH+4,X'80'      INPUT THIS TIME ?                            
         BNO   VK02                YES - CLEAR LIST START FIELDS                
*                                                                               
VK01     DS    0H                                                               
         XC    SORTLAST,SORTLAST   CLEAR LIST START FIELD AND                   
*                                                                               
         LA    R0,BKFRMSEL         CLEAR THIS AREA FOR STORING ALL              
         LHI   R1,SORTDATX-BKFRMSEL  ID CODES TO BE SORTED                      
         SR    RE,RE                 IN LISTREC                                 
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
VK02     DS    0H                                                               
         LA    R2,SFMMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         LA    R6,SVKEY            CLEAR KEY                                    
         USING GRPKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
*                                                                               
         CLI   RECNUM,36           CLIENT GROUP DEFINITION?                     
         BNE   *+12                                                             
         MVI   GRPKRCOD,GRPKCTYQ                                                
         B     VK10                                                             
         CLI   RECNUM,42           PUB GROUP DEFINITION?                        
         BNE   *+12                                                             
         MVI   GRPKRCOD,GRPKBTYQ                                                
         B     VK10                                                             
         DC    H'0'                HOW DID WE GET HERE?                         
*                                                                               
VK10     DS    0H                                                               
         MVC   GRPKAGY,AGENCY                                                   
         MVC   GRPKMED,QMED                                                     
*                                                                               
         LA    R2,SFMIDH           GROUP ID                                     
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VKX                 YES - OPTIONAL                               
         MVI   ERROR,MISSING       OTHERWISE REQUIRED                           
         B     TRAPERR                                                          
*                                                                               
VK20     MVI   ERROR,NOTALPHA                                                   
         TM    4(R2),X'04'         ALPHABETIC CHARACTER?                        
         BZ    TRAPERR                                                          
         MVC   GRPKID,8(R2)        PUT ID IN KEY                                
         CLI   RECNUM,36           CLIENT GROUP DEFINITION?                     
         BE    VK21                YES                                          
         CLI   5(R2),1             PUB GROUP ID LIMITED TO 1 CHAR               
         BNE   BADGRPID            GROUP ID NOT VALID                           
         B     VKX                                                              
* TRANSLATE 2 CHAR CODE TO ONE CHAR                                             
VK21     MVC   WORK(2),8(R2)                                                    
         OI    WORK+1,C' '                                                      
         LA    RE,SPCGRTAB                                                      
         LHI   RF,SPCGRTBX-SPCGRTAB                                             
*                                                                               
VK22     CLC   WORK(2),0(RE)                                                    
         BE    VK24                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,VK22                                                          
         B     BADGRPID            GROUP ID NOT VALID                           
*                                                                               
VK24     MVC   GRPKID,2(RE)        SET 1 BYTE GROUP ID                          
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,SFMBK1H          BREAK 1 NAME REQUIRED                        
         CLI   5(R2),0                                                          
         BE    TRAPERR                                                          
         LA    R2,SFMLN1H          BREAK 1 LENGTH REQUIRED                      
         BAS   RE,CHECKLN                                                       
         BNE   NOCHANGE            BREAK LENGTH CANNOT BE CHANGED               
         CLI   5(R2),0                                                          
         BE    TRAPERR                                                          
         TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         ZIC   R7,SFMLN1                                                        
         N     R7,=X'0000000F'                                                  
         CH    R7,=H'1'            WITHIN RANGE 1..4?                           
         BL    BADLENG                                                          
         CH    R7,=H'4'                                                         
         BH    BADLENG                                                          
*                                                                               
         SR    R4,R4               WILL CONTAIN BREAK 2 LENGTH                  
         LA    R2,SFMLN2H                                                       
         BAS   RE,CHECKLN                                                       
         BNE   NOCHANGE            BREAK LENGTH CANNOT BE CHANGED               
         CLI   SFMBK2H+5,0         BREAK 2 NAME OPTIONAL                        
         BNE   *+16                                                             
         CLI   SFMLN2H+5,0                                                      
         BE    VR10                BREAK 2 LENGTH INVALID WITHOUT NAME          
         B     NOLENG                                                           
         CLI   5(R2),0             BUT REQUIRED WITH NAME                       
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         ZIC   R4,SFMLN2                                                        
         N     R4,=X'0000000F'                                                  
         CH    R4,=H'1'            WITHIN RANGE 1..4?                           
         BL    BADLENG                                                          
         CH    R4,=H'4'                                                         
         BH    BADLENG                                                          
         LA    R1,0(R4,R7)                                                      
         CH    R1,=H'4'                                                         
         BH    BADSUML             SUM OF LENGTHS CANNOT EXCEED FOUR            
*                                                                               
VR10     XC    ELEM,ELEM           BUILD ELEMENT                                
         LA    R6,ELEM                                                          
         USING GRPBRKD,R6                                                       
         MVI   GRPBRKCD,GRPBRKCQ                                                
         MVI   GRPBRKLN,GRPBRKLQ                                                
         MVC   GRPBK1,SFMBK1                                                    
         STC   R7,GRPBK1LN                                                      
         MVC   GRPBK2,SFMBK2                                                    
         STC   R4,GRPBK2LN                                                      
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
*        VALIDATE SECURITY MANAGEMENT PIDS                                      
*                                                                               
VRPID    DS    0H                                                               
*                                                                               
         MVI   ERROR,0             INIT ERROR CODE                              
*                                                                               
         XC    SVSECELM,SVSECELM   INIT SECURITY ELM SAVEAREA                   
*                                                                               
         L     R6,AIO              POINT TO CLIENT GROUP DEFN RECORD            
         MVI   ELCODE,GRPSECCQ     SECURITY MANAGEMENT ELEMENT                  
         BRAS  RE,GETEL            FIND CURRENT ELEMENT                         
         BNE   *+10                NOT FOUND                                    
         MVC   SVSECELM,0(R6)      SAVE OLD SECURITY ELEMENT                    
*                                                                               
         GOTO1 REMELEM             REMOVE OLD ELEMENT IF THERE                  
*                                                                               
         XC    ELEM,ELEM           INIT SECURITY MANAGEMENT ELEMENT             
         LA    R6,ELEM                                                          
         USING GRPSECD,R6                                                       
*                                                                               
         MVI   GRPSECCD,GRPSECCQ   ELEMENT CODE                                 
         MVI   GRPSECLN,GRPSECLQ   ELEMENT LENGTH                               
*                                                                               
         LA    R0,6                NUMBER OF PID FIELDS ON SCREEN               
         LA    R2,SFMPIDH          POINT TO FIRST PID FIELD                     
*                                                                               
         LA    R3,GRPSPID          POINT TO 1ST SECURITY MANAGEMENT PID         
*                                                                               
VRPIDLP  DS    0H                                                               
*                                                                               
         GOTOR VALPID,DMCB,0(R3)   VALIDATE PID AND STORE AT R3                 
*                                                                               
VRPIDCN  DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT UNPROTECTED                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT PID                             
         BCT   R0,VRPIDLP                                                       
*                                                                               
VRPIDDN  DS    0H                                                               
*                                                                               
*        SHUFFLE PIDS SO THERE ARE NO GAPS IN DISPLAY                           
*                                                                               
         LA    RE,GRPSPIDS                                                      
         LA    RF,GRPSPIDS                                                      
         LA    R0,6                MAX SIX PIDS                                 
*                                                                               
VRPIDSLP DS    0H                                                               
*                                                                               
         OC    0(L'GRPSPID,RF),0(RF) SKIP IF NO PID                             
         BZ    VRPIDSCN                                                         
*                                                                               
         MVC   0(L'GRPSPID,RE),0(RF) MOVE PID TO NEXT SLOT                      
         LA    RE,L'GRPSPID(RE)    BUMP TO NEXT SLOT                            
*                                                                               
VRPIDSCN DS    0H                                                               
*                                                                               
         LA    RF,L'GRPSPID(RF)    NEXT PID                                     
         BCT   R0,VRPIDSLP                                                      
*                                                                               
VRPIDSDN DS    0H                                                               
*                                                                               
VRPIDXLP DS    0H                                                               
*                                                                               
         CR    RE,RF               IF SLOTS REMAIN IN LIST                      
         BNL   VRPIDXDN                                                         
*                                                                               
         XC    0(L'GRPSPID,RE),0(RE)    CLEAR THEM                              
*                                                                               
VRPIDXCN DS    0H                                                               
*                                                                               
         LA    RE,L'GRPSPID(RE)                                                 
         B     VRPIDXLP                                                         
*                                                                               
VRPIDXDN DS    0H                                                               
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
*                                                                               
VRPIDX   DS    0H                                                               
*                                                                               
         MVC   KEY,SVKEY           RESTORE RECORD KEY                           
*                                                                               
VRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* IF ANY GROUPS EXIST FOR THIS GROUP ID, THEN BREAK LENGTHS                     
* CANNOT BE CHANGED.                                                            
*                                                                               
CHECKLN  NTR1                                                                   
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    YES                 DON'T BOTHER CHECKING                        
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R6,KEY                                                           
         USING GRPKEY,R6                                                        
         L     R1,AIO                                                           
         MVC   GRPKEY,0(R1)        GROUP ID KEY                                 
         MVC   GRPKCODE,=X'0001'   THE LOWEST POSSIBLE GROUP CODE               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(8),KEYSAVE      ANY GROUPS FOR THIS GROUP ID?                
         BNE   YES                 NO -- OK TO CHANGE BREAK LENGTHS             
*                                                                               
         LR    R1,R2               R1 = A(THIS LENGTH FIELD)                    
         ZIC   R0,0(R1)                                                         
         AR    R1,R0               R1 = A(ASSOCIATED INVISIBLE FIELD)           
         CLC   8(1,R2),8(R1)       WAS FIELD CHANGED?                           
         BE    YES                 NO                                           
*                                                                               
         MVC   8(1,R2),8(R1)       YES -- RESTORE OLD LENGTH                    
         OI    6(R2),X'80'         XMIT                                         
         B     NO                                                               
         EJECT                                                                  
         DROP  R6                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       CLI   ACTNUM,ACTSEL       SELECT FROM LIST?                            
         BNE   DR20                                                             
         LA    R7,LISTDIR          LIST SCREEN INFO                             
         CLI   SELLISTN,0          FIRST ON LIST SCREEN?                        
         BE    DR10                                                             
         ZIC   R0,SELLISTN         RELATIVE LINE NUMBER                         
         LA    R7,6(R7)            NEXT LIST ENTRY                              
         BCT   R0,*-4                                                           
DR10     CLI   0(R7),C'D'          ACTION DELETE?                               
         BE    DRX                 DON'T DISPLAY RECORD ON DELETE               
*                                                                               
DR20     L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   SFMBK1,GRPBK1       NAMES                                        
         OI    SFMBK1H+6,X'80'                                                  
         MVC   SFMBK2,GRPBK2                                                    
         OI    SFMBK2H+6,X'80'                                                  
         EDIT  GRPBK1LN,(1,SFMLN1) LENGTHS                                      
         OI    SFMLN1H+6,X'80'                                                  
         MVC   SFMZLN1,SFMLN1                                                   
         CLI   SFMZLN1,C' '        A SPACE?                                     
         BNE   *+8                                                              
         MVI   SFMZLN1,0                                                        
         EDIT  GRPBK2LN,(1,SFMLN2)                                              
         OI    SFMLN2H+6,X'80'                                                  
         MVC   SFMZLN2,SFMLN2                                                   
         CLI   SFMZLN2,C' '        A SPACE?                                     
         BNE   *+8                                                              
         MVI   SFMZLN2,0                                                        
         DROP  R6                                                               
*                                                                               
*        DISPLAY SECURITY MANAGET PIDS                                          
*                                                                               
DRPID    L     R6,AIO                                                           
         MVI   ELCODE,GRPSECCQ     PID ELEMENT                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         SR    R6,R6               NO ELEMENT FOUND                             
*                                                                               
         USING GRPSECD,R6          ESTABLISH SECURITY ELEMENT                   
*                                                                               
         LA    R0,6                NUMBER OF PID FIELDS ON SCREEN               
         LA    R2,SFMPIDH          POINT TO FIRST PID FIELD                     
*                                                                               
         LA    R3,GRPSPID          POINT TO 1ST SEURITY MANAGET PID             
*                                                                               
DRPIDLP  DS    0H                                                               
*                                                                               
         LR    R5,R2               SAVE FIELD POINTER                           
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
         BRAS  RE,BUMP             BUMP TO NAME FIELD                           
         BRAS  RE,CLRFLD           CLEAR IT                                     
*                                                                               
         LTR   R6,R6               SKIP IF NO SECURITY ELEMENT                  
         BZ    DRPIDCN                                                          
*                                                                               
         OC    0(L'GRPSPID,R3),0(R3) SKIP IF NO PID                             
         BZ    DRPIDCN                                                          
*                                                                               
         LR    R2,R5               RESTORE FIELD POINTER                        
*                                                                               
         GOTOR TRNPID,DMCB,0(R3)   DISLAY PID                                   
*                                                                               
DRPIDCN  DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT UNPROTECTED                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT PID                             
         BCT   R0,DRPIDLP                                                       
*                                                                               
DRPIDDN  DS    0H                                                               
*                                                                               
DRPIDX   DS    0H                                                               
*                                                                               
DRACTV   DS    0H                  DISPLAY ACTIVITY                             
*                                                                               
         LA    R2,SFMDTEUH         POINT TO LAST CHANGED DATE                   
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
*                                                                               
         LA    R2,SFMPIDUH         POINT TO LAST CHANGED PID                    
*                                                                               
         BRAS  RE,CLRFLD           CLEAR THE FIELD                              
         BRAS  RE,BUMP             BUMP TO NEXT FIELD                           
         BRAS  RE,CLRFLD           CLEAR NAME FIELD                             
*                                                                               
         MVI   ELCODE,X'F1'        SET ACTIVITY ELEMENT CODE                    
         L     R6,AIO              POINT TO RECORD                              
         BRAS  RE,GETEL            FIND ACTIVITY ELEMENT                        
         BNE   DRACTVX             SKIP IF NONE FOUND                           
*                                                                               
         USING ACTVD,R6            ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         LA    R3,ACTVCHDT         ASSUME LAST CHANGED DATE                     
         OC    ACTVCHDT,ACTVCHDT   IF NO LAST CHANGED DATE                      
         BNZ   *+8                                                              
         LA    R3,ACTVADDT            USE ADDED DATE                            
*                                                                               
         OC    0(3,R3),0(R3)       SKIP IF NO DATE AVAILABLE                    
         BZ    DRACTVX                                                          
*                                                                               
         GOTOR DATCON,DMCB,(3,0(R3)),(17,SFMDTEU) DISP DATE                     
*                                                                               
         LA    R2,SFMPIDUH         POINT TO LAST CHANGED FIELD                  
*                                                                               
         LA    R3,ACTVCHID         ASSUME LAST CHANGED PID                      
         OC    ACTVCHID,ACTVCHID   IF NO LAST CHANGED PID                       
         BNZ   *+8                                                              
         LA    R3,ACTVADID            USE ADDED PID                             
*                                                                               
         GOTOR TRNPID,DMCB,0(R3)   DISPLAY PID                                  
*                                                                               
DRACTVX  DS    0H                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING GRPKEY,R6                                                        
*                                                                               
         MVI   BKFRMSEL,C'Y'       TO TELL LISTREC HAVE BEEN HERE               
*                                                                               
         MVC   SFMMED,QMED         MEDIA                                        
         OI    SFMMEDH+6,X'80'                                                  
*                                                                               
         XC    SFMID,SFMID                                                      
         MVC   SFMID(1),GRPKID     GROUP ID                                     
         OI    SFMIDH+6,X'80'                                                   
*                                                                               
         CLI   RECNUM,36           CLIENT GROUP DEFN ?                          
         BNE   DKX                                                              
*                                                                               
* TRANSLATE 1 CHAR CODE TO DISPLAYABLE 2 CHARS                                  
*                                                                               
         LA    RE,SPCGRTAB                                                      
         LHI   RF,SPCGRTBX-SPCGRTAB                                             
*                                                                               
DK12     CLC   GRPKID,2(RE)                                                     
         BE    DK14                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,DK12                                                          
         B     BADGRPID                                                         
*                                                                               
DK14     MVC   SFMID,0(RE)         SET ID CODE FOR DISPLAY                      
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY                                                           
         USING GRPKEY,R4                                                        
*                                                                               
         OC    KEY(25),KEY         FIRST TIME THROUGH?                          
         BNZ   LR30                                                             
*                                                                               
LR2      LA    R0,SORTDATA                                                      
         LHI   R1,SORTDATX-SORTDATA                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   SORTLAST,0                                                       
         LA    R5,SORTDATA                                                      
*                                                                               
         MVI   GRPKRCOD,GRPKCTYQ                                                
         CLI   RECNUM,36           CLIENT GROUP DEFINITION?                     
         BE    LR10                                                             
*                                                                               
         MVI   GRPKRCOD,GRPKBTYQ                                                
         CLI   RECNUM,42           PUB GROUP DEFINITION?                        
         BE    LR10                                                             
         DC    H'0'                HOW DID WE GET HERE?                         
*                                                                               
LR10     MVC   GRPKAGY,AGENCY      AGY/MED                                      
         MVC   GRPKMED,QMED                                                     
         MVC   SAVEKEY,KEY         SAVE THE LIST FILTER                         
*                                                                               
LR12     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(4),SAVEKEY      SAME TYPE/AGY/MED?                           
         BNE   LR20                                                             
         OC    KEY+8(9),KEY+8      GROUP DESCRIPTION RECORD?                    
         BZ    *+6                                                              
         DC    H'0'                                                             
* TRANSLATE 1 CHAR CODE TO 2 CHARS                                              
         LA    RE,SPCGRTAB                                                      
         LHI   RF,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
LR14     CLC   GRPKID,2(RE)                                                     
         BE    LR16                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,LR14                                                          
         DC    H'0'                                                             
*                                                                               
LR16     CLC   0(2,RE),SFMID       COMPARE TO START VALUE                       
         BL    LR18                                                             
         MVC   0(3,R5),0(RE)       SAVE BOTH CODES                              
         AHI   R5,3                                                             
*                                                                               
LR18     MVC   KEY+8(2),=2X'FF'    FORCE NEXT GROUP                             
         B     LR12                                                             
*                                                                               
LR20     LR    R1,R5                                                            
         LA    R0,SORTDATA                                                      
         SR    R1,R0               GIVES LENGTH USED                            
         BZ    LRX                                                              
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LR    R0,R1                                                            
*                                                                               
         GOTO1 QSORT,DMCB,SORTDATA,(R0),3,2,0                                   
         EJECT                                                                  
*=============================================================                  
* NOW DISPLAY DATA FROM SORTED LIST                                             
*=============================================================                  
*                                                                               
LR30     DS    0H                                                               
         CLI   BKFRMSEL,C'Y'       BACK FROM SELECT ?                           
         BNE   LR31                NO                                           
         MVI   BKFRMSEL,C' '       CLEAR                                        
         OC    SORTFRST,SORTFRST   ANYTHING HERE ?                              
         BZ    LR31                NO                                           
         MVC   SORTLAST,SORTFRST   REPEAT SAME SCREEN                           
*                                                                               
LR31     XC    SORTFRST,SORTFRST                                                
         LA    R5,SORTDATA         FIND LAST ENTRY DISPLAYED                    
         LA    R5,SORTDATA         FIND LAST ENTRY DISPLAYED                    
         LHI   R0,(SORTDATX-SORTDATA)/3  LOOP PREVENTION                        
         CLI   SORTLAST,0                                                       
         BE    LR34                                                             
*                                                                               
LR32     CLC   SORTLAST,2(R5)                                                   
         BE    LR34                                                             
         AHI   R5,3                                                             
         BCT   R0,LR32                                                          
         DC    H'0'                                                             
*                                                                               
LR34     MVC   SORTLAST,2(R5)      SET START POINT                              
*                                                                               
         OC    0(3,R5),0(R5)       TEST MORE DATA                               
         BZ    LRX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SAVEKEY                                                      
         MVC   GRPKID,2(R5)        MOVE 1 CHAR CODE                             
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         MVC   LSTID,0(R5)         2 CHAR GROUP ID                              
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   LSTBK1,GRPBK1       BREAK 1                                      
         MVI   LSTOPRN1,C'('                                                    
         MVC   LSTBKLN1,GRPBK1LN                                                
         OI    LSTBKLN1,X'F0'                                                   
         MVI   LSTCPRN1,C')'                                                    
*                                                                               
         CLI   GRPBK2LN,0          BREAK 2 (MAY NOT BE THERE)                   
         BE    LR38                                                             
         MVC   LSTBK2,GRPBK2                                                    
         MVI   LSTOPRN2,C'('                                                    
         MVC   LSTBKLN2,GRPBK2LN                                                
         OI    LSTBKLN2,X'F0'                                                   
         MVI   LSTCPRN2,C')'                                                    
         DROP  R6                                                               
*                                                                               
LR38     DS    0H                                                               
         OC    SORTFRST,SORTFRST   ANYTHING THERE ?                             
         BNZ   *+10                YES - LEAVE ALONE                            
         MVC   SORTFRST,SORTLAST   SET "FIRST LINE" POINTER                     
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
*                                                                               
         AHI   R5,3                                                             
         OC    0(3,R5),0(R5)                                                    
         BNZ   LR34                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
BADLENG  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADLENGM),BADLENGM                                     
         GOTO1 ERREX2                                                           
BADLENGM DC    C'* ERROR * LENGTH MUST BE WITHIN RANGE 1 TO 4'                  
         SPACE 2                                                                
BADGRPID XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADGRPM),BADGRPM                                       
         GOTO1 ERREX2                                                           
BADGRPM  DC    C'* ERROR * GROUP ID IS NOT VALID'                               
         SPACE 2                                                                
NOLENG   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOLENGM),NOLENGM                                       
         GOTO1 ERREX2                                                           
NOLENGM  DC    C'* ERROR * LENGTH INVALID WITHOUT BREAK NAME'                   
         SPACE 2                                                                
BADSUML  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADSUMLM),BADSUMLM                                     
         GOTO1 ERREX2                                                           
BADSUMLM DC    C'* ERROR * SUM OF BREAK LENGTHS MAY NOT EXCEED 4'               
         SPACE 2                                                                
NOCHANGE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCHANGM),NOCHANGM                                     
         GOTO1 ERREX2                                                           
NOCHANGM DC    C'* ERROR * BREAK LENGTHS CANNOT BE CHANGED'                     
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM25 - PRINT WEB IO CONTROLLER - CLRFLD'                     
***********************************************************************         
*                                                                     *         
*        CLEARS A FIELD ON SCREEN AND FORCES RE-TRANSMITTAL           *         
*                                                                     *         
*NTRY   R2==>   FIELD ON SCREEN                                       *         
*                                                                     *         
*EXIT    FIELD CLEARED TO NULLS                                       *         
*        FIELD SET TO BE RE-TRANSMITTED                               *         
*        OUTPUT DATA LENGTH SET TO MAXIMUM                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLRFLD   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD ON SCREEN                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         STC   RF,FLDOLEN          SET MAX OUTPUT LENGTH                        
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR FIELD                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
CLRFLDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM25 - PRINT WEB IO CONTROLLER - BUMP'                       
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUMP TO NEXT FIELD ON SCREEN                      *         
*                                                                     *         
*              BUMP -  NEXT FIELD                                     *         
*              BUMPU - NEXT UNPROTECTED FIELD                         *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*NTRY    R2==> CURRENT FIELD                                          *         
*                                                                     *         
*EXIT    R2==> NEXT (UNPROTECTED) FIELD                               *         
*        CC    NEQ - NOT END OF SCREEN                                *         
*              EQ  - END OF SCREEN                                    *         
*                                                                     *         
*                                                                     *         
*NOTE: RF DESTROYED                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUMP     DS    0H                  BUMP TO NEXT FIELD                           
         SR    RF,RF                                                            
         ICM   RF,1,0(R2)          GET LENGTH OF TWA FIELD                      
         AR    R2,RF               POINT TO NEXT FIELD                          
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
*        THIS VERSION BUMPS TO NEXT UNPROTECTED FIELD                           
*                                                                               
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BER   RE                                                               
*                                                                               
         TM    1(R2),X'20'         IF PROTECTED FIELD                           
         JNZ   BUMPU                  GO TO NEXT FIELD                          
*                                                                               
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         TITLE 'PRSFM25 - PRINT WEB IO CONTROLLER - CLRSCRN'                    
***********************************************************************         
*                                                                     *         
*        ROUTINE TO CLEAR FIELDS TO END OF SCREEN                     *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*NTRY    R2==> STARTING FIELD                                         *         
*                                                                     *         
*EXIT    ALL UNPROTECTED FIELDS ARE CLEARED TO END OF SCREEN          *         
*                                                                     *         
*NOTE: RF DESTROYED                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
CLRSCRLP DS    0H                                                               
*                                                                               
         BRAS  RE,CLRFLD           CLEAR UNPROTECTED FIELD                      
*                                                                               
CLRSCRCN DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BZ    CLRSCRLP            MORE FIELDS ON SCREEN                        
*                                                                               
CLRSCRDN DS    0H                                                               
*                                                                               
CLRSCRNX DS    0H                  ALL DONE                                     
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C25 - INVOICE COMMENTS MAINT/LIST - TRNPID'                  
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1       A(PID)                                              *         
*                                                                     *         
*EXIT    PUTS PERSONAL ID IN FIRST FIELD                              *         
*        BUMPS TO NEXT FIELD                                          *         
*        PUTS NAME IN THIS FIELD                                      *         
*                                                                     *         
*NOTE    MORE EXTENSIVE THAN ROUTINE IN OTHER MODULES                 *         
*        THEY JUST DISPLAY THE NAME                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNPID   NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         L     R5,0(R1)            POINT TO PID                                 
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
         LTR   R2,R2               IF R2 GIVEN                                  
         BZ    *+8                                                              
         BRAS  RE,CLRFLD              INIT OUTPUT                               
*                                                                               
         LA    R4,KEY                                                           
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   CT0KNUM,0(R5)       SET PID                                      
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
         MVC   AIO,AIO1            RESET IOAREA                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TPIDNOTF                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
*        DISPLAY PERSON ID                                                      
*                                                                               
         MVC   FLDDATA(8),2(RE)    USER'S PID                                   
*                                                                               
         LR    R0,RE               SAVE POINTER                                 
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         LR    RE,R0               RESTORE POINTER                              
*                                                                               
         LA    R3,FLDDATA          USE DATAAREA OF SCREEN FIELD                 
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,2(RE)       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
         MVC   AIO,AIO1            RESET IOAREA                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE SKIP IF REC NOT FOUND           
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND NAME ELEMENT                                                      
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENOT FOUND                          
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WORK             BUILD NAME IN WORKAREA                       
         XC    WORK,WORK                                                        
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,WORK+7           POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WORK             START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,(R0) SQUASH NAME                              
*                                                                               
*        MOVE NAME TO SCREEN                                                    
*                                                                               
         LTR   R2,R2               IF NO SCREEN FIELD GIVEN                     
         BNZ   TPIDSCR                                                          
*                                                                               
         LR    RF,R6                  GET RETURN AREA LENGTH                    
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES         INIT OUT PUT AREA                         
*                                                                               
         L     RF,4(R1)               SAVE SQUASHED LENGTH                      
*                                                                               
         CR    RF,R6                  IF NAME TOO LONG                          
         BNH   *+6                                                              
         LR    RF,R6                     USE MAX FOR RETURN AREA                
*                                                                               
         B     TPIDMVC                                                          
*                                                                               
TPIDSCR  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         STC   RF,FLDOLEN          SET MAX OUTPUT LENGTH                        
*                                                                               
TPIDMVC  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY NAME                                 
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C25 - INVOICE COMMENTS MAINT/LIST - VALPID'                  
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1       A(PID SAVEAREA)                                     *         
*                                                                     *         
*EXIT    VALIDATES PID IN SCREEN FIELD                                *         
*        BUMPS TO NEXT FIELD                                          *         
*        PUTS NAME IN THIS FIELD                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALPID   NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         L     R5,0(R1)            POINT TO PID SAVEAREA                        
*                                                                               
         CLI   FLDILEN,0           SKIP IF NO PID ENTERED                       
         BZ    VALPIDX                                                          
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,SPACES      INIT PID                                     
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDILEN          GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SAPEPID(0),FLDDATA  MOVE PID TO KEY                              
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
         MVC   AIO,AIO1            RESET IOAREA                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE MUST FIND RECORD                
         BNE   VPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND PASSWORD ELEMENT                                                  
*                                                                               
VPIDPWLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    VPIDPWDN                                                         
*                                                                               
         USING SAPWDD,RE           ESTABLISH AS PWD ELEMENT                     
*                                                                               
         CLI   SAPWDEL,SAPWDELQ    LOOKING FOR PWD ELEMENT                      
         BE    VPIDPWFD                                                         
*                                                                               
VPIDPWCN DS    0H                                                               
*                                                                               
         IC    RF,SAPWDLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     VPIDPWLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
VPIDPWDN DS    0H                  PWD ELEMENT NOT FOUND                        
*                                                                               
         B     VPIDNOTF                                                         
*                                                                               
VPIDPWFD DS    0H                                                               
*                                                                               
         MVC   0(L'SAPWDNUM,R5),SAPWDNUM RETURN PID                             
*                                                                               
VALPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
VPIDNOTF DS    0H                  INVALID PID                                  
*                                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT WEB IO CONTROLLER - PID'                        
***********************************************************************         
*   PID - THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK               *         
*         WHICH ARE "PERSONAL ID"                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
PID      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         XC    SVSFMPID,SVSFMPID   PASSWORD ID NUMBER CLEARED                   
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
         DROP  RF                                                               
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         MVC   SVSECAGY,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
*                                                                               
         TM    FATFLAG,X'08'       CHECK IF SECRET CODE IS THERE                
         BZ    *+10                                                             
         MVC   SVSFMPID,FAPASSWD   SAVE PASSWORD ID NUMBER                      
*                                                                               
PIDX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT NEW INSORD CONTROLLER - DISTIM'                 
***********************************************************************         
*                                                                     *         
*        DISPLAYS BINARY XL3 TIME AS HH:MM:SS                         *         
*                                                                     *         
*NTRY   R2==>   FIELD ON SCREEN                                       *         
*       P0      A(BINARY TIME FIELD)                                  *         
*                                                                     *         
*EXIT           TIME AS HH:MM:SS                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DISTIM   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD ON SCREEN                    
*                                                                               
         L     R3,0(R1)            SAVE PARAMETER LIST POINTER                  
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         IC    RF,0(R3)            GET HOURS                                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FLDDATA(2),DUB                                                   
         MVI   FLDDATA+2,C':'                                                   
*                                                                               
         IC    RF,1(R3)            GET MINUTES                                  
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FLDDATA+3(2),DUB                                                 
         MVI   FLDDATA+5,C':'                                                   
*                                                                               
         IC    RF,2(R3)            GET SECONDS                                  
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FLDDATA+6(2),DUB                                                 
*                                                                               
DISTIMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPCGRTAB                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*SEACSFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMB1D                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
* PROGRAM SAVED STORAGE AT BOTTOM OF TWA0                                       
*                                                                               
         ORG   T41CFFD+TWAENDLQ    ORG TO BOTTOM OF TWA0                        
*                                                                               
STSAVE   EQU   *                                                                
SECBLK   DS    CL1024              SECRET PARAMETER BLOCK                       
*                                                                               
SVSPARE  DS    CL(TWAMXLEN-(*-STSAVE))  SPARE - DEFINE NEW AREAS ABOVE          
*                                                                               
SVSPARED DSECT                     DATA SAVED IN TWA                            
*                                                                               
BKFRMSEL DS    CL1                 C'Y' = SELECT FROM LIST OCCURRED             
SORTFRST DS    XL1                 FIRST 1-BYTE CODE DISPLAYED                  
SORTLAST DS    XL1                 LAST  1-BYTE CODE DISPLAYED                  
SORTDATA DS    XL768               MAX 256 3 BYTE CODES                         
SORTDATX EQU   *                                                                
*                                                                               
SVSPAREL EQU   *-SVSPARED                                                       
*                                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 5                                                                
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SAVEKEY  DS    XL48                                                             
SVSECAGY DS    XL2                 SECURITY AGENCY                              
SVSECELM DS    XL256               SECURITY ELEMENT SAVEAREA                    
*                                                                               
         EJECT                                                                  
       ++INCLUDE PGENGRP                                                        
         EJECT                                                                  
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTID    DS    CL2                                                              
         DS    CL3                                                              
LSTBK1   DS    CL12                                                             
         DS    C                                                                
LSTOPRN1 DS    C                                                                
LSTBKLN1 DS    C                                                                
LSTCPRN1 DS    C                                                                
         DS    CL3                                                              
LSTBK2   DS    CL12                                                             
         DS    C                                                                
LSTOPRN2 DS    C                                                                
LSTBKLN2 DS    C                                                                
LSTCPRN2 DS    C                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014PRSFM25   09/06/06'                                      
         END                                                                    
