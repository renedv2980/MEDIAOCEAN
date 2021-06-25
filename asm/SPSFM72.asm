*          DATA SET SPSFM72    AT LEVEL 093 AS OF 12/18/18                      
*PHASE T21772C                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21772 - MAINTENANCE/LIST OF STANDARD COMMENTS                        
*                                                                               
*  COMMENTS: MAINTAINS STANDARD COMMENTS                                        
*                                                                               
*  CALLED FROM: ADDS CONTROLLER (T23400), WHICH CALLS                           
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS SCSFM32 (T29732) -- MAINTENANCE                              
*                  SCSFM33 (T29733) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW STANDARD COMMENTS                                    
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - POINTS TO THE OVERLAY STORAGE AREA DSECT                        
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - WORK                                                            
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM                                                          
*          RF - SYSTEM                                                          
*                                                                               
***********************************************************************         
T21772   TITLE 'SPOMS06 - MAINTENANCE OF STANDARD COMMENTS'                     
T21772   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21772*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         GOTO1 INITPFKY,DMCB,0     INITIALIZE WITHOUT PFKEYS                    
         CLI   PFKEY,12            IF USING PF12                                
         BNE   MAIN10              TO RETURN TO THE LIST,                       
         CLI   MODE,LISTRECS                                                    
         BE    MAIN10                                                           
         GOTO1 INITPFKY,DMCB,PFTABLE   INITIALIZE PFKEYS                        
MAIN10   XC    ACURSOR,ACURSOR                                                  
*                                                                               
         OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         CLI   MODE,XRECADD        IF RECORD JUST ADDED,                        
         BNE   MAIN20                                                           
         MVC   CONACT,=CL8'CHANGE' FORCE ACTION TO CHANGE                       
*                                                                               
MAIN20   CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R4,SVKEY                                                         
         USING COMKEY,R4                                                        
         MVI   COMKTYP,COMKTYPQ    TYPE                                         
         MVI   COMKSUB,COMKSUBQ    SUB-TYPE                                     
*                                                                               
         LA    R2,COMMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
         MVC   COMKAM,BAGYMD                                                    
*                                                                               
         LA    R2,COMIDH           VALIDATE COMMENT ID                          
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKX                                                              
         B     MISSFLD             MISSING INPUT                                
*                                                                               
VK10     OC    COMID,SPACES        BLANK PADDED                                 
         MVC   COMKCOM,COMID                                                    
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
*                                                                               
         MVI   PAGEFLG,1           ALWAYS START WITH PAGE 1                     
*                                                                               
         MVC   PREVKEY,KEY         SAVE FOR LIST SELECT                         
         MVI   PREVFLAG,1                                                       
         L     R6,AIO              SAVE AIO                                     
         ST    R6,AIOSAVE                                                       
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         LA    R4,SVKEY                                                         
         USING COMKEY,R4                                                        
*                                                                               
         MVI   RDUPDATE,C'N'       READ ONLY                                    
         GOTO1 READ                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DK10     BAS   RE,NEXTEL                                                        
         BNE   DKX                                                              
         CLC   COMKAM,3(R6)                                                     
         BNE   DK10                                                             
*                                                                               
         MVC   COMMED,2(R6)                                                     
         LA    R2,COMMEDH                                                       
         FOUT  (R2)                MOVE MEDIA TO SCREEN                         
*                                                                               
         MVC   COMID,COMKCOM                                                    
         LA    R2,COMIDH                                                        
         FOUT  (R2)                MOVE REF. NUM TO SCREEN                      
*                                                                               
DKX      DS    0H                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
*                                                                               
         L     R4,AIO                                                           
         USING COMRECD,R4                                                       
         LA    R2,COMTXTH          FIRST COMMENT TEXT FIELD                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,COMTXTEQ     COMMENT TEXT ELEMENT CODE                    
         BAS   RE,GETEL            ANY TEXT FIELDS?                             
         BE    *+6                                                              
         DC    H'00'               MUST BE SOME TEXT                            
*                                                                               
         CLI   PAGEFLG,1                                                        
         BH    DR30                                                             
*                                                                               
         USING COMTXTD,R6                                                       
         MVI   PAGEFLG,1           PAGE 1                                       
         SR    R7,R7               LINE COUNTER                                 
         B     DR20                                                             
*                                                                               
         ZIC   R7,COUNT                                                         
DR20     BAS   RE,FILLSCRN         FILL SCREEN WITH COMMENTS                    
*                                                                               
DR30     BAS   RE,SETSCRN          IF PFKEY HIT, GET NEXT/PREV COMMENTS         
         BNE   DRX                                                              
*                                                                               
         ZIC   R7,COUNT                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,COMTXTEQ     COMMENT TEXT ELEMENT CODE                    
         BAS   RE,GETEL            ANY TEXT FIELDS?                             
         BE    *+6                                                              
         DC    H'00'               MUST BE SOME TEXT                            
DR40     CLC   COMTXTSQ,COUNT      SEQUENCE NUMBER MUST BE >= COUNT             
         BH    DR50                                                             
         BAS   RE,NEXTEL                                                        
         BE    DR40                                                             
*                                                                               
DR50     BAS   RE,FILLSCRN                                                      
*                                                                               
DRX      MVC   ACURFORC,ACURSOR    PFKEY WAS HIT -- PLACE CURSOR                
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT,                         
         BNE   EXIT                                                             
         BAS   RE,FORCEACT         FORCE ACTION TO DISPLAY,CHA,OR DEL           
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         OI    GENSTAT2,NEXTSEL                                                 
         CLI   PFKEY,3             ERASE LINE?                                  
         BE    *+12                                                             
         CLI   PFKEY,4             ADD LINE?                                    
         BNE   VR100                                                            
*                                                                               
         L     R1,SYSPARMS         A(TIOB)                                      
         L     R7,0(R1)                                                         
         USING TIOBD,R7                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       ABSOLUTE CURSOR ADDRESS                      
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         MH    R1,=H'80'           ABSOLUTE ADDR OF BEGINNING OF LINE           
         DROP  R7                                                               
*                                                                               
         LA    R2,COMFILLH         1ST FIELD WHICH COULD CONTAIN CURSOR         
VR10     SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         LA    RE,79(RF)           ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
         CR    RF,R1               WAS CURSOR ON THIS LINE?                     
         BH    VR100               NO - IT'S ABOVE THIS FIELD                   
         CR    RE,R1                                                            
         BNL   VR20                YES                                          
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         LA    RF,COMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR100               YES                                          
         B     VR10                                                             
*                                                                               
VR20     LA    RF,COMTXTLH         A(LAST TEXT FIELD)                           
         CLI   PFKEY,3             ERASE LINE?                                  
         BNE   VR50                NO, ADD LINE                                 
*                                                                               
         LA    R0,COMFILLH                                                      
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BE    VR100               YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
VR30     CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR40                YES                                          
         ZIC   R0,0(R2)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'COMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R2,R3                                                            
         B     VR30                                                             
*                                                                               
VR40     XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'COMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR LAST TEXT FIELD                        
         CLI   PAGEFLG,4           IF PAGES 1,2,3 MOVE UP NEXT PAGES            
         BE    VR100                                                            
*                                                                               
         BAS   RE,SHIFTUP          SHIFT ELEMENTS UP                            
         B     VR100                                                            
*                                                                               
VR50     CR    R2,RF               ARE THEY TRYING TO INSERT AFTER END?         
         BE    VR100               YES                                          
*                                                                               
         LR    RF,R2               SAVE A(INSERTION)                            
         LA    R3,COMTXTLH         LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VR60     ZIC   R0,0(R2)                                                         
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR70                YES                                          
         LA    R1,L'COMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R3,R2                                                            
         B     VR60                                                             
*                                                                               
VR70     XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'COMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR TEXT FIELD (INSERT BLANK LINE)         
         ST    R3,ACURSOR          KEEP CURSOR IN PLACE                         
         CLI   PAGEFLG,4           IF PAGES 1,2,3 MOVE DOWN TO NEXT PG          
         BE    VR100                                                            
         BAS   RE,SHIFTDWN         SHIFT ELEMENTS DOWN                          
                                                                                
         EJECT                                                                  
VR100    L     R6,AIO              A(COMMENT RECORD)                            
         USING COMKEY,R6                                                        
*                                                                               
         LA    R2,COMTXTH          FIRST TEXT FIELD                             
         MVI   TXTFOUND,C'N'       NO TEXT FOUND YET                            
         BAS   RE,SETSEQN          SET START AND END SEQ OF PAGE NUMBER         
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR110                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING COMTXTD,R6                                                       
         MVI   ELCODE,COMTXTEQ     COMMENT TEXT ELEMENT CODE                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR105    BAS   RE,NEXTEL                                                        
         BNE   VR108                                                            
         CLC   COMTXTSQ,SEQNUM     MUST BE A SEQ ON THE PAGE DISPLAYED          
         BL    VR105                                                            
         CLC   COMTXTSQ,ENDNUM                                                  
         BH    VR105                                                            
         MVI   COMTXTEL,X'FF'      MARK ELCODE FOR DELETION                     
         B     VR105                                                            
                                                                                
VR108    MVI   ELCODE,X'FF'        MARKED COMMENT ELEMENT CODE                  
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         DROP  R6                                                               
*                                                                               
VR110    LA    R4,ELEM                                                          
         USING COMTXTD,R4                                                       
*                                                                               
VR120    LA    RF,COMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VRX                 YES                                          
         MVI   BLANKLN,C'Y'                                                     
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   COMTXTEL,COMTXTEQ   COMMENT TEXT LINE ELEMENT CODE               
         MVC   COMTXTSQ,SEQNUM     SEQUENCE NUMBER                              
*                                                                               
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         LTR   R1,R1               NO INPUT ON THIS LINE?                       
         BZ    VR130                                                            
         MVI   TXTFOUND,C'Y'       SOME TEXT WAS FOUND                          
         MVI   BLANKLN,C'N'        SOME TEXT WAS FOUND                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COMTXTTX(0),8(R2)   COMMENT TEXT LINE                            
         LA    R3,COMTXTOV         LENGTH OF ELEMENT OVERHEAD                   
         LA    R1,1(R3,R1)         TOTAL LENGTH OF ELEMENT                      
         B     VR150                                                            
*                                                                               
VR130    ST    R2,MYWORK           HANG ON TO CURRENT TWA POINTER               
         LA    RF,COMTXTLH                                                      
*                                                                               
VR140    ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         CR    R2,RF               END OF SCREEN?                               
         BNH   *+12                NO                                           
         L     R2,MYWORK                                                        
         B     VRX                                                              
*                                                                               
         CLI   5(R2),0             ANY INPUT THIS FIELD?                        
         BE    VR140               TRY NEXT FIELD                               
         L     R2,MYWORK                                                        
         B     VR160                                                            
*                                                                               
*        MVI   COMTXTTX,C' '       MUST SAVE A BLANK LINE                       
*        LA    R1,COMTXTOV                                                      
*        LA    R1,1(R1)            TOTAL LENGTH OF ELEMENT                      
*                                                                               
VR150    STC   R1,COMTXTLN                                                      
*                                                                               
VR160    ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         ZIC   RF,SEQNUM           INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
*                                                                               
         CLI   BLANKLN,C'Y'        DON'T ADD BLANK LINE                         
         BE    VR120                                                            
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VR120                                                            
         DC    H'0'                                                             
*                                                                               
VRX      CLI   PAGEFLG,1           IF PAGE 1,                                   
         BH    VRXX                                                             
         CLI   TXTFOUND,C'Y'       MAKE SURE SOME TEXT WAS FOUND                
         BNE   NOTXTERR                                                         
*                                                                               
         DROP  R4                                                               
VRXX     B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       LA    R4,KEY                                                           
         USING COMKEY,R4                                                        
*                                                                               
         CLI   PREVFLAG,0          FIRST TIME THRU??                            
         BE    LR05                                                             
*                                                                               
         MVC   KEY,PREVKEY                                                      
         XC    PREVFLAG,PREVFLAG                                                
         MVC   COMID,SVCOMM                                                     
         OI    COMIDH+6,X'80'                                                   
         B     LR10                                                             
*                                                                               
LR05     MVI   COMKTYP,COMKTYPQ    TYPE      '0D'                               
         MVI   COMKSUB,COMKSUBQ    SUB-TYPE  '33'                               
         MVC   COMKAM,BAGYMD                                                    
*                                                                               
LR10     MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
LR20     CLC   KEY(3),SAVEKEY      SAME TYPE, A/M ?                             
         BNE   LRX                                                              
*                                                                               
         MVC   SVCOMM,COMID        SAVE FILTER                                  
         OC    COMID,COMID         FILTER ON COMMENT CODE??                     
         BZ    LR40                                                             
         ZIC   R1,COMIDH+5         YES                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   COMKCOM(0),COMID         FILTER ON AGENCY ID                     
         BNE   LR60                                                             
*                                                                               
*                                                                               
LR40     GOTO1 GETREC                                                           
*                                                                               
         MVC   LISTAR,SPACES       FILL IN LIST LINE                            
         MVC   LSTCMCD,COMKCOM     MOVE IN COMMENT CODE                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,COMTXTEQ     TEXT LINE ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                 LOOK FOR FIRST TEXT LINE ELEMENT             
         DC    H'00'               MUST BE THERE                                
*                                                                               
         USING COMTXTD,R6                                                       
         MVC   MYWORK,SPACES                                                    
         ZIC   R3,COMTXTLN         TOTAL LENGTH OF ELEMENT                      
         LA    R1,COMTXTOV         LENGTH OF OVERHEAD                           
         SR    R3,R1               LENGTH OF TEXT                               
         C     R3,=F'1'            TEXT LENGTH 1?                               
         BNE   *+12                NO                                           
         CLI   COMTXTTX,C' '       BLANK LINE?                                  
         BE    LR55                YES - LEAVE THIS FIELD ALONE                 
         LA    R1,COMTXTTX         A(TEXT)                                      
*                                                                               
LR45     CLI   0(R1),C' '          IGNORE LEADING SPACES                        
         BNE   LR50                                                             
         BCTR  R3,0                SUBTRACT 1 FROM MOVE LENGTH                  
         LA    R1,1(R1)                                                         
         B     LR45                LOOK AT NEXT CHARACTER                       
*                                                                               
LR50     BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK(0),0(R1)     TEXT IS LEFT-JUSTIFIED                       
         MVC   LSTTEXT,MYWORK      AND PADDED WITH SPACES                       
*                                                                               
LR55     GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LR60     GOTO1 SEQ                 NEXT COMMENT RECORD                          
         LA    R4,KEY              POINT R4 BACK TO KEY                         
         B     LR20                                                             
         DROP  R4,R6                                                            
LRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FILL THE SCREEN WITH COMMENTS                                                 
***********************************************************************         
         USING COMRECD,R4                                                       
         USING COMTXTD,R6                                                       
FILLSCRN NTR1                                                                   
*                                                                               
FSCRN10  MVC   8(L'COMTXT,R2),SPACES                                            
         LA    R7,1(R7)            INCREMENT LINE COUNTER                       
         ZIC   R1,COMTXTSQ         SEQUENCE NUMBER                              
         CR    R1,R7               IF NOT EQUAL, PRINT BLANK LINE               
         BNE   FSCRN15                                                          
*                                                                               
         ZIC   R1,COMTXTLN         LENGTH OF ELEMENT                            
         LA    R3,COMTXTOV         OVERHEAD LENGTH                              
         SR    R1,R3               LENGTH OF TEXT                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),COMTXTTX    LINE OF TEXT                                 
FSCRN15  OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT TEXT FIELD                              
         AR    R2,R0                                                            
*                                                                               
         LA    RF,COMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    FSCRN30             YES                                          
*                                                                               
         ZIC   R1,COMTXTSQ         SEQUENCE NUMBER                              
         CR    R1,R7               IF NOT EQUAL, PRINT BLANK LINE               
         BNE   FSCRN10                                                          
*                                                                               
         BAS   RE,NEXTEL           NEXT LINE OF TEXT                            
         BE    FSCRN10                                                          
*                                                                               
FSCRN20  ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'            LENGTH OF DATA + 1 (FOR EX)                  
         TM    1(R2),X'02'         TEXT EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT REMAINING FIELDS                   
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
*                                                                               
         LA    RF,COMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BNH   FSCRN20             NO                                           
*                                                                               
FSCRN30  CLI   PAGEFLG,1           PAGE 1 - HIDE PF5                            
         BH    FSCRN40                                                          
         OI    COMPF5H+1,X'0C'     MAKE ZERO INTENSITY                          
         OI    COMPF5H+6,X'80'     TRANSMIT                                     
         NI    COMPF6H+1,X'FB'     TURN OFF ZERO INTENSITY                      
         OI    COMPF6H+6,X'80'     TRANSMIT                                     
         B     FSCRNX                                                           
*                                                                               
FSCRN40  CLI   PAGEFLG,4           PAGE 4 - HIDE PF6                            
         BNE   FSCRN50                                                          
         NI    COMPF5H+1,X'FB'     TURN OFF ZERO INTENSITY                      
         OI    COMPF5H+6,X'80'     TRANSMIT                                     
         OI    COMPF6H+1,X'0C'     MAKE ZERO INTENSITY                          
         OI    COMPF6H+6,X'80'     TRANSMIT                                     
         B     FSCRNX                                                           
*                                  PAGES 2 AND 3 - SHOW PF5 AND PF6             
FSCRN50  NI    COMPF5H+1,X'FB'     TURN OFF ZERO INTENSITY                      
         OI    COMPF5H+6,X'80'     TRANSMIT                                     
         NI    COMPF6H+1,X'FB'     TURN OFF ZERO INTENSITY                      
         OI    COMPF6H+6,X'80'     TRANSMIT                                     
FSCRNX   XIT1                                                                   
         DROP  R4,R6                                                            
***********************************************************************         
* GET NEXT OR PREVIOUS PAGE OF COMMENTS IF PF5 OR PF6 PRESSED                   
***********************************************************************         
SETSCRN  NTR1                                                                   
*                                                                               
         CLI   PFKEY,3        IF ERASING LINE OR                                
         BE    SSCRYES                                                          
         CLI   PFKEY,4        ADDING LINE, DISPLAY CURRENT PAGE                 
         BE    SSCRYES                                                          
*                                                                               
         CLI   PAGEFLG,1      IF ON PAGE 1                                      
         BNE   SSCRN10                                                          
         CLI   PFKEY,6        IF NEXT PAGE REQUESTED,                           
         BNE   SSCRNO                                                           
         MVI   PAGEFLG,2      SET TO DISPLAY PAGE 2                             
         MVI   COUNT,17                                                         
         B     SSCRYES                                                          
*                                                                               
SSCRN10  CLI   PAGEFLG,2      IF ON PAGE 2                                      
         BNE   SSCRN20                                                          
         CLI   PFKEY,5        IF PREV PAGE REQUESTED                            
         BNE   SSCRN15                                                          
         MVI   PAGEFLG,1      SET TO DISPLAY PAGE 1                             
         MVI   COUNT,0                                                          
         B     SSCRYES                                                          
SSCRN15  CLI   PFKEY,6        IF NEXT PAGE REQUESTED                            
         BNE   SSCRNO                                                           
         MVI   PAGEFLG,3      SET TO DISPLAY PAGE 3                             
         MVI   COUNT,34                                                         
         B     SSCRYES                                                          
*                                                                               
SSCRN20  CLI   PAGEFLG,3      IF ON PAGE 3                                      
         BNE   SSCRN30                                                          
         CLI   PFKEY,5        IF PREV PAGE REQUESTED                            
         BNE   SSCRN25                                                          
         MVI   PAGEFLG,2      SET TO DISPLAY PAGE 2                             
         MVI   COUNT,17                                                         
         B     SSCRYES                                                          
SSCRN25  CLI   PFKEY,6        IF NEXT PAGE REQUESTED                            
         BNE   SSCRNO                                                           
         MVI   PAGEFLG,4      SET TO DISPLAY PAGE 4                             
         MVI   COUNT,51                                                         
         B     SSCRYES                                                          
*                                                                               
SSCRN30  CLI   PAGEFLG,4      IF ON PAGE 4                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   PFKEY,5        IF PREV PAGE REQUESTED                            
         BNE   SSCRNO                                                           
         MVI   PAGEFLG,3      SET TO DISPLAY PAGE 3                             
         MVI   COUNT,34                                                         
*                                                                               
SSCRYES  MVI   PFKEY,0        CLEAR PFKEY                                       
         XR    RC,RC                                                            
SSCRNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SETS START AND END SEQUENCE NUMBER BASED ON PAGE ON DISPLAY                   
***********************************************************************         
SETSEQN  NTR1                                                                   
*                                                                               
         CLI   ACTNUM,ACTADD  IF ADDING RECORD                                  
         BNE   SSEQN10                                                          
         MVI   PAGEFLG,1      SET TO PAGE 1                                     
*                                                                               
SSEQN10  MVI   SEQNUM,1                                                         
         MVI   ENDNUM,17                                                        
         CLI   PAGEFLG,1      IF ON PAGE 1                                      
         BE    SSEQNX                                                           
*                                                                               
         MVI   SEQNUM,18                                                        
         MVI   ENDNUM,34                                                        
         CLI   PAGEFLG,2      IF ON PAGE 2                                      
         BE    SSEQNX                                                           
*                                                                               
         MVI   SEQNUM,35                                                        
         MVI   ENDNUM,51                                                        
         CLI   PAGEFLG,3      IF ON PAGE 3                                      
         BE    SSEQNX                                                           
*                                                                               
         MVI   SEQNUM,52                                                        
         MVI   ENDNUM,68                                                        
         CLI   PAGEFLG,4      IF ON PAGE 4                                      
         BE    SSEQNX                                                           
         DC    H'00'                                                            
*                                                                               
SSEQNX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* IF ACTION IS SELECT, FORCE ACTION TO DISPLAY, CHANGE OR DELETE                
***********************************************************************         
FORCEACT NTR1                                                                   
*                                                                               
         CLI   THISLSEL,C'D'  IF DELETING,                                      
         BNE   FRACT10                                                          
         MVI   ACTNUM,ACTDEL                                                    
         MVC   CONACT,=CL8'DELETE'                                              
         OI    CONACTH+5,X'80'                                                  
         B     FRACTX                                                           
*                                                                               
FRACT10  CLI   THISLSEL,C'C'  IF CHANGING,                                      
         BNE   FRACT20                                                          
         MVI   ACTNUM,ACTCHA                                                    
         MVC   CONACT,=CL8'CHANGE'                                              
         OI    CONACTH+5,X'80'                                                  
         B     FRACTX                                                           
*                                                                               
FRACT20  MVI   ACTNUM,ACTDIS  IF DISPLAYING,                                    
         MVC   CONACT,=CL8'DISPLAY'                                             
         OI    CONACTH+5,X'80'                                                  
*                                                                               
FRACTX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SHIFT COMMENT ELEMENT SEQUENCES UP WHEN ERASING A LINE                        
* COUNT SHOULD BE SET IN SETSCRN                                                
* 0(R2) ---> LAST SCREEN FIELD                                                  
***********************************************************************         
SHIFTUP  NTR1                                                                   
*                                                                               
         ZIC   R1,COUNT            START WITH COUNT ON NEXT PAGE                
         AHI   R1,18                                                            
         STC   R1,BYTE                                                          
         L     R6,AIO                                                           
         USING COMTXTD,R6                                                       
         MVI   ELCODE,COMTXTEQ     COMMENT TEXT ELEMENT CODE                    
         BAS   RE,GETEL            ANY TEXT FIELDS?                             
         BE    *+8                                                              
SHFTUP20 BAS   RE,NEXTEL                                                        
         BNE   SHFTUPX                                                          
*                                                                               
         CLC   COMTXTSQ,BYTE       SEQUENCE NUMBER MUST BE >= BYTE              
         BL    SHFTUP20                                                         
         BH    SHFTUP30                                                         
*                                  FIRST LINE OF NEXT SCREEN,                   
         ZIC   R1,COMTXTLN         TOTAL LENGTH OF ELEMENT                      
         LA    R3,COMTXTOV         LENGTH OF OVERHEAD                           
         SR    R1,R3               LENGTH OF TEXT                               
         STC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),COMTXTTX    LINE OF TEXT                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
SHFTUP30 ZIC   R1,COMTXTSQ                                                      
         AHI   R1,-1               DEDUCT 1 FROM SEQUENCE                       
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'00'                                                            
         STC   R1,COMTXTSQ                                                      
         B     SHFTUP20                                                         
*                                                                               
SHFTUPX  CLI   ACTNUM,ACTADD                                                    
         BE    EXIT                                                             
         GOTO1 PUTREC                                                           
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SHIFT COMMENT ELEMENT SEQUENCES DOWN WHEN ADDING A LINE                       
* COUNT SHOULD BE SET IN SETSCRN                                                
* 0(R2) ---> LAST SCREEN FIELD                                                  
***********************************************************************         
SHIFTDWN NTR1                                                                   
*                                                                               
         ZIC   R1,COUNT            START WITH COUNT ON NEXT PAGE                
         AHI   R1,17                                                            
         STC   R1,BYTE                                                          
         L     R6,AIO                                                           
         USING COMTXTD,R6                                                       
         MVI   ELCODE,COMTXTEQ     COMMENT TEXT ELEMENT CODE                    
         BAS   RE,GETEL            ANY TEXT FIELDS?                             
         BE    *+8                                                              
SHFTDN20 BAS   RE,NEXTEL                                                        
         BNE   SHFTDNX                                                          
*                                                                               
         CLC   COMTXTSQ,BYTE       SEQUENCE NUMBER MUST BE >= BYTE              
         BL    SHFTDN20                                                         
         ZIC   R1,COMTXTSQ                                                      
         AHI   R1,1                ADD 1 TO SEQUENCE                            
         STC   R1,COMTXTSQ                                                      
         CHI   R1,68               IF SEQUENCE GOES PAST 4TH PAGE,              
         BNH   SHFTDN20                                                         
         MVI   COMTXTEL,X'FF'      MARK ELEMENT FOR DELETION                    
         B     SHFTDN20                                                         
*                                                                               
SHFTDNX  CLI   ACTNUM,ACTADD                                                    
         BE    EXIT                                                             
         GOTO1 PUTREC                                                           
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
INVLREFN MVI   ERROR,INVREFN                                                    
         B     ERREXIT                                                          
*                                                                               
NOTXTERR MVI   ERROR,TXTREQ        AT LEAST ONE LINE OF TEXT REQUIRED           
         B     ERREXIT                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
*                                                                               
PFTABLE  DS    0C                                                               
* RETURN TO COMMENT LIST                                                        
         DC    AL1(PF12X-*,12,0,(PF12X-PF12)/KEYLNQ,0)                          
         DC    CL3' ',CL8'COMMENT',CL8'LIST'                                    
PF12     DC    AL1(KEYTYTWA,L'COMMED-1),AL2(COMMED-T217FFD)                     
         DC    AL1(KEYTYWS,L'SVCOMM-1),AL2(SVCOMM-MYAREAD)                      
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
       ++INCLUDE SPOMSDSCTS                                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPSFMFFD          (BASE SCREEN FOR SYSTEM)                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM32D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM33D          (OUR LIST SCREEN OVERLAY)                    
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
ACURSOR  DS    A                   FORCE CURSOR HERE                            
AIOSAVE  DS    A                                                                
MYWORK   DS    XL96                                                             
SAVEKEY  DS    XL48                                                             
PREVKEY  DS    XL48                                                             
PREVFLAG DS    XL1                                                              
SEQNUM   DS    XL1                 TEXT LINE NUMBER                             
BLANKLN  DS    CL1                 BLANK LINE? Y/N                              
TXTFOUND DS    CL1                 'Y' IF TEXT LINE WAS FOUND                   
PAGEFLG  DS    XL1                 PAGE=1,2,3,4                                 
COUNT    DS    XL1                 COUNT FOR COMMENT LINE                       
ENDNUM   DS    XL1                 END OF SCREEN SEQUENCE NUMBER                
SVCOMM   DS    CL8                 COMMENT FILTER                               
         SPACE                                                                  
*                                                                               
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCMCD  DS    CL8                                                              
         DS    CL3                                                              
LSTTEXT  DS    CL50                                                             
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093SPSFM72   12/18/18'                                      
         END                                                                    
