*          DATA SET SPADD06    AT LEVEL 075 AS OF 05/01/02                      
*PHASE T21206A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21206 - MAINTENANCE/LIST OF STANDARD COMMENTS                        
*                                                                               
*  COMMENTS: MAINTAINS STANDARD COMMENTS                                        
*                                                                               
*  CALLED FROM: ADDS CONTROLLER (T21200), WHICH CALLS                           
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS SPADDF6 (T212F6) -- MAINTENANCE                              
*                  SPADDE6 (T212E6) -- LIST                                     
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
         TITLE 'SPADD06 MAINTENANCE OF STANDARD COMMENTS'                       
T21206   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21206*,R7,RR=R3                                              
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
         GOTO1 INITIAL,DMCB,0      INITIALIZE PFKEYS                            
         XC    ACURSOR,ACURSOR                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
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
         USING COMTXTD,R6                                                       
         SR    R7,R7               LINE COUNTER                                 
DR10     MVC   8(L'COMTXT,R2),SPACES                                            
         LA    R7,1(R7)            INCREMENT LINE COUNTER                       
         ZIC   R1,COMTXTSQ         SEQUENCE NUMBER                              
         CR    R1,R7               IF NOT EQUAL, PRINT BLANK LINE               
         BNE   DR15                                                             
*                                                                               
         ZIC   R1,COMTXTLN         LENGTH OF ELEMENT                            
         LA    R3,COMTXTOV         OVERHEAD LENGTH                              
         SR    R1,R3               LENGTH OF TEXT                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),COMTXTTX    LINE OF TEXT                                 
DR15     OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT TEXT FIELD                              
         AR    R2,R0                                                            
*                                                                               
         LA    RF,COMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    DRX                 YES                                          
*                                                                               
         ZIC   R1,COMTXTSQ         SEQUENCE NUMBER                              
         CR    R1,R7               IF NOT EQUAL, PRINT BLANK LINE               
         BNE   DR10                                                             
*                                                                               
         BAS   RE,NEXTEL           NEXT LINE OF TEXT                            
         BE    DR10                                                             
*                                                                               
DR20     ZIC   R1,0(R2)                                                         
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
         BNH   DR20                NO                                           
*                                                                               
DRX      MVC   ACURFORC,ACURSOR    PFKEY WAS HIT -- PLACE CURSOR                
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
         L     R7,ATIOB            A(TIOB)                                      
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
         EJECT                                                                  
VR100    L     R6,AIO              A(COMMENT RECORD)                            
         USING COMKEY,R6                                                        
*                                                                               
         LA    R2,COMTXTH          FIRST TEXT FIELD                             
         MVI   TXTFOUND,C'N'       NO TEXT FOUND YET                            
         MVI   SEQNUM,0                                                         
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR110                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,COMTXTEQ     COMMENT TEXT ELEMENT CODE                    
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
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
         ZIC   RF,SEQNUM           INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
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
*                                                                               
         CLI   BLANKLN,C'Y'        DON'T ADD BLANK LINE                         
         BE    VR120                                                            
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VR120                                                            
         DC    H'0'                                                             
*                                                                               
VRX      CLI   TXTFOUND,C'Y'       MAKE SURE SOME TEXT WAS FOUND                
         BNE   NOTXTERR                                                         
*                                                                               
         DROP  R4,R6                                                            
         B     DR                                                               
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
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
INVLREFN MVI   GERROR1,INVREFN                                                  
         B     ERREXIT                                                          
*                                                                               
NOTXTERR MVI   GERROR1,TXTREQ      AT LEAST ONE LINE OF TEXT REQUIRED           
         B     ERREXIT                                                          
*                                                                               
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPADDSECTS                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPADDFFD          (BASE SCREEN FOR SYSTEM)                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDF6D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDE6D          (OUR LIST SCREEN OVERLAY)                    
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPADDWORKD        (SYSTEM AREAS)                               
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
         SPACE                                                                  
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075SPADD06   05/01/02'                                      
         END                                                                    
