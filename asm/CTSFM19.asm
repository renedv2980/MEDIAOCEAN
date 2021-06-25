*          DATA SET CTSFM19    AT LEVEL 039 AS OF 05/01/02                      
*PHASE TA0A19A                                                                  
*INCLUDE DECODE                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A19 - FAX INFORMATION RECORD MAINTENANCE/LIST            *         
*                                                                     *         
*  COMMENTS: MAINTAINS FAX INFORMATION RECORDS.                       *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMCA (TA0ACA) -- MAINTENANCE                    *         
*                  CTSFMDA (TA0ADA) -- LIST                           *         
*                  CTSFMAB (TA0AAB) -- REPORT                         *         
*                                                                     *         
*  OUTPUTS: UPDATED FAX INFORMATION RECORDS                           *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A19 FAX INFORMATION MAINTENANCE /LIST'                       
TA0A19   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A19**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         XC    ACURSOR,ACURSOR                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE KEY ROUTINE      * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
VK       XC    FAXCD,FAXCD         CLEAR SAVED FAXCODE                          
         XC    SUBCD,SUBCD                                                      
         MVI   FAXFILT,0                                                        
*                                                                               
         LA    R2,SFMFAXCH         FAX CODE FIELD                               
         CLI   5(R2),0             NUMBER GIVEN?                                
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR                                                          
         B     VK17                                                             
*                                                                               
VK10     CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK15                                                             
         CLI   5(R2),3             INPUT LENGTH 3?                              
         BNE   VK15                                                             
         CLC   =C'ALL',8(R2)       DISPLAY ALL CODES                            
         BNE   VK15                                                             
         OI    FAXFILT,ALL                                                      
         B     VK17                                                             
*                                                                               
VK15     ZIC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   FAXCD(0),SFMFAXC                                                 
*                                                                               
VK17     LA    R2,SFMSUBCH         FAX SUB-CODE FIELD                           
         ZICM  R1,5(R2)            LENGTH OF INPUT                              
         BZ    VK20                                                             
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SUBCD(0),SFMSUBC                                                 
         OC    SUBCD,=6X'40'       BLANK FILL                                   
*                                                                               
VK20     XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING CTFXREC,R4                                                       
         MVI   CTFXKTYP,CTFXEQU    X'09'                                        
         MVC   CTFXAGY,AGENCY      AGENCY                                       
         MVC   CTFXCODE,FAXCD      FAX CODE                                     
         OC    CTFXCODE,=7X'40'    BLANK FILL                                   
         MVC   CTFXSUBC,SUBCD      FAX SUB-CODE                                 
VKX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD ROUTINE   * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
VR       L     R4,AIO              A(FAX RECORD)                                
         USING CTFXREC,R4                                                       
*                                                                               
         MVI   ELCODE,CTFX1ELQ     (01 ELEMENT)                                 
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'01'          ELEMENT CODE                                 
*                                                                               
         LA    R2,SFMFAXNH                                                      
         ZIC   RE,5(R2)                                                         
         LA    RE,2(RE)                                                         
         STCM  RE,1,ELEM+1         VARIABLE ELEMENT LENGTH                      
*                                                                               
         CLI   5(R2),0             CHECK FOR INPUT                              
*        BE    NEEDFLDN                                                         
         BNE   *+18                                                             
         MVI   ELEM+1,18           MAX ELEMENT LENGTH                           
         MVC   ELEM+2(16),SPACES   BLANK FAX NUMBER                             
         B     VR40                                                             
*                                                                               
         TM    4(R2),X'08'         TEST FIELD FOR NUMERIC                       
         BZ    INVNUM                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+2(0),8(R2)     MOVE OUT THE FAX NUMBER                      
*                                                                               
VR40     GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,X'02'        (ATTENTION ELEMENT)                          
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SFMATTH                                                       
         CLI   5(R2),0                                                          
         BE    INPREQ                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'021B'                                                 
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+2(0),8(R2)     MOVE OUT THE ATTENTION NAME                  
         GOTO1 ADDELEM                                                          
*                                                                               
VR80     MVI   ELCODE,X'03'        (MESSAGE ELEMENT)                            
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SFMMSG1H                                                      
         LA    R3,1                                                             
         LA    R5,8                                                             
*                                                                               
VR100    XC    ELEM,ELEM                                                        
         MVI   ELEM,X'03'          ELEMENT CODE                                 
         CLI   5(R2),0                                                          
         BE    VR150                                                            
         ZIC   R1,5(R2)                                                         
         LA    R1,3(R1)                                                         
         STCM  R1,1,ELEM+1         ELEMENT LENGTH                               
         STCM  R3,1,ELEM+2         LINE NUMBER                                  
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VR130                                                            
         MVC   ELEM+3(0),8(R2)                                                  
VR130    GOTO1 ADDELEM                                                          
VR150    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,1(R3)                                                         
         BCT   R5,VR100                                                         
*                                                                               
         MVI   ELCODE,CTFX4ELQ     (04 ELEMENT)                                 
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'04'          ELEMENT CODE                                 
*                                                                               
         LA    R2,SFMTELEH                                                      
         ZIC   RE,5(R2)                                                         
         LA    RE,2(RE)                                                         
         STCM  RE,1,ELEM+1         VARIABLE ELEMENT LENGTH                      
*                                                                               
         CLI   5(R2),0             CHECK FOR INPUT                              
         BE    VREX                                                             
         TM    4(R2),X'08'         TEST FIELD FOR NUMERIC                       
         BZ    INVNUM                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+2(0),8(R2)     MOVE OUT THE FAX NUMBER                      
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VREX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD      * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
DR       MVC   SAVEKEY,KEY                                                      
         L     R4,AIO              A(FAX RECORD)                                
         USING CTFXREC,R4                                                       
*                                                                               
*--  FAX NUMBER                                                                 
         XC    SFMFAXN,SFMFAXN                                                  
         LA    R2,SFMFAXNH         FAX NUMBER                                   
         ZIC   R1,CTFX1LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,FAXMOVE                                                       
         OI    SFMFAXNH+6,X'80'                                                 
*                                                                               
*--  ATTENTION                                                                  
         MVC   SFMATT,SPACES       CLEAR FIELD                                  
         OI    SFMATTH+6,X'80'                                                  
*                                                                               
         LA    R6,CTFX1EL          A(FIRST ELEMENT)                             
         USING CTFXATT,R6                                                       
         MVI   ELCODE,X'02'        MESSAGE ELEMENT                              
         BAS   RE,FIRSTEL          ANY TEXT FIELDS?                             
         BNE   DR50                                                             
*                                                                               
         MVC   SFMATT,CTFX2ATT                                                  
*                                                                               
*--  MESSAGES                                                                   
DR50     MVC   SFMMSG1,SPACES      CLEAR FIELD                                  
         OI    SFMMSG1H+6,X'80'                                                 
         MVC   SFMMSG2,SPACES      CLEAR FIELD                                  
         OI    SFMMSG2H+6,X'80'                                                 
         MVC   SFMMSG3,SPACES      CLEAR FIELD                                  
         OI    SFMMSG3H+6,X'80'                                                 
         MVC   SFMMSG4,SPACES      CLEAR FIELD                                  
         OI    SFMMSG4H+6,X'80'                                                 
         MVC   SFMMSG5,SPACES      CLEAR FIELD                                  
         OI    SFMMSG5H+6,X'80'                                                 
         MVC   SFMMSG6,SPACES      CLEAR FIELD                                  
         OI    SFMMSG6H+6,X'80'                                                 
         MVC   SFMMSG7,SPACES      CLEAR FIELD                                  
         OI    SFMMSG7H+6,X'80'                                                 
         MVC   SFMMSG8,SPACES      CLEAR FIELD                                  
         OI    SFMMSG8H+6,X'80'                                                 
*                                                                               
         LA    R6,CTFX1EL          A(FIRST ELEMENT)                             
         USING CTFXMSG,R6                                                       
         MVI   ELCODE,X'03'        MESSAGE ELEMENT                              
         BAS   RE,FIRSTEL          ANY TEXT FIELDS?                             
         BNE   DR400               NO MESSAGES DISPLAY DONE                     
*                                                                               
DR100    CLI   CTFX3LIN,1          FIRST MESSAGE LINE                           
         BNE   DR120               LENGTH OF ELEMENT                            
         ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMMSG1(0),CTFX3MSG MESSAGE 1                                    
         BAS   RE,NEXTEL           NEXT TEXT FIELD                              
         BNE   DR400                                                            
*                                                                               
DR120    CLI   CTFX3LIN,2          SECOND MESSAGE LINE                          
         BNE   DR130               LENGTH OF ELEMENT                            
         ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMMSG2(0),CTFX3MSG MESSAGE 2                                    
         BAS   RE,NEXTEL           NEXT TEXT FIELD                              
         BNE   DR400                                                            
*                                                                               
DR130    CLI   CTFX3LIN,3          THIRD MESSAGE LINE                           
         BNE   DR140               LENGTH OF ELEMENT                            
         ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMMSG3(0),CTFX3MSG MESSAGE 3                                    
         BAS   RE,NEXTEL           NEXT TEXT FIELD                              
         BNE   DR400                                                            
*                                                                               
DR140    CLI   CTFX3LIN,4          FOURTH MESSAGE LINE                          
         BNE   DR150               LENGTH OF ELEMENT                            
         ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMMSG4(0),CTFX3MSG MESSAGE 4                                    
         BAS   RE,NEXTEL           NEXT TEXT FIELD                              
         BNE   DR400                                                            
*                                                                               
DR150    CLI   CTFX3LIN,5          FIFTH MESSAGE LINE                           
         BNE   DR160               LENGTH OF ELEMENT                            
         ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMMSG5(0),CTFX3MSG MESSAGE 5                                    
         BAS   RE,NEXTEL           NEXT TEXT FIELD                              
         BNE   DR400                                                            
*                                                                               
DR160    CLI   CTFX3LIN,6          SIXTH MESSAGE LINE                           
         BNE   DR170               LENGTH OF ELEMENT                            
         ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMMSG6(0),CTFX3MSG MESSAGE 6                                    
         BAS   RE,NEXTEL           NEXT TEXT FIELD                              
         BNE   DR400                                                            
*                                                                               
DR170    CLI   CTFX3LIN,7          SEVENTH MESSAGE LINE                         
         BNE   DR180               LENGTH OF ELEMENT                            
         ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMMSG7(0),CTFX3MSG MESSAGE 7                                    
         BAS   RE,NEXTEL           NEXT TEXT FIELD                              
         BNE   DR400                                                            
*                                                                               
DR180    CLI   CTFX3LIN,8          EIGHTH MESSAGE LINE                          
         BE    *+6                 LENGTH OF ELEMENT                            
         DC    H'0'                                                             
         ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMMSG8(0),CTFX3MSG MESSAGE 3                                    
*                                                                               
*--  TELEPHONE NUMBER                                                           
DR400    XC    SFMTELE,SFMTELE                                                  
         OI    SFMTELEH+6,X'80'                                                 
*                                                                               
         LA    R6,CTFX1EL          A(FIRST ELEMENT)                             
         USING CTFXTEL,R6                                                       
         MVI   ELCODE,X'04'        MESSAGE ELEMENT                              
         BAS   RE,FIRSTEL          ANY TEXT FIELDS?                             
         BNE   DREX                                                             
*                                                                               
         XC    SFMTELE,SFMTELE                                                  
         LA    R2,SFMTELEH         TELEPHONE NUMBER                             
         ZIC   R1,CTFX4LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,TELMOVE                                                       
         OI    SFMTELEH+6,X'80'                                                 
         B     DREX                                                             
*                                                                               
DREX     B     EXIT                                                             
FAXMOVE  MVC   SFMFAXN(0),CTFX1NUM                                              
TELMOVE  MVC   SFMTELE(0),CTFX4TEL                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* DISPLAY KEY   * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
DK       L     R4,AIO              RECORD SELECTED                              
         USING CTFXREC,R4                                                       
         MVC   SFMFAXC,CTFXCODE                                                 
         OI    SFMFAXCH+6,X'80'    FAX CODE                                     
         MVC   SFMSUBC,CTFXSUBC                                                 
         OI    SFMSUBCH+6,X'80'    FAX SUB-CODE                                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* ONLINE LIST ROUTINE   * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
LR       LA    R4,KEY                                                           
         USING CTFXREC,R4                                                       
         OC    KEY,KEY             DISPLAY FROM SELECT?                         
         BNZ   LR10                READ RECORD                                  
*                                                                               
         MVI   CTFXKTYP,CTFXEQU    X'09'                                        
         MVC   CTFXAGY,AGENCY      AGENCY                                       
         MVC   CTFXCODE,FAXCD      FAX CODE                                     
         MVC   CTFXSUBC,SUBCD      FAX SUB-CODE                                 
*                                                                               
LR10     GOTO1 HIGH                                                             
*                                                                               
LR40     CLI   CTFXKTYP,CTFXEQU    X'09'                                        
         BNE   EXIT                                                             
         CLC   CTFXAGY,AGENCY     AGENCY                                        
         BNE   EXIT                                                             
*                                                                               
         CLI   SFMSUBCH+5,0        SUB-CODE ENTERED                             
         BE    *+14                                                             
         CLC   CTFXSUBC,SUBCD      IF SO, TEST KEY MATCH                        
         BNE   LR60                                                             
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LSTCODE,CTFXCODE    FAX CODE                                     
         MVC   LSTSUBCD,CTFXSUBC   SUB-CODE                                     
         XC    LSTNUMBR,LSTNUMBR                                                
         ZIC   R1,CTFX1LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,LISTFAXN         MOVE FAX NUMBER                              
*                                                                               
         L     R4,AIO                                                           
         LA    R6,CTFX1EL          A(FIRST ELEMENT)                             
         USING CTFXATT,R6                                                       
         MVI   ELCODE,X'02'        MESSAGE ELEMENT                              
         BAS   RE,FIRSTEL          ANY TEXT FIELDS?                             
         BNE   *+10                                                             
         MVC   LSTATT,CTFX2ATT     MOVE ATTENTION NAME                          
*                                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LR60     GOTO1 SEQ                 NEXT ESCAPE RECORD                           
         LA    R4,KEY              POINT R4 BACK TO KEY                         
         B     LR40                                                             
LISTFAXN MVC   LSTNUMBR(0),CTFX1NUM                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* PRINT REPORT  * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
PR       DS    0H                                                               
*                                                                               
         LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING CTFXREC,R4                                                       
         MVI   CTFXKTYP,CTFXEQU    X'09'                                        
         MVC   CTFXAGY,AGENCY      AGENCY                                       
         MVC   CTFXCODE,FAXCD      FAX CODE                                     
*                                                                               
PR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    PR25                                                             
         DC    H'0'                                                             
*                                                                               
PR20     MVC   KEYSAVE,KEY                                                      
         LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    PR25                                                             
         DC    H'0'                                                             
*                                                                               
PR25     CLC   KEY(4),KEYSAVE      SAME RECORD TYPE & AGENCY                    
         BNE   PRX                                                              
*                                                                               
         TM    FAXFILT,ALL         PRINT ALL FAX CODES                          
         BO    PR30                                                             
*                                                                               
         LA    R2,SFMFAXCH         COMPARE FAX CODES                            
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   PRX                                                              
         CLC   CTFXCODE(0),FAXCD                                                
*                                                                               
PR30     DS    0H                                                               
         LA    R2,SFMSUBCH         COMPARE SUB-CODES                            
         ZICM  R1,5(R2)            LENGTH OF INPUT                              
         BZ    PR32                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   PR20                                                             
         CLC   CTFXSUBC(0),SUBCD                                                
*                                                                               
PR32     LA    R3,1                INITIALIZE COUNT OF LINES TO 1               
*                                                                               
         L     R4,AIO                                                           
         LA    R6,CTFX1EL          A(FIRST ELEMENT)                             
         USING CTFXMSG,R6                                                       
         MVI   ELCODE,CTFX3ELQ     MESSAGE ELEMENT                              
         BAS   RE,FIRSTEL          ANY TEXT FIELDS?                             
         BNE   PR45                NO MESSAGES                                  
         B     PR40                                                             
*                                                                               
PR35     CLI   CTFX3EL,CTFX3ELQ    IS IT A MESSAGE ELEMENT?                     
         BNE   PR45                                                             
PR40     ZIC   R3,CTFX3LIN         SAVE LAST LINE NUMBER OF MESSAGE             
         BAS   RE,NEXTEL                                                        
         BE    PR35                                                             
*                                                                               
*--  TELEPHONE : IT GOES UNDER ATTENTION ON THE SECOND LINE                     
PR45     XC    TELE,TELE                                                        
         LA    R6,CTFX1EL          A(FIRST ELEMENT)                             
         USING CTFXTEL,R6                                                       
         MVI   ELCODE,X'04'        TELEPHONE NUMBER                             
         BAS   RE,FIRSTEL          ANY TEXT FIELDS?                             
         BNE   PR50                                                             
*                                                                               
         ZIC   R1,CTFX4LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   TELE(0),CTFX4TEL    SAVE TELEPHONE NUMBER TO PRINT LATER         
*                                                                               
         STC   R3,ALLOWLIN         ALLOW THIS # OF LINES ON SAME PAGE           
         CLI   ALLOWLIN,1                                                       
         BNE   PR50                                                             
         MVI   ALLOWLIN,2          AT LEAST 2 LINES IF TELEPHONE EXISTS         
*                                                                               
*--  FAX CODE                                                                   
PR50     L     R6,AIO                                                           
         MVC   PRTCODE,CTFXCODE                                                 
         MVC   PRTSUBC,CTFXSUBC                                                 
*                                                                               
*--  FAX NUMBER                                                                 
         ZIC   R1,CTFX1LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   PRTFAXN(0),CTFX1NUM                                              
*                                                                               
*--  ATTENTION                                                                  
         LA    R6,CTFX1EL          A(FIRST ELEMENT)                             
         USING CTFXATT,R6                                                       
         MVI   ELCODE,X'02'        MESSAGE ELEMENT                              
         BAS   RE,FIRSTEL          ANY TEXT FIELDS?                             
         BNE   *+10                                                             
         MVC   PRTATTN,CTFX2ATT                                                 
*                                                                               
*--  MESSAGES                                                                   
         MVI   LASTLINE,0          INITIALIZE LAST MESSAGE LINE                 
         LA    R6,CTFX1EL          A(FIRST ELEMENT)                             
         USING CTFXMSG,R6                                                       
         MVI   ELCODE,X'03'        MESSAGE ELEMENT                              
         BAS   RE,FIRSTEL          ANY TEXT FIELDS?                             
         BE    PR55                                                             
         GOTO1 SPOOL,DMCB,(R8)     PRINT LINE                                   
         BCTR  R3,0                                                             
         B     PR65                NO COMMENT LINES                             
*                                                                               
* PRINT FIRST COMMENT LINE                                                      
PR55     CLI   CTFX3LIN,1                                                       
         BNE   PR60                                                             
         ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+4                                                           
         MVC   PRTMSG(0),CTFX3MSG  MESSAGE                                      
         MVI   LASTLINE,1                                                       
PR60     GOTO1 SPOOL,DMCB,(R8)     PRINT LINE                                   
         BCTR  R3,0                                                             
*                                                                               
*--  TELEPHONE : IT GOES UNDER ATTENTION ON THE SECOND LINE                     
PR65     MVC   PRTTELE,TELE        TELEPHONE NUMBER                             
         CH    R3,=H'0'            IN CASE THERE'S NO COMMENTS                  
         BE    PR95                                                             
         CLI   CTFX3LIN,2                                                       
         BL    PR75                                                             
         MVI   LASTLINE,2          PRINTED SECOND LINE                          
         BE    PR70                                                             
         GOTO1 SPOOL,DMCB,(R8)     THE SECOND MESSAGE LINE IS BLANK             
         BCTR  R3,0                                                             
         B     PR80                                                             
*                                                                               
PR70     ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+4                                                           
         MVC   PRTMSG(0),CTFX3MSG  MESSAGE                                      
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         BCTR  R3,0                                                             
*                                                                               
PR75     BAS   RE,NEXTEL           NEXT TEXT FIELD                              
         BNE   PR100                                                            
*                                                                               
PR80     ZIC   R1,LASTLINE         GET LAST LINE NUMBER                         
         LA    R1,1(R1)            INCREMENT BY 1                               
         ZIC   R5,CTFX3LIN         GET NEXT LINE NUMBER                         
         STC   R5,LASTLINE         SAVE NEW ONE AS LAST LINE NUMBER             
         CR    R1,R5               CHECK IF BLANK LINES EXIST                   
         BE    PR90                                                             
         SR    R5,R1                                                            
*                                                                               
PR85     MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         BCT   R5,PR85                                                          
*                                                                               
PR90     ZIC   R1,CTFX3LEN         ELEMENT LENGTH                               
         SH    R1,=H'4'            LENGTH OF TEXT                               
         EX    R1,*+4                                                           
         MVC   PRTMSG(0),CTFX3MSG  MESSAGE                                      
         GOTO1 SPOOL,DMCB,(R8)     PRINT LINE                                   
         BCT   R3,PR75                                                          
         B     PR100                                                            
*                                                                               
PR95     OC    TELE,TELE           WAS THERE A TELEPHONE NUMBER?                
         BZ    PR100                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT RECORD                                 
*                                                                               
PR100    CLI   LINE,99             CHECK IF FIRST LINE                          
         BE    PR20                                                             
         MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         B     PR20                NEXT RECORD                                  
         DROP  R4,R6                                                            
*                                                                               
PRX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* HEAD HOOK ROUTINE * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
HDHOOK   NTR1                                                                   
*                                                                               
         MVC   H1+57(18),=C'FAX RECORDS REPORT'                                 
         MVI   H2+57,X'BF'         UNDERLINE CHARACTER                          
         MVC   H2+58(17),H2+57                                                  
*                                                                               
         ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
         BZ    HDHOOKX             NO                                           
*                                                                               
         USING BOXD,RF             DEFINE BOX AREA                              
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+57,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+8,C'C'                                                   
         MVI   BOXCOLS+25,C'C'                                                  
         MVI   BOXCOLS+51,C'C'                                                  
         MVI   BOXCOLS+131,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  RF                                                               
HDHOOKX  B     EXIT                                                             
         SPACE 3                                                                
HEDSPECS SSPEC H1,1,AGYNAME                                                     
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,1,AGYADD                                                      
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H7,2,C'CODE'                                                     
         SSPEC H7,10,C'SUB-CODE'                                                
         SSPEC H7,22,C'FAX NUMBER'                                              
         SSPEC H7,39,C'ATTENTION/TELEPHONE'                                     
         SSPEC H7,65,C'COMMENTS'                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
NEEDFLDN XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NEEDFLDM),NEEDFLDM                                     
         GOTO1 ERREX2                                                           
NEEDFLDM DC    C'* ERROR * FAX NUMBER REQUIRED *'                               
*                                                                               
INVNUM   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVNUMM),INVNUMM                                       
         GOTO1 ERREX2                                                           
INVNUMM  DC    C'* ERROR * FAX NUMBER NON NUMERIC *'                            
*                                                                               
INVFMT   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVFMTM),INVFMTM                                       
         GOTO1 ERREX2                                                           
INVFMTM  DC    C'* ERROR * INVALID FORMAT *'                                    
*                                                                               
INPREQ   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INPREQM),INPREQM                                       
         GOTO1 ERREX2                                                           
INPREQM  DC    C'* ERROR * INPUT REQUIRED *'                                    
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         B     EXIT                                                             
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENFAX                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMCAD                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* TA0A19 STORAGE DSECT    * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
         ORG   SYSSPARE                                                         
ACURSOR  DS    A                   FORCE CURSOR HERE                            
MYWORK   DS    XL96                                                             
SAVEKEY  DS    XL25                ESCAPE SEQUENCE RECORD KEY                   
FAXCD    DS    XL7                 SELECTED FAX CODE                            
SUBCD    DS    XL6                 FAX SUB-CODE                                 
LASTLINE DS    X                   LAST MESSAGE LINE PRINTED                    
TELE     DS    CL16                TELEPHONE NUMBER                             
*                                                                               
FAXFILT  DS    X                                                                
ALL      EQU   X'80'               PRINT REPORT W/ ALL FAX CODES                
*                                                                               
* ONLINE LIST LINE   * * * * * * * * * * * * * * * * * * * * * * * * *          
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCODE  DS    CL7                                                              
         DS    CL4                                                              
LSTSUBCD DS    CL6                                                              
         DS    CL5                                                              
LSTNUMBR DS    CL16                                                             
         DS    CL2                                                              
LSTATT   DS    CL25                                                             
*                                                                               
* REPORT LINE  * * * * * * * * * * * * * * * * * * * * * * * * * * * *          
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    C                                                                
PRTCODE  DS    CL7                 CODE                                         
         DS    C                                                                
PRTSUBC  DS    CL6                 SUBC                                         
         DS    CL6                                                              
PRTFAXN  DS    CL16                FAX NUMBER                                   
         DS    C                                                                
PRTATTN  DS    CL25                ATTENTION                                    
         DS    C                                                                
PRTMSG   DS    CL65                COMMENTS                                     
         ORG   PRTATTN                                                          
PRTTELE  DS    CL16                TELEPHONE                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039CTSFM19   05/01/02'                                      
         END                                                                    
