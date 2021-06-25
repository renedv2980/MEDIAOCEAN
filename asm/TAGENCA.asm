*          DATA SET TAGENCA    AT LEVEL 098 AS OF 02/21/17                      
*PHASE T702CAB,*                                                                
         TITLE 'T702CA - SOAP CAST LIST/MAINTENANCE'                            
T702CA   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702CA,R8,R7,R5                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         LA    R6,TWAHOLE          R6=TWAHOLE                                   
         USING WORKD,R6                                                         
         NI    GENSTAT4,ALL-CONFDEL TURN OFF CONFIRM DELETE                     
*                                                                               
         CLI   MODE,SETFILE        IGNORE THIS MODE                             
         BE    XIT                                                              
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE OVERLAY                           
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    INIT05                                                           
         CLI   ACTNUM,ACTLDEL      LDELETE? OR                                  
         BE    INIT05                                                           
         CLI   ACTNUM,ACTPDEL      PDELETE?                                     
         BE    INIT05                                                           
         MVC   SCASHED(7),=C'Pid Num'                                           
         OI    SCASHEDH+6,X'80'                                                 
         B     INIT10                                                           
*                                                                               
INIT05   MVC   SCLTAG(7),=C'Sel^Pid'                                            
         OI    SCLTAGH+6,X'80'                                                  
*                                                                               
*                                  SAVE A(BUILD DISK/ADDR LIST ROUTINE)         
INIT10   L     RF,=A(BLDLIST-T702CA)                                            
         AR    RF,RB                                                            
         ST    RF,ABLDLIST                                                      
*                                  SAVE A(BUILD SORT KEY ROUTINE)               
         L     RF,=A(BLDSORT-T702CA)                                            
         AR    RF,RB                                                            
         ST    RF,ABLDSORT                                                      
*                                  SAVE A(TEST FIRST MODE ROUTINE)              
         L     RF,=A(TESTFRST-T702CA)                                           
         AR    RF,RB                                                            
         ST    RF,ATSTFRST                                                      
*                                  SAVE A(STATUS CODES TABLE)                   
         L     RF,=A(STATTAB-T702CA)                                            
         AR    RF,RB                                                            
         ST    RF,ASTATTAB                                                      
*                                  SAVE A(STATUS CODES TABLE)                   
         L     RF,=A(WROLTAB-T702CA)                                            
         AR    RF,RB                                                            
         ST    RF,AWROLTAB                                                      
*                                  SAVE A(NORMAL LIST PF TABLE)                 
         L     RF,=A(LPFTAB-T702CA)                                             
         AR    RF,RB                                                            
         ST    RF,ALPFTAB                                                       
*                                  SAVE A(DELETED LIST PF TABLE)                
         L     RF,=A(DPFTAB-T702CA)                                             
         AR    RF,RB                                                            
         ST    RF,ADPFTAB                                                       
*                                  SAVE A(FAKE POP PF TABLE)                    
         L     RF,=A(POPTAB-T702CA)                                             
         AR    RF,RB                                                            
         ST    RF,APOPTAB                                                       
*                                  SAVE A(EXTENSION SCREEN PF TABLE)            
         L     RF,=A(EPFTAB-T702CA)                                             
         AR    RF,RB                                                            
         ST    RF,AEPFTAB                                                       
*                                  SAVE A(LIST SCREEN LINE ADDRS TABLE)         
         L     RF,=A(LINATAB-T702CA)                                            
         AR    RF,RB                                                            
         ST    RF,ALINATAB                                                      
*                                                                               
         DROP  RA                                                               
         EJECT                                                                  
***********************************************************************         
* THIS PROGRAM CURRENTLY HANDLES 4 SITUATIONS:                        *         
*     1) THE USER EXPLICITLY REQUESTS A CAST LIST                     *         
*     2) THE CAST VERIFY PROGRAM CALLS THIS PROGRAM TO GET A CAST     *         
*        LIST                                                         *         
*     3) THE USER SELECTS A CAST MEMBER FROM THE CAST LIST            *         
*     4) THE COMMERCIAL OR AGENT CAST LIST PROGRAMS CALL THIS         *         
*        PROGRAM TO DISPLAY A CAST MEMBER THE USER SELECTED           *         
*                                                                     *         
* IN CASES 1) AND 2) THE SCREEN FLAG BECOMES 'L' AND THE LIST DRIVER  *         
* IS CALLED.  IN CASES 3) AND 4) THE SCREEN FLAG IS 'E' AND THE       *         
* EXTENSION SCREEN DRIVER IS CALLED.                                  *         
***********************************************************************         
         SPACE 1                                                                
MAIN     DS    0H                                                               
         L     R4,ATWA             R4 = A(ATWA)                                 
         USING T702FFD,R4                                                       
*                                                                               
         MVI   SCRFLAG,C'E'        IF SCREEN IS EXTENSION SCREEN THEN           
         CLI   TWASCR,SCRCA            SCREEN FLAG = 'E'                        
         BE    *+8                                                              
         MVI   SCRFLAG,C'L'        ELSE SCREEN FLAG = 'L'                       
*                                                                               
         GOTO1 ATSTFRST,DMCB,(RC)  TAKE SPECIAL ACTION FIRST TIME IN            
*                                                                               
         CLI   SCRFLAG,C'L'        IF SCREEN IS LIST SCREEN                     
         BE    LDRIVER             THEN GO TO LIST DRIVER                       
         B     EDRIVER             ELSE GO TO EXTENSION SCREEN DRIVER           
         EJECT                                                                  
LDRIVER  DS    0H                                                               
         MVI   VALMODE,0           CLEAR VALIDATION MODE                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    LD10                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    LD20                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LD30                                                             
         B     LDX                                                              
*                                                                               
LD10     BAS   RE,LVKEY            VALIDATE KEY                                 
         B     LDX                                                              
*                                                                               
LD20     CLI   ACTNUM,ACTPDEL      IF ACTION IS PGDELETE                        
         BNE   LD27                                                             
         BAS   RE,LPGDEL           THEN DELETE PAGE OF RECORDS                  
         B     LDX                                                              
*                                                                               
LD27     BAS   RE,LVREC            ELSE VALIDATE RECORD                         
         B     LDX                                                              
*                                                                               
LD30     BAS   RE,LPRINTR          PRINT REPORT                                 
*                                                                               
LDX      B     XIT                                                              
         DROP  R4                                                               
         USING T702FFD,RA                                                       
         EJECT                                                                  
* LIST SCREEN MODE VALKEY ROUTINE.                                              
*                                                                               
LVKEY    NTR1                                                                   
         MVI   EPICHANG,C'N'                                                    
         TM    SCLSEPIH+4,X'20'    IF EPISODE NUMBER CHANGED                    
         BO    *+8                                                              
         MVI   EPICHANG,C'Y'       SET EPISODE NUMBER CHANGE FLAG               
*                                                                               
         MVI   KEYCHANG,C'N'       IF AGENCY OR COMMERCIAL FIELD HAS            
         TM    SCLAGYH+4,X'20'         CHANGED                                  
         BZ    LVK10                                                            
         TM    SCLCIDH+4,X'20'                                                  
         BZ    LVK10                                                            
         CLC   ACTNUM,TWALACT      OR ACTION HAS CHANGED                        
         BE    LVK20                                                            
         TM    TRNSTAT,RETURNED    AND WE HAVEN'T JUST POPPED                   
         BO    LVK20                                                            
*                                                                               
LVK10    MVI   KEYCHANG,C'Y'       THEN SET KEY CHANGE FLAG                     
*                                                                               
*                                  VALIDATE AGENCY                              
LVK20    GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',SCLAGYH)                              
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         TM    TAAYSTA3,TAAYSRES   RESIDUAL AGENCY?                             
         BO    ERRREC              YES INVALID RECORD TYPE FOR AGENCY           
         DROP  R4                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TASOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LVK30                                                            
         USING TASOD,R4                                                         
*******  MVC   TGEPI,TASOEPI       SAVE AGY DEFAULT START EPI NUMBER            
         LH    RE,TASOEPI          SET CHARACTER DEFAULT START EPISODE          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TGEPI,DUB                                                        
         DROP  R4                                                               
*                                                                               
*                                  VALIDATE COMMERCIAL ID                       
LVK30    GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',SCLCIDH),SCLCIDNH                    
*                                                                               
*                                  DISPLAY COMMENTS AT BOTTOM                   
         GOTO1 CHAROUT,DMCB,TACMELQ,SCLCOMMH,TACMTYPG                           
*                                                                               
         LA    R3,KEY              SAVE INTERNAL CMCL NUMBER IN GLOBAL          
         USING TLCOPD,R3                                                        
         MVC   TGCOM,TLCOICOM                                                   
         DROP  R3                                                               
*                                                                               
         LA    R2,SCLCIDH                                                       
         BAS   RE,GETCOMM          GET OTHER COMMERCIAL INFO                    
*                                                                               
         LA    R2,SCLSEPIH                                                      
         GOTO1 RECVAL,DMCB,TLEPCDQ,(R2)                                         
*                                                                               
         LA    R4,EPITMAX          SET EPISODE NUMBERS FOR WEEK                 
         LA    R3,EPITAB           R3=A(CHAR EPISODE NUMBERS TABLE)             
         PACK  DUB(8),TGEPI                                                     
         B     *+10                                                             
LVK48    AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  0(L'EPITAB,R3),DUB                                               
         GOTO1 RECVAL,DMCB,TLEPCDQ,(X'84',(R3))                                 
         BNE   ERREPI              EPISODE NUMBER RECORD MUST EXIST             
         LA    R3,L'EPITAB(R3)                                                  
         BCT   R4,LVK48                                                         
         MVI   0(R3),X'FF'         MARK END OF TABLE                            
         MVC   TGEPI,EPITAB        RESET FIRST EPISODE NUMBER TO GLOBAL         
*                                                                               
LVK50    CLI   CONWHENH+5,0        IF PRINT FIELD HAS SOMETHING IN IT           
         BE    *+8                                                              
         NI    WHENOK,X'FE'        CLEAR BIT THAT PREVENTS PRINTREP             
*                                                                               
LVKX     B     XIT                                                              
         EJECT                                                                  
* LIST SCREEN PGDELETE ROUTINE (ALWAYS TAKE ERROR EXIT IN ORDER TO              
* DISPLAY OWN ERROR MESSAGE).                                                   
*                                                                               
LPGDEL   NTR1                                                                   
         XC    NAMESH,NAMESH       CLEAR NAMES FIELD                            
         MVI   NAMESH,8+16         SET LENGTH OF FAKE NAMES FIELD               
*                                                                               
         CLI   EPICHANG,C'Y'       IF EPISODE NUMBER HAS CHANGED OR             
         BE    *+12                                                             
         CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED                           
         BNE   LPG20                                                            
         MVI   PFKEYOK,C'N'                                                     
         GOTO1 ABLDLIST,DMCB,1,(RC)                                             
         MVC   NITEM,NUMITEMS      SAVE ORIGINAL N'ITEMS                        
         XC    ITEM2,ITEM2                                                      
         B     LPG40                                                            
*                                                                               
LPG20    GOTO1 ABLDLIST,DMCB,0,(RC)   LOCK RECORD                               
         CLI   PFAID,19            IF PF19 PRESSED                              
         BNE   LPG30                                                            
         CLI   PFKEYOK,C'Y'        & OK TO PROCESS PF KEY                       
         BNE   LPG30                                                            
         BAS   RE,ANYDIS           IF NOTHING LEFT TO DISPLAY                   
         BNE   ENDLIST             EXIT                                         
         BAS   RE,DELPG            ELSE DELETE PAGE                             
         NI    TRNSTAT,X'FF'-OKINTPFK                                           
         MVI   PFKEYOK,C'N'              TURN OFF PF KEY OK                     
         B     PGDEL                     GIVE MESSAGE PAGE DELETED              
*                                                                               
LPG30    L     R1,ITEM2            IF NO MORE ITEMS TO DISPLAY                  
         A     R1,=A(NUMSCR)                                                    
         C     R1,NUMITEMS                                                      
         BL    LPG35               SET TO START FROM BEGINING                   
         XC    ITEM,ITEM                                                        
         XC    ITEM2,ITEM2                                                      
         B     LPG40                                                            
*                                                                               
LPG35    L     R2,ITEM             BUMP TO NEXT PAGE                            
         A     R2,=A(NUMSCR)                                                    
         ST    R2,ITEM                                                          
         L     R2,ITEM2            BUMP TO NEXT PAGE                            
         A     R2,=A(NUMSCR)                                                    
         ST    R2,ITEM2                                                         
*                                                                               
LPG40    BAS   RE,ANYDIS           IF NOTHING LEFT TO DISPLAY                   
         BNE   ENDLIST             EXIT                                         
         BAS   RE,DISPLIST         DISPLAY 1 PAGE OF LIST                       
         OI    TRNSTAT,OKINTPFK    SET PF19 OK FOR DELETE                       
         MVI   PFKEYOK,C'Y'                                                     
         L     R1,ITEM2                                                         
         A     R1,=A(NUMSCR)                                                    
         C     R1,NUMITEMS         ITEM JUST DISPLAYED                          
         BL    HITPFMSG                                                         
         B     HITPFEND                                                         
*                                                                               
         EJECT                                                                  
* DELETE 1 PAGE OF SCAST                                                        
*                                                                               
DELPG    NTR1                                                                   
         XC    CNTR,CNTR           N'ITEMS DELETED THIS PAGE                    
         L     R3,ITEM             R3 = A(TOP OF PAGE DISK ADDRESS)             
         SLL   R3,2                                                             
         LA    R3,DALIST(R3)                                                    
         SR    R4,R4               NUMBER OF ITEMS DELETED                      
*                                                                               
DP10     CH    R4,=H'4'            4 PERFORMERS PER SCREEN                      
         BE    DPX                                                              
         OC    0(4,R3),0(R3)       IF THERE IS A RECORD TO DISPLAY              
         BZ    DP50                                                             
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         LA    R4,1(R4)            INCREMENT N'RECORDS DELETED                  
         BAS   RE,READREC          THEN READ IN RECORD                          
         BE    DP20                                                             
*                                                                               
         LR    RE,R3               RECORD DISAPPEARED - REMOVE DISKADD          
         LA    RF,DALIST+DALISTL-4     FROM LIST                                
         SR    RF,R3                                                            
         LA    R0,4(RE)                                                         
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RF,NUMITEMS         DECREMENT NUMBER OF ITEMS                    
         BCTR  RF,0                                                             
         ST    RF,NUMITEMS                                                      
         B     DP10                LOOP BACK                                    
*                                                                               
DP20     GOTO1 SAVPTRS,DMCB,PPBLOCK                                             
         USING TLCAD,R1                                                         
         L     R1,AIO              MARK RECORD (DOUBLE) DELETED                 
         OI    TLCASTAT,X'C0'                                                   
         GOTO1 PUTREC              WRITE BACK RECORD                            
*                                                                               
         USING TLDRD,R2                                                         
         LA    R2,KEY              AND (DOUBLE) DELETE DIRECTORY                
         MVC   KEY(L'TLDRKEY),TLCAKEY                                           
         MVC   TLDRSTAT,TLCASTAT   SET STATUS FROM PARENT                       
         MVC   TLDRDA,0(R3)        SET D/A                                      
         GOTO1 WRITE                                                            
*                                  UPDATE PASSIVE POINTERS                      
         GOTO1 ADDPTRS,DMCB,(X'60',PPBLOCK)                                     
         ZIC   R1,CNTR             INCREMENT N'ITEMS DELETED THIS PAGE          
         LA    R1,1(R1)                                                         
         STC   R1,CNTR                                                          
*                                                                               
DP50     LA    R3,4(R3)            BUMP TO NEXT D/A IN LIST                     
         L     R1,NITEM                                                         
         MH    R1,=H'4'                                                         
         LA    R2,DALIST                                                        
         AR    R1,R2                                                            
         CR    R3,R1               DON'T GO BEYOND LAST ENTRY                   
         BNH   DP10                                                             
*                                                                               
DPX      L     R1,NUMITEMS         DECREMENT NUMBER OF ITEMS                    
         ZIC   R2,CNTR                                                          
         SR    R1,R2                                                            
         ST    R1,NUMITEMS                                                      
         L     R1,ITEM2                                                         
         ZIC   R2,CNTR                                                          
         SR    R1,R2                                                            
         ST    R1,ITEM2                                                         
         B     XIT                                                              
         SPACE 3                                                                
         DROP  R1                                                               
         SPACE 3                                                                
*                                                                               
*        DETERMINE IF ANYTHING TO DISPLAY                                       
*                                                                               
ANYDIS   NTR1                                                                   
         ICM   R1,15,NUMITEMS      IF NOTHING FOUND                             
         BNZ   YES                                                              
         TWAXC SCLL1H,SCLCOMMH,PROT=Y    CLEAR SCREEN &                         
         XC    ITEM,ITEM                                                        
         XC    ITEM2,ITEM2                                                      
         B     NO                                                               
         EJECT                                                                  
* LIST SCREEN MODE VALREC ROUTINE (ALWAYS TAKE ERROR EXIT IN ORDER TO           
* DISPLAY OWN ERROR MESSAGE).                                                   
*                                                                               
LVREC    NTR1                                                                   
         XC    NAMESH,NAMESH       CLEAR NAMES FIELD                            
         MVI   NAMESH,8+16         SET LENGTH OF FAKE NAMES FIELD               
         MVI   DISAPPR,C'N'        SET RECORD HAS NOT DISAPPEARED               
*                                                                               
         CLI   ACTNUM,ACTLDEL      IF USER WANTS DELETED RECORDS                
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       THEN TELL DATAMGR TO PASS BACK DELS          
*                                                                               
         CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED                           
         BNE   LVR10                                                            
*                                  BUILD DISK ADDRESS LIST                      
         GOTO1 ABLDLIST,DMCB,1,(RC)                                             
*                                                                               
         BAS   RE,DISPLIST         DISPLAY LIST                                 
         B     LVR100                                                           
*                                                                               
LVR10    MVI   RECCHANG,C'N'       ELSE SET RECORD HAS NOT CHANGED              
*                                                                               
*                                  LOCK LIST                                    
         GOTO1 ABLDLIST,DMCB,0,(RC)                                             
*                                                                               
         BAS   RE,VALILST          VALIDATE LIST                                
*                                                                               
         L     R2,ALPFTAB          R2 = A(PF TABLE)                             
         CLI   ACTNUM,ACTLDEL                                                   
         BNE   *+8                                                              
         L     R2,ADPFTAB                                                       
         GOTO1 INITIAL,DMCB,(R2)   TEST FOR SELECT CODES                        
*                                                                               
         CLI   EPICHANG,C'Y'       IF NO EPISODE NUMBER CHANGE                  
         BE    LVR95                                                            
         CLI   RECCHANG,C'N'       AND IF NO RECORD HAS CHANGED                 
         BNE   LVR90                                                            
         TM    TRNSTAT,RETURNED    THEN IF WE HAVEN'T JUST POPPED               
         BO    *+8                                                              
         BAS   RE,TESTPFKS         TEST DIRECTIONAL PFKEYS                      
         BAS   RE,DISPLIST         DISPLAY LIST                                 
         B     LVR100                                                           
*                                                                               
LVR90    CLI   DISAPPR,C'Y'        ELSE IF SOME RECORD HAS DISAPPEARED          
         BNE   LVR100                                                           
LVR95    BAS   RE,DISPLIST         THEN RE-DISPLAY LIST                         
*                                                                               
LVR100   LA    R2,SCLL1H           POSITION CURSOR TO FIRST SEL FIELD           
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
         CLI   ACTNUM,ACTLIST      IF ACTION IS LIST                            
         BNE   LVR120                                                           
*                                                                               
         OC    NUMITEMS,NUMITEMS   THEN IF NUMBER OF ITEMS = 0                  
         BNZ   LVR110                                                           
         ZIC   R0,0(R2)            THEN BUMP R2 TO FIRST SSN FIELD              
         AR    R2,R0                                                            
         MVI   MYMSGNO1,2          AND GIVE PLEASE ENTER FIELDS MESSAGE         
         B     LVRX                                                             
*                                                                               
LVR110   CLI   PFAID,14            ELSE IF PFKEY FOR ADD PERF PRESSED           
         BNE   LVR120                                                           
         BAS   RE,LSETADDR         THEN BUMP R2 TO SECOND SSN FIELD             
         L     R2,ANEXTLIN                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   MYMSGNO1,2          AND GIVE PLEASE ENTER FIELDS MESSAGE         
         B     LVRX                                                             
*                                                                               
LVR120   L     RF,ITEM             ELSE IF DISPLAYING LAST PAGE                 
         A     RF,=A(NUMSCR)                                                    
         C     RF,NUMITEMS                                                      
         BL    LVR130                                                           
         MVI   MYMSGNO1,10         THEN GIVE END OF LIST MESSAGE                
         B     LVRX                                                             
*                                                                               
LVR130   MVI   MYMSGNO1,9          ELSE MIDDLE OF LIST MESSAGE                  
*                                                                               
LVRX     B     ERREXIT             TAKE ERROR EXIT                              
         EJECT                                                                  
* PRINT CAST LIST.                                                              
*                                                                               
LPRINTR  NTR1                                                                   
         L     R4,ASPOOLD          R4 = A(SPOOL DSECT)                          
         USING SPOOLD,R4                                                        
*                                  SET REPORT SPECS                             
         L     RF,=A(MYSPECS-T702CA)                                            
         AR    RF,RB                                                            
         ST    RF,SPECS                                                         
*                                                                               
         LA    RF,HOOK             SET A(HOOK ROUTINE)                          
         ST    RF,HEADHOOK                                                      
*                                                                               
         XC    NAMESH,NAMESH       CLEAR NAME FIELD                             
         MVI   NAMESH,8+16         SET LENGTH OF FAKE NAMES FIELD               
*                                                                               
*                                  BUILD DISK ADDRESS LIST                      
         GOTO1 ABLDLIST,DMCB,1,(RC)                                             
*                                                                               
         LA    R3,DALIST           R3 = A(FIRST DISK ADDRESS)                   
         LA    R2,SCLL1H           R2 = A(FIRST SCREEN LINE)                    
         BAS   RE,LSETADDR         SET ADDRESSES OF SCREEN FIELDS               
*                                                                               
         L     RE,ASELFLD          SAVE FIRST SCREEN LINE TO RESTORE            
         L     RF,ANEXTLIN             WHEN DONE REPORT                         
         SR    RF,RE                                                            
         LA    R0,SVFIRSTL                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,NUMITEMS         R0 = NUMBER OF ITEMS IN LIST                 
*                                                                               
LPR10    BAS   RE,READREC          READ IN RECORD                               
         BNE   LPR50               IF RECORD DISAPPEARED THEN SKIP              
*                                                                               
         BAS   RE,DISPLINE         DISPLAY LINE TO SCREEN                       
         MVI   BYTE,C'P'           PRINT FIRST REC FROM SCREEN                  
         GOTO1 PRTSCRN,DMCB,(R2),ANEXTLIN,P                                     
         GOTO1 CATCHIOS                                                         
*                                                                               
LPR50    LA    R3,4(R3)            BUMP R3 TO NEXT DISK ADDRESS                 
         BCT   R0,LPR10            REPEAT UNTIL NO MORE RECORDS                 
*                                                                               
         L     RE,ASELFLD          RESTORE FIRST SCREEN LINE                    
         L     RF,ANEXTLIN                                                      
         SR    RF,RE                                                            
         LA    R0,SVFIRSTL                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                  PRINT TOTAL NUMBER OF CAST MEMBERS           
         MVC   P(24),=C'NUMBER OF CAST MEMBERS :'                               
         EDIT  (4,NUMITEMS),(7,P+25),ALIGN=LEFT,ZERO=NOBLANK                    
         GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
         XC    CONSERV,CONSERV     AUTO CALL $DQU                               
         MVC   CONSERV(4),=C'$DQU'                                              
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
LPRX     B     XIT                                                              
         SPACE 2                                                                
* THIS IS THE HOOK ROUTINE FOR THE SPOOLED CAST LIST                            
*                                                                               
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'          MOVE KEY FIELDS TO HEADS                      
         GOTO1 PRTSCRN,DMCB,EFHTAG,SCLL1H,H4                                    
         MVI   BYTE,C'P'          RESET                                         
HKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY THE LIST OF RECORDS ON THE SCREEN.                                    
*                                                                               
DISPLIST NTR1                                                                   
         L     R3,ITEM             R3 = A(TOP OF PAGE DISK ADDRESS)             
         SLL   R3,2                                                             
         LA    R3,DALIST(R3)                                                    
         LA    R2,SCLL1H           R2 = A(FIRST LINE ON SCREEN)                 
*                                                                               
DL10     BAS   RE,LSETADDR         SET ADDRESSES OF FILEDS ON THIS LINE         
*                                                                               
         L     R2,ASELFLD          CLEAR SELECT FIELD                           
         XC    8(3,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
DL20     OC    0(4,R3),0(R3)       IF THERE IS A RECORD TO DISPLAY              
         BZ    DL50                                                             
*                                                                               
         BAS   RE,READREC          THEN READ IN RECORD                          
         BE    DL30                                                             
*                                                                               
         LR    RE,R3               RECORD DISAPPEARED - REMOVE DISKADD          
         LA    RF,DALIST+DALISTL-4     FROM LIST                                
         SR    RF,R3                                                            
         LA    R0,4(RE)                                                         
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RF,NUMITEMS         DECREMENT NUMBER OF ITEMS                    
         BCTR  RF,0                                                             
         ST    RF,NUMITEMS                                                      
         B     DL20                LOOP BACK                                    
*                                                                               
DL30     BAS   RE,DISPLINE         DISPLAY RECORD                               
         LA    R3,4(R3)            BUMP TO NEXT D/A IN LIST                     
         B     DL90                                                             
*                                                                               
DL50     BAS   RE,CLRLINE          ELSE CLEAR LINE OUT FOR ADDING               
         CLI   PFAID,14            IF ADDING NOW                                
         BNE   DL90                                                             
         L     RE,ACHCTFLD         DISPLAY CMNT AND EPISODE NUMBERS             
         MVC   8(4,RE),=C'Cmnt'                                                 
         BAS   RE,DREPILBL                                                      
         L     RE,AEPIFLD                                                       
         MVC   8(8,RE),=C'Episodes'                                             
*                                                                               
DL90     L     R2,ANEXTLIN         R2 = A(NEXT DISPLAY LINE)                    
*                                                                               
         LA    R1,SCLCOMMH         REPEAT UNTIL END OF SCREEN                   
         CR    R2,R1                                                            
         BL    DL10                                                             
*                                                                               
DLX      B     XIT                                                              
         EJECT                                                                  
* VALIDATE THE LIST OF RECORDS ON THE SCREEN.  FOR EACH LINE THAT HAD           
* A RECORD THERE BEFORE THERE WILL BE A DISK ADDRESS TABLE ENTRY FOR            
* THAT RECORD.                                                                  
*                                                                               
VALILST  NTR1                                                                   
         XC    SVACAT,SVACAT       CLEAR SAVED A(FIELDS ABOVE)                  
         XC    SVAONOF,SVAONOF                                                  
         XC    SVAUNIT,SVAUNIT                                                  
         XC    SVAUNLO,SVAUNLO                                                  
         XC    SVAYEAR,SVAYEAR                                                  
*                                  TRANSMIT ALL RECORD DATA FIELDS              
         GOTO1 FLDVAL,DMCB,(X'02',SCLL1H),999                                   
*                                                                               
         L     R3,ITEM             R3 = A(TOP OF PAGE DISK ADDRESS)             
         SLL   R3,2                                                             
         LA    R3,DALIST(R3)                                                    
         LA    R2,SCLL1H           R2 = A(FIRST LINE ON SCREEN)                 
*                                                                               
VL10     BAS   RE,LSETADDR         SET ADDRESSES OF FIELDS ON THIS LINE         
*                                                                               
         BAS   RE,VALISEL          TEST VALID DATA IN SELECT FIELD              
*                                                                               
         BAS   RE,TESTLINE         IF DISPLAY LINE EMPTY OR UNCHANGED           
         BE    VL20                                                             
*                                                                               
         L     R2,ARECFLD          THEN IF DISPLAY LINE IS EMPTY AND            
         CLI   8(R2),C' '              THERE WAS A RECORD THERE BEFORE          
         BH    VL90                    AND IT HASN'T DISAPPEARED                
         OC    0(4,R3),0(R3)                                                    
         BZ    VL90                                                             
         BAS   RE,READREC                                                       
         BE    VL80                THEN REDISPLAY RECORD                        
*                                                                               
         B     VL90                ELSE SKIP DISPLAY LINE                       
*                                                                               
VL20     OC    0(4,R3),0(R3)       ELSE IF RECORD WAS THERE BEFORE              
         BZ    VL50                                                             
*                                                                               
         MVI   VALMODE,C'C'        SET VALIDATION MODE TO CHANGE                
*                                                                               
         BAS   RE,READREC          READ OLD CAST RECORD                         
         BNE   VL90                SKIP THIS LINE IF REC DISAPPEARED            
*                                                                               
         MVC   AIO,AIO3            READ W4 RECORD INTO IO3                      
         L     R2,ASSNFLD          R2 = A(SSN FIELD)                            
         BRAS  RE,UNPKPID                                                       
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',8(R2))                                
         MVC   AIO,AIO1                                                         
         BNE   VL80                SKIP THIS LINE IF W4 REC DISAPPEARED         
         BRAS  RE,PKSSN                                                         
*                                                                               
         GOTO1 TESTANN,DMCB,-1     TEST ANN STATUS AFTER DELETE OLD             
*                                                                               
         BAS   RE,VALILINE         VALIDATE AND BUILD RECORD                    
         BNE   VL80                SKIP IF CHANGES NOT ALLOWED                  
*                                                                               
         MVI   RDUPDATE,C'Y'       READ AND LOCK OLD CAST RECORD INTO           
         MVC   AIO,AIO2                VOID                                     
         BAS   RE,READREC                                                       
         MVC   AIO,AIO1                                                         
         BNE   VL90                SKIP THIS LINE IF REC DISAPPEARED            
*                                                                               
         BAS   RE,WRITENEW         WRITE BACK NEW CAST RECORD                   
*                                                                               
         GOTO1 TESTANN,DMCB,1      TEST ANN STATUS AFTER ADDING NEW             
         B     VL80                                                             
*                                                                               
VL50     MVI   VALMODE,C'A'        ELSE SET VALIDATION MODE TO ADD              
*                                                                               
         BAS   RE,VRSSN            VALIDATE SSN                                 
*                                                                               
         BAS   RE,INITADD          INITIALIZE IOAREA FOR NEW RECORD             
*                                                                               
         BAS   RE,VALILINE         VALIDATE AND BUILD RECORD                    
*                                                                               
         BAS   RE,ADDNEW           ADD NEW CAST RECORD TO FILE                  
*                                                                               
         GOTO1 TESTANN,DMCB,1      TEST ANN STATUS AFTER ADDING NEW             
*                                                                               
VL80     L     R2,ASSNFLD                                                       
         BRAS  RE,PKSSN            CONVERT SSN TO PID                           
         BAS   RE,DISPLINE         DISPLAY BACK RECORD                          
*                                                                               
VL90     LA    R3,4(R3)            BUMP TO NEXT D/A IN LIST                     
*                                                                               
*                                  MAKE ALL FIELDS VALID                        
         GOTO1 FLDVAL,DMCB,(X'20',ASSNFLD),(X'80',ACHCMFLD)                     
*                                                                               
         MVC   SVACAT,ACATFLD      SAVE A(FIELDS ABOVE)                         
         MVC   SVAONOF,AONOFFLD                                                 
         MVC   SVAUNIT,AUNITFLD                                                 
         MVC   SVAUNLO,AUNLOFLD                                                 
         MVC   SVAYEAR,AYEARFLD                                                 
*                                                                               
         L     R2,ANEXTLIN         R2 = A(NEXT DISPLAY LINE)                    
*                                                                               
         LA    R1,SCLCOMMH         REPEAT UNTIL END OF SCREEN                   
         CR    R2,R1                                                            
         BL    VL10                                                             
*                                                                               
         CLI   RECCHANG,C'Y'       IF ANY RECORD HAS CHANGED                    
         BNE   *+8                                                              
         BAS   RE,UPDCOMM          THEN UPDATE COMMERCIAL STATUS                
*                                                                               
VLX      B     XIT                                                              
         EJECT                                                                  
* TEST DIRECTIONAL PFKEYS PRESSED.                                              
*                                                                               
TESTPFKS NTR1                                                                   
         L     R3,ITEM             R3 = ITEM NUMBER                             
         L     R4,NUMITEMS         R4 = NUMBER OF ITEMS - 1                     
         BCTR  R4,0                                                             
*                                                                               
         CLI   PFAID,8             IF PFKEY = FIRST                             
         BNE   TP10                                                             
*                                  THEN REBUILD DISK ADDRESS LIST               
         GOTO1 ABLDLIST,DMCB,1,(RC)                                             
*                                                                               
         SR    R3,R3               SET ITEM TO FIRST PAGE                       
         B     TP100                                                            
*                                                                               
TP10     CLI   PFAID,7             ELSE IF PFKEY = PREVIOUS                     
         BNE   TP20                                                             
         S     R3,=A(NUMSCR)       SET ITEM TO PREVIOUS PAGE                    
         B     TP100                                                            
*                                                                               
TP20     CLI   PFAID,0             ELSE IF PFKEY = NEXT (ENTER)                 
         BNE   TP30                                                             
         A     R3,=A(NUMSCR)       SET ITEM TO NEXT PAGE                        
*                                                                               
         CR    R3,R4               IF ITEM IS PAST LAST ITEM                    
         BNH   TP100                                                            
*                                  THEN REBUILD DISK ADDRESS LIST               
         GOTO1 ABLDLIST,DMCB,1,(RC)                                             
*                                                                               
         SR    R3,R3               SET ITEM TO FIRST PAGE                       
         B     TP100                                                            
*                                                                               
TP30     CLI   PFAID,14            ELSE IF PFKEY = LAST                         
         BNE   ERRPFK                                                           
         LR    R3,R4               THEN SET ITEM TO LAST ITEM                   
*                                                                               
TP100    LTR   R3,R3               IF ITEM IS LESS THAN ZERO                    
         BNM   *+6                                                              
         SR    R3,R3               THEN SET ITEM TO FIRST PAGE                  
*                                                                               
         ST    R3,ITEM             SAVE ITEM NUMBER                             
*                                                                               
TPX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY RECORD TO DISPLAY LINE.                                               
*                                                                               
DISPLINE NTR1                                                                   
         L     R4,AIO              SET A(CAST ELEMENT)                          
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R4,ACAELEM                                                       
*                                  MAKE SSN FIELD PROT/NORM INTENSITY           
         GOTO1 FLDVAL,DMCB,(X'08',ASSNFLD),(X'10',ASSNFLD)                      
*                                                                               
         L     R2,ASSNFLD          CLEAR THIS RECORDS FLDS                      
         L     R3,ACHCMFLD                                                      
         TWAXC (R2),(R3),PROT=Y                                                 
*                                                                               
         BAS   RE,DRSSN            DISPLAY SSN FIELD                            
         BNE   DLIN5               ABORT IF W4 RECORD NOT FOUND                 
*                                                                               
         BAS   RE,DRCAT            DISPLAY CATEGORY FIELD                       
         BAS   RE,DRONOF                   CAMERA FIELD                         
         BAS   RE,DRUNIT                   TAX UNIT FIELD                       
         BAS   RE,DRNCDE                   AGENT FIELD                          
         BAS   RE,DRUNLO                   UNION/LOCAL FIELD                    
         BAS   RE,DRYEAR                   YEAR FIELD                           
*        BAS   RE,DRLIFT                   LIFT FIELD                           
         BAS   RE,DRCORP                   CORPORATION FIELD                    
         BAS   RE,DRPERF                   PERFORMANCE RATE                     
         BAS   RE,DRWAMT                   WAGES AND WARDROBE FEE FLDS          
         BAS   RE,DRCHCM                   CHECK COMMENT FIELD                  
         L     RE,ACHCTFLD                                                      
         MVC   8(4,RE),=C'Cmnt'            CMNT TAG                             
*                                                                               
         BAS   RE,DREPI                    EPISODE NUMBERS                      
         L     RE,AEPIFLD                                                       
         MVC   8(8,RE),=C'Episodes'        EPISODES TAG                         
*                                                                               
DLIN5    GOTO1 SETLINE,DMCB,ARECFLD        SET PROT/UNPROTECT                   
         B     XIT                                                              
         EJECT                                                                  
* SET PROT/UNPROT AND CLEAR LINE                                                
*                                                                               
CLRLINE  NTR1                                                                   
         GOTO1 SETLINE,DMCB,ASSNFLD        SET PROT/UNPROTECT                   
*                                                                               
         L     R2,ASSNFLD                                                       
         L     R3,ACHCMFLD                                                      
         TWAXC (R2),(R3),PROT=Y    CLEAR FIELDS                                 
         B     XIT                                                              
         SPACE                                                                  
* SET FIELDS PROTECTED OR UNPROTECTED DEPENDING ON ACTION                       
*                                                                               
SETLINE  NTR1                                                                   
         L     R3,0(R1)            A(START FIELD)                               
         CLI   ACTNUM,ACTLDEL      IF USER WANTS DELETED RECORDS                
         BE    SL5                                                              
         CLI   ACTNUM,ACTPDEL      IF USER DELETING PAGE                        
         BNE   SL10                                                             
*                                  THEN PROTECT ALL RECORD DATA FIELDS          
SL5      GOTO1 FLDVAL,DMCB,(X'08',(R3)),ACHCMFLD                                
         B     SLX                                                              
*                                  ELSE UNPROTECT                               
SL10     GOTO1 FLDVAL,DMCB,(X'04',(R3)),AWARDFLD                                
         L     RE,ACHCMFLD         COMMENT FIELD GETS UNPROTECTED               
         NI    1(RE),X'DF'                                                      
         L     R2,AEPIFLD          EPISODES GET UNPROTECTED                     
         BAS   RE,BUMP                                                          
         USING EPID,R2                                                          
         LA    RE,5                                                             
SL15     NI    EPIINPH+1,X'DF'                                                  
         LA    R2,EPILNQ(R2)                                                    
         BCT   RE,SL15                                                          
         DROP  R2                                                               
*                                  SET ALL DATA FIELDS VALID                    
         GOTO1 FLDVAL,DMCB,(X'20',(R3)),ACHCMFLD                                
SLX      B     XIT                                                              
         SPACE                                                                  
* TEST IF DISPLAY LINE HAS CHANGED.  IF IT HAS THEN SET RECCHANG TO 'Y'         
* AND RETURN 'YES'.  OTHERWISE RETURN 'NO'.                                     
*                                                                               
TESTLINE NTR1                                                                   
         CLI   ACTNUM,ACTLDEL      IF USER WANTS DELETED RECORDS                
         BE    TLNO                THEN DISPLAY LINE CAN'T CHANGE               
*                                                                               
         L     R2,ASSNFLD          R2 = A(FIRST UNPROTECTED FIELD)              
         TM    1(R2),X'20'                                                      
         BZ    *+8                                                              
         L     R2,ARECFLD                                                       
         GOTO1 TESTCHG,DMCB,ACHCMFLD  DETERMINE IF ANYTHING CHANGED             
         BNE   TLNO                                                             
*                                                                               
TLYES    B     YES                 RETURN 'YES'                                 
TLNO     B     NO                  RETURN 'NO'                                  
         EJECT                                                                  
* VALIDATE DISPLAY LINE AND BUILD RECORD.                                       
*                                                                               
VALILINE NTR1                                                                   
         CLI   ACCESSED,C'Y'       IF NOT FULLY ACCESSED                        
         BE    VLIN05                                                           
         CLI   VALMODE,C'A'        AND ADDING                                   
         BE    ERRNOIP             THEN INPUT NOT ALLOWED                       
*                                                                               
         CLI   ACCESSED,C'N'       IF NOT ACCESSED AT ALL                       
         BE    NO                  THEN GET OUT NOW                             
*                                                                               
VLIN05   CLI   VALMODE,C'C'        IF VALIDATION MODE IS CHANGE                 
         BNE   VLIN10                                                           
*                                  SAVE POINTER BLOCK                           
         GOTO1 SAVPTRS,DMCB,PPBLOCK                                             
         B     VLIN20                                                           
*                                                                               
VLIN10   XC    PPBLOCK,PPBLOCK     ELSE CLEAR POINTER BLOCK                     
*                                                                               
VLIN20   L     R4,AIO              IF OLD CAST ELEMENT NOT FOUND                
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    VLIN30                                                           
         USING TACAD,R4                                                         
*                                                                               
         LA    R4,CAELEM           THEN BUILD NEW CAST ELEM IN CAELEM           
         XC    CAELEM,CAELEM                                                    
         MVI   TACAEL,TACAELQ                                                   
         MVI   TACALEN,TACALNQ                                                  
         B     VLIN50                                                           
*                                                                               
VLIN30   XC    CAELEM,CAELEM       ELSE COPY OLD CAST ELEMENT FOR THE           
         ZIC   RF,TACALEN              OLD LENGTH TO CAELEM                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CAELEM(0),0(R4)                                                  
*                                                                               
         MVI   CAELEM+1,TACALNQ    SET NEW LENGTH IN CAELEM                     
*                                                                               
         GOTO1 REMELEM             REMOVE OLD CAST ELEMENT                      
*                                                                               
VLIN50   LA    R4,CAELEM           SAVE A(CAST ELEMENT)                         
         ST    R4,ACAELEM                                                       
*                                                                               
         BAS   RE,VRCAT            VALIDATE CATEGORY FIELD                      
         BAS   RE,VRONOF                    CAMERA FIELD                        
         BAS   RE,VRUNIT                    TAX UNIT FIELD                      
         BAS   RE,VRNCDE                    AGENT FIELD                         
         BAS   RE,VRUNLO                    UNION/LOCAL FIELD                   
         BAS   RE,VRYEAR                    YEAR FIELD                          
         BAS   RE,VRCORP                    CORPORATION                         
         BAS   RE,VRPERF                    PERFORMANCE RATE                    
         BAS   RE,VRWAGE                    WAGES FIELD                         
         BAS   RE,VRWARD                    WARDROBE FEE FIELD                  
         BAS   RE,VRCHCM                    CHECK COMMENTS FIELD                
*                                                                               
*                                  ADD NEW CAST ELEMENT TO RECORD               
VLIN70   MVC   ELEM(TACALNQ),CAELEM                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         BAS   RE,VREPI            VALIDATE EPISODE NUMBERS                     
*                                                                               
         GOTO1 ACTVIN,DMCB,0       UPDATE LAST CHANGED ELEMENT                  
*                                                                               
         GOTO1 ABLDSORT,DMCB,(RC)  UPDATE SORT KEY                              
*                                                                               
VLINX    B     YES                                                              
         EJECT                                                                  
* SET ADDRESSES OF FIELDS ON CURRENT DISPLAY LINE.                              
LSETADDR NTR1                                                                   
         ST    R2,ASELFLD          SAVE A(SEL FIELD)                            
         BAS   RE,BUMP                                                          
         ST    R2,ASSNFLD          SAVE A(SSN FIELD)                            
         BAS   RE,BUMP                                                          
         ST    R2,ACATFLD          SAVE A(CATEGORY FIELD)                       
         ST    R2,ARECFLD          SAVE A(FIRST RECORD DATA FIELD)              
         BAS   RE,BUMP                                                          
         ST    R2,AONOFFLD         SAVE A(CAMERA FIELD)                         
         BAS   RE,BUMP                                                          
         ST    R2,AUNITFLD         SAVE A(TAX UNIT FIELD)                       
         BAS   RE,BUMP                                                          
         ST    R2,ANCDEFLD         SAVE A(AGENT FIELD)                          
         BAS   RE,BUMP                                                          
         ST    R2,AUNLOFLD         SAVE A(UNION/LOCAL FIELD)                    
         BAS   RE,BUMP                                                          
         ST    R2,AYEARFLD         SAVE A(YEAR FIELD)                           
         BAS   RE,BUMP                                                          
         ST    R2,ACORPFLD         SAVE A(CORPORATION FIELD)                    
         BAS   RE,BUMP                                                          
         ST    R2,APERFFLD         SAVE A(PERFORMANCE RATE FIELD)               
         BAS   RE,BUMP                                                          
         ST    R2,AWAGEFLD         SAVE A(WAGES FIELD)                          
         BAS   RE,BUMP                                                          
         ST    R2,AWARDFLD         SAVE A(WARDROBE FEE FIELD)                   
         BAS   RE,BUMP                                                          
         ST    R2,ANAMEFLD         SAVE A(NAMES FIELD)                          
         BAS   RE,BUMP                                                          
         ST    R2,AEPIFLD          SAVE A(EPI TAG FIELD)                        
         BAS   RE,BUMP                                                          
         LA    R1,EPITMAX          NUMBER OF EPISODES ON SCREEN                 
         MH    R1,=Y(EPILNQ)       SCREEN LENGTH OF AN EPISODE                  
         AR    R2,R1                                                            
         ST    R2,ACHCTFLD         SAVE A(CMNT TAG FIELD)                       
         BAS   RE,BUMP                                                          
         ST    R2,ACHCMFLD         SAVE A(CHECK COMMENT FIELD)                  
         BAS   RE,BUMP                                                          
         ST    R2,ANEXTLIN         SAVE A(NEXT SCREEN LINE)                     
LSAX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE TESTS IF THE DATA IN THE SELECT FIELD FOR THIS LINE IS           
* VALID BY LOOKING FOR A MATCHING PFTAB ENTRY.  IF NO MATCH IS FOUND AN         
* ERROR IS GIVEN.                                                               
*                                                                               
VALISEL  NTR1                                                                   
         L     R2,ASELFLD          R2 = A(SELECT FIELD)                         
         OC    8(3,R2),=CL7' '          PAD WITH SPACES                         
*                                                                               
         L     R3,ALPFTAB          R3 = A(FIRST PFTAB ENTRY)                    
         CLI   ACTNUM,ACTLDEL                                                   
         BNE   *+8                                                              
         L     R3,ADPFTAB                                                       
         USING PFTABD,R3                                                        
*                                                                               
VS10     CLI   0(R3),X'FF'         IF END OF PFTAB THEN ERROR                   
         BE    ERRINV                                                           
         CLC   PFTSEL,8(R2)        IF MATCH FOUND THEN VALID                    
         BE    VS20                                                             
         ZIC   RF,PFTLEN           BUMP TO NEXT PFTAB ENTRY                     
         AR    R3,RF                                                            
         B     VS10                LOOP BACK                                    
*                                                                               
VS20     CLC   PFTSEL(2),=C'C '    IF SELECTING FOR CHANGE                      
         BE    VS30                                                             
         CLC   PFTSEL(2),=C'DE'    OR DELETE                                    
         BE    VS30                                                             
         CLC   PFTSEL(2),=C'R '    OR RESTORE                                   
         BNE   VSX                                                              
VS30     CLI   ACCESSED,C'Y'       AND NOT FULLY ACCESSED                       
         BNE   ERRINV              THEN GIVE ERROR                              
*                                                                               
VSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE READS THE CAST RECORD WITH THE DISK ADDRESS POINTED TO           
* BY R3.  IF THE DELETE STATUS BIT DOES NOT MATCH THE ACTION TYPE (LIST         
* OR LIST DELETED) THEN THE ROUTINE SETS THE DISAPPR FLAG TO 'Y' AND            
* RETURNS 'NO'. OTHERWISE IT RETURNS 'YES'.                                     
*                                                                               
READREC  NTR1                                                                   
         LA    R4,KEY              READ CAST RECORD                             
         USING TLDRD,R4                                                         
         MVC   TLDRDA,0(R3)                                                     
         OI    DMINBTS,X'08'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   ACTNUM,ACTLDEL      IF ACTION IS NOT LDELETE                     
         BE    *+8                                                              
         NI    DMINBTS,X'F7'       THEN RESET PASS BACK DELETED FLAG            
*                                                                               
         L     R4,AIO              R4 = A(RECORD KEY)                           
         USING TLCAD,R4                                                         
*                                                                               
         CLI   ACTNUM,ACTLDEL      IF ACTION IS LIST DELETED                    
         BNE   RR10                                                             
         TM    TLCASTAT,X'40'      AND RECORD IS NOT DELETED                    
         BZ    RR20                                                             
         B     RRYES                                                            
*                                                                               
RR10     TM    TLCASTAT,X'80'      OR ACTION IS LIST OR VERIFY AND              
         BZ    RRYES                   RECORD IS DELETED                        
*                                                                               
RR20     MVI   DISAPPR,C'Y'        THEN SET RECORD HAS DISAPPEARED              
*                                                                               
RRNO     B     NO                  RETURN 'NO'                                  
*                                                                               
RRYES    B     YES                 RETURN 'YES'                                 
         SPACE 3                                                                
* WRITE BACK NEW CAST RECORD.  UPDATE PASSIVE POINTERS.                         
*                                                                               
WRITENEW NTR1                                                                   
*                                                                               
         GOTO1 PUTREC              WRITE BACK RECORD                            
*                                                                               
*                                  UPDATE PASSIVE POINTERS                      
         GOTO1 ADDPTRS,DMCB,(X'80',PPBLOCK)                                     
*                                                                               
         MVC   0(4,R3),DMDSKADD    UPDATE DISK ADDRESS OF RECORD                
*                                                                               
WNX      B     XIT                                                              
         EJECT                                                                  
* INITIALIZE IOAREA WITH KEY AND ZERO ELEMENTS.                                 
*                                                                               
INITADD  NTR1                                                                   
         XC    TGCASORT,TGCASORT   BUILD ACTIVE KEY                             
         GOTO1 RECVAL,DMCB,TLCACDQ,(X'C0',TGCAT)                                
*                                                                               
         L     R4,AIO              INITIALIZE RECORD WITH KEY                   
         USING TLCAD,R4                                                         
         XC    TLCAKEY(TLCAELEM+1-TLCAKEY),TLCAKEY                              
         MVC   TLCAKEY,KEY                                                      
*                                                                               
         MVC   TLCALEN,DATADISP    SET RECORD HAS NO ELEMS                      
*                                                                               
IAX      B     XIT                                                              
         EJECT                                                                  
* ADD NEW CAST RECORD.  FIRST ADD THE NEXT AVAILABLE SEQUENCE NUMBER TO         
* THE KEY.  THEN ADD THE RECORD AND ADD ITS DISK ADDRESS TO THE DISK            
* ADDRESS LIST.  THEN ADD THE PASSIVE POINTERS.  FINALLY, INCREMENT THE         
* NEXT AVAILABLE SEQUENCE NUMBER.                                               
*                                                                               
ADDNEW   NTR1                                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'B4',0)     GET COMML, RDUPDATE           
*                                                                               
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSEQ))                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R4,TGELEM                                                        
         MVC   NEXTSEQ,TANUNXTC    SAVE NEXT CAST SEQUENCE NUMBER               
*                                                                               
         ZICM  RE,TANUNXTC,2                                                    
         AHI   RE,1                                                             
         STCM  RE,3,TANUNXTC                                                    
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R4,AIO              INSERT SEQUENCE NUMBER INTO SORT             
         USING TLCAKEY,R4                                                       
         MVC   TLCASEQ,NEXTSEQ                                                  
         GOTO1 ADDREC              ELSE ADD NEW CAST RECORD                     
*                                                                               
         MVC   0(4,R3),DMDSKADD    MOVE DISK ADDRESS TO LIST                    
*                                                                               
         L     RF,NUMITEMS         INCREMENT NUMBER OF ITEMS                    
         LA    RF,1(RF)                                                         
         ST    RF,NUMITEMS                                                      
*                                  ADD NEW POINTERS                             
         GOTO1 ADDPTRS,DMCB,PPBLOCK                                             
         B     XIT                                                              
         EJECT                                                                  
EDRIVER  DS    0H                                                               
*                                  INITIALIZE OVERLAY                           
         GOTO1 INITIAL,DMCB,AEPFTAB                                             
         BAS   RE,EPOINTER         SAVE OR ADD POINTERS                         
*                                                                               
         BAS   RE,ANNMODE          TEST MODE EFFECTS ANNOUNCER STATUS           
*                                                                               
         CLI   MODE,RECDEL         IF MODE IS RECDEL                            
         BNE   ED5                                                              
         L     R4,AIO              THEN DOUBLE DELETE RECORD                    
         USING TLCAD,R4                                                         
         OI    TLCASTAT,X'40'                                                   
         LA    R3,KEY              AND DOUBLE DELETE KEY                        
         USING TLDRD,R3                                                         
         OI    TLDRSTAT,X'40'                                                   
*                                                                               
ED5      CLI   MODE,RECREST        IF MODE IS RECREST                           
         BNE   ED10                                                             
         L     R4,AIO              THEN DOUBLE UNDELETE RECORD                  
         USING TLCAD,R4                                                         
         NI    TLCASTAT,X'BF'                                                   
         LA    R3,KEY              AND DOUBLE UNDELETE KEY                      
         USING TLDRD,R3                                                         
         NI    TLDRSTAT,X'BF'                                                   
*                                                                               
ED10     CLI   MODE,XRECPUT        IF MODE IS XRECPUT, XRECDEL, OR              
         BE    ED20                    XRECREST                                 
         CLI   MODE,XRECDEL                                                     
         BE    ED20                                                             
         CLI   MODE,XRECREST                                                    
         BNE   ED30                                                             
*                                                                               
ED20     BAS   RE,UPDCOMM          THEN UPDATE COMMERCIAL STATUS                
*                                                                               
ED30     CLI   PUSHED,C'Y'         IF CAST LIST CALLED THIS SCREEN              
         BNE   *+8                                                              
         BAS   RE,ECLIST           THEN TAKE CARE OF SPECIAL SITUATIONS         
*                                                                               
         BAS   RE,ESETADDR         SET ADDRESSES OF SCREEN FIELDS               
*                                                                               
         CLI   MODE,VALKEY         IF MODE IS NOT VALKEY                        
         BE    ED50                                                             
         CLI   MODE,DISPKEY        OR DISPKEY                                   
         BE    ED50                                                             
*                                  AND SSN RECORD WASN'T FOUND                  
         CLC   =C'** NOT FOUND **',SCASSNN                                      
         BE    EDX                 THEN RETURN TO GENCON                        
*                                                                               
ED50     CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    ED100                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    ED110                                                            
         CLI   MODE,DISPREC        DISPLAY THE RECORD                           
         BE    ED120                                                            
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BE    ED130                                                            
         CLI   MODE,XRECPUT        DISPLAY THE RECORD                           
         BE    ED120                                                            
         CLI   MODE,XRECDEL        DISPLAY THE RECORD                           
         BE    ED120                                                            
         CLI   MODE,XRECREST       DISPLAY THE RECORD                           
         BE    ED120                                                            
         B     EDX                                                              
*                                                                               
ED100    BAS   RE,EVALKEY          VALIDATE KEY                                 
         B     EDX                                                              
*                                                                               
ED110    BAS   RE,EDISPKEY         DISPLAY KEY                                  
         B     EDX                                                              
*                                                                               
ED120    BAS   RE,EDISPREC         DISPLAY THE RECORD                           
         B     EDX                                                              
*                                                                               
ED130    BAS   RE,EVALREC          VALIDATE THE RECORD                          
*                                                                               
EDX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE HANDLES PROBLEMS THAT PERTAIN ONLY TO THE SITUATION              
* WHERE THE CAST LIST CALLS THE EXTENSION SCREEN USING THE 'PUSH' ENTRY         
* FOUND IN THE CAST LIST'S PF TABLE.  SINCE THE SCREEN HAS BEEN CALLED          
* WITH A 'PUSH' AS OPPOSED TO GENCON'S STANDARD LIST LOGIC, CERTAIN             
* TEST'S WILL BE MADE TO DETERMINE IF IT IS TIME TO RETURN TO THE CAST          
* LIST VIA A 'POP'.                                                             
*                                                                               
ECLIST   NTR1                                                                   
         CLI   ACTNUM,ACTCHA       IF ACTION IS NOT CHANGE                      
         BE    ECL10                                                            
         CLI   SCRCHANG,C'N'       AND THIS IS SECOND TIME FOR SCREEN           
         BE    ECL20                                                            
*                                                                               
ECL10    CLI   MODE,XRECPUT        OF IF THE MODE IS XRECPUT                    
         BNE   ECLX                                                             
         TM    TRNSTAT,USERCHA     AND USER DIDN'T SET ACTION TO 'CHA'          
         BO    ECLX                                                             
*                                                                               
ECL20    CLI   ACTNUM,ACTCHA       THEN IF ACTION IS CHANGE                     
         BNE   ECL50                                                            
         MVC   DALPTR,EDALPTR      THEN SAVE POINTER TO D/A LIST ENTRY          
         MVC   CASTDA,EDISKADD     AND NEW DISK ADDRESS TO PUT THERE            
*                                                                               
ECL50    MVI   PFAID,21            POP BACK TO LIST SCREEN                      
         GOTO1 INITIAL,DMCB,APOPTAB                                             
         DC    H'0'                *** ROUTINE SHOULD NEVER RETURN ***          
*                                                                               
ECLX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE EITHER SAVES OR ADDS PASSIVE POINTERS DEPENDING ON THE           
* GENCON MODE.                                                                  
*                                                                               
EPOINTER NTR1                                                                   
         CLI   MODE,VALREC         IF MODE VALREC, RECDEL, OR RECREST           
         BE    EP10                                                             
         CLI   MODE,RECDEL                                                      
         BE    EP10                                                             
         CLI   MODE,RECREST                                                     
         BNE   EP20                                                             
*                                  THEN SAVE CURRENT PASSIVE POINTER(S)         
EP10     GOTO1 SAVPTRS,DMCB,PPBLOCK                                             
         B     EPX                                                              
*                                                                               
EP20     CLI   MODE,XRECPUT        ELSE IF MODE IS XRECPUT                      
         BNE   EP40                                                             
*                                  THEN UPDATE PASSIVE POINTER(S)               
EP30     GOTO1 ADDPTRS,DMCB,(X'80',PPBLOCK)                                     
*                                                                               
         L     RF,AIO              SAVE NEW KEY IN TWAKEYSV                     
         MVC   TWAKEYSV,0(RF)                                                   
         MVC   EDISKADD,DMDSKADD   SAVE NEW D/A IN EDISKADD                     
         B     EPX                                                              
*                                                                               
EP40     CLI   MODE,XRECADD        ELSE IF MODE XRECADD, XRECDEL, OR            
         BE    EP50                    XRECREST                                 
         CLI   MODE,XRECDEL                                                     
         BE    EP50                                                             
         CLI   MODE,XRECREST                                                    
         BNE   EP60                                                             
*                                  THEN ADD PASSIVE POINTER(S)                  
EP50     GOTO1 ADDPTRS,DMCB,PPBLOCK                                             
*                                                                               
EP60     DS    0H                                                               
*                                                                               
EPX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE TESTS WHETHER THE MODE IS ONE THAT WILL EFFECT THE               
* ANNOUNCER-ONLY-NON-MUSICIAN STATUS, AND IF IT IT, IT WILL CALL                
* TESTANN WITH THE PROPER PARAMETER FOR THE MODE.                               
*                                                                               
ANNMODE  NTR1                                                                   
         CLI   MODE,XRECDEL        IF JUST DELETED A RECORD                     
         BNE   AM10                                                             
         GOTO1 TESTANN,DMCB,-1     THEN TEST STATUS AFTER DELETING              
         B     AMX                                                              
*                                                                               
AM10     CLI   MODE,XRECREST       IF JUST RESTORED A RECORD                    
         BNE   AM20                                                             
         GOTO1 TESTANN,DMCB,1      THEN TEST STATUS AFTER RESTORING             
         B     AMX                                                              
*                                                                               
AM20     CLI   MODE,VALREC         IF ABOUT TO CHANGE A RECORD                  
         BNE   AM30                                                             
         GOTO1 TESTANN,DMCB,-1     THEN TEST STATUS AFTER DELETING OLD          
         B     AMX                                                              
*                                                                               
AM30     CLI   MODE,XRECPUT        IF JUST CHANGED A RECORD                     
         BNE   AMX                                                              
         GOTO1 TESTANN,DMCB,1      THEN TEST STATUS AFTER ADDING NEW            
*                                                                               
AMX      B     XIT                                                              
         EJECT                                                                  
* SET ADDRESSES OF FIELDS ON EXTENSION SCREEN.                                  
*                                                                               
ESETADDR NTR1                                                                   
         LA    R2,SCACATH          SAVE A(CATEGORY FIELD)                       
         ST    R2,ACATFLD                                                       
         ST    R2,ARECFLD          SAVE A(FIRST RECORD FIELD)                   
         LA    R2,SCAONOFH         SAVE A(CAMERA FIELD)                         
         ST    R2,AONOFFLD                                                      
         LA    R2,SCAUNITH         SAVE A(TAX UNIT FIELD)                       
         ST    R2,AUNITFLD                                                      
         LA    R2,SCAUNLOH         SAVE A(UNION/LOCAL FIELD)                    
         ST    R2,AUNLOFLD                                                      
         LA    R2,SCAYEARH         SAVE A(YEAR FIELD)                           
         ST    R2,AYEARFLD                                                      
         LA    R2,SCACORPH         SAVE A(CORPORATION FIELD)                    
         ST    R2,ACORPFLD                                                      
         LA    R2,SCACORNH         SAVE A(CORPORATION NAME FIELD)               
         ST    R2,ACORNFLD                                                      
         LA    R2,SCANCDEH         SAVE A(AGENT FIELD)                          
         ST    R2,ANCDEFLD                                                      
         LA    R2,SCANCDNH         SAVE A(AGENT NAME FIELD)                     
         ST    R2,ANCDNFLD                                                      
         LA    R2,SCAGUARH         SAVE A(GUARANTEE FIELD)                      
         ST    R2,AGUARFLD                                                      
         LA    R2,SCAGUADH         SAVE A(GUARANTEE DESC. FIELD)                
         ST    R2,AGUADFLD                                                      
         LA    R2,SCASTATH         SAVE A(STATUS FIELD)                         
         ST    R2,ASTATFLD                                                      
         LA    R2,SCAOPH           SAVE A(OVERSCALE PERCENTAGE FIELD)           
         ST    R2,AOPFLD                                                        
         LA    R2,SCAPRATH         SAVE A(PERFORMANCE RATE FIELD)               
         ST    R2,APERFFLD                                                      
         LA    R2,SCAWAGH          SAVE A(TOTAL WAGES FIELD)                    
         ST    R2,AWAGEFLD                                                      
         LA    R2,SCAWFEEH         SAVE A(TOTAL WAGES FIELD)                    
         ST    R2,AWARDFLD                                                      
         LA    R2,SCACMH           SAVE A(COMMENTS FIELD)                       
         ST    R2,ACMFLD                                                        
         LA    R2,SCACHCMH         SAVE A(CHECK COMMENTS FIELD)                 
         ST    R2,ACHCMFLD                                                      
         LA    R2,SCAEPITH         SAVE A(EPISODE NUMBERS FIELD)                
         ST    R2,AEPIFLD                                                       
*                                                                               
ESAX     B     XIT                                                              
         EJECT                                                                  
* EXTENSION SCREEN MODE VALKEY ROUTINE.  THE ROUTINE BUILDS THE KEY             
* BY READING THE RECORD WITH THE DISK ADDRESS FOUND IN EDISKADD AND             
* EXTRACTING ITS KEY.                                                           
*                                                                               
EVALKEY  NTR1                                                                   
         LA    R3,KEY              R3 = A(KEY)                                  
         USING TLDRD,R3                                                         
*                                                                               
         MVC   TLDRDA,EDISKADD     READ RECORD POINTED TO BY EDISKADD           
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO              R4 = A(RECORD)                               
         USING TLCAD,R4                                                         
*                                                                               
         XC    KEY,KEY             EXTRACT KEY FROM RECORD                      
         MVC   TLDRKEY,TLCAKEY                                                  
*                                                                               
         TM    TLCASTAT,X'80'      IF RECORD IS NOT DELETED                     
         BO    *+8                                                              
         NI    DMINBTS,X'F7'       THEN RESET PASS BACK DELETED FLAG            
*                                                                               
         BAS   RE,ESCRKEY          DISPLAY THE KEY OF THE RECORD                
*                                                                               
EVKX     B     XIT                                                              
         EJECT                                                                  
* EXTENSION SCREEN MODE DISPKEY ROUTINE.  THE ROUTINE SAVES THE DISK            
* ADDRESS OF THE RECORD IN CASE THE USER PRESSES PF2 CAUSING THE ACTION         
* TO BE CHANGE AND IN TURN CAUSING GENCON TO CALL THIS OVERLAY WITH             
* MODE VALKEY (THE ROUTINE EVALKEY ASSUMES THE DISK ADDRESS WAS                 
* RETRIEVED THE FIRST TIME THROUGH).  THEN THE ROUTINE CALLS THE LOWER          
* LEVEL ROUTINE TO DISPLAY THE KEY.                                             
*                                                                               
EDISPKEY NTR1                                                                   
         LA    R3,KEY              R3 = A(CAST KEY)                             
         USING TLDRD,R3                                                         
         MVC   EDISKADD,TLDRDA     SAVE D/A IN CASE USER PRESSES PF2            
*                                                                               
         BAS   RE,ESCRKEY          DISPLAY THE RECORD'S KEY TO THE SCR          
*                                                                               
EDKX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE INFORMATION FOUND IN THE KEY OF THE RECORD          
* TO THE SCREEN.  IT MUST READ THE COMMERCIAL RECORD TO DISPLAY THE             
* COMMERCIAL INFO.                                                              
*                                                                               
ESCRKEY  NTR1                                                                   
         L     R4,AIO              R4 = A(ACTIVE KEY)                           
         USING TLCAD,R4                                                         
* NO-OP  MVC   SCAAGY,TLCAAGY      DISPLAY AGENCY                               
         MVC   SCASSN,TLCASSN      DISPLAY SSN                                  
*                                                                               
         GOTO1 HEXOUT,DMCB,TLCASORT,SCASORT+1,6,0 DISPLAY CAST SORT KEY         
         OI    SCASORTH+6,X'80'                                                 
         CLI   TGCTSTTY,TASTTYPP   IF CONNECTED STAFF IS A PROGRAMMER           
         BNE   *+8                                                              
         NI    SCASORTH+1,X'FB'    SET TO HIGH INTENSITY (ELSE ZERO)            
*                                                                               
         MVC   AIO,AIO2            READ COMMERCIAL RECORD AND DISP NAME         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A8',TLCACOM),SCACIDNH                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO              R4 = A(COMM'L RECORD)                        
         USING TLCOD,R4                                                         
         MVC   SCAAGY,TLCOAGY      DISPLAY AGENCY                               
         LA    R2,SCACIDH                                                       
         BAS   RE,GETCOMM          GET COMMERCIAL INFO FROM RECORD              
*                                                                               
         LA    R4,COELEM           R4 = A(COMMERCIAL DETAILS ELEMENT)           
         USING TACOD,R4                                                         
*                                                                               
         MVC   SCACID,TACOCID      DISPLAY COMMERCIAL ID                        
         OI    SCACIDH+6,X'80'                                                  
*                                                                               
         MVC   AIO,AIO3            DISPLAY W4 NAME                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',SCASSN),SCASSNNH                      
         GOTO1 SSNPACK,DMCB,SCASSN,TGPID                                        
         MVC   SCASSN,SSNSPACE                                                  
         MVC   SCASSN(L'TGPID),TGPID                                            
         MVI   SCASSNH+5,6                                                      
         OI    SCASSNH+6,X'80'                                                  
*                                                                               
ESK10    MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         CLI   ERROR,0             IF W4 RECORD NOT FOUND                       
         BE    *+10                THEN INDICATE THAT IN NAME FIELD             
         MVC   SCASSNN(16),=C'** NOT FOUND ** '                                 
*                                                                               
         L     RF,AIO              RESTORE KEY FROM RECORD                      
         MVC   KEY(L'TLDRKEY),0(RF)                                             
*                                                                               
ESKX     B     XIT                                                              
         EJECT                                                                  
* EXTENSION SCREEN MODE DISPREC ROUTINE.                                        
*                                                                               
EDISPREC NTR1                                                                   
         XC    NAMESH,NAMESH       CLEAR NAMES FIELD                            
         MVI   NAMESH,8+16         SET LENGTH OF FAKE NAMES FIELD               
*                                                                               
         L     R2,ARECFLD          CLEAR ALL RECORD FIELDS                      
         TWAXC (R2)                                                             
*                                                                               
         XC    SCACORN,SCACORN     CLEAR NAME FLDS                              
         OI    SCACORNH+6,X'80'                                                 
         XC    SCANCDN,SCANCDN                                                  
         OI    SCANCDNH+6,X'80'                                                 
*                                                                               
         XC    SCAGUAD,SCAGUAD     CLEAR GUARANTEE DESC. FIELD                  
         OI    SCAGUADH+6,X'80'                                                 
*                                                                               
         L     R4,AIO              SET A(CAST ELEMENT)                          
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R4,ACAELEM                                                       
*                                                                               
         MVC   AIO,AIO3            READ W4 RECORD INTO IO3 FOR DRCORP           
         CLI   SCASSNH+5,6                                                      
         BH    EDR02                                                            
         MVC   TGPID,SCASSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   EDR02                                                            
         MVC   SCASSN,TGSSN                                                     
         MVI   SCASSNH+5,9                                                      
EDR02    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',SCASSN)                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SCASSN,SSNSPACE                                                  
         MVC   SCASSN(L'TGPID),TGPID                                            
         MVI   SCASSNH+5,6                                                      
         OI    SCASSNH+6,X'80'                                                  
EDR04    MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         BAS   RE,DRCAT            DISPLAY CATEGORY FIELD                       
         BAS   RE,DRONOF                   CAMERA FIELD                         
         BAS   RE,DRUNIT                   TAX UNIT FIELD                       
         BAS   RE,DRUNLO                   UNION/LOCAL FIELD                    
         BAS   RE,DRYEAR                   YEAR FIELD                           
         BAS   RE,DRCORP                   CORPORATION FIELD                    
         BAS   RE,DRNCDE                   AGENT FIELD                          
         BAS   RE,DRGUAR                   GUARANTEE FIELD                      
         BAS   RE,DRSTAT                   STATUS FIELD                         
         BAS   RE,DROP                     OVERSCALE PERCENTAGE FIELD           
         BAS   RE,DRPERF                   PERFORMANCE RATE                     
         BAS   RE,DRWAMT                   WAGES AND WARDROBE FIELD             
         BAS   RE,DRCM                     COMMENTS FIELD                       
         BAS   RE,DRCHCM                   CHECK COMMENTS FIELD                 
*                                                                               
         BAS   RE,DREPI            DISPLAY EPISODE NUMBERS                      
*                                                                               
*                                  DISPLAY LAST CHANGED ELEMENT                 
         GOTO1 ACTVOUT,DMCB,SCALCHGH                                            
*                                                                               
*                                  PROTECT ALL KEY FIELDS                       
         GOTO1 FLDVAL,DMCB,(X'08',SCAAGYH),SCASSNH                              
*                                                                               
*                                  SET ALL RECORD DATA FIELDS VALID             
         GOTO1 FLDVAL,DMCB,(X'20',ARECFLD),(X'80',999)                          
*                                                                               
         L     R2,ARECFLD          POSITION CURSOR TO FIRST RECORD FLD          
         OI    6(R2),X'40'                                                      
*                                                                               
         CLI   ACTNUM,ACTDIS       IF ACTION DISPLAY                            
         BNE   EDR10                                                            
         MVI   MYMSGNO1,11         THEN GIVE SELECTION DISPLAYED MSG            
         B     EDR30                                                            
*                                                                               
EDR10    CLI   ACTNUM,ACTDEL       ELSE IF ACTION DELETE                        
         BNE   EDR20                                                            
         MVI   MYMSGNO1,13         THEN GIVE SELECTION DELETED MESSAGE          
*                                                                               
EDR20    CLI   ACTNUM,ACTREST      ELSE IF ACTION RESTORE                       
         BNE   EDRX                                                             
         MVI   MYMSGNO1,18         THEN GIVE SELECTION RESTORED MESSAGE         
*                                                                               
EDR30    MVI   MYMSYS,X'FF'        SET GENCON TO USE GETTXT                     
         OI    GENSTAT2,USGETTXT                                                
         B     ERREXIT                                                          
*                                                                               
EDRX     B     XIT                                                              
         EJECT                                                                  
* EXTENSION SCREEN MODE VALREC ROUTINE.                                         
*                                                                               
EVALREC  NTR1                                                                   
         MVI   RECCHANG,C'N'                                                    
         L     R2,ARECFLD                                                       
         GOTO1 TESTCHG,DMCB,999    DETERMINE IF ANYTHING CHANGED                
*                                                                               
         MVI   ELCODE,TACAELQ      REMOVE OLD CAST ELEMENT                      
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,CAELEM           BUILD NEW CAST ELEMENT                       
         XC    CAELEM,CAELEM                                                    
         USING TACAD,R4                                                         
         MVI   TACAEL,TACAELQ                                                   
         MVI   TACALEN,TACALNQ                                                  
*                                                                               
         ST    R4,ACAELEM          SAVE A(CAST ELEMENT)                         
*                                                                               
         MVC   AIO,AIO3            READ W4 RECORD INTO IO3 FOR VRCORP           
         CLI   SCASSNH+5,6                                                      
         BH    EVR10                                                            
         MVC   TGPID,SCASSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   EVR10                                                            
         MVC   SCASSN,TGSSN                                                     
         MVI   SCASSNH+5,9                                                      
EVR10    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',SCASSN)                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SCASSN,SSNSPACE                                                  
         MVC   SCASSN(L'TGPID),TGPID                                            
         MVI   SCASSNH+5,6                                                      
         OI    SCASSNH+6,X'80'                                                  
EVR20    MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         BAS   RE,VRCAT            VALIDATE CATEGORY FIELD                      
         BAS   RE,VRONOF                    CAMERA FIELD                        
         BAS   RE,VRUNIT                    TAX UNIT FIELD                      
         BAS   RE,VRUNLO                    UNION/LOCAL FIELD                   
         BAS   RE,VRYEAR                    YEAR FIELD                          
         BAS   RE,VRCORP                    CORPORATION                         
         BAS   RE,VRNCDE                    AGENT FIELD                         
         BAS   RE,VRGUAR                    GUARANTEE FIELD                     
         BAS   RE,VRSTAT                    STATUS FIELD                        
         BAS   RE,VROP                      OVERSCALE PERCENTAGE FIELD          
         BAS   RE,VRPERF                    PERFORMANCE RATE                    
         BAS   RE,VRWAGE                    TOTAL WAGES FIELD                   
         BAS   RE,VRWARD                    WARDROBE FEE FIELD                  
         BAS   RE,VRCM                      COMMENTS FIELD                      
         BAS   RE,VRCHCM                    CHECK COMMENTS FIELD                
         MVC   ELEM(TACALNQ),CAELEM         ADD NEW CAST ELEMENT                
         GOTO1 ADDELEM                                                          
*                                                                               
         BAS   RE,VREPI            VALIDATE EPISODE NUMBERS                     
*                                                                               
*                                  UPDATE LAST CHANGED ELEMENT                  
         GOTO1 ACTVIN,DMCB,SCALCHGH                                             
*                                                                               
         GOTO1 ABLDSORT,DMCB,(RC)  UPDATE SORT KEY                              
*                                                                               
         LA    R3,KEY              REREAD CAST RECORD FOR UPDATE BEFORE         
         USING TLDRD,R3                RETURNING TO GENCON                      
         MVC   TLDRDA,GLOBDA                                                    
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         DROP  R3                                                               
*                                                                               
EVRX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DETERMINES IF ANY FIELDS CHANGED                         
         SPACE 1                                                                
*                                  R2=A(FIRST FIELD)                            
TESTCHG  NTR1                                                                   
         L     R3,0(R1)            R3=A(LAST FIELD)                             
*                                                                               
*                                  IF SOME FLD IS NOT EMPTY                     
         GOTO1 FLDVAL,DMCB,(X'80',(R2)),(X'80',(R3))                            
         BE    NO                                                               
*                                  AND SOME FIELD HAS CHANGED                   
         GOTO1 FLDVAL,DMCB,(X'40',(R2)),(X'80',(R3))                            
         BE    NO                                                               
*                                                                               
         MVI   RECCHANG,C'Y'       SET SOMETHING CHANGED                        
         B     YES                                                              
         EJECT                                                                  
* THIS ROUTINE INSPECTS THE RECORD IN AIO AND DETERMINES HOW ITS                
* CATEGORY WILL EFFECT THE ANNOUNCER-ONLY-NON-MUSICIAN STATUS FOR               
* THE COMMERCIAL.  IF PARAMETER 1 IS 1 THEN THE ROUTINE TREATS THE              
* RECORD AS AN ADDITION OF A CATEGORY MEMBER.  IF PARAMETER 1 IS -1             
* THEN IT TREATS IT AS A SUBTRACTION.  FOR EXAMPLE, DELETES WILL CALL           
* THIS ROUTINE WITH A -1, ADDS AND RESTORES WITH A 1, AND CHANGES WITH          
* A -1 FOR THE OLD RECORD AND A 1 FOR THE NEW ONE.                              
*                                                                               
TESTANN  NTR1                                                                   
         L     R3,0(R1)            R3 = ADJUSTMENT INDEX (-1/1)                 
*                                                                               
         L     R4,AIO              LOOK UP CATTAB FOR THIS RECORD               
         USING TLCAD,R4                                                         
         GOTO1 CATVAL,DMCB,TLCACAT                                              
*                                                                               
         CLI   TGCAEQU,CTANN       IF CAST MEMBER IS AN ANNOUNCER               
         BNE   *+14                                                             
         L     RF,NUMANN           THEN ADJUST NUMBER OF ANNOUNCERS             
         AR    RF,R3                                                            
         ST    RF,NUMANN                                                        
*                                                                               
*        TM    TGCAUNI,AFM         IF CAST MEMBER IS AN NON-MUSICIAN            
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BO    *+14                                                             
         L     RF,NUMNONM          THEN ADJUST NUMBER OF NON-MUSICIANS          
         AR    RF,R3                                                            
         ST    RF,NUMNONM                                                       
*                                                                               
TAX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE EXTRACTS INFORMATION FFOM THE COMMERCIAL RECORD (FOUND           
* IN AIO) THAT BOTH THE CAST LIST AND EXTENSION SCREENS NEED.                   
* R2 = A(COMM ID FIELD)                                                         
*                                                                               
GETCOMM  NTR1                                                                   
         L     R4,AIO              R4 = A(COMMERCIAL REC)                       
         USING TLCOD,R4                                                         
         MVC   CMCLCLI,TLCOCLI     SAVE CLIENT CODE FOR LATER USE               
*                                                                               
         MVI   ELCODE,TACOELQ      R4 = A(COMMERCIAL DETAILS ELEMENT)           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
*                                                                               
         CLI   TACOTYPE,CTYSOAP    COMMERCIAL TYPE MUST BE SOAP                 
         BNE   ERRCTYPE                                                         
*                                                                               
         MVC   COELEM,0(R4)        SAVE ELEMENT FOR LATER USE                   
*                                                                               
         MVC   CMCLMED,TACOMED     SAVE COMMERCIAL MEDIA FOR LATER USE          
*                                                                               
*&&UK                                                                           
         MVI   HASLIFT,C'N'        SET COMMERCIAL HAS LIFT FLAG                 
         L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   HASLIFT,C'Y'                                                     
*&&                                                                             
*                                                                               
GCX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE IS CALLED WHEN A CHANGE HAS BEEN MADE TO ANY CAST RECORD         
* BECAUSE ANY CHANGE TO THE CAST WILL CAUSE THE COMMERCIAL TO BE IN AN          
* UNVERIFIED STATE.  THE ROUTINE WILL READ THE COMMERCIAL RECORD AND            
* SAVE THE DATE, USER ID, AND STAFF ID OF THE PERSON WHO CHANGED THE            
* CAST.  ADDITIONALLY, THE ROUTINE WILL SET THE 'ANNOUNCER-ONLY-NON-            
* MUSICIAN' BIT TO ITS CORRECT STATE.                                           
*                                                                               
UPDCOMM  NTR1                                                                   
         CLI   ACCESSED,C'Y'       DON'T BOTHER IF NOT FULLY ACCESSED           
         BNE   UCX                                                              
*                                                                               
         MVC   AIO,AIO2            READ COMMERCIAL REC FOR UPDATE               
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'B0',TGCOM)                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO              R4 = A(COMMERCIAL DETAILS ELEMENT)           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
*                                                                               
         CLI   MODE,XRECDEL        IF RECORD DELETED                            
         BE    UPD5                                                             
         CLI   MODE,XRECREST       OR RESTORED                                  
         BE    UPD5                                                             
         CLI   RECCHANG,C'Y'       OR IF ANY CAST RECORD CHANGED                
         BNE   UPD10                                                            
UPD5     XC    TACOVDTE,TACOVDTE   CLEAR DATE OF VERIFICATION                   
         XC    TACOVTIM,TACOVTIM         TIME OF VERIFICATION                   
         XC    TACOVSTU,TACOVSTU         USER ID                                
         XC    TACOVST,TACOVST           STAFF ID                               
*                                                                               
UPD10    CLC   NUMANN,=F'1'        IF ANNOUNCER IS ONLY NON-MUSICIAN            
         BNE   UC10                                                             
         CLC   NUMNONM,=F'1'                                                    
         BNE   UC10                                                             
         OI    TACOSTA2,TACOSANO   THEN SET BIT                                 
         B     UC20                                                             
*                                  ELSE CLEAR BIT                               
UC10     NI    TACOSTA2,ALL-TACOSANO                                            
*                                                                               
UC20     GOTO1 PUTREC              WRITE RECORD BACK                            
         MVC   AIO,AIO1                                                         
*                                                                               
UCX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY SS NUMBER FIELD.  SAVE RECORD IN IO3 FOR LATER USE.                   
*                                                                               
DRSSN    NTR1                                                                   
         USING TLCAD,R4                                                         
         L     R4,AIO              R4 = A(RECORD)                               
         L     R2,ASSNFLD          R2 = A(SSN FIELD)                            
*                                                                               
         L     R3,ANAMEFLD         R3 = A(NAME FIELD)                           
         USING NAMESD,R3                                                        
*                                                                               
         MVC   8(9,R2),TLCASSN     DISPLAY SSN                                  
*                                                                               
*                                  DISPLAY W4 NAME                              
         MVC   AIO,AIO3            USE IO3 FOR RECVAL CALL                      
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',8(R2)),NAMESH                         
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         BE    DRSSN10             IF NO ERROR THEN SKIP                        
*                                                                               
         BRAS  RE,PKSSN                                                         
         MVC   NAMSSN(16),=C'** NOT FOUND ** ' ELSE REC MUST BE MISSING         
*                                                                               
         OI    CVERSTAT,CVERSNW4   SET NO W4 RECORD IN CVERSTAT                 
         B     DRSSNNO             AND RETURN 'NO'                              
*                                                                               
DRSSN10  BRAS  RE,PKSSN            CONVERT THE SSN TO PID                       
         MVI   ELCODE,TAW4ELQ      GET W4 ELEMENT                               
         L     R4,AIO3                                                          
         USING TAW4D,R4                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS W4 IS LOCKED                         
         BNO   DRSSN20                                                          
         MVC   NAMSSN(16),=C'** W4 LOCKED ** ' PUT IT OUT                       
         B     DRSSNYES            AND RETURN 'YES'                             
*                                                                               
DRSSN20  MVC   NAMSSN,NAMESH+8     ELSE MOVE NAME TO SCREEN                     
*                                                                               
DRSSNYES B     YES                 RETURN 'YES'                                 
*                                                                               
DRSSNNO  B     NO                  RETURN 'NO'                                  
         DROP  R3                                                               
         SPACE 3                                                                
* DISPLAY CATEGORY CODE FIELD.                                                  
*                                                                               
DRCAT    NTR1                                                                   
         USING TLCAD,R4                                                         
         L     R4,AIO              R4 = A(RECORD)                               
         L     R2,ACATFLD          R2 = A(CATEGORY FIELD)                       
*                                                                               
         MVC   8(3,R2),TLCACAT     DISPLAY CATEGORY CODE                        
*                                                                               
DRCATX   B     XIT                                                              
         EJECT                                                                  
* DISPLAY CAMERA (ON/OFF) FIELD.                                                
*                                                                               
DRONOF   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AONOFFLD         R2 = A(CAMERA FIELD)                         
*                                                                               
         MVC   8(3,R2),TACAONOF    DISPLAY CAMERA (ON/OFF)                      
*                                                                               
DRONOFX  B     XIT                                                              
         SPACE 3                                                                
* DISPLAY TAX UNIT FIELD.                                                       
*                                                                               
DRUNIT   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AUNITFLD         R2 = A(TAX UNIT FIELD)                       
*                                                                               
         MVC   8(3,R2),TACAUNIT    DISPLAY TAX UNIT                             
*                                                                               
DRUNITX  B     XIT                                                              
         EJECT                                                                  
* DISPLAY AGENT FIELD.                                                          
*                                                                               
DRNCDE   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ANCDEFLD         R2 = A(AGENT FIELD)                          
*                                                                               
         L     R3,ANCDNFLD         R3 = A(AGENT NAME FIELD DATA)                
         LA    R3,8(R3)                                                         
         CLI   SCRFLAG,C'L'        IF LIST SCREEN                               
         BNE   DRNCDE10                                                         
         L     R3,ANAMEFLD         THEN POINT R3 WITHIN NAMES FIELD             
         USING NAMESD,R3                                                        
         LA    R3,NAMNCDE                                                       
         DROP  R3                                                               
*                                                                               
DRNCDE10 OC    TACANCDE,TACANCDE   IF NO AGENT THEN SKIP                        
         BZ    DRNCDE30                                                         
*                                                                               
*                                  DISPLAY AGENT CODE                           
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),8(R2)                              
*                                  DISPLAY AGENT NAME                           
         MVC   AIO,AIO2            USE AIO2 FOR RECVAL CALL                     
         GOTO1 RECVAL,DMCB,TLANCCDQ,(X'88',8(R2)),NAMESH                        
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         CLI   ERROR,0             IF AGENT RECORD NOT FOUND                    
         BE    DRNCDE20            THEN INDICATE THAT IN NAME FIELD             
         MVC   0(16,R3),=C'** NOT FOUND ** '                                    
         B     DRNCDEX                                                          
*                                                                               
DRNCDE20 MVC   0(16,R3),NAMESH+8   ELSE MOVE NAME TO SCREEN                     
         B     DRNCDEX                                                          
*                                                                               
DRNCDE30 MVI   ELCODE,TAPEELQ      LOOK FOR PAYEE ELEMENT ON W4                 
         L     R4,AIO3                                                          
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   0(16,R3),=C'* PAYEE ON W4 * '                                    
*                                                                               
DRNCDEX  B     XIT                                                              
         EJECT                                                                  
* DISPLAY UNION/LOCAL FIELD.                                                    
*                                                                               
DRUNLO   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AUNLOFLD         R2 = A(UNION/LOCAL FIELD)                    
*                                                                               
         MVC   8(3,R2),TACAUN      DISPLAY UNION/LOCAL                          
         MVI   11(R2),C' '                                                      
         MVC   12(3,R2),TACALOCL                                                
*                                                                               
DRUNLOX  B     XIT                                                              
         SPACE 3                                                                
* DISPLAY CONTRACT YEAR FIELD.                                                  
*                                                                               
DRYEAR   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AYEARFLD         R2 = A(YEAR FIELD)                           
*                                                                               
         MVC   8(3,R2),TACAYEAR    DISPLAY YEAR                                 
*                                                                               
DRYEARX  B     XIT                                                              
         SPACE 3                                                                
* DISPLAY LIFT FIELD.                                                           
*                                                                               
*&&UK                                                                           
DRLIFT   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ALIFTFLD         R2 = A(LIFT FIELD)                           
*                                                                               
         TM    TACASTAT,TACASTLO   IF ONLY ON LIFT                              
         BZ    DRLIFT10                                                         
         MVI   8(R2),C'O'          THEN DISPLAY 'O'                             
         B     DRLIFTX                                                          
*                                                                               
DRLIFT10 TM    TACASTAT,TACASTLF   ELSE IF ON LIFT                              
         BZ    DRLIFT20                                                         
         MVI   8(R2),C'Y'          THEN DISPLAY 'Y'                             
         B     DRLIFTX                                                          
*                                                                               
DRLIFT20 DS    0H                  ELSE DISPLAY BLANK                           
*                                                                               
DRLIFTX  B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
* DISPLAY CORPORATION NUMBER FIELD.                                             
*                                                                               
DRCORP   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ACORPFLD         R2 = A(CORPORATION FIELD)                    
*                                                                               
         MVC   8(1,R2),TACACORP    DISPLAY CORPORATION NUMBER                   
*                                                                               
* THIS SECTION GETS THE CORPORATION ID NUMBER FROM THE W4 RECORD FOUND          
* IN IO3.  IT SEARCHES THROUGH THE TAX ID ELEMENTS FOR ONE THAT MATCHES         
* THE CORPORATION NUMBER FROM THE CAST ELEMENT.  IT THEN USES THE ID            
* NUMBER IN A CALL TO RECVAL TO DISPLAY THE CORPORATION NAME.                   
*                                                                               
         L     R2,ACORNFLD         R2 = A(CORPORATION NAME FIELD DATA)          
         LA    R2,8(R2)                                                         
*                                                                               
         CLI   SCRFLAG,C'L'        IF LIST SCREEN                               
         BNE   DRCORP10                                                         
         L     R3,ANAMEFLD         THEN POINT R2 WITHIN NAMES FIELD             
         USING NAMESD,R3                                                        
         LA    R2,NAMCORP                                                       
         DROP  R3                                                               
*                                                                               
DRCORP10 MVC   AIO,AIO3            W4 RECORD IS IN IO3                          
         MVI   ELCODE,TATIELQ      CORPORATION IS IN TAX ID ELEMENT             
*                                                                               
         CLI   TACACORP,C' '       IF CAST HAS NO CORP                          
         BH    DRCORP20                                                         
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TATITYCO)) THEN IF PERF HAS NO CORP            
         BNE   DRCORP90                     THEN DISPLAY NOTHING                
*                                                                               
*                                  ELSE DISPLAY 'PERFORMER HAS CORP'            
         MVC   0(16,R2),=C'*PERF HAS CORP* '                                    
         B     DRCORP90                                                         
*                                                                               
DRCORP20 MVI   HALF,TATITYCO       ELSE IF PERF DOESN'T HAVE CAST CORP          
         MVC   HALF+1(1),TACACORP                                               
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   DRCORP30                                                         
*                                                                               
         L     R4,TGELEM           OR CAST CORP W4 RECORD NOT FOUND             
         USING TATID,R4                                                         
         MVC   AIO,AIO2                                                         
         MVC   0(L'TGSSN,R2),TGSSN SAVE SSN TEMPORARILY IN OUTPUT AREA          
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',TATIID),NAMESH                        
         MVC   TGSSN,0(R2)         RESTORE SSN                                  
         BE    DRCORP40                                                         
*                                  THEN DISPLAY 'NOT FOUND'                     
DRCORP30 MVC   0(16,R2),=C'** NOT FOUND ** '                                    
         B     DRCORP90                                                         
*                                                                               
DRCORP40 DS    0H                                                               
         MVC   0(16,R2),NAMESH+8   ELSE MOVE CAST CORP NAME TO SCREEN           
         L     R4,AIO2                                                          
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS CRP IS LOCKED                        
         BNO   DRCORP90                                                         
         MVC   0(16,R2),=C'** CRP LOCKED **'                                    
*                                                                               
DRCORP90 MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
DRCORPX  B     XIT                                                              
         EJECT                                                                  
* DISPLAY GUARANTEE FIELD.                                                      
*                                                                               
DRGUAR   NTR1                                                                   
         USING TACAD,R4                                                         
         CLI   TACALEN,X'2F'       IF LENGTH IS NOT OLD LENGTH                  
         BE    DRGUAR50                                                         
         OC    TACAGUA,TACAGUA     AND GUARANTEE EXISTS                         
         BZ    DRGUAR50                                                         
*                                                                               
         L     R2,AGUARFLD         THEN DISPLAY GUARANTEE CODE                  
         MVC   8(4,R2),TACAGUA                                                  
         B     DRGUARX                                                          
*                                                                               
DRGUAR50 MVC   AIO,AIO2            READ SOAP GUARANTEE REC INTO AIO2            
         GOTO1 RECVAL,DMCB,TLSGCDQ,(X'80',=F'0')                                
         MVC   AIO,AIO1                                                         
         CLC   KEY(TLSGGUA-TLSGD),KEYSAVE                                       
         BNE   DRGUARX                                                          
         L     R3,AGUADFLD         SOAP GUARANTEE DESC FIELD                    
         MVC   8(11,R3),=C'* ON SGRT *'                                         
DRGUARX  B     XIT                                                              
         EJECT                                                                  
* DISPLAY OVERSCALE PERCENTAGE ELEMENT.                                         
*                                                                               
DROP     NTR1                                                                   
         L     R2,AOPFLD           R2 = A(OVERSCALE PERCENTAGE FIELD)           
*                                                                               
         CLI   SCRFLAG,C'L'        IF LIST SCREEN                               
         BNE   DROP50                                                           
*                                                                               
         L     R4,AIO              THEN IF OVERSCALE AMOUNTS ELEMENT            
         MVI   ELCODE,TAOAELQ          EXISTS                                   
         BAS   RE,GETEL                                                         
         BE    DROP10                                                           
*                                                                               
         L     R4,AIO              OR OVERSCALE PERC ELEMENT EXISTS             
         MVI   ELCODE,TAOPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DROPX                                                            
         USING TAOPD,R4                                                         
*                                                                               
         CLI   TAOPNUM,1           AND NUMBER OF PERCENTAGES IS NOT 1           
         BNE   DROP10                                                           
         CLC   TAOPUSE,=CL3' '     OR USE TYPE IS NOT ALL                       
         BE    DROP20                                                           
*                                                                               
DROP10   MVC   8(4,R2),=C'****'    THEN DISPLAY '****'                          
         B     DROPX                                                            
*                                                                               
DROP20   ICM   R3,15,TAOPPCT       ELSE R3 = PERCENTAGE                         
*                                                                               
*                                  DISPLAY FIRST PERCENTAGE                     
         GOTO1 DR2DEC,DMCB,(R3),8(R2),4                                         
         B     DROPX                                                            
*                                                                               
*        FOR EXTENSION SCREEN DISPLAY ENTIRE ELEMENT                            
*                                                                               
DROP50   L     R4,AIO              IF OVERSCALE PERC ELEMENT DOESN'T            
         MVI   ELCODE,TAOPELQ          EXIST THEN DONE                          
         BAS   RE,GETEL                                                         
         BNE   DROPX                                                            
         USING TAOPD,R4                                                         
*                                                                               
         ZIC   R0,TAOPNUM          R0 = # SUB ELEMENTS                          
         LA    R2,8(R2)            R2 = A(FIELD DATA)                           
         LA    R3,TAOPSBEL         R3 = A(FIRST SUB ELEMENT)                    
*                                                                               
DROP60   CLI   0(R2),C' '          IF NOT FIRST CODE                            
         BNH   DROP70                                                           
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
*                                                                               
DROP70   CLC   0(3,R3),=CL7' '     IF USE CODE IS SPACES                        
         BNE   *+14                                                             
         MVC   0(3,R2),=C'ALL'     THEN DISPLAY 'ALL'                           
         B     *+10                                                             
         MVC   0(3,R2),0(R3)       ELSE DISPLAY USE CODE                        
*                                                                               
         MVI   3(R2),C'='          DISPLAY '= PERCENTAGE'                       
         ICM   RF,15,3(R3)                                                      
         GOTO1 DR2DEC,DMCB,(RF),4(R2),10                                        
*                                                                               
         LA    R2,3(R2)            POINT R2 TO LAST NON-SPACE                   
         A     R2,0(R1)                                                         
         LA    R3,7(R3)            BUMP R3 TO NEXT SUB ELEMENT                  
         BCT   R0,DROP60           REPEAT UNTIL NO MORE SUB-ELEMENTS            
*                                                                               
DROPX    B     XIT                                                              
         EJECT                                                                  
* DISPLAY WAGES AND WARDROBE FEE FROM OVERSCALE AMOUNTS ELEMENT.                
*                                                                               
DRWAMT   NTR1                                                                   
         USING TAOAD,R4                                                         
         L     R4,AIO              IF OVERSCALE AMOUNTS ELEMENT DOESN'T         
         MVI   ELCODE,TAOAELQ          EXIST THEN DONE                          
         BAS   RE,GETEL                                                         
         BNE   DRWAX                                                            
*                                                                               
         ZIC   RE,TAOANUM          RE = # SUB ELEMENTS                          
         LA    R3,TAOASBEL         R3 = A(FIRST SUB ELEMENT)                    
*                                                                               
DRWA10   CLC   0(3,R3),=C'SOP'     LOOK FOR SOP CODE                            
         BE    DRWA12                                                           
         CLC   0(3,R3),=C'OTH'     AND OTH CODE                                 
         BE    DRWA14                                                           
         B     DRWA25                                                           
*                                                                               
DRWA12   L     R2,AWAGEFLD         R2 = A(WAGES FIELD)                          
         CLI   SCRFLAG,C'E'                                                     
         BNE   DRWA13                                                           
         EDIT  (4,3(R3)),(11,8(R2)),2,ALIGN=LEFT                                
         B     DRWA25                                                           
DRWA13   EDIT  (4,3(R3)),(11,8(R2)),2                                           
         B     DRWA25                                                           
*                                                                               
DRWA14   L     R2,AWARDFLD         R2 = A(WARDROBE FEE FIELD)                   
         CLI   SCRFLAG,C'E'                                                     
         BNE   DRWA20                                                           
         EDIT  (4,3(R3)),(8,8(R2)),2,ALIGN=LEFT                                 
         B     DRWA25                                                           
DRWA20   EDIT  (4,3(R3)),(8,8(R2)),2                                            
*                                                                               
DRWA25   LA    R3,7(R3)            BUMP R3 TO NEXT SUB ELEMENT                  
         BCT   RE,DRWA10           REPEAT UNTIL NO MORE SUB-ELEMENTS            
DRWAX    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE 2 DEC PLACE NUMBER FOUND IN PARM1 TO THE            
* DESTINATION POINTED TO BY PARM2.  IF THE DISPLAYED AMOUNT IS BIGGER           
* THEN THE LENGTH SPECIFIED IN PARM3 THEN IT WILL DISPLAY '*'S INSTEAD.         
* THE ROUTINE WILL RETURN THE LENGTH IN PARM1.                                  
*                                                                               
DR2DEC   NTR1                                                                   
         LM    R2,R4,0(R1)         R2 = NUMBER                                  
*                                  R3 = A(DESTINATION)                          
         BCTR  R4,0                R4 = LENGTH OF DESTINATION - 1               
*                                                                               
         EX    R4,*+8              PRE-CLEAR DESTINATION                        
         B     *+10                                                             
         XC    0(0,R3),0(R3)                                                    
*                                                                               
         LTR   R2,R2               IF NUMBER = 0                                
         BNZ   *+12                                                             
         MVI   0(R3),C'0'          THEN DISPLAY '0' AND RETURN                  
         B     DR2DECX                                                          
*                                  ELSE EDIT PERCENTAGE INTO BLOCK              
         EDIT  (R2),(10,BLOCK),2,ALIGN=LEFT                                     
*                                                                               
         LA    RF,BLOCK            RF = A(LAST CHAR IN BLOCK)                   
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
*                                                                               
DR2DEC10 CLI   0(RF),C'.'          IF CHAR IS '.' THEN DONE                     
         BE    DR2DEC50                                                         
         CLI   0(RF),C'0'          ELSE IF CHAR IS <> '0' THEN DONE             
         BNE   DR2DEC60                                                         
         BCT   RF,DR2DEC10         ELSE BACK UP POINTER AND LOOP BACK           
*                                                                               
DR2DEC50 BCTR  RF,0                BACK UP ONE MORE FOR '.'                     
*                                                                               
DR2DEC60 LA    RE,BLOCK            RF = LENGTH OF NUMBER - 1                    
         SR    RF,RE                                                            
*                                                                               
         CR    RF,R4               IF L(NUMBER) - 1 > L(DEST) - 1               
         BH    DR2DEC90            THEN DISPLAY '*'S                            
*                                                                               
         EX    RF,*+8              ELSE MOVE NUMBER TO DESTINATION              
         B     *+10                                                             
         MVC   0(0,R3),BLOCK                                                    
*                                                                               
         LA    RF,1(RF)            RETURN LENGTH IN PARM1                       
         ST    RF,0(R1)                                                         
         B     DR2DECX                                                          
*                                                                               
DR2DEC90 MVI   0(R3),C'*'          DISPLAY '*'S                                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(R3)                                                    
         LA    RF,2(R4)            RETURN LENGTH IN PARM1                       
         ST    RF,0(R1)                                                         
DR2DECX  B     XIT                                                              
         EJECT                                                                  
* DISPLAY COMMENTS.                                                             
*                                                                               
DRCM     NTR1                                                                   
         USING TACMD,R4                                                         
         L     R2,ACMFLD           R2 = A(COMMENTS FIELD)                       
*                                                                               
         GOTO1 CHAROUT,DMCB,TACMELQ,(R2),TACMTYPG                               
*                                                                               
DRCMX    B     XIT                                                              
         SPACE 3                                                                
* DISPLAY CHECK COMMENTS.                                                       
*                                                                               
DRCHCM   NTR1                                                                   
         USING TACMD,R4                                                         
         L     R2,ACHCMFLD         R2 = A(CHECK COMMENTS FIELD)                 
*                                                                               
         GOTO1 CHAROUT,DMCB,TACMELQ,(R2),TACMTYPC                               
*                                                                               
DRCHCMX  B     XIT                                                              
         EJECT                                                                  
* DISPLAY STATUS ELEMENT.                                                       
*                                                                               
DRSTAT   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ASTATFLD         R2 = A(STATUS FIELD DATA)                    
         LA    R2,8(R2)                                                         
*                                                                               
         L     RF,ASTATTAB         RF = A(STATUS CODE TABLE)                    
DRS10    CLI   0(RF),X'FF'         WHILE NOT END OF TABLE                       
         BE    DRSX                                                             
*                                                                               
         MVC   HALF,0(RF)          IF BIT IS ON IN STATUS BYTE                  
         NC    HALF,TACASTAT                                                    
         BZ    DRS30                                                            
*                                                                               
         CLI   0(R2),C' '          IF NOT FIRST CODE                            
         BNH   DRS20                                                            
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
DRS20    MVC   0(10,R2),2(RF)      DISPLAY CODE                                 
*                                                                               
         LA    R2,9(R2)            POINT R2 TO LAST NON-SPACE                   
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
DRS30    LA    RF,L'STATTAB(RF)    BUMP TO NEXT STATUS CODE TABLE ENTRY         
         B     DRS10               LOOP BACK                                    
*                                                                               
DRSX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY PERFORMANCE RATE                                                      
*                                                                               
         USING TACRD,R4                                                         
DRPERF   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRPERFX                                                          
*                                                                               
         L     R2,APERFFLD         R2 = A(PERFORMANCE RATE FIELD)               
         CLI   SCRFLAG,C'E'                                                     
         BNE   DRPERF10                                                         
         EDIT  TACRSCAL,(10,8(R2)),2,ALIGN=LEFT                                 
         B     DRPERFX                                                          
DRPERF10 EDIT  TACRSCAL,(10,8(R2)),2                                            
*                                                                               
DRPERFX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY EPISODE NUMBERS                                                       
*                                                                               
         USING TASOD,R4                                                         
DREPI    NTR1                                                                   
         BAS   RE,DREPILBL         DISPLAY EPISODE NUMBER LABELS                
         L     R4,AIO                                                           
         MVI   ELCODE,TASOELQ                                                   
         USING TASOD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   DREPIX                                                           
*                                                                               
         ZIC   RF,TASONUM          CHECK OFF APPROPRIATE EPISODES               
         LTR   RF,RF                                                            
         BZ    DREPIX              NO EPISODES                                  
         LA    R1,TASOSEPI         EPISODES IN ELEMENT                          
         USING TASOSEPI,R1                                                      
*                                                                               
         USING EPID,R2                                                          
DREPI30  LA    R3,EPITAB           R3=A(CHAR EPISODE NUMBERS TABLE)             
         L     R2,AEPIFLD          R2 = A(EPISODE FIELD)                        
         BAS   RE,BUMP                                                          
         LH    RE,TASOEPI          SET CHARACTER EPISODE NUMBER                 
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CHAREPI,DUB                                                      
*                                                                               
DREPI35  CLC   CHAREPI,0(R3)                                                    
         BE    DREPI40                                                          
         LA    R2,EPILNQ(R2)       BUMP TO NEXT EPISODE ON SCREEN               
         LA    R3,L'EPITAB(R3)     BUMP TO NEXT EPISODE IN TABLE                
         CLI   0(R3),X'FF'                                                      
         BNE   DREPI35                                                          
         B     DREPI45             EPISODE DOESN'T FIT ON SCREEN                
*                                                                               
DREPI40  BAS   RE,SETCKOFF         SET CHECK OFF CODE IN HALF                   
         MVC   EPIINP,HALF                                                      
DREPI45  LA    R1,L'TASOSEPI(R1)   NEXT EPISODE IN ELEMENT                      
         BCT   RF,DREPI30                                                       
*                                                                               
DREPIX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SETCKOFF- SET IN HALF CHECK OFF CODE                                          
*                                                                               
SETCKOFF NTR1                                                                   
         L     RE,AWROLTAB                                                      
         USING WROLTABD,RE                                                      
SETCOFF5 CLC   TASOSTAT,WROLEQU                                                 
         BE    SETCOFF8                                                         
         LA    RE,WROLTABL(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   SETCOFF5                                                         
         MVC   HALF,=C'X '         DEFAULT DISPLAY                              
         B     SETCOFFX                                                         
*                                                                               
SETCOFF8 MVC   HALF,WROLCHAR       SET CHARACTER CHECK OFF                      
SETCOFFX B     XIT                                                              
         DROP  R4,R1,RE                                                         
         SPACE                                                                  
* DREPILBL - DISPLAY EPISODE NUMBER LABELS                                      
*                                                                               
DREPILBL NTR1                                                                   
         LA    R3,EPITAB           R3 = A(CHAR EPISODE NUMBERS TABLE)           
         L     R2,AEPIFLD          R2 = A(EPISODE TAG FIELD)                    
         USING EPID,R2                                                          
         BAS   RE,BUMP             DISPLAY EPISODE NUMBERS                      
DREPIL10 MVC   EPINUM,0(R3)                                                     
         MVI   EPISPAR,C'('                                                     
         MVI   EPIEPAR,C')'                                                     
         LA    R2,EPILNQ(R2)                                                    
         LA    R3,L'EPITAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   DREPIL10                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* VALIDATE SSN FIELD.                                                           
*                                                                               
VRSSN    NTR1                                                                   
         MVC   AIO,AIO3            VALIDATE SSN (READ W4 REC INTO IO3)          
         L     R2,ASSNFLD                                                       
         GOTO1 ANY                                                              
         BRAS  RE,UNPKPID          CONVERT TO SSN FOR RECVAL CALL               
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'24',(R2))                                 
         BE    VRSSN10                                                          
         MVC   8(L'TGSSN,R2),BLOCK  RESTORE WHAT WAS ENTERED                    
         OI    6(R2),X'80'                                                      
         B     ERRRNF                                                           
VRSSN10  BRAS  RE,PKSSN            CONVERT BACK TO PID                          
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         MVI   ELCODE,TAW4ELQ      GET W4 ELEMENT                               
         L     R4,AIO3                                                          
         USING TAW4D,R4                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS W4 IS LOCKED                         
         BO    ERW4LCK             EXIT WITH ERROR MESSAGE                      
         CLI   TAW4TYPE,TAW4TYTR   IF THIS W4 RECORD IS TRUSTEE                 
         BE    ERNOTRS             EXIT WITH ERROR MESSAGE                      
*                                                                               
VRSSNX   B     XIT                                                              
         SPACE 3                                                                
* VALIDATE CATEGORY FIELD.                                                      
*                                                                               
VRCAT    NTR1                                                                   
         L     R4,AIO              R4 = A(CAST RECORD)                          
         USING TLCAD,R4                                                         
         L     R2,ACATFLD          R2 = A(CATEGORY FIELD)                       
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VRCAT10                                                          
         ICM   RF,15,SVACAT        AND THERE IS A FIELD ABOVE                   
         BZ    VRCAT10                                                          
         MVC   5(1,R2),5(RF)       THEN MOVE LEN AND DATA TO THIS FIELD         
         MVC   8(3,R2),8(RF)                                                    
*                                                                               
VRCAT10  GOTO1 ANY                 MOVE AND SPACE PAD FIELD TO WORK             
*                                                                               
         GOTO1 CATVAL,DMCB,WORK    VALIDATE CATEGORY CODE                       
         BNE   ERRINV                                                           
*                                                                               
         TM    TGCATYPE,WRITER     IF WRITER CATEGORY                           
         BNO   VRCAT12                                                          
         CLI   TGCAEQU,CTW         ONLY W & W2 & W3 VALID                       
         BE    VRCAT12                                                          
         CLI   TGCAEQU,CTW2                                                     
         BE    VRCAT12                                                          
         CLI   TGCAEQU,CTW3                                                     
         BNE   ERRINV                                                           
*                                                                               
VRCAT12  CLI   SCRFLAG,C'L'                                                     
         BNE   VRCAT15                                                          
         CLI   VALMODE,C'A'        IF ADDING NEW RECORD                         
         BE    VRCAT20                                                          
VRCAT15  CLC   TLCACAT,TGCAT       OR IF CHANGE CATEGORY CHANGED                
         BE    VRCATX                                                           
*                                                                               
VRCAT20  MVC   TLCACAT,TGCAT       SAVE IN RECORD                               
         SPACE 1                                                                
         TM    TGCATYPE,WRITER     IF WRITER CATEGORY                           
         BO    VRCAT30                                                          
         LA    RE,COELEM           OR IF SOAP RESIDUAL COMMERCIAL               
         USING TACOD,RE                                                         
         TM    TACOSTAT,TACOSRES   DUPLICATES NOT ALLOWED                       
         BNO   VRCATX                                                           
         DROP  RE                                                               
VRCAT30  XC    TGCSORT,TGCSORT     SO MAKE SURE CAST DOESN'T EXIST              
         GOTO1 RECVAL,DMCB,TLCACCDQ,0                                           
         CLC   KEY(TLCACSEQ-TLCAPD),KEYSAVE                                     
         BE    ERRDUPP             GIVE ERROR MESSAGE                           
VRCATX   B     XIT                                                              
         EJECT                                                                  
* VALIDATE CAMERA (ON/OFF) FIELD.                                               
*                                                                               
VRONOF   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AONOFFLD         R2 = A(CAMERA FIELD)                         
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VRONOF50                                                         
*                                                                               
         TM    TGCASTAT,OKON       THEN IF OK ON CAMERA ONLY                    
         BZ    VRONOF10                                                         
         TM    TGCASTAT,OKOFF                                                   
         BO    VRONOF20                                                         
         MVC   8(3,R2),=C'ON '     THEN MOVE 'ON' TO FIELD                      
         B     VRONOF40                                                         
*                                                                               
VRONOF10 TM    TGCASTAT,OKON       ELSE IF OK OFF CAMERA ONLY                   
         BO    VRONOF20                                                         
         MVC   8(3,R2),=C'OFF'     THEN MOVE 'OFF' TO FIELD                     
         B     VRONOF40                                                         
*                                                                               
*RONOF20 TM    TGCAUNI,AFM         ELSE IF CATEGORY IS MUSICIAN                 
VRONOF20 GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BZ    VRONOF25                                                         
         MVC   8(3,R2),=C'OFF'     THEN MOVE 'OFF' TO FIELD                     
         B     VRONOF40                                                         
*                                                                               
VRONOF25 CLC   TGCACDE,=C'PIL'     ELSE IF CATEGORY IS 'PIL'                    
         BNE   VRONOF30                                                         
         MVC   8(3,R2),=C'ON '     THEN MOVE 'ON' TO FIELD                      
         B     VRONOF40                                                         
*                                                                               
VRONOF30 ICM   RF,15,SVAONOF       ELSE IF THERE IS A FIELD ABOVE               
         BZ    VRONOF50                                                         
         MVC   8(3,R2),8(RF)       THEN MOVE DATA TO THIS FIELD                 
         OC    8(3,R2),=CL7' '     PAD WITH SPACES                              
*                                                                               
VRONOF40 MVI   5(R2),3             SET FIELD LENGTH TO 3                        
VRONOF50 GOTO1 ANY                 MOVE AND SPACE PAD FIELD TO WORK             
*                                                                               
         CLC   =C'ON ',WORK        IF FIELD CONTAINS 'ON'                       
         BNE   VRONOF60                                                         
         TM    TGCASTAT,OKON       THEN MUST BE OK FOR CATEGORY                 
         BZ    ERRINV                                                           
         CLI   CMCLMED,C'R'        AND MEDIA MUST NOT BE RADIO                  
         BE    ERRINV                                                           
         B     VRONOF90                                                         
*                                                                               
VRONOF60 CLC   =C'OFF',WORK        ELSE FIELD MUST CONTAIN 'OFF'                
         BNE   ERRINV                                                           
         TM    TGCASTAT,OKOFF      AND MUST BE VALID FOR CATEGORY               
         BZ    ERRINV                                                           
*                                                                               
VRONOF90 MVC   TACAONOF,WORK       SAVE IN ELEMENT                              
VRONOFX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE TAX UNIT FIELD.                                                      
*                                                                               
VRUNIT   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AUNITFLD         R2 = A(TAX UNIT FIELD)                       
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VRUNIT10                                                         
         ICM   RF,15,SVAUNIT       AND THERE IS A FIELD ABOVE                   
         BZ    VRUNIT10                                                         
         MVC   5(1,R2),5(RF)       THEN MOVE LEN AND DATA TO THIS FIELD         
         MVC   8(3,R2),8(RF)                                                    
*                                                                               
VRUNIT10 GOTO1 ANY                 MOVE AND PAD SCREEN FIELD TO WORK            
*                                                                               
         CLI   5(R2),2             MUST BE AT LEAST TWO CHARS                   
         BL    ERRINV                                                           
*                                                                               
         ZIC   R3,5(R2)            VALIDATE TAX UNIT                            
         GOTO1 TAXVAL,DMCB,((R3),WORK)                                          
         BNE   ERRINV                                                           
         TM    TGTASTAT,TASUINGF   ENSURE TAX UNIT IS VALID GOING               
         BO    ERRINV              GOING FORWARD                                
*                                                                               
         CLC   WORK(2),=C'FD'      IF UNIT STARTS WITH 'FD'                     
         BE    ERRINV                                                           
         CLI   WORK+2,C'0'         OR ENDS WITH '0' THEN ERROR                  
         BE    ERRINV                                                           
*                                                                               
         MVC   TACAUNIT,WORK       SAVE TAX UNIT IN ELEMENT                     
*                                                                               
VRUNITX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE AGENT FIELD (OPTIONAL).                                              
*                                                                               
VRNCDE   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ANCDEFLD         R2 = A(AGENT FIELD)                          
*                                                                               
         XC    TACANCDE,TACANCDE   IF NOT ENTERED THEN ZEROS                    
         CLI   5(R2),0                                                          
         BE    VRNCDEX                                                          
*                                                                               
*                                  VALIDATE AGENT CODE                          
         GOTO1 RECVAL,DMCB,TLANCCDQ,(R2)                                        
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),TACANCDE                              
*                                                                               
VRNCDEX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE UNION/LOCAL FIELD (DEFAULT TO ABOVE).                                
*                                                                               
VRUNLO   NTR1                                                                   
         USING TACAD,R4                                                         
         USING SCAND,R3                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AUNLOFLD         R2 = A(UNION/LOCAL FIELD)                    
         LA    R3,BLOCK            R3 = A(SCANNER BLOCK)                        
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VRUNLO10                                                         
         ICM   RF,15,SVAUNLO       AND THERE IS A FIELD ABOVE                   
         BZ    VRUNLO10                                                         
         MVI   5(R2),7             THEN MOVE LEN AND DATA TO THIS FIELD         
         MVC   8(7,R2),8(RF)                                                    
*                                                                               
VRUNLO10 CLI   5(R2),3             FIELD MUST BE LONGER THEN 3 BYTES            
         BNH   ERRINV                                                           
*                                                                               
         OC    8(7,R2),=CL7' '     SET NULLS TO SPACES                          
*                                                                               
         GOTO1 UNIVAL,DMCB,8(R2)   VALIDATE UNION                               
         BNE   ERRINV                                                           
*                                                                               
*        ZIC   RF,TGUNEQU          TEST UNION VALID FOR THIS CATEGORY           
*        EX    RF,*+8                                                           
*        B     *+8                                                              
*        TM    TGCAUNI,0                                                        
         GOTO1 UNITEST,DMCB,(X'80',TGUNEQUS),TGCAUNIS                           
         BZ    ERRINV              TEST UNION VALID FOR THIS CATEGORY           
*                                                                               
         CLI   CMCLMED,TACOMEDR    IF COMMERCIAL MEDIA IS RADIO                 
         BNE   *+14                                                             
         CLC   =C'SAG',8(R2)       THEN UNION CAN'T BE SAG                      
         BE    ERRINV                                                           
*                                                                               
         MVC   TACAUN,8(R2)        SAVE UNION IN ELEMENT                        
*                                                                               
         LA    R0,12(R2)           R0 = A(LOCAL CODE WITHIN FIELD)              
         CLI   11(R2),C' '                                                      
         BE    VRUNLO20                                                         
         CLI   11(R2),C'/'                                                      
         BE    VRUNLO20                                                         
         LA    R0,11(R2)                                                        
*                                                                               
VRUNLO20 MVC   TGUNI,TACAUN        VALIDATE LOCAL CODE                          
         GOTO1 RECVAL,DMCB,TLLOCDQ,(X'80',(R0))                                 
         BNE   ERRINV                                                           
         CLC   =C'LA ',TACAUNIT    IF TAX STATE IS LOUSIANA                     
         BNE   VRUNLO30                                                         
         CLC   =C'LA ',TGLCL       LOCAL CAN'T BE FOR LOS ANGELES               
         BE    ERRTAX                                                           
         CLC   =C'47 ',TGLCL                                                    
         BE    ERRTAX                                                           
VRUNLO30 MVC   TACALOCL,TGLCL      SAVE LOCAL IN ELEMENT                        
*                                                                               
VRUNLOX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE CONTRACT YEAR FIELD (DEFAULT TO ABOVE).                              
*                                                                               
VRYEAR   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AYEARFLD         R2 = A(CONTRACT YEAR FIELD)                  
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VRYEAR5                                                          
         ICM   RF,15,SVAYEAR       AND THERE IS A FIELD ABOVE                   
         BZ    VRYEAR5                                                          
         MVC   5(1,R2),5(RF)       THEN MOVE LEN AND DATA TO THIS FIELD         
         MVC   8(3,R2),8(RF)                                                    
*                                                                               
VRYEAR5  GOTO1 ANY                 MOVE AND PAD SCREEN FIELD TO WORK            
*                                                                               
         GOTO1 YRVAL,DMCB,WORK     VALIDATE YEAR                                
         BNE   ERRINV                                                           
*                                                                               
         LA    R3,TGUNYR           R3 = A(VALID UNION YEARS)                    
*                                                                               
VRYEAR10 CLI   0(R3),0             IF END OF TABLE THEN ERROR                   
         BE    ERRINV                                                           
         CLC   0(1,R3),TGYREQU     IF MATCH THEN FOUND                          
         BE    VRYEAR20                                                         
         LA    R3,1(R3)            ELSE TRY NEXT TABLE ENTRY                    
         B     VRYEAR10                                                         
*                                                                               
VRYEAR20 CLC   TACAUN,=C'SAG'      IF UNION IS SAG                              
         BE    VRYEAR25                                                         
         CLC   TACAUN,=C'AFT'      OR UNION IS AFTRA                            
         BNE   VRYEAR30                                                         
VRYEAR25 CLC   =C'01',WORK         YEAR CANNOT BE 2001 FOR SOAPS                
         BE    ERRINV                                                           
*                                                                               
VRYEAR30 MVC   TACAYEAR,WORK       SAVE YEAR IN ELEMENT                         
*                                                                               
VRYEARX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE LIFT FIELD (OPTIONAL).                                               
*                                                                               
*&&UK                                                                           
VRLIFT   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ALIFTFLD         R2 = A(LIFT FIELD)                           
*                                                                               
         CLI   5(R2),0             IF FIELD IS NOT EMPTY                        
         BE    VRLIFT5                                                          
         CLI   HASLIFT,C'Y'        THEN COMMERCIAL MUST HAVE LIFT               
         BNE   ERRINV                                                           
*                                  IF FIELD IS EMPTY THEN CLEAR BITS            
VRLIFT5  NI    TACASTAT,X'FF'-TACASTLO-TACASTLF                                 
         CLI   5(R2),0                                                          
         BE    VRLIFTX                                                          
*                                                                               
         CLI   8(R2),C'O'          ELSE IF FIELD CONTAINS 'O'                   
         BNE   VRLIFT10            THEN SET LIFT AND LIFT ONLY BITS             
         OI    TACASTAT,TACASTLO+TACASTLF                                       
         B     VRLIFTX                                                          
*                                                                               
VRLIFT10 CLI   8(R2),C'Y'          ELSE IF FIELD CONTAINS 'Y'                   
         BNE   VRLIFT20            THEN SET LIFT BIT                            
         OI    TACASTAT,TACASTLF                                                
         B     VRLIFTX                                                          
*                                                                               
VRLIFT20 B     ERRINV              ELSE INVALID INPUT FIELD                     
*                                                                               
VRLIFTX  B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
* VALIDATE CORPORATION NUMBER FIELD (OPTIONAL).                                 
*                                                                               
VRCORP   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ACORPFLD         R2 = A(CORPORATION FIELD)                    
*                                                                               
         MVI   TACACORP,0          IF FIELD IS EMPTY THEN SET TO ZERO           
         CLI   5(R2),0                                                          
         BE    VRCORPX                                                          
*                                                                               
         MVC   TACACORP,8(R2)      SAVE IN ELEMENT                              
*                                                                               
         CLI   TACACORP,C'Y'       IF FIELD HAD 'Y'                             
         BNE   *+8                                                              
         MVI   TACACORP,C'1'       THEN SET TO '1' IN ELEMENT                   
*                                                                               
         MVI   HALF,TATITYCO       BUILD GETL SEARCH ARGUMENT                   
         MVC   HALF+1(1),TACACORP                                               
*                                                                               
         MVC   AIO,AIO3            SEARCH IO3 FOR TAX UNIT ELEMENT              
         MVI   ELCODE,TATIELQ                                                   
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   ERRINV              ERROR IF NOT FOUND                           
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         CLI   8(R2),C'Y'          IF 'Y' WAS INPUT                             
         BNE   VRCORPX                                                          
         L     R4,TGELEM                                                        
         BAS   RE,NEXTEL           SEARCH FOR ADDL CORPS ON W4 REC.             
         BE    ERRCRP              NEED PRECISE CORP CODE                       
*                                                                               
VRCORPX  MVC   CASTCORP,TACACORP   SAVE CAST CORP                               
*                                                                               
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         L     RF,TGELEM                                                        
         GOTOR W4LCKCRP,DMCB,TATIID-TATID(RF),AIO3,AIO1                         
         BE    XIT                                                              
         NI    4(R2),X'DF'         UNVALIDATE                                   
         B     ERCRPLK             EXIT WITH ERROR MESSAGE                      
         EJECT                                                                  
* VALIDATE GUARANTEE FIELD (OPTIONAL).                                          
*                                                                               
VRGUAR   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AGUARFLD         R2 = A(GUARANTEE FIELD)                      
         XC    TACAGUA,TACAGUA     PRE-CLEAR ELEMENT DATA                       
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY THEN DONE                  
         BE    VRGUARX                                                          
         MVC   TACAGUA,8(R2)       ELSE SAVE GUARANTEE CODE IN ELEMENT          
         MVC   FULL,8(R2)          COMPLEMENT FIELD DATA INTO FULL              
         XC    FULL,XFFS                                                        
*                                                                               
         MVC   AIO,AIO2            VALIDATE SOAP GUARANTEE CODE                 
         GOTO1 RECVAL,DMCB,TLSGCDQ,(X'A0',FULL)                                 
         MVC   AIO,AIO1                                                         
         BNE   ERRRNF                                                           
*                                                                               
         L     R4,AIO2             R4 = A(SOAP GUARANTEE DETAILS ELE)           
         MVI   ELCODE,TASGELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASGD,R4                                                         
*                                                                               
         CLC   TASGAGY,TGAGY       MUST BE SAME AGENCY                          
         BNE   ERRINV                                                           
         CLI   TASGCRP,0           IF GUARANTEE HAS A CORP                      
         BE    VRGUARX                                                          
         CLC   TASGCRP,CASTCORP    THEN CORP MUST MATCH CAST CORP               
         BNE   ERRINV                                                           
*                                                                               
VRGUARX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE OVERSCALE PERCENTAGE FIELD (OPTIONAL).                               
*                                                                               
VROP     NTR1                                                                   
         USING TAOPD,R4                                                         
         MVI   ELCODE,TAOPELQ      ELCODE = OVERSCALE PERC ELEMENT              
         L     R2,AOPFLD           R2 = A(OVERSCALE PERCENTAGE FIELD)           
*                                                                               
         TM    4(R2),X'20'         IF FIELD HASN'T CHANGED THEN DONE            
         BO    VROPX                                                            
*                                                                               
         GOTO1 REMELEM             REMOVE OLD ELEMENT                           
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY THEN DONE                  
         BE    VROPX                                                            
*                                                                               
         LA    R4,ELEM             BUILD NEW OVERSCALE PERCENT ELEMENT          
         XC    ELEM,ELEM                                                        
         MVI   TAOPEL,TAOPELQ                                                   
*                                                                               
         CLI   SCRFLAG,C'L'        IF LIST SCREEN THEN VALIDATE PERC            
         BNE   VROP10                  ONLY AND SET USE CODE TO 'ALL'           
*                                                                               
         GOTO1 VALIDEC             VALIDATE PERCENTAGE                          
         MVC   TAOPPCT,FULL        SAVE IN ELEMENT                              
         MVC   TAOPUSE,=CL7' '     SET USE CODE TO SPACES FOR ALL               
         MVI   TAOPNUM,1           SET NUMBER OF SUB ELEMENTS TO 1              
         MVI   TAOPLEN,TAOPLNQ+L'TAOPSBEL    SET LENGTH                         
         B     VROP90                                                           
*                                                                               
*        VALIDATE ENTIRE ELEMENT FOR EXTENSION SCREEN                           
*                                                                               
*                                  SCAN FIELD INTO SCANNER BLOCK                
VROP10   GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         CLI   4(R1),0                                                          
         BE    ERRINV                                                           
         MVC   BYTE,4(R1)          SAVE NUMBER OF ENTRIES IN BYTE               
*                                                                               
         LA    R3,BLOCK            R3 = A(SCANNER BLOCK)                        
         USING SCAND,R3                                                         
         ZIC   RF,4(R1)            FULL = NUMBER OF SCAN BLOCK ENTRIES          
         ST    RF,FULL                                                          
         SR    R0,R0               R0 = DISPLACEMENT INTO FIELD                 
         LA    R4,TAOPSBEL         R4 = A(FIRST SUB ELEMENT)                    
*                                                                               
VROP20   CLC   =C'ALL ',SCDATA1    IF USE CODE IS 'ALL'                         
         BNE   VROP30                                                           
         MVC   0(3,R4),=CL7' '     SET USE CODE TO SPACES IN ELEM               
         B     VROP40                                                           
*                                  ELSE VALIDATE USE CODE                       
VROP30   GOTO1 USEVAL,DMCB,(X'40',SCDATA1),0                                    
         BNE   ERRFLD                                                           
         MVC   0(3,R4),SCDATA1     SAVE USE CODE IN ELEM                        
*                                                                               
VROP40   SR    RF,RF               VALIDATE AMOUNT                              
         ICM   RF,1,SCLEN2                                                      
         BZ    ERRFLD                                                           
         GOTO1 CASHVAL,DMCB,SCDATA2,(RF)                                        
         CLI   0(R1),0                                                          
         BNE   ERRFLD                                                           
         MVC   3(4,R4),4(R1)       SAVE AMOUNT IN ELEM                          
*                                                                               
         ZIC   RF,SCLEN1           BUMP R0 TO NEXT FIELD                        
         LA    RF,1(RF)                                                         
         AR    R0,RF                                                            
         ZIC   RF,SCLEN2                                                        
         LA    RF,1(RF)                                                         
         AR    R0,RF                                                            
         LA    R4,7(R4)            BUMP R4 TO NEXT SUB ELEMENT                  
         LA    R3,SCANNEXT         BUMP R3 TO NEXT SCANNER FIELD                
*                                                                               
         L     RF,FULL             REPEAT UNTIL NO MORE SCANNER FIELDS          
         BCTR  RF,0                                                             
         ST    RF,FULL                                                          
         LTR   RF,RF                                                            
         BNZ   VROP20                                                           
*                                                                               
         LR    RF,R4               SET LENGTH OF ELEMENT                        
         LA    R4,ELEM                                                          
         SR    RF,R4                                                            
         STC   RF,TAOPLEN                                                       
         MVC   TAOPNUM,BYTE        SET NUMBER OF SUB ELEMENTS                   
*                                                                               
VROP90   GOTO1 ADDELEM             ADD ELEMENT                                  
*                                                                               
VROPX    B     XIT                                                              
         EJECT                                                                  
* VALIDATE WAGES FIELD (OPTIONAL).                                              
*                                                                               
         USING TAOAD,R4                                                         
VRWAGE   NTR1                                                                   
         L     R2,AWAGEFLD         R2=A(WAGE FIELD)                             
         MVC   TGUSCDE,=C'SOP'     SET USE CODE                                 
         BAS   RE,VRWAMT           VALIDATE INPUT                               
         B     XIT                                                              
         SPACE 3                                                                
* VALIDATE WARDROBE FEE FIELD (OPTIONAL).                                       
*                                                                               
         USING TAOAD,R4                                                         
VRWARD   NTR1                                                                   
         L     R2,AWARDFLD         R2=A(WARDROBE FEE FIELD)                     
         MVC   TGUSCDE,=C'OTH'     SET USE CODE                                 
         BAS   RE,VRWAMT           VALIDATE INPUT                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE SOAP AMOUNT FIELD AT R2 (OPTIONAL).                                  
* TGUSCDE IS USE CODE                                                           
*                                                                               
         USING TAOAD,R4                                                         
VRWAMT   NTR1                                                                   
         TM    4(R2),X'20'         IF FIELD HASN'T CHANGED THEN DONE            
         BO    VRWAX                                                            
         MVI   ELCODE,TAOAELQ      OVERSCALE AMOUNTS ELEMENT                    
         GOTO1 REMELEM             REMOVE OLD ELEMENT - COPY TO ELEM            
*                                                                               
         BAS   RE,PROCOAEL         PROCESS OVERSCALE AMOUNTS ELEMENT            
         BNE   VRWA20              RETURNS R3=A(END OF ELEMNT IN BLOCK)         
*                                                                               
*                                  HAVE PREVIOUS OVERSCALE AMOUNT               
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VRWA5                                                            
         LA    R4,BLOCK                                                         
         ZIC   R0,TAOANUM          ORIGINAL N'SUB ELEMENTS                      
         BCTR  R0,0                - 1                                          
         LTR   R0,R0                                                            
         BZ    VRWAX               DON'T BOTHER IF NONE LEFT                    
         STC   R0,TAOANUM          = NEW N'SUB ELEMS                            
*                                                                               
         IC    R0,TAOALEN          ORIGINAL LENGTH OF ELEMENT                   
         SH    R0,=H'7'            - 7                                          
         STC   R0,TAOALEN          = NEW LENGTH                                 
         B     VRWA30                                                           
*                                                                               
VRWA5    BAS   RE,VALAMT           HAVE PREV OVSC AMT & FIELD NOT EMPTY         
         MVC   DUB+3(4),FULL       CHANGE ORIGINAL AMOUNT IN DUB                
         MVC   0(7,R3),DUB         MOVE TO BLOCK                                
         B     VRWA30                                                           
*                                  NO PREVIOUS OVERSCALE AMOUNT                 
VRWA20   CLI   5(R2),0             IF FIELD IS EMPTY THEN DONE                  
         BE    VRWAX                                                            
         BAS   RE,VALAMT                                                        
         MVC   3(4,R3),FULL        MOVE AMOUNT TO BLOCK                         
         MVC   0(3,R3),TGUSCDE     AND SET USE CODE                             
*                                                                               
         LA    R4,BLOCK                                                         
         ZIC   R0,TAOALEN          ORIGINAL LENGTH OF ELEMENT                   
         AH    R0,=H'7'            + 7                                          
         STC   R0,TAOALEN          = NEW LENGTH                                 
         IC    R0,TAOANUM                                                       
         AH    R0,=H'1'                                                         
         STC   R0,TAOANUM          ADD 1 TO N'SUB ELEMENTS                      
*                                                                               
VRWA30   MVC   ELEM,BLOCK          MOVE NEW ELEMENT TO ELEM                     
         GOTO1 ADDELEM             ADD ELEMENT                                  
*                                                                               
VRWAX    B     XIT                                                              
         EJECT                                                                  
* COPIES THE OVERSCALE AMOUNTS ELEMENT AT ELEM TO BLOCK                         
* EXCEPT FOR THE SUB-ELEMENT FOR TGUSCDE, WHICH IS SAVED IN DUB.                
* RETURNS CC EQUAL IF FINDS THE SUB-ELEMENT, ELSE CC NOT EQUAL.                 
* RETURNS R3=A(END OF ELEMENT IN BLOCK)                                         
*                                                                               
PROCOAEL NTR1                                                                   
         USING TAOAD,R4                                                         
         XC    DUB,DUB                                                          
         LA    R4,ELEM             SET R4=A(ORIGINAL OVSCALE AMOUNT EL)         
         OC    ELEM,ELEM                                                        
         BNZ   PROA10              BRANCH IF FOUND                              
*                                                                               
         LA    R4,BLOCK            ELSE BUILD DUMMY ELEMENT                     
         XC    BLOCK(256),BLOCK                                                 
         MVI   TAOAEL,TAOAELQ                                                   
         MVI   TAOALEN,TAOALNQ                                                  
         LA    R3,TAOASBEL         SET R3=A(FIRST SUB-ELEMENT IN BLOCK)         
         B     PROANO              SET CC NOT EQ                                
*                                                                               
PROA10   MVC   BLOCK(TAOALNQ),0(R4)    JUST COPY UPTO FIRST SUB-ELEMENT         
         ZIC   R0,TAOANUM              R0=NUMBER OF SUB-ELS                     
         LA    R4,TAOASBEL             R4 = A(FIRST SUB-ELEMENT)                
         LA    R3,BLOCK+TAOASBEL-TAOAD   R3 = A(FIRST SUB EL IN BLOCK)          
*                                                                               
PROA15   CLC   TGUSCDE,0(R4)       IF MATCHING USE CODE                         
         BNE   PROA20                                                           
         OC    DUB,DUB             AND HAVEN'T FOUND ONE ALREADY                
         BNZ   PROA20                                                           
         MVC   DUB(7),0(R4)        COPY SUB ELEMENT TO DUB                      
         B     PROA25                                                           
PROA20   MVC   0(7,R3),0(R4)       ELSE COPY SUB ELEMENT TO ELEM                
         LA    R3,7(R3)            BUMP R3 TO NEXT SUB ELEMENT                  
PROA25   LA    R4,7(R4)            BUMP R4 TO NEXT SUB ELEMENT                  
*                                                                               
         BCT   R0,PROA15           LOOP TILL NO MORE SUB-ELEMENTS               
         OC    DUB,DUB                                                          
         BZ    PROANO              SET CC NO IF DIDN'T FIND SUB-ELEMENT         
         XR    RC,RC               ELSE SET CC YES                              
PROANO   LTR   RC,RC                                                            
         XIT1  REGS=(R3)           RETURN R3                                    
         EJECT                                                                  
* VALIDATE COMMENTS (OPTIONAL).                                                 
*                                                                               
VRCM     NTR1                                                                   
         L     R2,ACMFLD           R2 = A(COMMENTS FIELD)                       
*                                                                               
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',(R2)),TACMTYPG                         
*                                                                               
VRCMX    B     XIT                                                              
         SPACE 3                                                                
* VALIDATE CHECK COMMENTS (OPTIONAL).                                           
*                                                                               
VRCHCM   NTR1                                                                   
         L     R2,ACHCMFLD         R2 = A(CHECK COMMENTS FIELD)                 
*                                                                               
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',(R2)),TACMTYPC                         
*                                                                               
VRCHCMX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE STATUS CODES AND SET CORRESPONDING BITS.                             
*                                                                               
VRSTAT   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ASTATFLD         R2 = A(STATUS FIELD)                         
*                                                                               
         MVI   BYTE,0              PRE-CLEAR NO HOLD FEE NOTICE FLAG            
*                                  IF FIELD IS EMPTY THEN CLEAR BITS            
         XC    TACASTAT(2),TACASTAT                                             
         CLI   5(R2),0                                                          
         BE    VRSX                                                             
*                                  ELSE SCAN FIELD INTO SCANNER BLOCK           
         LA    R3,BLOCK            R3 = A(SCANNER BLOCK)                        
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',BLOCK),0                                
         CLI   4(R1),0                                                          
         BE    ERRINV                                                           
         CLI   4(R1),10                                                         
         BH    ERRINV                                                           
         ZIC   R0,4(R1)            FULL = NUMBER OF SCAN BLOCK ENTRIES          
*                                                                               
VRS10    MVC   ERRDISP,SCDISP1                                                  
         L     RF,ASTATTAB         LOOK IN STATUS TABLE FOR MATCH               
*                                                                               
VRS20    CLI   0(RF),X'FF'         ERROR IF END OF TABLE FOUND                  
         BE    ERRFLD2                                                          
         ZIC   RE,SCLEN1           IF MATCH THEN SET BIT                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),2(RF)                                                 
         BE    VRS30                                                            
         LA    RF,L'STATTAB(RF)    ELSE TRY NEXT TABLE ENTRY                    
         B     VRS20                                                            
*                                                                               
VRS30    MVC   ERRDISP,SCDISP2                                                  
         CLI   SCLEN2,0            ERROR IF RHS EXISTS                          
         BNE   ERRFLD2                                                          
         OC    TACASTAT(2),0(RF)   SET APPROPRIATE BIT IN APPR BYTE             
*                                                                               
         LA    R3,SCANNEXT         BUMP R3 TO NEXT SCANNER ENTRY                
         BCT   R0,VRS10                                                         
         MVI   ERRDISP,0                                                        
*                                                                               
VRSX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE PERFORMANCE RATE                                                     
*                                                                               
VRPERF   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TACRELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         L     R2,APERFFLD         VALIDATE PERFORMANCE RATE                    
         CLI   5(R2),0                                                          
         BE    VRPERFX                                                          
         LA    RE,CAELEM                                                        
         USING TACAD,RE                                                         
         OC    TACAGUA,TACAGUA                                                  
         BNZ   ERRINV              CAN'T HAVE PERF RATE & GUAR CODE             
         DROP  RE                                                               
         BAS   RE,VALAMT           SETS PERF RATE IN FULL                       
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING TACRD,R4                                                         
         MVI   TACREL,TACRELQ                                                   
         MVI   TACRLEN,TACRLNQ                                                  
         MVC   TACRSCAL,FULL       SET RATE                                     
         GOTO1 ADDELEM                                                          
*                                                                               
VRPERFX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE EPISODE NUMBERS                                                      
*                                                                               
VREPI    NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TASOELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING TASOD,R4                                                         
         MVI   TASOEL,TASOELQ                                                   
*                                                                               
         LA    R1,TASOSEPI                                                      
         USING TASOSEPI,R1                                                      
         XR    RF,RF               NUMBER OF EPISODES IN ELEMENT                
         LA    R3,EPITMAX          EPISODES FOR WEEK                            
         L     R2,AEPIFLD          R2 = A(EPISODE FIELD)                        
         BAS   RE,BUMP                                                          
         USING EPID,R2                                                          
*                                                                               
VREPI30  CLI   EPIINPH+5,0         IF CHECK OFF INPUT                           
         BE    VREPI50                                                          
         OC    EPINUM,EPINUM       AND EPISODE NUMBER,EPINUM                    
         BZ    VREPI50                                                          
         BAS   RE,VRCKOFF          SET STATUS EQUATE IN BYTE                    
         BNE   VRERROR                                                          
         MVC   TASOSTAT,BYTE                                                    
*********MVC   TASOEPI,EPINUM      SAVE EPISODE NUMBER                          
         PACK  DUB,EPINUM          SAVE BINARY EPISODE NUMBER                   
         CVB   RE,DUB                                                           
         STH   RE,TASOEPI                                                       
         LA    R1,L'TASOSEPI(R1)   NEXT EPISODE IN ELEMENT                      
         LA    RF,1(RF)            INCREMENT # IN ELEMENT                       
*                                                                               
VREPI50  LA    R2,EPILNQ(R2)                                                    
         BCT   R3,VREPI30                                                       
         DROP  R1                                                               
*                                                                               
         LTR   RF,RF               IF EPISODE NUMBERS                           
         BZ    VREPIX                                                           
         STC   RF,TASONUM          ADD ELEMENT                                  
         MH    RF,=Y(L'TASOSEPI)                                                
         LA    RF,TASOLNQ(RF)                                                   
         STC   RF,TASOLEN          TOTAL LENGTH OF ELEMENT                      
         GOTO1 ADDELEM                                                          
*                                                                               
VREPIX   B     XIT                                                              
         SPACE                                                                  
VRERROR  LA    R2,EPIINPH          INVALID CHECK OFF CODE                       
         B     ERRINV                                                           
         EJECT                                                                  
* VRCKOFF - VALIDATE CHECK OFF CODE                                             
*     XIT - CC EQUAL 0      BYTE = CATEGORY STATUS                              
*           CC NOT EQUAL 0  CHECK OFF CODE NOT VALID                            
VRCKOFF  NTR1                                                                   
         MVI   BYTE,0                                                           
         ZIC   R1,EPIINPH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HALF,EPIINP                                                      
         OC    HALF,=CL7' '        PAD WITH SPACES                              
         CLI   TGCAEQU,CTW         IF NOT WRITER                                
         BE    VRCOFF5                                                          
         CLI   TGCAEQU,CTW2                                                     
         BE    VRCOFF5                                                          
         CLI   TGCAEQU,CTW3                                                     
         BE    VRCOFF5                                                          
         CLC   HALF,=C'X '         INPUT MUST BE IN FORM OF AN X                
         B     XIT                 CC & BYTE SET                                
*                                                                               
VRCOFF5  L     RE,AWROLTAB         CHECK INPUT W/ VALID WRITER ROLES            
         USING WROLTABD,RE                                                      
VRCOFF10 CLC   WROLCHAR,HALF                                                    
         BE    VRCOFF20                                                         
         LA    RE,WROLTABL(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   VRCOFF10                                                         
         B     NO                  INVALID INPUT                                
*                                                                               
VRCOFF20 MVC   BYTE,WROLEQU        SET EQUATED STATUS CHECK OFF                 
         B     YES                                                              
         DROP  R4,R2,RE                                                         
         EJECT                                                                  
* THIS ROUTINE VALIDATES SCREEN AMOUNT (R2)                                     
*                                                                               
VALAMT   NTR1                                                                   
         ZIC   RF,5(R2)            SET LENGTH OF INPUT                          
         GOTO1 CASHVAL,DMCB,8(R2),(RF)  VALIDATE AMOUNT                         
         CLI   0(R1),0                                                          
         BNE   ERRINV                                                           
         TM    4(R1),X'80'                                                      
         BO    ERRINV                                                           
         MVC   FULL,4(R1)                                                       
         B     XIT                                                              
*                                                                               
* THIS ROUTINE BUMPS R2 TO THE NEXT SCREEN FIELD.                               
*                                                                               
BUMP     ZIC   R0,0(R2)            BUMP R2 TO NEXT FIELD                        
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
HITPFMSG MVI   MYMSGNO1,73         HIT PF19 TO DELETE                           
         B     INFXIT                                                           
*                                                                               
PGDEL    MVI   MYMSGNO1,74         PAGE DELETED                                 
         B     INFXIT                                                           
*                                                                               
HITPFEND MVI   MYMSGNO1,75         END OF LIST HIT PF19 TO DELETE               
         B     INFXIT                                                           
*                                                                               
ENDLIST  MVI   MYMSGNO1,10         END OF LIST                                  
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,CONRECH                                                       
         B     ERREXIT                                                          
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
ERRRNF   MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     ERREXIT                                                          
*                                                                               
ERRDUPP  MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         L     R2,ASSNFLD                                                       
         CLI   SCRFLAG,C'L'                                                     
         BE    *+8                                                              
         L     R2,ARECFLD                                                       
         B     ERREXIT                                                          
*                                                                               
ERREPI   MVI   MYMSGNO1,ERRECEPI    EPISODE NUMBER NOT FOUND                    
         MVI   MYMTYP,GTMERR       SET ERROR MSG TYPE                           
         MVI   BLOCK,L'TGEPI+1                                                  
         MVC   BLOCK+1(L'TGEPI),0(R3)                                           
         MVI   BLOCK+1+L'TGEPI,0                                                
         OI    GENSTAT2,USGETTXT                                                
         B     ERREXIT                                                          
*                                                                               
ERCRPLK  MVC   MYMSGNO,=Y(ERCPSLCK)    CORP RECORD LOCKED - NON-PAY             
         OI    GENSTAT2,USGETTXT   NEW THEEND FOR TWO BYTE ERROR MSGS           
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     ERREXIT                                                          
*                                                                               
ERRREC   MVI   ERROR,ERINVRAG      INVALID RECORD TYPE FOR THIS AGENCY          
         LA    R2,CONRECH                                                       
         B     ERREXIT                                                          
*                                                                               
ERRCTYPE MVI   ERROR,ERRECCTY      INVALID REC FOR COMMERCIAL TYPE              
         B     ERREXIT                                                          
*                                                                               
ERRNOIP  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     ERREXIT                                                          
*                                                                               
ERREXT   MVI   ERROR,INVALID       USER CAN'T ENTER EXTENSION SCREEN            
         LA    R2,CONRECH                                                       
         B     ERREXIT                                                          
*                                                                               
ERRNOCA  MVI   ERROR,ERNOCAST      NO CAST MEMBERS ON COMMERCIAL                
         LA    R2,CONRECH                                                       
         B     ERREXIT                                                          
*                                                                               
ERRPFK   MVI   ERROR,ERINVPFK      INVALID PFKEY PRESSED                        
         LA    R2,SCLL1H                                                        
         B     ERREXIT                                                          
*                                                                               
ERRCRP   MVI   ERROR,ERGT1CRP      GT 1 CORP - NEED EXACT CODE                  
         B     ERREXIT                                                          
*                                                                               
ERRTAX   MVC   MYMSGNO,=Y(ERTAXST) LOCAL DOESN'T MATCH TAX STATE                
         J     ERRSEND                                                          
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERRSEND                                                          
                                                                                
ERRSEND  OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     ERREXIT                                                          
*                                                                               
ERRFLD   STC   R0,ERRDISP                                                       
ERRFLD2  MVI   ERROR,INVALID       INVALID INPUT FIELD IN MIDDLE                
         B     ERREXIT                                                          
*                                                                               
ERW4LCK  MVI   ERROR,ERW4SLCK      W4 RECORD LOCKED                             
         B     ERREXIT                                                          
*                                                                               
ERNOTRS  MVC   MYMSGNO,=Y(ERNOTRST) TRUSTEE NOT ALLOWED ON CAST                 
         B     ERRSEND                                                          
*                                                                               
INFXIT   LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
ERREXIT  GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
XFFS     DC    16X'FF'                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE READS THROUGH THE ACTIVE CAST KEYS FOR THIS COMM'L,              
* LOCKING THE FIRST, AND DETERMINES THE NEXT AVAILABLE SEQUENCE NUMBER.         
* IF PARMAMETER 1 IS NOT ZERO, THEN THE ROUTINE WILL BUILD THE LIST OF          
* CAST RECORD DISK ADDRESSES.  IN ALL CASES, THE ROUTINE WILL COUNT             
* THE NUMBER OF ANNOUNCERS AND NON-MUSICIANS WHICH WILL ALLOW THE               
* PROGRAM TO MAINTAIN THE 'ANOUNCER ONLY NON-MUSICIAN' BIT IN THE               
* COMMERCIAL RECORD.  THE VERIFY PROGRAM NEEDS TO KNOW IF THE CAST HAS          
* A MUSICIAN IN IT.  THIS ROUTINE WILL SET THE CVERSMUS BIT IN CVERSTAT         
* IF IT DOES.                                                                   
*                                                                               
BLDLIST  NMOD1 0,**BLDL**                                                       
         L     RC,4(R1)            RC = A(GENCON STORAGE)                       
         MVC   FULL,0(R1)          SAVE PARAMETER 1                             
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS BACK DELETED KEYS                
*                                                                               
         XC    NUMANN,NUMANN       NUMBER OF ANNOUNCERS = 0                     
         XC    NUMNONM,NUMNONM     NUMBER OF NON-MUSICIANS = 0                  
*                                                                               
         OC    FULL,FULL           IF CALLER WANTS DISK ADDRESS LIST            
         BZ    BL10                                                             
         LA    RE,DALIST           THEN CLEAR DISK ADDRESS LIST                 
         LA    RF,DALISTL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING TLCAD,R4                                                         
BL10     LA    R4,KEY              BUILD KEY WITH CODE/COMM ID NUMBER           
         XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
*                                                                               
         LA    R3,DALIST           R3 = A(DISK ADDRESS LIST)                    
         SR    R0,R0               R0 = NUMBER OF ITEMS                         
*                                                                               
         MVI   RDUPDATE,C'Y'       READ AND LOCK FIRST KEY                      
         GOTO1 HIGH                                                             
*                                                                               
*                                  WHILE SAME COMMERCIAL                        
BL20     CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BNE   BL100                                                            
*                                                                               
         SR    RF,RF               IF SORT KEY SEQUENCE NUM > HIGHEST           
         ICM   RF,3,TLCASEQ                                                     
*                                                                               
         USING TLDRD,R4                                                         
         CLI   ACTNUM,ACTLDEL      IF USES WANTS DELETED RECORDS                
         BNE   BL30                                                             
         TM    TLDRSTAT,X'40'      THEN SKIP RECORDS THAT AREN'T                
         BZ    BL90                                                             
         B     BL40                                                             
*                                                                               
BL30     TM    TLDRSTAT,X'80'      ELSE SKIP RECORDS THAT ARE                   
         BO    BL90                                                             
*                                                                               
BL40     OC    FULL,FULL           IF CALLER WANTS DISK ADDRESS LIST            
         BZ    *+10                                                             
         MVC   0(4,R3),TLDRDA      THEN SAVE DISK ADDRESS                       
*                                                                               
*                                  LOOK UP CATEGORY CODE IN TABLE               
         USING TLCAD,R4                                                         
         GOTO1 CATVAL,DMCB,TLCACAT                                              
*                                                                               
         CLI   TGCAEQU,CTANN       IF CAST MEMBER IS AN ANNOUNCER               
         BNE   *+16                                                             
         L     RF,NUMANN           THEN INCREMENT NUMBER OF ANNOUNCERS          
         LA    RF,1(RF)                                                         
         ST    RF,NUMANN                                                        
*                                                                               
         TM    TLCASORT,TLCASRMQ   IF MUS. BIT ISN'T SET IN SORT KEY            
         BO    BL50                                                             
         L     RF,NUMNONM          THEN INCREMENT NUMBER OF NON-MUSICS          
         LA    RF,1(RF)                                                         
         ST    RF,NUMNONM                                                       
         B     BL60                                                             
*                                                                               
BL50     OI    CVERSTAT,CVERSMUS   ELSE SET VERIFY STATUS MUSICIAN BIT          
*                                                                               
BL60     AH    R0,=H'1'            INCREMENT NUMBER OF ITEMS                    
*                                                                               
         LA    R3,4(R3)            BUMP TO NEXT DISK ADDRESS                    
*                                                                               
BL90     GOTO1 SEQ                 READ NEXT KEY                                
         B     BL20                LOOP BACK                                    
*                                                                               
BL100    OC    FULL,FULL           IF CALLER WANTS DISK ADDRESS LIST            
         BZ    BL110                                                            
         ST    R0,NUMITEMS         SAVE NUMBER OF ITEMS                         
         XC    ITEM,ITEM           SET ITEM NUMBER TO FIRST PAGE                
*                                                                               
BL110    CLI   ACTNUM,ACTLDEL      IF NOT ACTION LDELETE                        
         BE    *+8                                                              
         NI    DMINBTS,X'F7'       THEN CLEAR PASS BACK DELETED FLAG            
*                                                                               
BLX      XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* BUILD THE FIRST FOUR BYTES OF THE SORT KEY FROM THE RECORD DATA.              
*                                                                               
BLDSORT  NMOD1 0,**BLDS**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R3,AIO              R3 = A(CAST RECORD)                          
         USING TLCAD,R3                                                         
*                                  PRE-CLEAR FIRST 4 BYTES OF SORT KEY          
         XC    TLCASRT,TLCASRT                                                  
*                                                                               
         L     R4,AIO              R4 = A(CAST ELEMENT)                         
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
*                                                                               
         GOTO1 UNIVAL,DMCB,TACAUN  GET UNION TABLE ENTRY                        
*                                                                               
*        TM    TGUNEQU,AFM         IF MUSICIAN                                  
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    *+8                                                              
         OI    TLCASORT,TLCASRMQ   THEN SET HIGHEST BIT                         
*                                                                               
         TM    TGCATYPE,EXTRA      IF EXTRA                                     
         BZ    *+8                                                              
         OI    TLCASORT,TLCASREQ   THEN SET NEXT HIGHEST BIT                    
*                                                                               
         L     R2,AONOFFLD         IF OFF CAMERA                                
         CLC   8(3,R2),=C'OFF'                                                  
         BE    BS10                                                             
*        TM    TGUNEQU,AFM         OR MUSICIAN                                  
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    *+8                                                              
BS10     OI    TLCASORT,TLCASRFQ   THEN SET NEXT HIGHEST BIT                    
*                                                                               
         L     R4,AIO              R4 = A(CAST DETAILS ELEMENT)                 
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
*                                                                               
         CLI   TACALEN,X'2F'       IF LENGTH IS OLD LENGTH                      
         BE    *+14                                                             
         OC    TACAGUA,TACAGUA     OR IF NOT GUARANTEE                          
         BNZ   *+8                                                              
         OI    TLCASORT,TLCASRGQ   THEN SET NEXT HIGHEST BIT                    
*                                                                               
*                                  SAVE CATEGORY SORT CODE                      
         MVC   TLCASRSC,TGCASORT                                                
*                                                                               
BSX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE TESTS IF GENCON HAS CALLED THE PROGRAM WITH ITS FIRST            
* MODE.  IF SO THE ROUTINE WILL ESTABLISH IF THE SCREEN HAS CHANGED             
* AND, FOR THE EXTENSION SCREEN, HANDLE SPECIAL INITIALIZATION LOGIC.           
*                                                                               
TESTFRST NMOD1 0,**TSTF**                                                       
         L     RC,0(R1)            RC = A(GENCON STORAGE)                       
*                                                                               
         TM    TRNSTAT,FRSTMODE    IF THIS IS FIRST MODE PASSED BY GENC         
         BZ    TFX                                                              
         NI    TRNSTAT,ALL-FRSTMODE   THEN RESET FIRST MODE BIT                 
*                                                                               
         MVI   ACCESSED,C'Y'                                                    
         MVC   FULL,CASTCMSK       VALID STAFF ACCESS BITS FOR ADD/CHG          
         NC    FULL,SECMASKS       MASK BIT FOR THIS STAFF TYPE                 
         BNZ   TF20                                                             
         CLC   TWAAGY,=CL2'D2'                                                  
         BE    TF20                                                             
         CLC   TWAAGY,=CL2'D3'                                                  
         BE    TF20                                                             
         CLC   TWAAGY,=CL2'D8'                                                  
         BE    TF20                                                             
         CLC   TWAAGY,=CL2'DP'                                                  
         BE    TF20                                                             
         MVI   ACCESSED,C'N'                                                    
*                                                                               
TF20     BAS   RE,TESTLIST         TEST SCREEN CALLED FROM CAST LIST            
*                                                                               
         MVI   SCRCHANG,C'N'       IF SCREEN HAS CHANGED THEN                   
         TM    SCRSTAT,SCRCHG          SCRCHANG = 'N'                           
         BZ    *+8                                                              
         MVI   SCRCHANG,C'Y'       ELSE SCRCHANG = 'Y'                          
*                                                                               
         CLI   SCRFLAG,C'L'        IF THIS IS THE LIST SCREEN                   
         BNE   TF50                                                             
*                                                                               
         TM    TRNSTAT,RETURNED    THEN IF JUST RETURNED FROM EXT SCR           
         BZ    TFX                                                              
         OC    CASTDA,CASTDA       AND RECORD WAS BEING CHANGED                 
         BZ    TFX                                                              
         LA    RF,DALIST           SAVE D/A OF SELECTED RECORD IN               
         A     RF,DALPTR               D/A LIST                                 
         MVC   0(4,RF),CASTDA                                                   
         B     TFX                                                              
*                                                                               
TF50     BAS   RE,TRAPEXT          TEST EXTENSION SCREEN TRAPS                  
*                                                                               
*                                  LOCK LIST AND GET ANNONLY STATUS             
         GOTO1 ABLDLIST,DMCB,0,(RC)                                             
*                                                                               
         CLI   PUSHED,C'Y'         IF EXTENSION CALLED FROM LIST                
         BNE   TFX                                                              
*                                                                               
         CLI   SCRCHANG,C'Y'       THEN IF THIS IS FIRST TIME                   
         BNE   *+8                                                              
         BAS   RE,EFINDREC         THEN GET DISK ADDRESS OF RECORD              
*                                                                               
*                                  IF USER HASN'T CHANGED REC/ACT               
         TM    TRNSTAT,RACHANG+USERCHA                                          
         BNZ   *+10                                                             
         MVC   SVACTION,CONACT     SAVE ACTION FIELD CONTENTS                   
*                                                                               
         MVC   CONACT,=CL8'SELECT' DISPLAY ACTION 'SELECT' TO USER              
*                                                                               
TFX      B     XIT1                                                             
         EJECT                                                                  
* THIS ROUTINE USES THE CALL STACK TO DETERMINE IF THIS PROGRAM WAS             
* CALLED BY WAY OF A SELECTION FROM THE CAST LIST.  THIS KNOWLEDGE IS           
* USED BY THE EXTENSION SCREEN IN VARIOUS PLACES.  THE ROUTINE WILL             
* SET THE FLAG, PUSHED, TO 'Y' IF THE SCREEN WAS SELECTED FROM THE CAST         
* LIST AND 'N' OTHERWISE.                                                       
*                                                                               
TESTLIST NTR1                                                                   
         MVI   PUSHED,C'N'         SET PUSHED FLAG TO 'N'                       
*                                                                               
         CLI   CALLSP,0            IF THE STACK IS CLEAR                        
         BE    TLSX                THEN DONE                                    
*                                                                               
         ZIC   RF,CALLSP           IF THE PREV SCREEN IS NOT CAST LIST          
         LA    RF,CALLSTCK-1(RF)                                                
         CLI   0(RF),SCR01                                                      
         BNE   TLSX                THEN DONE                                    
*                                                                               
         MVI   PUSHED,C'Y'         SET PUSHED FLAG TO 'Y'                       
*                                                                               
TLSX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE TRAPS TWO UNEXCEPTABLE CONDITIONS ON THE CAST EXTENSION          
* SCREEN:                                                                       
*                                                                               
*     1) THE ACTION IS ADD.                                                     
*     2) THIS IS THE FIRST TIME THROUGH THE EXTENSION SCREEN, THE               
*        ACTION IS NOT SELECT, AND THE SCREEN WAS NOT SELECTED FROM THE         
*        CAST LIST.                                                             
*                                                                               
* IF EITHER OF THE TWO CONDITIONS ARE TRUE THE ROUTINE WILL RESTORE THE         
* SCREEN AND GIVE THE 'INVALID RECORD/ACTION COMBINATION' MESSAGE TO            
* THE USER AND EXIT.  OTHERWISE, IT RETURNS CONTROL TO THE CALLING              
* ROUTINE.                                                                      
*                                                                               
TRAPEXT  NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF THE ACTION IS ADD                         
         BE    TE10                                                             
*                                                                               
* NO-OP  CLI   SCRCHANG,C'Y'       OR THIS IS FIRST TIME FOR SCREEN             
* 8/3 DH BNE   TEX                                                              
         CLI   ACTNUM,ACTSEL       OR IF THE ACTION IS NOT SELECT               
         BE    TEX                                                              
         CLI   PUSHED,C'Y'         AND THE PREV SCREEN IS NOT CAST LIST         
         BE    TEX                                                              
*                                  THEN READ OLD SCREEN INTO TIA                
TE10     GOTO1 GETTWA,DMCB,(X'00',ATIA)                                         
*                                                                               
         LA    R0,T702FFD          MOVE FIRST 64 BYTES TO BEGIN OF TWA          
         LA    R1,64                                                            
         L     RE,ATIA                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,=A(5010)         MOVE FROM CONTAGH TO END                     
         LA    R0,CONTAGH-T702FFD                                               
         SR    R1,R0                                                            
         LA    R0,CONTAGH                                                       
         L     RE,ATIA                                                          
         LA    RE,CONTAGH-T702FFD(RE)                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                  CLR INSERT CURSOR BIT FROM ALL FLDS          
         GOTO1 FLDVAL,DMCB,CONSERVH,(X'04',999)                                 
*                                                                               
         MVI   SVSCR,0             CLEAR PREVIOUS SCREEN NUMBER                 
         MVI   CALLSP,0            CLEAR CALLPROG STACK POINTER                 
         XC    SVACTION,SVACTION   CLEAR SAVED ACTION                           
*                                                                               
         MVI   ERROR,INVRCACT      GIVE INVALID RECORD/ACTION COMBO             
         LA    R2,CONRECH              MESSAGE AND EXIT                         
         B     ERREXIT1                                                         
*                                                                               
TEX      B     XIT1                                                             
         EJECT                                                                  
* THIS ROUTINE FINDS THE DISK ADDRESS LIST ENTRY FOR THE RECORD THAT            
* WAS SELECTED AND SAVES A POIONTER TO IT, ITS DISPLACEMENT FROM THE            
* BEGINNING OF DALIST, IN EDALPTR.  ADDITIONALLY IT SAVES THE DISK              
* ADDRESS IN EDISKADD FOR USE BY THE EXTENSION SCREEN CODE.                     
*                                                                               
EFINDREC NTR1                                                                   
         L     R0,ROWADDR          R0 = DISPLACEMENT TO LIST LINE               
         SR    R0,RA                                                            
         L     R2,ALINATAB         R2 = A(DISPLACEMENTS TO LIST LINES)          
         L     R3,ITEM             R3 = A(FIRST DISK ADDRESS OF LIST)           
         SLL   R3,2                                                             
         LA    R3,DALIST(R3)                                                    
*                                                                               
EFR10    OC    0(2,R2),0(R2)       DIE IF END OF DISPLACEMENT TABLE             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLM   R0,3,0(R2)          IF MATCH FOUND THEN DONE                     
         BE    EFR20                                                            
*                                                                               
         LA    R2,2(R2)            ELSE BUMP R2 TO NEXT DISPLACEMENT            
         LA    R3,4(R3)            BUMP R3 TO NEXT DISK ADDRESS                 
         B     EFR10               LOOP BACK                                    
*                                                                               
EFR20    MVC   EDISKADD,0(R3)      SAVE DISK ADDRESS IN EDISKADD                
*                                                                               
         LA    RF,DALIST           SAVE POINTER IN EDALPTR                      
         SR    R3,RF                                                            
         ST    R3,EDALPTR                                                       
*                                                                               
EFRX     B     XIT1                                                             
         EJECT                                                                  
* MISCELLANEOUS CODE FOR TESTFRST NMOD.                                         
*                                                                               
ERREXIT1 GOTO1 EXIT                                                             
*                                                                               
XIT1     XIT1                                                                   
*                                                                               
SSNSPACE DS    CL9' '     SPACES FIELD FOR SS#                                  
*                                                                               
* TABLE OF STATUS CODES AND THERE DISPLAYABLE FORM.                             
*                                                                               
STATTAB  DS    0CL12                                                            
         DC    AL1(TACASTAG,0),CL10'CHAGT'                                      
         DC    X'FF'                                                            
         SPACE 3                                                                
WROLTAB  DS    0CL3                                                             
         DC    AL1(TASOSH+TASOSS+TASOSB),CL2'A '                                
         DC    AL1(TASOSH+TASOSS),CL2'HS'                                       
         DC    AL1(TASOSH+TASOSB),CL2'HB'                                       
         DC    AL1(TASOSS+TASOSB),CL2'SB'                                       
         DC    AL1(TASOSH),CL2'H '                                              
         DC    AL1(TASOSS),CL2'S '                                              
         DC    AL1(TASOSB),CL2'B '                                              
         DC    X'FF'                                                            
         SPACE                                                                  
* PFTABLE FOR CAST LIST IF DELETE OPTION NOT REQUESTED.                         
*                                                                               
LPFTAB   DS    0H                                                               
         DC    AL1(LPF3X-*,3,0,(LPF3X-LPF3)/KEYLNQ,0)                           
         DC    CL3'W4 ',CL8'W4      ',CL8'DISPLAY'                              
LPF3     DC    AL1(KEYTYCUR,L'SCLSSN-1),AL2(SCLSSN-SCLSSN)                      
LPF3X    EQU   *                                                                
*                                                                               
         DC    AL1(LPF13DX-*,13,0,(LPF13DX-LPF13D)/KEYLNQ,0)                    
         DC    CL3'CH ',CL8'CHECK   ',CL8'LIST'                                 
LPF13D   DC    AL1(KEYTYCUR,L'SCLSSN-1),AL2(SCLSSN-SCLSSN)                      
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
LPF13DX  EQU   *                                                                
*                                                                               
         DC    AL1(LPF14X-*,14,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        ',CL8'       '                              
LPF14X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF15DX-*,15,0,(LPF15DX-LPF15D)/KEYLNQ,0)                    
         DC    CL3'   ',CL8'CEPISODE',CL8'LIST'                                 
LPF15D   DC    AL1(KEYTYCUR,L'SCLSSN-1),AL2(SCLSSN-SCLSSN)                      
LPF15DX  EQU   *                                                                
*                                                                               
         DC    AL1(LPF17DX-*,17,0,(LPF17DX-LPF17D)/KEYLNQ,0)                    
         DC    CL3'CE ',CL8'CERROR  ',CL8'DISPLAY'                              
LPF17D   DC    AL1(KEYTYCUR,L'SCLSSN-1),AL2(SCLSSN-SCLSSN)                      
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYCUR,L'SCLCAT-1),AL2(SCLCAT-SCLSSN)                      
LPF17DX  EQU   *                                                                
*                                                                               
         DC    AL1(LPF19X-*,19,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        ',CL8'       '                              
LPF19X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF22X-*,22,PFTINT+PFTCPROG,(LPF22X-LPF22)/KEYLNQ,0)         
         DC    CL3'S  ',CL8'        ',CL8'DISPLAY'                              
LPF22    DC    AL1(KEYTYCOM,0),AL2(0)                                           
LPF22X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF23X-*,23,PFTINT+PFTCPROG,(LPF23X-LPF23)/KEYLNQ,0)         
         DC    CL3'C  ',CL8'        ',CL8'CHANGE'                               
LPF23    DC    AL1(KEYTYCOM,0),AL2(0)                                           
LPF23X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF24X-*,24,PFTINT+PFTCPROG,(LPF24X-LPF24)/KEYLNQ,0)         
         DC    CL3'DE ',CL8'        ',CL8'DELETE'                               
LPF24    DC    AL1(KEYTYCOM,0),AL2(0)                                           
LPF24X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
* PFTABLE FOR CAST LIST IF DELETE OPTION REQUESTED.                             
*                                                                               
DPFTAB   DS    0H                                                               
         DC    AL1(DPF3X-*,3,0,(DPF3X-DPF3)/KEYLNQ,0)                           
         DC    CL3'W4 ',CL8'W4      ',CL8'DISPLAY'                              
DPF3     DC    AL1(KEYTYCUR,L'SCLSSN-1),AL2(SCLSSN-SCLSSN)                      
DPF3X    EQU   *                                                                
*                                                                               
         DC    AL1(DPF13X-*,13,0,(DPF13X-DPF13)/KEYLNQ,0)                       
         DC    CL3'CH ',CL8'CHECK   ',CL8'LIST'                                 
DPF13    DC    AL1(KEYTYCUR,L'SCLSSN-1),AL2(SCLSSN-SCLSSN)                      
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
DPF13X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF14X-*,14,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        ',CL8'       '                              
DPF14X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF19X-*,19,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        ',CL8'       '                              
DPF19X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF22X-*,22,PFTINT+PFTCPROG,(DPF22X-DPF22)/KEYLNQ,0)         
         DC    CL3'S  ',CL8'        ',CL8'DISPLAY'                              
DPF22    DC    AL1(KEYTYCOM,0),AL2(0)                                           
DPF22X   EQU   *                                                                
*                                                                               
         DC    AL1(DPF23X-*,23,PFTINT+PFTCPROG,(DPF23X-DPF23)/KEYLNQ,0)         
         DC    CL3'R  ',CL8'        ',CL8'RESTORE'                              
DPF23    DC    AL1(KEYTYCOM,0),AL2(0)                                           
DPF23X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
POPTAB   DS    0H                                                               
         DC    AL1(PT21X-*,21,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PT21X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
EPFTAB   DS    0H                                                               
         DC    AL1(EPF13X-*,13,0,(EPF13X-EPF13)/KEYLNQ,0)                       
         DC    CL3'CH ',CL8'CHECK   ',CL8'LIST'                                 
EPF13    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
EPF13X   EQU   *                                                                
*                                                                               
         DC    AL1(EPF15X-*,15,0,(EPF15X-EPF15)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'CEPISODE',CL8'LIST'                                 
EPF15    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
EPF15X   EQU   *                                                                
*                                                                               
         DC    AL1(EPF17X-*,17,0,(EPF17X-EPF17)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'CERROR  ',CL8'DISPLAY'                              
EPF17    DC    AL1(KEYTYTWA,L'SCASSN-1),AL2(SCASSN-T702FFD)                     
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYTWA,3-1),AL2(SCASORT+10-T702FFD)                        
EPF17X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
LINATAB  DS    0H                  DISPLACEMENT TO LIST LINES 1-4               
         DC    AL2(SCLL1H-T702FFD)                                              
         DC    AL2(SCLL2H-T702FFD)                                              
         DC    AL2(SCLL3H-T702FFD)                                              
         DC    AL2(SCLL4H-T702FFD)                                              
         DC    AL2(0)                                                           
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,31,C'SOAP CAST LIST'                                          
         SSPEC H2,31,C'--------------'                                          
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* UNPACK THE PID NUMBER TO A SS NUMBER                                          
* R2 POINTS TO THE SSN FIELD HEADER                                             
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
UNPKPID  NTR1  BASE=*,LABEL=*                                                   
         MVC   BLOCK(L'TGSSN),8(R2)       SAVE PID/SSN                          
         OC    BLOCK(L'TGSSN),SSNSPACE                                          
         CLI   5(R2),9                                                          
         BE    UNPIDX                                                           
         CLI   5(R2),6                                                          
         BH    ERRINV                                                           
         MVC   TGPID,8(R2)                                                      
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   UNPIDX                                                           
         MVC   8(L'TGSSN,R2),TGSSN                                              
         MVI   5(R2),9                                                          
*                                                                               
UNPIDX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* PACK THE SS NUMBER INTO A PID NUMBER                                          
* R2 POINTS TO THE SSN FIELD HEADER                                             
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
PKSSN    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   8(L'TGSSN,R2),SSNSPACE                                           
         MVC   8(L'TGPID,R2),TGPID                                              
         MVI   5(R2),6                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
PKSSNX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAW4LCRP                                                       
       ++INCLUDE TAGENCAD                                                       
         EJECT                                                                  
NAMESD   DSECT                                                                  
         DS    XL8                                                              
NAMSSN   DS    CL16                W4 NAME                                      
         DS    XL6                                                              
NAMNCDE  DS    CL16                AGENT NAME                                   
         DS    XL2                                                              
NAMCORP  DS    CL16                CORPORATION NAME                             
         DS    XL6                                                              
NAMGUAR  DS    CL6                 GUARANTEE INDICATOR                          
         SPACE                                                                  
EPID     DSECT                                                                  
         DS    CL8                                                              
EPINUM   DS    CL5                 EPISODE NUMBER                               
EPISPAR  DS    CL1                 STARTING PAREN C'('                          
EPIINPH  DS    CL8                                                              
EPIINP   DS    CL2                 INPUT C'X'                                   
         DS    CL8                 EXTENDED INPUT HEADER                        
         DS    CL8                                                              
EPIEPAR  DS    CL1                 CLOSING PAREN C')'                           
         DS    CL1                                                              
EPILNQ   EQU   (*-EPID)                                                         
         SPACE                                                                  
* WRITER ROLE TABLE EQUATE/CHECK OFF                                            
*                                                                               
WROLTABD DSECT                                                                  
WROLEQU  DS    XL1                 EQUATE FOR THIS CATEGORY                     
WROLCHAR DS    CL2                 CHECK OFF                                    
WROLTABL EQU   (*-WROLTABD)                                                     
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TAGENEQUS                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRCAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR01D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098TAGENCA   02/21/17'                                      
         END                                                                    
