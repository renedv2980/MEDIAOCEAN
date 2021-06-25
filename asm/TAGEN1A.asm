*          DATA SET TAGEN1A    AT LEVEL 122 AS OF 02/16/17                      
*PHASE T7021AB,*                                                                
         TITLE 'T7021A - CAST LIST/MAINTENANCE'                                 
T7021A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7021A,R8,R7,R5                                                
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
         CLI   ACTNUM,ACTVER       IF NOT VERIFY                                
         BE    M10                                                              
         CLI   TWASCR,SCR0A        AND CAST/LIST SCREEN                         
         BNE   M05                                                              
         NI    SCLVERLH+1,X'F7'    SHOW VERSION FIELD                           
         NI    SCLVERH+1,X'F7'                                                  
         NI    SCLVERH+1,X'DF'                                                  
         OI    SCLVERLH+6,X'80'                                                 
         NI    SCLVERH+6,X'80'                                                  
*                                                                               
M05      CLI   MODE,SETFILE        IGNORE THIS MODE                             
         BE    XIT                                                              
*                                                                               
         CLI   MODE,XRECPUT        IF WE JUST PUT A RECORD                      
         BNE   *+8                                                              
         BRAS  RE,CHKREL           CHECK IF RELEASE REP WAS REQUESTED           
*                                                                               
         MVI   BYTE,0                                                           
         CLI   ACTNUM,ACTVER       IF COMING FROM VERIFY                        
         BNE   M10                                                              
         NI    STATVER,X'FF'-(TACOUVNM+TACOUVMU)                                
         MVI   BYTE,X'40'             DO NOT CLEAR STORAGE                      
*                                                                               
M10      GOTO1 INITIAL,DMCB,(BYTE,0)  INITIALIZE OVERLAY                        
         CLI   ACTNUM,ACTLIST         ACTION LIST?                              
         BE    M20                                                              
         CLI   ACTNUM,ACTLDEL         LDELETE? OR                               
         BE    M20                                                              
         CLI   ACTNUM,ACTPDEL         PDELETE?                                  
         BE    M20                                                              
         CLI   ACTNUM,ACTVER          IF ACTION VERIFY,                         
         BNE   M15                                                              
         CLI   TWASCR,SCR0A           AND NOT CAST/LIST SCREEN                  
         BE    M20                                                              
M15      MVC   SCASPID(7),=C'Pid Num'                                           
         OI    SCASPIDH+6,X'80'                                                 
         B     M30                                                              
*                                                                               
M20      MVC   SCLTAG(7),=C'Sel^Pid'  CAST/LIST SCREEN                          
         OI    SCLTAGH+6,X'80'                                                  
*                                                                               
*                                  SAVE A(BUILD DISK/ADDR LIST ROUTINE)         
M30      L     RF,=A(BLDLIST-T7021A)                                            
         AR    RF,RB                                                            
         ST    RF,ABLDLIST                                                      
*                                  SAVE A(BUILD SORT KEY ROUTINE)               
         L     RF,=A(BLDSORT-T7021A)                                            
         AR    RF,RB                                                            
         ST    RF,ABLDSORT                                                      
*                                  SAVE A(TEST FIRST MODE ROUTINE)              
         L     RF,=A(TESTFRST-T7021A)                                           
         AR    RF,RB                                                            
         ST    RF,ATSTFRST                                                      
*                                  SAVE A(STATUS CODES TABLE)                   
         L     RF,=A(STATTAB-T7021A)                                            
         AR    RF,RB                                                            
         ST    RF,ASTATTAB                                                      
*                                  SAVE A(NORMAL LIST PF TABLE)                 
         L     RF,=A(LPFTAB-T7021A)                                             
         AR    RF,RB                                                            
         ST    RF,ALPFTAB                                                       
*                                  SAVE A(DELETED LIST PF TABLE)                
         L     RF,=A(DPFTAB-T7021A)                                             
         AR    RF,RB                                                            
         ST    RF,ADPFTAB                                                       
*                                  SAVE A(FAKE POP PF TABLE)                    
         L     RF,=A(POPTAB-T7021A)                                             
         AR    RF,RB                                                            
         ST    RF,APOPTAB                                                       
*                                  SAVE A(EXTENSION SCREEN PF TABLE)            
         L     RF,=A(EPFTAB-T7021A)                                             
         AR    RF,RB                                                            
         ST    RF,AEPFTAB                                                       
*                                  SAVE A(LIST SCREEN LINE ADDRS TABLE)         
         L     RF,=A(LINATAB-T7021A)                                            
         AR    RF,RB                                                            
         ST    RF,ALINATAB                                                      
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
*                                                                               
         MVI   SCRFLAG,C'E'        IF SCREEN IS EXTENSION SCREEN THEN           
         CLI   TWASCR,SCR1A            SCREEN FLAG = 'E'                        
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
LD10     BAS   RE,LSVALKEY         VALIDATE KEY                                 
         B     LDX                                                              
*                                                                               
LD20     CLI   ACTNUM,ACTVER       IF ACTION IS VERIFY                          
         BNE   LD25                                                             
         BAS   RE,LVERIFY          THEN VERIFY RECORD                           
         B     LDX                                                              
*                                                                               
LD25     BAS   RE,LVREC            ELSE VALIDATE RECORD                         
         B     LDX                                                              
*                                                                               
LD30     BAS   RE,LPRINTR          PRINT REPORT                                 
*                                                                               
LDX      B     XIT                                                              
         EJECT                                                                  
* LIST SCREEN MODE VALKEY ROUTINE.                                              
*                                                                               
LSVALKEY NTR1                                                                   
         MVI   KEYCHANG,C'N'                                                    
         TM    SCLAGYH+4,X'20'     IF AGENCY FIELD HAS CHANGED                  
         BZ    LVK10                                                            
         TM    SCLCIDH+4,X'20'     OR COMMERCIAL ID FIELD HAS CHANGED           
         BZ    LVK10                                                            
         TM    SCLVERH+1,X'20'     OR VERSION FIELD IS NOT PROTECTED            
         BO    *+12                                                             
         TM    SCLVERH+4,X'20'     AND HAS CHANGED                              
         BZ    LVK10                                                            
         TM    SCLOPTH+4,X'20'     OR OPTIONS FIELD HAS CHANGED                 
         BZ    LVK20                                                            
         CLC   ACTNUM,TWALACT      OR ACTION HAS CHANGED                        
         BE    LVK20                                                            
         TM    TRNSTAT,RETURNED    AND WE HAVEN'T JUST POPPED                   
         BO    LVK20                                                            
         CLI   ACTNUM,ACTVER       OR ACTION IS VERIFY                          
         BE    LVK20                                                            
*                                                                               
LVK10    MVI   KEYCHANG,C'Y'       SET KEY CHANGE FLAG                          
         MVI   SCLOPTH+5,0         AND CLEAR OPTIONS                            
         XC    SCLOPT,SCLOPT                                                    
*                                                                               
LVK20    GOTO1 CHKCLG,DMCB,SCLAGYH,SCLCIDH  CHECK/HANDLE CLI GRP                
         MVI   BYTE,0              ENFORCE AGENCY LIMIT RESTRICTIONS            
         BE    *+8                                                              
         MVI   BYTE,X'80'          SKIP AGENCY LIMIT RESTRICTIONS               
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'20',SCLAGYH)                       
         MVI   AYSTAT,0                                                         
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
         L     R4,AIO                                                           
         USING TAAYD,R4            KEEP 2ND STATUS BYTE TO                      
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL            CHECK LOCKED STATUS OF AGENCY                
         BNE   *+16                                                             
         MVC   AYSTAT,TAAYSTA3                                                  
         MVC   AYOFF,TAAYTPOF      SAVE OFFICE FOR RELEASE REP                  
         MVC   SVAYSTA6,TAAYSTA6                                                
                                                                                
         GOTO1 RECVAL,DMCB,(X'30',TLCOICDQ),(X'08',SCLCIDH),SCLCIDNH            
         MVI   COMSTAT,0                                                        
                                                                                
         USING TLCOPD,R4                                                        
         LA    R4,KEY              SAVE INTERNAL CMCL NUMBER IN GLOBAL          
         MVC   TGCOM,TLCOICOM                                                   
                                                                                
         CLI   TLCOIVER,26         IF VERSION CODE IS GREATER THAN 26           
         BNH   LVK30               READ MAIN COMMERCIAL RECORD                  
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'AC',0),SCLCIDNH                          
         BE    LVK30                                                            
         DC    H'00'                                                            
         DROP  R4                                                               
*                                                                               
LVK30    L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING TACOD,R4            KEEP 2ND STATUS BYTE TO                      
         LA    R2,SCLCIDH                                                       
         CLI   TACOMED,TACOMEDE    REC NOT FOUND                                
         BNE   LVK40                                                            
         MVC   SCLCIDN,=CL20' '    CLEAR OUT COMML NAME                         
         OI    SCLCIDNH+6,X'80'    TRANSMIT                                     
         B     ERRRNF                                                           
                                                                                
LVK40    L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
*                                                                               
         CLI   TGCTEQU,CTYSOAP     MUST USE SCAST FOR SOAP COMMLS               
         BE    ERRRECTY                                                         
         TM    TGMEEQU,PRINT       MUST USE PCAST FOR PRINT COMMLS              
         BZ    LVK50                                                            
         OI    TRNSTAT,OKINTPFK    INTERNAL PFKEY                               
         MVI   PFAID,21            TO GO TO PCAST/LIST                          
         GOTO1 INITIAL,DMCB,ALPFTAB                                             
         DC    H'0'                SHOULD NOT RETURN                            
*                                                                               
LVK50    GOTO1 CHAROUT,DMCB,TACMELQ,SCLCOMMH,TACMTYPG  BOTTOM COMMENTS          
         BAS   RE,PROCVERS         PROCESS VERSION                              
         BAS   RE,GETCOMM          GET OTHER COMMERCIAL INFO                    
         BAS   RE,SAVTRKS          SAVE COMMERCIAL TRACKS                       
*                                                                               
*                                  READ CLIENT RECORD FOR STA2                  
         MVC   AIO,AIO2                                                         
         MVI   SVCISTA2,0                                                       
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A4',CMCLCLI)                              
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ                                                   
         USING TACID,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   LVK51                                                            
         MVC   SVCISTA2,TACISTA2                                                
LVK51    MVC   AIO,AIO1                                                         
*                                                                               
         USING TACOD,R4                                                         
         CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED                           
         BNE   LVK53                                                            
         LA    R4,COELEM                                                        
         OC    TACOVDTE,TACOVDTE   AND COMMERCIAL HAS A VERIFICATION            
         BZ    LVK53               DATE                                         
         LA    R2,SCLOPTH                                                       
         ZIC   RE,5(R2)                                                         
         AHI   RE,8                                                             
         AR    RE,R2                                                            
         MVC   0(4,RE),=C'EX=N'    INSERT EX=N INTO OPTIONS FIELD               
         ZIC   RE,5(R2)                                                         
         AHI   RE,4                                                             
         STC   RE,5(R2)            AND RESET FIELD'S LENGTH                     
         DROP  R4                                                               
*                                                                               
LVK53    OI    SCLOPTH+6,X'80'                                                  
*                                                                               
         LA    R2,SCLVERH          VALIDATE VERSION FIELD                       
         MVI   VERCODE,0                                                        
         TM    SCLVERH+1,X'20'     IF FIELD NOT PROTECTED                       
         BO    LVK60                                                            
         CLI   5(R2),0                                                          
         BE    LVK58                                                            
         BRAS  RE,MYANY                                                         
         BRAS  RE,VNUMVER          VALIDATE NUMERIC VERSION                     
*                                                                               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ      CHECK AGAINST VERSIONS ON COMML              
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LVK55    BAS   RE,NEXTEL                                                        
         BNE   ERRINV                                                           
         USING TAVRD,R4                                                         
         CLI   5(R2),3             IF 3 CHARACTER OR LESS INPUT                 
         BH    *+14                                                             
         CLC   TAVRVERS,WORK       ASSUME VERSION CODE                          
         B     *+10                                                             
         CLC   TAVRCID,WORK        ELSE, CID                                    
         BNE   LVK55                                                            
         MVC   VERCODE,TAVRVERS                                                 
LVK58    OI    4(R2),X'20'                                                      
*                                                                               
         USING TLCOD,R4                                                         
         L     R4,AIO              IF VERSION WAS NOT ON MAIN                   
         CLI   TLCOVER,TLCOV026    COMMERCIAL RECORD, RESTORE                   
         BE    LVK60               MAIN COMMERCIAL IN AIO                       
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)                                   
         BE    LVK60                                                            
         DC    H'00'                                                            
         DROP  R4                                                               
*                                                                               
LVK60    BRAS  RE,VALOPTS          VALIDATE OPTIONS FIELD                       
*                                                                               
         CLI   CONWHENH+5,0        IF PRINT FIELD HAS SOMETHING IN IT           
         BE    *+8                                                              
         NI    WHENOK,X'FE'        CLEAR BIT THAT PREVENTS PRINTREP             
LVKX     B     XIT                                                              
         SPACE 2                                                                
*              IF COMML IS A VERSION, SHOWS MAIN COMMERCIAL ID                  
         SPACE 1                                                                
PROCVERS NTR1                                                                   
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TACOCID,TGCID       IF INPUT CID DOESN'T MATCH RECORD            
         BE    XIT                                                              
         MVC   SCLCID,TACOCID      REPLACE VERSION CID W/MAIN COMM'L'S          
         MVC   TGCID,TACOCID       SET TGCID                                    
         OI    SCLCIDH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
* LIST SCREEN VERIFY RECORD (THIS SUBROUTINE IS CALLED WHEN THE CAST            
* VERIFY SCREEN CALLS THE CAST LIST SCREEN WITH THE ACTION 'VERIFY').           
*                                                                               
LVERIFY  NTR1                                                                   
         XC    NAMESH,NAMESH       CLEAR NAMES FIELD                            
         MVI   NAMESH,8+16         SET LENGTH OF FAKE NAMES FIELD               
         MVI   DISAPPR,C'N'        SET RECORD HAS NOT DISAPPEARED               
*                                                                               
         CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED                           
         BNE   LVF10                                                            
*                                  BUILD DISK ADDRESS LIST                      
         GOTO1 ABLDLIST,DMCB,1,(RC)                                             
         OC    NUMITEMS,NUMITEMS   IF NO CAST MEBERS                            
         BNZ   LVF20                                                            
         OI    CVERSTA2,CVERSNCA   SET NO CAST TO DISPLAY                       
         B     NO                  THEN EXIT                                    
*                                                                               
LVF10    L     R2,ITEM             ELSE BUMP TO NEXT PAGE                       
         A     R2,=A(NUMSCR)                                                    
         ST    R2,ITEM                                                          
*                                                                               
LVF20    BAS   RE,DISPLIST         DISPLAY LIST                                 
*                                                                               
**NO-OP  OC    NUMITEMS,NUMITEMS   IF NO CAST MEBERS THEN ERROR                 
**NO-OP  BZ    ERRNOCA                                                          
*                                                                               
         L     RF,ITEM             IF NO MORE ITEMS TO DISPLAY                  
         A     RF,=A(NUMSCR)                                                    
         C     RF,NUMITEMS                                                      
         BNL   NO                  THEN RETURN 'NO'                             
*                                                                               
         B     YES                 ELSE RETURN 'YES'                            
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
         MVI   HLDCHANG,0                                                       
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
         CLI   RECCHANG,C'N'       IF NO RECORD HAS CHANGED                     
         BNE   LVR90                                                            
         TM    TRNSTAT,RETURNED    THEN IF WE HAVEN'T JUST POPPED               
         BO    *+8                                                              
         BRAS  RE,TESTPFKS         TEST DIRECTIONAL PFKEYS                      
         BAS   RE,DISPLIST         DISPLAY LIST                                 
         B     LVR100                                                           
*                                                                               
LVR90    CLI   DISAPPR,C'Y'        ELSE IF SOME RECORD HAS DISAPPEARED          
         BNE   LVR100                                                           
         BAS   RE,DISPLIST         THEN RE-DISPLAY LIST                         
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
         BRAS  RE,LSETADDR         THEN BUMP R2 TO SECOND SSN FIELD             
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
         L     RF,=A(MYSPECS-T7021A)                                            
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
         BRAS  RE,LSETADDR         SET ADDRESSES OF SCREEN FIELDS               
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
LPR10    BRAS  RE,READREC          READ IN RECORD                               
         BNE   LPR50               IF RECORD DISAPPEARED THEN SKIP              
*                                                                               
         BAS   RE,DISPLINE         DISPLAY LINE TO SCREEN                       
*                                                                               
         LA    RF,P1               FILL FIRST PRINT LINE FROM SCREEN            
         USING PLINED,RF                                                        
         MVC   PLSSN,SCLSSN                                                     
         MVC   PLCAT,SCLCAT                                                     
         MVC   PLONOF,SCLONOF                                                   
         MVC   PLUNIT,SCLUNIT                                                   
         MVC   PLNCDE,SCLNCDE                                                   
         MVC   PLUNLO,SCLUNLO                                                   
         MVC   PLYEAR,SCLYEAR                                                   
         MVC   PLLIFT,SCLLIFT                                                   
         MVC   PLCORP,SCLCORP                                                   
         MVC   PLOP,SCLOP                                                       
         MVC   PLGUAR,SCLGUAR                                                   
         MVC   PLDBL,SCLDBL                                                     
         MVC   PLFRST,SCLFRST                                                   
         MVC   PLTRKS,SCLTRKS                                                   
         DROP  RF                                                               
*                                  FILL SECOND PRINT LINE FROM NAME FLD         
         MVC   P2(L'SCLNAME),SCLNAME                                            
*                                                                               
         GOTO1 CATCHIOS            PRINT BOTH PRINT LINES                       
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
LPR50    LA    R3,4(R3)            BUMP R3 TO NEXT DISK ADDRESS                 
*                                                                               
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
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
         XC    CONSERV,CONSERV     AUTO CALL $DQU                               
         MVC   CONSERV(4),=C'$DQU'                                              
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
LPRX     B     XIT                                                              
         EJECT                                                                  
* THIS IS THE HOOK ROUTINE FOR THE SPOOLED CAST LIST                            
*                                                                               
HOOK     NTR1                                                                   
         LA    RF,HEAD3            FILL HEAD3 FROM SCREEN                       
         USING HEAD3D,RF                                                        
         MVC   H3AGYT,SCLAGYT                                                   
         MVC   H3AGY,SCLAGY                                                     
         MVC   H3CIDT,SCLCIDT                                                   
         MVC   H3CID,SCLCID                                                     
         MVC   H3CIDN,SCLCIDN                                                   
         DROP  RF                                                               
*                                                                               
*                                  MOVE LIST TAG TO HEAD5 (SKIP SEL)            
         MVC   HEAD6(L'SCLTAG-4),SCLTAG+4                                       
*                                  UNDERLINE LIST TAG IN HEAD6                  
         MVC   HEAD7(67),=C'--------  --- --- --- ---- ------- --  - --X        
               - ---- ---- --- --------'                                        
*                                                                               
HKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY THE LIST OF RECORDS ON THE SCREEN.                                    
*                                                                               
DISPLIST NTR1                                                                   
         XC    VERTAB,VERTAB       CLEAR EXTENTION LIST TABLE                   
         L     R3,ITEM             R3 = A(TOP OF PAGE DISK ADDRESS)             
         SLL   R3,2                                                             
         LA    R3,DALIST(R3)                                                    
         LA    R2,SCLL1H           R2 = A(FIRST LINE ON SCREEN)                 
*                                                                               
DL10     BRAS  RE,LSETADDR         SET ADDRESSES OF FIELDS ON THIS LINE         
*                                                                               
         L     R2,ASELFLD          CLEAR SELECT FIELD                           
         XC    8(L'SCLL1,R2),8(R2)                                              
         OI    6(R2),X'80'                                                      
*                                                                               
DL20     OC    0(4,R3),0(R3)       IF THERE IS A RECORD TO DISPLAY              
         BZ    DL50                                                             
*                                                                               
         BRAS  RE,READREC          THEN READ IN RECORD                          
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
         BRAS  RE,CKEXT            CHECK IF EXTENSION SCREEN HAS                
*                                  INFO NOT ON LIST                             
         LA    R3,4(R3)            BUMP TO NEXT D/A IN LIST                     
         B     DL90                                                             
*                                                                               
DL50     BRAS  RE,CLRLINE          ELSE CLEAR LINE OUT FOR ADDING               
*                                                                               
DL90     L     R2,ANEXTLIN         R2 = A(NEXT DISPLAY LINE)                    
*                                                                               
         LA    R1,SCLCOMMH                                                      
         CR    R2,R1               REPEAT UNTIL END OF SCREEN                   
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
VL10     BRAS  RE,LSETADDR         SET ADDRESSES OF FIELDS ON THIS LINE         
         NI    HLDCHANG,X'FF'-ONTLINE                                           
*                                                                               
         BRAS  RE,VALISEL          TEST VALID DATA IN SELECT FIELD              
*                                                                               
         BAS   RE,TESTLINE         IF DISPLAY LINE EMPTY OR UNCHANGED           
         BE    VL20                                                             
*                                                                               
         L     R2,ARECFLD          THEN IF DISPLAY LINE IS EMPTY AND            
         CLI   8(R2),C' '              THERE WAS A RECORD THERE BEFORE          
         BH    VL90                    AND IT HASN'T DISAPPEARED                
         OC    0(4,R3),0(R3)                                                    
         BZ    VL90                                                             
         BRAS  RE,READREC                                                       
         BE    VL80                THEN REDISPLAY RECORD                        
*                                                                               
         B     VL90                ELSE SKIP DISPLAY LINE                       
*                                                                               
VL20     OC    0(4,R3),0(R3)       ELSE IF RECORD WAS THERE BEFORE              
         BZ    VL50                                                             
*                                                                               
         MVI   VALMODE,C'C'        SET VALIDATION MODE TO CHANGE                
*                                                                               
         BRAS  RE,READREC          READ OLD CAST RECORD                         
         BNE   VL90                SKIP THIS LINE IF REC DISAPPEARED            
*                                                                               
         MVC   AIO,AIO3            READ W4 RECORD INTO IO3                      
         L     R2,ASSNFLD          R2 = A(SSN FIELD)                            
         BRAS  RE,UNPKPID                                                       
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',8(R2))                                
         MVC   AIO,AIO1                                                         
         BNE   VL80                SKIP THIS LINE IF W4 REC DISAPPEARED         
         BRAS  RE,PKSSN            CONVERT SSN TO PID                           
*                                                                               
         GOTO1 TESTANN,DMCB,-1     TEST ANN STATUS AFTER DELETE OLD             
         GOTO1 TESTLIFT,DMCB,-1    TEST LIST STATUS AFTER DELETE OLD            
*                                                                               
         BAS   RE,VALILINE         VALIDATE AND BUILD RECORD                    
         BNE   VL80                SKIP IF CHANGES NOT ALLOWED                  
*                                                                               
         MVI   RDUPDATE,C'Y'       READ AND LOCK OLD CAST RECORD INTO           
         MVC   AIO,AIO2                VOID                                     
         BRAS  RE,READREC                                                       
         MVC   AIO,AIO1                                                         
         BNE   VL90                SKIP THIS LINE IF REC DISAPPEARED            
*                                                                               
         BAS   RE,WRITENEW         WRITE BACK NEW CAST RECORD                   
*                                                                               
         GOTO1 TESTANN,DMCB,1      TEST ANN STATUS AFTER ADDING NEW             
         GOTO1 TESTLIFT,DMCB,1     TEST LIFT STATUS AFTER ADDING NEW            
         B     VL80                                                             
*                                                                               
VL50     MVI   VALMODE,C'A'        ELSE SET VALIDATION MODE TO ADD              
*                                                                               
         TM    COELEM+TACOSTA2-TACOD,TACOPCYC                                   
         BO    ERRNOADD                                                         
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
         GOTO1 TESTLIFT,DMCB,1     TEST LIFT STATUS AFTER ADDING NEW            
*                                                                               
VL80     L     R2,ASSNFLD                                                       
         BRAS  RE,PKSSN            CONVERT SSN TO PID                           
         BAS   RE,DISPLINE         DISPLAY BACK RECORD                          
*                                                                               
         BRAS  RE,UPDMUS           UPDATE LINKED MUSICIANS                      
*                                                                               
VL90     LA    R3,4(R3)            BUMP TO NEXT D/A IN LIST                     
*                                                                               
*                                  MAKE ALL FIELDS VALID                        
         GOTO1 FLDVAL,DMCB,(X'20',ASSNFLD),(X'80',ALASTUNP)                     
*                                                                               
         MVC   SVACAT,ACATFLD      SAVE A(FIELDS ABOVE)                         
         MVC   SVAONOF,AONOFFLD                                                 
         MVC   SVAUNIT,AUNITFLD                                                 
         MVC   SVAUNLO,AUNLOFLD                                                 
         MVC   SVAYEAR,AYEARFLD                                                 
*                                                                               
         L     R2,ANEXTLIN         R2 = A(NEXT DISPLAY LINE)                    
*                                                                               
         LA    R1,SCLCOMMH                                                      
         CR    R2,R1               REPEAT UNTIL END OF SCREEN                   
         BL    VL10                                                             
*                                                                               
         CLI   RECCHANG,C'Y'       IF ANY RECORD HAS CHANGED                    
         BNE   *+8                                                              
         BRAS  RE,UPDCOMM          THEN UPDATE COMMERCIAL STATUS                
         BRAS  RE,UPDHSTA                                                       
*                                                                               
VLX      B     XIT                                                              
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
         L     R2,ASSNFLD          CLEAR THIS LINE'S PROTECTED FLDS             
         L     R3,AWIDFLD                                                       
         TWAXC (R2),(R3),PROT=Y                                                 
*                                                                               
         L     R2,ARECFLD          CLEAR THIS LINE'S UNPROTECTED FLDS           
         L     R3,ALASTUNP                                                      
         TWAXC (R2),(R3)                                                        
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
         BAS   RE,DRLIFT                   LIFT FIELD                           
         BAS   RE,DRCORP                   CORPORATION FIELD                    
         BAS   RE,DROP                     OVERSCALE PERCENTAGE FIELD           
         BAS   RE,DRGUAR                   GUARANTEE FIELD                      
         BAS   RE,DRDBL                    DOUBLES FIELD                        
         BAS   RE,DRFRST                   FIRST SERVICES FIELD                 
         BAS   RE,DRTRKS                   TRACKS                               
         BAS   RE,DROTH                    OTHER DETAILS (OPTIONAL)             
         BRAS  RE,DRWID                    WEB APPLICATION ID                   
*                                                                               
DLIN5    CLI   ACTNUM,ACTLDEL      IF USER WANTS DELETED RECORDS                
         BNE   DLIN10                                                           
*                                  THEN MAKE ALL DATA FIELDS PROTECTED          
         GOTO1 FLDVAL,DMCB,(8,ARECFLD),ALASTUNP                                 
         B     DLIN30                                                           
                                                                                
DLIN10   GOTOR HASTRKAS,DMCB,AIO   IF MUSICIAN HAS TRACK ASSOCIATION            
         BNE   DLIN20              PROTECT ALL FIELDS EXCEPT CONT YEAR          
         GOTO1 FLDVAL,DMCB,(8,ASSNFLD),AUNLOFLD            AND DOUBLES          
         GOTO1 (RF),(R1),(8,ALIFTFLD),AGUARFLD                                  
         GOTO1 (RF),(R1),(8,AFRSTFLD),ATRKSFLD                                  
         B     DLIN30                                                           
*                                  ELSE MAKE ALL DATA FLDS UNPROTECTED          
DLIN20   GOTO1 FLDVAL,DMCB,(4,ARECFLD),ALASTUNP                                 
*                                                                               
*                                  SET ALL DATA FIELDS VALID                    
DLIN30   GOTO1 FLDVAL,DMCB,(X'20',ARECFLD),ALASTUNP                             
*                                                                               
DLINX    B     XIT                                                              
         EJECT                                                                  
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
         GOTOR TESTCHG,DMCB,ALASTUNP  DETERMINE IF ANYTHING CHANGED             
         BNE   TLNO                                                             
*                                                                               
TLYES    B     YES                 RETURN 'YES'                                 
*                                                                               
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
         CLI   ACCESSED,C'G'       IF CAN ONLY GUAR FIELD ON LIST               
         BNE   VLIN05                                                           
         L     R1,AGUARFLD         IF GUAR FIELD HASN'T CHANGED                 
         TM    4(R1),X'20'                                                      
         BO    NO                  THEN GET OUT NOW                             
*                                                                               
VLIN05   CLI   VALMODE,C'C'        IF VALIDATION MODE IS CHANGE                 
         BNE   VLIN10                                                           
*                                  SAVE POINTER BLOCK                           
         GOTO1 SAVPTRS,DMCB,PPBLOCK                                             
         BRAS  RE,TESTHLD                                                       
         B     VLIN20                                                           
*                                                                               
VLIN10   LA    R0,PPBLOCK          ELSE CLEAR POINTER BLOCK                     
         LHI   R1,L'PPBLOCK                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
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
         CLI   ACCESSED,C'G'       IF CAN ONLY GUAR FIELD ON LIST               
         BNE   VLIN60                                                           
         BAS   RE,VRGUAR           VALIDATE GUARANTEE FIELD                     
         B     VLIN70              AND SKIP THE REST                            
*                                                                               
VLIN60   BRAS  RE,VRHLD            MAKE SURE HOLDING FEE ELEM EXISTS            
*                                                                               
         BRAS  RE,DRWID            DISPLAY WEB APPLICATION ID                   
*                                                                               
         BAS   RE,VRCAT            VALIDATE CATEGORY FIELD                      
         BAS   RE,VRONOF                    CAMERA FIELD                        
         BRAS  RE,VRUNIT                    TAX UNIT FIELD                      
         BRAS  RE,VRNCDE                    AGENT FIELD                         
         BAS   RE,VRUNLO                    UNION/LOCAL FIELD                   
         BRAS  RE,VRYEAR                    YEAR FIELD                          
         BAS   RE,VRLIFT                    LIFT FIELD                          
         BAS   RE,VRCORP                    CORPORATION                         
         BAS   RE,VROP                      OVERSCALE PERCENTAGE FIELD          
         BAS   RE,VRGUAR                    GUARANTEE FIELD                     
         BAS   RE,VRDBL                     DOUBLES FIELD                       
         BAS   RE,VRFRST                    FIRST SERVICES FIELD                
         BRAS  RE,VRTRKS                    TRACKS FIELD                        
*                                                                               
*                                  ADD NEW CAST ELEMENT TO RECORD               
VLIN70   MVC   ELEM(TACALNQ),CAELEM                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         BRAS  RE,AUTOVER1         POSSIBLY AUTO-ADD TO VERSION 1               
         GOTO1 ACTVIN,DMCB,0       UPDATE LAST CHANGED ELEMENT                  
*                                                                               
         GOTO1 ABLDSORT,DMCB,(RC)  UPDATE SORT KEY                              
*                                                                               
VLINX    B     YES                                                              
         EJECT                                                                  
* WRITE BACK NEW CAST RECORD.  UPDATE PASSIVE POINTERS.                         
*                                                                               
WRITENEW NTR1                                                                   
         USING TLCAD,R4                                                         
         L     R4,AIO              R4=A(CAST RECORD)                            
         CLC   TLCAKEY,PPBLOCK     IF KEY HAS NOT CHANGED                       
         BNE   WNEW10                                                           
         GOTO1 PUTREC              WRITE BACK CAST RECORD                       
*                                                                               
         USING TLDRD,R2                                                         
         LA    R2,PPBLOCK                                                       
         MVI   BYTE,X'88'          PROCESS ACTIVE POINTER                       
         CLC   TLCASTAT,TLDRSTAT   IF THE STATUS CHANGED                        
         BE    *+8                                                              
         MVI   BYTE,X'A8'          FORCE WRITE NEW PTRS                         
         GOTO1 ADDPTRS,DMCB,(BYTE,PPBLOCK),UPBLOCK                              
         MVC   0(4,R3),DMDSKADD    UPDATE DISK ADDRESS OF RECORD                
         B     XIT                                                              
         DROP  R2,R4                                                            
*                                                                               
WNEW10   XC    KEY,KEY             IF KEY HAS CHANGED, REREAD ORIGINAL          
         MVC   KEY(L'TLCAKEY),PPBLOCK                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING TLRCD,R2                                                         
         L     R2,AIO2                                                          
         MVC   WORK(L'TLTMKEY),0(R2)                                            
         OI    TLRCSTAT,X'80'      DELETE ORIGINAL AND ITS POINTERS             
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'E8',PPBLOCK),UPBLOCK                             
         DROP  R2                                                               
*                                                                               
         USING TLTMD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY             READ ALL TIMESHEET KEYS FOR                  
         MVI   TLTMCD,TLTMCDQ      THE COMMERCIAL                               
         MVC   TLTMCOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         B     WNEW30                                                           
WNEW20   GOTO1 SEQ                                                              
WNEW30   CLC   KEY(TLTMINV-TLTMD),KEYSAVE                                       
         BNE   WNEW40                                                           
*                                                                               
         USING TLCAD,R1                                                         
         LA    R1,WORK                                                          
         CLC   TLTMSORT,TLCASORT   IF WE FIND A TIMESHEET THAT MATCHES          
         BNE   WNEW20              THE ORIGIANL SORT KEY                        
         CLC   TLTMCAT,TLCACAT     AND ORIGINAL CATEGORY                        
         BNE   WNEW20                                                           
         MVC   TGINV,TLTMINV       SAVE THE INVOICE NUMBER                      
         DROP  R1,R4                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'       READ THE TIMESHEET RECORD FOR                
         GOTO1 GETREC              UPDATE                                       
*                                                                               
         USING TLDRD,R4                                                         
         OI    TLDRSTAT,X'80'      MARK THE KEY DELETED                         
         GOTO1 WRITE                                                            
         DROP  R4                                                               
*                                                                               
         USING TLTMD,R2                                                         
         OI    TLTMSTAT,X'80'                                                   
         GOTO1 PUTREC                                                           
         DROP  R2                                                               
*                                                                               
         USING TLTMD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLTMCD,TLTMCDQ      IF THERE ARE NO TIMESHEETS                   
         MVC   TLTMCOM,TGCOM       LEFT FOR THE INVOICE                         
         MVC   TLTMINV,TGINV                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLTMSSN-TLTMD),KEYSAVE                                       
         BE    WNEW40                                                           
         DROP  R4                                                               
*                                                                               
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B4',0)                                    
         BNE   WNEW40                                                           
*                                                                               
         USING TAIND,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAINELQ      DELETE TIMESHEET COMMERCIAL                  
         BAS   RE,GETEL            FROM THE INVOICE                             
         BE    *+6                                                              
         DC    H'00'                                                            
         XC    TAINTMCO,TAINTMCO                                                
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
*                                                                               
WNEW40   MVC   AIO,AIO1            RESTORE AIO TO UPDATED CAST                  
         LA    R0,PPBLOCK          AND CLEAR POINTER POINTERS                   
         LHI   R1,L'PPBLOCK                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING TLDRD,R4                                                         
         L     R2,AIO1                                                          
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TLDRKEY,0(R2)       MOVE RECORD KEY TO DIRECTORY KEY             
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                LOOK FOR RECORD ALREADY ON FILE              
*                                                                               
         CLC   TLDRKEY,KEYSAVE     IF RECORD EXISTS                             
         BNE   WNEW50                                                           
         TM    TLDRSTAT,X'80'      IT HAD BETTER BE DELETED                     
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    TLDRSTAT,X'3F'      TURN OFF CAST DELETED BITS                   
         GOTO1 WRITE               AND WRITE IT BACK                            
*                                                                               
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         MVC   AIO,AIO2            SET ALT. I/O AREA                            
         GOTO1 GETREC              GET THE RECORD SO THAT WE CAN ...            
         ST    R2,AIO                                                           
         GOTO1 PUTREC              WRITE NEW ONE BACK OVER DELETED ONE          
         B     WNEW60                                                           
*                                                                               
WNEW50   MVC   TLDRKEY,KEYSAVE                                                  
         GOTO1 ADDREC              OK TO ADD THE RECORD                         
*                                                                               
WNEW60   GOTO1 ADDPTRS,DMCB,(X'28',PPBLOCK),UPBLOCK                             
         MVC   0(4,R3),DMDSKADD    UPDATE DISK ADDRESS OF RECORD                
         NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         DROP  R4                                                               
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
         USING TLFTD,R4                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY              IF CAST MEMBER ALREADY HAS FTRACK            
         MVI   TLFTCD,TLFTCDQ                                                   
         MVC   TLFTSSN,TGSSN                                                    
         MVC   TLFTCOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLFTCAST-TLFTD),KEYSAVE                                      
         JNE   AN20                                                             
*                                                                               
         L     R2,ASSNFLD          THEN IF SSN FIELD IS NOT HIGHLIGHTED         
         TM    1(R2),X'0C'                                                      
         JNZ   AN10                                                             
         OI    1(R2),X'08'         THEN HIGHLIGHT SSN FIELD                     
         OI    6(R2),X'80'                                                      
         J     ERRFTRK             AND TELL USER TO HIT PF19                    
*                                                                               
AN10     CLI   PFAID,19            ELSE IF PFKEY IS NOT 19                      
         JNE   ERRFTRK             THEN TELL USER AGAIN                         
*                                                                               
         XC    ELEM,ELEM           BUILD CAST ERROR ELEMENT                     
         LA    R4,ELEM                                                          
         USING TACED,R4                                                         
         MVI   TACEEL,TACEELQ                                                   
         MVI   TACELEN,TACELNQ+1                                                
         MVI   TACENUM,1           HAVE ONE ERROR                               
         MVC   TACEDATE,TGTODAY1                                                
         TIME  DEC                                                              
         STCM  R0,14,TACETIME                                                   
         MVI   TACEERR,TACEEFTK    SET ERROR CODE                               
         GOTO1 ADDELEM             ADD ELEMENT                                  
*                                                                               
AN20     MVC   AIO,AIO2                                                         
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
         OI    HLDCHANG,ONSCREEN                                                
         DROP  R4                                                               
*                                                                               
         MVC   0(4,R3),DMDSKADD    MOVE DISK ADDRESS TO LIST                    
*                                                                               
         L     RF,NUMITEMS         INCREMENT NUMBER OF ITEMS                    
         LA    RF,1(RF)                                                         
         ST    RF,NUMITEMS                                                      
*                                  ADD NEW POINTERS                             
         GOTO1 ADDPTRS,DMCB,(8,PPBLOCK),UPBLOCK                                 
         B     XIT                                                              
         EJECT                                                                  
EDRIVER  DS    0H                                                               
*                                  INITIALIZE OVERLAY                           
         CLI   ACTNUM,ACTVER       IF COMING FROM VERIFY                        
         BNE   ED1                 DON'T CLEAR STORAGE/SET PF TABLE             
         GOTO1 INITIAL,DMCB,(X'40',0)                                           
         B     ED2                                                              
*                                                                               
ED1      GOTO1 INITIAL,DMCB,AEPFTAB                                             
*                                                                               
         CLI   RECNUM,FT           IF CALLED FROM FTRACK LIST                   
         BNE   ED2                                                              
         CLI   THISLSEL,C'D'       AND SELECT CODE IS D                         
         BNE   ED2                                                              
         MVC   CONACT,=CL8'DELETE' MOVE DELETE INTO ACTION FIELD                
         OI    CONACTH+6,X'80'                                                  
         LA    R2,CONACTH          SET CURSOR                                   
         B     DELLSTER            GIVE ERROR                                   
*                                                                               
ED2      BAS   RE,ANLFMOD          TEST MODE EFFECTS ANNOUNCER/LIFT             
*                                                                               
         BAS   RE,EPOINTER         SAVE OR ADD POINTERS                         
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
ED20     BRAS  RE,UPDCOMM          THEN UPDATE COMMERCIAL STATUS                
         BRAS  RE,UPDHSTA                                                       
         BRAS  RE,UPDMUS           UPDATE LINKED MUSICIANS                      
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
         DROP  R3                                                               
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
         BRAS  RE,TESTHLD                                                       
         B     EPX                                                              
*                                                                               
EP20     CLI   MODE,XRECPUT        ELSE IF MODE IS XRECPUT                      
         BNE   EP40                                                             
*                                  THEN UPDATE PASSIVE POINTER(S)               
         MVI   BYTE,X'88'          PROCESS ACTIVE POINTER                       
         USING TLDRD,R2                                                         
         LA    R2,PPBLOCK                                                       
         USING TLCAD,R4                                                         
         L     R4,AIO                                                           
         CLC   TLCASTAT,TLDRSTAT   IF THE STATUS CHANGED                        
         BE    *+8                                                              
         MVI   BYTE,X'A8'          FORCE WRITE NEW PTRS                         
         GOTO1 ADDPTRS,DMCB,(BYTE,PPBLOCK),UPBLOCK                              
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
EP50     GOTO1 ADDPTRS,DMCB,(8,PPBLOCK),UPBLOCK                                 
*                                                                               
EP60     DS    0H                                                               
*                                                                               
EPX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* THIS ROUTINE TESTS WHETHER THE MODE IS ONE THAT WILL EFFECT THE               
* ANNOUNCER-ONLY-NON-MUSICIAN STATUS, AND IF IT IT, IT WILL CALL                
* TESTANN WITH THE PROPER PARAMETER FOR THE MODE.                               
* IT ALSO UPDATES THE NUMBER OF CAST MEMBERS ON THE LIFT.                       
*                                                                               
ANLFMOD  NTR1                                                                   
         CLI   MODE,XRECDEL        IF JUST DELETED A RECORD                     
         BE    AM10                                                             
         CLI   MODE,VALREC         OR IF ABOUT TO CHANGE A RECORD               
         BNE   AM20                                                             
*                                                                               
AM10     GOTO1 TESTANN,DMCB,-1     THEN TEST STATUS                             
         GOTO1 TESTLIFT,DMCB,-1                                                 
         B     AMX                                                              
*                                                                               
AM20     CLI   MODE,XRECREST       IF JUST RESTORED A RECORD                    
         BE    AM30                                                             
         CLI   MODE,XRECPUT        OR IF JUST CHANGED A RECORD                  
         BNE   AMX                                                              
*                                                                               
AM30     GOTO1 TESTANN,DMCB,1      THEN TEST STATUS                             
         GOTO1 TESTLIFT,DMCB,1                                                  
*                                                                               
AMX      B     XIT                                                              
         EJECT                                                                  
* SET ADDRESSES OF FIELDS ON EXTENSION SCREEN.                                  
*                                                                               
ESETADDR NTR1                                                                   
         LA    R2,SCACATH          SAVE A(CATEGORY FIELD)                       
         ST    R2,ACATFLD                                                       
         ST    R2,ARECFLD          SAVE A(FIRST RECORD FIELD)                   
         LA    R2,SCALAGYH         SAVE A(LIFTED FROM AGENCY FIELD)             
         ST    R2,ALFTAFLD                                                      
         LA    R2,SCALIFTH         SAVE A(LIFTED FROM ID FIELD)                 
         ST    R2,ALFTFFLD                                                      
         LA    R2,SCAONOFH         SAVE A(CAMERA FIELD)                         
         ST    R2,AONOFFLD                                                      
         LA    R2,SCAUNITH         SAVE A(TAX UNIT FIELD)                       
         ST    R2,AUNITFLD                                                      
         LA    R2,SCAUNLOH         SAVE A(UNION/LOCAL FIELD)                    
         ST    R2,AUNLOFLD                                                      
         LA    R2,SCAYEARH         SAVE A(YEAR FIELD)                           
         ST    R2,AYEARFLD                                                      
         LA    R2,SCAFCYCH         SAVE A(FIRST CYCLE FIELD)                    
         ST    R2,AFCYCFLD                                                      
         LA    R2,SCAFRSTH         SAVE A(FIRST SERVICES FIELD)                 
         ST    R2,AFRSTFLD                                                      
         LA    R2,SCALSTH          SAVE A(LAST SERVICES FIELD)                  
         ST    R2,ALSTFLD                                                       
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
         LA    R2,SCAGUADH         SAVE A(GUARANTEE DATES FIELD)                
         ST    R2,AGUADFLD                                                      
         LA    R2,SCARERH          SAVE A(RERECORD DATE FIELD)                  
         ST    R2,ARERFLD                                                       
         LA    R2,SCAEXPH          SAVE A(EXPIRATION DATE FIELD)                
         ST    R2,AEXPFLD                                                       
         LA    R2,SCASTATH         SAVE A(STATUS FIELD)                         
         ST    R2,ASTATFLD                                                      
         LA    R2,SCADBLH          SAVE A(DOUBLES FIELD)                        
         ST    R2,ADBLFLD                                                       
         LA    R2,SCAOPH           SAVE A(OVERSCALE PERCENTAGE FIELD)           
         ST    R2,AOPFLD                                                        
         LA    R2,SCAOV2H          SAVE A(2ND OVERSCALE PERC FIELD)             
         ST    R2,AOV2FLD                                                       
         LA    R2,SCAOAH           SAVE A(OVERSCALE AMOUNTS FIELD)              
         ST    R2,AOAFLD                                                        
         LA    R2,SCATRKSH         SAVE A(TRACKS FIELD)                         
         ST    R2,ATRKSFLD                                                      
         LA    R2,SCACMH           SAVE A(COMMENTS FIELD)                       
         ST    R2,ACMFLD                                                        
         LA    R2,SCAROLEH         SAVE A(ROLE DESCRIPTION FIELD)               
         ST    R2,AROLEFLD                                                      
         LA    R2,SCARLSDH         SAVE A(RELEASED STATUS FIELD)                
         ST    R2,ARLSDFLD                                                      
         LA    R2,SCARLDTH         SAVE A(RELEASED DATE FIELD)                  
         ST    R2,ARLDTFLD                                                      
         LA    R2,SCARLSPH         SAVE A(PREV REL STATUS FIELD)                
         ST    R2,ARLSPFLD                                                      
         LA    R2,SCARLDPH         SAVE A(PREV REL DATE FIELD)                  
         ST    R2,ARLDPFLD                                                      
         LA    R2,SCAEFDTH         SAVE A(REL EFFECTIVE DATE FIELD)             
         ST    R2,ARLEFDTD                                                      
         LA    R2,SCAWIDH          SAVE A(WEB APPLICATION ID FIELD)             
         ST    R2,AWIDFLD                                                       
         LA    R2,SCAEQTYH         SAVE A(EQUITY)                               
         ST    R2,AEQTYFLD                                                      
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
         MVC   SCASSN,TLCASSN      DISPLAY SSN                                  
*                                                                               
         GOTO1 HEXOUT,DMCB,TLCASORT,SCASORT+1,6,0 DISPLAY CAST SORT KEY         
         OI    SCASORTH+6,X'80'                                                 
         CLI   TGCTSTTY,TASTTYPP   IF CONNECTED STAFF IS A PROGRAMMER           
         BNE   *+8                                                              
         NI    SCASORTH+1,X'FB'    SET TO HIGH INTENSITY (ELSE ZERO)            
*                                                                               
         CLI   THISLSEL,C'V'       IF WANT TO GO TO VERSIONS SCREEN             
         BNE   ESCRK2                                                           
         LA    R2,SCASSNH          CHANGE SSN TO PID NOW                        
         MVC   TGSSN,TLCASSN                                                    
         BRAS  RE,PKSSN                                                         
         MVC   SCACAT,TLCACAT      AND MOVE CATEGORY TO SCREEN                  
*                                                                               
ESCRK2   MVC   AIO,AIO2            READ COMMERCIAL RECORD AND DISP NAME         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A8',TLCACOM),SCACIDNH                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   COMSTAT,0                                                        
*                                                                               
         L     R4,AIO              R4=A(COMM'L RECORD)                          
         USING TLCOD,R4                                                         
         MVC   SCAAGY,TLCOAGY      DISPLAY AGENCY                               
         MVC   TGAGY,TLCOAGY       SAVE IN GLOBAL TOO                           
         BAS   RE,GETCOMM          GET COMMERCIAL INFO FROM RECORD              
*                                                                               
         LA    R4,COELEM           R4 = A(COMMERCIAL DETAILS ELEMENT)           
         USING TACOD,R4                                                         
*                                                                               
         MVC   SCACID,TACOCID      DISPLAY COMMERCIAL ID                        
         MVC   TGCID,TACOCID       SAVE IN GLOBAL                               
         OI    SCACIDH+6,X'80'                                                  
*                                                                               
         CLI   THISLSEL,C'V'       IF WANT TO GO TO VERSIONS SCREEN             
         BNE   ESCRK3                                                           
         MVI   PFAID,18                                                         
         GOTO1 INITIAL,DMCB,AEPFTAB                                             
*                                                                               
ESCRK3   BAS   RE,SAVTRKS          SAVE COMMERCIAL TRACKS                       
*                                                                               
         MVC   AIO,AIO3            DISPLAY W4 NAME                              
                                                                                
         BAS   RE,SPANVER          SEE IF COMMERCIAL HAS SPAN VERSIONS          
         DROP  R4                                                               
*                                                                               
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',SCASSN),SCASSNNH                      
         GOTO1 SSNPACK,DMCB,SCASSN,TGPID                                        
         MVC   SCASSN,SSNSPACE                                                  
         MVC   SCASSN(L'TGPID),TGPID                                            
         MVI   SCASSNH+5,6                                                      
         OI    SCASSNH+6,X'80'                                                  
*                                                                               
ESCRK20  MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         CLI   ERROR,0             IF W4 RECORD NOT FOUND                       
         BE    *+10                THEN INDICATE THAT IN NAME FIELD             
         MVC   SCASSNN(16),=C'** NOT FOUND ** '                                 
*                                                                               
         L     RF,AIO              RESTORE KEY FROM RECORD                      
         MVC   KEY(L'TLDRKEY),0(RF)                                             
*                                                                               
ESKX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE OFF TRACKS FROM COMMERCIAL                       
*                                                                               
SAVTRKS  NTR1                                                                   
         XC    TRACKS,TRACKS                                                    
                                                                                
         USING TAMCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAMCELQ                                                   
         BAS   RE,GETEL            R4=A(MUSIC CONTRACT DETAILS)                 
         BNE   XIT                                                              
                                                                                
         LA    R3,TRACKS                                                        
                                                                                
ST10     CLI   TAMCTRK,0                                                        
         BE    ST20                                                             
         MVC   0(L'TAMCTRK,R3),TAMCTRK                                          
         LA    R3,1(R3)                                                         
         DROP  R4                                                               
                                                                                
ST20     BAS   RE,NEXTEL                                                        
         BE    ST10                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS STATUS IF COMMERCIAL HAS SPANISH VERSIONS       *         
*        ON ENTRY ... R4=A(COMMERCIAL DETAILS ELEMENT)                *         
***********************************************************************         
                                                                                
         USING TACOD,R4                                                         
SPANVER  NTR1                                                                   
         CLI   TACOTYPE,CTYSPAN    IF MASTER COMMERCIAL TYPE                    
         BE    XIT                 IS NOT SPANISH                               
         DROP  R4                                                               
                                                                                
         USING TLVRD,R3                                                         
         LA    R3,KEY                                                           
         XC    TLVRKEY,TLVRKEY     READ ALL ATTACHED VERSION                    
         MVI   TLVRCD,TLVRCDQ      KEYS/RECORDS                                 
         MVC   TLVRCOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         B     SV20                                                             
SV10     GOTO1 SEQ                                                              
SV20     CLC   KEY(TLVRVER-TLVRD),KEYSAVE                                       
         JNE   XIT                                                              
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   SV10                                                             
         CLI   TACOTYPE,CTYSPAN    IF ANY ARE SPANISH                           
         JNE   SV10                                                             
         OI    COMSTAT,CSSPANVR    SET SPANISH VERSION STATUS                   
         J     XIT                                                              
         DROP  R4                                                               
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
         GOTO1 FLDVAL,DMCB,(X'0A',SCAGRRHH),(8,SCAGRRFH)                        
         MVI   SCAGRRFH+5,0                                                     
*                                                                               
         XC    SCACORN,SCACORN     CLEAR NAME FLDS                              
         OI    SCACORNH+6,X'80'                                                 
         XC    SCANCDN,SCANCDN                                                  
         OI    SCANCDNH+6,X'80'                                                 
         XC    SCALCLN,SCALCLN                                                  
         OI    SCALCLNH+6,X'80'                                                 
*                                                                               
         XC    SCAGUAD,SCAGUAD     CLEAR GUARANTEE DATES FIELD                  
         OI    SCAGUADH+6,X'80'                                                 
*                                                                               
         XC    SCAHLD,SCAHLD       CLEAR HOLDING FEE CREDITS FIELD              
         OI    SCAHLDH+6,X'80'                                                  
*                                                                               
         OI    SCAHFNH+1,X'0C'     MAKE HLD FEE NOTIF ZERO INTENSITY            
         OI    SCAHFNH+6,X'80'                                                  
         OI    SCA2NDH+1,X'0C'                                                  
         OI    SCA2NDH+6,X'80'                                                  
*                                                                               
         TM    COELEM+TACOSTA2-TACOD,TACOPCYC                                   
         BZ    EDR01                                                            
         OI    SCAFCYCH+1,X'20'       FOR PER CYCLE COMMERCIALS,                
         OI    SCAFCYCH+6,X'80'       PROTECT FIRST FIXED CYCLE                 
*                                                                               
EDR01    L     R4,AIO              SET A(CAST ELEMENT)                          
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
         BAS   RE,DRLFTF                   LIFTED FROM FIELD                    
         BAS   RE,DRONOF                   CAMERA FIELD                         
         BAS   RE,DRUNIT                   TAX UNIT FIELD                       
         BAS   RE,DRUNLO                   UNION/LOCAL FIELD                    
         BAS   RE,DRYEAR                   YEAR FIELD                           
         BAS   RE,DRFCYC                   FIRST CYCLE FIELD                    
         BAS   RE,DRFRST                   FIRST SERVICES FIELD                 
         BAS   RE,DRLST                    LAST SERVICES FIELD                  
         BAS   RE,DRCORP                   CORPORATION FIELD                    
         BAS   RE,DRNCDE                   AGENT FIELD                          
         BAS   RE,DRGUAR                   GUARANTEE FIELD                      
         BAS   RE,DRRER                    RERECORD DATE FIELD                  
         BAS   RE,DREXP                    EXPIRATION DATE FIELD                
         BAS   RE,DRSTAT                   STATUS FIELD                         
         BAS   RE,DROP                     OVERSCALE PERCENTAGE FIELD           
         BAS   RE,DROV2                    2ND OVERSCALE PERC FIELD             
         BAS   RE,DRDBL                    DOUBLES FIELD                        
         BAS   RE,DROA                     OVERSCALE AMOUNTS FIELD              
         BAS   RE,DRTRKS                   TRACKS FIELD                         
         BAS   RE,DRCM                     COMMENTS FIELD                       
         BRAS  RE,DRROLE                   ROLE DESCRIPTION                     
*                                                                               
         BRAS  RE,DRRLSD                   RELEASED STATUS                      
*                                                                               
*                                  DISPLAY LAST CHANGED ELEMENT                 
         GOTO1 ACTVOUT,DMCB,SCALCHGH                                            
         BRAS  RE,DISWID                                                        
*                                  PROTECT ALL KEY FIELDS                       
         GOTO1 FLDVAL,DMCB,(X'08',SCAAGYH),SCASSNH                              
*                                                                               
*                                  SET ALL RECORD DATA FIELDS VALID             
         GOTO1 FLDVAL,DMCB,(X'20',ARECFLD),(X'80',999)                          
*                                                                               
         XR    R0,R0               USE R0 AS STATUS FOR WHETHER                 
         GOTOR HASTRKAS,DMCB,AIO1  CAST MEMBER IS LINKED TO AN                  
         BNE   *+8                 AFM CONTRACT                                 
         LHI   R0,1                                                             
                                                                                
         L     R2,ACATFLD                                                       
EDR05    TM    1(R2),X'02'         ALL FIELDS WITH EXTENDED HEADERS             
         BZ    EDR07                                                            
         C     R2,AYEARFLD         EXCEPT CONTRACT YEAR                         
         BE    EDR07                                                            
         C     R2,ADBLFLD          AND DOUBLES                                  
         BE    EDR07                                                            
         LTR   R0,R0               GET PROTECTED/UNPROTECTED BASED              
         BNZ   EDR06               ON CAST MEMBER LINKED TO AN AFM              
         NI    1(R2),X'DF'         CONTRACT STATUS                              
         B     EDR07                                                            
EDR06    OI    1(R2),X'20'                                                      
EDR07    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         C     R2,AROLEFLD                                                      
         BNH   EDR05                                                            
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
         MVI   HLDCHANG,0                                                       
         L     R2,ARECFLD                                                       
         GOTOR TESTCHG,DMCB,999    DETERMINE IF ANYTHING CHANGED                
*                                                                               
         CLC   SCAWID,=CL10'VIDA'                                               
         JE    EVALR0                                                           
         CLC   SCAWID,=CL10'ATIV'                                               
         JNE   *+8                                                              
EVALR0   BRAS  RE,DISWID                                                        
         BRAS  RE,VALWID           VALIDATE WEB APPLICATION ID                  
*                                                                               
         CLI   ACCESSED,C'G'       IF ONLY ACCESSED PARTIALLY                   
         BNE   EVALR10                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         USING TACAD,R4                                                         
         BAS   RE,GETEL            IF NO CAST DETAILS ELEMENT                   
         BE    *+12                                                             
         BAS   RE,INITCAEL         INIT CAELEM & RETURN R4=A(CAELEM)            
         B     EVALR5                                                           
         XC    CAELEM,CAELEM       ELSE SET TACAD TO CAELEM                     
         ZIC   RF,TACALEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CAELEM(0),0(R4)                                                  
         MVI   CAELEM+1,TACALNQ                                                 
         GOTO1 REMELEM                                                          
         LA    R4,CAELEM                                                        
EVALR5   ST    R4,ACAELEM          SAVE A(NEW CAST DETAILS ELEMENT)             
         BAS   RE,VRGUAR           VALIDATE GUARANTEE FIELD                     
         BAS   RE,VRROLE           VALIDATE ROLE DESCRIPTION FIELD              
         B     EVALR50                                                          
*                                                                               
EVALR10  MVI   ELCODE,TACAELQ      REMOVE OLD CAST ELEMENT                      
         GOTO1 REMELEM                                                          
*                                                                               
         BAS   RE,INITCAEL         INIT CAELEM & RETURN R4=A(CAELEM)            
         ST    R4,ACAELEM          SAVE A(CAST ELEMENT)                         
*                                                                               
         MVC   AIO,AIO3            READ W4 RECORD INTO IO3 FOR VRCORP           
         CLI   SCASSNH+5,6                                                      
         BH    EVALR20                                                          
         MVC   TGPID,SCASSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   EVALR20                                                          
         MVC   SCASSN,TGSSN                                                     
         MVI   SCASSNH+5,9                                                      
EVALR20  GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',SCASSN)                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SCASSN,SSNSPACE                                                  
         MVC   SCASSN(L'TGPID),TGPID                                            
         MVI   SCASSNH+5,6                                                      
         OI    SCASSNH+6,X'80'                                                  
EVALR30  MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         BRAS  RE,VRHLD            MAKE SURE HOLDING FEE ELEM EXISTS            
*                                                                               
         BAS   RE,VRCAT            VALIDATE CATEGORY FIELD                      
         BAS   RE,VRLFTF                    LIFTED FROM FIELD                   
         BAS   RE,VRONOF                    CAMERA FIELD                        
         BRAS  RE,VRUNIT                    TAX UNIT FIELD                      
         BAS   RE,VRUNLO                    UNION/LOCAL FIELD                   
         BRAS  RE,VRYEAR                    YEAR FIELD                          
         BAS   RE,VRFCYC                    FIRST CYCLE FIELD                   
         BAS   RE,VRFRST                    FIRST SERVICES FIELD                
         BAS   RE,VRCORP                    CORPORATION                         
         BRAS  RE,VRNCDE                    AGENT FIELD                         
         BAS   RE,VRGUAR                    GUARANTEE FIELD                     
         BAS   RE,VRLST                     LAST SERVICES FIELD                 
         BAS   RE,VRRER                     RERECORD DATE FIELD                 
         BAS   RE,VREXP                     EXPIRATION DATE FIELD               
         BAS   RE,VRSTAT                    STATUS FIELD                        
         BAS   RE,VROP                      OVERSCALE PERCENTAGE FIELD          
         BAS   RE,VROA                      OVERSCALE AMOUNTS FIELD             
         BAS   RE,VROP2                     2ND OVERSCALE PERC FIELD            
         BAS   RE,VRDBL                     DOUBLES FIELD                       
         BRAS  RE,VRTRKS                    TRACKS FIELD                        
         BRAS  RE,VRCM                      COMMENTS FIELD                      
         BAS   RE,VRROLE                    ROLE DESCRIPTION                    
*                                                                               
*                                  ADD NEW CAST ELEMENT                         
EVALR50  MVC   ELEM(TACALNQ),CAELEM                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         BRAS  RE,VRRLSD           VALIDATE RELEASED STATUS                     
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
         SPACE 2                                                                
INITCAEL DS    0H                  INIT. CAELEM & RETURN R4=A(CAELEM)           
         LA    R4,CAELEM                                                        
         XC    CAELEM,CAELEM                                                    
         USING TACAD,R4                                                         
         MVI   TACAEL,TACAELQ                                                   
         MVI   TACALEN,TACALNQ                                                  
         BR    RE                                                               
         EJECT                                                                  
* THIS ROUTINE INSPECTS THE RECORD IN AIO AND DETERMINES HOW ITS                
* CATEGORY WILL EFFECT THE ANNOUNCER-ONLY-NON-MUSICIAN STATUS FOR               
* THE COMMERCIAL.  IF PARAMETER 1 IS 1 THEN THE ROUTINE TREATS THE              
* RECORD AS AN ADDITION OF A CATEGORY MEMBER.  IF PARAMETER 1 IS -1             
* THEN IT TREATS IT AS A SUBTRACTION.  DELETES WILL CALL THIS                   
* ROUTINE WITH A -1, ADDS AND RESTORES WITH A 1, AND CHANGES WITH               
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
         SPACE 2                                                                
* THIS ROUTINE INSPECTS THE RECORD IN AIO AND DETERMINES IF THE                 
* CAST MEMBER IS ON THE LIFT OR NOT.                                            
* IF PARAMETER 1 IS 1 THEN THE ROUTINE TREATS THE RECORD AS AN                  
* ADDITION,  IF PARAMETER 1 IS -1, THEN IT TREATS IT AS A                       
* SUBTRACTION.  DELETES WILL CALL THIS ROUTINE WITH A -1,                       
* ADDS AND RESTORES WITH A 1, AND CHANGES WITH A -1 FOR THE                     
* OLD RECORD AND A 1 FOR THE NEW ONE.                                           
*                                                                               
TESTLIFT NTR1                                                                   
         L     R3,0(R1)            R3 = ADJUSTMENT INDEX (-1/1)                 
*                                                                               
         L     R4,AIO                                                           
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   TLX                                                              
*                                                                               
         TM    TACASTAT,TACASTLF   IF CAST MEMBER IS ON LIFT                    
         BNO   TLX                                                              
         LH    RF,NUMLIFT          THEN ADJUST NUMBER                           
         AR    RF,R3                                                            
         STH   RF,NUMLIFT                                                       
*                                                                               
TLX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE EXTRACTS INFORMATION FFOM THE COMMERCIAL RECORD (FOUND           
* IN AIO) THAT BOTH THE CAST LIST AND EXTENSION SCREENS NEED.                   
*                                                                               
GETCOMM  NTR1                                                                   
         L     R4,AIO              R4 = A(COMMERCIAL KEY)                       
         USING TLCOD,R4                                                         
         MVC   CMCLCLI,TLCOCLI     SAVE CLIENT CODE FOR LATER USE               
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,TACOELQ      R4 = A(COMMERCIAL DETAILS ELEMENT)           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
*                                                                               
         MVC   COELEM,0(R4)        SAVE ELEMENT FOR LATER USE                   
*                                                                               
         MVC   CMCLMED,TACOMED     SAVE COMMERCIAL MEDIA FOR LATER USE          
         MVC   CMCLFCYC,TACOFCYC        FIRST CYCLE DATE                        
         MVC   CMCLEXP,TACOEXP          EXPIRATION DATE                         
         MVC   CMCLTYP,TACOTYPE         COMMERCIAL TYPE                         
*                                                                               
         MVI   HASLIFT,C'N'        SET COMMERCIAL HAS LIFT FLAG                 
         L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   HASLIFT,C'Y'                                                     
                                                                                
         B     XIT                                                              
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
         BRAS  RE,PKSSN            CONVERT SSN TO PID                           
         MVC   NAMSSN(16),=C'** NOT FOUND ** ' ELSE REC MUST BE MISSING         
*                                                                               
         OI    CVERSTAT,CVERSNW4   SET NO W4 RECORD IN CVERSTAT                 
         B     DRSSNNO             AND RETURN 'NO'                              
*                                                                               
DRSSN10  BRAS  RE,PKSSN            CONVERT SSN TO PID                           
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
* DISPLAY LIFTED FROM FIELD.                                                    
*                                                                               
DRLFTF   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
*                                                                               
         OC    TACALFTF,TACALFTF                                                
         BZ    DRLFTFX                                                          
         MVC   AIO,AIO2                                                         
         MVC   SVCOM,TGCOM                                                      
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TACALFTF)                            
         DROP  R4                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO2                                                          
         L     R2,ALFTAFLD                                                      
         MVC   8(6,R2),TLCOAGY                                                  
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      GET COMM DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R2,ALFTFFLD         R2 = A(LIFTED FROM FIELD)                    
         MVC   8(12,R2),TACOCID    DISPLAY LIFTED FROM                          
         DROP  R4                                                               
*                                                                               
         GOTO1 RECVAL,DMCB,(X'30',TLCOICDQ),SCACIDH                             
         MVC   TGCOM,SVCOM                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
DRLFTFX  B     XIT                                                              
         EJECT                                                                  
* DISPLAY CAMERA (ON/OFF) FIELD.                                                
*                                                                               
DRONOF   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AONOFFLD         R2 = A(CAMERA FIELD)                         
*                                                                               
         MVI   5(R2),3                                                          
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
         MVI   5(R2),7             NEED THIS FOR COPYDOWN RTN                   
*                                                                               
         CLC   TACAUN,=C'AFM'      IF UNION IS AFM                              
         BNE   DRUNLOX                                                          
*                                                                               
         GOTO1 YRVAL,DMCB,TACAYEAR CALL YRVAL TO SET EQUATE                     
         BNE   DRUNLOX                                                          
*                                                                               
         CLI   TGYREQU,91          AND YEAR IS '91 OR LATER                     
         BL    DRUNLOX                                                          
*                                                                               
         LA    R3,SCALCLN          R3 = A(LOCAL NAME FIELD DATA)                
         CLI   SCRFLAG,C'L'        IF LIST SCREEN                               
         BNE   DRUNLO10                                                         
         L     R3,ANAMEFLD         THEN POINT R3 WITHIN NAMES FIELD             
         USING NAMESD,R3                                                        
         LA    R3,NAMLOCL                                                       
         DROP  R3                                                               
*                                                                               
DRUNLO10 L     R4,AIO3             W4 RECORD IN AIO3                            
         MVI   ELCODE,TAW4ELQ      R4 = A(W4 DETAILS ELEMENT)                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
         CLC   TAW4LOCL,=CL7' '              IF AFM LOCAL NOT DEFINED           
         BH    *+14                                                             
         MVC   0(16,R3),=CL16'* NO W4 LOCAL *'  INDICATE SO ON SCREEN           
         B     DRUNLOX                                                          
*                                                                               
         CLI   SCRFLAG,C'L'        ELSE IF NOT LIST SCREEN                      
         BNE   *+12                                                             
         TM    OPTS,OPTOTH         OR IF DISPLAYING 'OTH'ER INFO                
         BNO   DRUNLOX                                                          
         MVC   0(16,R3),=CL16'W4 LOCAL ='  DISPLAY W4 LOCAL                     
         MVC   11(3,R3),TAW4LOCL                                                
*                                                                               
DRUNLOX  B     XIT                                                              
         EJECT                                                                  
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
DRLIFT   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ALIFTFLD         R2 = A(LIFT FIELD)                           
*                                                                               
         TM    TACASTAT,TACASTLO   IF ONLY ON LIFT                              
         BZ    DRLIFT10                                                         
         MVI   8(R2),C'O'          THEN DISPLAY 'O'                             
         B     DRLIFT20                                                         
*                                                                               
DRLIFT10 TM    TACASTAT,TACASTLF   ELSE IF ON LIFT                              
         BZ    DRLIFTX                                                          
         MVI   8(R2),C'Y'          THEN DISPLAY 'Y'                             
*                                                                               
DRLIFT20 DS    0H                  ELSE DISPLAY BLANK                           
*                                                                               
DRLIFTX  B     XIT                                                              
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
DRCORP40 MVC   0(16,R2),NAMESH+8   ELSE MOVE CAST CORP NAME TO SCREEN           
         MVI   ELCODE,TAW4ELQ      GET W4 ELEMENT                               
         L     R4,AIO3                                                          
         USING TAW4D,R4                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAW4STA2,TAW4SLCK   IF THIS CRP IS LOCKED                        
         BNO   DRCORP90                                                         
         MVC   0(16,R2),=C'** CRP LOCKED **' PUT IT OUT                         
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
*                                                                               
         CLI   SCRFLAG,C'L'        IF LIST SCREEN THEN DONE                     
         BE    DRGUARX                                                          
*                                                                               
         MVC   FULL,8(R2)          ELSE COMPLEMENT CODE INTO FULL               
         XC    FULL,XFFS                                                        
*                                                                               
         MVC   AIO,AIO2            READ GUARANTEE RECORD INTO AIO2              
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'A0',FULL)                                 
         MVC   AIO,AIO1                                                         
         BNE   DRGUARX                                                          
*                                                                               
         L     R4,AIO2             R4 = A(GUARANTEE DETAILS ELEMENT)            
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGUD,R4                                                         
*                                                                               
         L     R2,AGUADFLD         DISPLAY GUARANTEE DATES                      
         GOTO1 DATCON,DMCB,(X'11',TAGUSTRT),(8,8(R2))                           
         B     DRGUARX                                                          
*                                                                               
DRGUAR50 CLI   SCRFLAG,C'L'        ELSE IF LIST SCREEN                          
         BNE   DRGUARX                                                          
         CLI   VALMODE,C'A'        AND ADDING THIS RECORD                       
         BNE   DRGUARX                                                          
*                                  THEN IF PERFORMER HAS A GUARANTEE            
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'80',=F'0')                                
         CLC   KEY(TLGUGUA-TLGUD),KEYSAVE                                       
         BNE   DRGUARX                                                          
*                                                                               
         L     R3,ANAMEFLD         DISPLAY 'ON GRT' IN NAMES FIELD              
         USING NAMESD,R3                                                        
         MVC   NAMGUAR,=C'ON GRT'                                               
*                                                                               
DRGUARX  B     XIT                                                              
         DROP  R3                                                               
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
         L     R4,ACAELEM          OR 2ND OVERSCALE PERCNETAGE EXISTS           
         USING TACAD,R4                                                         
         OC    TACAOV2,TACAOV2                                                  
         BNZ   DROP10                                                           
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
DROP10   MVC   8(3,R2),=C'***'     THEN DISPLAY '****'                          
         B     DROPX                                                            
*                                                                               
DROP20   LA    RF,8(R2)                                                         
         ICM   R3,15,TAOPPCT       ELSE R3 = PERCENTAGE                         
*                                                                               
*                                  DISPLAY FIRST PERCENTAGE                     
         BNM   DROP29              IS IT NEGATIVE                               
         MVI   0(RF),C'%'          IT IS PERCENT SCALE                          
         LA    RF,1(RF)                                                         
         N     R3,=X'7FFFFFFF'     STRIP OFF HOB                                
         LHI   R4,2                                                             
         B     *+8                                                              
DROP29   LHI   R4,3                                                             
         GOTOR DR2DEC,DMCB,(R3),(RF),(R4)                                       
         B     DROPX                                                            
*                                                                               
*        FOR EXTENSION SCREEN DISPLAY ENTIRE ELEMENT                            
*                                                                               
DROP50   GOTOR DROPA,DMCB,('TAOPELQ',(R2))                                      
DROPX    B     XIT                                                              
         EJECT                                                                  
* DISPLAY OVERSCALE AMOUNTS ELEMENT.                                            
*                                                                               
DROA     NTR1                                                                   
         L     R2,AOAFLD           R2 = A(OVERSCALE AMOUNT FIELD)               
         MVI   BYTE,0                                                           
         GOTOR DROPA,DMCB,('TAOAELQ',(R2))                                      
*                                                                               
         CLI   BYTE,C'Y'           IF HOLDING FEE AMOUNT FOUND                  
         BNE   XIT                                                              
*                                                                               
         USING TACRD,R4                                                         
         L     R4,AIO              THEN R4 = A(FIRST CR ELEM)                   
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL            DONE IF NONE FOUND                           
         BE    DROA20                                                           
         B     XIT                                                              
*                                                                               
DROA10   CLC   TACRINV,DUB                                                      
         BNH   DROA30                                                           
DROA20   MVC   FULL,TACRBAL            SAVE BALANCE OF LAST ELEMENT             
         MVC   DUB(L'TACRINV),TACRINV  AND ITS INV NUM                          
*                                                                               
DROA30   BAS   RE,NEXTEL           GET NEXT CR ELEMENT                          
         BE    DROA10              REPEAT UNTIL NO MORE                         
         DROP  R4                                                               
*                                                                               
         LA    R2,SCAHLDH          DISPLAY 'BAL=AMOUNT'                         
         MVC   8(4,R2),=C'BAL='                                                 
         EDIT  (4,FULL),(11,12(R2)),2,ALIGN=LEFT,MINUS=YES                      
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY SECOND OVERSCALE PERCENTAGE FIELD.                                    
*                                                                               
DROV2    NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AOV2FLD          R2 = A(2ND OVERSCALE PERC FIELD)             
*                                                                               
         ICM   R3,15,TACAOV2       R3 = PERCENTAGE                              
         BZ    DROV210                                                          
         MVC   8(4,R2),=C'ALL='    DISPLAY PERCENTAGE                           
         GOTOR DR2DEC,DMCB,(R3),12(R2),10                                       
         B     XIT                                                              
*                                                                               
DROV210  GOTOR DROPA,DMCB,('TAO2ELQ',(R2))                                      
DROV2X   B     XIT                                                              
         EJECT                                                                  
* DISPLAY DOUBLES FIELD.                                                        
*                                                                               
DRDBL    NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ADBLFLD          R2 = A(DOUBLES FIELD)                        
*                                                                               
         MVC   8(1,R2),TACADBL     DISPLAY DOUBLES                              
*                                                                               
DRDBLX   B     XIT                                                              
         SPACE 3                                                                
* DISPLAY TRACKS CAST IS ON                                                     
DRTRKS   NTR1                                                                   
         USING TATRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRTRKS20                                                         
         L     R2,ATRKSFLD                                                      
         LA    R2,8(R2)                                                         
DRTRKS10 MVC   0(1,R2),TATRTRK                                                  
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         LA    R2,1(R2)                                                         
         B     DRTRKS10                                                         
         DROP  R4                                                               
*                                                                               
DRTRKS20 GOTO1 CHAROUT,DMCB,TAFNELQ,ATRKSFLD,TAFNTTRK                           
         B     XIT                                                              
         SPACE 3                                                                
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
* DISPLAY FIRST CYCLE DATE.                                                     
*                                                                               
DRFCYC   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AFCYCFLD         R2 = A(FIRST CYCLE FIELD)                    
*                                                                               
*                                  DISPLAY Y/M/D                                
         GOTO1 DATCON,DMCB,(1,TACAFCYC),(8,8(R2))                               
*                                                                               
DRFCYCX  B     XIT                                                              
         EJECT                                                                  
* DISPLAY FIRST SERVICES DATE.                                                  
*                                                                               
DRFRST   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AFRSTFLD         R2 = A(FIRST SERVICES FIELD)                 
*                                                                               
*                                  DISPLAY Y/M/D                                
         GOTO1 DATCON,DMCB,(1,TACAFRST),(8,8(R2))                               
         OC    TACAFRST,TACAFRST                                                
         BZ    XIT                                                              
         MVI   5(R2),8                                                          
         B     XIT                                                              
         SPACE 3                                                                
* DISPLAY LAST SERVICES DATE.                                                   
*                                                                               
DRLST    NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ALSTFLD          R2 = A(LAST SERVICES FIELD)                  
*                                                                               
*                                  DISPLAY Y/M/D                                
         GOTO1 DATCON,DMCB,(1,TACALAST),(8,8(R2))                               
*                                                                               
DRLSTX   B     XIT                                                              
         SPACE 3                                                                
* DISPLAY RERECORD DATE.                                                        
*                                                                               
DRRER    NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         CLI   TACALEN,TACALNQ                                                  
         BL    XIT                                                              
         L     R2,ARERFLD          R2 = A(RERECORD DATE FIELD)                  
*                                                                               
*                                  DISPLAY Y/M/D                                
         GOTO1 DATCON,DMCB,(1,TACARERC),(8,8(R2))                               
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
* DISPLAY EXPIRATION DATE.                                                      
*                                                                               
DREXP    NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,AEXPFLD          R2 = A(EXPIRATION DATE FIELD)                
*                                                                               
*                                  DISPLAY Y/M/D                                
         GOTO1 DATCON,DMCB,(1,TACAEXP),(8,8(R2))                                
*                                                                               
DREXPX   B     XIT                                                              
         EJECT                                                                  
* DISPLAY STATUS ELEMENT.                                                       
*                                                                               
DRSTAT   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ASTATFLD         R2 = A(STATUS FIELD DATA)                    
         LA    R2,8(R2)                                                         
*                                                                               
         MVI   SCAEQTY,C'N'                                                     
         TM    TACASTA3,TACASEQY   EQUITY PERFORMER?                            
         BZ    *+8                                                              
         MVI   SCAEQTY,C'Y'                                                     
*                                                                               
         L     RF,ASTATTAB         RF = A(STATUS CODE TABLE)                    
*                                                                               
DRS10    CLI   0(RF),X'FF'         WHILE NOT END OF TABLE                       
         BE    DRS100                                                           
*                                                                               
         OC    0(2,RF),0(RF)                                                    
         BZ    DRS11                                                            
         MVC   HALF,0(RF)          IF BIT IS ON IN STATUS BYTE                  
         NC    HALF,TACASTAT                                                    
         BZ    DRS30                                                            
                                                                                
         CLI   HALF,TACASTLF       DON'T DISPLAY 'ONLIFT' IF ALSO GOING         
         BNE   DRS15                   TO DISPLAY 'LIFTONLY'                    
         TM    TACASTAT,TACASTLO                                                
         BO    DRS30                                                            
         B     DRS15                                                            
*                                                                               
DRS11    MVC   BYTE,2(RF)                                                       
         NC    BYTE,TACASTA3                                                    
         BZ    DRS30                                                            
                                                                                
DRS15    CLI   0(R2),C' '          IF NOT FIRST CODE                            
         BNH   DRS20                                                            
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
*                                                                               
DRS20    MVC   0(10,R2),3(RF)      DISPLAY CODE                                 
*                                                                               
         LA    R2,9(R2)            POINT R2 TO LAST NON-SPACE                   
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
DRS30    LA    RF,L'STATTAB(RF)    BUMP TO NEXT STATUS CODE TABLE ENTRY         
*                                                                               
         B     DRS10               LOOP BACK                                    
*                                                                               
DRS100   L     R4,AIO              R4 = A(HOLDING FEE NOTICE ELEMENT)           
         MVI   ELCODE,TAHFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRSX                RETURN IF NONE FOUND                         
         USING TAHFD,R4                                                         
*                                                                               
         TM    TAHFSTAT,TAHFSNO    IF SUPPRESS HLD FEE NOTICE BIT SET           
         BZ    DRS120                                                           
*                                                                               
         CLI   0(R2),C' '          THEN IF SOMETHING ALREADY DISPLAYED          
         BNH   DRS110                                                           
         MVI   1(R2),C','          DISPLAY ',' AND BUMP PAST                    
         LA    R2,2(R2)                                                         
*                                                                               
DRS110   MVC   0(5,R2),=C'NOHFN'   DISPLAY 'NOHFN'                              
         B     DRSX                                                             
*                                                                               
DRS120   OC    TAHFMDTE,TAHFMDTE   IF WE'VE SENT A HOLDING FEE NOTICE           
         BZ    DRSX                                                             
         GOTO1 DATCON,DMCB,(1,TAHFMDTE),(8,SCAHFN+17)  DISP MARKED DATE         
         GOTO1 (RF),(R1),(1,TAHFNXTS),(8,SCAHFN+34)         CYCLE START         
         NI    SCAHFNH+1,X'F3'                    MAKE NORMAL INTENSITY         
*                                                                               
         OC    TAHFHDTE,TAHFHDTE   IF SEASONAL PRINT DATE EXISTS                
         BZ    DRS130              PRINT THIS START DATE INSTEAD                
         GOTO1 (RF),(R1),(1,TAHFHDTE),(8,SCAHFN+34)                             
*                                                                               
DRS130   OC    TAHF2DTE,TAHF2DTE   IF WE'VE SENT A SECOND NOTICE                
         BZ    DRSX                                                             
         GOTO1 DATCON,DMCB,(1,TAHF2DTE),(8,SCA2ND+17)  DISP MARKED DATE         
         NI    SCA2NDH+1,X'F3'                    MAKE NORMAL INTENSITY         
*                                                                               
DRSX     B     XIT                                                              
         EJECT                                                                  
* DISPLAY OTHER DETAILS (OPTIONAL).                                             
*                                                                               
DROTH    NTR1                                                                   
         TM    OPTS,OPTOTH                                                      
         BNO   DROTHX                                                           
*                                                                               
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         USING TACAD,R4                                                         
         L     R3,ANAMEFLD         R3 = A(NAMES FIELD)                          
         USING NAMESD,R3                                                        
*                                                                               
         XC    WORK,WORK           DISPLAY LAST CHANGED INFO                    
         MVI   WORK,17+8                                                        
         GOTO1 ACTVOUT,DMCB,WORK                                                
         MVC   NAMLCHG,WORK+8                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(1,TACAEXP),(8,NAMEXP)  DISPLAY EXPIRY DATE          
*                                                                               
DROTHX   B     XIT                                                              
         EJECT                                                                  
* VALIDATE SSN FIELD.                                                           
*                                                                               
VRSSN    NTR1                                                                   
         MVC   AIO,AIO3            VALIDATE SSN (READ W4 REC INTO IO3)          
         L     R2,ASSNFLD                                                       
         BRAS  RE,UNPKPID          CONVERT TO SSN FOR RECVAL CALL               
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'24',ASSNFLD)                              
         BE    VRSSN10                                                          
         MVC   8(L'TGSSN,R2),BLOCK   RESTORE WHAT WAS ENTERED                   
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
                                                                                
         TM    TAW4STA3,TAW4SREG   IF W4 IS FOR REGRESSION TESTING              
         JZ    VRSSN14                                                          
                                                                                
         TM    SVAYSTA6,TAAYSREG   ENSURE AGENCY                                
         JO    VRSSNX                                                           
         TM    SVCISTA2,TACISREG   OR CLIENT IS FOR REGRESSION TESTING          
         JO    VRSSNX                                                           
         J     ERW4REG                                                          
                                                                                
VRSSN14  TM    SVAYSTA6,TAAYSREG   IF W4 IS NOT FOR REGRESSION TESTING          
         JO    ERW4NREG            ENSURE AGENCY                                
         TM    SVCISTA2,TACISREG   AND CLIENT ARE NOT FOR REGRESSION            
         JO    ERW4NREG            TESTING                                      
                                                                                
VRSSNX   B     XIT                                                              
         SPACE 3                                                                
* VALIDATE CATEGORY FIELD.                                                      
*                                                                               
VRCAT    NTR1                                                                   
         L     R2,ACATFLD          R2 = A(CATEGORY FIELD)                       
         GOTOR VALWEB,DMCB,(0,0)                                                
*                                                                               
         USING TLCAD,R4                                                         
         L     R4,AIO              R4 = A(CAST RECORD)                          
*                                                                               
         GOTOR COPYDOWN,DMCB,SVACAT COPY FIELD FROM ABOVE IF NECCESSARY         
*                                                                               
         BRAS  RE,MYANY            MOVE AND SPACE PAD FIELD TO WORK             
*                                                                               
         GOTO1 CATVAL,DMCB,WORK    VALIDATE CATEGORY CODE                       
         BNE   ERRINV                                                           
*                                                                               
         TM    TGSYSTAT,TASYSMUS   IF NEW MUSIC RULES ENABLED                   
         BZ    VRCAT30                                                          
         CLI   VALMODE,C'A'        AND ADDING NEW PERFORMER                     
         BE    VRCAT10                                                          
         CLC   TLCACAT,TGCAT       OR CATEGORY HAS BEEN CHANGED                 
         BE    VRCAT30                                                          
VRCAT10  CLI   CMCLTYP,CTYMUS      IF COMMERCIAL TYPE IS MUSIC                  
         BNE   VRCAT20                                                          
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BZ    ERM489              NEW CATEGORY MUST BE MUSICIAN                
         B     VRCAT30                                                          
VRCAT20  CLI   TGCAEQU,CTZZ        IF COMMERCIAL TYPE IS NOT MUSIC              
         BE    VRCAT30             NEW CATEGORY MUST BE ZZ                      
         CLI   TGCAEQU,CTZZZ       OR ZZZ                                       
         BE    VRCAT30                                                          
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BO    ERRNOMUS            OR NON-MUSICIAN                              
*                                                                               
VRCAT30  MVC   TLCACAT,TGCAT       SAVE IN RECORD                               
*                                                                               
VRCATX   B     XIT                                                              
         EJECT                                                                  
* VALIDATE LIFTED FROM FIELD (OPTIONAL).                                        
*                                                                               
VRLFTF   NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         XC    TACALFTF,TACALFTF   IF NOT ENTERED THEN ZEROS                    
                                                                                
         L     R2,ALFTAFLD         R2=A(LIFTED FROM AGENCY FIELD)               
         GOTOR VALWEB,DMCB,(X'80',0)                                            
                                                                                
         L     R3,ALFTFFLD         R3=A(LIFTED FROM ID FIELD)                   
                                                                                
         CLI   5(R2),0             IF LIFTED FROM AGENCY IS                     
         BNE   VRLFTF10            NOT PROVIDED                                 
         CLI   5(R3),0             ENSURE LIFTED FROM ID IS NOT                 
         BNE   FLDMISS             PROVIDED                                     
         B     XIT                                                              
*                                                                               
VRLFTF10 CLI   5(R3),0             IF LIFTED FROM AGENCY IS                     
         BNE   VRLFTF20            PROVIDED                                     
         LR    R2,R3               ENSURE LIFTED FROM ID IS                     
         B     FLDMISS             PROVIDED AS WELL                             
                                                                                
VRLFTF20 MVC   SVAGY,TGAGY         SAVE GLOBAL VARIABLES                        
         MVC   SVCOM,TGCOM                                                      
         MVC   SVCID,TGCID                                                      
                                                                                
         XR    R0,R0               CLEAR ERROR INDICATOR                        
                                                                                
         MVC   AIO,AIO2            VALIDATE LIFTED FROM AGENCY                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(4,(R2))                                     
         BNE   VRLFTX                                                           
*                                                                               
         LR    R2,R3               VALIDATE LIFTED FROM ID                      
         GOTOR VALWEB,DMCB,(X'80',0)                                            
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'24',(R2))                                
         BNE   VRLFTX                                                           
                                                                                
         LHI   R0,1                SET COMMERCIAL FOUND                         
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
         MVC   TACALFTF,TLCOICOM   SAVE INTERNAL COMMERCIAL ID                  
         DROP  R3,R4                                                            
                                                                                
VRLFTX   MVC   TGAGY,SVAGY         ALWAYS RESTORE GLOBAL VARIABLES              
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'A0',SVCID)                               
         MVC   TGCOM,SVCOM                                                      
         MVC   AIO,AIO1            AND I/O AREA                                 
                                                                                
         LTR   R0,R0               IF AGENCY OR COMMERCIAL ID DOES              
         BZ    ERRINV              NOT EXIST, GIVE ERROR                        
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE CAMERA (ON/OFF) FIELD.                                               
*                                                                               
VRONOF   NTR1                                                                   
         L     R2,AONOFFLD         R2 = A(CAMERA FIELD)                         
         GOTOR VALWEB,DMCB,(0,0)                                                
*                                                                               
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
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
VRONOF30 GOTOR COPYDOWN,DMCB,SVAONOF  ELSE TRY TO COPY FLD FROM ABOVE           
         B     VRONOF50                                                         
*                                                                               
VRONOF40 MVI   5(R2),3             SET FLD LENGTH TO 3                          
*                                                                               
VRONOF50 BRAS  RE,MYANY            MOVE AND SPACE PAD FIELD TO WORK             
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
*                                                                               
VRONOFX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE UNION/LOCAL FIELD (DEFAULT TO ABOVE).                                
*                                                                               
VRUNLO   NTR1                                                                   
         L     R2,AUNLOFLD         R2 = A(UNION/LOCAL FIELD)                    
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
*                                                                               
         USING SCAND,R3                                                         
         LA    R3,BLOCK            R3 = A(SCANNER BLOCK)                        
                                                                                
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
*                                                                               
         GOTOR COPYDOWN,DMCB,SVAUNLO  COPY FLD FROM ABOVE IF NECCESSARY         
*                                                                               
         CLI   5(R2),3             FIELD MUST BE LONGER THEN 3 BYTES            
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
         CLC   =C'LA ',TACAUNIT    IF TAX STATE IS LOUISIANA                    
         BNE   VRUNLO30                                                         
         CLC   =C'LA ',TGLCL       LOCAL CAN'T BE FOR LOS ANGELES               
         BE    ERRTAX                                                           
         CLC   =C'47 ',TGLCL                                                    
         BE    ERRTAX                                                           
VRUNLO30 MVC   TACALOCL,TGLCL      SAVE LOCAL IN ELEMENT                        
*                                                                               
         CLC   TACAUNIT,=C'CN '    IF TAX UNIT IS CANADA                        
         BE    VRUNLO40                                                         
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',TACAUNIT)                                     
         BNE   VRUNLOX                                                          
VRUNLO40 CLC   =C'NON',TACALOCL    AND LOCAL IS NON                             
         BNE   VRUNLOX                                                          
         CLC   =C'NON',TACAUN      AND UNION IS NON                             
         BE    ERRINV              RETURN ERROR (SHOULD BE NON CAN)             
VRUNLOX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE LIFT FIELD (OPTIONAL).                                               
*                                                                               
VRLIFT   NTR1                                                                   
         L     R4,AIO              TURN OFF X'20' BIT IN STATUS BYTE            
         USING TLCAD,R4                                                         
         NI    TLCASTAT,X'DF'                                                   
*                                                                               
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ALIFTFLD         R2 = A(LIFT FIELD)                           
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
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
         B     VRLIFT30                                                         
*                                                                               
VRLIFT10 CLI   8(R2),C'Y'          ELSE IF FIELD CONTAINS 'Y'                   
         BNE   VRLIFT20            THEN SET LIFT BIT                            
         OI    TACASTAT,TACASTLF                                                
         B     VRLIFT30                                                         
*                                                                               
VRLIFT20 B     ERRINV              ELSE INVALID INPUT FIELD                     
*                                                                               
VRLIFT30 L     R4,AIO              SET X'20' BIT IN STATUS BYTE                 
         USING TLCAD,R4                                                         
         OI    TLCASTAT,X'20'      CAST MEMBER ON LIFT                          
*                                                                               
VRLIFTX  B     XIT                                                              
         EJECT                                                                  
* VALIDATE CORPORATION NUMBER FIELD (OPTIONAL).                                 
*                                                                               
VRCORP   NTR1                                                                   
         L     R2,ACORPFLD         R2 = A(CORPORATION FIELD)                    
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
*                                                                               
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
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
         L     R2,AGUARFLD         R2 = A(GUARANTEE FIELD)                      
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
*                                                                               
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         XC    TACAGUA,TACAGUA     PRE-CLEAR ELEMENT DATA                       
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY THEN DONE                  
         BE    XIT                                                              
*                                                                               
         MVC   TACAGUA,8(R2)       ELSE SAVE GUARANTEE CODE IN ELEMENT          
*                                                                               
         MVC   FULL,8(R2)          COMPLEMENT FIELD DATA INTO FULL              
         XC    FULL,XFFS                                                        
*                                                                               
         MVC   AIO,AIO2            VALIDATE GUARANTEE CODE AND READ REC         
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'A0',FULL)                                 
         MVC   AIO,AIO1                                                         
         BNE   ERRRNF                                                           
*                                                                               
         L     R4,AIO2             R4 = A(GUARANTEE DETAILS ELEMENT)            
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGUD,R4                                                         
*                                                                               
         TM    TAGUSTAT,TAGUSLCK   IF GUARANTEE RECORD IS LOCKED                
         BO    ERRGUAR             CANNOT BE ATTACHED TO CAST                   
*                                                                               
         CLI   TAGUCRP,0           IF GUARANTEE HAS A CORP                      
         BE    VRGUAR10                                                         
         CLC   TAGUCRP,CASTCORP    THEN CORP MUST MATCH CAST CORP               
         BNE   ERRINV                                                           
*                                                                               
VRGUAR10 XR    R0,R0                                                            
*                                                                               
         OC    TAGUAGY(L'TAGUAGY+L'TAGUCLI),TAGUAGY                             
         BNZ   VRGUAR20                                                         
         TM    TGCTSTST,TGCTSCLI                                                
         BO    ERRINV                                                           
*                                                                               
VRGUAR20 OC    TAGUAGY,TAGUAGY     IF GUARANTEE IS FOR SPECIFIC AGENCY          
         BZ    VRGUAR30                                                         
         CLC   TAGUAGY,TGAGY       MUST BE SAME AGENCY                          
         BE    VRGUAR30                                                         
         LHI   R0,1                                                             
*                                                                               
VRGUAR30 OC    TAGUCLI,TAGUCLI     IF GUARANTEE IS FOR SPECIFIC CLIENT          
         BZ    VRGUAR40                                                         
         CLC   TAGUCLI,CMCLCLI     THEN MUST BE SAME CLIENT AS CMCL             
         BE    VRGUAR40                                                         
         LHI   R0,1                                                             
*                                                                               
VRGUAR40 LTR   R0,R0               IF USING NEW GUARANTEE SYSTEM                
         BZ    XIT                 AND AGY/CLI MATCH NOT YET FOUND              
*                                                                               
         USING TAVAD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TAVAELQ      READ ALL AGENCY/CLIENT                       
         BAS   RE,GETEL            ELEMENTS                                     
         B     *+8                                                              
VRGUAR50 BAS   RE,NEXTEL                                                        
         BNE   ERRINV                                                           
*                                                                               
         CLC   TAVAAGY,TGAGY       IF AGENCY IS FOUND IN GRT LIMITS             
         BNE   VRGUAR50                                                         
         CLI   TAVALEN,TAVALNQ     AND CLIENT LIMITS ARE NOT DEFINED            
         BE    XIT                 ACCESS IS GRANTED                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
VRGUAR60 CLC   CMCLCLI,0(RF)       IF CLIENT IS FOUND IN GRT LIMITS             
         BE    XIT                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   VRGUAR60                                                         
         B     VRGUAR50                                                         
         DROP  R4                                                               
         SPACE 2                                                                
         EJECT                                                                  
* VALIDATE ROLE DESCRIPTION FIELD (OPTIONAL).                                   
*                                                                               
VRROLE   NTR1                                                                   
         L     R2,AROLEFLD         R2 = A(ROLE DESC. FIELD)                     
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',(R2)),TACMTYPD                         
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE OVERSCALE PERCENTAGE FIELD (OPTIONAL).                               
*                                                                               
VROP     NTR1                                                                   
         L     R2,AOPFLD           R2 = A(OVERSCALE PERCENTAGE FIELD)           
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
*                                                                               
         TM    4(R2),X'20'         IF FIELD HASN'T CHANGED THEN DONE            
         BO    XIT                                                              
*                                                                               
         MVI   ELCODE,TAOPELQ      ELCODE = OVERSCALE PERC ELEMENT              
         GOTO1 REMELEM             REMOVE OLD ELEMENT                           
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY THEN DONE                  
         BE    XIT                                                              
*                                                                               
         CLI   SCRFLAG,C'L'        IF LIST SCREEN THEN VALIDATE PERC            
         BNE   VROP20                  ONLY AND SET USE CODE TO 'ALL'           
*                                                                               
         USING TAOPD,R4                                                         
         LA    R4,ELEM             BUILD NEW OVERSCALE PERCENT ELEMENT          
         XC    ELEM,ELEM                                                        
         MVI   TAOPEL,TAOPELQ                                                   
*                                                                               
         MVI   BYTE,0                                                           
         CLI   8(R2),C'%'          PERCENT SCALE WANTED                         
         BNE   VROP10                                                           
         SR    RF,RF                                                            
         IC    RF,5(R2)            GET LENGTH                                   
         BCTR  RF,0                                                             
         STC   RF,5(R2)            TAKE OFF THE %                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),9(R2)       SHIFT LEFT THE WHOLE FIELD                   
         LA    RF,9(RF,R2)                                                      
         MVI   0(RF),C' '          SPACE OUT LAST CHAR                          
         OI    BYTE,X'80'                                                       
*                                                                               
VROP10   GOTO1 VALIDEC             VALIDATE PERCENTAGE                          
         TM    FULL,X'80'                                                       
         BO    ERRINV              DON'T ALLOW NEGATIVE AMOUNTS                 
         CLI   BYTE,0                                                           
         BE    *+8                                                              
         OI    FULL,X'80'          MAKE IT NEGATIVE FOR PERCENT SCALE           
         MVC   TAOPPCT,FULL        SAVE IN ELEMENT                              
         MVC   TAOPUSE,=CL7' '     SET USE CODE TO SPACES FOR ALL               
         MVI   TAOPNUM,1           SET NUMBER OF SUB ELEMENTS TO 1              
         MVI   TAOPLEN,TAOPLNQ+L'TAOPSBEL    SET LENGTH                         
         B     VROP30                                                           
*                                                                               
*        VALIDATE ENTIRE ELEMENT FOR EXTENSION SCREEN                           
*                                                                               
VROP20   GOTOR VROPA,DMCB,('TAOPELQ',(R2))                                      
VROP30   GOTO1 ADDELEM             ADD ELEMENT                                  
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE SECOND OVERSCALE PERCENTAGE FIELD (OPTIONAL).                        
*                                                                               
VROP2    NTR1                                                                   
         L     R2,AOV2FLD          R2 = A(2ND OVERSCALE PERC FIELD)             
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
*                                                                               
         MVI   ELCODE,TAO2ELQ      ELCODE = OVERSCALE PERC ELEMENT              
         GOTO1 REMELEM             REMOVE OLD ELEMENT                           
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY THEN DONE                  
         BE    XIT                                                              
*                                                                               
         GOTOR VROPA,DMCB,('TAO2ELQ',(R2))                                      
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE OVERSCALE AMOUNTS FIELD (OPTIONAL).                                  
*                                                                               
VROA     NTR1                                                                   
         L     R2,AOAFLD           R2 = A(OVERSCALE AMOUNTS FIELD)              
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         MVI   ELCODE,TAOAELQ      ELCODE = OVERSCALE AMOUNTS ELEMENT           
         GOTO1 REMELEM             REMOVE OLD ELEMENT                           
                                                                                
         CLI   5(R2),0             IF FIELD IS EMPTY THEN DONE                  
         BE    XIT                                                              
                                                                                
         GOTOR VROPA,DMCB,('TAOAELQ',(R2))                                      
         GOTO1 ADDELEM             ADD ELEMENT                                  
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE DOUBLES FIELD (OPTIONAL).                                            
*                                                                               
VRDBL    NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ADBLFLD          R2 = A(DOUBLES FIELD)                        
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
*                                                                               
         MVI   TACADBL,C' '        IF FIELD IS EMPTY THEN SET TO BLANK          
         CLI   5(R2),0                                                          
         BE    VRDBLX                                                           
*                                                                               
         TM    TGCASTAT,OKDOUBLE   ELSE IF NOT OK TO DOUBLE                     
         BZ    ERRINV              THEN INVALID INPUT                           
*                                                                               
         GOTO1 VALINUM             ELSE VALIDATE NUMBER                         
         MVC   TACADBL,8(R2)       SAVE CHAR FORM IN ELEMENT                    
*                                                                               
VRDBLX   B     XIT                                                              
         EJECT                                                                  
* VALIDATE FIRST CYCLE DATE (OPTIONAL).                                         
*                                                                               
VRFCYC   NTR1                                                                   
         L     R2,AFCYCFLD         R2 = A(FIRST CYCLE FIELD)                    
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
*                                                                               
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
*                                                                               
         GOTO1 DTVAL,DMCB,(X'80',TACAFCYC)  VALIDATE IF INPUT                   
*                                                                               
         CLI   5(R2),0             INPUT NOT ALLOWED FOR SEASONALS              
         BE    XIT                                                              
         CLI   CMCLTYP,CTYSEAS2                                                 
         BE    DATSEA                                                           
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE FIRST SERVICES DATE (OPTIONAL).                                      
*                                                                               
VRFRST   NTR1                                                                   
         L     R2,AFRSTFLD         R2 = A(FIRST SERVICES FIELD)                 
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
******** GOTOR VALWEB,DMCB,(X'80',0)                                            
*                                                                               
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         XC    TACAFRST,TACAFRST   PRE-CLEAR FIELD                              
*                                                                               
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+10                                                             
         XC    TACAEXP,TACAEXP     THEN CLEAR EXPIRATION DATE AS WELL           
*                                                                               
         CLI   5(R2),0             IF NO INPUT THEN DONE                        
         BE    VRFRSTX                                                          
*                                  ELSE VALIDATE Y/M/D                          
         GOTO1 DTVAL,DMCB,TACAFRST                                              
*                                                                               
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+8                                                              
         BRAS  RE,CALCEXP          CALCULATE EXPIRATION DATE NOW                
*                                                                               
VRFRSTX  B     XIT                                                              
         SPACE 3                                                                
* VALIDATE LAST SERVICES DATE (OPTIONAL).                                       
*                                                                               
VRLST    NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ALSTFLD          R2 = A(LAST SERVICES FIELD)                  
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
*                                                                               
         GOTO1 DTVAL,DMCB,(X'80',TACALAST)  VALIDATE IF INPUT                   
*                                                                               
         OC    TACALAST,TACALAST   IF LAST SERVICES FIELD IS POPULATED          
         BZ    XIT                                                              
         OC    TACAGUA,TACAGUA     AND GUARANTEE CODE IS SET ...                
         BZ    XIT                                                              
*                                                                               
         USING TAGUD,R4                                                         
         L     R4,AIO2             R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         BNE   XIT                 NO FURTHER VALIDATION REQUIRED               
         OC    TAGUCOM,TAGUCOM     IF LARGE OVERSCALE GUARANTEE                 
         BZ    XIT                                                              
         CLC   TGCOM,TAGUCOM       OR SUBSIDIARY COMMERCIAL FOR                 
         BNE   XIT                 PER CYCLE GUARANTEE                          
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
*                                                                               
         USING TLCAPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY             READ CAST RECORDS FOR ALL OF                 
         MVI   TLCAPCD,TLCAGCDQ    THE GUARANTEE'S SUBSIDIARY                   
         MVC   TLCAGSSN,TGSSN      COMMERCIALS                                  
         MVC   TLCAGGUA,TGGUA                                                   
         XC    TLCAGGUA,=6X'FF'                                                 
         GOTO1 HIGH                                                             
         B     VRLST20                                                          
VRLST10  GOTO1 SEQ                                                              
VRLST20  CLC   KEY(TLCAGCOM-TLCAPD),KEYSAVE                                     
         BNE   VRLSTX                                                           
         CLC   TLCACCOM,TGCOM      SKIP THE PRIMARY COMMERCIAL                  
         BE    VRLST10                                                          
         GOTO1 GETREC                                                           
         DROP  R3                                                               
*                                                                               
         USING TACAD,R4                                                         
         L     R4,AIO              R4=A(SUBSIDIARY COMMERCIAL'S CAST            
         MVI   ELCODE,TACAELQ           RECORD)                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         OC    TACALAST,TACALAST   SKIP IF CAST HAS BEEN LAST                   
         BNZ   VRLST10             SERVICED                                     
         DROP  R4                                                               
*                                                                               
         MVC   SVGCKEY,KEY         SAVE GUARANTEE CAST KEY                      
*                                                                               
         USING TLCOPD,R3                                                        
         XC    KEY,KEY             READ SUBSIDIARY COMMERCIAL RECORD            
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVGCKEY+TLCAGCOM-TLCAPD                                 
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
*                                                                               
         USING TACOD,R4                                                         
         L     R4,AIO              R4=A(SUBSIDIARY COMMERCIAL RECORD)           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    TACOSTAT,TACOSTRL   IF COMMERCIAL IS NOT RELEASED,               
         BZ    ERRPRIRL            RETURN ERROR                                 
         DROP  R4                                                               
*                                                                               
         MVC   KEY,SVGCKEY         RESTORE GUARANTEE COMMERCIAL'S               
         GOTO1 HIGH                READ SEQUENCE                                
         B     VRLST10                                                          
*                                                                               
VRLSTX   MVC   AIO,AIO1                                                         
         B     XIT                                                              
*                                                                               
SVGCKEY  DC    XL(L'KEY)'0'                                                     
         EJECT                                                                  
* VALIDATE RERECORD DATE (OPTIONAL).                                            
*                                                                               
VRRER    NTR1                                                                   
         L     R2,ARERFLD          R2 = A(RERECORD DATE FIELD)                  
******** GOTOR VALWEB,DMCB,(0,0)                                                
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
                                                                                
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         GOTO1 DTVAL,DMCB,(X'80',TACARERC)  VALIDATE IF INPUT                   
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE EXPIRATION DATE (OPTIONAL).                                          
*                                                                               
VREXP    NTR1                                                                   
         L     R2,AEXPFLD          R2 = A(EXPIRATION DATE FIELD)                
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R1,AFRSTFLD         R1 = A(FIRST SERVICES DATE FIELD)            
*                                                                               
         TM    4(R1),X'20'         IF FIRST SERVICES DATE CHANGED               
         BZ    VREXPX              IGNORE INPUT TO EXPIRY DATE FIELD            
*                                                                               
         GOTO1 DTVAL,DMCB,(X'80',TACAEXP)  VALIDATE IF INPUT                    
*                                                                               
         CLI   5(R2),0             INPUT NOT ALLOWED FOR SEASONALS              
         BE    VREXPX                                                           
         CLI   CMCLTYP,CTYSEAS2                                                 
         BE    DATSEA                                                           
VREXPX   B     XIT                                                              
         EJECT                                                                  
* VALIDATE STATUS CODES AND SET CORRESPONDING BITS IN EITHER AGENCY OR          
* BILLING RULES STATUS BYTE.  ALSO VALIDATE OVERRIDE EMPLOYER OF RECORD         
* IF ONE IS GIVEN.                                                              
*                                                                               
VRSTAT   NTR1                                                                   
         L     R4,AIO                                                           
         USING TLCAD,R4                                                         
         NI    TLCASTAT,X'DF'                                                   
*                                                                               
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
         L     R2,ASTATFLD         R2 = A(STATUS FIELD)                         
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
*                                                                               
         MVI   BYTE,0              PRE-CLEAR NO HOLD FEE NOTICE FLAG            
         MVC   SVCASTAT,TACASTAT                                                
*                                  IF FIELD IS EMPTY THEN CLEAR BITS            
         XC    TACASTAT(2),TACASTAT                                             
         CLI   5(R2),0                                                          
         BE    VRS100                                                           
*                                  ELSE SCAN FIELD INTO SCANNER BLOCK           
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         CLI   4(R1),0                                                          
         BE    ERRINV                                                           
*                                                                               
         LA    R3,BLOCK            R3 = A(SCANNER BLOCK)                        
         USING SCAND,R3                                                         
         ZIC   RF,4(R1)            FULL = NUMBER OF SCAN BLOCK ENTRIES          
         ST    RF,FULL                                                          
         SR    R0,R0               R0 = DISPLACEMENT INTO FIELD                 
*                                                                               
VRS10    CLI   SCLEN2,0            ERROR IF RHS EXISTS                          
         BNE   ERRFLD                                                           
         L     RF,ASTATTAB         LOOK IN STATUS TABLE FOR MATCH               
*                                                                               
VRS20    CLI   0(RF),X'FF'         ERROR IF END OF TABLE FOUND                  
         BE    ERRFLD                                                           
*                                                                               
         ZIC   RE,SCLEN1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),=CL3'NOP'                                             
         BNE   *+14                                                             
         MVI   SCLEN1,8                                                         
         MVC   SCDATA1,=CL10'NOAPPPAX'                                          
*                                                                               
         ZIC   RE,SCLEN1           IF MATCH THEN SET BIT                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),3(RF)                                                 
         BE    VRS30                                                            
*                                                                               
         LA    RF,L'STATTAB(RF)    ELSE TRY NEXT TABLE ENTRY                    
         B     VRS20                                                            
*                                                                               
VRS30    OC    0(2,RF),0(RF)                                                    
         JZ    VRS40                                                            
         OC    TACASTAT(2),0(RF)   SET APPROPRIATE BIT IN APPR BYTE             
*                                                                               
*                                                                               
         CLI   HASLIFT,C'N'        IF COMMERCIAL HAS NO LIFT                    
         BNE   *+12                AND A LIFT STATUS BIT IS SET                 
         TM    TACASTAT,TACASTLO+TACASTLF                                       
         BNZ   ERRFLD              THEN ERROR                                   
*                                                                               
         OC    0(2,RF),0(RF)       IF NO BITS SET IN THIS TABLE ENTRY           
         BNZ   *+8                                                              
         MVI   BYTE,1              THEN SET NO HOLD FEE NOTICE FLAG             
         B     VRS90                                                            
*                                                                               
VRS40    OC    TACASTA3,2(RF)                                                   
*                                                                               
VRS90    ZIC   RF,SCLEN1           BUMP R0 TO NEXT STATUS CODE                  
         LA    RF,1(RF)                                                         
         AR    R0,RF                                                            
*                                                                               
         LA    R3,SCANNEXT         BUMP R3 TO NEXT SCANNER ENTRY                
*                                                                               
         L     RF,FULL             REPEAT UNTIL NO MORE SCANNER FIELDS          
         BCTR  RF,0                                                             
         ST    RF,FULL                                                          
         LTR   RF,RF                                                            
         BNZ   VRS10                                                            
         DROP  R3                                                               
*                                                                               
VRS100   CLC   =C'VS',SCAWID       IF WEB APPLICATION ID CONTAINS               
         BE    VRS105              A VITA SESSION ID                            
         CLC   =C'TS',SCAWID                                                    
         BE    VRS105                                                           
         CLC   =C'RS',SCAWID       OR RADIO SESSION ID                          
         BE    VRS105                                                           
         CLC   =C'VC',SCAWID       OR VITA COMPLETION ID                        
         BE    VRS105                                                           
         CLC   =C'TC',SCAWID                                                    
         BE    VRS105                                                           
         CLC   =C'RC',SCAWID       OR RADIO SESSION ID ...                      
         BNE   VRS120                                                           
VRS105   TM    SCAWIDH+1,X'20'                                                  
         BO    VRS120                                                           
                                                                                
         TM    TACASTAT,TACASXAC   ENSURE THAT "DON'T APPLY FIXED               
         BZ    VRS110              CYCLE GUARANTEE AGAINST CABLE"               
         TM    SVCASTAT,TACASXAC   STATUS WAS NOT CHANGED                       
         BZ    WEBERR                                                           
         B     VRS120                                                           
                                                                                
VRS110   TM    SVCASTAT,TACASXAC                                                
         BO    WEBERR                                                           
                                                                                
VRS120   TM    TACASTAT,TACASTLO   IF LIFT ONLY BIT SET                         
         BZ    *+8                                                              
         OI    TACASTAT,TACASTLF   THEN SET ON LIFT BIT                         
                                                                                
         NI    TACASTA3,X'FF'-TACASEQY      EQUITY PERFORMER?                   
         L     R2,AEQTYFLD          R2 = A(EQUITYFIELD)                         
         CLI   SCAEQTY,C'N'                                                     
         BE    VRS126                                                           
         CLI   SCAEQTY,C'Y'                                                     
         BNE   ERRFLD                                                           
         OI    TACASTA3,TACASEQY                                                
*                                                                               
VRS126   TM    TACASTAT,TACASTLF   IF LIFT BIT ON                               
         BZ    VRS130                                                           
         L     R4,AIO                                                           
         USING TLCAD,R4                                                         
         OI    TLCASTAT,X'20'      SET BIT IN STATUS BYTE                       
*                                                                               
VRS130   L     R4,AIO              R4 = A(HOLDING FEE NOTICE ELEMENT)           
         MVI   ELCODE,TAHFELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAHFD,R4                                                         
*                                                                               
         OI    TAHFSTAT,TAHFSNO    IF FLAG IS SET THEN SET NO HLD BIT           
         CLI   BYTE,1                                                           
         BE    *+8                 ELSE CLEAR NO HLD BIT                        
         NI    TAHFSTAT,X'FF'-TAHFSNO                                           
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE BUMPS R2 TO THE NEXT SCREEN FIELD.                               
*                                                                               
BUMP     ZIC   R0,0(R2)            BUMP R2 TO NEXT FIELD                        
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
ERRNOMUS MVC   MYMSGNO,=Y(ERMUS487) CANNOT ADD MUSICIANS                        
         B     EXTEXIT                                                          
*                                                                               
ERRNOADD MVC   MYMSGNO,=Y(ERRNAPCY) CANNOT ADD TO PER CYCLE COMMERCIAL          
         B     EXTEXIT                                                          
*                                                                               
ERRNODEL MVC   MYMSGNO,=Y(ERRNDPCY) CANNOT DELETE FROM PER CYCLE COMML          
         B     EXTEXIT                                                          
*                                                                               
WEBERR   MVC   MYMSGNO,=Y(ERUSEWEB) RECORD MUST BE UPDATED FROM                 
         B     EXTEXIT              WEB APPLICATION                             
*                                                                               
ERRNOCHK MVC   MYMSGNO,=Y(ERCSTCHK) CAN'T DELETE IF CHECK ATTACHED              
         B     EXTEXIT                                                          
*                                                                               
ERRNOTIM MVC   MYMSGNO,=Y(ERCSTTIM) CAN'T DELETE IF TIMESHEETS ATTACHED         
         B     EXTEXIT                                                          
*                                                                               
ERRNOTRK MVC   MYMSGNO,=Y(ERCSTTRK) CAN'T DELETE IF TRACK ATTACHED              
         B     EXTEXIT                                                          
*                                                                               
ERCRPLK  MVC   MYMSGNO,=Y(ERCPSLCK) CORP RECORD LOCKED - NON-PAY                
         B     EXTEXIT                                                          
*                                                                               
ERRRNF   MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     ERREXIT                                                          
*                                                                               
ERRRECTY MVI   ERROR,ERRECCTY      WRONG COMML TYPE FOR THIS SCREEN             
         B     RECEXIT                                                          
*                                                                               
ERRNOIP  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     ERREXIT                                                          
*                                                                               
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT                                
         B     ERREXIT                                                          
*                                                                               
ERRGUAR  MVI   ERROR,ERGRTLCK      GUARANTEE RECORD IS LOCKED                   
         B     ERREXIT                                                          
*                                                                               
ERREXT   MVI   ERROR,INVALID       USER CAN'T ENTER EXTENSION SCREEN            
         B     RECEXIT                                                          
*                                                                               
ERRNOCA  MVI   ERROR,ERNOCAST      NO CAST MEMBERS ON COMMERCIAL                
         B     RECEXIT                                                          
*                                                                               
ERRTAX   MVC   MYMSGNO,=Y(ERTAXST) LOCAL DOESN'T MATCH TAX STATE                
         B     EXTEXIT                                                          
*                                                                               
ERRPFK   MVI   ERROR,ERINVPFK      INVALID PFKEY PRESSED                        
         LA    R2,SCLL1H                                                        
         B     ERREXIT                                                          
*                                                                               
ERRDUPP  MVI   ERROR,ERPF19DP      HIT PF19 TO ADD DUPLICATE                    
         B     ERREXIT                                                          
*                                                                               
ERRFTRK  MVI   ERROR,ERPF19FT      WARNING - CAST ALREADY HAS FTRACK            
         B     ERREXIT             HIT PF19 TO ADD                              
*                                                                               
DELLSTER MVI   ERROR,ERDELLST      DELETE NOT ALLOWED FROM LIST                 
         B     ERREXIT                                                          
*                                                                               
ERRCRP   MVI   ERROR,ERGT1CRP      GT 1 CORP - NEED EXACT CODE                  
         B     ERREXIT                                                          
*                                                                               
ERRLOCK  MVI   ERROR,ERAGYLCK      AGENCY IS LOCKED                             
         LA    R2,SCAAGYH                                                       
         B     ERREXIT                                                          
*                                                                               
ERRFLD   MVI   ERROR,INVALID       INVALID INPUT FIELD IN MIDDLE                
         STC   R0,ERRDISP                                                       
         B     ERREXIT                                                          
*                                                                               
CTYPERR  MVI   ERROR,ERUSECTY      INVALID TYPE FOR USE IN MIDDLE               
         STC   R0,ERRDISP          OF FIELD                                     
         B     ERREXIT                                                          
*                                                                               
MEDERR   MVI   ERROR,ERUSEMED      INVALID MEDIA FOR USE IN MIDDLE              
         STC   R0,ERRDISP          OF FIELD                                     
         B     ERREXIT                                                          
*                                                                               
ERW4LCK  MVI   ERROR,ERW4SLCK      W4 RECORD LOCKED                             
         B     ERREXIT                                                          
*                                                                               
ERNOTRS  MVC   MYMSGNO,=Y(ERNOTRST) TRUSTEE NOT ALLOWED ON CAST                 
         B     EXTEXIT                                                          
*                                                                               
DATSEA   MVC   MYMSGNO,=Y(ERDATSEA)                                             
         B     EXTEXIT                                                          
*                                                                               
ERRPRIRL MVC   MYMSGNO,=Y(ERRLSPRI) CANNOT LAST SERVICE PRI.COMM'L              
         B     EXTEXIT                                                          
*                                                                               
ERM489   MVC   MYMSGNO,=Y(ERMUS489) CANNOT ADD NON-MUSICIAN                     
         B     EXTEXIT                                                          
*                                                                               
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     EXTEXIT                                                          
*                                                                               
ERW4REG  MVC   MYMSGNO,=Y(536)        W4 IS FOR REGRESSION TESTING              
         J     EXTEXIT                                                          
*                                                                               
ERW4NREG MVC   MYMSGNO,=Y(537)        W4 IS NOT FOR REGRESSION TESTING          
         J     EXTEXIT                                                          
                                                                                
EXTEXIT  MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     ERREXIT                                                          
*                                                                               
RECEXIT  LA    R2,CONRECH                                                       
ERREXIT  GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
XFFS     DC    16X'FF'                                                          
SSNSPACE DC    CL9' '                                                           
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
         XC    NUMLIFT,NUMLIFT     NUMBER OF CAST MEMBERS ON LIFT               
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
*                                  WHILE SAME COMM'L                            
BL20     CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BNE   BL100                                                            
*                                                                               
         USING TLCAD,R4                                                         
         CLC   TLCASEQ,=H'1250'    DO NOT CONSIDER VITA-ADDED CAST              
         JNL   BL21                WHEN FIGURING NEXT SEQUENCE NUMBER           
         SR    RF,RF               IF SORT KEY SEQUENCE NUM > HIGHEST           
         ICM   RF,3,TLCASEQ                                                     
*                                                                               
BL21     CLI   ACTNUM,ACTVER       IF ACTION VERIFY                             
         BNE   BL27                                                             
         TM    TLCASORT,TLCASRMQ   IF NON MUSICIAN                              
         BO    BL25                                                             
         TM    CVERSTA2,CVERSOMS   & ONLY WANT MUSICIANS                        
         BO    BL90                IGNORE RECORD                                
         B     BL27                                                             
*                                                                               
BL25     TM    CVERSTA2,CVERSNMS   IF MUS & ONLY WANT NON-MUSICIANS             
         BO    BL90                IGNORE RECORD                                
*                                                                               
         USING TLDRD,R4                                                         
BL27     CLI   ACTNUM,ACTLDEL      IF USES WANTS DELETED RECORDS                
         BNE   BL30                                                             
         TM    TLDRSTAT,X'40'      THEN SKIP RECORDS THAT AREN'T                
         BZ    BL90                                                             
         BAS   RE,CHKTATR          AND SKIP RECORDS WITH TRACK                  
         BE    BL90                ASSOCIATIONS                                 
         B     BL40                                                             
*                                                                               
BL30     TM    TLDRSTAT,X'80'      ELSE SKIP RECORDS THAT ARE                   
         BO    BL90                                                             
*                                                                               
BL40     CLI   VERCODE,0           IF WANT CAST FOR A VERSION OF COMML          
         BE    BL45                                                             
         BAS   RE,CHKVERS          CHECK VERSION CODE ON CAST RECORD            
         BNE   BL90                                                             
*                                                                               
BL45     TM    OPTS,OPTREL         IF RELEASED CAST OPTION NOT SET,             
         BO    BL46                                                             
         BRAS  RE,FILTREL          FILTER OUT RELEASE CAST                      
         BE    BL90                IF RELEASED, SKIP                            
*                                                                               
BL46     TM    OPTS,OPTPRN         IF PRINCPAL CAST OPTION SET                  
         BZ    BL48                                                             
         BRAS  RE,FILTEXT          FILTER OUT EXTRAS                            
         BE    BL90                IF EXTRA, SKIP                               
*                                                                               
BL48     CLI   ACTNUM,ACTVER       IF NOT VERIFYING                             
         BE    BL49                                                             
         TM    OPTS,OPTLSC         AND NOT INCLUDING LAST SERVICED              
         BO    BL49                PERFORMERS                                   
         BRAS  RE,FILTLSC          SKIP LAST SERVICED PERFORMERS                
         BE    BL90                                                             
*                                                                               
BL49     TM    OPTS,OPTEQY         IF ONLY WANT EQY PERFORMERS LISTED           
         BNO   *+12                                                             
         BRAS  RE,FILTEQY                                                       
         BNE   BL90                                                             
*                                                                               
         OC    FULL,FULL           IF CALLER WANTS DISK ADDRESS LIST            
         BZ    *+10                                                             
         MVC   0(4,R3),TLDRDA      THEN SAVE DISK ADDRESS                       
*                                                                               
         TM    TLDRSTAT,TLCASLFT   IF CAST MEMBER IS ON LIFT                    
         BNO   *+16                                                             
         LH    RF,NUMLIFT          THEN INCREMENT NUM OF CAST ON LIFT           
         LA    RF,1(RF)                                                         
         STH   RF,NUMLIFT                                                       
*                                                                               
         USING TLCAD,R4                                                         
         GOTO1 CATVAL,DMCB,TLCACAT                                              
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
BL50     MVC   AIO,AIO3            IF MUSICIAN, GET CAST RECORD                 
         GOTO1 GETREC              INTO AIO3                                    
         MVC   AIO,AIO1                                                         
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
******** OC    TACALAST,TACALAST   IF MUSICIAN IS NOT LAST SERVICED             
******** BNZ   BL60                                                             
         OI    CVERSTAT,CVERSMUS   SET VERIFY STATUS MUSICIAN BIT               
         DROP  R4                                                               
*                                                                               
BL60     AH    R0,=H'1'            INCREMENT NUMBER OF ITEMS                    
         LA    R4,KEY              AND RESET KEY POINTER                        
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
         B     BLX                                                              
*                                                                               
BLYES    SR    RC,RC                                                            
BLNO     LTR   RC,RC                                                            
BLX      XIT1                                                                   
*                                                                               
CHKTATR  NTR1                                                                   
         L     R0,FULL             SAVE OFF FULL                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         GOTOR HASTRKAS,DMCB,AIO2                                               
         ST    R0,FULL             RESTORE FULL                                 
         MVC   AIO,AIO1                                                         
         BE    BLYES                                                            
         B     BLNO                                                             
                                                                                
CHKVERS  NTR1                                                                   
         L     R0,FULL             SAVE OFF FULL                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         MVC   SVKEY,KEY                                                        
         BRAS  RE,TRNTATR                                                       
         BNE   CHKVER1                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
                                                                                
CHKVER1  MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         ST    R0,FULL             RESTORE FULL                                 
         MVC   AIO,AIO1                                                         
         BNE   BLNO                                                             
                                                                                
         L     R4,TGELEM           VERSION CODE MUST BE ON CAST RECORD          
         USING TAFND,R4                                                         
         ZIC   R0,TAFNLEN                                                       
         SH    R0,=Y(TAFNLNQ)                                                   
         LA    R1,TAFNNAME                                                      
                                                                                
CHKVER5  CLC   VERCODE,0(R1)                                                    
         BE    BLYES                                                            
         CLI   0(R1),251                                                        
         BE    BLYES                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,CHKVER5                                                       
         B     BLNO                                                             
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* BUILD THE FIRST FOUR BYTES OF THE SORT KEY FROM THE RECORD DATA.              
*                                                                               
BLDSORT  NMOD1 0,**BLDS**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R3,AIO              R3 = A(CAST RECORD)                          
         USING TLCAD,R3                                                         
*                                  PRE-CLEAR FIRST 2 BYTES OF SORT KEY          
         XC    TLCASORT(2),TLCASORT                                             
         CLI   VALMODE,C'A'        IF ADDING CAST                               
         BNE   *+10                                                             
         XC    TLCASRT,TLCASRT     PRE-CLEAR FIRST 4 BYTES OF SORT KEY          
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
*        MVI   ACCESSED,C'G'       SET ACCESSED TO SOME FIELDS                  
         MVI   ACCESSED,C'N'       CLIENTS NO LONGER HAVE CHANGE                
         CLI   TGCTSTTY,TASTTYPC   ACCESS TO ANY FIELDS                         
         BE    TF20                                                             
         CLI   TGCTSTTY,TASTTYPF                                                
         BE    TF20                                                             
         CLI   TGCTSTTY,TASTTYPD                                                
         BE    TF20                                                             
*                                                                               
         MVI   ACCESSED,C'Y'       ELSE, SET ACCESSED TO EVERYTHING             
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
         CLI   0(RF),SCR0A                                                      
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
         CLI   ACTNUM,ACTVER       OR IF NOT COMING FROM CAST VERIFY            
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
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF STATUS CODES AND THERE DISPLAYABLE FORM.                             
*                                                                               
STATTAB  DS    0CL13                                                            
         DC    AL1(TACASTLF,0,0),CL10'LIFT'                                     
         DC    AL1(TACASTLO,0,0),CL10'LFONLY'                                   
         DC    AL1(TACASTAG,0,0),CL10'CHAGT'                                    
         DC    AL1(TACASXAC,0,0),CL10'NOAPPCBL'                                 
         DC    AL1(TACASXAP,0,0),CL10'NOAPPPAX'                                 
         DC    AL1(TACASTNF,0,0),CL10'NOFTRK'                                   
         DC    AL1(0,TACASTCR,0),CL10'CRATE'                                    
         DC    AL1(0,TACASTDP,0),CL10'NOCRATE'                                  
         DC    AL1(0,TACASCLB,0),CL10'CELEBRITY'                                
         DC    AL1(0,TACASFGR,0),CL10'NOFGR'                                    
         DC    AL1(0,TACASINR,0),CL10'NOINA'                                    
         DC    AL1(0,TACASINA,0),CL10'NOINR'                                    
*****    DC    AL1(0,0,0),CL10'NOHFN'                                           
         DC    AL1(0,0,TACASXFT),CL10'NOAHF'                                    
         DC    X'FF'                                                            
         SPACE 3                                                                
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
         DC    AL1(LPF15X-*,15,0,0,0)                                           
         DC    CL3'   ',CL8'CAST    ',CL8'VERIFY '                              
LPF15X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF16DX-*,16,0,(LPF16DX-LPF16D)/KEYLNQ,0)                    
         DC    CL3'FT ',CL8'FTRACK  ',CL8'REPORT'                               
LPF16D   DC    AL1(KEYTYCUR,L'SCLSSN-1),AL2(SCLSSN-SCLSSN)                      
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYCUR,L'SCLCAT-1),AL2(SCLCAT-SCLSSN)                      
LPF16DX  EQU   *                                                                
*                                                                               
         DC    AL1(LPF17X-*,17,0,(LPF17X-LPF17)/KEYLNQ,0)                       
         DC    CL3'FT ',CL8'FTRACK  ',CL8'LIST'                                 
LPF17    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
LPF17X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF18DX-*,18,0,(LPF18DX-LPF18D)/KEYLNQ,0)                    
         DC    CL3'CE ',CL8'CERROR  ',CL8'DISPLAY'                              
LPF18D   DC    AL1(KEYTYCUR,L'SCLSSN-1),AL2(SCLSSN-SCLSSN)                      
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYCUR,L'SCLCAT-1),AL2(SCLCAT-SCLSSN)                      
LPF18DX  EQU   *                                                                
*                                                                               
         DC    AL1(LPF19X-*,19,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        ',CL8'       '                              
LPF19X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF20X-*,20,PFTINT+PFTCPROG,(LPF20X-LPF20)/KEYLNQ)           
         DC    AL1(PFTVCAST)                                                    
         DC    CL3'V  ',CL8'        ',CL8'DISPLAY'                              
LPF20    DC    AL1(KEYTYCOM,0),AL2(0)                                           
LPF20X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF21X-*,21,PFTINT,(LPF21X-LPF21)/KEYLNQ,0)                  
         DC    CL3'   ',CL8'PCAST   ',CL8'LIST   '                              
LPF21    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
LPF21X   EQU   *                                                                
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
         DC    AL1(DPF15X-*,15,0,0,0)                                           
         DC    CL3'   ',CL8'CAST    ',CL8'VERIFY '                              
DPF15X   EQU   *                                                                
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
         DC    AL1(LPF14DX-*,14,0,(LPF14DX-LPF14D)/KEYLNQ,0)                    
         DC    CL3'FT ',CL8'FTRACK  ',CL8'REPORT'                               
LPF14D   DC    AL1(KEYTYTWA,L'SCASSN-1),AL2(SCASSN-T702FFD)                     
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYTWA,3-1),AL2(SCASORT+10-T702FFD)                        
LPF14DX  EQU   *                                                                
*                                                                               
         DC    AL1(EPF15X-*,15,0,(EPF15X-EPF15)/KEYLNQ,0)                       
         DC    CL3'FT ',CL8'FTRACK  ',CL8'LIST'                                 
EPF15    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
EPF15X   EQU   *                                                                
*                                                                               
         DC    AL1(EPF16X-*,16,0,(EPF16X-EPF16)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'CERROR  ',CL8'DISPLAY'                              
EPF16    DC    AL1(KEYTYTWA,L'SCASSN-1),AL2(SCASSN-T702FFD)                     
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYTWA,3-1),AL2(SCASORT+10-T702FFD)                        
EPF16X   EQU   *                                                                
*                                                                               
         DC    AL1(EPF17X-*,17,0,(EPF17X-EPF17)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'USAGE   ',CL8'DISPLAY'                              
EPF17    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYTWA,L'SCASSN-1),AL2(SCASSN-T702FFD)                     
         DC    AL1(KEYTYTWA,3-1),AL2(SCASORT+10-T702FFD)                        
EPF17X   EQU   *                                                                
*                                                                               
         DC    AL1(EPF18X-*,18,0,(EPF18X-EPF18)/KEYLNQ,0)                       
         DC    CL3'   ',CL8'VCAST   ',CL8'CHANGE'                               
EPF18    DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYTWA,L'SCASSN-1),AL2(SCASSN-T702FFD)                     
         DC    AL1(KEYTYTWA,L'SCACAT-1),AL2(SCACAT-T702FFD)                     
         DC    AL1(KEYTYTWA,L'SCASORT-1),AL2(SCASORT-T702FFD)                   
EPF18X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
LINATAB  DS    0H                  DISPLACEMENT TO LIST LINES 1-8               
         DC    AL2(SCLL1H-T702FFD)                                              
         DC    AL2(SCLL2H-T702FFD)                                              
         DC    AL2(SCLL3H-T702FFD)                                              
         DC    AL2(SCLL4H-T702FFD)                                              
         DC    AL2(SCLL5H-T702FFD)                                              
         DC    AL2(SCLL6H-T702FFD)                                              
         DC    AL2(SCLL7H-T702FFD)                                              
         DC    AL2(SCLL8H-T702FFD)                                              
         DC    AL2(0)                                                           
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,33,C'CAST LIST'                                               
         SSPEC H2,33,C'---------'                                               
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
* VALIDATE TRACKS                                                               
VRTRKS   NTR1  BASE=*,LABEL=*                                                   
         GOTOR HASTRKAS,DMCB,AIO   IF CAST RECORD IS LINKED TO AFM TRK          
         JE    VRTRKSX             DO NOT ATTEMPT TO BUILD ELEMENT              
                                                                                
         L     R2,ATRKSFLD                                                      
         XC    ORIGTRKS,ORIGTRKS                                                
         MVI   ELCODE,TAFNELQ                                                   
                                                                                
         TM    TGSYSTAT,TASYSMUS   IF NEW MUSIC RULES ENABLED                   
         JZ    VRTRKS20                                                         
         CLI   CMCLTYP,CTYMUS      AND COMMERICAL TYPE IS MUSIC                 
         JNE   VRTRKS20                                                         
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTTRK)) AND MUSICIAN ALREADY                
         JE    VRTRKS00                     ATTACHED TO TRACKS                  
         CLI   SCRFLAG,C'L'        OR IF ON LIST SCREEN                         
         JNE   VRTRKS20                                                         
         CLI   VALMODE,C'A'        AND ADDING A MUSICIAN                        
         JNE   VRTRKS20                                                         
VRTRKS00 CLI   5(R2),0             INPUT IS REQUIRED                            
         JE    FLDMISS                                                          
                                                                                
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTTRK))                                     
         JNE   VRTRKS30            IF MUSICIAN ALREADY EXISTED ...              
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         CLI   TAFNNAME,C'*'       SAVE CAST'S ORIGINAL TRACK SETTINGS          
         JNE   VRTRKS10                                                         
         MVC   ORIGTRKS,TRACKS                                                  
         J     VRTRKS20                                                         
                                                                                
VRTRKS10 ZIC   RE,TAFNLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ORIGTRKS(0),TAFNNAME                                             
         DROP  R4                                                               
                                                                                
VRTRKS20 GOTO1 DELL,DMCB,(1,=AL1(TAFNTTRK))                                     
                                                                                
         OC    TRACKS,TRACKS       IF NO TRACKS FOR COMMERCIAL                  
         JNZ   VRTRKS30                                                         
         CLI   5(R2),0                                                          
         JNE   ERRNOIP             ERROR IF TRACKS INPUT                        
         J     VRTRKSX                                                          
                                                                                
VRTRKS30 ZIC   R0,5(R2)            R0=N'TRACKS INPUT                            
         LTR   R0,R0                                                            
         JZ    VRTRKSX                                                          
         CLI   TGUNEQU,AFM         MUST BE ZERO IF NON-MUSICIAN                 
         JNE   ERRNOIP                                                          
                                                                                
         CLI   8(R2),C'*'          IF INPUT IS * (ALL TRACKS)                   
         JNE   *+12                                                             
         CHI   R0,1                * MUST BE ONLY THING INPUT                   
         JE    VRTRKS90            SAVE IT                                      
                                                                                
         LA    R3,8(R2)            R3=A(TRACK WE'RE VALIDATING NOW)             
VRTRKS40 LA    R4,TRACKS           R4=A(TRACKS ON COMM'L)                       
                                                                                
VRTRKS50 CLI   0(R4),0                                                          
         JE    ERM481                                                           
         CLC   0(1,R4),0(R3)       ENSURE THAT TRACK EXISTS ON                  
         JE    VRTRKS60            COMMERCIAL                                   
         LA    R4,1(R4)                                                         
         J     VRTRKS50                                                         
                                                                                
VRTRKS60 LA    R1,8(R2)                                                         
VRTRKS70 CR    R1,R3               ENSURE THAT TRACK ISN'T DUPLICATED           
         JE    VRTRKS80                                                         
         CLC   0(1,R1),0(R3)                                                    
         JE    VRTRKSER                                                         
         LA    R1,1(R1)                                                         
         J     VRTRKS70                                                         
                                                                                
VRTRKS80 LA    R3,1(R3)            BUMP TO NEXT TRACK INPUT                     
         BCT   R0,VRTRKS40                                                      
                                                                                
VRTRKS90 GOTO1 NAMIN,DMCB,TAFNELQ,(R2),TAFNTTRK                                 
VRTRKSX  OI    4(R2),X'20'                                                      
         J     XIT                                                              
                                                                                
ERM481   MVC   MYMSGNO,=Y(ERMUS481) TRACK NUMBER MISSING FROM COM2              
         LA    R1,8(R2)            FIND DISPLACEMENT INTO FLD FOR ERROR         
         SR    R3,R1                                                            
         STC   R3,ERRDISP          SAVE IN ERRDISP                              
         J     EXTEXIT                                                          
                                                                                
VRTRKSER LA    R1,8(R2)            FIND DISPLACEMENT INTO FLD FOR ERROR         
         SR    R3,R1                                                            
         STC   R3,ERRDISP          SAVE IN ERRDISP                              
         J     ERRINV              GIVE ERROR                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* SET ADDRESSES OF FIELDS ON CURRENT DISPLAY LINE.                              
*                                                                               
LSETADDR NTR1  BASE=*,LABEL=*                                                   
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
         ST    R2,ALIFTFLD         SAVE A(LIFT FIELD)                           
         BAS   RE,BUMP                                                          
         ST    R2,ACORPFLD         SAVE A(CORPORATION FIELD)                    
         BAS   RE,BUMP                                                          
         ST    R2,AOPFLD           SAVE A(OVERSCALE PERCENTAGE FIELD)           
         BAS   RE,BUMP                                                          
         ST    R2,AGUARFLD         SAVE A(GUARANTEE FIELD)                      
         BAS   RE,BUMP                                                          
         ST    R2,ADBLFLD          SAVE A(DOUBLES FIELD)                        
         BAS   RE,BUMP                                                          
         ST    R2,AFRSTFLD         SAVE A(FIRST SERVICES FIELD)                 
         BAS   RE,BUMP                                                          
         ST    R2,ATRKSFLD         SAVE A(TRACKS FIELD)                         
         ST    R2,ALASTUNP         SAVE A(LAST UNPROTECTED FLD OF LINE)         
         BAS   RE,BUMP                                                          
         ST    R2,ANAMEFLD         SAVE A(NAMES FIELD)                          
         BAS   RE,BUMP                                                          
         ST    R2,AWIDFLD          SAVE A(WEB APPLICATION ID FIELD)             
         BAS   RE,BUMP                                                          
         ST    R2,ANEXTLIN         SAVE A(NEXT SCREEN LINE)                     
*                                                                               
LSAX     J     XIT                                                              
         EJECT                                                                  
* VALIDATE CONTRACT YEAR FIELD (DEFAULT TO ABOVE).                              
                                                                                
VRYEAR   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AYEARFLD         R2 = A(CONTRACT YEAR FIELD)                  
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
                                                                                
         GOTOR COPYDOWN,DMCB,SVAYEAR  COPY FLD FROM ABOVE IF NECCESSARY         
                                                                                
         BRAS  RE,MYANY            MOVE AND PAD SCREEN FIELD TO WORK            
                                                                                
         GOTO1 YRVAL,DMCB,WORK     VALIDATE YEAR                                
         JNE   ERRINV                                                           
                                                                                
         LA    R3,TGUNYR           R3 = A(VALID UNION YEARS)                    
                                                                                
VRYEAR10 CLI   0(R3),0             IF END OF TABLE THEN ERROR                   
         JE    ERRINV                                                           
         CLC   0(1,R3),TGYREQU     IF MATCH THEN FOUND                          
         JE    VRYEAR20                                                         
         LA    R3,1(R3)            ELSE TRY NEXT TABLE ENTRY                    
         J     VRYEAR10                                                         
                                                                                
VRYEAR20 CLC   =C'01',WORK         IF YEAR IS 2001                              
         JNE   VRYEAR40                                                         
         CLC   TACAUN,=C'SAG'      AND UNION IS SAG                             
         JE    VRYEAR30                                                         
         CLC   TACAUN,=C'AFT'      OR UNION IS AFTRA                            
         JNE   VRYEAR40                                                         
VRYEAR30 CLI   CMCLTYP,CTYIND      COMMERCIAL TYPE MUST BE INDUSTRIAL           
         JNE   ERRINV                                                           
                                                                                
VRYEAR40 CLC   WORK(2),=C'13'      IF YEAR IS 2013 OR GREATER                   
         BL    VRYEAR45                                                         
         CLI   TGCAEQU,CTGD6         GD6 AND GD9 ARE NO LONGER VALID            
         JE    ERRINV                                                           
         CLI   TGCAEQU,CTGD9                                                    
         JE    ERRINV                                                           
         B     VRYEAR50                                                         
VRYEAR45 CLI   TGCAEQU,CTGDP       IF YEAR IS < 2013 GD+ IS NOT VALID           
         JE    ERRINV                                                           
                                                                                
VRYEAR50 CLC   WORK(2),=C'16'      IF YEAR IS < 2016,                           
         BNL   VRYEAR60                                                         
         CLI   TGCAEQU,CTESB       ESB AND ESI ARE NOT VALID                    
         JE    ERRINV                                                           
         CLI   TGCAEQU,CTESI                                                    
         JE    ERRINV                                                           
                                                                                
VRYEAR60 CLC   WORK(2),=C'16'      IF YEAR IS 2016,                             
         BNE   VRYEAR70                                                         
         CLC   TACAUN,=C'ACT'      AND UNION IS ACTRA                           
         BNE   VRYEAR70                                                         
         LA    RE,COELEM                                                        
         USING TACOD,RE                                                         
         CLI   TACOCTYP,CCTY2404   ACTRA TYPE MUST BE 2404                      
         JNE   ERRINV                                                           
         DROP  RE                                                               
                                                                                
VRYEAR70 MVC   TACAYEAR,WORK       SAVE YEAR IN ELEMENT                         
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE TAX UNIT FIELD.                                                      
*                                                                               
VRUNIT   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AUNITFLD         R2 = A(TAX UNIT FIELD)                       
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
                                                                                
         GOTOR COPYDOWN,DMCB,SVAUNIT  COPY FLD FROM ABOVE IF NECCESSARY         
                                                                                
         BRAS  RE,MYANY            MOVE AND PAD SCREEN FIELD TO WORK            
                                                                                
         CLI   5(R2),2             MUST BE AT LEAST TWO CHARS                   
         JL    ERRINV                                                           
                                                                                
         ZIC   R3,5(R2)            VALIDATE TAX UNIT                            
         GOTO1 TAXVAL,DMCB,((R3),WORK)                                          
         JE    VRU10                                                            
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',WORK)                                         
         JNE   ERRINV                                                           
                                                                                
VRU10    TM    TGTASTAT,TASUINGF   ENSURE TAX UNIT IS VALID GOING               
         JO    ERRINV              FOWARD                                       
         CLC   WORK(2),=C'FD'      IF UNIT STARTS WITH 'FD'                     
         JE    ERRINV                                                           
         CLI   WORK+2,C'0'         OR ENDS WITH '0' THEN ERROR                  
         JE    ERRINV                                                           
                                                                                
         MVC   TACAUNIT,WORK       SAVE TAX UNIT IN ELEMENT                     
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE AGENT FIELD (OPTIONAL).                                              
                                                                                
VRNCDE   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ANCDEFLD         R2 = A(AGENT FIELD)                          
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
                                                                                
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
                                                                                
         XC    TACANCDE,TACANCDE   IF NOT ENTERED THEN ZEROS                    
         CLI   5(R2),0                                                          
         JE    XIT                                                              
*                                  VALIDATE AGENT CODE                          
         GOTO1 RECVAL,DMCB,TLANCDQ,(R2)                                         
         GOTO1 TRNSAGT,DMCB,(X'80',TGAGT),TACANCDE                              
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCULATES NEW EXPIRATION DATE                           
         SPACE 1                                                                
         USING TACAD,R4                                                         
CALCEXP  NTR1  BASE=*,LABEL=*                                                   
         USING TACOD,R2                                                         
         LA    R2,COELEM                                                        
         CLI   TACOCTYP,CCTY04A    IF ACTRA TYPE IS 2404A                       
         BE    CEXP00                                                           
         CLI   TACOCTYP,CCTY2404   OR 2404                                      
         BNE   CEXP40                                                           
CEXP00   CLC   TACAUN,=C'ACT'      AND (UNION IS ACTRA)                         
         BE    CEXP10                                                           
         CLC   TACALOCL,=C'CAN'    OR (LOCAL IS CAN)                            
         BE    CEXP10                                                           
         CLC   TACAUN,=C'AFM'      OR (UNION IS AFM                             
         BNE   CEXP40                                                           
         CLC   TACAUNIT(2),=C'CN'  AND TAX UNIT IS CANADIAN)                    
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'AB'  PERFORMER BELONGS ON CANADIAN                
         BE    CEXP10              INVOICE                                      
         CLC   TACAUNIT(2),=C'BC'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'MB'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'NB'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'NL'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'NS'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'NT'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'NU'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'ON'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'PE'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'QC'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'SK'                                               
         BE    CEXP10                                                           
         CLC   TACAUNIT(2),=C'YT'                                               
         BNE   CEXP40                                                           
CEXP10   OC    TACAFRST,TACAFRST   AND (FIRST SERVICES DATE IS                  
         BZ    CEXPX               POPULATED)                                   
         GOTO1 DATCON,DMCB,(1,TACAFRST),(8,WORK)                                
         MVC   WORK+8(6),=C'-(24M)'                                             
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         GOTO1 PERVAL,DMCB,(14,WORK),('PVIN1DYL',BLOCK)                         
         MVC   TACAEXP,PVALPEND                                                 
         OC    TACOAIR,TACOAIR                                                  
         BZ    CEXPX                                                            
         GOTO1 DATCON,DMCB,(1,TACOAIR),(8,WORK)                                 
         MVC   WORK+8(6),=C'-(18M)'                                             
         GOTO1 PERVAL,DMCB,(14,WORK),('PVIN1DYL',BLOCK)                         
         CLC   TACAEXP,PVALPEND                                                 
         BL    CEXPX                                                            
         MVC   TACAEXP,PVALPEND    CALCULATE EXPIRATION DATE                    
         B     CEXPX               BASED ON ACTRA RULES                         
         DROP  R2,R3                                                            
*                                                                               
CEXP40   OC    CMCLFCYC,CMCLFCYC   IF NO CMCL FCYC DATE                         
         BZ    CEXPX               THEN SET EXPIRATION DATE TO ZERO             
*                                                                               
*                                  ELSE CONVERT FRST SERV DATE TO DUB           
         GOTO1 DATCON,DMCB,(1,TACAFRST),(0,DUB)                                 
*                                  CONVERT CMCL FCYC DATE TO WORK               
         GOTO1 DATCON,DMCB,(1,CMCLFCYC),(0,WORK)                                
*                                                                               
         CLC   =C'ON',TACAONOF     IF ON CAMERA                                 
         BNE   CEXP50                                                           
*                                                                               
         CLC   TACAFRST,CMCLFCYC   THEN IF FRST <= CMCL FCYC                    
         BNH   CEXP100             THEN CALC EXP BASED ON FRST                  
*                                                                               
*                                  ELSE IF DIFFERENCE > 90 DAYS                 
         GOTO1 ADDAY,DMCB,WORK,WORK,F'90'                                       
         CLC   WORK,DUB                                                         
         BL    CEXP100             THEN CALC EXP BASED ON FRST                  
         B     CEXP110             ELSE CALC EXP BASED ON CMCL FCYC             
*                                                                               
*                                  FOR OFF CAMERA...                            
CEXP50   CLC   TACAFRST,CMCLFCYC   IF FRST <= CMCL FCYC                         
         BH    CEXP60                                                           
*                                  THEN IF DIFFERENCE < 90 DAYS                 
         GOTO1 ADDAY,DMCB,DUB,DUB,F'90'                                         
         CLC   DUB,WORK                                                         
         BH    CEXP110             THEN CALC EXP BASED ON CMCL FCYC             
         B     CEXP100             ELSE CALC EXP BASED ON FRST                  
*                                                                               
*                                  ELSE IF DIFFERENCE < 45 DAYS                 
CEXP60   GOTO1 ADDAY,DMCB,WORK,WORK,F'45'                                       
         CLC   WORK,DUB                                                         
         BH    CEXP110             THEN CALC EXP BASED ON CMCL FCYC             
*                                                                               
*                                  ELSE IF DIFFERENCE <= 90 DAYS                
         GOTO1 ADDAY,DMCB,WORK,WORK,F'45'                                       
         CLC   WORK,DUB                                                         
         BNL   CEXP120             THEN CALC EXP FROM FCYC + 3 MONTHS           
         B     CEXP100             ELSE CALC EXP BASED ON FRST                  
*                                                                               
*                                  CALC EXP DATE BASED ON FRST                  
CEXP100  GOTO1 EXPIRE,DMCB,COELEM,TACAFRST,TACAEXP                              
         B     CEXPX                                                            
*                                                                               
CEXP110  MVC   TACAEXP,CMCLEXP     CALC EXP DATE BASED ON CMCL FCYC             
         B     CEXPX                                                            
*                                  CALC EXP FROM CMCL FCYC + 3 MONTHS           
CEXP120  GOTO1 DATCON,DMCB,(1,CMCLEXP),(5,WORK)                                 
         MVC   WORK+8(5),=C'-(3M)'                                              
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PERVAL,DMCB,(13,WORK),BLOCK                                      
         MVC   TACAEXP,PVALPEND                                                 
CEXPX    XIT1                                                                   
         DROP  R3,R4                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              PROCESS GRR "ONCE PER USE PER CYCLE" FIELD                       
*              ON ENTRY ... P1=A(USE CODE)                                      
*                                                                               
PRGRRFLD NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)                                                         
         CLC   0(3,R3),=C'GRR'     AND IF CODE IS 'GRR'                         
         BNE   PGRRFX                                                           
         GOTO1 FLDVAL,DMCB,SCAGRRHH,(X'10',SCAGRRFH)                            
         NI    SCAGRRFH+1,X'DF'    DISPLAY "ONCE PER USE PER CYCLE" FLD         
*                                                                               
         USING TACAD,R4                                                         
         L     R4,ACAELEM          R4 = A(CAST ELEMENT)                         
*                                                                               
         CLI   MODE,VALREC         IF MODE IS VALIDATE RECORD                   
         BNE   PGRRF10                                                          
         LA    R2,SCAGRRFH         STATUS MUST BE SET                           
         CLI   5(R2),0                                                          
         BE    PGRRMIS                                                          
         CLI   SCAGRRF,C'Y'                                                     
         BE    PGRRFX                                                           
         CLI   SCAGRRF,C'N'                                                     
         BNE   PGRRINV                                                          
         OI    TACASTA2,TACASPUS                                                
         B     PGRRFX                                                           
*                                                                               
PGRRF10  MVI   SCAGRRFH+5,1                                                     
         MVI   SCAGRRF,C'Y'        ELSE ...                                     
         TM    TACASTA2,TACASPUS   DISPLAY "ONCE PER USE PER CYCLE"             
         BZ    PGRRFX              STATUS                                       
         MVI   SCAGRRF,C'N'                                                     
PGRRFX   XIT1                                                                   
         DROP  R4                                                               
*                                                                               
PGRRINV  MVI   ERROR,INVALID       INVALID FIELD                                
         B     PGRREND                                                          
*                                                                               
PGRRMIS  MVI   ERROR,MISSING       MISSING FIELD                                
         B     PGRREND                                                          
*                                                                               
PGRREND  GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE NUMERIC VERSIONS AND RETURN                  
*              MAIN COMMERCIAL RECORD IN AIO                                    
*              ON ENTRY ... R2=A(VERSION FIELD)                                 
         SPACE 1                                                                
VNUMVER  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VALINUM           VALIDATE VERSION FIELD AS NUMERIC              
         MVC   WORK(1),ACTUAL                                                   
         SPACE 1                                                                
         CLI   WORK,26           IF VERSION CODE IS GREATER THAN                
         BNH   VNVX              26                                             
         SPACE 1                                                                
         USING VINDEXD,RE                                                       
         LA    RE,VERINDEX       FIND RECORD EQUATE FOR THIS                    
VNV10    CLI   0(RE),X'FF'       VERSION NUMBER                                 
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   WORK(1),VINDUPLM                                                 
         BNH   VNV20                                                            
         LA    RE,VINDLNQ(RE)                                                   
         B     VNV10                                                            
         SPACE 1                                                                
VNV20    XC    KEY,KEY              GET COMMERCIAL RECORD FOR                   
         L     R4,AIO                                                           
         MVC   KEY(L'TLCOKEY),0(R4) THAT VERSION                                
         MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),VINDEQUT                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   VNVINV                                                           
         GOTO1 GETREC                                                           
         DROP  RE                                                               
         SPACE 1                                                                
VNVX     XIT1                                                                   
         SPACE 2                                                                
VNVINV   MVI   ERROR,INVALID                                                    
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
*              TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE                   
*              VERSION CODE IS ON                                               
         SPACE 1                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SEE IF CAST WAS INITIALLY SET UP TO                   
*              RECEIVE HOLDING FEES. IF IT WAS AND A TRIGGER                    
*              FIELD WAS CHANGED, MUST ALWAYS SET TO GENERATE                   
*              NEW HOLDING FEE                                                  
         SPACE 1                                                                
TESTHLD  NTR1  BASE=*,LABEL=*                                                   
         CLI   MODE,RECDEL       ALWAYS EXECUTE ROUTINE IF CAST                 
         BE    THLD10            IS BEING DELETED                               
         TM    HLDCHANG,ONTLINE  ELSE, EXIT IF NO FIELDS THAT TRIGGER           
         BZ    THLDX             A NEW HOLDING FEE HAVE CHANGED                 
         SPACE 1                                                                
THLD10   LA    R6,TWAHOLE                                                       
         SPACE 1                                                                
         LA    RE,PPBLOCK                                                       
THLD20   CLI   0(RE),0           FIND ORIGINAL HOLDING FEE POINTER              
         BE    THLDX             IN SAVED POINTER BLOCK                         
         CLI   0(RE),TLCAHCDQ                                                   
         BE    THLD30                                                           
         LA    RE,L'TLDRREC(RE)                                                 
         B     THLD20                                                           
         SPACE 1                                                                
         USING TLCAPD,RE                                                        
THLD30   OC    TLCAHNXT,TLCAHNXT IF HOLDING FEE HAS ALREADY BEEN                
         BZ    THLDX             GENERATED                                      
         CLC   TLCAHDTE,TLCAHNXT AND HAS NOT BEEN PAID                          
         BNL   THLDX                                                            
         OI    HLDCHANG,PREVELIG ALWAYS REGENERATE HOLDING FEE                  
         DROP  RE                                                               
THLDX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE IS CALLED WHEN A CHANGE HAS BEEN MADE TO ANY CAST RECORD         
* BECAUSE ANY CHANGE TO THE CAST WILL CAUSE THE COMMERCIAL TO BE IN AN          
* UNVERIFIED STATE.  THE ROUTINE WILL READ THE COMMERCIAL RECORD AND            
* SAVE THE DATE, USER ID, AND STAFF ID OF THE PERSON WHO CHANGED THE            
* CAST.  ADDITIONALLY, THE ROUTINE WILL SET THE 'ANNOUNCER-ONLY-NON-            
* MUSICIAN' BIT TO ITS CORRECT STATE.                                           
*                                                                               
UPDCOMM  NTR1  BASE=*,LABEL=*                                                   
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
         BRAS  RE,GETEL                                                         
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
         OC    TACOUVST,STATVER          UNVERIFIED STATUS                      
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
UC20     OC    NUMLIFT,NUMLIFT     ARE THERE CAST MEMBERS ON LIFT               
         BZ    UC30                                                             
         OI    TACOSTA2,TACOSLFT   THEN SET BIT                                 
         B     UC50                                                             
*                                  ELSE CLEAR BIT                               
UC30     NI    TACOSTA2,ALL-TACOSLFT                                            
*                                                                               
UC50     GOTO1 PUTREC              WRITE RECORD BACK                            
         MVC   AIO,AIO1                                                         
*                                                                               
UCX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO UPDATE COMMERCIAL'S "CHANGED SINCE LAST               
*              HOLDING FEE ISSUED" STATUS BYTE                                  
         SPACE 1                                                                
UPDHSTA  NTR1  BASE=*,LABEL=*                                                   
         TM    HLDCHANG,ONSCREEN   IF HOLDING FEE NEEDS TO BE                   
         BO    UHS10               REGENERATED BECAUSE OF THIS CHANGE           
         CLI   MODE,XRECDEL                                                     
         BNE   UHSX                OR CAST THAT IS BEING DELETED                
         TM    HLDCHANG,PREVELIG   WAS PREVIOUSLY ELIGIBLE FOR                  
         BZ    UHSX                HOLDING FEE                                  
         SPACE 1                                                                
UHS10    XR    R3,R3               SET TO COUNT HOLDING FEE POINTERS            
         SPACE 1                                                                
         USING TLCAPD,R4                                                        
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCAPCD,TLCAHCDQ    READ ALL HOLDING FEE KEYS FOR                
         MVC   TLCAHCOM,TGCOM      COMMERCIAL'S CAST                            
         GOTO1 HIGH                (SKIP COMMERCIAL POINTER)                    
UHS20    GOTO1 SEQ                                                              
         CLC   KEY(TLCAHSRT-TLCAPD),KEYSAVE                                     
         BNE   UHS30                                                            
         AHI   R3,1                                                             
         OC    TLCAHNXT,TLCAHNXT   ONLY NEED TO REGENERATE HOLDING              
         BZ    UHS20               FEE NOTICE IF THERE IS A CAST                
         CLC   TLCAHDTE,TLCAHNXT   MEMBER WHO HASN'T BEEN PAID FOR              
         BNH   UHS40               THE LATEST HOLDING FEE NOTICE                
UHS30    TM    HLDCHANG,PREVELIG                                                
         BZ    UHSX                                                             
         LTR   R3,R3                                                            
         BZ    UHSX                                                             
         DROP  R4                                                               
         SPACE 1                                                                
UHS40    MVC   AIO,AIO2            READ COMMERCIAL REC FOR UPDATE               
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'B0',TGCOM)                               
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING TACOD,R4                                                         
         L     R4,AIO              R4 = A(COMMERCIAL DETAILS ELEMENT)           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TACOSTA2,TACOCHHF   TURN ON STATUS                               
         GOTO1 PUTREC              WRITE RECORD BACK                            
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         GOTOR SNDMQHFR,DMCB,(PRGSTAT,TGCOM),(TGSYSTA2,HEXOUT),MQIO             
UHSX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* TEST DIRECTIONAL PFKEYS PRESSED.                                              
*                                                                               
TESTPFKS NTR1  BASE=*,LABEL=*                                                   
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
TPX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* THIS ROUTINE TESTS IF THE DATA IN THE SELECT FIELD FOR THIS LINE IS           
* VALID BY LOOKING FOR A MATCHING PFTAB ENTRY.  IF NO MATCH IS FOUND AN         
* ERROR IS GIVEN.                                                               
*                                                                               
VALISEL  NTR1  BASE=*,LABEL=*                                                   
         LR    R0,R3               R0=D/A OF THIS RECORD                        
*                                                                               
         L     R2,ASELFLD          R2 = A(SELECT FIELD)                         
         OC    8(L'SCLL1,R2),=CL7' '  PAD WITH SPACES                           
*                                                                               
         L     R3,ALPFTAB          R3 = A(FIRST PFTAB ENTRY)                    
         CLI   ACTNUM,ACTLDEL                                                   
         BNE   *+8                                                              
         L     R3,ADPFTAB                                                       
         USING PFTABD,R3                                                        
*                                                                               
VS10     CLI   0(R3),X'FF'         IF END OF PFTAB THEN ERROR                   
         BE    ERRINV                                                           
         CLC   8(L'SCLL1,R2),PFTSEL  IF MATCH FOUND THEN VALID                  
         BE    VS20                                                             
         ZIC   RF,PFTLEN           BUMP TO NEXT PFTAB ENTRY                     
         AR    R3,RF                                                            
         B     VS10                LOOP BACK                                    
*                                                                               
VS20     TM    TGAYSTA7,TAAYSPPL   IF PAYROLL PLUS AGENCY                       
         BZ    VS21                                                             
         CLC   PFTSEL(2),=C'C '    SELECTING FOR CHANGE NOT ALLOWED             
         JE    ERPPLSI                                                          
         CLC   PFTSEL(2),=C'DE'    SELECTING FOR DELETE NOT ALLOWED             
         JE    ERPPLSI                                                          
*                                                                               
VS21     TM    COELEM+TACOSTA2-TACOD,TACOPCYC IF PER CYCLE COMMERCIAL           
         BZ    VS30                                                             
         CLC   PFTSEL(2),=C'DE'    AND SELECTING FOR DELETE                     
         BE    ERRNODEL            RETURN ERROR                                 
*                                                                               
VS30     L     RE,AWIDFLD                                                       
         CLI   TWASCR,SCR1A                                                     
         BNE   VS40                                                             
         TM    1(RE),X'20'                                                      
         BO    VS50                IF WEB APPLICATION ID FIELD                  
VS40     CLC   =C'VS',8(RE)        CONTAINS AN ACTIVE VITA SESSION ID           
         BE    VS41                                                             
         CLC   =C'TS',8(RE)                                                     
         BE    VS41                                                             
         CLC   =C'RS',8(RE)        OR ACTIVE VITA RADIO SESSION ID              
         BE    VS41                                                             
         CLC   =C'VC',8(RE)        OR ACTIVE VITA COMPLETION ID                 
         BE    VS41                                                             
         CLC   =C'TC',8(RE)                                                     
         BE    VS41                                                             
         CLC   =C'RC',8(RE)        OR ACTIVE VITA RADIO COMPLETION ID           
         BNE   VS50                                                             
VS41     CLC   PFTSEL(2),=C'DE'    AND SELECTING FOR DELETE                     
         BE    WEBERR              RETURN ERROR                                 
*                                                                               
VS50     CLC   PFTSEL(2),=C'C '    IF SELECTING FOR CHANGE                      
         BNE   VS60                                                             
         CLI   ACCESSED,C'N'       IF ACCESS COMPLETELY DENIED                  
         BE    ERRINV              THEN GIVE ERROR                              
         B     VSX                                                              
*                                                                               
VS60     CLC   PFTSEL(2),=C'DE'    IF SELECTING FOR DELETE                      
         BE    *+14                                                             
         CLC   PFTSEL(2),=C'R '    OR RESTORE                                   
         BNE   VS70                                                             
         CLI   ACCESSED,C'Y'       IF NOT FULLY ACCESSED                        
         BNE   ERRINV              THEN GIVE ERROR                              
*                                                                               
VS70     CLC   PFTSEL(2),=C'DE'    IF SELECTING FOR DELETE                      
         BNE   VSX                                                              
*                                                                               
         CLI   TWASCR,SCR0A        IF ON CAST/LIST SCREEN                       
         BNE   VS75                                                             
         L     RE,ANAMEFLD         CAN ONLY DELETE A CAST RECORD                
         CLI   8(RE),0             THAT IS ACTUALLY ON FILE                     
         BE    ERRINV                                                           
*                                                                               
         USING TLCAD,R4                                                         
VS75     LR    R3,R0               R3=D/A OF CAST RECORD                        
         BRAS  RE,READREC          READ CAST RECORD INTO AIO                    
                                                                                
         CLI   CMCLTYP,CTYMUS      IF COMMERCIAL TYPE IS NOT MUSIC              
         BE    VS80                                                             
         GOTOR HASTRKAS,DMCB,AIO   ERROR IS CAST IS ASSOCIATED TO               
         BE    ERRINV              A TRACK                                      
                                                                                
VS80     L     R4,AIO              R4 = A(CAST RECORD)                          
         MVC   SYSDIR,=CL8'CHKDIR'                                              
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY             READ FOR CHECK PAYMENT HISTORY               
         MVI   TLCKPCD,TLCKHCDQ                                                 
         MVC   TLCKHCOM,TLCACOM                                                 
         MVC   TLCKHSSN,TLCASSN                                                 
         GOTO1 HIGH                                                             
         B     VS100                                                            
VS90     GOTO1 SEQ                                                              
VS100    CLC   KEY(TLCKHCAT-TLCKPD),KEYSAVE                                     
         BNE   VS110                                                            
         CLC   TLCKHSEQ,TLCASEQ    ERROR IF ANY FOUND FOR THIS                  
         BE    ERRNOCHK            CAST SEQUENCE NUMBER                         
         B     VS90                                                             
         DROP  R3                                                               
                                                                                
VS110    MVC   SYSDIR,=CL8'TALDIR'                                              
                                                                                
         USING TLTMD,R3                                                         
         XC    KEY,KEY             READ ALL TIMESHEET KEYS                      
         MVI   TLTMCD,TLTMCDQ      ATTACHED TO THIS COMMERCIAL                  
         MVC   TLTMCOM,TLCACOM                                                  
         GOTO1 HIGH                                                             
         B     VS130                                                            
VS120    GOTO1 SEQ                                                              
VS130    CLC   TLTMKEY(TLTMINV-TLTMKEY),KEYSAVE                                 
         BNE   VS140                                                            
         CLC   TLCASEQ,TLTMSORT+4  ERROR IF ONE FOUND FOR THIS CAST             
         BE    ERRNOTIM            SEQUENCE NUMBER                              
         B     VS120                                                            
         DROP  R3                                                               
                                                                                
VS140    CLI   CMCLTYP,CTYMUS      IF COMMERCIAL TYPE IS MUSIC                  
         BNE   VSX                                                              
                                                                                
         USING TLCAPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCAPCD,TLCATCDQ    READ ALL COMMERCIAL ASSOCIATIONS             
         MVC   TLCATMCO,TLCACOM                                                 
         MVC   TLCATMCS,TLCASEQ                                                 
         GOTO1 HIGH                                                             
         B     VS160                                                            
VS150    GOTO1 SEQ                                                              
VS160    CLC   KEY(TLCATCOM-TLCAPD),KEYSAVE                                     
         BNE   VSX                                                              
         MVC   SVKEY,KEY                                                        
         DROP  R3                                                               
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY             READ FOR CHECK PAYMENT HISTORY               
         MVI   TLCKPCD,TLCKHCDQ    ON THOSE ASSOCIATED COMMERCIALS              
         MVC   TLCKHCOM,SVKEY+TLCATCOM-TLCAPD                                   
         MVC   TLCKHSSN,SVKEY+TLCATSSN-TLCAPD                                   
         GOTO1 HIGH                                                             
         B     VS180                                                            
VS170    GOTO1 SEQ                                                              
VS180    CLC   KEY(TLCKHCAT-TLCKPD),KEYSAVE                                     
         BNE   VS190                                                            
         CLC   TLCKHSEQ,SVKEY+TLCATSEQ-TLCAPD  ERROR IF ANY FOUND FOR           
         BE    ERRNOCHK                        THIS CAST SEQUENCE NUM           
         B     VS170                                                            
         DROP  R3                                                               
                                                                                
VS190    MVC   SYSDIR,=CL8'TALDIR'                                              
                                                                                
         MVC   SVKEY,KEY                                                        
         GOTO1 HIGH                                                             
         B     VS150                                                            
                                                                                
VSX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ROUTINE DETERMINES IF ANY FIELDS CHANGED                         
         SPACE 1                                                                
*                                  R2=A(FIRST FIELD)                            
TESTCHG  NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            R3=A(LAST FIELD)                             
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'80',(R2)),(X'80',(R3))                            
         BE    TCNO                                                             
*                                  AND SOME FIELD HAS CHANGED                   
         GOTO1 FLDVAL,DMCB,(X'40',(R2)),(X'80',(R3))                            
         BE    TCNO                                                             
         L     R2,8(R1)            R2=A(FIRST CHANGED FIELD)                    
         TM    AYSTAT,TAAYSLCK     IF AGENCY IS LOCKED                          
         BO    ERRLOCK             SET ERROR                                    
         TM    TGAYSTA7,TAAYSPPL   IF PAYROLL PLUS AGENCY                       
         JO    ERPPLSI             SET ERROR                                    
*                                                                               
TC10     CLM   R2,7,AGUARFLD+1     IF GUARANTEE FIELD IS FIRST                  
         BNE   TC20                                                             
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         GOTO1 FLDVAL,DMCB,(X'40',(R2)),(X'80',(R3))  LOOK ONWARD               
         BNE   TC20                                                             
*                                                                               
         CLI   RECCHANG,C'N'       IF THIS IS FIRST CHANGE                      
         BNE   *+8                                                              
         MVI   RECCHANG,C'G'       SET ONLY GUAR FIELD HAS CHANGED              
         B     TC40                                                             
*                                                                               
TC20     MVI   RECCHANG,C'Y'       SET SOMETHING CHANGED                        
         L     R1,AUNLOFLD                                                      
         CLI   5(R1),0             IF FIELD IS EMPTY                            
         BNE   TC25                                                             
         ICM   R1,15,SVAUNLO       USE FIELD ABOVE IT                           
         BZ    TC30                                                             
*                                                                               
TC25     CLC   8(3,R1),=C'AFM'     IF CAST MEMBER IS A MUSICIAN                 
         BNE   TC30                                                             
         OI    STATVER,TACOUVMU    SET MUSICIAN CHANGED                         
         B     TC40                                                             
*                                                                               
TC30     OI    STATVER,TACOUVNM    ELSE SET NON-MUSICIAN CHANGED                
*                                                                               
         USING HLDTABD,R1                                                       
TC40     LA    R1,HLDTAB           CHECK IF ANY FIELDS THAT SHOULD              
TC50     CLI   0(R1),X'FF'         TRIGGER AN UPDATED HOLDING FEE               
         BE    TCYES               HAVE BEEN CHANGED                            
         CLI   HTSCRN,0                                                         
         BE    TC60                                                             
         CLC   HTSCRN,TWASCR       IF FIELD NOT ON CURRENT SCREEN               
         BNE   TC70                DON'T CHECK IT                               
TC60     ZICM  RF,HTFLDA,2                                                      
         AR    RF,R6                                                            
         L     RE,0(RF)                                                         
         TM    4(RE),X'20'         IF FIELD HAS BEEN CHANGED                    
         BNZ   TC70                                                             
         OI    HLDCHANG,ONSCREEN+ONTLINE                                        
         B     TCYES               SET STATUS TO SEND OUT AN                    
TC70     LA    R1,HTLNQ(R1)        UPDATED HOLDING FEE                          
         B     TC50                                                             
         DROP  R1                                                               
*                                                                               
TCYES    SR    RC,RC                                                            
TCNO     LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
*              TABLE OF SCREEN FIELDS THAT, WHEN CHANGED, TRIGGER               
*              UPDATED HOLDING FEES                                             
*                                                                               
HLDTAB   DC    X'00',AL2(AOPFLD-WORKD)   OVERSCALE PERCENTAGE FIELD             
         DC    X'1A',AL2(AOV2FLD-WORKD)  2ND OVER% FLD (EXT SCR ONLY)           
         DC    X'1A',AL2(AOAFLD-WORKD)   OVER AMTS FLD (EXT SCR ONLY)           
         DC    X'00',AL2(ACATFLD-WORKD)  CATEGORY FIELD                         
         DC    X'00',AL2(AONOFFLD-WORKD) ON/OFF CAMERA FIELD                    
         DC    X'00',AL2(AYEARFLD-WORKD) CONTRACT YEAR FIELD                    
         DC    X'00',AL2(AGUARFLD-WORKD) GUARANTEE FIELD                        
         DC    X'00',AL2(AFRSTFLD-WORKD) FIRST SERVICES DATE                    
         DC    X'1A',AL2(AFCYCFLD-WORKD) FIRST FIXED CYCLE (EXT SCR)            
         DC    X'1A',AL2(ALSTFLD-WORKD)  LAST SERVICES DATE (EXT SCR)           
         DC    X'1A',AL2(AEXPFLD-WORKD)  EXPIRATION DATE (EXT SCR ONLY)         
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY ROLE DESCRIPTION                                                      
*                                                                               
DRROLE   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AROLEFLD           R2 = A(ROLE DESCRIPTION FIELD)             
         GOTO1 CHAROUT,DMCB,TACMELQ,(R2),TACMTYPD                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY RELEASED STATUS (OPTIONAL).                                           
*                                                                               
DRRLSD   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TARLELQ      GET RELEASED STATUS ELEMENT                  
         BRAS  RE,GETEL                                                         
         BNE   DRLSDX                                                           
         USING TARLD,R4                                                         
         L     R2,ARLSDFLD                                                      
         MVC   8(1,R2),TARLSTAT                                                 
         MVI   5(R2),1             INPUT LENGTH                                 
         L     R2,ARLDTFLD                                                      
         OC    TARLDATE,TARLDATE   MAY NOT HAVE A DATE                          
         BZ    DRLSD10                                                          
         GOTO1 DATCON,DMCB,(1,TARLDATE),(8,8(R2))                               
         MVI   5(R2),3             INPUT LENGTH                                 
*                                                                               
DRLSD10  L     R2,ARLSPFLD                                                      
         MVC   8(1,R2),TARLSTAP    PREVIOUS STATUS                              
         MVI   5(R2),1             INPUT LENGTH                                 
         L     R2,ARLDPFLD                                                      
         OC    TARLDATP,TARLDATP   MAY NOT HAVE A PREVIOUS DATE                 
         BZ    DRLSD20                                                          
         GOTO1 DATCON,DMCB,(1,TARLDATP),(8,8(R2))                               
         MVI   5(R2),3             INPUT LENGTH                                 
*                                                                               
DRLSD20  L     R2,ARLEFDTD         DISPLAY EFFECTIVE DATE                       
         OC    TARLEFDT,TARLEFDT                                                
         BZ    DRLSDX                                                           
         GOTO1 DATCON,DMCB,(1,TARLEFDT),(8,8(R2))                               
*                                                                               
DRLSDX   XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* VALIDATE COMMENTS (OPTIONAL).                                                 
*                                                                               
VRCM     NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACMFLD           R2 = A(COMMENTS FIELD)                       
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(0,0)                                                
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',(R2)),TACMTYPG                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE RELEASED STATUS (OPTIONAL).                                          
*                                                                               
VRRLSD   NTR1  BASE=*,LABEL=*                                                   
         MVI   RELSTAT,0                                                        
         L     R2,ARLSDFLD                                                      
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTOR VALWEB,DMCB,(X'80',0)                                            
         MVC   BYTE,8(R2)          BYTE = LETTER CODE                           
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VRLSD10                                                          
         CLI   8(R2),C'A'          LETTER CODE MUST BE A - D                    
         BL    RLERRINV                                                         
         CLI   8(R2),C'D'                                                       
         BH    RLERRINV                                                         
*                                                                               
         CLI   8(R2),C'A'          DOWNGRADE NOT ALLOWED IF EXTRA               
         BNE   *+12                                                             
         TM    TGCATYPE,EXTRA                                                   
         BO    RLERNODW                                                         
*                                                                               
         L     R2,ALSTFLD          LAST SERVICES DATE IS REQUIRED               
         CLI   5(R2),0                                                          
         BE    RLERRMIS                                                         
*                                                                               
VRLSD10  XC    ELEM,ELEM                                                        
         L     R4,AIO                                                           
         MVI   ELCODE,TARLELQ      GET RELEASED STATUS ELEMENT                  
         BRAS  RE,GETEL                                                         
         BE    VRLSD30                                                          
         CLI   5(R2),0             EXIT IF NO INPUT AND NO ELEMENT              
         BE    VRLSDX                                                           
*                                                                               
         USING TARLD,R4                                                         
         LA    R4,ELEM             ADD NEW ELEMENT                              
         MVI   TARLEL,TARLELQ      ELEMENT CODE                                 
         MVI   TARLLEN,TARLLNQ     LENGTH                                       
         B     VRLSD40                                                          
*                                                                               
VRLSD30  CLC   TARLSTAT,BYTE       IF STATUS HAS NOT CHANGED, EXIT              
******   BE    VRLSDX                                                           
         BNE   VRLSD32                                                          
         L     R2,ARLEFDTD         IF STATUS  NOT CHANGED ALSO                  
         TM    4(R2),X'80'         PROCEED IF EFF DATE CHANGED                  
         BNO   VRLSDX                                                           
*                                                                               
* STATUS CHANGED                                                                
VRLSD32  ZIC   R1,TARLLEN          LENGTH                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R4)       COPY ELEMENT INTO ELEM                       
         GOTO1 REMELEM             REMOVE THIS ELEMENT                          
         LA    R4,ELEM                                                          
         MVC   TARLSTAP,TARLSTAT   MOVE OLD STATUS TO PREVIOUS                  
         OC    TARLDATE,TARLDATE   IF THERE IS NO OLD DATE?                     
         BZ    *+10                LEAVE THE PREVIOUS DATE                      
         MVC   TARLDATP,TARLDATE   MOVE DATE TO PREVIOUS DATE                   
         XC    TARLDATE,TARLDATE   CLEAR NEW DATE                               
         XC    TARLEFDT,TARLEFDT   CLEAR EFFECTIVE DATE                         
*                                                                               
VRLSD40  MVC   TARLSTAT,BYTE       NEW RELEASE LETTER CODE                      
*                                                                               
         L     R2,ARLEFDTD         EFFECTIVE DATE                               
****     CLI   BYTE,C'B'           REQUIRED FOR CODES C AND D                   
****     BNH   VRLSD50                                                          
         CLI   5(R2),0                                                          
         BE    RLERRMIS                                                         
         GOTO1 DTVAL,DMCB,TARLEFDT  VALIDATE EFFECTIVE DATE                     
         B     *+12                                                             
VRLSD50  CLI   5(R2),0             INVALID FOR CODES A AND B                    
         BNE   RLERRINV                                                         
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         CLI   TARLSTAT,0          IF A,B,C, OR D                               
         BE    VRLSDX              REQUEST RELEASE REPORT                       
*                                                                               
         BRAS  RE,ADDREQ           ADD REQUEST FOR RELEASE REPORT               
*                                                                               
         MVI   TWASCR,X'81'        SCREEN CODE FOR RELEASE REPORT               
         GOTO1 ACTVIN,DMCB,(X'80',0) ADD ACTIVITY EL WITH SCRN CODE             
         MVI   TWASCR,SCR1A        RESTORE SCREEN CODE                          
*                                                                               
         MVI   RELSTAT,1           SET STATUS - REL REPORT REQUESTED            
*                                                                               
VRLSDX   XIT1                                                                   
         DROP  R4                                                               
*                                                                               
RLERRMIS MVI   ERROR,MISSING     MISSING INPUT FIELD                            
         B     RLERRX                                                           
*                                                                               
RLERRINV MVI   ERROR,INVALID     INVALID INPUT FIELD                            
         B     RLERRX                                                           
*                                                                               
RLERNODW MVC   MYMSGNO,=Y(EREXNODW)                                             
         B     NTHEEND                                                          
*                                                                               
NTHEEND  OI    GENSTAT2,USGETTXT  NEW THEEND FOR TWO BYTE ERROR MSGS            
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     RLERRX                                                           
*                                                                               
RLERRX   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ADD RELEASE REPORT REQUEST                                                    
*                                                                               
ADDREQ   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,BLOCK            R3=A(REQUEST RECORD)                         
         USING REQD,R3                                                          
         XC    REQHDR,REQHDR       CLEAR HEADER                                 
         OI    REQHDR+15,X'01'     SET LINKED REQUEST                           
         MVC   REQUEST,RLEQCARD    INITIALIZE REQUEST CARD                      
*                                                                               
*                             BUILD SORT AREA AT BEG. OF CARD                   
         MVC   REQUEST+2(2),TGCTALPH  CONNECT ALPHA USER ID                     
         MVC   REQUEST+4(1),AYOFF     OFFICE CODE                               
         MVC   REQUEST+5(6),TGAGY     AGENCY                                    
         OC    AYOFF,AYOFF                                                      
         BNZ   *+6                                                              
         DC    H'00'                  DIE IF NO OFFICE                          
*                                                                               
         L     R4,AIO              CAST RECORD                                  
         USING TLCAD,R4                                                         
         GOTO1 HEXOUT,DMCB,TLCASORT+4,REQSEQ,2,=C'TOG'    SORT KEY              
         MVC   REQUEST+72(4),REQSEQ    SORT KEY                                 
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,TARLELQ         RELEASE ELEMENT                           
         BRAS  RE,GETEL                                                         
         BE    *+6                    WAS JUST ADDED                            
         DC    H'00'                                                            
         USING TARLD,R4                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,TGCOM,REQCOM,4,=C'TOG'  INT COM                      
         MVC   REQUEST+38(8),REQCOM    INTERNAL COMML NUMBER                    
         MVC   REQUEST+46(1),TARLSTAT  REQUEST LETTER                           
         MVC   REQUEST+52(6),ZEROS     DEFAULT EFFECTIVE DATE                   
         OC    TARLEFDT,TARLEFDT       IF WE HAVE EFFECTIVE DATE,               
         BZ    AREQ10                                                           
         GOTO1 DATCON,DMCB,(1,TARLEFDT),(20,REQDATE)                            
         MVC   REQUEST+52(6),REQDATE+2 MOVE IN EFFECTIVE DATE                   
AREQ10   MVC   REQUEST+63(9),TGSSN     SSN                                      
         MVC   REQUEST+63(9),SSNSPACE                                           
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID CONVERT SSN TO PID                      
         MVC   REQUEST+63(6),TGPID                                              
AREQ20   MVC   REQUEST+76(3),TGCAT     CATEGORY                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMADD'),REQFILE,(R3),(R3)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
REQCOM   DS    CL8                                                              
REQSEQ   DS    CL4                                                              
REQDATE  DS    CL8                                                              
*                                                                               
*        CONSTANTS, ETC.                                                        
         SPACE 1                                                                
ZEROS    DC    CL9'000000000'                                                   
         SPACE 1                                                                
RLEQCARD DC    CL80'RLXXOAGENCY 0203REL 0301R 0503DDS 0909INTCOMNOL 100X        
               6YYMMDD 1116123456789SEQ CAT*'                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PUT OUT MESSAGE IF RELEASE REPORT WAS REQUESTED                               
*                                                                               
CHKREL   NTR1  BASE=*,LABEL=*                                                   
         CLI   RELSTAT,0           AND JUST REQUEST A RELEASE REPORT,           
         BE    CHKRELX                                                          
         MVI   MYMSGNO1,68         GIVE MESSAGE - REPORT WILL BE                
         L     R2,EFHREC           PROCESSED OVERNIGHT                          
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
CHKRELX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* VALIDATE OPTIONS FIELD                                                        
*                                                                               
VALOPTS  NTR1  BASE=*,LABEL=*                                                   
         MVI   OPTS,0              CLEAR OPTIONS                                
*                                                                               
         LA    R2,SCLOPTH          OPTIONS FIELD                                
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               INVALID INPUT                                
         BZ    ERRINV                                                           
VOPT20   MVC   ERRDISP,SCDISP1                                                  
         CLI   SCLEN1,0            LHS IS REQUIRED                              
         BE    ERRINV                                                           
         CLC   =C'OTH',SCDATA1                                                  
         BNE   VOPT30                                                           
         OI    OPTS,OPTOTH         LIST OTHER INFORMATION                       
         B     VOPT70                                                           
VOPT30   CLC   =C'REL',SCDATA1                                                  
         BNE   VOPT40                                                           
         CLI   SCDATA2,C'N'        DEFAULT                                      
         BE    VOPT70                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   ERRINV                                                           
         OI    OPTS,OPTREL         LIST RELEASED CAST                           
         B     VOPT70                                                           
VOPT40   CLC   =C'EX',SCDATA1                                                   
         BNE   VOPT50                                                           
         CLI   SCDATA2,C'Y'        DEFAULT                                      
         BE    VOPT70                                                           
         CLI   SCDATA2,C'N'                                                     
         BNE   ERRINV                                                           
         OI    OPTS,OPTPRN         LIST ONLY PRINCIPALS                         
         B     VOPT70                                                           
VOPT50   CLC   =C'LS',SCDATA1                                                   
         BNE   VOPT60                                                           
         CLI   SCDATA2,C'N'        DEFAULT                                      
         BE    VOPT70                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   ERRINV                                                           
         OI    OPTS,OPTLSC         LIST LAST SERVICED PERFORMERS                
         B     VOPT70                                                           
*                                                                               
VOPT60   CLC   =C'EQY',SCDATA1                                                  
         BNE   ERRINV                                                           
         CLI   SCDATA2,C'N'        DEFAULT                                      
         BE    VOPT70                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   ERRINV                                                           
         OI    OPTS,OPTEQY         LIST ONLY EQY PERFORMER                      
         B     VOPT70                                                           
*                                                                               
VOPT70   LA    R3,SCANNEXT                                                      
         BCT   R0,VOPT20                                                        
VOPTX    OI    4(R2),X'20'                                                      
         MVI   ERRDISP,0                                                        
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* UNPACK THE PID NUMBER TO A SS NUMBER                                          
* R2 POINTS TO THE SSN FIELD HEADER                                             
*                                                                               
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
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PACK THE SS NUMBER INTO A PID NUMBER                                          
* R2 POINTS TO THE SSN FIELD HEADER                                             
*                                                                               
         SPACE 1                                                                
PKSSN    NTR1  BASE=*,LABEL=*                                                   
         CLC   TGCTSTAF,=C'YZASDDNY'                                            
         BE    PKSSNX                                                           
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   8(L'TGSSN,R2),SSNSPAC2                                           
         MVC   8(L'TGPID,R2),TGPID                                              
         MVI   5(R2),6                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
PKSSNX   XIT1                                                                   
SSNSPAC2 DC    CL9' '                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        IF THERE IS INFO ON EXTENSION SCREEN THAT IS NOT ON LIST     *         
*        SCREEN, SET D/A FOR CAST VERIFY                              *         
*        ON ENTRY ... R3 = A(DISK ADDRESS)                            *         
***********************************************************************         
                                                                                
CKEXT    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,VERTAB           A(VERIFY EXTENTION TABLE)                    
         LA    R1,NUMSCR           LOOP COUNTER                                 
                                                                                
CK10     OC    0(4,R2),0(R2)       IS THERE ROOM FOR NEXT ENTRY                 
         JZ    CK20                                                             
         LA    R2,4(R2)                                                         
         BCT   R1,CK10                                                          
         DC    H'0'                THERE MUST BE ENOUGH ROOM                    
                                                                                
         USING TACAD,R4                                                         
CK20     L     R4,ACAELEM          SET A(CAST ELEMENT)                          
         OC    TACAFCYC,TACAFCYC   FIRST FIXED CYCLE                            
         JNZ   CK30                                                             
         OC    TACALAST,TACALAST   LAST SERVICE DATE                            
         JNZ   CK30                                                             
         OC    TACAEXP,TACAEXP     EXPIRATION DATE                              
         JNZ   CK30                                                             
         OC    TACAOV2,TACAOV2     2ND OVERSCALE AMOUNT                         
         JNZ   CK30                                                             
         TM    TACASTAT,X'FF'-TACASTLF-TACASTLO                                 
         JNZ   CK30                CHECK STATUS EXCEPT LIFT INFO                
         OC    TACASTA2,TACASTA2   2ND STATUS BYTE                              
         JNZ   CK30                                                             
                                                                                
         L     R4,AIO              CHECK IF OVERSCALE % ELEMENT EXISTS          
         MVI   ELCODE,TAOPELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    CK30                                                             
                                                                                
         L     R4,AIO              CHECK IF OVERSCALE AMT ELEM EXISTS           
         MVI   ELCODE,TAOAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
CK30     MVC   0(4,R2),0(R3)                                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE WILL ADD A HOLDING FEE NOTICE ELEMENT TO THE CAST    *         
*        RECORD IS THERE ISN'T ONE ALREADY THERE                      *         
***********************************************************************         
                                                                                
VRHLD    NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO              IF HOLDING FEE NOTICE ELEMENT DOES           
         MVI   ELCODE,TAHFELQ          NOT YET EXIST                            
         BRAS  RE,GETEL                                                         
         JE    XIT                                                              
                                                                                
         USING TAHFD,R4                                                         
         LA    R4,ELEM             THEN BUILD HOLDING FEE NOTICE ELEM           
         XC    ELEM,ELEM                                                        
         MVI   TAHFEL,TAHFELQ                                                   
         MVI   TAHFLEN,TAHFLNQ                                                  
         GOTO1 ADDELEM             ADD ELEMENT                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CAST LIST SCREEN ROUTINE TO DISPLAY WEB APPLICATION PREAMBLE *         
***********************************************************************         
                                                                                
DRWID    NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   XIT                                                              
                                                                                
         L     R2,AWIDFLD                                                       
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         MVC   8(2,R2),TAFNNAME                                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        EXTENDED CAST SCREEN ROUTINE TO DISPLAY WEB APPLICATION ID   *         
***********************************************************************         
                                                                                
DISWID   NTR1  BASE=*,LABEL=*                                                   
         OI    SCAWIDH+1,X'20'                                                  
         OI    SCAWIDH+6,X'80'                                                  
                                                                                
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTOWB))                                     
         JNE   DWID10                                                           
         L     R4,TGELEM                                                        
         MVC   SCAWID,TAFNNAME                                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
DWID10   MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
         MVC   SCAWID,TAFNNAME                                                  
         NI    SCAWIDH+1,X'DF'                                                  
         DROP  R4                                                               
                                                                                
         USING TAACD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAACELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   SVACCDTE,TAACCDTE                                                
         MVC   SVACCTIM,TAACCTIM                                                
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        EXTENDED CAST SCREEN ROUTINE TO VALIDATE FOR SUPER USER      *         
*        PASSWORD IN WEB APPLICATION ID FIELD. IF PRESENT, REMOVE     *         
*        WEB APPLICATION ID FROM CAST RECORD                          *         
***********************************************************************         
                                                                                
VALWID   NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   XIT                                                              
                                                                                
         USING TAACD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAACELQ      IF RECORD HAS BEEN UPDATED FROM              
         BRAS  RE,GETEL            WEB APPLICATION SINCE INITIAL                
         JNE   VWID00              DISPLAY, PROMPT FOR REFRESH                  
         CLC   TAACCDTE,SVACCDTE                                                
         JNE   ERRREF                                                           
         CLC   TAACCTIM,SVACCTIM                                                
         JNE   ERRREF                                                           
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
VWID00   OC    SCAWID,=10C' '                                                   
         CLC   SCAWID,=CL10'VIDA'                                               
         JE    VWID20                                                           
         TM    PRGSTAT,TESTSYS     TST STYSTEM                                  
         JO    VWID10                                                           
         CLC   TGUSER,=H'7538'     AND USER-IDS TALFQA                          
         JE    VWID10                                                           
         CLC   TGUSER,=H'7697'     AND CLIFQA                                   
         JNE   XIT                                                              
VWID10   CLC   SCAWID,=CL10'ATIV'                                               
         JNE   XIT                                                              
                                                                                
VWID20   L     R4,TGELEM                                                        
         MVI   TAFNTYPE,TAFNTOWB                                                
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
ERRREF   MVC   MYMSGNO,=Y(ERRCAREF)                                             
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         GOTO1 EXIT,DMCB,0                                                      
*                                                                               
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES THAT VITA SESSION LOCKED FIELDS ARE NOT      *         
*        UPDATED FROM THIS MAINFRAME APPLICATION.                     *         
*        ON ENTRY ... P1 BYTE = X'80' ONLY CHECK FOR VITA COMPLETIONS *         
*                     R2 = A(LOCKED FIELD)                            *         
***********************************************************************         
                                                                                
VALWEB   NTR1  BASE=*,LABEL=*                                                   
         TM    4(R2),X'20'         IF FIELD HAS CHANGED                         
         JO    XIT                                                              
         L     RE,AWIDFLD          ENSURE WEB APPLICATION ID                    
         CLI   TWASCR,SCR1A                                                     
         JNE   VW10                                                             
         TM    1(RE),X'20'                                                      
         JO    XIT                                                              
VW10     CLC   =C'VC',8(RE)        DOES NOT CONTAIN AN ACTIVE                   
         JE    VW20                VITA COMPLETION ID                           
         CLC   =C'TC',8(RE)                                                     
         JE    VW20                                                             
         CLC   =C'RC',8(RE)        OR VITA RADIO COMPLETION ID                  
         JE    VW20                                                             
         TM    0(R1),X'80'                                                      
         JO    XIT                                                              
         CLC   =C'VS',8(RE)        OR ACTIVE VITA SESSION ID                    
         JE    VW20                                                             
         CLC   =C'TS',8(RE)                                                     
         JE    VW20                                                             
         CLC   =C'RS',8(RE)        OR ACTIVE VITA RADIO SESSION ID              
         JNE   XIT                                                              
                                                                                
VW20     MVC   MYMSGNO,=Y(ERUSEWEB) RECORD MUST BE UPDATED FROM                 
         MVI   BLOCK,0              VITA SESSION/COMPLETION                     
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS POSITIVE CONDITION CODE IF PERFORMER IS      *         
*        A PRINCIPAL, RETURNS NEGATIVE CONDITION CODE IF PERFORMER    *         
*        IS AN EXTRA                                                  *         
*        ON ENTRY ... R4 = A(CAST KEY)                                *         
***********************************************************************         
                                                                                
         USING TLCAD,R4                                                         
FILTEXT  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 CATVAL,DMCB,TLCACAT                                              
         TM    TGCATYPE,EXTRA                                                   
         JO    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS POSITIVE CONDITION CODE IF PERFORMER IS      *         
*        RELEASED                                                               
***********************************************************************         
                                                                                
FILTREL  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         USING TARLD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TARLELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         CLI   TARLSTAT,C'A'                                                    
         JL    NO                                                               
         DROP  R4                                                               
                                                                                
         TM    OPTS,OPTLSC                                                      
         JZ    YES                                                              
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACALAST,TACALAST                                                
         JZ    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS POSITIVE CONDITION CODE IF PERFORMER IS      *         
*        LAST SERVICED                                                          
***********************************************************************         
                                                                                
FILTLSC  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACALAST,TACALAST                                                
         JZ    NO                                                               
         DROP  R4                                                               
                                                                                
         TM    OPTS,OPTREL                                                      
         JZ    YES                                                              
                                                                                
         USING TARLD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TARLELQ      CHECK FOR RELEASED STATUS ELEMENT            
         BRAS  RE,GETEL                                                         
         JNE   YES                                                              
         USING TARLD,R4                                                         
         CLI   TARLSTAT,C'A'       REL STATUS OF A,B,C,D ?                      
         JL    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS POSITIVE CONDITION CODE IF PERFORMER IS      *         
*        EQUITY PERFORMER                                                       
***********************************************************************         
                                                                                
FILTEQY  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         TM    TACASTA3,TACASEQY                                                
         JZ    NO                                                               
         DROP  R4                                                               
                                                                                
         TM    OPTS,OPTEQY                                                      
         JO    YES                                                              
         J     NO                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE AUTOMATICALLY ADDS PERFORMER TO VERSION 1 WHEN       *         
*        APPROPRIATE                                                  *         
***********************************************************************         
                                                                                
AUTOVER1 NTR1  BASE=*,LABEL=*                                                   
         CLI   VALMODE,C'A'         IF ADDING CAST                              
         JNE   XIT                                                              
                                                                                
         MVC   AIO,AIO2             READ COMMERCIAL RECORD                      
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',0)                                   
         MVC   AIO,AIO1                                                         
         JNE   XIT                                                              
                                                                                
         L     R4,AIO2                                                          
         MVI   ELCODE,TAVRELQ       IF COMMERCIAL HAS VERSIONS                  
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         BRAS  RE,NEXTEL                                                        
         JE    XIT                                                              
                                                                                
         USING TLVRD,R3                                                         
         LA    R3,KEY                                                           
         XC    TLVRKEY,TLVRKEY      BUT ONLY 1 VERSION                          
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLVRVER-TLVRD),KEYSAVE                                       
         JE    XIT                                                              
         DROP  R3                                                               
                                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEMENT                                                       
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,4                                                        
         MVI   TAFNTYPE,TAFNTVER    AUTOMATICALLY ADD CAST TO                   
         MVI   TAFNNAME,1           VERSION 1                                   
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE READS THE CAST RECORD WITH THE DISK ADDRESS POINTED TO           
* BY R3.  IF THE DELETE STATUS BIT DOES NOT MATCH THE ACTION TYPE (LIST         
* OR LIST DELETED) THEN THE ROUTINE SETS THE DISAPPR FLAG TO 'Y' AND            
* RETURNS 'NO'. OTHERWISE IT RETURNS 'YES'.                                     
*                                                                               
READREC  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,KEY              READ CAST RECORD                             
         USING TLDRD,R4                                                         
         MVC   TLDRDA,0(R3)                                                     
         OI    DMINBTS,X'08'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   ACTNUM,ACTLDEL      IF ACTION IS NOT LDELETE                     
         JE    *+8                                                              
         NI    DMINBTS,X'F7'       THEN RESET PASS BACK DELETED FLAG            
*                                                                               
         L     R4,AIO              R4 = A(RECORD KEY)                           
         USING TLCAD,R4                                                         
*                                                                               
         CLI   ACTNUM,ACTLDEL      IF ACTION IS LIST DELETED                    
         JNE   RR10                                                             
         TM    TLCASTAT,X'40'      AND RECORD IS NOT DELETED                    
         JZ    RR20                                                             
         J     YES                                                              
*                                                                               
RR10     TM    TLCASTAT,X'80'      OR ACTION IS LIST OR VERIFY AND              
         JZ    YES                     RECORD IS DELETED                        
*                                                                               
RR20     MVI   DISAPPR,C'Y'        THEN SET RECORD HAS DISAPPEARED              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DISPLAYS OVERSCALE PERCENTAGES/AMOUNTS FOR EXTENDED  *         
*        CAST SCREEN FIELDS                                           *         
*        ON ENTRY ... P1 BYTE 0 = OVERSCALE PCT/AMT ELEMENT CODE      *         
*                     P1        = A(OVERSCALE PCT/AMT FIELD)          *         
*                     AIO       = A(CAST RECORD)                      *         
***********************************************************************         
                                                                                
DROPA    NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(OVERSCALE PCT/AMT FIELD)                
         LA    R2,8(R2)                                                         
                                                                                
         USING TAOPD,R4                                                         
         L     R4,AIO              R4=A(CAST RECORD)                            
         MVC   ELCODE,0(R1)                                                     
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         ZIC   R0,TAOPNUM          R0 = # SUB ELEMENTS                          
         LA    R3,TAOPSBEL         R3 = A(FIRST SUB ELEMENT)                    
                                                                                
DROPA10  BAS   RE,DOA              DISPLAY OVERSCALE AMOUNT                     
                                                                                
         CLI   0(R2),C' '          IF NOT FIRST CODE                            
         JNH   DROPA20                                                          
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
                                                                                
DROPA20  CLC   0(3,R3),=CL7' '     IF USE CODE IS SPACES                        
         JNE   *+14                                                             
         MVC   0(3,R2),=C'ALL'     THEN DISPLAY 'ALL'                           
         J     *+10                                                             
         MVC   0(3,R2),0(R3)       ELSE DISPLAY USE CODE                        
                                                                                
         MVI   3(R2),C'='          DISPLAY '= PERCENTAGE'                       
         ICM   RF,15,3(R3)                                                      
         JNM   DROPA30             IF NEGATIVE, SHOW '%PERCENTAGE'              
         MVI   4(R2),C'%'          FOR PERCENT SCALE                            
         LA    R2,1(R2)                                                         
         N     RF,=X'7FFFFFFF'     STRIP OFF HOB                                
DROPA30  GOTOR DR2DEC,DMCB,(RF),4(R2),10                                        
                                                                                
         LA    R2,3(R2)            POINT R2 TO LAST NON-SPACE                   
         A     R2,0(R1)                                                         
         LA    R3,7(R3)            BUMP R3 TO NEXT SUB ELEMENT                  
         BCT   R0,DROPA10          REPEAT UNTIL NO MORE SUB-ELEMENTS            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE DISPLAYS OVERSCALE PERCENTAGES/AMOUNTS FOR EXTENDED  *         
*        CAST SCREEN FIELDS                                           *         
*        ON ENTRY ... P1 BYTE 0 = OVERSCALE PCT/AMT ELEMENT CODE      *         
*                     P1        = A(OVERSCALE PCT/AMT FIELD)          *         
*                     AIO       = A(CAST RECORD)                      *         
***********************************************************************         
                                                                                
DOA      NTR1                                                                   
         CLI   ELCODE,TAOAELQ      IF DISPLAYING OVERSCALE AMOUNT FIELD         
         JNE   XIT                                                              
         GOTOR PRGRRFLD,DMCB,(R3)  DO SPECIAL PROCESSING FOR GRR                
         CLC   =C'HLD',0(R3)                                                    
         JE    DOA10                                                            
         CLC   =C'SHL',0(R3)       IF HOLDING FEE USE                           
         JE    DOA10                                                            
         CLC   =C'ADH',0(R3)                                                    
         JNE   XIT                                                              
DOA10    MVI   BYTE,C'Y'           SET HOLDING FEE AMOUNT FOUND                 
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE 2 DEC PLACE NUMBER FOUND IN PARM1 TO THE            
* DESTINATION POINTED TO BY PARM2.  IF THE DISPLAYED AMOUNT IS BIGGER           
* THEN THE LENGTH SPECIFIED IN PARM3 THEN IT WILL DISPLAY '*'S INSTEAD.         
* THE ROUTINE WILL RETURN THE LENGTH IN PARM1.                                  
*                                                                               
DR2DEC   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)         R2 = NUMBER                                  
*                                  R3 = A(DESTINATION)                          
         BCTR  R4,0                R4 = LENGTH OF DESTINATION - 1               
                                                                                
         EX    R4,*+8              PRE-CLEAR DESTINATION                        
         J     *+10                                                             
         XC    0(0,R3),0(R3)                                                    
                                                                                
         LTR   R2,R2               IF NUMBER = 0                                
         JNZ   *+18                                                             
         MVI   0(R3),C'0'          THEN DISPLAY '0' AND RETURN                  
         MVC   0(4,R1),=F'1'                                                    
         J     XIT                 ELSE EDIT PERCENTAGE INTO BLOCK              
         EDIT  (R2),(10,BLOCK),2,ALIGN=LEFT                                     
                                                                                
         LA    RF,BLOCK            RF = A(LAST CHAR IN BLOCK)                   
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
                                                                                
DR2DEC10 CLI   0(RF),C'.'          IF CHAR IS '.' THEN DONE                     
         JE    DR2DEC50                                                         
         CLI   0(RF),C'0'          ELSE IF CHAR IS <> '0' THEN DONE             
         JNE   DR2DEC60                                                         
         BCT   RF,DR2DEC10         ELSE BACK UP POINTER AND LOOP BACK           
                                                                                
DR2DEC50 BCTR  RF,0                BACK UP ONE MORE FOR '.'                     
                                                                                
DR2DEC60 LA    RE,BLOCK            RF = LENGTH OF NUMBER - 1                    
         SR    RF,RE                                                            
                                                                                
         CR    RF,R4               IF L(NUMBER) - 1 > L(DEST) - 1               
         JH    DR2DEC90            THEN DISPLAY '*'S                            
                                                                                
         EX    RF,*+8              ELSE MOVE NUMBER TO DESTINATION              
         J     *+10                                                             
         MVC   0(0,R3),BLOCK                                                    
                                                                                
         LA    RF,1(RF)            RETURN LENGTH IN PARM1                       
         ST    RF,0(R1)                                                         
         J     XIT                                                              
                                                                                
DR2DEC90 MVI   0(R3),C'*'          DISPLAY '*'S                                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         J     *+10                                                             
         MVC   1(0,R3),0(R3)                                                    
                                                                                
         LA    RF,2(R4)            RETURN LENGTH IN PARM1                       
         ST    RF,0(R1)                                                         
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES OVERSCALE PERCENTAGES/AMOUNTS FOR EXTENDED *         
*        CAST SCREEN FIELDS                                           *         
*        ON ENTRY ... P1 BYTE 0 = OVERSCALE PCT/AMT ELEMENT CODE      *         
*                     P1        = A(OVERSCALE PCT/AMT FIELD)          *         
***********************************************************************         
                                                                                
VROPA    NTR1  BASE=*,LABEL=*                                                   
         USING TAOPD,R4                                                         
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVC   TAOPEL,0(R1)                                                     
                                                                                
         L     R2,0(R1)            R2=A(OVERSCALE PCT/AMT FIELD)                
         BRAS  RE,STLENPRO         SET LENGTH IF PROTECTED                      
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         CLI   4(R1),0                                                          
         JE    ERRINV                                                           
         ZIC   RF,4(R1)                                                         
                                                                                
         LHI   RE,6                                                             
         CLI   ELEM,TAOAELQ        ENSURE THAT THE MAXMIUM AMOUNT               
         JE    *+8                 OF ENTRIES IS NOT EXCEEDED                   
         LHI   RE,5                                                             
         CR    RE,RF                                                            
         JL    ERRINV                                                           
                                                                                
         ST    RF,FULL                                                          
         STC   RF,TAOPNUM          SAVE NUMBER OF SUB ELEMENTS                  
                                                                                
         MVI   PARAS,X'FF'                                                      
                                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK            R3 = A(SCANNER BLOCK)                        
         XR    R0,R0               R0 = DISPLACEMENT INTO FIELD                 
         LA    R4,TAOPSBEL         R4 = A(FIRST SUB ELEMENT)                    
         DROP  R4                                                               
                                                                                
VROPA10  CLC   SCDATA1(3),=C'ALL'  TRANSLATE ALL INTO INTERNAL                  
         JNE   *+10                REPRESENTATION                               
         MVC   SCDATA1(3),ALLUSES                                               
                                                                                
         MVC   TGUSCDE,SCDATA1     INITIALIZE USE CODE                          
                                                                                
         LA    RE,PARAS                                                         
VROPA20  CLI   0(RE),X'FF'         ENSURE USE HAS NOT ALREADY                   
         JE    VROPA30             BEEN ENTERED                                 
         CLC   TGUSCDE,0(RE)                                                    
         JE    ERRFLD                                                           
         LA    RE,L'TGUSCDE(RE)                                                 
         J     VROPA20                                                          
VROPA30  MVC   0(L'TGUSCDE,RE),TGUSCDE                                          
         MVI   L'TGUSCDE(RE),X'FF'                                              
                                                                                
         CLC   TGUSCDE,ALLUSES     IF USE CODE IS NOT ALL                       
         JE    VROPA50                                                          
         CLC   TGUSCDE,=C'ARE'     OR ARE, VALIDATE IT                          
         JE    VROPA50                                                          
         GOTO1 USEVAL,DMCB,(X'40',SCDATA1),0                                    
         JNE   ERRFLD                                                           
                                                                                
         CLI   TGUSMEDS,ALL        ENSURE USE IS VALID FOR MEDIA                
         JE    VROPA40                                                          
         MVC   BYTE,TGUSMEDS                                                    
         NC    BYTE,TGMEEQU                                                     
         JZ    MEDERR                                                           
                                                                                
VROPA40  GOTO1 VALTYP,DMCB,TGCTEQU ENSURE USE IS VALID FOR COMMERCIAL           
         JE    VROPA50             TYPE                                         
         TM    COMSTAT,CSSPANVR                                                 
         JZ    CTYPERR             IF NOT AND COMMERCIAL HAS SPANISH            
         MVI   BYTE,CTYSPAN        VERSIONS, USE CAN ALSO BE SPANISH            
         GOTO1 VALTYP,DMCB,BYTE                                                 
         JNE   CTYPERR                                                          
                                                                                
VROPA50  BAS   RE,VOA              VALIDATE OVERSCALE AMOUNT USE                
                                                                                
         ZICM  RF,SCLEN2,1         VALIDATE AMOUNT                              
         JZ    ERRFLD                                                           
         CLI   SCDATA2,C'%'        PERCENT SCALE USED?                          
         JNE   VROPA60                                                          
         MVI   SCDATA2,C' '                                                     
VROPA60  GOTO1 CASHVAL,DMCB,SCDATA2,(RF)                                        
         CLI   0(R1),0                                                          
         JNE   ERRFLD                                                           
         TM    4(R1),X'80'         DON'T ALLOW NEGATIVE AMOUNTS                 
         JO    ERRFLD                                                           
                                                                                
         BAS   RE,VOP              VALIDATE OVERSCALE PERCENTAGE                
         BAS   RE,VOP2             VALIDATE 2ND OVERSCALE PCT USE               
                                                                                
         MVC   0(3,R4),TGUSCDE     SAVE USE CODE IN ELEM                        
                                                                                
         CLI   SCDATA2,C' '        PERCENT SCALE USED?                          
         JNE   *+8                                                              
         OI    4(R1),X'80'                                                      
         MVC   3(4,R4),4(R1)       SAVE AMOUNT IN ELEM                          
                                                                                
         ZIC   RF,SCLEN1           BUMP R0 TO NEXT FIELD                        
         LA    RF,1(RF)                                                         
         AR    R0,RF                                                            
         ZIC   RF,SCLEN2                                                        
         LA    RF,1(RF)                                                         
         AR    R0,RF                                                            
         LA    R4,7(R4)            BUMP R4 TO NEXT SUB ELEMENT                  
         LA    R3,SCANNEXT         BUMP R3 TO NEXT SCANNER FIELD                
                                                                                
         L     RF,FULL             REPEAT UNTIL NO MORE SCANNER FIELDS          
         BCTR  RF,0                                                             
         ST    RF,FULL                                                          
         LTR   RF,RF                                                            
         JNZ   VROPA10                                                          
                                                                                
         USING TAOPD,R4                                                         
         LR    RF,R4               SET LENGTH OF ELEMENT                        
         LA    R4,ELEM                                                          
         SR    RF,R4                                                            
         STC   RF,TAOPLEN                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES OVERSCALE AMOUNT                           *         
*        ON ENTRY ... TGUSCDE = OVERSCALE AMOUNT USE                  *         
*                     ELEM    = OVERSCALE AMOUNT ELEMENT              *         
*                     AIO     = A(CAST RECORD)                        *         
***********************************************************************         
                                                                                
VOA      NTR1                                                                   
         CLI   ELEM,TAOAELQ        IF VALIDATING OVERSCALE AMOUNT ...           
         JNE   XIT                                                              
                                                                                
         CLC   TGUSCDE,ALLUSES     ENSURE USE CODE IS NOT ALL                   
         JE    ERRFLD                                                           
         CLC   TGUSCDE,=C'ARE'     OR ARE                                       
         JE    ERRFLD                                                           
                                                                                
         USING TAOPD,R4                                                         
         L     R4,AIO              IF OVERSCALE PERCENTAGE ELEMENT              
         MVI   ELCODE,TAOPELQ      EXISTS ...                                   
         BRAS  RE,GETEL                                                         
         JNE   VOA20                                                            
                                                                                
         ZIC   R1,TAOPNUM          R1 = # SUB ELEMENTS                          
         LA    R4,TAOPSBEL         R4 = A(FIRST SUB ELEMENT)                    
         DROP  R4                                                               
                                                                                
         USING TAOPSBEL,R4                                                      
VOA10    CLC   TGUSCDE,TAOPUSE     ENSURE USE IS NOT DEFINED AS                 
         JE    ERRFLD              AN OVERSCALE PERCENTAGE                      
                                                                                
         LA    R4,L'TAOPSBEL(R4)   BUMP R4 TO NEXT SUB ELEMENT                  
         BCT   R1,VOA10            REPEAT UNTIL NO MORE SUB-ELEMENTS            
         DROP  R4                                                               
                                                                                
VOA20    GOTOR PRGRRFLD,DMCB,TGUSCDE                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES OVERSCALE 2ND PERCENTAGE USE CODE          *         
*        ON ENTRY ... TGUSCDE = OVERSCALE AMOUNT USE                  *         
*                     ELEM    = OVERSCALE 2ND PERCENTAGE ELEMENT      *         
*                     AIO     = A(CAST RECORD)                        *         
***********************************************************************         
                                                                                
VOP2     NTR1                                                                   
         CLI   ELEM,TAO2ELQ        IF VALIDATING OVERSCALE SECOND               
         JNE   XIT                 PERCENTAGE ...                               
                                                                                
         USING TAOPD,R4                                                         
         L     R4,AIO              ENSURE THAT OVERSCALE PERCENTAGE             
         MVI   ELCODE,TAOPELQ      ELEMENT EXISTS                               
         BRAS  RE,GETEL                                                         
         JNE   ERRFLD                                                           
                                                                                
         ZIC   R1,TAOPNUM          R1 = # SUB ELEMENTS                          
         LA    R4,TAOPSBEL         R4 = A(FIRST SUB ELEMENT)                    
         DROP  R4                                                               
                                                                                
         USING TAOPSBEL,R4                                                      
VOP210   CLC   TAOPUSE,TGUSCDE     ALLOW USE IF IT MATCHES OVERSCALE            
         JE    VOP230              PERCENTAGE EXACTLY                           
         CLC   TAOPUSE,ALLUSES     ALLOW ANY USE IS OVERSCALE                   
         JE    VOP230              PERCENTAGE IS ALL                            
                                                                                
         TM    TGUSSTAT,SESSION    ALLOW ANY REUSE IF OVERSCALE                 
         JO    VOP220              PERCENTAGE IS ARE                            
         CLC   TAOPUSE,=C'ARE'                                                  
         JE    VOP230                                                           
         DROP  R4                                                               
                                                                                
VOP220   LA    R4,L'TAOPSBEL(R4)   BUMP R3 TO NEXT SUB ELEMENT                  
         BCT   R1,VOP210           REPEAT UNTIL NO MORE SUB-ELEMENTS            
         J     ERRFLD                                                           
                                                                                
         USING TAOAD,R4                                                         
VOP230   L     R4,AIO              IF OVERSCALE AMOUNT ELEMENT                  
         MVI   ELCODE,TAOAELQ      EXISTS ...                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         ZIC   R1,TAOANUM          R1 = # SUB ELEMENTS                          
         LA    R4,TAOASBEL         R4 = A(FIRST SUB ELEMENT)                    
         DROP  R4                                                               
                                                                                
         USING TAOASBEL,R4                                                      
VOP240   CLC   TGUSCDE,TAOAUSE     ENSURE USE IS NOT DEFINED AS                 
         JE    ERRFLD              AN OVERSCALE AMOUNT                          
                                                                                
         LA    R4,L'TAOASBEL(R4)   BUMP R4 TO NEXT SUB ELEMENT                  
         BCT   R1,VOP240           REPEAT UNTIL NO MORE SUB-ELEMENTS            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES OVERSCALE PERCENTAGE                       *         
*        ON ENTRY ... ELEM    = OVERSCALE PERCENTAGE ELEMENT          *         
*                     4(R1)   = OVERSCALE PERCENTAGE                  *         
***********************************************************************         
                                                                                
VOP      NTR1                                                                   
         CLI   ELEM,TAOAELQ        IF VALIDATING OVERSCALE PERCENTAGE           
         JE    XIT                                                              
         CLC   4(4,R1),=F'99999'   ENSURE THAT PERCENTAGE DOES NOT              
         JH    ERRFLD              EXCEED 999.99                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
ALLUSES  DC    CL3' '                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WHEN A MUSICIAN IS ADDED, CHANGED OR DELETED TO/FROM A       *         
*        MUSIC CONTRACT (TYPE M COMMERCIAL), ROUTINE UPDATES ALL      *         
*        LINKED CAST WITH THE LATEST INFORMATION                      *         
*        ON ENTRY ... AIO  = AIO1                                     *         
*                     AIO1 = A(MUSIC CONTRACT CAST RECORD)            *         
***********************************************************************         
                                                                                
UPDMUS   NTR1  BASE=*,LABEL=*                                                   
         TM    TGSYSTAT,TASYSMUS   IF NEW MUSIC RULES ENABLED                   
         JZ    XIT                                                              
         CLI   CMCLTYP,CTYMUS      AND COMMERCIAL TYPE IS MUSIC ...             
         JNE   XIT                                                              
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTTRK))                                     
         JNE   XIT                                                              
                                                                                
         XC    WORK(L'ORIGTRKS),WORK                                            
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         CLI   TAFNNAME,C'*'                                                    
         JNE   UM10                                                             
         MVC   WORK(L'TRACKS),TRACKS                                            
         J     UM20                                                             
UM10     ZIC   RE,TAFNLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         B     *+10                SAVE THE MUSICIAN'S UPDATED TRACK            
         MVC   WORK(0),TAFNNAME    LIST IN WORK                                 
         DROP  R4                                                               
                                                                                
UM20     GOTO1 L1NOTL2,DMCB,ADDTRKS,WORK,ORIGTRKS SAVE ANY TRACKS THAT          
         GOTO1 (RF),(R1),DELTRKS,ORIGTRKS,WORK    WERE ADDED/DELETED            
                                                                                
         USING TLCAD,R4                                                         
         L     R4,AIO1                                                          
                                                                                
         CLI   MODE,XRECDEL        IF CAST RECORD HAS JUST BEEN                 
         JNE   UM30                DELETED                                      
         XC    ADDTRKS,ADDTRKS     PREPARE TO DELETE ALL ASSOCIATIONS           
         MVC   DELTRKS,WORK                                                     
         XC    WORK,WORK                                                        
         LA    R4,PPBLOCK                                                       
                                                                                
UM30     MVC   TGCOM,TLCACOM       SAVE CONTRACT'S INTERNAL COMMERCIAL          
         MVC   TGCSORT,TLCASORT    NUMBER AND MUSICIAN'S SEQ NUMBER             
         DROP  R4                                                               
                                                                                
         CLI   MODE,XRECREST       IF CAST RECPRD HAS JUST BEEN                 
         JNE   UM40                RESTORED                                     
         MVC   ADDTRKS,WORK        PREPARE TO ALL ALL ASSOCIATIONS              
         XC    DELTRKS,DELTRKS                                                  
         XC    WORK,WORK                                                        
                                                                                
UM40     MVC   AIO,AIO2            PREPARE TO DO ALL WORK IN AIO2               
                                                                                
***********************************************************************         
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOTCDQ    READ KEYS FOR ALL COMMERCIALS THAT           
         MVC   TLCOTMCO,TGCOM      ARE ATTACHED TO THIS MUSIC CONTRACT          
         GOTO1 HIGH                                                             
         J     UM60                                                             
UM50     GOTO1 SEQ                                                              
UM60     CLC   KEY(TLCOTTRK-TLCOPD),KEYSAVE                                     
         JNE   UM90                                                             
         MVC   SVKEY,KEY                                                        
         DROP  R3                                                               
*                                  BUILD LIST OF ADDED CONTRACT TRACKS          
         BAS   RE,UMBACT           THAT APPLY TO THIS COMMERCIAL                
                                                                                
***********************************************************************         
                                                                                
         USING TLCAPD,R3                                                        
         XC    KEY,KEY             SEE IF CAST FROM TYPE M ALREADY              
         MVI   TLCAPCD,TLCATCDQ    EXISTS ON THE COMMERCIAL                     
         MVC   TLCATMCO,TGCOM                                                   
         MVC   TLCATMCS,TGCSORT+4                                               
         MVC   TLCATCOM,SVKEY+TLCOTCOM-TLCOPD                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLCATSEQ-TLCAPD),KEYSAVE                                     
         JE    UM70                                                             
         DROP  R3                                                               
                                                                                
         BAS   RE,UMADDCST         IF IT DOES NOT, ADD IT                       
         J     UM80                                                             
                                                                                
UM70     BAS   RE,UMUPDCST         IF IT DOES, UPDATE IT                        
                                                                                
***********************************************************************         
                                                                                
UM80     MVC   KEY,SVKEY           RESTORE COMMERCIAL READ SEQUENCE             
         GOTO1 HIGH                                                             
         J     UM50                                                             
                                                                                
***********************************************************************         
                                                                                
UM90     MVC   AIO,AIO1            RESTORE AIO AND EXIT                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE BUILDS LIST OF ADDED CONTRACT TRACKS (ACOTRKS)       *         
*        THAT APPLY TO THE CURRENT COMMERCIAL                         *         
*        ON ENTRY ... KEY = KEY OF CURRENT COMMERCIAL                 *         
***********************************************************************         
                                                                                
UMBACT   NTR1                                                                   
         XC    ACOTRKS,ACOTRKS                                                  
         CLI   ADDTRKS,0           IF ANY TRACKS ARE BEING ADDED TO             
         JE    NO                  THE MUSIC CONTRACT ...                       
         LA    R2,ACOTRKS                                                       
                                                                                
         GOTO1 GETREC              GET CURRENT COMMERCIAL RECORD                
                                                                                
         USING TATRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ      READ THROUGH ALL OF THE COMMERCIAL'S         
         BRAS  RE,GETEL            TRACKS                                       
         J     *+8                                                              
UMBACT10 BRAS  RE,NEXTEL                                                        
         JNE   YES                                                              
                                                                                
         CLC   TATRCOM,TGCOM       REJECT THOSE THAT ARE NOT FOR THIS           
         JNE   UMBACT10            MUSIC CONTRACT                               
                                                                                
         LA    RE,ADDTRKS                                                       
UMBACT20 CLC   TATRTRK,0(RE)       SAVE ANY TRACKS THAT WERE ADDED              
         JNE   UMBACT30            TO THE MUSIC CONTRACT THAT ARE               
         MVC   0(1,R2),0(RE)       VALID FOR THE CURRENT COMMERCIAL             
         LA    R2,1(R2)                                                         
         J     UMBACT10                                                         
UMBACT30 CLI   1(RE),0                                                          
         JE    UMBACT10                                                         
         LA    RE,1(RE)                                                         
         J     UMBACT20                                                         
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE UPDATES COMMERCIAL CAST RECORD WITH THE CURRENT      *         
*        STATE OF THE LINKED MUSIC CONTRACT CAST RECORD               *         
*        ON ENTRY ... R3 = A(COMMERCIAL CAST KEY)                     *         
***********************************************************************         
                                                                                
UMADDCST NTR1                      IF CAST DOES NOT EXIST FOR COMM'L            
         CLI   ACOTRKS,0           BUT TRACKS WERE ADDED THAT ARE ON            
         JE    XIT                 THE COMMERCIAL ...                           
                                                                                
         USING TLCOPD,R3                                                        
         XR    R0,R0                                                            
         XC    KEY,KEY             READ COMMERCIAL RECORD                       
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVKEY+TLCOTCOM-TLCOPD                                   
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSEQ))                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R4,TGELEM                                                        
         ZICM  R0,TANUNXTC,2       SAVE NEXT CAST SEQ NUMBER                    
                                                                                
         LR    RE,R0                                                            
         AHI   RE,1                AND UPDATE COMMERCIAL RECORD                 
         STCM  RE,3,TANUNXTC       WITH NEXT CAST SEQ NUMBER                    
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
                                                                                
         USING TLCAD,RE                                                         
UMAC30   L     RE,AIO2             INITIALIZE CAST RECORD                       
         XC    0(255,RE),0(RE)     TO AIO2                                      
         L     RF,AIO1                                                          
         MVC   TLCAKEY,0(RF)                                                    
         MVC   TLCACOM,SVKEY+TLCOTCOM-TLCOPD                                    
         STCM  R0,3,TLCASEQ        ALTER KEY TO MAKE IT FOR THE                 
         MVI   TLCALEN+1,41        TO THE ATTACHED COMMERCIAL                   
         DROP  RE                                                               
                                                                                
         BAS   RE,UMCPYCST         COPY CAST INFO FROM MUSIC CONTRACT           
         BAS   RE,UMADDTRK         ADD TRACKS THAT ARE BEING ADDED              
                                                                                
UMAC40   BRAS  RE,MYADDREC         ADD NEW CAST RECORD                          
         LA    R0,PPBLOCK                                                       
         LHI   R1,L'PPBLOCK                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 ADDPTRS,DMCB,(8,PPBLOCK),UPBLOCK                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE UPDATES COMMERCIAL CAST RECORD WITH THE CURRENT      *         
*        STATE OF THE LINKED MUSIC CONTRACT CAST RECORD               *         
*        ON ENTRY ... R3 = A(COMMERCIAL CAST KEY)                     *         
***********************************************************************         
                                                                                
UMUPDCST NTR1                                                                   
         MVI   RDUPDATE,C'Y'       READ COMMERCIAL CAST RECORD                  
         GOTO1 GETREC              FOR UPDATE                                   
         GOTO1 SAVPTRS,DMCB,PPBLOCK                                             
                                                                                
         USING TLCAD,R4                                                         
         LA    R4,NEWCAKEY                                                      
         L     RE,AIO              INITIALIZE NEW KEY WITH ORIGINAL             
         MVC   TLCAKEY,0(RE)                                                    
                                                                                
         L     RE,AIO1             MOVE IN NEW SORT KEY AND CATEGORY            
         MVC   TLCASORT(4),TLCASORT-TLCAD(RE)                                   
         MVC   TLCACAT,TLCACAT-TLCAD(RE)                                        
         DROP  R4                                                               
                                                                                
         BAS   RE,UMCPYCST         COPY CAST INFO FROM MUSIC CONTRACT           
         BAS   RE,UMDELTRK         DELETE TRACKS THAT ARE BEING DELETED         
         BAS   RE,UMADDTRK         ADD TRACKS THAT ARE BEING ADDED              
                                                                                
         MVI   TGBYTE3,8           INITIALIZE CHANGE/DELETE INDICATOR           
                                                                                
         USING TLCAD,R4                                                         
         L     R4,AIO                                                           
         CLC   TLCAKEY,NEWCAKEY    IF KEY HAS NOT CHANGED                       
         JNE   UMUC10                                                           
         MVI   ELCODE,TATRELQ      AND TRACK ASSOCIATIONS REMAIN                
         BRAS  RE,GETEL                                                         
         JNE   UMUC10                                                           
         GOTO1 PUTREC              PUT CAST RECORD BACK TO FILE                 
         GOTO1 ADDPTRS,DMCB,(TGBYTE3,PPBLOCK),UPBLOCK                           
         J     XIT                                                              
                                                                                
         USING TLDRD,R3                                                         
UMUC10   L     R4,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   TLDRKEY,0(R4)       DELETE THE KEY                               
         MVI   RDUPDATE,C'Y'       AND PREPEARE TO DELETE THE                   
         GOTO1 HIGH                CAST RECORDS AND ITS PASSIVES                
         CLC   TLDRKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         OI    TLDRSTAT,TLCASDCG                                                
         GOTO1 WRITE                                                            
         OI    TLCASTAT,TLCASDCG                                                
         MVI   TGBYTE3,X'40'                                                    
         GOTO1 PUTREC              PUT CAST RECORD BACK TO FILE                 
         GOTO1 ADDPTRS,DMCB,(TGBYTE3,PPBLOCK),UPBLOCK                           
         DROP  R3                                                               
                                                                                
         GOTOR HASTRKAS,DMCB,AIO   IF CAST HAS TRACK ASSOCIATIONS               
         JNE   XIT                                                              
         MVC   TLCAKEY,NEWCAKEY    NOW GO RE-ADD WITH NEW KEY                   
         MVI   TLCASTAT,0                                                       
         BRAS  RE,MYADDREC         ADD NEW CAST RECORD                          
         LA    R0,PPBLOCK                                                       
         LHI   R1,L'PPBLOCK                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 ADDPTRS,DMCB,(8,PPBLOCK),UPBLOCK                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ADDS CURRENT TRACK TO LIST (P1) IF IT IS IN PROVIDED *         
*        LIST (P2)                                                    *         
*        ON ENTRY ... R3 = A(COMMERCIAL TRACK KEY)                    *         
***********************************************************************         
                                                                                
         USING TLCOPD,R3                                                        
UMSV     NTR1                                                                   
         L     R4,4(R1)            R4=A(PROVIDED LIST)                          
         CLI   0(R4),0                                                          
         JE    XIT                                                              
                                                                                
UMSV10   CLC   TLCOTTRK,0(R4)      IF TRACK IS BEING ADDED                      
         JE    UMSV20                                                           
         CLI   1(R4),0                                                          
         JE    XIT                                                              
         LA    R4,1(R4)                                                         
         J     UMSV10                                                           
                                                                                
UMSV20   L     R2,0(R1)            R2=A(LIST TO BUILD)                          
UMSV30   CLI   0(R2),0                                                          
         JE    UMSV40                                                           
         CLC   TLCOTTRK,0(R2)      AND THIS VERSION IS NOT ALREADY              
         JE    XIT                 SAVED IN THE "ADD VERSIONS" LIST             
         LA    R2,1(R2)                                                         
         J     UMSV30                                                           
UMSV40   MVC   0(1,R2),TLCOTVER    SAVE IT THERE NOW                            
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE COPIES INFO FROM TYPE M COMMERCIAL'S CAST TO         *         
*        COMMERCIAL'S CAST RECORD                                     *         
*        ON ENTRY ... AIO1=A(TYPE M CAST RECORD)                      *         
*                     AIO=A(ATTACHED COMMERCIAL'S CAST RECORD)        *         
***********************************************************************         
                                                                                
UMCPYCST NTR1                                                                   
         XC    TGTHREE,TGTHREE                                                  
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      GET ATTACHED COMMERCIAL'S CAST               
         BRAS  RE,GETEL            DETAILS ELEMENT                              
         JNE   UMCC10                                                           
         MVC   TGTHREE,TACAYEAR    AND SAVE OFF CONTRACT YEAR                   
         DROP  R4                                                               
                                                                                
UMCC10   MVI   ELCODE,TAOPELQ      REMOVE ELEMENTS FROM ATTACHED                
         GOTO1 REMELEM             COMMERCIAL'S CAST RECORD                     
         MVI   ELCODE,TAOAELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAO2ELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TACAELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTTRK))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTVER))                                     
                                                                                
         L     R4,AIO1                                                          
         MVI   ELCODE,0            READ THROUGH ELEMENTS FROM                   
         BRAS  RE,GETEL            TYPE M COMMERCIAL'S CAST RECORD              
         J     *+8                                                              
UMCC20   BRAS  RE,NEXTEL                                                        
         JNE   UMCC40                                                           
         CLI   0(R4),TAOPELQ       COPY CERTAIN ONES FROM TO THE                
         JE    UMCC30              COMMERCIAL'S CAST RECORD                     
         CLI   0(R4),TAOAELQ                                                    
         JE    UMCC30                                                           
         CLI   0(R4),TAO2ELQ                                                    
         JE    UMCC30                                                           
         CLI   0(R4),TACAELQ                                                    
         JE    UMCC30                                                           
         CLI   0(R4),TACMELQ                                                    
         JNE   UMCC20                                                           
UMCC30   XC    ELEMENT,ELEMENT                                                  
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ELEMENT(0),0(R4)                                                 
         GOTO1 ADDELEM             AND ADD THEM                                 
         J     UMCC20                                                           
                                                                                
         USING TACAD,R4                                                         
UMCC40   OC    TGTHREE,TGTHREE     IF CAST ALREADY EXISTED ON                   
         JZ    XIT                 NON-TYPE M                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      GET ATTACHED COMMERCIAL'S CAST               
         BRAS  RE,GETEL            DETAILS ELEMENT                              
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TACAYEAR,TGTHREE    AND RESTORE CONTRACT YEAR                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE DELETES TRACK ELEMENTS FROM CAST RECORD              *         
***********************************************************************         
                                                                                
UMDELTRK NTR1                                                                   
         CLI   DELTRKS,0           IF ANY TRACKS ARE BEING DELETED ...          
         JE    XIT                                                              
                                                                                
         USING TATRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ      READ THROUGH ALL TRACK                       
         BRAS  RE,GETEL            ASSOCIATION ELEMENTS                         
         J     *+8                                                              
UMDT10   BRAS  RE,NEXTEL                                                        
         JNE   UMDT40                                                           
         LA    RE,DELTRKS                                                       
UMDT20   CLC   TATRTRK,0(RE)                                                    
         JE    UMDT30                                                           
         CLI   1(RE),0                                                          
         JE    UMDT10                                                           
         LA    RE,1(RE)                                                         
         J     UMDT20                                                           
UMDT30   MVI   TATREL,X'FF'        MARK THEM FOR DELETE                         
         J     UMDT10                                                           
         DROP  R4                                                               
                                                                                
UMDT40   MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             AND DELETE THEM                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ADDS TRACK ELEMENTS TO THE CAST RECORD               *         
***********************************************************************         
                                                                                
UMADDTRK NTR1                                                                   
         CLI   ACOTRKS,0           IF ANY TRACKS ARE ADDED ...                  
         JE    XIT                                                              
         LA    R1,ACOTRKS                                                       
                                                                                
         LA    R2,BLOCK                                                         
         XC    BLOCK(L'ACOTRKS),BLOCK                                           
                                                                                
         USING TATRD,R4                                                         
UMAT10   L     R4,AIO              ... NARROW DOWN THE LIST TO                  
         MVI   ELCODE,TATRELQ      JUST THOSE TRACKS THAT ARE NOT               
         BRAS  RE,GETEL            YET ON THIS CAST RECORD                      
         J     *+8                                                              
UMAT20   BRAS  RE,NEXTEL                                                        
         JNE   UMAT30                                                           
         CLC   TATRTRK,0(R1)                                                    
         JNE   UMAT20                                                           
         J     UMAT40                                                           
         DROP  R4                                                               
                                                                                
UMAT30   MVC   0(1,R2),0(R1)                                                    
         LA    R2,1(R2)                                                         
UMAT40   CLI   1(R1),0                                                          
         JE    UMAT50                                                           
         LA    R1,1(R1)                                                         
         J     UMAT10                                                           
                                                                                
UMAT50   CLI   BLOCK,0             IF ANY ADDED TRACKS ARE NOT YET              
         JE    XIT                 ON THIS CAST RECORD ...                      
         LA    R2,BLOCK                                                         
                                                                                
         USING TATRD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     INITIALIZE MUSIC TRACK ELEMENT               
         MVI   TATREL,TATRELQ                                                   
         MVI   TATRLEN,TATRLNQ                                                  
         MVC   TATRCOM,TGCOM                                                    
         MVC   TATRCSQ,TGCSORT+4                                                
                                                                                
UMAT60   MVC   TATRTRK,0(R2)       ADD IN SPECIFIC TRACK INFORMATION            
         GOTO1 ADDELEM             AND ADD ELEMENT TO CAST RECORD               
         CLI   1(R2),0                                                          
         JE    XIT                                                              
         LA    R2,1(R2)                                                         
         J     UMAT60                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILD LISTS OF ENTRIES THAT ARE IN 1ST PROVIDED LIST *         
*        BUT NOT THE 2ND LIST                                         *         
*        ON ENTRY ... P1=A(LIST TO BUILD)                             *         
*                     P2=A(1ST COMPARISON LIST)                       *         
*                     P3=A(2ND COMPARISON LIST)                       *         
***********************************************************************         
                                                                                
L1NOTL2  NTR1                                                                   
         L     R2,0(R1)            R2=A(LIST TO BUILD)                          
         XC    0(8,R2),0(R2)                                                    
                                                                                
         L     R3,4(R1)            R3=A(1ST COMPARISON LIST)                    
         L     R4,8(R1)            R4=A(2ND COMPARISON LIST)                    
                                                                                
         CLI   0(R4),0             IF SECOND LIST IS EMPTY                      
         JNE   L1NL210                                                          
         MVC   0(8,R2),0(R3)       ADD ALL ENTRIES IN 1ST LIST                  
         J     XIT                 TO LIST TO BUILD AND EXIT                    
                                                                                
L1NL210  LR    RF,R4                                                            
                                                                                
L1NL220  CLC   0(1,R3),0(RF)       IF ENTRY IS IN BOTH LISTS                    
         JE    L1NL260             DO NOT ADD IT TO LIST TO BUILD               
                                                                                
         CLI   1(RF),0             IF ENTRY IS NOT IN 2ND LIST                  
         JNE   L1NL250                                                          
         LR    R1,R2                                                            
L1NL230  CLI   0(R1),0                                                          
         JE    L1NL240                                                          
         LA    R1,1(R1)                                                         
         J     L1NL230                                                          
L1NL240  MVC   0(1,R1),0(R3)       ADD ENTRY TO LIST TO BUILD                   
         J     L1NL260                                                          
                                                                                
L1NL250  LA    RF,1(RF)                                                         
         J     L1NL220                                                          
                                                                                
L1NL260  CLI   1(R3),0                                                          
         JE    XIT                                                              
         LA    R3,1(R3)                                                         
         J     L1NL210                                                          
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD RECORD                                        *         
*        ON ENTRY ... AIO2 = A(RECORD TO ADD)                         *         
***********************************************************************         
                                                                                
MYADDREC NTR1  BASE=*,LABEL=*                                                   
         USING TLDRD,R4                                                         
         L     R2,AIO2                                                          
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TLDRKEY,0(R2)       MOVE RECORD KEY TO DIRECTORY KEY             
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                LOOK FOR RECORD ALREADY ON FILE              
                                                                                
         CLC   TLDRKEY,KEYSAVE     IF RECORD EXISTS                             
         BNE   MAR10                                                            
         TM    TLDRSTAT,X'80'      IT HAD BETTER BE DELETED                     
         JO    *+6                                                              
         DC    H'0'                                                             
         NI    TLDRSTAT,X'3F'      TURN OFF CAST DELETED BITS                   
         GOTO1 WRITE               AND WRITE IT BACK                            
                                                                                
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         MVC   AIO,AIO3            SET ALT. I/O AREA                            
         GOTO1 GETREC              GET THE RECORD SO THAT WE CAN ...            
         ST    R2,AIO                                                           
                                                                                
         USING TLCAD,R2                                                         
         NI    TLCASTAT,X'FF'-TLCASDCG                                          
         GOTO1 PUTREC              WRITE NEW ONE BACK OVER DELETED ONE          
         J     XIT                                                              
                                                                                
MAR10    MVC   TLDRKEY,KEYSAVE                                                  
         GOTO1 ADDREC              OK TO ADD THE RECORD                         
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DETERMINES IF RECORD AT P1 HAS A TRACK ASSOCIATION   *         
*        ON ENTRY ... P1=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
HASTRKAS NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)                                                         
         MVI   ELCODE,TATRELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES FIELD POINTED AT BY R2 HAS INPUT             *         
*        RETURNS VALUE IN WORK                                                  
***********************************************************************         
                                                                                
MYANY    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,STLENPRO         IF FIELD IS PROECTED                         
         JE    XIT                 SET LENGTH OF FIELD                          
                                                                                
         GOTO1 ANY                 IF FIELD IS NOT PROTECTED                    
         J     XIT                 CALL REGULAR ANY                             
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SET LENGTH OF DATA IN PROTECTED FIELD AND            *         
*        RETURNS VALUE IN WORK                                                  
***********************************************************************         
                                                                                
STLENPRO NTR1  BASE=*,LABEL=*                                                   
         TM    1(R2),X'20'         IF FIELD IS PROECTED                         
         JZ    NO                                                               
                                                                                
         XC    WORK,WORK           COPY IT INTO WORK                            
         ZIC   RF,0(R2)                                                         
         SHI   RF,9                                                             
         TM    1(R2),X'02'                                                      
         JZ    *+8                                                              
         SHI   RF,8                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         OC    WORK,=64C' '                                                     
                                                                                
         LHI   R3,64                                                            
         LA    RE,WORK             PREPARE TO SET FIELD LENGTH                  
         LR    RF,R3                                                            
         LA    R4,0(RF,RE)                                                      
         SHI   R4,1                                                             
                                                                                
SLP10    CLI   0(R4),C' '          FINDS LAST NON-BLANK CHAR                    
         JH    SLP20               THAT'S THE NEW LENGTH                        
         SHI   R4,1                PREVIOUS CHAR                                
         SHI   RF,1                DECREMENT LENGTH                             
         JNZ   SLP10                                                            
                                                                                
SLP20    STC   RF,5(R2)            SET FIELD LENGTH                             
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES FIELD FROM ABOVE IF NECESSARY                 *         
*        AND IF POSSIBLE.    R2=A(FIELD TO COPY TO)                   *         
*                                  P1=A(FIELD ABOVE)                  *         
***********************************************************************         
                                                                                
COPYDOWN NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,0(R1)         TEST THERE IS A FIELD ABOVE                  
         JZ    XIT                                                              
         TM    1(RF),X'20'         THAT IS NOT PROTECTED                        
         JO    XIT                                                              
         CLI   5(R2),0             AND THIS FIELD IS EMPTY                      
         JNE   XIT                                                              
         TM    1(R2),X'20'         AND NOT PROTECTED                            
         JO    XIT                                                              
         CLI   SCRFLAG,C'E'        IF SCREEN IS EXTENSION SCREEN                
         JE    XIT                 THEN COPY DOWN DOESN'T MAKE SENSE            
         ZIC   R1,5(RF)            R1=LENGTH OF INPUT FOR FLD ABOVE             
         STC   R1,5(R2)            MOVE LEN AND DATA TO THIS FIELD              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   8(0,R2),8(RF)                                                    
         NI    4(R2),X'DF'         INVALIDATE AS IF USER INPUT                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CLEAR SCREEN LINE AND SET SSN FIELD TO UNPROTECTED TO ALLOW  *         
*        A RECORD TO BE ADDED ON THIS LINE                            *         
***********************************************************************         
                                                                                
CLRLINE  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTLDEL      IF USER WANTS DELETED RECORDS                
         JNE   CL10                THEN PROTECT ALL RECORD DATA FIELDS          
         GOTO1 FLDVAL,DMCB,(X'08',ASSNFLD),(X'10',ALASTUNP)                     
         J     CL20                                                             
*                                  ELSE UNPROTECT ALL DATA FIELDS AND           
*                                      SET TO NORMAL INTENSITY                  
CL10     GOTO1 FLDVAL,DMCB,(X'04',ASSNFLD),(X'10',ALASTUNP)                     
                                                                                
*                                  SET ALL DATA FIELDS VALID                    
         GOTO1 FLDVAL,DMCB,(X'20',ASSNFLD),ALASTUNP                             
                                                                                
CL20     L     R2,ASSNFLD          CLEAR UNPROTECTED FIELDS                     
         L     R3,AWIDFLD                                                       
         TWAXC (R2),(R3),PROT=Y                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TATRNTR                                                        
       ++INCLUDE TAMQHFR                                                        
       ++INCLUDE TAW4LCRP                                                       
       ++INCLUDE TAGEN1AD                                                       
         EJECT                                                                  
NAMESD   DSECT                                                                  
         DS    XL8                                                              
NAMSSN   DS    CL16                W4 NAME                                      
         DS    XL6                                                              
NAMLOCL  DS    0CL16               W4 LOCAL                                     
NAMNCDE  DS    CL16                AGENT NAME                                   
         DS    XL3                                                              
NAMLCHG  DS    0CL17               LAST CHANGED INFO                            
NAMCORP  DS    CL16                CORPORATION NAME                             
         DS    XL2                                                              
NAMEXP   DS    0CL8                EXPIRATION DATE                              
         DS    XL3                                                              
NAMGUAR  DS    CL6                 GUARANTEE INDICATOR                          
         SPACE 3                                                                
HEAD3D   DSECT                     HEADLINE 3 DSECT                             
H3AGYT   DS    CL(L'SCLAGYT)       AGENCY TAG                                   
         DS    CL3                                                              
H3AGY    DS    CL(L'SCLAGY)        AGENCY                                       
         ORG   HEAD3D+L'HEAD3                                                   
H3CIDT   DS    CL(L'SCLCIDT)       COMMERCIAL ID TAG                            
         DS    CL2                                                              
H3CID    DS    CL(L'SCLCID)        COMMERCIAL ID                                
         DS    CL2                                                              
H3CIDN   DS    CL(L'SCLCIDN)       COMMERCIAL ID NAME                           
         SPACE 3                                                                
PLINED   DSECT                     PRINT LINE DSECT                             
PLSSN    DS    CL9                 SSN                                          
         DS    C                                                                
PLCAT    DS    CL3                 CATEGORY                                     
         DS    C                                                                
PLONOF   DS    CL3                 CAMERA (ON/OFF)                              
         DS    C                                                                
PLUNIT   DS    CL3                 TAX UNIT                                     
         DS    C                                                                
PLNCDE   DS    CL4                 AGENT                                        
         DS    C                                                                
PLUNLO   DS    CL7                 UNION/LOCAL                                  
         DS    C                                                                
PLYEAR   DS    CL3                 YEAR                                         
         DS    C                                                                
PLLIFT   DS    CL1                 LIFT (Y/N)                                   
         DS    CL2                                                              
PLCORP   DS    CL1                 CORPORATION                                  
         DS    CL2                                                              
PLOP     DS    CL4                 OVERSCALE PERCENTAGE                         
         DS    C                                                                
PLGUAR   DS    CL4                 GUARANTEE                                    
         DS    C                                                                
PLDBL    DS    CL2                 DOUBLES                                      
         DS    CL2                                                              
PLFRST   DS    CL8                 FIRST SERVICES DATE                          
         DS    CL1                                                              
PLTRKS   DS    CL7                 TRACKS                                       
         EJECT                                                                  
*              DSECT TO COVER TABLE OF SCREEN FIELDS THAT, WHEN                 
*              CHANGED TRIGGER PDATED HOLDING FEES                              
         SPACE 1                                                                
HLDTABD  DSECT                                                                  
HTSCRN   DS    XL1                 SPECIFIC SCREEN FOR FIELD, ELSE 00           
HTFLDA   DS    AL2                 ADDRESS OF SCREEN FIELD                      
HTLNQ    EQU   *-HLDTABD                                                        
         EJECT                                                                  
*              DSECT TO COVER TABLE THAT DETERMINES WHICH COMMERCIAL            
*              RECORD THE VERSION CODE IS ON                                    
         SPACE 1                                                                
VINDEXD  DSECT                                                                  
VINDUPLM DS    X                 RECORD'S UPPER LIMIT                           
VINDEQUT DS    X                 RECORD'S EQUATE                                
VINDLNQ  EQU   *-VINDEXD                                                        
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
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
       ++INCLUDE TASCR1AD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR0AD                                                       
         ORG   SCLWORK                                                          
SVAYSTA6 DS    X                                                                
SVCISTA2 DS    X                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122TAGEN1A   02/16/17'                                      
         END                                                                    
