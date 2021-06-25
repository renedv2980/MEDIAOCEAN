*          DATA SET TAGEND2    AT LEVEL 025 AS OF 07/15/14                      
*PHASE T702D2C                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T702D2 - RECORD LIST'                                           
T702D2   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702D2,R7,RR=R3                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,SETFILE        SET FILE                                     
         BNE   RECL5                                                            
         MVC   SYSDIR,SVSYSDIR                                                  
         MVC   SYSFIL,SVSYSFIL                                                  
         CLI   SETCHK,C'Y'         IF NEED CHECK DIR/FILE                       
         BNE   XIT                                                              
         MVC   SYSDIR,=CL8'CHKDIR' SET IT                                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         B     XIT                                                              
         SPACE 1                                                                
RECL5    CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BAS   RE,LREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LVALREC        VALIDATE CHANGE OF LISTED RECORD             
         BNE   *+12                                                             
         BAS   RE,LVREC                                                         
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       LINE COUNTER                                 
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,MYSPECS          SET A(SPECS)                                 
         ST    R0,SPECS                                                         
         LA    R0,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R0,HEADHOOK                                                      
*                                                                               
         LA    R1,LRCDATAH         SET A(SCREEN LINE)                           
         ST    R1,ATHISLST                                                      
         BAS   RE,LREC             LIST RECORDS                                 
*                                                                               
         CP    COUNTER,=P'0'       PRINT TOTAL RECORDS                          
         BE    XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  COUNTER,(12,P),ALIGN=LEFT,COMMAS=YES                             
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(7,R1),=C'RECORDS'                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         L     R1,=V(DUMMY)        SET A(WHERE TO LOAD DSECT PHASES)            
         A     R1,RELO                                                          
         ST    R1,APHASE                                                        
*                                                                               
         TM    WHEN,X'80'          IF NOT LISTING TO SCREEN                     
         BO    *+8                                                              
         NI    LRCCODEH+4,X'DF'    SET TO RE-VALIDATE ALL                       
*                                                                               
         BAS   RE,VALCODE          VALIDATE RECORD CODE FIELD                   
*                                                                               
         TM    LRCSTRTH+4,X'20'    IF START FIELD NOT PREV. VALIDATED           
         BO    VK20                                                             
         BAS   RE,VALSTRT          VALIDATE START FIELD                         
         MVI   LISTSW,0            SET TO START LIST OVER                       
         MVC   STARTKEY,SAVEKEY    SAVE START KEY                               
*                                                                               
VK20     MVI   NLISTS,17           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   SET OKAY TO RETURN EXTRA FOR EOL             
         OI    GLSTSTAT,APPLCDSP   SET I WILL DISPLAY THE LIST                  
         OI    GLSTSTAT,CHNGLIST   SET OK TO CHANGE FROM LIST                   
         MVC   LLIST,=Y(LINLNQ)    L'LIST LINE                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE RECORD CODE FIELD                            
         SPACE 1                                                                
VALCODE  NTR1                                                                   
         LA    R2,LRCCODEH         R2 = A(FIELD)                                
         MVI   SETCHK,C'N'         INIT NOT CHECK DIR/FILE                      
         TM    4(R2),X'20'                                                      
         BO    VCODE10             IF PREV. VALIDATED THEN JUST LOOK UP         
         GOTO1 ANY                                                              
         MVC   LABEL,SPACES        ELSE CLEAR LABEL                             
         NI    LRCSTRTH+4,X'DF'                                                 
         XC    SAVEKEY,SAVEKEY     BUILD NEW KEY                                
*                                                                               
         CLI   8(R2),C''''         IF STARTS WITH ' THEN IT'S EQUATE            
         BNE   VCODE20                                                          
         GOTO1 HEXIN,DMCB,9(R2),SAVEKEY,2  VALIDATE / MOVE TO KEY               
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRINV                                                           
*                                                                               
VCODE10  LA    R4,SAVEKEY          R4 = A(KEY)                                  
         BAS   RE,LOOKUP           LOOK IT UP                                   
         B     VCODE40                                                          
*                                                                               
VCODE20  MVC   LABEL,SPACES        ELSE IT MUST BE LABEL                        
         MVC   LABEL(2),=C'TL'     BUILD LABEL FOR RECORD EQUATE                
         MVC   LABEL+2(3),8(R2)                                                 
         ZIC   R1,5(R2)                                                         
         LA    RF,LABEL+2(R1)                                                   
         MVC   0(3,RF),=C'CDQ'                                                  
         BAS   RE,LOOKUP           LOOK UP LABEL                                
         L     R2,ACURRPOS         R2 = A(DSECT PHASE ENTRY)                    
         USING SRFADSEC,R2                                                      
         GOTO1 HEXIN,DMCB,SRFADTY+2,SAVEKEY,2  MOVE RECORD CODE TO KEY          
         CLI   REC24ON,C'Y'                                                     
         BNE   VCODE40                                                          
         MVC   SAVEKEY+1(1),SAVEKEY                                             
         MVI   SAVEKEY,X'24'                                                    
*                                                                               
VCODE40  MVC   ADSECT,ACURRPOS     SAVE A(DSECT FOR THIS KEY)                   
*                                                                               
         MVI   PASSIVE,C'N'                                                     
         TM    SAVEKEY,X'03'       SET PASSIVE VS. ACTIVE SWITCH                
         BZ    *+8                                                              
         MVI   PASSIVE,C'Y'                                                     
*                                                                               
         LA    R1,CHKDKEYS         DETERMINE IF KEY IS FOR CHKDIR               
VCODE50  CLC   SAVEKEY(1),0(R1)                                                 
         BE    VCODE60                                                          
         LA    R1,1(R1)                                                         
         CLI   0(R1),0             TEST END OF TABLE                            
         BE    VCODE53                                                          
         B     VCODE50                                                          
*                                                                               
VCODE53  CLI   SAVEKEY,X'24'       SPECIAL X'24' RECORD?                        
         BNE   VCODEX                                                           
         LA    R1,CHK24KYS         DETERMINE IF KEY IS FOR CHKDIR               
VCODE55  CLC   SAVEKEY+1(1),0(R1)                                               
         BE    VCODE60                                                          
         AHI   R1,1                                                             
         CLI   0(R1),0             TEST END OF TABLE                            
         BE    VCODEX                                                           
         B     VCODE55                                                          
*                                                                               
VCODE60  MVI   SETCHK,C'Y'         SET CHKDIR/CHKFIL FLAG                       
*                                                                               
VCODEX   OI    LRCCODEH+4,X'20'    SET PREVIOUSLY VALIDATED                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE START FIELD                                  
         SPACE 1                                                                
VALSTRT  NTR1                                                                   
         LA    R2,LRCSTRTH         R2 = A(FIELD)                                
         CLI   SAVEKEY,X'24'                                                    
         BE    VSTRT05                                                          
         XC    SAVEKEY+1(L'SAVEKEY-1),SAVEKEY+1                                 
         B     VSTRT10                                                          
                                                                                
VSTRT05  XC    SAVEKEY+2(L'SAVEKEY-2),SAVEKEY+2                                 
                                                                                
VSTRT10  XC    FILTKEY,FILTKEY     CLEAR FILTER KEY                             
         SPACE 1                                                                
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VSTRTX                                                           
         LA    R3,BLOCK            R3 = A(SCAN BLOCK)                           
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    ERRINV                                                           
         ZIC   R0,4(R1)            R0 = N'SCAN BLOCK ENTRIES                    
         SPACE 1                                                                
         MVC   ACURRPOS,ADSECT                                                  
         BAS   RE,CURRSRF          R2 = A(DSECT FOR THIS KEY)                   
         LA    R5,SAVEKEY          R5 = A(AREA TO BUILD START KEY)              
         SPACE 1                                                                
VSTRT20  MVC   ERRDISP,SCDISP1     SET ERROR DISPLACEMENT                       
         SPACE 1                                                                
         BAS   RE,GETNXTDS         GET NEXT DS FIELD                            
         BNE   ERRSTRT                                                          
         SPACE 1                                                                
         CLI   SCLEN1,0            IF EMPTY ENTRY ENCOUNTERED                   
         BNE   *+12                                                             
         LA    R5,FILTKEY          FURTHER INPUT IS A FILTER                    
         B     VSTRT60                                                          
         SPACE 1                                                                
         LR    R4,R5                                                            
         AH    R4,SRFADDSP         R4 = A(FIELD IN KEY)                         
         SPACE 1                                                                
         CLI   SRFADTY,C'C'        IF CHARACTER FIELD                           
         BNE   VSTRT40                                                          
         CLC   SCLEN1,FLDLEN+1     INSURE NOT TOO LONG                          
         BH    ERRFLD                                                           
         CLI   COMPL,C'Y'          IF FIELD SHOULD BE COMPLEMENTED              
         BNE   *+10                                                             
         XC    SCDATA1(20),HEXFFS  COMPLEMENT IT                                
         LH    R1,FLDLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VSTRT60                                                          
         MVC   0(0,R4),SCDATA1     MOVE TO KEY                                  
         SPACE 1                                                                
VSTRT40  CLI   INV,C'Y'            IF CURRENT FIELD IS INVOICE                  
         BNE   VSTRT50                                                          
         CLC   SCLEN1,FLDLEN+1     INSURE LENGTH IS EXACT                       
         BNE   ERRFLD                                                           
         GOTO1 TINVCON,DMCB,SCDATA1,(R4),DATCON  CVT TO INTERNAL FORMAT         
         CLI   0(R1),X'FF'                                                      
         BE    ERRFLD                                                           
         CLI   COMPL,C'Y'          IF FIELD SHOULD BE COMPLEMENTED              
         BNE   VSTRT60                                                          
         ZIC   R1,SCLEN1           DO IT                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VSTRT60                                                          
         XC    0(0,R4),HEXFFS                                                   
         SPACE 1                                                                
VSTRT50  ZIC   RF,SCLEN1           VALIDATE HEX FIELD                           
         SRA   RF,1                                                             
         CH    RF,FLDLEN           INSURE VALID LENGTH                          
         BH    ERRFLD                                                           
         IC    RF,SCLEN1                                                        
         GOTO1 HEXIN,DMCB,SCDATA1,(R4),(RF)  MOVE IT TO KEY                     
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRFLD                                                           
         CLI   COMPL,C'Y'          IF FIELD SHOULD BE COMPLEMENTED              
         BNE   VSTRT60                                                          
         L     R1,12(R1)           DO IT                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),HEXFFS                                                   
         SPACE 1                                                                
VSTRT60  LA    R3,SCANNEXT         BUMP TO NEXT SCAN BLOCK ENTRY                
         BCT   R0,VSTRT20          LOOP                                         
         SPACE 1                                                                
         MVI   ERRDISP,0           CLEAR ERROR DISPLACEMENT                     
         SPACE 1                                                                
VSTRTX   OI    LRCSTRTH+4,X'20'    SET PREVIOUSLY VALIDATED                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'23',LRCSELH),999  CLEAR/SET VERIFIED              
         SPACE 1                                                                
         MVC   LABEL,=CL8'TLDRSTAT'  LOOK UP DIRECTORY STATUS ENTRY             
         BAS   RE,LOOKUP                                                        
         MVC   ATLDRD,ACURRPOS     SAVE ITS ADDRESS                             
         SPACE 1                                                                
         BAS   RE,BLDHEADS         BUILD HEADINGS                               
         SPACE 1                                                                
         OI    DMINBTS,X'08'       SET OK TO READ DELETED                       
         LA    R4,KEY              R4 = A(KEY)                                  
         USING TLDRD,R4                                                         
         GOTO1 PGCNTL,DMCB,('NPGTBL',PGTBL),SAVEKEY,TLDRKEY  SET KEY            
         SPACE 1                                                                
         OC    TLDRKEY,TLDRKEY     IF NEW KEY NOT SET                           
         BNZ   *+10                                                             
         MVC   TLDRKEY,STARTKEY    USE START KEY                                
         SPACE 1                                                                
         GOTO1 HIGH                READ FIRST RECORD                            
         SPACE 1                                                                
LR10     CLC   TLDRCD,KEYSAVE      TEST STILL SAME KEY TYPE                     
         BNE   LRX                                                              
         CLI   KEYSAVE,X'24'       IF X'24' RECORD                              
         BNE   LR15                                                             
         CLC   TLDRCD(2),KEYSAVE   CHECK FIRST 2 BYTES                          
         BNE   LRX                                                              
LR15     MVC   SAVEKEY,TLDRKEY     SAVE THE KEY                                 
         SPACE 1                                                                
         BAS   RE,FILTER           POSSIBLE FILTER                              
         BNE   LR50                                                             
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   LR30                                                             
         BAS   RE,DISPLAY          DISPLAY TO SCREEN                            
         MVI   BYTE,C'P'           PRINT ONE LINE                               
         GOTO1 PRTSCRN,DMCB,ATHISLST,0,P-5                                      
         L     RF,ATHISLST                                                      
         LA    RF,LINDAH-LINED(RF)                                              
         GOTO1 FLDVAL,DMCB,(X'01',ATHISLST),(RF)  CLEAR THE LINE                
         AP    COUNTER,=P'1'       ADD TO COUNTER                               
         B     LR50                                                             
         SPACE 1                                                                
LR30     CLC   LISTNUM,NLISTS      IF HAVEN'T ALREADY DISPLAYED MAX             
         BE    *+8                                                              
         BAS   RE,DISPLAY          DISPLAY TO SCREEN                            
         SPACE 1                                                                
         MVC   DMDSKADD,TLDRDA     SET DISK ADDRESS                             
         GOTO1 LISTMON             CALL LISTMON                                 
         SPACE 1                                                                
LR50     GOTO1 SEQ                 GET NEXT KEY                                 
         B     LR10                LOOP                                         
         SPACE 1                                                                
LRX      NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         MVC   SAVEKEY,STARTKEY    SET TO START OVER NEXT TIME                  
         B     XIT                                                              
         EJECT                                                                  
*              KEY FILTERING ROUTINE                                            
         SPACE 1                                                                
         USING TLDRD,R4            R4 = A(KEY)                                  
FILTER   NTR1                                                                   
         LA    R1,FILTKEY          R1 = A(FILTER KEY)                           
         LA    R0,L'TLDRKEY        R0 = L'KEY                                   
         SPACE 1                                                                
FILT10   CLI   0(R1),0             IF SIGNIFICANT DATA IN FILTER                
         BE    *+14                                                             
         CLC   0(1,R1),0(R4)       THEN IT MUST MATCH                           
         BNE   NO                                                               
         LA    R1,1(R1)            BUMP TO NEXTS                                
         LA    R4,1(R4)                                                         
         BCT   R0,FILT10           LOOP                                         
         B     YES                                                              
         EJECT                                                                  
*              BUILD / DISPLAY HEADINGS                                         
         SPACE 1                                                                
BLDHEADS NTR1                                                                   
         LA    R3,BLOCK            R3 = A(OUTPUT AREA)                          
         MVI   BLOCK,C' '                                                       
         MVC   BLOCK+1(255),BLOCK                                               
         SPACE 1                                                                
         MVC   ACURRPOS,ADSECT     R2 = A(LOCATION IN DSECT PHASE)              
         BAS   RE,CURRSRF                                                       
         USING SRFADSEC,R2                                                      
         SPACE 1                                                                
BLDH20   BAS   RE,GETNXTDS         GET NEXT DSECT ENTRY                         
         BNE   BLDHX                                                            
         CLI   DIDTLDRD,C'Y'       IGNORE IF PROCESSING TLDRD                   
         BE    BLDH20                                                           
         SPACE 1                                                                
         LA    R4,SRFADLBL+4       R4 = A(START OF LABEL SUFFIX)                
         CLI   PASSIVE,C'Y'        IF PASSIVE KEY                               
         BNE   BLDH30                                                           
         CLC   =C'NAME',SRFADLBL+4 AND IF NOT NAME LABEL                        
         BE    BLDH30                                                           
         CLC   =C'YEAR',SRFADLBL+4 AND IF NOT YEAR LABEL                        
         BE    BLDH30                                                           
         LA    R4,SRFADLBL+5       SUFFIX STARTS ONE POSITION LATER             
         SPACE 1                                                                
BLDH30   LH    R1,LBLLEN                                                        
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)       MOVE LABEL SUFFIX TO OUTPUT AREA             
         SPACE 1                                                                
         LTR   R1,R1               IF MORE THAN ONE CHAR IN LABEL               
         BZ    BLDH40                                                           
         LA    RE,1(R3)            SET TO MAKE MIXED CASE                       
BLDH35   CLI   0(RE),C'A'          IF CHARACTER IS ALPHA                        
         BL    *+16                                                             
         CLI   0(RE),C'Z'                                                       
         BH    *+8                                                              
         NI    0(RE),X'BF'         MAKE LOWER CASE                              
         LA    RE,1(RE)                                                         
         BCT   R1,BLDH35                                                        
         SPACE 1                                                                
BLDH40   LH    R1,FLDLEN           BUMP TO NEXT OUTPUT POSITION                 
         CLI   SRFADTY,C'C'        IF FIELD TYPE NOT CHARACTER                  
         BE    *+16                                                             
         CLI   INV,C'Y'            AND NOT INVOICE NUMBER                       
         BE    *+8                                                              
         SLA   R1,1                DISPLAYED FIELD LENGTH WILL BE 2X            
         CH    R1,LBLLEN                                                        
         BNL   *+8                                                              
         LH    R1,LBLLEN                                                        
         LA    R3,1(R1,R3)                                                      
         B     BLDH20              LOOP                                         
         SPACE 1                                                                
BLDHX    MVC   LRCHD1,BLOCK        DISPLAY AS MUCH AS POSSIBLE                  
         OI    LRCHD1H+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY A KEY                                                    
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         LA    R3,BLOCK            R3 = A(OUTPUT AREA)                          
         MVI   BLOCK,C' '                                                       
         MVC   BLOCK+1(254),BLOCK                                               
         SPACE 1                                                                
         L     R4,ATHISLST         R4 = A(THIS LIST LINE)                       
         USING LINED,R4                                                         
         MVC   LINKEY,KEY          SAVE ENTIRE KEY                              
         SPACE 1                                                                
         MVC   ACURRPOS,ADSECT     R2 = A(LOCATION IN DSECT PHASE)              
         BAS   RE,CURRSRF                                                       
         USING SRFADSEC,R2                                                      
         SPACE 1                                                                
DISP20   BAS   RE,GETNXTDS         GET NEXT DSECT ENTRY                         
         BNE   DISPX                                                            
         CLC   SRFADLBL+4(4),=C'STAT'  IF PROCESSING STATUS                     
         BNE   *+8                                                              
         LA    R3,LINSTAT          SET TO DISPLAY IN ITS OWN FIELD              
         CLC   SRFADLBL+4(4),=C'DA  '  IF PROCESSING DISK ADDRESS               
         BNE   *+8                                                              
         LA    R3,LINDA            SET TO DISPLAY IN ITS OWN FIELD              
         SPACE 1                                                                
         BAS   RE,DISDATA          DISPLAY DATA                                 
         SPACE 1                                                                
         LH    R1,FLDLEN           BUMP TO NEXT OUTPUT POSITION                 
         CH    R1,LBLLEN                                                        
         BNL   *+8                                                              
         LH    R1,LBLLEN                                                        
         LA    R3,1(R1,R3)                                                      
         B     DISP20              LOOP                                         
         SPACE 1                                                                
DISPX    MVC   LINDATA,BLOCK       DISPLAY AS MUCH DATA AS POSSIBLE             
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SUBSIDIARY ROUTINES                                      
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE RECORD)           
*                                  R3 = A(OUTPUT AREA)                          
DISDATA  NTR1                                                                   
         LA    R4,KEY              SET R4 = A(DATA)                             
         AH    R4,SRFADDSP                                                      
         MVC   WORK,0(R4)          MOVE DATA TO TEMP AREA                       
         CLI   COMPL,C'Y'          IF FIELD IS COMPLEMENTED                     
         BNE   *+10                                                             
         XC    WORK(20),HEXFFS     UNCOMPLEMENT IT                              
         SPACE 1                                                                
         LH    RF,FLDLEN           RF = L'FIELD                                 
         SPACE 1                                                                
         CLI   SRFADTY,C'C'        TEST FOR CHARACTER                           
         BE    *+14                                                             
         CLC   SRFADTY(2),=C'0C'                                                
         BNE   DDAT20                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+40(0),WORK     SAVE DATA                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    WORK(0),SPACES      INSURE IT'S VALID CHARACTER DATA             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),WORK+40                                                  
         BNE   DDAT10              IT'S NOT - GO DISPLAY HEX                    
         EX    RF,*+8                                                           
         B     DDATX                                                            
         MVC   0(0,R3),WORK        DISPLAY CHARACTER DATA                       
         SPACE 1                                                                
DDAT10   MVC   WORK(40),WORK+40    IT'S NOT - RESTORE DATA                      
         AH    RF,=H'1'            AND CORRECT LENGTH                           
         SRA   RF,1                DISPLAY 1ST HALF OF DATA                     
         STH   RF,FLDLEN                                                        
         SPACE 1                                                                
DDAT20   CLI   INV,C'Y'            IF THIS IS INVOICE NUMBER                    
         BNE   DDAT50                                                           
         CLI   WORK,0              AND DATA NOT PRESENT                         
         BNE   *+16                                                             
         SRA   RF,1                DISPLAY 1ST HALF OF DATA                     
         STH   RF,FLDLEN                                                        
         B     DDAT50                                                           
         GOTO1 TINVCON,DMCB,WORK,(R3),DATCON  DISPLAY INVOICE NO.               
         B     DDATX                                                            
         SPACE 1                                                                
DDAT50   GOTO1 HEXOUT,DMCB,WORK,(R3),(RF),0  DISPLAY DATA IN HEX.               
         SPACE 1                                                                
         LH    RF,FLDLEN           SET ACTUAL FIELD LENGTH DISPLAYED            
         SLA   RF,1                                                             
         STH   RF,FLDLEN                                                        
         SPACE 1                                                                
DDATX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES LISTED RECORD                                  
         SPACE 1                                                                
LVREC    NTR1                                                                   
         L     R4,ATHISLST         R4 = A(THIS LIST LINE)                       
         USING LINED,R4                                                         
         LA    R3,KEY              R3 = A(KEY)                                  
         USING TLDRD,R3                                                         
         MVC   TLDRKEY,LINKEY      SET KEY FROM SCREEN                          
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         OI    DMINBTS,X'08'       OK TO READ DELETED                           
         GOTO1 READ                                                             
         SPACE 1                                                                
         LA    R2,LINSTATH                                                      
         GOTO1 ANY                                                              
         GOTO1 HEXIN,DMCB,8(R2),TLDRSTAT,L'LINSTAT                              
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRINV                                                           
         SPACE 1                                                                
         LA    R2,LINDAH                                                        
         GOTO1 ANY                                                              
         GOTO1 HEXIN,DMCB,8(R2),TLDRDA,L'LINDA                                  
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRINV                                                           
         SPACE 1                                                                
         GOTO1 WRITE               WRITE BACK POINTER                           
         MVI   IOOPT,C'Y'          SET WE DID UPDATE                            
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE LOOKS UP CODE OR LABEL IN DSECT PHASE                    
         SPACE 1                                                                
*                                  IF LABEL THEN USE LABEL FIELD                
*                                  ELSE R4 = A(CODE)                            
LOOKUP   NTR1                                                                   
         MVI   REC24ON,C'N'                                                     
         CLC   LABEL,SPACES        IF NOT PROCESSING LABEL                      
         BH    LOOK10                                                           
         GOTO1 HEXOUT,DMCB,(R4),HALF,1,0  CONVERT CODE TO CHARACTER             
         CLI   0(R4),X'24'         IF X'24' RECORD                              
         BNE   LOOK10                                                           
         GOTO1 HEXOUT,DMCB,1(R4),RECSUB,1,0  CONVERT CODE TO CHARACTER          
                                                                                
LOOK10   MVI   OVERLAY,X'70'       LOAD FIRST PHASE                             
         BAS   RE,LOADPHAS                                                      
         L     R2,APHASE           R2 = A(CURRENT POSITION IN PHASE)            
         USING SRFADSEC,R2                                                      
         BAS   RE,FIRSTSRF         BUMP TO FIRST RECORD                         
         SPACE 1                                                                
LOOK20   BAS   RE,SVDATA           SAVE RELEVENT DATA FOR PHASE RECORD          
                                                                                
         CLC   LABEL,SPACES        IF PROCESSING LABEL                          
         BNH   LOOK40                                                           
                                                                                
         CLI   REC24ON,C'Y'                                                     
         BE    LOOK30                                                           
         CLC   LABEL,SRFADLBL      MATCH ON IT                                  
         BNE   LOOK70                                                           
         CLC   SRFADTY+2(2),=C'24'                                              
         BNE   LOOKX                                                            
         MVI   REC24ON,C'Y'                                                     
         B     LOOK70                                                           
                                                                                
LOOK30   CLI   SRFADDEF,SRFQEQU    IF THIS IS NOT EQUATE TYPE                   
         BNE   LOOK70              SKIP TO NEXT RECORD                          
         CLC   SRFADLBL(4),LABEL                                                
         BNE   LOOK70                                                           
         CLC   SRFADLBL+4(4),=C'SCDQ'   AND SUBCODE                             
         BNE   LOOK70                                                           
         B     LOOKX                                                            
         SPACE 1                                                                
LOOK40   CLI   SRFADDEF,SRFQEQU    IF THIS IS NOT EQUATE TYPE                   
         BNE   LOOK70              SKIP TO NEXT RECORD                          
         SPACE 1                                                                
         CLC   SRFADLBL(2),=C'TL'  INSURE THIS IS RECORD DSECT                  
         BNE   LOOK70                                                           
         CLI   REC24ON,C'Y'        IF LOOKING FOR X'24' REC SUBCODE             
         BE    LOOK43              YES, DO SO                                   
         CLC   SRFADLBL+4(4),=C'CDQ '                                           
         BNE   LOOK45                                                           
         CLC   SRFADTY+2(2),HALF     MATCH ON CODE                              
         BNE   LOOK50                                                           
         CLC   SRFADTY+2(2),=C'24'   IF X'24' RECORD?                           
         BNE   LOOKX                                                            
         MVI   REC24ON,C'Y'        LOOKING FOR X'24' REC SUBCODE                
         B     LOOK70              AND CONTINUE LOOKING                         
                                                                                
LOOK43   CLC   SRFADLBL+4(4),=C'SCDQ'   AND SUBCODE                             
         BNE   LOOK70                                                           
         CLC   SRFADTY+2(2),RECSUB      MATCH ON CODE                           
         BE    LOOKX                                                            
         MVI   REC24ON,C'N'                                                     
         B     LOOK70                                                           
                                                                                
LOOK45   CLC   SRFADLBL+5(3),=C'CDQ '                                           
         BE    LOOK60                                                           
         LA    RE,PSSVKEYS                                                      
LOOK50   CLI   0(RE),X'FF'                                                      
         BE    LOOK70                                                           
         CLC   0(L'PSSVKEYS,RE),SRFADLBL                                        
         BE    LOOK60                                                           
         LA    RE,L'PSSVKEYS(RE)                                                
         B     LOOK50                                                           
         SPACE 1                                                                
LOOK60   CLC   SRFADTY+2(2),HALF   MATCH ON CODE                                
         BE    LOOKX               FOUND IT - RETURN                            
         SPACE 1                                                                
LOOK70   BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         B     LOOK20              LOOP                                         
         SPACE 1                                                                
LOOKX    MVC   LABEL,SPACES        CLEAR LABEL                                  
         B     XIT                                                              
         EJECT                                                                  
*              GET NEXT DSECT RECORD TO DISPLAY                                 
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE ENTRY)            
GETNXTDS NTR1                                                                   
GDS10    BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         BAS   RE,SVDATA           SAVE RELEVENT DATA                           
         SPACE 1                                                                
         CLC   SRFADDSP,=AL2(L'TLDRKEY) IF REACHED END OF KEY                   
         BE    GDS40                                                            
         CLI   SRFADDEF,SRFQDSCT   OR IF STARTING NEW DSECT                     
         BE    GDS40                                                            
         CLI   SRFADDEF,SRFQEQU    OR IF THIS IS EQUATE TYPE                    
         BNE   GDS60                                                            
         CLC   SRFADLBL+4(4),=C'CDQ ' AND IT'S FOR NEXT RECORD EQUATE           
         BE    GDS40                                                            
         CLC   SRFADLBL+4(4),=C'SCDQ' IF IT'S FOR SUB RECORD, SKIP              
         BE    GDS10                                                            
         CLC   SRFADLBL+5(3),=C'CDQ '                                           
         BNE   GDS60                                                            
         SPACE 1                                                                
GDS40    CLI   DIDTLDRD,C'Y'       THEN IF HAVEN'T PROCESSED TLDRD YET          
         BE    GDSX                                                             
         MVC   ACURRPOS,ATLDRD     SWITCH TO TLDRD DSECT                        
         BAS   RE,CURRSRF                                                       
         MVI   DIDTLDRD,C'Y'                                                    
         CLI   PASSIVE,C'Y'        IF PROCESSING PASSIVE POINTER                
         BNE   *+8                                                              
         MVI   PASSIVE,C'D'        SET TEMPORARILY PROCESSING DR DSECT          
         SPACE 1                                                                
GDS60    CLI   SRFADDEF,SRFQDS     WANT DS                                      
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC ONLY                                  
         BNE   GDS10                                                            
         CLC   SRFADLBL,SPACES     IGNORE IF NO LABEL DEFINED                   
         BE    GDS10                                                            
         SPACE 1                                                                
         BAS   RE,GETFLEN          GET L'FIELD                                  
         BZ    GDS10                                                            
         BAS   RE,SETCOMP          SET COMPLEMENTED STATUS                      
         BAS   RE,SETINV           SET INVOICE FIELD STATUS                     
         CR    RE,RE               FIELD IS GOOD - RETURN CC EQU                
         B     XITR2               AND RETURN A(FIELD)                          
         SPACE 1                                                                
GDSX     MVI   DIDTLDRD,C'N'       TURN OFF SWITCH                              
         CLI   PASSIVE,C'D'        IF NECESSARY                                 
         BNE   *+8                                                              
         MVI   PASSIVE,C'Y'        RESTORE PASSIVE POINTER INDICATOR            
         B     NO                  RETURN CC NE - DONE                          
         EJECT                                                                  
*              ROUTINE RETURNS LENGTH OF CURRENT FIELD AND ITS LABEL            
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT FIELD ENTRY)                  
GETFLEN  NTR1                                                                   
         LA    R1,4                SET L'LABEL SUFFIX                           
         CLI   PASSIVE,C'Y'                                                     
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         LA    RF,SRFADLBL+7                                                    
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         SPACE 1                                                                
         CLI   PASSIVE,C'Y'        IF PASSIVE KEY                               
         BNE   GETF10                                                           
         CLC   =C'NAME',SRFADLBL+4 AND NAME PASSIVE LABEL                       
         BE    *+14                                                             
         CLC   =C'YEAR',SRFADLBL+4 OR YEAR PASSIVE LABEL                        
         BNE   *+8                                                              
         LA    R1,1(R1)            LABEL WILL INCLUDE 1ST CHARACTER             
         SPACE 1                                                                
GETF10   STH   R1,LBLLEN                                                        
         SPACE 1                                                                
         MVC   FLDLEN,=H'4'        NOW SET L'FIELD                              
         CLI   SRFADTY,C'F'        TEST FOR FULLWORD                            
         BE    GETFX                                                            
         MVC   FLDLEN,=H'2'                                                     
         CLI   SRFADTY,C'H'        TEST FOR HALFWORD                            
         BE    GETFX                                                            
         SPACE 1                                                                
         XC    FLDLEN,FLDLEN       RETURN LENGTH IN FLDLEN                      
         LH    R3,SRFADDSP         SAVE DISP. TO CURRENT FIELD                  
         SPACE 1                                                                
         BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE                 
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   GETF20                                                           
         LH    R1,SRFADDSP         DISP. TO NEXT FIELD                          
         SR    R1,R3               LESS DISP. TO CURRENT FIELD                  
         STH   R1,FLDLEN           IS L'CURRENT FIELD                           
         B     GETFX                                                            
         SPACE 1                                                                
GETF20   BAS   RE,CURRSRF          RESTORE A(CURRENT ENTRY)                     
         ZIC   R1,SRFADTYL         ATTEMPT TO EXTRACT LENGTH FROM TYPE          
         SH    R1,=H'3'                                                         
         BM    GETF40                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SRFADTY+2   ASSUME TYPE IS CL???                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    WORK(0),=10C'0'     INSURE IT'S NUMERIC                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SRFADTY+2                                                
         BNE   GETF40                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   RF,DUB                                                           
         STH   RF,FLDLEN           SAVE IT                                      
         B     GETFX                                                            
         SPACE 1                                                                
GETF40   BAS   RE,NEXTSRF          BUMP TO NEXT RECORD IN PHASE AGAIN           
         SPACE 1                                                                
         CLI   SRFADDEF,SRFQDSCT   IF IT'S A DSECT                              
         BE    GETF50                                                           
         CLI   SRFADDEF,SRFQORG    OR IT'S AN ORG                               
         BNE   GETFX                                                            
         CLC   SRFADTY+4(5),=C'PCD+1' OF PASSIVE KEY                            
         BNE   GETFX                                                            
         SPACE 1                                                                
GETF50   BAS   RE,CURRSRF          RESTORE A(CURRENT ENTRY)                     
         LA    R1,L'TLDRKEY        L'KEY                                        
         SH    R1,SRFADDSP         - DISP. TO THIS FIELD                        
         BM    *+8                                                              
         STH   R1,FLDLEN           IS L'THIS FIELD                              
         SPACE 1                                                                
GETFX    BAS   RE,CURRSRF          INSURE WE'VE RESTORED CURRENT PTR.           
         SPACE 1                                                                
         OC    FLDLEN,FLDLEN       SET CC                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DETERMINES IF CURRENT FIELD IS COMPLEMENTED              
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE ENTRY)            
SETCOMP  NTR1                                                                   
         MVI   COMPL,C'Y'          ASSUME IT IS                                 
         SPACE 1                                                                
         ZIC   R1,SRFADTYL         BUMP PAST TYPE                               
         LA    RF,SRFADTY(R1)      RF = A(COMMENT)                              
         ICM   R1,1,0(RF)          R1 = L'COMMENT)                              
         BZ    SETC20                                                           
         SPACE 1                                                                
         CLC   =C'COMPL',0(RF)     LOOP THROUGH COMMENT                         
         BE    SETCX                                                            
         LA    RF,1(RF)                                                         
         BCT   R1,*-14                                                          
         SPACE 1                                                                
SETC20   MVI   COMPL,C'N'          IT'S NOT - TURN OFF SWITCH                   
         SPACE 1                                                                
SETCX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE DETERMINES IF CURRENT FIELD IS INVOICE NUMBER            
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE ENTRY)            
SETINV   DS    0H                                                               
         MVI   INV,C'N'                                                         
         CLI   SRFADTY,C'X'        TEST FOR HEX                                 
         BNER  RE                                                               
         CLC   FLDLEN,=H'6'        6 BYTE FIELD                                 
         BNER  RE                                                               
         CLC   SRFADLBL+4(4),=C'INV ' AND SUFFIX IS INV                         
         BE    *+12                                                             
         CLC   SRFADLBL+5(3),=C'INV '                                           
         BNER  RE                                                               
         MVI   INV,C'Y'            SET THIS IS INVOICE FIELD                    
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE GETS NEXT DSECT PHASE RECORD                             
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT ENTRY)                        
FIRSTSRF NTR1                                                                   
         B     NEXT10                                                           
         SPACE 1                                                                
NEXTSRF  NTR1                                                                   
         ZIC   R1,SRFADTYL                                                      
         LA    R2,SRFADTY(R1)      BUMP PAST L'TYPE, TYPE                       
         IC    R1,0(R2)                                                         
         LA    R2,1(R1,R2)         BUMP PAST L'COMMENT, COMMENT                 
         SPACE 1                                                                
NEXT10   CLC   SRFADDSP,=AL2(SRFQSPCL) IF THIS ISN'T SPECIAL RECORD             
         BNE   NEXTX                   THEN DONE                                
         SPACE 1                                                                
         CLI   SRFSPCL,SRFQEOF     IF REACHED END OF FILE                       
         BE    ERRLABEL            THEN GIVE ERROR - INVALID LABEL              
         SPACE 1                                                                
         CLI   SRFSPCL,SRFQMEM     IF START OF MEMBER                           
         BNE   *+12                                                             
         LA    R2,SRFMEMB+L'SRFMEMB  BUMP TO NEXT RECORD                        
         B     NEXT10                                                           
         SPACE 1                                                                
         CLI   SRFSPCL,SRFQEOP     MUST BE END OF PHASE                         
         BE    *+6                                                              
         DC    H'0'                UNRECOGNIZED SPECIAL RECORD TYPE             
         ZIC   R1,OVERLAY                                                       
         LA    R1,1(R1)            BUMP OVERLAY NUMBER                          
         STC   R1,OVERLAY                                                       
         SPACE 1                                                                
         BAS   RE,LOADPHAS         LOAD NEXT DSECT PHASE                        
         L     R2,APHASE                                                        
         B     NEXT10              CONTINUE                                     
         SPACE 1                                                                
NEXTX    B     XITR2                                                            
         EJECT                                                                  
*              ROUTINE LOADS A DSECT PHASE                                      
         SPACE 1                                                                
*                                  OVERLAY = OVERLAY NUMBER                     
LOADPHAS NTR1                                                                   
         CLC   OVERLAY,APHASE      IF REQUESTED OVERLAY ALREADY LOADED          
         BE    LOADX               GET OUT                                      
         MVC   DMCB+4(3),=X'D9015E'  SET TO LOAD T15E PHASE                     
         MVC   DMCB+7(1),OVERLAY     SET OVERLAY                                
         GOTO1 CALLOV,DMCB,APHASE,,0                                            
         OC    9(3,R1),9(R1)                                                    
         BZ    ERRPHASE                                                         
         MVC   APHASE(1),OVERLAY   SAVE CURRENT OVERLAY                         
LOADX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE RESTORES SAVED POSITION IN DSECT PHASE                   
         SPACE 1                                                                
CURRSRF  NTR1                                                                   
         CLC   OVERLAY,ACURRPOS    TEST CORRECT PHASE IS LOADED                 
         BE    *+14                                                             
         MVC   OVERLAY,ACURRPOS    NO, SO LOAD IT                               
         BAS   RE,LOADPHAS                                                      
         SPACE 1                                                                
         L     R2,ACURRPOS         RESTORE A(DSECT RECORD)                      
         B     XITR2                                                            
         SPACE 3                                                                
*              ROUTINE SAVES VARIOUS DATA FOR A DSECT PHASE RECORD              
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE RECORD)           
SVDATA   NTR1                                                                   
         ST    R2,ACURRPOS         SAVE A(CURRENT POSITION)                     
         MVC   ACURRPOS(1),OVERLAY SAVE CURRENT OVERLAY NUMBER IN HOB           
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 1                                                                
HDHOOK   NTR1                                                                   
         MVI   BYTE,C'H'           SET PRINTING HEADINGS                        
         GOTO1 PRTSCRN,DMCB,EFHTAG,LRCHDH,H4-1                                  
         GOTO1 (RF),(R1),LRCHDH,LRCSELH,H6-5                                    
         MVC   H6-5(5),SPACES      CLEAR SELECT FIELD                           
         B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 2                                                                
ERRSTRT  LA    R2,LRCSTRTH                                                      
         B     *+8                                                              
ERRLABEL LA    R2,LRCCODEH                                                      
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
         USING SRFADSEC,R2         R2 = A(DSECT PHASE ENTRY)                    
ERRFLD   MVI   MYMSGNO1,ERKEYFLD   INVALID KEY FIELD                            
         MVI   MYMTYP,GTMERR       SET ERROR TYPE MESSAGE                       
         MVI   BLOCK,8+1                                                        
         MVC   BLOCK+1(8),SRFADLBL PUT LABEL IN MESSAGE                         
         MVI   BLOCK+9,0                                                        
         LA    R2,LRCSTRTH         POINT TO FIELD                               
         B     INFEND                                                           
         SPACE 1                                                                
ERRPHASE MVI   ERROR,NOPHASE       PHASE NOT FOUND                              
         LA    R2,CONRECH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
XITR2    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
HEXFFS   DC    20X'FF'                                                          
RELO     DS    F                                                                
         SPACE 1                                                                
CHKDKEYS DC    AL1(TLCKCDQ)        KEYS ON CHKDIR                               
         DC    AL1(TLCKHCDQ)                                                    
         DC    AL1(TLCKDCDQ)                                                    
         DC    AL1(TLCKLCDQ)                                                    
         DC    AL1(TLCKCCDQ)                                                    
         DC    AL1(TLCKECDQ)                                                    
         DC    AL1(TLCKYCDQ)                                                    
         DC    AL1(TLCKBCDQ)                                                    
         DC    AL1(TLCKRCDQ)                                                    
         DC    AL1(TLDTCDQ)                                                     
         DC    AL1(TLW2CDQ)                                                     
         DC    AL1(0)                                                           
         SPACE 1                                                                
CHK24KYS DC    AL1(TLT4SCDQ)       X'24' KEYS ON CHKDIR                         
         DC    AL1(TLR1SCDQ)                                                    
         DC    AL1(TLTASCDQ)                                                    
         DC    AL1(TLN4SCDQ)                                                    
         DC    AL1(0)                                                           
         SPACE 1                                                                
PSSVKEYS DC    0CL8                                                             
         DC    C'TLCOVRDQ'         PASSIVE KEYS THAT WERE NOT DEFINED           
         DC    C'TLCOGIDQ'         CORRECTLY (WITH A CDQ POSTFIX ON             
         DC    C'TLCOGNDQ'         THE RECORD EQUATE)                           
         DC    C'TLDVEDDQ'                                                      
         DC    C'TLDVRDDQ'                                                      
         DC    C'TLDVMDDQ'                                                      
         DC    C'TLMTNMDQ'                                                      
         DC    C'TLMTALDQ'                                                      
         DC    C'TLUHMTDQ'                                                      
         DC    X'FF'                                                            
         SPACE 1                                                                
*              SPECS FOR REPORT                                                 
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,34,C'Record List'                                             
         SSPEC H2,34,C'-----------'                                             
         DC    X'00'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER A LIST LINE ON SCREEN                             
         SPACE 1                                                                
LINED    DSECT                                                                  
LINDATAH DS    CL8                                                              
LINDATA  DS    CL(L'LRCDATA)       KEY DATA                                     
LINSTATH DS    CL8                                                              
LINSTAT  DS    CL4                 KEY STATUS                                   
LINDAH   DS    CL8                                                              
LINDA    DS    CL8                 DISK ADDRESS                                 
LINKEYH  DS    CL8                                                              
LINKEY   DS    CL32                ACTUAL KEY                                   
LINLNQ   EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE SRFADDSECT                                                     
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRD2D                                                       
         SPACE 3                                                                
*              SAVED STORAGE AT BOTTOM OF TWA0                                  
         SPACE 1                                                                
APHASE   DS    A                   A(DSECT PHASE)                               
ADSECT   DS    A                   A(DSECT FOR THIS KEY)                        
ATLDRD   DS    A                   A(TLDRD) IN DSECT PHASE                      
ACURRPOS DS    A                   A(CURRENT POSITION IN DSECT PHASE)           
FLDLEN   DS    H                   L'CURRENT FIELD                              
LBLLEN   DS    H                   L'CURRENT LABEL SUFFIX                       
RECSUB   DS    H                   RECORD SUBCODE                               
REC24ON  DS    C                   X'24' RECORD MODE                            
STARTKEY DS    CL(L'TLDRREC)       START KEY                                    
FILTKEY  DS    CL(L'TLDRREC)       FILTER KEY                                   
SAVEKEY  DS    CL(L'TLDRREC)       SAVED KEY                                    
PASSIVE  DS    CL1                 PASSIVE/ACTIVE KEY INDICATOR                 
LABEL    DS    CL8                 SEARCH LABEL                                 
DIDTLDRD DS    CL1                 PROCESSED STATUS/DA SWITCH                   
COMPL    DS    CL1                 COMPLEMENTED FIELD SWITCH                    
INV      DS    CL1                 INVOICE FIELD SWITCH                         
COUNTER  DS    PL4                 RECORD COUNT                                 
SETCHK   DS    C                   Y=SET CHKDIR/CHKFIL                          
PGTBL    DS    CL(NPGTBL*L'TLRCKEY) SAVED KEYS FOR PAGE CONTROL RTN.            
NPGTBL   EQU   8                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* TAGENWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025TAGEND2   07/15/14'                                      
         END                                                                    
