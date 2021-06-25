*          DATA SET TAGENEE    AT LEVEL 010 AS OF 04/11/14                      
*PHASE T702EEA,*                                                                
         TITLE 'T702EE - CONTRACT MAINTENANCE'                                  
T702EE   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLNQ,T702EE,R7                                                
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R6,TWAHOLE          R6=TWAHOLE DSECT                             
         USING TWAHOLED,R6                                                      
         ST    RE,ASPBLK                                                        
         AHI   RE,SPBLK                                                         
         ST    RE,APBLK                                                         
         EJECT                                                                  
*                                                                               
         MVI   OVERLAY,MAINSCR     SET OVERLAY AS MAIN SCREEN                   
         CLI   ACTNUM,ACTASSGN     UNLESS ACTION IS ASSIGN                      
         BE    MODE10                                                           
         CLI   ACTNUM,ACTUASGN     OR UNASSIGN                                  
         BNE   MODE20                                                           
MODE10   MVI   OVERLAY,ASSNSCR     THEN SET OVERLAY AS ASSIGN SCREEN            
*                                                                               
         MVI   CNAASUNH+5,17                                                    
         OI    CNAASUNH+6,X'80'    SET UP DISPLAY BASED ON                      
         MVC   CNAASUN,ASG2COM     ASSIGN OR UNASSIGN                           
         CLI   ACTNUM,ACTASSGN                                                  
         BE    MODE20                                                           
         MVC   CNAASUN,UASFCOM                                                  
*                                                                               
MODE20   BAS   RE,PFCMNT           POSSIBLE FLIP TO COMMENTS                    
         LA    R2,PFTABM                                                        
         CLI   OVERLAY,ASSNSCR                                                  
         BNE   *+8                                                              
         LA    R2,PFTABA                                                        
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE OVERLAY                           
         BAS   RE,SETGLO           AND INSERT GLOBALS ON SCREEN                 
         BAS   RE,MORE             AND SEE IF PF KEY 17 IS VALID                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY THE RECORD                           
         BE    DREC                                                             
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BE    VREC                                                             
         CLI   MODE,RECDEL         DELETE THE RECORD                            
         BE    DELREC                                                           
         CLI   MODE,XRECADD        DISPLAY THE RECORD                           
         BE    DREC                                                             
         CLI   MODE,XRECPUT        DISPLAY THE RECORD                           
         BE    DREC                                                             
         CLI   MODE,XRECREST       DISPLAY THE RECORD                           
         BE    DREC                                                             
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO SET UP COMMENT SCREEN IF PF16 HIT                     
         SPACE 2                                                                
PFCMNT   NTR1                                                                   
         CLI   PFAID,16                                                         
         BNE   PFCX                                                             
         MVI   COMTYPA,TLCMTCON                                                 
         MVI   CODEBLN,C' '                                                     
         MVI   SVRECUSE,ON                                                      
PFCX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INSERT GLOBALS INTO KEY FIELDS                        
         SPACE 2                                                                
SETGLO   NTR1                                                                   
         BAS   RE,CORGEN           CORRECT GENCON MANUEVERS                     
         SPACE                                                                  
         LA    R2,CNTAGYH          R2=A(AGENCY FIELD)                           
         CLI   OVERLAY,MAINSCR                                                  
         BE    *+8                                                              
         LA    R2,CNAAGYH                                                       
         SPACE                                                                  
         CLI   5(R2),0             IF NOT ALREADY ENTERED                       
         BNE   SG10                FILL IN WITH AGENCY GLOBAL                   
         OC    TGAGY,TGAGY                                                      
         BZ    SG10                                                             
         MVC   8(L'TGAGY,R2),TGAGY                                              
         MVI   5(R2),L'TGAGY                                                    
         SPACE                                                                  
SG10     LA    R2,CNTCONIH         R2=A(CONTRACT ID FIELD)                      
         CLI   OVERLAY,MAINSCR                                                  
         BE    *+8                                                              
         LA    R2,CNACONIH                                                      
         SPACE                                                                  
         CLI   5(R2),0             IF NOT ALREADY ENTERED                       
         BNE   SG20                FILL IN WITH CONTRACT ID GLOBAL              
         OC    TGCNID,TGCNID                                                    
         BZ    SG20                                                             
         MVC   8(L'TGCNID,R2),TGCNID                                            
         MVI   5(R2),L'TGCNID                                                   
         SPACE                                                                  
SG20     LA    R2,CNTTSTAH         R2=A(TERM START DATE FIELD)                  
         CLI   OVERLAY,MAINSCR                                                  
         BE    *+8                                                              
         LA    R2,CNATSTAH                                                      
         SPACE                                                                  
         CLI   5(R2),0             IF NOT ALREADY ENTERED                       
         BNE   SG30                FILL IN WITH TERM START GLOBAL               
         OC    TGCNTRMS,TGCNTRMS                                                
         BZ    SG30                                                             
         GOTO1 DATCON,DMCB,(1,TGCNTRMS),(8,8(R2))                               
         MVI   5(R2),8                                                          
         SPACE                                                                  
SG30     LA    R2,CNTTENDH         R2=A(TERM END DATE FIELD)                    
         CLI   OVERLAY,MAINSCR                                                  
         BE    *+8                                                              
         LA    R2,CNATENDH                                                      
         SPACE                                                                  
         CLI   5(R2),0              IF NOT ALREADY ENTERED                      
         BNE   SG40                 FILL IN WITH TERM END GLOBAL                
         OC    TGCNTRME,TGCNTRME                                                
         BZ    SG40                                                             
         GOTO1 DATCON,DMCB,(1,TGCNTRME),(8,8(R2))                               
         MVI   5(R2),8                                                          
         SPACE                                                                  
SG40     LA    R2,CNTAGYH                                                       
         LA    R3,CNTTENDH                                                      
         CLI   OVERLAY,MAINSCR                                                  
         BE    *+12                                                             
         LA    R2,CNAAGYH                                                       
         LA    R3,CNATENDH                                                      
         GOTO1 FLDVAL,DMCB,(X'02',(R2)),(X'80',(R3))                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CORRECT GENCON MANUEVERS                              
         SPACE 2                                                                
CORGEN   NTR1                                                                   
         LA    R4,KEYTABM          R3=A(SCREEN'S KEY FIELD                      
         CLI   OVERLAY,MAINSCR          DISPLACEMENTS TABLE)                    
         BE    CG10                                                             
         LA    R4,KEYTABA                                                       
         SPACE 1                                                                
CG10     LR    R3,R4                                                            
CG20     CLI   0(R3),X'FF'         IF REACHED END OF TABLE THEN                 
         BE    CGX                 THERE'S NOTHING TO CORRECT                   
         ZICM  R2,0(R3),2                                                       
         AR    R2,RA               R2=A(KEY FIELD)                              
         TM    4(R2),X'C0'         IF 'INPUT THIS TIME' AND                     
         BNO   CG30                AND 'PREVIOUSLY INPUT'                       
         TM    6(R2),X'80'         AND 'TRANSMIT' BITS ARE ON                   
         BO    CG40                                                             
CG30     LA    R3,2(R3)            GENCON HAS MESSED US UP AND WE               
         B     CG20                MUST CORRECT SCREEN                          
         SPACE 1                                                                
         USING KEYTABD,R4                                                       
CG40     ZICM  R2,KEYAGY,2         R2=A(AGENCY FIELD)                           
         AR    R2,RA               R3=A(TERM END FIELD)                         
         ZICM  R3,KEYTEND,2        CORRECT GENCON BY CLEARING                   
         AR    R3,RA               KEY FIELDS                                   
         GOTO1 FLDVAL,DMCB,(X'11',(R2)),(X'80',(R3))                            
         DROP  R4                                                               
         SPACE 1                                                                
CGX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE PF17 (MORE) FOR ASSIGN SCREEN                
         SPACE 2                                                                
MORE     NTR1                                                                   
         NI    STATUS,ALL-DISPMORE  TURN OFF DISPLAY MORE STATUS                
         SPACE                                                                  
         CLI   OVERLAY,ASSNSCR      IF ON ASSIGN/UNASSIGN SCREEN                
         BNE   MOREX                                                            
         CLI   PFAID,17             AND PF17 IS HIT                             
         BNE   MOREX                                                            
         LA    R2,CNACIDH                                                       
         CLI   5(R2),0              NO INPUT ALLOWED IN COM ID FIELD            
         BNE   ERRINV                                                           
         SPACE                                                                  
         OI    STATUS,DISPMORE      TURN ON DISPLAY MORE STATUS                 
MOREX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CONTRACT KEY                                 
         SPACE 2                                                                
VKEY     LA    R2,CNTAGYH          R2=A(AGENCY FIELD)                           
         LA    R3,CNTTENDH         R3=A(TERM END FIELD)                         
         CLI   OVERLAY,MAINSCR                                                  
         BE    *+12                                                             
         LA    R2,CNAAGYH          EXIT IF KEY ALREADY VALIDATED                
         LA    R3,CNATENDH                                                      
         GOTO1 FLDVAL,DMCB,(X'40',(R2)),(X'80',(R3))                            
         BE    VKX                                                              
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',(R2))                                 
         OI    4(R2),X'20'         VALIDATE AGENCY                              
         OI    6(R2),X'80'         AND SET AS VALIDATED                         
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         SPACE                                                                  
         USING TLCND,R4                                                         
         LA    R4,KEY              BEGIN BUILDING CONTRACT KEY                  
         XC    TLCNKEY,TLCNKEY                                                  
         MVI   TLCNCD,TLCNCDQ      WITH CONTRACT RECORD CODE                    
         MVC   TLCNAGY,TGAGY       AND VALIDATED AGENCY CODE                    
         SPACE 1                                                                
         LA    R2,CNTCONIH         R2=A(CONTRACT ID FIELD)                      
         CLI   OVERLAY,MAINSCR                                                  
         BE    *+8                                                              
         LA    R2,CNACONIH                                                      
         SPACE 1                                                                
         GOTO1 ANY                 FIELD MUST BE POPULATED                      
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK05                                                             
         LA    RE,8(R2)            CANNOT CONTAIN A COMMA                       
         LHI   RF,L'CNTCONI                                                     
VK02     CLI   0(RE),C','          CANNOT CONTAIN A COMMA                       
         BE    ERRINV                                                           
         CLI   0(RE),C'"'          OR DOUBLE QUOTES                             
         BE    ERRINV                                                           
         CLI   0(RE),X'7D'         OR SINGLE QUOTES                             
         BE    ERRINV                                                           
         CLI   0(RE),C'-'          OR DASHES                                    
         BE    ERRINV                                                           
         CLI   0(RE),C'/'          OR SLASHES                                   
         BE    ERRINV                                                           
         CLI   0(RE),X'50'         OR AND SYMBOLS                               
         BE    ERRINV                                                           
         CLI   0(RE),C'('          OR PARENTHESIS                               
         BE    ERRINV                                                           
         CLI   0(RE),C')'                                                       
         BE    ERRINV                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,VK02                                                          
         SPACE 1                                                                
VK05     MVC   TLCNCNID,8(R2)      ADD VALIDATED CONTRACT ID                    
         OC    TLCNCNID,SPACES     AND PAD WITH SPACES                          
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    VK10                RETURN ERROR IF CONTRACT ID                  
         GOTO1 HIGH                DOES NOT EXIST                               
         CLC   KEY(TLCNCNID+L'TLCNCNID-TLCND),KEYSAVE                           
         BNE   ERRINV                                                           
         XC    TLCNTRMS(6),TLCNTRMS                                             
         SPACE 1                                                                
VK10     MVC   TGCNID,TLCNCNID     SAVE IN GLOBAL                               
         OI    4(R2),X'20'         SET AS VALIDATED                             
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         LA    R2,CNTTSTAH         R2=A(TERM START DATE)                        
         CLI   OVERLAY,MAINSCR                                                  
         BE    *+8                                                              
         LA    R2,CNATSTAH                                                      
         SPACE 1                                                                
         GOTO1 DTVAL,DMCB,TLCNTRMS ADD VALIDATED TERM START                     
         GOTO1 DATCON,DMCB,(1,TLCNTRMS),(8,8(R2))                               
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    VK20                RETURN ERROR IF CONTRACT ID/                 
         GOTO1 HIGH                TERM START DOES NOT EXIST                    
         CLC   KEY(TLCNTRMS+L'TLCNTRMS-TLCND),KEYSAVE                           
         BNE   ERRINV                                                           
         XC    TLCNTRME,TLCNTRME                                                
         SPACE 1                                                                
VK20     MVC   TGCNTRMS,TLCNTRMS   SAVE IN GLOBAL                               
         OI    4(R2),X'20'         SET AS VALIDATED                             
         OI    6(R2),X'80'         AND DISPLAY IT                               
         SPACE 1                                                                
         LA    R2,CNTTENDH         R2=A(TERM END DATE)                          
         CLI   OVERLAY,MAINSCR                                                  
         BE    *+8                                                              
         LA    R2,CNATENDH                                                      
         SPACE 1                                                                
         GOTO1 DTVAL,DMCB,TLCNTRME ADD VALIDATED TERM END                       
         GOTO1 DATCON,DMCB,(1,TLCNTRME),(8,8(R2))                               
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    VK30                RETURN ERROR IF CONTRACT ID/                 
         GOTO1 HIGH                TERMS DOES NOT EXIST                         
         CLC   KEY(TLCNTRME+L'TLCNTRME-TLCND),KEYSAVE                           
         BNE   ERRINV                                                           
         SPACE 1                                                                
VK30     MVC   TGCNTRME,TLCNTRME   (SAVE IN GLOBAL)                             
         OI    4(R2),X'20'         SET AS VALIDATED                             
         OI    6(R2),X'80'         AND DISPLAY IT                               
         SPACE 1                                                                
         CLC   TLCNTRMS,TLCNTRME   START DATE CANNOT BE                         
         BH    ERRINV              LATER THAN END DATE                          
         SPACE                                                                  
         CLI   OVERLAY,ASSNSCR     IF ON ASSIGN/UNASSIGN SCREEN                 
         BNE   VKX                                                              
         XC    CURCCNT,CURCCNT     CLEAR COMMERCIALS DISPLAYED COUNTER          
         XC    LSTCCNT,LSTCCNT                                                  
         B     DREC                AND GO DISPLAY RECORD                        
         SPACE                                                                  
VKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY CONTRACT KEY                                  
         SPACE 2                                                                
         USING TLCND,R4                                                         
DKEY     L     R4,AIO              MOVE KEY FIELDS TO SCREEN AND GLOBAL         
         SPACE                                                                  
         MVC   CNTAGY,TLCNAGY                                                   
         SPACE                                                                  
         MVC   CNTCONI,TLCNCNID                                                 
         MVC   TGCNID,TLCNCNID                                                  
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(1,TLCNTRMS),(8,CNTTSTA)                             
         MVC   TGCNTRMS,TLCNTRMS                                                
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(1,TLCNTRME),(8,CNTTEND)                             
         MVC   TGCNTRME,TLCNTRME                                                
         SPACE                                                                  
         GOTO1 FLDVAL,DMCB,(X'02',CNTAGYH),(X'80',CNTTENDH)                     
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY CONTRACT RECORD                               
         SPACE 2                                                                
DREC     CLI   OVERLAY,MAINSCR                                                  
         BE    DMAIN                                                            
         CLI   OVERLAY,ASSNSCR                                                  
         BE    DASGN                                                            
         DC    H'00'                                                            
         EJECT                                                                  
*              ROUTINE TO DISPLAY MAIN CONTRACT MAINTENANCE SCREEN              
         SPACE 2                                                                
DMAIN    GOTO1 FLDVAL,DMCB,(X'01',CNTCONDH),(X'80',CNTCMNTH)                    
         SPACE                                                                  
         GOTO1 CHAROUT,DMCB,TAFNELQ,(1,CNTCONDH),TAFNTCON                       
         SPACE                                                                  
         USING TARDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TARDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(1,TARDDATE),(8,CNTDATE)                             
         MVI   CNTDATEH+5,8                                                     
         SPACE                                                                  
         USING TYPETABD,RE                                                      
         LA    RE,TYPETAB          RE=A(TYPE TABLE)                             
DM10     CLI   0(RE),X'FF'                                                      
         BE    DM30                                                             
         CLC   TYPEEQU1,TARDTYP1   IF TYPE IN TABLE MATCHES                     
         BNE   DM20                TYPE IN ELEMENT                              
         ZICM  R2,TYPEDISP,2                                                    
         AR    R2,RA               R2=A(TYPE FIELD)                             
         MVI   8(R2),C'X'          CHECK OFF TYPE                               
         MVI   5(R2),1                                                          
         B     DM60                                                             
DM20     LA    RE,TYPELNQ(RE)                                                   
         B     DM10                                                             
         SPACE                                                                  
DM30     LA    RE,TYPETAB          RE=A(TYPE TABLE)                             
DM40     CLC   TYPEEQU2,TARDTYP2   IF TYPE IN TABLE MATCHES                     
         BNE   DM50                TYPE IN ELEMENT                              
         ZICM  R2,TYPEDISP,2                                                    
         AR    R2,RA               R2=A(TYPE FIELD)                             
         MVI   8(R2),C'X'          CHECK OFF TYPE                               
         MVI   5(R2),1                                                          
         B     DM60                                                             
DM50     LA    RE,TYPELNQ(RE)                                                   
         B     DM40                                                             
         DROP  RE                                                               
         SPACE                                                                  
DM60     DS    0H                                                               
*&&DO                                                                           
         USING STATTABD,RE                                                      
DM60     LA    RE,STATTAB          RE=A(STATUS TABLE)                           
DM70     CLC   STATEQU,TARDSTAT                                                 
         BNE   DM80                                                             
         ZICM  R2,STATDISP,2                                                    
         AR    R2,RA               R2=A(TYPE FIELD)                             
         MVI   8(R2),C'X'                                                       
         MVI   5(R2),1                                                          
         B     DM90                                                             
DM80     LA    RE,STATLNQ(RE)                                                   
         B     DM70                                                             
         DROP  RE                                                               
*&&                                                                             
         SPACE                                                                  
DM90     GOTO1 CPSMOVE,DMCB,(L'CNTCLI,CNTCLIH),(L'CNTCLIN,CNTCLINH),   +        
               (L'TARDCLI,TARDCLI),TLCLCDQ                                      
         GOTO1 CPSMOVE,DMCB,(L'CNTPRD,CNTPRDH),(L'CNTPRDN,CNTPRDNH),   +        
               (L'TARDPRD,TARDPRD),TLPRCDQ                                      
         GOTO1 CPSMOVE,DMCB,(L'CNTSSN,CNTSSNH),(L'CNTSSNN,CNTSSNNH),   +        
               (L'TARDSSN,TARDSSN),TLW4CDQ                                      
         SPACE                                                                  
         EDIT  (4,TARDCFEE),(10,CNTFEE),2,ZERO=NOBLANK,ALIGN=LEFT               
         SPACE                                                                  
         GOTO1 CHAROUT,DMCB,TACMELQ,(1,CNTCMNTH),TACMTYPG                       
         GOTO1 ACTVOUT,DMCB,(X'80',CNTLCHGH)                                    
         SPACE                                                                  
         CLI   MODE,XRECADD        UPDATE PASSIVE POINTERS                      
         BE    DM110                                                            
         CLI   MODE,XRECPUT                                                     
         BE    DM110                                                            
         CLI   MODE,XRECREST                                                    
         BNE   DMX                                                              
DM110    GOTO1 ADDPTRS,DMCB,(X'08',ASPBLK),APBLK                                
DMX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY CONTRACT (UN)ASSIGN SCREEN                    
         SPACE 2                                                                
DASGN    LA    R2,CNAAGYH                                                       
         GOTO1 RECVAL,DMCB,TLCNCDQ,(X'A4',TGCNTRME)                             
         BNE   ERRNOFND                                                         
         SPACE                                                                  
         TM    STATUS,DISPMORE     IF WANT TO DISPLAY MORE                      
         BZ    *+14                                                             
         MVC   LSTCCNT,CURCCNT     STORE NUMBER OF COMM'LS DISPLAYED            
         MVI   PFAID,0                                                          
         XC    CURCCNT,CURCCNT     LAST AND CLEAR CURRENT COUNTER               
         SPACE                                                                  
         GOTO1 FLDVAL,DMCB,(X'01',CNACLIH),(X'80',CNAVERH)                      
         GOTO1 FLDVAL,DMCB,(X'01',CNAFRSTH),CNAMOREH                            
         SPACE                                                                  
         USING TARDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TARDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE                                                                  
         GOTO1 CPSMOVE,DMCB,(L'CNACLI,CNACLIH),(L'CNACLIN,CNACLINH),   +        
               (L'TARDCLI,TARDCLI),TLCLCDQ                                      
         GOTO1 CPSMOVE,DMCB,(L'CNAPRD,CNAPRDH),(L'CNAPRDN,CNAPRDNH),   +        
               (L'TARDPRD,TARDPRD),TLPRCDQ                                      
         DROP  R4                                                               
         SPACE                                                                  
         LA    R2,CNAFRSTH         R2=A(FIRST COMMERCIAL FIELD)                 
         LA    R3,CNALSTH          R3=A(LAST COMMERCIAL FIELD)                  
         SPACE                                                                  
         USING TAASD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAASELQ      GET CONTRACT ASSIGN ELEMENTS                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DA10     BAS   RE,NEXTEL                                                        
         BNE   DA30                                                             
         SPACE                                                                  
         CR    R2,R3               IF NOT PAST THE LAST COMMERCIAL              
         BH    DA20                FIELD                                        
         LH    RE,CURCCNT                                                       
         AHI   RE,1                                                             
         STH   RE,CURCCNT          BUMP UP CURRENT COMMERCIAL COUNTER           
         CH    RE,LSTCCNT          IF NOT LOWER THAN LAST TIME'S                
         BL    DA10                COMMERCIAL COUNTER                           
         BAS   RE,DADCID           DISPLAY THE ASSIGNED COMMERCIAL INFO         
         AHI   R2,DASLNQ           AND BUMP TO NEXT SET OF COMMERCIAL           
         B     DA10                FIELDS                                       
         DROP  R4                                                               
         SPACE                                                                  
DA20     MVC   CNAMORE,MOREMSG     IF MORE CONTRACTS THAN COMMERCIAL            
         MVI   CNAMOREH+5,L'MOREMSG         FIELDS, INDICATE AS SUCH            
         B     DA40                                                             
         SPACE                                                                  
DA30     XC    CURCCNT,CURCCNT     IF ALL FIT ON SCREEN, CLEAR COUNTER          
         SPACE                                                                  
DA40     GOTO1 ACTVOUT,DMCB,(X'80',CNALCHGH)          DISPLAY ACTIVITY          
         SPACE                                                                  
         LA    R2,CNACLIH          IF CLIENT NOT INPUT                          
         CLI   CNACLIH+5,0         POSITION CURSOR AT CLIENT                    
         BE    *+8                                                              
         LA    R2,CNACIDH          IF CLIENT INPUT                              
         OI    6(R2),X'C0'         POSITION CURSOR AT COM ID                    
         SPACE                                                                  
         CLI   MODE,VALKEY                                                      
         BE    INFENTC                                                          
DAX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MOVE CLIENT/PRODUCT/SS# TO SCREEN                     
         SPACE 2                                                                
CPSMOVE  NTR1                                                                   
         ZICM  R2,1(R1),3          R2=A(CODE SCREEN FIELD)                      
         MVC   CSFLEN,0(R1)        CSFLEN=L'CODE SCREEN FIELD                   
         SPACE                                                                  
         ZICM  R3,5(R1),3          R3=A(NAME SCREEN FIELD)                      
         MVC   NSFLEN,4(R1)        CSFLEN=L'NAME SCREEN FIELD                   
         SPACE                                                                  
         ZICM  R4,9(R1),3          R4=A(ELEMENT FIELD)                          
         MVC   ELMLEN,8(R1)        ELMLEN=L'ELEMENT FIELD                       
         SPACE                                                                  
         MVC   RECEQU,15(R1)       RECEQU=RECORD EQUATE                         
         SPACE                                                                  
         GOTO1 FLDVAL,DMCB,(X'01',(R2)),(R3)                                    
         SPACE                                                                  
         ZIC   RE,ELMLEN                                                        
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                EXIT IF NOTHING IN ELEMENT                   
         OC    0(0,R4),0(R4)       FIELD                                        
         BZ    CPSMX                                                            
         SPACE                                                                  
         ZIC   RE,CSFLEN                                                        
         STC   RE,5(R2)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                MOVE ELEMENT CODE INTO                       
         MVC   8(0,R2),0(R4)       SCREEN CODE FIELD                            
         SPACE                                                                  
         ZIC   R5,RECEQU                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,(R5),(X'0C',(R2)),(R3)                               
         MVC   AIO,AIO1                                                         
CPSMX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY ASSIGNED COMEMRCIAL'S CID                     
*              ON ENTRY, R2=A(FIELD TO DISPLAY CID IN)                          
*                        R4=A(CONTRACT ASSIGN ELEMENT)                          
         SPACE 2                                                                
         USING DASTABD,R2                                                       
         USING TAASD,R3                                                         
DADCID   NTR1                                                                   
         LR    R3,R4                                                            
         MVC   DASCID,TAASCID                                                   
         MVI   DASCIDH+5,L'TAASCID 1ST MOVE CID FROM ELEMENT TO SCREEN          
         SPACE                                                                  
         MVC   AIO,AIO2            THEN TRY TO GET THE COMMERCIAL               
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',TAASCOM)                             
         BNE   DADCX                                                            
         SPACE                                                                  
         USING TACOD,R4                                                         
         L     R4,AIO              IF ELEMENT IS FOR MAIN COMMERCIAL            
         MVI   ELCODE,TACOELQ      USE THE CID AND LENGTH FROM THE              
         BAS   RE,GETEL            MAIN COMMERCIAL ELEMENT                      
         BE    *+6                 AND TITLE FROM MAIN TITLE ELEMENT            
         DC    H'00'                                                            
         MVC   DASCID,TACOCID                                                   
         EDIT  (B1,TACOSEC),(3,DASLEN),ALIGN=LEFT                               
         GOTO1 CHAROUT,DMCB,TANAELQ,DASTITH                                     
         SPACE                                                                  
         MVI   TGBYTE,C'A'                                                      
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    *+8                                                              
         MVI   TGBYTE,1                                                         
         CLC   TAASVER,TGBYTE                                                   
         BE    DADCX                                                            
         CLI   TAASVER,0                                                        
         BE    DADCX                                                            
         DROP  R4                                                               
         SPACE                                                                  
         TM    TGSYSTAT,TASYS3VR   IF SYSTEM SET TO HANDLE                      
         BZ    DADC30              3 CHARACTER VERSION CODES                    
         CLI   TAASVER,26          AND VERSION IS GREATER THAN 26               
         BNH   DADC30                                                           
         SPACE 1                                                                
         USING VINDEXD,RE                                                       
         LA    RE,VERINDEX         FIND RECORD EQUATE FOR THIS                  
DADC10   CLI   0(RE),X'FF'         VERSION NUMBER                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TAASVER,VINDUPLM                                                 
         BNH   DADC20                                                           
         LA    RE,VINDLNQ(RE)                                                   
         B     DADC10                                                           
         SPACE 1                                                                
DADC20   L     R4,AIO              GET COMMERCIAL RECORD FOR                    
         XC    KEY,KEY             THAT VERSION                                 
         MVC   KEY(L'TLCOKEY),0(R4)                                             
         MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),VINDEQUT                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   DADCX                                                            
         GOTO1 GETREC                                                           
         DROP  RE                                                               
         SPACE 1                                                                
         USING TAVRD,R4                                                         
DADC30   L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ      IF ELEMENT IS FOR VERSION                    
         BAS   RE,GETEL            USE THE CID AND LENGTH FROM                  
         B     *+8                 THE VERSION ELEMENT                          
DADC40   BAS   RE,NEXTEL                                                        
         BNE   DADCX                                                            
         CLC   TAVRVERS,TAASVER                                                 
         BNE   DADC40                                                           
         MVC   DASCID,TAVRCID                                                   
         EDIT  (B1,TAVRSEC),(3,DASLEN),ALIGN=LEFT                               
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLVRCDQ,(X'A4',TAVRVERS)                             
         BNE   DADCX               TRY TO GET VERSION RECORD                    
         DROP  R4                                                               
         SPACE                                                                  
         USING TACOD,R4                                                         
         L     R4,AIO              IF IT EXISTS                                 
         MVI   ELCODE,TACOELQ      USE ITS LENGTH AND TITLE                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         EDIT  (B1,TACOSEC),(3,DASLEN),ALIGN=LEFT                               
         GOTO1 CHAROUT,DMCB,TANAELQ,DASTITH                                     
         DROP  R4                                                               
         SPACE                                                                  
DADCX    MVC   AIO,AIO1                                                         
         MVI   ELCODE,TAASELQ                                                   
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*              ROUTINE TO VALIDATE CONTRACT RECORD                              
         SPACE 2                                                                
VREC     CLI   OVERLAY,MAINSCR                                                  
         BE    VMAIN                                                            
         CLI   OVERLAY,ASSNSCR                                                  
         BE    VASGN                                                            
         DC    H'00'                                                            
         EJECT                                                                  
*              ROUTINE TO VALIDATE MAIN CONTRACT SCREEN                         
         SPACE 2                                                                
VMAIN    GOTO1 SAVPTRS,DMCB,ASPBLK SAVE PASSIVE POINTERS                        
         SPACE                                                                  
         MVI   ELCODE,TARDELQ      DELETE OLD CONTRACT DETAILS ELEMENT          
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         MVI   ELCODE,TAXCELQ      DELETE OLD CONTRACT COMMENT ELEMENT          
         GOTO1 REMELEM                                                          
         SPACE                                                                  
         GOTO1 NAMIN,DMCB,(1,TAFNELQ),CNTCONDH,TAFNTCON                         
         SPACE                                                                  
         USING TARDD,R4                                                         
         LA    R4,ELEMENT          R4=A(CONTRACT DETAILS ELEMENT)               
         XC    ELEMENT,ELEMENT                                                  
         MVI   TARDEL,TARDELQ      MOVE IN ELEMENT CODE                         
         MVI   TARDLEN,TARDLNQ     MOVE IN ELEMENT LENGTH                       
         SPACE                                                                  
         LA    R2,CNTDATEH                                                      
         GOTO1 DTVAL,DMCB,TARDDATE MOVE IN VALIDATED CONTRACT DATE              
         SPACE                                                                  
         USING TYPETABD,RE                                                      
         LA    RE,TYPETAB          RE=A(TYPE TABLE)                             
VM10     CLI   0(RE),X'FF'         EXIT WHEN END OF TABLE REACHED               
         BE    VM30                                                             
         ZICM  R2,TYPEDISP,2                                                    
         AR    R2,RA               R2=A(TYPE FIELD)                             
         CLI   5(R2),0             IF TYPE FIELD CHECKED OFF                    
         BE    VM20                                                             
         CLI   TARDTYP1,0          ERROR IF PREVIOUS TYPE FIELD                 
         BNE   ERRINV              CHECKED                                      
         CLI   TARDTYP2,0                                                       
         BNE   ERRINV                                                           
         MVC   TARDTYP1,TYPEEQU1   MOVE IN TYPE EQU                             
         MVC   TARDTYP2,TYPEEQU2                                                
VM20     LA    RE,TYPELNQ(RE)                                                   
         B     VM10                                                             
VM30     LA    R2,CNTNONUH         ONE TYPE MUST BE CHECKED OFF                 
         OC    TARDTYP1(2),TARDTYP1                                             
         BZ    ERRMISS                                                          
         DROP  RE                                                               
         SPACE                                                                  
*&&DO                                                                           
         USING STATTABD,RE                                                      
         LA    RE,STATTAB          RE=A(STATUS TABLE)                           
VM40     CLI   0(RE),X'FF'         EXIT WHEN END OF TABLE REACHED               
         BE    VM60                                                             
         ZICM  R2,STATDISP,2                                                    
         AR    R2,RA               R2=A(TYPE FIELD)                             
         CLI   5(R2),0             IF TYPE FIELD CHECKED OFF                    
         BE    VM50                                                             
         CLI   TARDSTAT,0          ERROR IF PREVIOUS STATUS FIELD               
         BNE   ERRINV              CHECKED                                      
         MVC   TARDSTAT,STATEQU    MOVE IN STATUS EQU                           
VM50     LA    RE,STATLNQ(RE)                                                   
         B     VM40                                                             
VM60     LA    R2,CNTSTAAH         ONE STATUS MUST BE CHECKED OFF               
         CLI   TARDSTAT,0                                                       
         BE    ERRMISS                                                          
         DROP  RE                                                               
*&&                                                                             
         SPACE                                                                  
         MVC   AIO,AIO2            USE AIO2 FOR RECVAL CALLS                    
         MVC   SAVEKEY,KEY         SAVE CONTRACT RECORD KEY                     
         SPACE                                                                  
         LA    R2,CNTCLIH                                                       
         CLI   CNTCLIH+5,0         CLIENT                                       
         BE    ERRMISS                                                          
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'28',CNTCLIH),CNTCLINH                     
         MVC   TARDCLI,TGCLI                                                    
         SPACE                                                                  
VM70     CLI   CNTPRDH+5,0         PRODUCT                                      
         BE    VM80                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'28',CNTPRDH),CNTPRDNH                     
         MVC   TARDPRD,TGPRD       MOVE IN PRODUCT INPUT                        
         SPACE                                                                  
VM80     LA    R2,CNTSSNH                                                       
         CLI   TARDTYP1,TARDTYCG   PERFORMER S/S IS REQUIRED                    
         BE    VM85                FOR PERFORMER S/S                            
         CLI   5(R2),0                                                          
         BE    VM90                                                             
         CLI   TARDTYP1,TARDTYCL   AND OPTIONAL FOR CHARACTER/                  
         BNE   ERRINV              NAME/LIKENESS                                
         B     VM86                                                             
VM85     CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
VM86     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'2C',CNTSSNH),CNTSSNNH                     
         MVC   TARDSSN,TGSSN       MOVE IN PRODUCT INPUT                        
         SPACE                                                                  
VM90     CLI   ACTNUM,ACTADD       IF NOT ACTION CHANGE                         
         BE    VM100               RESORE CONTRACT KEY AND RECORD               
         GOTO1 RECVAL,DMCB,TLCNCDQ,(X'A4',TGCNTRME)                             
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE                                                                  
VM100    MVC   AIO,AIO1            RESTORE AIO TO CONTRACT RECORD               
         SPACE                                                                  
         LA    R2,CNTFEEH          VALIDATE CONTRACT FEE                        
         ZIC   RF,5(R2)            FOR TWO DECIMAL PLACES                       
         LTR   RF,RF                                                            
         BZ    ERRMISS                                                          
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ERRINVA                                                          
         TM    4(R1),X'80'         DO NO ALLOW NEGATIVES                        
         BO    ERRINVA                                                          
         MVC   TARDCFEE,4(R1)                                                   
         SPACE                                                                  
VM110    GOTO1 ADDELEM             ADD THE CONTRACT DETAILS ELEMENT             
         DROP  R4                                                               
         SPACE                                                                  
         LA    R2,CNTCMNTH         ENSURE THAT THE COMMENT WON'T                
         ZIC   R3,5(R2)            PUSH RECORD LENGTH OVER 2000                 
         AHI   RE,3                                                             
         BAS   RE,ENSURE                                                        
         SPACE                                                                  
         GOTO1 NAMIN,DMCB,(1,TACMELQ),(X'80',CNTCMNTH),TACMTYPG                 
         GOTO1 ACTVIN,DMCB,(X'80',CNTLCHGH)                                     
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING COMMERCIAL RECORD                  
         JNE   VMX                 READ SYSTEM RECORD                           
         BAS   RE,MYADDREC                                                      
         MVI   IOOPT,C'Y'                                                       
                                                                                
VMX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
                                                                                
MYADDREC NTR1                                                                   
         USING TLCND,R4                                                         
         L     R4,AIO1                                                          
                                                                                
         USING TLDRD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TLDRKEY,TLCNKEY     SET KEY WITH RECORD KEY                      
         MVC   TLDRSTAT,TLCNSTAT                                                
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                                                             
                                                                                
         CLC   TLDRKEY,KEYSAVE     IF RECORD IS FOUND                           
         JNE   MYAR10                                                           
         TM    TLDRSTAT,X'80'      IT MUST BE DELETED                           
         JO    *+6                                                              
         DC    H'0'                                                             
         MVC   TLDRKEY,KEYSAVE     WRITE BACK KEY IN UNDELETED STATUS           
         MVC   TLDRSTAT,KEYSAVE+TLDRSTAT-TLDRD                                  
         GOTO1 WRITE                                                            
         DROP  R3                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'       READ RECORD FOR UPDATE                       
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         NI    TLCNSTAT,X'7F'      PUT BACK IN UNDELETED STATUS                 
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
         J     MYARX                                                            
         DROP  R4                                                               
                                                                                
MYAR10   MVC   KEY,KEYSAVE         IF RECORD WAS NOT FOUND                      
         GOTO1 ADDREC              ADD IT                                       
                                                                                
MYARX    NI    DMINBTS,X'F7'       TURN OFF READ FOR DELETED                    
         J     XIT                                                              
                                                                                
*              ROUTINE TO ENSURE THAT CHANGE WON'T PUSH RECORD                  
*              LENGTH OVER 2000                                                 
*              ON ENTRY, R3=NUMBER OF BYTES TO BE ADDED                         
         SPACE                                                                  
         USING TLRCD,R4                                                         
ENSURE   NTR1                                                                   
         L     R4,AIO                                                           
         ZICM  RE,TLRCLEN          ENSURE THAT THE NEW ELEMENT(S)               
         AR    RE,R3               WON'T PUSH RECORD LENGTH OVER                
         AHI   RE,TAACLNQ          2000                                         
         CHI   RE,2000                                                          
         BNL   ERRINV                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE CONTRACT (UN)ASSIGN SCREEN                   
         SPACE 2                                                                
VASGN    TM    STATUS,DISPMORE     IF JUST WANT TO DISPLAY NEXT                 
         BO    DREC                PAGE OF COMM'LS, GO DO SO                    
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCNCDQ,(X'A4',TGCNTRME)                             
         BE    *+6                                                              
         DC    H'00'               GET CONTRACT RECORD                          
         GOTO1 SAVPTRS,DMCB,ASPBLK AND SAVE PASSIVE POINTERS                    
         SPACE                                                                  
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY,KEY         SAVE CONTRACT RECORD KEY                     
         SPACE                                                                  
         LA    R2,CNACLIH          CLIENT MUST EXIST                            
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'28',CNACLIH),CNACLINH                     
         SPACE                                                                  
         XC    TGPRD,TGPRD         PRODUCT IS OPTIONAL                          
         CLI   CNAPRDH+5,0                                                      
         BE    VA10                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'28',CNAPRDH),CNAPRDNH                     
         SPACE                                                                  
VA10     LA    R2,CNACIDH          COMMERCIAL MUST EXIST                        
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         GOTO1 RECVAL,DMCB,(X'20',TLCOICDQ),(X'20',(R2))                        
         SPACE                                                                  
         USING TLCOPD,R4                                                        
         LA    R4,KEY                                                           
         MVC   SVCOMCID,TLCOICID   SAVE COMMERCIAL/VERSION CID                  
         MVC   TGCOM,TLCOICOM      INTERNAL COMMERCIAL NUM                      
         MVC   TGVER,TLCOIVER      AND VERSION CODE                             
         DROP  R4                                                               
         SPACE                                                                  
         USING TLCOD,R4                                                         
         TM    TGSYSTAT,TASYS3VR   IF SYSTEM SET TO HANDLE 3-CHARACTER          
         BZ    VA12                VERSIONS                                     
         L     R4,AIO                                                           
         CLI   TLCOVER,TLCOV026    AND AIO DOES NOT CONTAIN MAIN                
         BE    VA12                COMMERCIAL RECORD, GO GET IT                 
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)                                   
         BE    VA12                                                             
         DC    H'00'                                                            
         DROP  R4                                                               
         SPACE                                                                  
VA12     MVI   INPUTVER,0                                                       
         SPACE                                                                  
         LA    R2,CNAVERH                                                       
         CLI   5(R2),0                                                          
         BE    VA15                                                             
         CLI   5(R2),1                                                          
         BNE   ERRINV                                                           
         MVC   INPUTVER,8(R2)      SAVE VERSION LETTER/NUMBER                   
         TM    TGSYSTAT,TASYS3VR   IN INPUTVER                                  
         BZ    VA15                                                             
         CLI   INPUTVER,C'*'                                                    
         BE    VA15                                                             
         GOTO1 VALINUM                                                          
         MVC   INPUTVER,ACTUAL                                                  
         SPACE                                                                  
VA15     BAS   RE,HNDVERID         SET UP SCREEN IF VERSION ID                  
         BE    VA20                INPUTTED (INSTEAD OF COM ID)                 
         SPACE                                                                  
         BAS   RE,HNDLVERS         IF ASSIGNING ALL VERSIONS                    
         BE    VA20                SAVE ALL VERSIONS INTO TABLE                 
         SPACE                                                                  
         BAS   RE,HNDAVERS         SET UP SCREEN IF VERSION A ID                
         BE    VA20                INPUTTED WITH NO VERS LETTER                 
         SPACE                                                                  
         BAS   RE,HNDVERIN         VALIDATE VERSION LETTER INPUT                
         SPACE                                                                  
         USING TLCOD,R4                                                         
VA20     CLI   ACTNUM,ACTASSGN     IF ACTION IS ASSIGN                          
         BNE   VA30                                                             
         L     R4,AIO                                                           
         LA    R2,CNACIDH                                                       
         CLC   TLCOCLI,TGCLI       COMMERCIAL CLIENT MUST MATCH                 
         BNE   ERRINV              CONTRACT CLIENT                              
         OC    TGPRD,TGPRD                                                      
         BZ    VA30                                                             
         CLC   TLCOPRD,TGPRD       COMMERCIAL PRODUCT MUST MATCH                
         BNE   ERRINV              CONTRACT PRODUCT                             
         DROP  R4                                                               
         SPACE                                                                  
VA30     MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLCNCDQ,(X'B4',TGCNTRME)                             
         BE    *+6                                                              
         DC    H'00'               RESTORE CONTRACT KEY AND RECORD              
         SPACE                                                                  
         CLI   ACTNUM,ACTASSGN     IF ACTION IS ASSIGN                          
         BNE   VA50                                                             
         LHI   R3,TAASLNQ          ENSURE THAT THE NEW ELEMENT(S)               
         CLI   CNAVERH+5,1         WON'T PUSH RECORD LENGTH OVER                
         BNE   VA40                2000                                         
         CLI   INPUTVER,C'*'                                                    
         BNE   VA40                                                             
         MH    R3,VTCOUNT                                                       
VA40     BAS   RE,ENSURE                                                        
         SPACE                                                                  
         USING TARDD,R4                                                         
VA50     L     R4,AIO                                                           
         MVI   ELCODE,TARDELQ                                                   
         BAS   RE,GETEL            R4=A(CONTRACT DETAILS ELEMENT)               
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   TARDCLI,TGCLI       SAVE CLIENT                                  
         MVC   TARDPRD,TGPRD       AND PRODUCT                                  
         DROP  R4                                                               
         SPACE                                                                  
         LA    R2,CNACIDH          R2=A(COMMERCIAL ID FIELD)                    
         SPACE                                                                  
         USING TAASD,R4                                                         
VA60     L     R4,AIO                                                           
         MVI   ELCODE,TAASELQ      GET CONTRACT ASSIGN ELEMENTS                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VA70     BAS   RE,NEXTEL                                                        
         BNE   VA110                                                            
         SPACE                                                                  
         CLI   CNAVERH+5,1                                                      
         BNE   VA80                                                             
         CLI   INPUTVER,C'*'       IF ASSIGNING/UNASSIGNING ALL                 
         BNE   VA80                VERSIONS                                     
         CLC   TAASCOM,TGCOM       MARK ALL VERSIONS WITH MATCHING              
         BNE   VA70                INTERNAL COMMERCIAL ID FOR                   
         MVI   0(R4),X'FF'         DELETION                                     
         B     VA70                                                             
         SPACE                                                                  
VA80     CLC   TAASCOM,TGCOM       IF COMMERCIAL ID/                            
         BNE   VA70                                                             
         MVI   TGBYTE,C'A'                                                      
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    *+8                                                              
         MVI   TGBYTE,1                                                         
         CLC   INPUTVER,TGBYTE     (TREAT VERSION LETTER ZERO AS                
         BNE   VA90                 A VERSION)                                  
         CLI   TAASVER,0                                                        
         BE    VA100                                                            
VA90     CLC   TAASVER,INPUTVER    VERSION LETTER ALREADY EXISTS                
         BNE   VA70                                                             
VA100    CLI   ACTNUM,ACTASSGN     CANNOT BE ASSIGNED AGAIN                     
         BE    ERRINV                                                           
         MVI   TAASEL,X'FF'        IF UNASSIGN, MARK FOR DELETION               
         B     VA140                                                            
         SPACE                                                                  
VA110    CLI   CNAVERH+5,1                                                      
         BNE   VA130                                                            
         CLI   INPUTVER,C'*'       IF ASSIGNING/UNASSIGNING ALL                 
         BNE   VA130               VERSIONS                                     
         CLI   ACTNUM,ACTUASGN                                                  
         BE    VA140                                                            
         MVI   ELCODE,X'FF'        DELETE ALL PRE-EXISTING VERSIONS             
         GOTO1 REMELEM                                                          
         USING VERTABD,R3                                                       
         LA    R3,VERTAB                                                        
VA120    CLI   0(R3),X'FF'         AND ADD ALL THE COMMERCIAL'S                 
         BE    VA150               VERSIONS                                     
         GOTO1 ADDTAAS,DMCB,VTCID,VTVER                                         
         LA    R3,VTLNQ(R3)                                                     
         B     VA120                                                            
         SPACE                                                                  
VA130    CLI   ACTNUM,ACTUASGN     IF UNASSIGN AND COMM ID NOT FOUND            
         BE    ERRINV              GIVE ERROR                                   
         SPACE                                                                  
         GOTO1 ADDTAAS,DMCB,SVCOMCID,INPUTVER                                   
         B     VA150                BUILD COMMERCIAL ASSIGN ELEMENT             
         DROP  R4                                                               
         SPACE                                                                  
VA140    MVI   ELCODE,X'FF'         IF UNASSIGN, DELETE UNASSIGNED              
         GOTO1 REMELEM              COMMERCIAL ELEMENT                          
         SPACE                                                                  
VA150    GOTO1 ACTVIN,DMCB,(X'80',CNTLCHGH)                                     
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'08',ASPBLK),APBLK                                
         B     DREC                                                             
         EJECT                                                                  
*              ROUTINE TO SET UP SCREEN IF VERSION ID IS INPUT                  
*              (INSTEAD OF COMMERCIAL ID)                                       
         SPACE 2                                                                
HNDVERID NTR1                                                                   
         CLI   TGVER,0             EXIT IF VERSION ID WAS                       
         BE    NO                  NOT ENTERED                                  
         SPACE                                                                  
         CLI   5(R2),0             VERSION LETTER INPUT                         
         BNE   ERRINV              IS INVALID (IN THIS CASE)                    
         SPACE                                                                  
         MVC   INPUTVER,TGVER                                                   
         SPACE                                                                  
         MVC   CNAVER(1),TGVER     DISPLAY VERSION LETTER                       
         MVI   CNAVERH+5,1         ON SCREEN                                    
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    HVI10                                                            
         EDIT  TGVER,CNAVER,ALIGN=LEFT                                          
         STC   R0,CNAVERH+5                                                     
         SPACE                                                                  
         USING TACOD,R4                                                         
HVI10    L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      REPLACE VERSION ID ON                        
         BAS   RE,GETEL            SCREEN WITH COMMERCIAL ID                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   CNACID,TACOCID                                                   
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET UP SCREEN IF VERSION A ID IS                      
*              INPUTTED WITH NO VERSION LETTER                                  
         SPACE 2                                                                
HNDAVERS NTR1                                                                   
         CLI   CNAVERH+5,0          IF NO VERSION LETTER INPUTTED               
         BNE   NO                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ       AND VERSION LETTER NOT IN KEY               
         BAS   RE,GETEL             BUT VERSION ELEMENTS EXIST                  
         BNE   NO                                                               
         MVI   CNAVERH+5,1                                                      
         MVI   CNAVER,C'A'          MUST BE A VERSION                           
         MVI   INPUTVER,C'A'                                                    
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    YES                                                              
         MVI   CNAVER,C'1'                                                      
         MVI   INPUTVER,1                                                       
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD INTERNAL TABLE IF ASSIGNING                     
*              ALL VERSIONS                                                     
         SPACE 2                                                                
HNDLVERS NTR1                                                                   
         CLI   CNAVERH+5,1                                                      
         BNE   NO                                                               
         CLI   INPUTVER,C'*'       IF ASSIGNING ALL VERSIONS                    
         BNE   NO                                                               
         CLI   ACTNUM,ACTASSGN     AND ACTION IS ASSIGN                         
         BNE   YES                                                              
         SPACE                                                                  
         USING VERTABD,R3                                                       
         LA    R3,VERTAB                                                        
         MVI   0(R3),X'FF'         INITIALIZE VERSION TABLE                     
         XC    VTCOUNT,VTCOUNT     AND CLEAR VERSION COUNTER                    
         SPACE                                                                  
         USING TAVRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,GETEL            COMMERCIAL MUST HAVE AT LEAST                
         BNE   ERRINV              ONE VERSION                                  
         B     HLV20                                                            
         SPACE                                                                  
HLV10    BAS   RE,NEXTEL           FOR EACH VERSION ELEMENT                     
         BNE   HLV30                                                            
HLV20    MVC   VTCID,TAVRCID       ADD VERSION ID                               
         MVC   VTVER,TAVRVERS      AND VERSION LETTER                           
         AHI   R3,VTLNQ            TO VERSION TABLE                             
         MVI   0(R3),X'FF'         MARK NEW END OF VERSION TABLE                
         LH    RE,VTCOUNT          AND BUMP UP THE VERSION COUNTER              
         AHI   RE,1                                                             
         STH   RE,VTCOUNT                                                       
         B     HLV10                                                            
         DROP  R3,R4                                                            
         SPACE 1                                                                
HLV30    L     R4,AIO                                                           
         MVC   KEYSAVE(L'TLCOKEY),0(R4)                                         
         SPACE 1                                                                
HLV40    ZIC   RE,KEYSAVE+TLCOVER-TLCOD                                         
         AHI   RE,1                                                             
         CHI   RE,TLCOV250                                                      
         BH    HLV50                                                            
         STC   RE,KEYSAVE+TLCOVER-TLCOD                                         
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLCOKEY),KEYSAVE                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   HLV40                                                            
         GOTO1 GETREC                                                           
         BAS   RE,GETEL                                                         
         BE    HLV20                                                            
         B     HLV40                                                            
         SPACE 1                                                                
HLV50    XC    KEY,KEY                                                          
         MVC   KEY(L'TLCOKEY),KEYSAVE                                           
         MVI   KEY+TLCOVER-TLCOD,TLCOV026                                       
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE VERSION LETTER INPUT                         
         SPACE 2                                                                
HNDVERIN NTR1                                                                   
         CLI   5(R2),0             IF VERSION LETTER INPUTTED                   
         BE    HVX                                                              
         SPACE                                                                  
HV10     L     R4,AIO                                                           
         MVC   KEYSAVE(L'TLCOKEY),0(R4)                                         
         SPACE                                                                  
         USING TAVRD,R4                                                         
         MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
HV20     BAS   RE,NEXTEL           VERSION MUST EXIST                           
         BNE   HV30                ON COMMERCIAL                                
         CLC   INPUTVER,TAVRVERS                                                
         BNE   HV20                                                             
         MVC   SVCOMCID,TAVRCID                                                 
         B     HV40                                                             
         DROP  R4                                                               
         SPACE                                                                  
HV30     ZIC   RE,KEYSAVE+TLCOVER-TLCOD                                         
         AHI   RE,1                                                             
         CHI   RE,TLCOV250                                                      
         BH    ERRINV                                                           
         STC   RE,KEYSAVE+TLCOVER-TLCOD                                         
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLCOKEY),KEYSAVE                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   HV30                                                             
         GOTO1 GETREC                                                           
         B     HV10                                                             
         SPACE 1                                                                
HV40     XC    KEY,KEY                                                          
         MVC   KEY(L'TLCOKEY),KEYSAVE                                           
         MVI   KEY+TLCOVER-TLCOD,TLCOV026                                       
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
HVX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD CONTRACT ASSIGN ELEMENT                           
         SPACE 2                                                                
         USING TAASD,R4                                                         
ADDTAAS  NTR1                                                                   
         L     RE,0(R1)            RE=A(COMMERCIAL/VERSION CID)                 
         L     RF,4(R1)            RF=A(VERSION LETTER)                         
         SPACE                                                                  
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     BUILD COMMERCIAL ASSIGN ELEMENT              
         MVI   TAASEL,TAASELQ                                                   
         MVI   TAASLEN,TAASLNQ                                                  
         MVC   TAASCID,0(RE)       WITH COMERCIAL/VERSION CID                   
         MVC   TAASCOM,TGCOM       INTERNAL COMMERCIAL ID                       
         MVC   TAASVER,0(RF)       AND VERSION LETTER                           
         GOTO1 ADDELEM             ADD IT TO CONTRACT RECORD                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO HANDLE DELETION OF CONTRACT RECORD                    
         SPACE                                                                  
DELREC   GOTO1 RECVAL,DMCB,TLCNCDQ,(X'A4',TGCNTRME)                             
         BE    *+6                                                              
         DC    H'00'                GET CONTRACT RECORD                         
         LA    R2,CNTAGYH                                                       
         L     R4,AIO                                                           
         MVI   ELCODE,TAASELQ                                                   
         BAS   RE,GETEL                                                         
         BE    ERRNODL                                                          
DELRECX  B     XIT                                                              
         EJECT                                                                  
*              ERRORS                                                           
         SPACE                                                                  
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
         SPACE                                                                  
ERRINVA  MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     ERREXIT                                                          
         SPACE                                                                  
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERREXIT                                                          
         SPACE                                                                  
ERRNOFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     ERREXIT                                                          
         SPACE                                                                  
INVPFK   MVI   ERROR,ERINVPFK      INVALID PF KEY                               
         B     ERREXIT                                                          
         SPACE                                                                  
ERRNODL  MVI   ERROR,ERINVDEL      RECORD NOT AVAILABLE FOR DELETION            
         B     ERREXIT                                                          
         SPACE                                                                  
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREXIT  B     MESSXIT             ERROR EXIT                                   
         SPACE                                                                  
INFENTC  MVI   MYMSGNO1,107        RECORD DISPLAYED - ENTER CHANGES             
         B     INFEXIT                                                          
         SPACE                                                                  
INFEXIT  LA    R2,EFHREC           INFORMATION EXIT                             
         OI    GENSTAT2,USGETTXT                                                
         B     MESSXIT                                                          
         SPACE                                                                  
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         B     MESSXIT                                                          
         SPACE                                                                  
MESSXIT  GOTO1 EXIT,DMCB,0         ALL MESSAGES EXIT                            
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              TABLES                                                           
         SPACE 2                                                                
*              TABLE OF TYPE FIELDS AND CORRESPONDING EQUATES                   
         SPACE 1                                                                
TYPETAB  DS    0X                                                               
         DC    AL2(CNTNONUH-T702FFD)   NON-UNION AGREEMENT                      
         DC    AL1(TARDTYNU,0)                                                  
         DC    AL2(CNTMUSLH-T702FFD)   MUSIC LICENSE                            
         DC    AL1(TARDTYML,0)                                                  
         DC    AL2(CNTCNLKH-T702FFD)   CHARACTER/NAME/LIKENESS                  
         DC    AL1(TARDTYCL,0)                                                  
         DC    AL2(CNTPRORH-T702FFD)   PROPERTY RELEASE                         
         DC    AL1(TARDTYPR,0)                                                  
         DC    AL2(CNTSTFAH-T702FFD)   STOCK FOOTAGE AGREEMENT                  
         DC    AL1(TARDTYSF,0)                                                  
         DC    AL2(CNTMSTRH-T702FFD)   MASTER RECORDING                         
         DC    AL1(TARDTYMR,0)                                                  
         DC    AL2(CNTCOVGH-T702FFD)   CELEBRITY/OVERSCALE GUARANTEE            
         DC    AL1(TARDTYCG,0)                                                  
         DC    AL2(CNTNEDMH-T702FFD)   NEEDLEDROP MUSIC                         
         DC    AL1(TARDTYNM,0)                                                  
         DC    AL2(CNTOTHRH-T702FFD)   OTHER                                    
         DC    AL1(TARDTYOT,0)                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*              TABLE OF STATUS FIELDS AND CORRESPONDING EQUATES                 
         SPACE 1                                                                
STATTAB  DS    0X                                                               
         DC    AL2(CNTSTAAH-T702FFD)   ACTIVE                                   
         DC    AL1(TARDSTAA)                                                    
         DC    AL2(CNTSTAEH-T702FFD)   EXPIRED                                  
         DC    AL1(TARDSTAE)                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*              TABLE OF SCREEN ADDRESSES FOR MAINTENANCE KEY FIELDS             
         SPACE 1                                                                
KEYTABM  DS    0X                                                               
         DC    AL2(CNTAGYH-T702FFD)    AGENCY                                   
         DC    AL2(CNTCONIH-T702FFD)   CONTRACT ID                              
         DC    AL2(CNTTSTAH-T702FFD)   TERM START                               
         DC    AL2(CNTTENDH-T702FFD)   TERM END                                 
         DC    X'FF'                                                            
         SPACE 2                                                                
*              TABLE OF SCREEN ADDRESSES FOR ASSIGN KEY FIELDS                  
         SPACE 1                                                                
KEYTABA  DS    0X                                                               
         DC    AL2(CNAAGYH-T702FFD)    AGENCY                                   
         DC    AL2(CNACONIH-T702FFD)   CONTRACT ID                              
         DC    AL2(CNATSTAH-T702FFD)   TERM START                               
         DC    AL2(CNATENDH-T702FFD)   TERM END                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*              TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE                   
*              VERSION CODE IS ON                                               
         SPACE 1                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*              TABLE OF SCREEN ADDRESSES FOR ASSIGN KEY FIELDS                  
*              TABLE OF PF KEYS FOR MAINTENANCE SCREEN                          
         SPACE 2                                                                
PFTABM   DS    0X                   TABLE OF PFKEY ACTIONS                      
         DC    AL1(PFM13X-*,13,0,(PFM13X-PFM13)/KEYLNQ,0)                       
         DC    CL3' ',CL8'CONTRACT',CL8'LIST    '                               
PFM13    DC    AL1(KEYTYTWA,L'CNTAGY-1),AL2(CNTAGY-T702FFD)                     
PFM13X   EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PFM14X-*,14,0,(PFM14X-PFM14)/KEYLNQ,0)                       
         DC    CL3' ',CL8'CONTRACT',CL8'ASSIGN  '                               
PFM14    DC    AL1(KEYTYTWA,L'CNTAGY-1),AL2(CNTAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'CNTCONI-1),AL2(CNTCONI-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNTTSTA-1),AL2(CNTTSTA-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNTTEND-1),AL2(CNTTEND-T702FFD)                   
PFM14X   EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PFM15X-*,15,0,(PFM15X-PFM15)/KEYLNQ,0)                       
         DC    CL3' ',CL8'CONTRACT',CL8'UASSIGN '                               
PFM15    DC    AL1(KEYTYTWA,L'CNTAGY-1),AL2(CNTAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'CNTCONI-1),AL2(CNTCONI-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNTTSTA-1),AL2(CNTTSTA-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNTTEND-1),AL2(CNTTEND-T702FFD)                   
PFM15X   EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PFM16X-*,16,0,(PFM16X-PFM16)/KEYLNQ,0)                       
         DC    CL3' ',CL8'COMMENT ',CL8'DISPLAY '                               
PFM16    DC    AL1(KEYTYTWA,L'CNTAGY-1),AL2(CNTAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'COMTYPA-1),AL2(COMTYPA-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNTCONI-1),AL2(CNTCONI-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CODEBLN-1),AL2(CODEBLN-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNTTSTA-1),AL2(CNTTSTA-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNTTEND-1),AL2(CNTTEND-T702FFD)                   
PFM16X   EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*              TABLE OF PF KEYS FOR ASSIGN/UNASSIGN SCREEN                      
         SPACE 2                                                                
PFTABA   DS    0X                   TABLE OF PFKEY ACTIONS                      
         DC    AL1(PFA13X-*,13,0,(PFA13X-PFA13)/KEYLNQ,0)                       
         DC    CL3' ',CL8'CONTRACT',CL8'LIST    '                               
PFA13    DC    AL1(KEYTYTWA,L'CNAAGY-1),AL2(CNAAGY-T702FFD)                     
PFA13X   EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PFA14X-*,14,0,(PFA14X-PFA14)/KEYLNQ,0)                       
         DC    CL3' ',CL8'CONTRACT',CL8'DISPLAY '                               
PFA14    DC    AL1(KEYTYTWA,L'CNAAGY-1),AL2(CNAAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'CNACONI-1),AL2(CNACONI-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNATSTA-1),AL2(CNATSTA-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNATEND-1),AL2(CNATEND-T702FFD)                   
PFA14X   EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PFA16X-*,16,0,(PFA16X-PFA16)/KEYLNQ,0)                       
         DC    CL3' ',CL8'COMMENT ',CL8'DISPLAY '                               
PFA16    DC    AL1(KEYTYTWA,L'CNAAGY-1),AL2(CNAAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'COMTYPA-1),AL2(COMTYPA-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNACONI-1),AL2(CNACONI-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CODEBLN-1),AL2(CODEBLN-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNATSTA-1),AL2(CNATSTA-T702FFD)                   
         DC    AL1(KEYTYTWA,L'CNATEND-1),AL2(CNATEND-T702FFD)                   
PFA16X   EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PFA17X-*,17,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8'        ',CL8'        '                               
PFA17X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              CONSTANTS, LITERALS, ETC.                                        
MAINSCR  EQU   X'EE'                                                            
ASSNSCR  EQU   X'F7'                                                            
         SPACE 2                                                                
ASG2COM  DC    CL17'Assign to Com ID:'                                          
UASFCOM  DC    CL17'Unassign Com ID:'                                           
         SPACE 2                                                                
MOREMSG  DC    CL12'(More...)'                                                  
         LTORG                                                                  
         EJECT                                                                  
VERTAB   DS    XL3251             SAVED VERSION INFORMATION TABLE               
*                                 ((12 + 1) X 250) + 1) = 339                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCREED                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRF7D                                                       
         EJECT                                                                  
*              DSECT TO COVER LOCAL SAVED STORAGE AT END OF TWA                 
         SPACE 2                                                                
         ORG   CNTWORK            ORG TO THE END OF BIGGER SCREEN               
         SPACE 2                                                                
         DS    D                                                                
STATUS   DS    X                  PROGRAM STATUS BYTE                           
DISPMORE EQU   X'80'              DISPLAY MORE                                  
         SPACE 1                                                                
LSTCCNT  DS    H                  # OF COMMERCIALS DISPLAYED LAST TIME          
CURCCNT  DS    H                  # OF COMMERCIALS PROCESSED THIS TIME          
         SPACE 1                                                                
CSFLEN   DS    X                  CPSMOVE ROUTINE'S VARIABLES                   
NSFLEN   DS    X                                                                
ELMLEN   DS    X                                                                
RECEQU   DS    X                                                                
         SPACE 1                                                                
SAVEKEY  DS    XL(L'TLCNKEY)      SAVE CONTRACT KEY                             
         SPACE 1                                                                
SVCOMCID DS    CL12               SAVED COMMERCIAL ID                           
         SPACE 1                                                                
VTCOUNT  DS    H                  # OF VERSIONS IN VERSION TABLE                
         SPACE 1                                                                
COMTYPA  DS    C                  COMMENT RECORD VARIABLES                      
CODEBLN  DS    C                                                                
         SPACE 1                                                                
INPUTVER DS    X                                                                
END      EQU   (*-CONHEADH)        INSURANCE AGAINST OVERWRITING                
         DS    CL(3520-END)        DDGENTWA                                     
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         EJECT                                                                  
*        DSECT TO COVER TABLE OF TYPE FIELDS AND EQUATES                        
         SPACE 2                                                                
TYPETABD DSECT                                                                  
TYPEDISP DS    AL2                     DISPLACEMENT OF SCREEN FIELD             
TYPEEQUS DS    0XL2                                                             
TYPEEQU1 DS    XL1                     TYPE EQUATE FOR TARDTYP1                 
TYPEEQU2 DS    XL1                     TYPE EQUATE FOR TARDTYP2                 
TYPELNQ  EQU   *-TYPETABD                                                       
         SPACE 2                                                                
STATTABD DSECT                                                                  
STATDISP DS    AL2                     DISPLACEMENT OF SCREEN FIELD             
STATEQU  DS    XL1                     STATUS EQUATE FOR TARDSTAT               
STATLNQ  EQU   *-STATTABD                                                       
         SPACE 2                                                                
KEYTABD  DSECT                                                                  
KEYAGY   DS    AL2                     DISPLACEMENT TO AGENCY FIELD             
KEYCONI  DS    AL2                     DISPLACEMENT TO CONTRACT ID FLD          
KEYTSTA  DS    AL2                     DISPLACEMENT TO TERM START FLD           
KEYTEND  DS    AL2                     DISPLACEMENT TO TERM END FLD             
         SPACE 2                                                                
VERTABD  DSECT                                                                  
VTCID    DS    CL(L'TAASCID)           VERSION ID                               
VTVER    DS    CL(L'TAASVER)           VERSION LETTER                           
VTLNQ    EQU   *-VTCID                                                          
         EJECT                                                                  
DASTABD  DSECT                                                                  
DASCIDH  DS    XL8                     ASSIGN SCREEN CID                        
DASCID   DS    CL12                                                             
DASTITH  DS    XL8                     ASSIGN SCREEN COMM TITLE                 
DASTIT   DS    CL36                                                             
DASLENH  DS    XL8                     ASSIGN SCREEN LENGTH                     
DASLEN   DS    CL3                                                              
DASLNQ   EQU   *-DASTABD                                                        
         EJECT                                                                  
*              DSECT TO COVER TABLE THAT DETERMINES WHICH COMMERCIAL            
*              RECORD THE VERSION CODE IS ON                                    
         SPACE 1                                                                
VINDEXD  DSECT                                                                  
VINDUPLM DS    X                 RECORD'S UPPER LIMIT                           
VINDEQUT DS    X                 RECORD'S EQUATE                                
VINDLNQ  EQU   *-VINDEXD                                                        
         EJECT                                                                  
TWAHOLED DSECT                                                                  
         DS    XL175                                                            
SVRECUSE DS    CL1                                                              
ASPBLK   DS    A                                                                
APBLK    DS    A                                                                
         EJECT                                                                  
SPBLK    EQU   L'TLDRREC*200+1                                                  
PBLK     EQU   L'TLDRREC*200+1                                                  
WORKLNQ  EQU   SPBLK+SPBLK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TAGENEE   04/11/14'                                      
         END                                                                    
