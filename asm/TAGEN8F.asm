*          DATA SET TAGEN8F    AT LEVEL 097 AS OF 05/29/15                      
*PHASE T7028FE,*                                                                
         TITLE 'T7028F - CLIENT/COMMERCIAL COPY'                                
T7028F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T7028F,R7                                                 
         LR    R5,RC               R5=A(TEMPORARY STORAGE)                      
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         LA    R6,TWAHOLE          R6=LOCAL STORAGE AREA                        
         USING LCLWSD,R6                                                        
         SPACE 1                                                                
         USING TMPD,R5                                                          
         LA    RE,SVPTRBLK                                                      
         ST    RE,ASVPBLK                                                       
         AHI   RE,L'SVPTRBLK                                                    
         ST    RE,AADPBLK                                                       
         DROP  R5                                                               
         EJECT                                                                  
*              MODE CONTROL                                                     
         SPACE 1                                                                
         BRAS  RE,SETSCRN          SETUP SCREEN                                 
         BRAS  RE,SETSELS          SETUP SELECT FIELDS                          
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   SCOSCST,C'Y'        IF SELECTING CAST                            
         BE    M100                                                             
         CLI   RECNUM,PC           OR RUNNING PER CYCLE TRANSFER                
         BE    M100                                                             
         MVI   ENDLIST,C'N'                                                     
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BNE   M20                                                              
         GOTO1 FLDVAL,DMCB,(X'40',AFRSTKEY),999 IF ANY FIELD CHGED              
         BNE   M04                                                              
         TM    STATUS,PFKPEND      IF PFKEY IS NOT PENDING                      
         BO    M10                                                              
         SPACE 1                                                                
M04      BAS   RE,VKEY             VALIDATE KEY FIELDS                          
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTKEY),999  MAKE ALL FIELDS VALID          
         SPACE 1                                                                
         CLI   TWASCR,SCR8E        IF CLIENT COPY - CONTINUE                    
         BE    MX                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE - CONTINUE                        
         BE    MX                                                               
         TM    STATUS,SELCAST      IF NOT SELECTING CAST                        
         BNO   PFKMSG              GIVE PFKEY MESSAGE                           
         B     M100                                                             
         SPACE 1                                                                
M10      CLI   PFAID,13            INSURE USER HIT PF13                         
         BNE   PFKMSG                                                           
         BAS   RE,VKEY                                                          
         B     MX                                                               
         SPACE 1                                                                
M20      CLI   MODE,VALREC         IF COMMERCIAL COPY/ONLINE                    
         BE    M30                                                              
         CLI   MODE,PRINTREP       CLIENT COPY/OFFLINE                          
         BNE   MX                                                               
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         SPACE 1                                                                
M30      MVC   AIO,AIO1            SET DEFAULT I/O AREA                         
         SPACE 1                                                                
         TM    STATUS,COPYHIST+CLICOPY  IF COPYING W/ HIST OR CLI COPY          
         BZ    *+12                                                             
         BAS   RE,RENAME           THEN REALLY ONLY RENAMING                    
         B     *+8                                                              
         BAS   RE,COPY             ELSE ACTUALLY COPY THE COMMERCIAL            
         SPACE 1                                                                
         TM    STATUS,CLICOPY      IF NOT CLIENT COPY                           
         BZ    COPIED              GIVE COMPLETION MESSAGE                      
         SPACE 1                                                                
         OC    COMLCTR,COMLCTR     IF SOMETHING WAS PRINTED                     
         BZ    M80                                                              
         BAS   RE,BXBOT            END BOX                                      
         EDIT  COMLCTR,(6,P+1),COMMAS=YES,ALIGN=LEFT  DISP COPY COUNT           
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(18,R1),=C'COMMERCIALS COPIED'                                  
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
M80      BAS   RE,ESTUPD           UPDATE ANY RELEVANT ESTIMATE RECS.           
         B     MX                                                               
         SPACE 2                                                                
M100     GOTO1 FLDVAL,DMCB,(X'40',AFRSTKEY),SCOSCST IF ANY FIELD CHGED          
         BE    M110                                                             
         MVI   ENDLIST,C'N'                                                     
         MVI   PCYSTAT,0                                                        
         BAS   RE,VKEY                VALIDATE KEY FIELDS                       
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTKEY),SCOSCST MAKE ALL FLDS VALID         
         SPACE 1                                                                
M110     TM    STATUS,PFKPEND      IF PFKEY IS NOT PENDING                      
         BO    M130                                                             
         CLI   LISTED,C'Y'         & IF A PAGE WAS ALREADY LISTED               
         BNE   M120                                                             
         BAS   RE,SELECTC          THEN SELECT FROM IT                          
         BRAS  RE,OUTSEL           OUTPUT N'CAST MEMBERS SELECTED               
         SPACE 1                                                                
M120     BRAS  RE,CCAST            LIST CAST                                    
         OC    CONTKEY,CONTKEY     IF END OF LIST                               
         BNZ   SELINF                                                           
         B     LSTINF              GIVE LAST PAGE MESSAGE                       
         SPACE 1                                                                
M130     CLI   LSTPAGE,C'Y'        IF LAST PAGE DISPLAYED                       
         BNE   M140                                                             
         BAS   RE,SELECTC          THEN SELECT FROM IT                          
         MVI   LSTPAGE,C'N'                                                     
         BRAS  RE,CLRSCR                                                        
         BRAS  RE,OUTSEL           OUTPUT N'CAST MEMBERS SELECTED               
         CLI   SCOSCST,C'Y'        IF SELECTING CAST                            
         BE    PFKMSG              AND GIVE PF MESSAGE                          
         SPACE 1                                                                
M140     CLI   PFAID,13            INSURE USER HIT PF13                         
         BNE   PFKMSG                                                           
         MVI   ENDLIST,C'Y'                                                     
         BAS   RE,VKEY                                                          
         BAS   RE,ADDSEL           COPY SELECTED MEMBERS TO NEW COMML           
         BAS   RE,UPDCOMML         UPDATE COMMERCIAL STATUS BYTES               
         BRAS  RE,CLRSCR           CLEAR BOTTOM OF SCREEN                       
         BRAS  RE,OUTSEL           OUTPUT N'CAST MEMBERS SELECTED               
         B     COPIED                                                           
         SPACE 1                                                                
MX       B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    RE,LOCALD           CLEAR STORAGE                                
         LH    RF,=AL2(LOCALLNQ)                                                
         XCEFL                                                                  
         SPACE 1                                                                
         BRAS  RE,SETPMUS          SET PMUSIC STATUS                            
         SPACE 1                                                                
         CLI   TWASCR,SCR8E        IF CLIENT COPY - CONTINUE                    
         BE    VKCLI                                                            
         CLI   ENDLIST,C'Y'                                                     
         BE    VK10                IF JUST CHECKING IF COMML NOT ADDED          
         BRAS  RE,CLRSCR                                                        
         XC    NSEL,NSEL           CLEAR SCREEN & STORAGE                       
         MVI   LSTPAGE,C'N'                                                     
         MVI   LISTED,C'N'                                                      
         MVI   ENDLIST,C'N'                                                     
         SPACE 1                                                                
**NO-OP  CLI   TWASCR,SCR8E        GO TO APPROPRIATE VALIDATION ROUTINE         
**NO-OP  BNE   VKCOM                                                            
**NO-OP  B     VKCLI                                                            
VK10     B     VKCOM                                                            
         EJECT                                                                  
*              KEY VALIDATION ROUTINES FOR COMMERCIAL/COPY                      
         SPACE 1                                                                
VKCOM    DS    0H                                                               
         BRAS  RE,CLRSCR1          CLEAR NAMES                                  
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         LA    R2,SCOFAYH                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SCOFAYH),SCOFAYNH  FROM AGY           
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
         L     R4,AIO                                                           
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   FAYSTA6,TAAYSTA6                                                 
                                                                                
                                                                                
         MVC   AIO,AIO1            GET & SAVE COMML REC IN AIO1                 
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',SCOFCIDH),SCOFCINH                   
         BAS   RE,SETFRTO          SET FROM INFO FOR THIS COMMERCIAL            
                                                                                
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   AIO,AIO2                                                         
         MVI   FCISTA2,0                                                        
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',TLCOCLI)                              
         L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ                                                   
         USING TACID,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   FCISTA2,TACISTA2                                                 
         MVC   AIO,AIO1                                                         
         DROP  R3                                                               
*                                                                               
                                                                                
         SPACE 1                                                                
         USING TACOD,R4                                                         
         L     R4,AIO              R4=A("TO" COMMERCIAL RECORD)                 
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    TACOSTA2,TACOPCYC   IF "FROM" COMMERCIAL IS A                    
         BZ    VCO20               PER CYCLE COMMERCIAL                         
         CLI   RECNUM,PC           PERCYCLE/TRANSFER IS INVALID                 
         BE    ERRFRPCY                                                         
         CLI   SCOHIST,C'Y'        AND MUST COPY HISTORY                        
         BNE   ERRPCYCH                                                         
         DROP  R4                                                               
         SPACE 1                                                                
         USING TATRTABD,R2                                                      
VCO20    LA    R2,TATRTAB                                                       
         SPACE 1                                                                
         USING TATRD,R4                                                         
         L     R4,AIO              SAVE COMMERCIAL'S MUSIC TRACKS               
         MVI   ELCODE,TATRELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VCO30    BAS   RE,NEXTEL                                                        
         BNE   VCO40                                                            
         MVC   TTCOM,TATRCOM                                                    
         MVC   TTTRK,TATRTRK                                                    
         LA    R2,TTLNQ(R2)                                                     
         J     VCO30                                                            
         DROP  R4                                                               
         SPACE 1                                                                
VCO40    MVI   0(R2),X'FF'                                                      
         SPACE 1                                                                
         LA    R2,SCOTAYH                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SCOTAYH),SCOTAYNH  TO AGENCY          
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI2                                                         
         MVC   TOAGY,TGAGY         NEW AGENCY                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAAYD,R4                                                         
         MVC   AYAYSTA6,TAAYSTA6   SAVE 6TH STATUS FOR AGENCY                   
         DROP  R4                                                               
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TABRD,R4                                                         
         MVC   AYBRSTAT,TABRSTAT   SAVE BILLING STATUS FOR AGENCY               
         DROP  R4                                                               
         SPACE 1                                                                
         LA    R2,SCOTCIDH         REQUIRE INPUT IN TARGET COMMERCIAL           
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,(X'20',TLCOICDQ),(X'04',(R2))  TO COMML              
         BE    RECXIST             IF RECORD FOUND - GIVE ERROR                 
         MVC   TOCID,TGCID                                                      
         SPACE 1                                                                
                                                                                
         CLC   TOAGY,FROMAGY                                                    
         BNE   *+14                                                             
         CLC   TOCID,FROMCID                                                    
         BE    FLDINV                                                           
         SPACE 1                                                                
         MVC   TOCLI,FROMCLI       INITIALIZE CLI/PRD TO NO CHANGE              
         MVC   TOPRD,FROMPRD                                                    
         SPACE 1                                                                
         LA    R2,SCOTCLH          CHANGING CLIENTS?                            
         CLI   5(R2),0                                                          
*        BE    VCO60                                                            
         BNE   VCO45                                                            
*                                                                               
         TM    FAYSTA6,TAAYSREG                                                 
         BNO   VCO44                                                            
* FROM AGY/CLIENT REG STATUS ON.  MAKE SURE TO AGY/CLIENT ALSO ON               
         TM    AYAYSTA6,TAAYSREG                                                
         BO    VCO60                                                            
         BNO   ERREGON                                                          
* FROM AGY/CLIENT REG STATUS OFF.  MAKE SURE TO AGY/CLIENT ALSO OFF             
*                                                                               
VCO44    TM    AYAYSTA6,TAAYSREG                                                
         BO    ERREGOFF                                                         
         B     VCO60                                                            
                                                                                
*                                                                               
VCO45    GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SCOTCLNH                        
         MVC   TOCLI,TGCLI                                                      
                                                                                
         L     R4,AIO                                                           
         MVI   TCISTA2,0                                                        
         MVI   ELCODE,TACIELQ                                                   
         USING TACID,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   TCISTA2,TACISTA2                                                 
******** MVC   AIO,AIO1                                                         
                                                                                
         TM    FAYSTA6,TAAYSREG                                                 
         BO    *+12                                                             
         TM    FCISTA2,TACISREG                                                 
         BNO   VCO46                                                            
* FROM AGY/CLIENT REG STATUS ON.  MAKE SURE TO AGY/CLIENT ALSO ON               
         TM    AYAYSTA6,TAAYSREG                                                
         BO    VCO48                                                            
         TM    TCISTA2,TACISREG                                                 
         BNO   ERREGON                                                          
         B     VCO48                                                            
* FROM AGY/CLIENT REG STATUS OFF.  MAKE SURE TO AGY/CLIENT ALSO OFF             
*                                                                               
VCO46    TM    AYAYSTA6,TAAYSREG                                                
         BO    ERREGOFF                                                         
         TM    TCISTA2,TACISREG                                                 
         BO    ERREGOFF                                                         
                                                                                
         SPACE 1                                                                
VCO48    TM    AYBRSTAT,TABRSINT   IF AGY ON INTERFACE                          
         BO    *+12                                                             
         TM    AYAYSTA6,TAAYST10   OR TYPE 10 JOB VALIDATION                    
         BZ    VCO50                                                            
         L     R4,AIO                                                           
         USING TLCLD,R4                                                         
         OC    TLCLAGY,TLCLAGY     ENSURE THIS IS NOT GLOBAL CLIENT             
         BZ    ERRNOGLB                                                         
         DROP  R4                                                               
VCO50    BRAS  RE,GETCLG           RETURNS CLIENT GROUP                         
         MVC   TOCLG,CGROUP                                                     
         SPACE 1                                                                
VCO60    LA    R2,SCOTPRH          CHANGING PRODUCTS?                           
         TM    AYBRSTAT,TABRSINT   IF AGY ON INTERFACE                          
         BO    *+12                                                             
         TM    AYAYSTA6,TAAYST10   OR TYPE 10 JOB VALIDATION                    
         BZ    VCO70               PRODUCT AND CLIENT REQUIRED                  
         CLI   SCOTCLH+5,0         1ST CHECK FOR CLIENT                         
         BNE   VCO80                                                            
         LA    R2,SCOTCLH                                                       
         BE    MISSERR                                                          
VCO70    CLI   5(R2),0             PRODUCT IS OPTIONAL                          
         BE    VCO90               IF AGY NOT ON INTERFACE                      
VCO80    GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',(R2)),SCOTPRNH                        
         MVC   TOPRD,TGPRD                                                      
         SPACE 1                                                                
         TM    AYBRSTAT,TABRSINT   IF AGY ON INTERFACE                          
         BO    *+12                                                             
         TM    AYAYSTA6,TAAYST10   OR TYPE 10 JOB VALIDATION                    
         BZ    VCO90                                                            
         L     R4,AIO                                                           
         USING TLPRD,R4                                                         
         OC    TLPRAGY,TLPRAGY     ENSURE THIS IS NOT GLOBAL PRODUCT            
         BZ    ERRNOGLB                                                         
         DROP  R4                                                               
         SPACE 1                                                                
VCO90    CLC   TOAGY,FROMAGY       IF AGENCY CHANGED                            
         BE    VCO110                                                           
         CLI   SCOTCLH+5,0         AND CLIENT DIDN'T                            
         BNE   VCO100                                                           
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'84',TOCLI)  INSURE CLI EXISTS FOR         
         BNE   CLIERR                             AGENCY                        
         SPACE 1                                                                
VCO100   CLI   SCOTPRH+5,0         IF PRODUCT DIDN'T CHANGE                     
         BNE   VCO110                                                           
         OC    TOPRD,TOPRD         AND PRODUCT DEFINED                          
         BZ    VCO110                                                           
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'84',TOPRD)  INSURE PRD EXISTS FOR         
         BNE   PRDERR                             AGENCY                        
         SPACE 1                                                                
VCO110   LA    R2,SCOCASTH         COPY CAST ?                                  
         GOTO1 ANY                                                              
         CLI   8(R2),C'N'                                                       
         BE    VCO120                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    STATUS,COPYCAST                                                  
         SPACE 1                                                                
VCO120   LA    R2,SCOHISTH         COPY HISTORY RECORDS                         
         GOTO1 ANY                                                              
         SPACE 1                                                                
         CLI   FROMCOTY,CTYMUS     IF COPYING MUSIC COMMERCIAL                  
         BNE   VCO125                                                           
         CLI   8(R2),C'Y'          MUST COPY HISTORY                            
         BNE   FLDINV                                                           
         SPACE 1                                                                
VCO125   CLI   8(R2),C'N'                                                       
         BE    VCO130                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         TM    STATUS,COPYCAST     REQUIRES CAST COPY AS WELL                   
         BZ    CASTINV             SET COPYHIST STATUS MODE                     
         OI    STATUS,COPYHIST                                                  
         SPACE 1                                                                
VCO130   CLI   FROMWEB,C'Y'        IF COMMERCIAL IS STAMPED WITH                
         BNE   VCO140              WEB APPLICATION ID                           
         TM    STATUS,COPYHIST     AND COPYING WITH HISTORY                     
         BZ    VCO140                                                           
         CLC   FROMAGY,TOAGY       AND AGENCY IS NOT BEING CHANGED              
         BNE   VCO140                                                           
         CLC   FROMCID,TOCID       COMMERCIAL ID CANNOT BE CHANGED              
         BNE   WEBERR                                                           
         SPACE 1                                                                
VCO140   LA    R2,SCOSCSTH         SELECT CAST                                  
         GOTO1 ANY                                                              
         NI    STATUS,X'FF'-SELCAST   TURN OFF SELCALST STATUS                  
         CLI   RECNUM,PC                                                        
         BE    VCO150                                                           
         CLI   8(R2),C'N'                                                       
         BE    VCO160                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         TM    STATUS,COPYCAST     REQUIRES CAST COPY AS WELL                   
         BZ    CASTINV                                                          
         TM    STATUS,COPYHIST     BUT CANNOT BE WITH COPY HISTORY              
         BNZ   HISTINV                                                          
VCO150   OI    STATUS,SELCAST      SET SELCALST STATUS MODE                     
         SPACE 1                                                                
         CLI   SCOSCST,C'Y'        IF SELECTING CAST                            
         BNE   VCO160                                                           
         NI    SCOTTL1H+1,X'FB'    TURN HEADINGS TO HIGH INTENSITY              
         NI    SCOTTL2H+1,X'FB'                                                 
         OI    SCOTTL3H+1,X'0C'                                                 
         OI    SCOTTL1H+6,X'80'                                                 
         OI    SCOTTL2H+6,X'80'                                                 
         OI    SCOTTL3H+6,X'80'                                                 
         SPACE 1                                                                
         CLI   RECNUM,PC           IF PER CYCLE TRANSFER                        
         BNE   VCO160                                                           
         NI    SCOTTL3H+1,X'FB'    SHOW GUARANTEE HEADING                       
         SPACE 1                                                                
VCO160   LA    R2,SCOCMUSH         COPY MUSIC                                   
         GOTO1 ANY                                                              
         OI    STATUS2,CPYMUS                                                   
         CLI   8(R2),C'Y'                                                       
         BE    VCO170                                                           
         CLI   8(R2),C'N'          MUSIC MUST BE COPIED                         
         BNE   FLDINV                                                           
         TM    STATUS,COPYHIST     IF COPYING HISTORY                           
         BO    FLDINV                                                           
         NI    STATUS2,X'FF'-CPYMUS                                             
         SPACE 1                                                                
VCO170   CLI   TGCTSTTY,TASTTYPP                                                
         BE    *+12                                                             
         CLI   TGCTSTTY,TASTTYP2                                                
         BNE   XIT                                                              
         LA    R2,SCOCCOMH         COPY COMMENT RECORD                          
         GOTO1 ANY                                                              
         CLI   8(R2),C'N'                                                       
         BE    VCOX                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV                                                           
         OI    STATUS,COPYCMT                                                   
         SPACE 1                                                                
VCOX     B     XIT                                                              
         EJECT                                                                  
*              KEY VALIDATION ROUTINES FOR CLIENT/COPY                          
VKCLI    DS    0H                                                               
         OI    STATUS,CLICOPY      SET CLIENT/COPY STATUS                       
         SPACE 1                                                                
         BRAS  RE,CLRSCR2          CLEAR NAMES                                  
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SCLFAYH),SCLFAYNH  FROM AGY           
         MVC   FRCLAGY,TGAGY       OLD AGENCY                                   
         MVC   FRCLAGN,SCLFAYN         NAME                                     
*                                                                               
         L     R4,AIO                                                           
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   FAYSTA6,TAAYSTA6                                                 
                                                                                
*                                                                               
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',SCLFCLIH),SCLFCLNH  FROM CLI          
         MVC   FRCLCLI,TGCLI       OLD CLIENT                                   
         MVC   FRCLCLN,SCLFCLN         NAME                                     
*                                                                               
         L     R4,AIO                                                           
         MVI   FCISTA2,0                                                        
         MVI   ELCODE,TACIELQ                                                   
         USING TACID,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   FCISTA2,TACISTA2                                                 
*                                                                               
         SPACE 1                                                                
         LA    R2,SCLFPRDH         CHECK IF PRODUCT                             
         CLI   5(R2),0                                                          
         BE    VCL10                                                            
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',SCLFPRDH),SCLFPRNH  FROM PRD          
         MVC   FRCLPRD,TGPRD       OLD PRODUCT                                  
         MVC   FRCLPRN,SCLFPRN         NAME                                     
         SPACE 1                                                                
VCL10    GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SCLTAYH),SCLTAYNH  TO AGY             
         MVC   TOCLAGY,TGAGY       NEW AGENCY                                   
         MVC   TOCLAGN,SCLTAYN         NAME                                     
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAAYD,R4                                                         
         MVC   AYAYSTA6,TAAYSTA6   SAVE 6TH STATUS FOR AGENCY                   
         DROP  R4                                                               
*                                                                               
         SPACE 1                                                                
         LA    R2,SCLTCLIH                                                      
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',SCLTCLIH),SCLTCLNH  TO CLI            
         MVC   TOCLCLI,TGCLI       NEW CLIENT                                   
         MVC   TOCLCLN,SCLTCLN         NAME                                     
         BRAS  RE,GETCLG           RETURNS CLIENT GROUP                         
         MVC   TOCLCLG,CGROUP                                                   
*                                                                               
         L     R4,AIO                                                           
         MVI   TCISTA2,0                                                        
         MVI   ELCODE,TACIELQ                                                   
         USING TACID,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   TCISTA2,TACISTA2                                                 
*                                                                               
         TM    FAYSTA6,TAAYSREG                                                 
         BO    *+12                                                             
         TM    FCISTA2,TACISREG                                                 
         BNO   VCL16                                                            
* FROM AGY/CLIENT REG STATUS ON.  MAKE SURE TO AGY/CLIENT ALSO ON               
         TM    AYAYSTA6,TAAYSREG                                                
         BO    VCL18                                                            
         TM    TCISTA2,TACISREG                                                 
         BNO   ERREGON                                                          
         B     VCL18                                                            
* FROM AGY/CLIENT REG STATUS OFF.  MAKE SURE TO AGY/CLIENT ALSO OFF             
*                                                                               
VCL16    TM    AYAYSTA6,TAAYSREG                                                
         BO    ERREGOFF                                                         
         TM    TCISTA2,TACISREG                                                 
         BO    ERREGOFF                                                         
                                                                                
*                                                                               
*                                                                               
         SPACE 1                                                                
VCL18    LA    R2,SCLTPRDH                                                      
         OC    FRCLPRD,FRCLPRD     IF THERE IS NO OLD PRODUCT                   
         BNZ   *+16                                                             
         CLI   5(R2),0             THEN THERE CAN'T BE A NEW ONE                
         BNE   FLDINV                                                           
         B     VCL30                                                            
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',SCLTPRDH),SCLTPRNH  TO PRD            
         MVC   TOCLPRD,TGPRD       TO PRODUCT                                   
         MVC   TOCLPRN,SCLTPRN         NAME                                     
         SPACE 1                                                                
VCL30    CLC   FRCLAGY(FRCL),TOCLAGY    DO NOT ALLOW USER TO COPY               
         BNE   VCL40                    TO SAME AGY/CLI/PRD                     
         CLI   5(R2),0                                                          
         BNE   FLDINV              SET R2 TO LOWEST LEVEL OF INPUT              
         LA    R2,SCLTCLIH                                                      
         B     FLDINV                                                           
         SPACE 1                                                                
VCL40    BRAS  RE,VALOPT           VALIDATE OPTIONS FIELD                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL RENAMING COMMERCIALS ('WITH HISTORY')         
         SPACE 1                                                                
RENAME   NTR1                                                                   
         BRAS  RE,KEYINIT          SET START KEY BASED ON COPY TYPE             
         GOTO1 HIGH                                                             
         B     REN20                                                            
         SPACE 1                                                                
REN10    GOTO1 SEQ                                                              
         SPACE 1                                                                
REN20    LA    R4,KEY                                                           
         USING TLCOD,R4                                                         
         ZIC   R3,LENKEY                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   TLCOKEY(0),KEYSAVE  TEST IF STILL CORRECT AGY/CLI(/PRD)          
         BNE   RENX                                                             
         TM    TLDRSTAT-TLDRD(R4),X'40'  SKIP COPIED COMMERCIALS                
         BO    REN10                                                            
         SPACE 1                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              GET FROM COMMERCIAL                          
         GOTOR MYTRACE,DMCB,=C'FROM COMMERCIAL'                                 
         SPACE 1                                                                
         GOTO1 SAVPTRS,DMCB,ASVPBLK  SAVE PASSIVE POINTERS                      
         SPACE 1                                                                
         TM    STATUS,CLICOPY      IF THIS IS CLIENT/COPY                       
         BZ    REN30                                                            
         BAS   RE,SETFRTO          SET FROM/TO INFO FOR THIS COMML              
         BAS   RE,PRDTEST          INSURE PRODUCT OK                            
         BNE   REN20               IT'S NOT - RTN BUMPED TO NEXT PRD            
         BAS   RE,COMTEST          ENSURE "TO" COMML DOESN'T EXIST              
         BNE   REN10               IT DOES -SKIP TO NEXT COMML                  
         DROP  R4                                                               
         SPACE 1                                                                
         USING TLRCD,R4                                                         
REN30    L     R4,AIO                                                           
         OI    TLRCSTAT,X'80'      DELETE OLD COMM'L RECORD                     
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'E8',ASVPBLK),AADPBLK                             
         XC    TLRCSTAT,TLRCSTAT                                                
         DROP  R4                                                               
         SPACE 1                                                                
         L     RE,ASVPBLK                                                       
         XC    0(255,RE),0(RE)                                                  
         SPACE 1                                                                
         USING TLCOD,R4                                                         
         MVC   TLCOKEY,SVKEY                                                    
         MVC   TLCOAGY,TOAGY       SET NEW AGENCY                               
         MVC   TLCOCLI,TOCLI               CLIENT                               
         MVC   TLCOPRD,TOPRD               PRODUCT                              
         MVC   TLCOCID,TOCID               COMMERCIAL ID                        
         SPACE 1                                                                
         MVC   TGCOM,TLCOCOM       SAVE INTERNAL COMMERCIAL NUMBER              
         SPACE 1                                                                
         MVI   ELCODE,TACOELQ                                                   
         L     R4,AIO                                                           
         USING TACOD,R4            PUT NEW CID IN COMML DETAILS                 
         BAS   RE,GETEL            ELEMENT                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TACOCID,TOCID                                                    
         MVC   TACOCLG,TOCLG                                                    
         MVC   TGCTEQU,TACOTYPE                                                 
         SPACE 1                                                                
         USING TLCAPD,R2                                                        
         LA    R2,KEY              READ ALL HOLDING FEE KEYS FOR                
         XC    KEY,KEY             COMMERCIAL'S CAST                            
         MVI   TLCAPCD,TLCAHCDQ                                                 
         MVC   TLCAHCOM,TGCOM                                                   
         GOTO1 HIGH                (SKIP COMMERCIAL POINTER)                    
         B     REN45                                                            
REN40    GOTO1 SEQ                                                              
REN45    CLC   KEY(TLCAHSRT-TLCAPD),KEYSAVE                                     
         BNE   REN50                                                            
         OC    TLCAHSRT,TLCAHSRT                                                
         BZ    REN40                                                            
         OC    TLCAHNXT,TLCAHNXT   IF CAST HAS RECEIVED HOLDING                 
         BZ    REN40               FEE                                          
         CLC   TLCAHDTE,TLCAHNXT   THAT HAS NOT BEEN PAID                       
         BH    REN40                                                            
         OI    TACOSTA2,TACOCHHF   TURN ON HF-REISSUE REQUIRED STATUS           
         GOTOR SNDMQHFR,DMCB,(PRGSTAT,TGCOM),(TGSYSTA2,HEXOUT),MQIO             
         DROP  R2                                                               
         SPACE 1                                                                
REN50    MVC   AIO,AIO2            PREPARE TO READ CAST INTO AIO2               
         SPACE 1                                                                
         USING TLCAD,R2                                                         
         XC    KEY,KEY             READ ALL CAST FOR COMMERCIAL                 
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         B     REN70                                                            
REN60    GOTO1 SEQ                                                              
REN70    CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BNE   REN80                                                            
         DROP  R2                                                               
         SPACE                                                                  
         MVC   SVCAKEY,KEY         SAVE CAST KEY                                
         GOTO1 GETREC              AND READ CAST RECORD                         
         SPACE 1                                                                
         BRAS  RE,UPDGUAR          UPDATE GUARANTEE RECORD                      
         SPACE 1                                                                
         MVC   KEY,SVCAKEY         AND CAST READ SEQUENCE                       
         GOTO1 HIGH                                                             
         B     REN60                                                            
         SPACE 1                                                                
REN80    MVC   AIO,AIO1            RESTORE I/O AREA                             
         SPACE 1                                                                
         CLC   TOAGY,FROMAGY       IF AGENCY IS CHANGING                        
         BE    REN90                                                            
         XC    TACOATT,TACOATT     CLEAR ATTENTION CODE                         
         SPACE 1                                                                
         TM    STATUS2,DELPMREF    IF DELETING PMUSIC REFERENCES                
         BZ    REN90                                                            
         MVI   ELCODE,TACPELQ      DELETE ANY PUBLISHED MUSIC ELEMENTS          
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
REN90    BAS   RE,ADDTAOC          ADD OLD COMMERCIAL ELEMENT                   
         SPACE 1                                                                
         MVI   ELCODE,TAVRELQ      CHANGE A VERSION ELEMENT                     
         L     R4,AIO              TO REFLECT NEW CID                           
         USING TAVRD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   REN100                                                           
         MVC   TAVRCID,TOCID                                                    
         SPACE 1                                                                
         USING TAMCD,R4                                                         
REN100   CLI   TGCTEQU,CTYMUS      IF COMMERCIAL TYPE IS MUSIC                  
         BNE   REN120                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAMCELQ      READ ALL MUSIC CONTRACT DETAILS              
         BAS   RE,GETEL            ELEMENTS                                     
         B     *+8                                                              
REN110   BAS   RE,NEXTEL                                                        
         BNE   REN120                                                           
         CLC   TAMCCON,TOCID       IF CONTRACT NUMBER MATCHES NEW               
         BNE   REN110              ID                                           
         XC    TAMCCON,TAMCCON     CLEAR CONTRACT NUMBER                        
         OI    TAMCSTAT,TAMCSNEW   AND SET CONFORMS TO NEW MUSIC RULES          
         B     REN110                                                           
         DROP  R4                                                               
         SPACE 1                                                                
REN120   GOTO1 ACTVIN,DMCB,0       UPDATE ACTIVITY ELEMENT                      
         SPACE 1                                                                
         L     R3,AIO                                                           
         BAS   RE,MYADDREC         ADD NEW COMMERCIAL RECORD                    
         GOTOR MYTRACE,DMCB,=C'PUTREC - TO COMML OVER FROM COMML'               
*                             (ADDPTRS WILL DELETE THIS AND ADD NEW)            
         SPACE 1                                                                
         MVI   BYTE,X'28'                                                       
         TM    STATUS,TRACING                                                   
         BZ    *+8                                                              
         OI    BYTE,X'10'                                                       
         L     RE,ASVPBLK                                                       
         XC    0(255,RE),0(RE)                                                  
         GOTO1 ADDPTRS,DMCB,(BYTE,ASVPBLK),AADPBLK    HANDLE ALL PTRS           
         SPACE 1                                                                
         BRAS  RE,CPYCOMS          COPY ADDITIONAL COMMERCIAL RECORDS           
         SPACE 1                                                                
         BAS   RE,CPYCMT           COPY COMMENT RECORD                          
         SPACE 1                                                                
         BRAS  RE,CPYVER           COPY VERSION RECORDS                         
         SPACE 1                                                                
         TM    STATUS,CLICOPY      IF THIS IS CLIENT COPY                       
         BZ    RENX                                                             
         BAS   RE,REPORT           PRINT REPORT FOR THIS COMMERCIAL             
         SPACE 1                                                                
         LH    R1,COMLCTR                                                       
         LA    R1,1(R1)            BUMP COMMERCIAL COUNTER                      
         STH   R1,COMLCTR                                                       
         SPACE 1                                                                
         MVC   KEY,SVKEY           RESTORE READ SEQ                             
         OI    DMINBTS,X'08'       NEED TO READ DELETED NOW                     
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         B     REN10               LOOK FOR ANOTHER COMML TO COPY               
         SPACE 1                                                                
RENX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS FROM/TO COMMERCIAL INFO FOR CLIENT/COPY             
         SPACE 1                                                                
SETFRTO  NTR1                                                                   
         L     R4,AIO              R4=A(FROM COMMERCIAL RECORD)                 
         USING TLCOD,R4                                                         
         MVC   SVKEY,TLCOKEY       SAVE KEY (WITH D/A)                          
         SPACE 1                                                                
         MVC   FROMAGY,TLCOAGY     SET AGENCY                                   
         MVC   FROMCLI,TLCOCLI         CLIENT                                   
         MVC   FROMPRD,TLCOPRD         PRODUCT                                  
         MVC   FROMCOM,TLCOCOM         INTERNAL COMMERCIAL NUMBER               
         SPACE 1                                                                
         L     R4,AIO                                                           
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      GET COMML ID                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FROMCID,TACOCID                                                  
         MVC   FROMCLG,TACOCLG                                                  
         MVC   FROMCOTY,TACOTYPE                                                
         DROP  R4                                                               
         SPACE 1                                                                
         MVI   FROMWEB,C'N'                                                     
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   *+8                                                              
         MVI   FROMWEB,C'Y'                                                     
         SPACE 1                                                                
         MVC   TOAGY,TOCLAGY       SET 'TO' AGENCY                              
         SPACE 1                                                                
         MVC   TOCLI,FROMCLI       SET 'TO'='FROM'                              
         OC    TOCLCLI,TOCLCLI     IF WE HAVE NEW CLIENT                        
         BZ    *+10                                                             
         MVC   TOCLI,TOCLCLI       THEN SET IT                                  
         SPACE 1                                                                
         MVC   TOPRD,FROMPRD       SET 'TO'='FROM'                              
         MVI   RCSUBPRG,1                                                       
         OC    TOCLPRD,TOCLPRD     IF WE HAVE NEW PRODUCT                       
         BZ    *+14                                                             
         MVC   TOPRD,TOCLPRD       THEN SET IT                                  
         MVI   RCSUBPRG,0                                                       
         SPACE 1                                                                
         MVC   TOCLG,FROMCLG       SET 'TO' ='FROM'                             
         TM    STATUS,CLICOPY      BUT IF CLIENT COPY                           
         BZ    *+10                                                             
         MVC   TOCLG,TOCLCLG       THEN SET CLIENT COPY TO                      
         SPACE 1                                                                
         MVC   TOCID,FROMCID       SET 'TO' COMML ID                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE INSURES PRODUCTS ARE OK                                  
         SPACE 1                                                                
         USING TLCOD,R4            R4=A(NEW COMMERCIAL ACTIVE KEY)              
PRDTEST  NTR1                                                                   
*******  CLC   TOAGY,FROMAGY       DON'T BOTHER IF AGY ISN'T CHANGING           
*******  BE    PRDX                                                             
         OC    TOCLPRD,TOCLPRD     OR IF PRODUCT-SPECIFIC REQUEST               
         BNZ   PRDX                                                             
         CLC   TLCOKEY(TLCOCID-TLCOD),KEYSAVE  OR IF PRD DIDN'T CHANGE          
         BE    PRDX                                                             
         OC    TOPRD,TOPRD         OR IF THERE IS NO PRODUCT                    
         BZ    PRDX                                                             
         SPACE 1                                                                
         MVC   TGAGY,TOAGY         ELSE INSURE PRD EXISTS FOR NEW AGY/          
         MVC   TGCLI,TOCLI                                        CLI           
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'80',TOPRD)  VALIDATE PRODUCT              
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY           RESTORE COMMERCIAL KEY                       
         BE    PRDX                                                             
         SPACE 1                                                                
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVC   LFPRD,TOPRD         DISPLAY PRODUCT CODE                         
         MVC   LFPRDN(36),=C'** NOT FOUND ON NEW AGY - SKIPPED **'              
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         LA    R4,KEY              SET TO BUMP TO NEXT PRODUCT                  
         ZIC   R1,TLCOPRD+L'TLCOPRD-1                                           
         LA    R1,1(R1)                                                         
         STC   R1,TLCOPRD+L'TLCOPRD-1                                           
         XC    TLCOCID,TLCOCID     CLEAR LOWER PORTION OF KEY                   
         XC    TLCOCOM,TLCOCOM                                                  
         GOTO1 HIGH                                                             
         B     NO                  RETURN CC NE                                 
         SPACE 1                                                                
PRDX     B     YES                 OK - RETURN CC EQ                            
         EJECT                                                                  
*              ROUTINE ENSURES "TO" COMMERCIAL DOESN'T EXIST                    
*                                  XIT - CC EQ COMML DOESN'T EXIST              
         SPACE 1                                                                
COMTEST  NTR1                                                                   
         CLC   TOAGY,FROMAGY       DON'T BOTHER IF AGY ISN'T CHANGING           
         BE    YES                                                              
         MVC   TGAGY,TOAGY                                                      
         MVC   TGCID,TOCID                                                      
         GOTO1 RECVAL,DMCB,(X'20',TLCOICDQ),(X'84',0)                           
         MVC   KEY,SVKEY           RESET KEY                                    
         BNE   YES                 COMMERCIAL DOESN'T EXIST                     
*                                                                               
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         OC    TOCLPRD,TOCLPRD     IF PROCESSING ONLY ONE PRODUCT               
         BZ    *+14                                                             
         MVC   L2RHS+1(30),=CL30'<-- FOUND ON NEW AGY - SKIPPED'                
         B     *+10                                                             
         MVC   LRHS+1(30),=CL30'<-- FOUND ON NEW AGY - SKIPPED'                 
*                                                                               
         BAS   RE,REPORT                                                        
         GOTO1 HIGH                RESET READ SEQUENCE                          
         B     NO                  COMMERCIAL EXISTS                            
         EJECT                                                                  
*              ROUTINE ADDS A NEW TAOCD ELEMENT TO COMMERCIAL REC               
         SPACE 1                                                                
ADDTAOC  NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT          R4=A(OLD COMMERCIAL ELEMENT)                 
         USING TAOCD,R4                                                         
         SPACE 1                                                                
         MVI   TAOCEL,TAOCELQ      ELEMENT CODE                                 
         MVI   TAOCLEN,TAOCLNQ     ELEMENT LENGTH                               
         MVC   TAOCDTE,TGTODAY1    TODAY'S DATE                                 
         XC    TAOCDTE,=X'FFFFFF'  (COMPLEMENTED)                               
         MVC   TAOCAGY,FROMAGY     FROM AGENCY                                  
         MVC   TAOCCID,FROMCID     FROM CID                                     
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT DETAILS FOR CLIENT/COPY REPORT                  
         SPACE 1                                                                
REPORT   NTR1                                                                   
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         SPACE 1                                                                
         OC    TOCLPRD,TOCLPRD     IF PROCESSING ONLY ONE PRODUCT               
         BZ    *+14                                                             
         MVC   L2FCID,FROMCID      DISPLAY ONLY OLD COMMERCIAL ID               
         B     REPX                                                             
         SPACE 1                                                                
         MVC   LFCID,FROMCID       ELSE DISPLAY OLD COMMERCIAL ID               
         SPACE 1                                                                
         CLC   FROMPRD,LASTPRD     AND IF PRODUCT CHANGED                       
         BE    REPX                                                             
         MVC   LASTPRD,FROMPRD                                                  
         MVC   LFPRD,FROMPRD       OLD PRODUCT CODE                             
         MVI   PRDNHEDH,36+8                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'88',FROMPRD),PRDNHEDH                     
         MVC   AIO,AIO1            RESET IOAREA                                 
         L     RE,AIO              RESET KEY                                    
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLDRKEY),0(RE)                                             
         SPACE 1                                                                
         MVC   LFPRDN,PRDNHEAD     AND OLD PRODUCT NAME                         
         SPACE 1                                                                
REPX     BAS   RE,PRNTIT           PRINT THE LINE                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE UPDATES RELEVANT ESTIMATE RECORDS (CLI/COPY)             
         SPACE 1                                                                
ESTUPD   NTR1                                                                   
         CLC   FRCLAGY,TOCLAGY     IF AGENCY CHANGED, DON'T BOTHER              
         BNE   ESTUX                                                            
         MVI   FORCEHED,C'Y'       SET TO START NEW PAGE                        
         MVI   RCSUBPRG,2                                                       
         OC    TOCLPRD,TOCLPRD                                                  
         BNZ   *+8                                                              
         MVI   RCSUBPRG,3                                                       
         SPACE 1                                                                
         LH    RF,=Y(TIEND-TASYSIOD)                                            
         XCEFL TASYSIOD                                                         
         LA    R2,EIOHOOK          SET A(I/O HOOK) FOR SYSIO                    
         ST    R2,TIHOOK                                                        
         MVC   TIUSERID,TWAORIG                                                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVC   TIACOMFC,ACOMFACS                                                
         SPACE 1                                                                
         MVI   TIREAD,TLESCDQ      SET TO READ ESTIMATE RECORDS                 
         MVC   TIFAGY,FRCLAGY      SET TO FILTER ON AGENCY                      
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO READ RECORDS              
         SPACE 1                                                                
         OC    ESTCTR,ESTCTR       IF SOMETHING WAS PRINTED                     
         BZ    ESTUX                                                            
         BAS   RE,BXBOT            END BOX                                      
         EDIT  ESTCTR,(6,P+1),COMMAS=YES,ALIGN=LEFT   DISP COPY COUNT           
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(17,R1),=C'ESTIMATES CHANGED'                                   
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
ESTUX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES ESTIMATE RECORDS FROM SYSIO                      
         SPACE 1                                                                
EIOHOOK  NTR1                                                                   
         CLI   TIMODE,PROCREC      TEST WE HAVE A RECORD                        
         BNE   EIOHX                                                            
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAESELQ      SCAN THROUGH ESTIMATE ELEMENTS               
         BAS   RE,GETEL                                                         
         B     *+8                                                              
EIOH10   BAS   RE,NEXTEL                                                        
         BNE   EIOH90                                                           
         USING TAESD,R4                                                         
         CLI   TAESTYPE,TAESTCLI   IF THIS IS CLIENT TYPE                       
         BNE   EIOH20                                                           
         XC    ATHSCLI,ATHSCLI                                                  
         CLC   FRCLCLI,TOCLCLI     AND CLIENT CHANGED                           
         BE    EIOH10                                                           
         CLC   TAESCLI,FRCLCLI     AND IF CLI ON ESTIMATE IS OLD CLI            
         BNE   EIOH10                                                           
         OC    FRCLPRD,FRCLPRD     THEN IF RUNNING FOR ONE PRODUCT              
         BZ    *+12                                                             
         ST    R4,ATHSCLI          SAVE A(CLIENT EL.)                           
         B     EIOH10              AND DON'T UPDATE YET                         
         MVC   TAESCLI,TOCLCLI     ELSE CHANGE TO NEW CLIENT CODE               
         MVI   TIMODE,PROCPTRC     TELL SYSIO TO WRITE BACK RECORD              
         B     EIOH10              KEEP ON LOOKING                              
         SPACE 1                                                                
EIOH20   CLI   TAESTYPE,TAESTPRD   IF THIS IS PRODUCT TYPE                      
         BNE   EIOH10                                                           
         OC    FRCLPRD,FRCLPRD     AND RUNNING FOR ONE PRODUCT                  
         BZ    EIOH25                                                           
         CLC   FRCLPRD,TOCLPRD     AND THE ONE PRODUCT WAS CHANGED              
         BE    EIOH10                                                           
         CLC   TAESPRD,FRCLPRD     AND IF PRD ON ESTIMATE IS OLD PRD            
         BNE   EIOH10                                                           
         CLC   FRCLCLI,TOCLCLI     THEN IF CLIENT CHANGED                       
         BE    EIOH30                                                           
         ICM   R1,15,ATHSCLI       AND IF WE HAVE A(CLIENT EL.)                 
         BZ    EIOH10                                                           
         MVC   TAESCLI-TAESD(6,R1),TOCLCLI  GO BACK & CHANGE TO NEW CLI         
         B     EIOH30                                                           
         SPACE 1                                                                
         USING TLPRD,R2                                                         
EIOH25   MVI   BYTE,0                                                           
         SPACE 1                                                                
         LA    R2,KEY              IF RUNNING FOR > 1 PRODUCT                   
         XC    TLPRKEY,TLPRKEY                                                  
         MVI   TLPRCD,TLPRCDQ      MAKE SURE PRODUCT EXISTS                     
         MVC   TLPRAGY,TOAGY       UNDER NEW AGENCY                             
         MVC   TLPRCLI,TOCLI       AND CLIENT                                   
         MVC   TLPRPRD,TAESPRD                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLPRPRD+L'TLPRPRD-TLPRD),KEYSAVE                             
         BNE   *+8                                                              
         MVI   BYTE,C'Y'                                                        
         DROP  R2                                                               
         SPACE 1                                                                
         MVC   KEY,TIKEY           RESET KEY AND RECORD                         
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         CLI   BYTE,C'Y'           IF PRD DOES EXIST, OK TO                     
         BE    EIOH10              CHANGE ESTIMATE                              
         SPACE 1                                                                
         MVI   TIMODE,0            IF IT DOESN'T, DO NOT CHANGE                 
         L     R1,TIAREC           THIS ESTIMATE                                
         USING TLESD,R1                                                         
         LA    R2,P                                                             
         USING LINED,R2                                                         
         MVC   L3EST,TLESEST                                                    
         MVC   L3ESTN(36),=C'**PRD NOT FND ON NEW AGY - SKIPPED**'              
         BAS   RE,PRNTIT                                                        
         DROP  R1                                                               
         SPACE 1                                                                
         ST    R4,AIO                                                           
         GOTOR MYTRACE,DMCB,=C'ESTIMATE'                                        
         MVC   AIO,AIO1                                                         
         B     EIOH90                                                           
         SPACE 1                                                                
EIOH30   MVC   TAESPRD,TOCLPRD     AND CHANGE TO NEW PRODUCT                    
         MVI   TIMODE,PROCPTRC     TELL SYSIO TO WRITE BACK RECORD              
         B     EIOH10              KEEP ON LOOKING                              
         SPACE 1                                                                
EIOH90   CLI   TIMODE,PROCPTRC     IF WILL WRITE BACK RECORD                    
         BNE   EIOHX                                                            
         LH    R1,ESTCTR                                                        
         LA    R1,1(R1)            BUMP COUNTER                                 
         STH   R1,ESTCTR                                                        
         SPACE 1                                                                
         L     R4,TIAREC           R4=A(RECORD)                                 
         USING TLESD,R4                                                         
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVC   L3EST,TLESEST       PRINT ESTIMATE CODE                          
         MVC   L3ESTN,TINAME                  AND NAME                          
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         ST    R4,AIO              MAY NEED TO TRACE IT                         
         GOTOR MYTRACE,DMCB,=C'ESTIMATE'                                        
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         CLI   TWAWRITE,C'N'       IF WRITE=NO SET                              
         BNE   *+8                                                              
         MVI   TIMODE,PROCREC      RESTORE ORIGINAL MODE                        
         SPACE 1                                                                
EIOHX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL COPYING A COMMERCIAL (W/O HISTORY)            
         SPACE 1                                                                
COPY     NTR1                                                                   
         BRAS  RE,GETCOM           GET NEW INTERNAL COMMERCIAL NUMBER           
         SPACE 1                                                                
         TM    STATUS,COPYCAST     IF CAST COPY REQUESTED                       
         BZ    *+8                                                              
         BRAS  RE,CCAST            AND DO SO NOW                                
         SPACE 1                                                                
         MVC   KEY,SVKEY           SET SAVED COMMERCIAL KEY                     
         GOTO1 HIGH                                                             
         GOTO1 GETREC              GET FROM COMMERCIAL                          
         SPACE 1                                                                
         XC    KEY,KEY             BUILD KEY OF NEW COMMERCIAL                  
         LA    R4,KEY                                                           
         USING TLCOD,R4                                                         
         MVI   TLCOCD,TLCOCDQ                                                   
         MVC   TLCOAGY,TOAGY       AGENCY                                       
         MVC   TLCOCLI,TOCLI       CLIENT                                       
         MVC   TLCOPRD,TOPRD       PRODUCT                                      
         MVC   TLCOCID,TOCID       COMMERCIAL ID                                
         MVC   TLCOCOM,TOCOM       INTERNAL COMMERCIAL NUMBER                   
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVC   TLCOKEY,KEY         SET NEW ACTIVE KEY IN RECORD                 
         XC    TLCOSTAT,TLCOSTAT   CLEAR STATUS BYTES                           
         SPACE 1                                                                
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS EL.                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   TACOCID,TOCID       SET NEW COMMERCIAL ID HERE                   
         MVC   TACOCLG,TOCLG       SET NEW CLIENT GROUP HERE                    
         SPACE 1                                                                
         XC    TACOPDTE,TACOPDTE   CLEAR LAST PAY DATE                          
         SPACE 1                                                                
         CLI   RECNUM,PC           IF RUNNING PER CYCLE TRANSFER                
         BNE   CPY10                                                            
         OC    TACOVDTE,TACOVDTE   AND VERIFICATION INFO IS PRESENT             
         BZ    CPY20                                                            
         MVI   TACOUVST,0          THIS TRANSACTION SERVES AS                   
         MVC   TACOVDTE,TGTODAY1   VERIFICATION                                 
         MVC   TACOVSTU,TWAORIG                                                 
         MVC   TACOVST,TGCTSTAF                                                 
         OI    TACOUVST,TACOUVER                                                
                                                                                
         TIME  DEC                                                              
         STCM  R0,14,TACOVTIM      CURRENT TIME                                 
         B     CPY20                                                            
         SPACE 1                                                                
CPY10    XC    TACOVDTE,TACOVDTE   CLEAR VERIFICATION INFO                      
         XC    TACOVTIM,TACOVTIM                                                
         XC    TACOVSTU,TACOVSTU                                                
         XC    TACOVST,TACOVST                                                  
         MVI   TACOUVST,0                                                       
         SPACE 1                                                                
CPY20    NI    TACOSTA2,X'FF'-TACOSJPC   CLEAR CSF STATUS                       
         SPACE 1                                                                
         TM    STATUS,COPYCAST     IF NOT COPYING CAST                          
         BNZ   CPY30                   CLEAR LIFT & ANN ONLY BITS               
         NI    TACOSTA2,ALL-(TACOSLFT+TACOSANO)                                 
         SPACE 1                                                                
CPY30    CLI   RECNUM,PC           IF PERCYCLE/TRANSFER                         
         BNE   CPY40                                                            
         OI    TACOSTA2,TACOPCYC   SET PER CYCLE COMMERCIAL                     
         MVC   TACOFCYC,EPCYSTRT   AND EARLIEST FIRST FIXED CYCLE DATE          
         SPACE 1                                                                
CPY40    CLC   TOAGY,FROMAGY       IF AGENCY IS CHANGING                        
         BE    CPY50                                                            
         XC    TACOATT,TACOATT     CLEAR ATTENTION CODE                         
         SPACE 1                                                                
         TM    STATUS2,DELPMREF    IF DELETING PMUSIC REFERENCES                
         BZ    CPY50                                                            
         MVI   ELCODE,TACPELQ      DELETE ANY PUBLISHED MUSIC ELEMENTS          
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
CPY50    MVI   ELCODE,TACVELQ      DELETE ANY CONVERSION ELEMENTS               
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         MVI   ELCODE,TAOCELQ      DELETE ANY OLD COMML ELEMENTS                
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   CPY60                                                            
         L     R4,TGELEM           REMOVE WEB APPLICATION ID ELEMENT            
         MVI   0(R4),X'FF'                                                      
         SPACE 1                                                                
CPY60    GOTO1 GETL,DMCB,(1,=AL1(TAFNTOWB))                                     
         BNE   CPY70                                                            
         L     R4,TGELEM           REMOVE ORIGINAL WEB APPLICATION              
         MVI   0(R4),X'FF'         ID ELEMENT                                   
         SPACE 1                                                                
CPY70    MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         USING TAVRD,R4                                                         
         L     R4,AIO              READ ALL VERSION ELEMENTS                    
         MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CPY80    BAS   RE,NEXTEL                                                        
         BNE   CPY100                                                           
         SPACE 1                                                                
         CLI   TAVRVERS,1          DO NOT DELETE VERSION 1 ELEMENT              
         BNE   CPY90                                                            
         MVC   TAVRCID,TOCID       AND INSERT "TO" COMMERCIAL ID                
         B     CPY80               INTO IT                                      
         SPACE 1                                                                
CPY90    MVI   TAVREL,X'FF'        SET TO DELETE ALL OTHER VERSIONS             
         B     CPY80                                                            
         DROP  R4                                                               
         SPACE 1                                                                
CPY100   MVI   ELCODE,X'FF'        DELETE ANY VERSIONS ELEMENTS                 
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         TM    STATUS2,CPYMUS      IF NOT COPYING MUSIC                         
         JO    CPY110                                                           
         MVI   ELCODE,TATRELQ      REMOVE ALL MUSIC ELEMENTS                    
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAMCELQ                                                   
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
CPY110   TM    STATUS,COPYCAST     IF COPYING CAST                              
         BZ    CPY120                                                           
         BRAS  RE,SETNXTC          SET NEXT CAST SEQUENCE NUMBER                
         SPACE 1                                                                
CPY120   GOTO1 ACTVIN,DMCB,0       ADD NEW ACTIVITY ELEMENT                     
         SPACE 1                                                                
         BAS   RE,MYADDREC         ADD THE RECORD TO THE FILE                   
         MVC   COMDA,DMDSKADD      SAVE D/A OF NEW COMMECIAL                    
         SPACE 1                                                                
         L     RE,ASVPBLK                                                       
         XC    0(256,RE),0(RE)                                                  
         GOTO1 ADDPTRS,DMCB,(X'08',ASVPBLK),AADPBLK    ADD PASSIVE PTRS         
         SPACE 1                                                                
         BAS   RE,CPYCMT           COPY COMMENT RECORD                          
         BRAS  RE,ADHSTCMT         ADD HISTORY COMMENT                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS CAST RECORDS FROM SYSIO                       
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   IOHX                                                             
         SPACE 1                                                                
         BRAS  RE,FILTER           FILTER THIS CAST RECORD                      
         BNE   IOH20                                                            
         SPACE 1                                                                
         TM    STATUS,SELCAST      IF WE ARE SELECTING FROM THE CAST            
         BNO   IOH10                                                            
         BAS   RE,LISTCAST                                                      
         B     IOH20                                                            
         SPACE 1                                                                
IOH10    L     R3,TIAREC           R3=A(CAST RECORD)                            
         ST    R3,AIO                                                           
         USING TLCAD,R3                                                         
         BAS   RE,NEWCAST          ADD A NEW CAST RECORD TO THE FILE            
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
IOH20    TM    STATUS,REREAD       TEST WE NEED TO RE-READ SYSIO'S KEY          
         BZ    IOHX                                                             
         XI    STATUS,REREAD                                                    
         MVC   KEY,TIKEY           SET SYSIO'S KEY                              
         GOTO1 HIGH                                                             
         SPACE 1                                                                
IOHX     B     XIT                                                              
         EJECT                                                                  
*        LIST CAST RECORD                                                       
*                                                                               
LISTCAST NTR1                                                                   
         MVC   CONTKEY,TIKEY       SET CONTINUATION KEY                         
         CLI   COUNTER,9                                                        
         BNE   LC10                IF END OF PAGE                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'90'           FORCE END TO SYSIO READ                      
         GOTO1 HIGH                                                             
         MVI   COUNTER,0           AND START OVER                               
         B     LCX                                                              
*                                                                               
LC10     L     R2,APLINE           A(HEADER OF NEXT LINE TO PRINT ON)           
         LA    R2,8(R2)            PAST HEADER                                  
         USING LLINED,R2                                                        
         L     R3,TIAREC                                                        
         USING TLCAD,R3                                                         
         SPACE                                                                  
         BRAS  RE,PCELIG           IF REC/ACT PERCYCLE/TRANSFER,                
         BNE   LCX                 CHECK IF PERFORMER IS ELIGIBLE               
         BRAS  RE,SETFFC           AND SAVE COMML'S FIRST FIXED CYCLE           
         SPACE                                                                  
         MVC   CASSN,TLCASSN       SSN                                          
         MVC   CACAT,TLCACAT       CATEGORY                                     
         MVC   CACAM,TIONOF        ON/OFF CAMERA                                
         MVC   CATAX,TIUNIT        TAX UNIT                                     
         MVC   CAUNI,TIUN          UNION                                        
         MVC   CALCL,TILOCL        LOCAL                                        
         MVC   CAYR,TIYEAR         CONTRACT YEAR                                
         SPACE                                                                  
         L     R4,TIAREC                                                        
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LC15                                                             
         GOTO1 TRNSAGT,DMCB,(X'40',TACANCDE),CAAGNT                             
         MVC   CACORP,TACACORP     CORP CODE                                    
         TM    TACASTAT,TACASTLF                                                
         BNO   LC12                                                             
         MVI   CALIFT,C'Y'         ON LIFT                                      
         SPACE 1                                                                
LC12     TM    TACASTAT,TACASTLO                                                
         BNO   LC15                                                             
         MVI   CALIFT,C'O'         ONLY ON LIFT                                 
         SPACE 1                                                                
LC15     ZIC   R1,COUNTER                                                       
         LA    R4,DATABLE          A(START OF D/A TABLE)                        
         LTR   R1,R1                                                            
         BZ    LC30                                                             
         SPACE 1                                                                
LC20     LA    R4,4(R4)            BUMP TO CORRECT ENTRY IN TABLE               
         BCT   R1,LC20                                                          
         SPACE 1                                                                
LC30     MVC   0(4,R4),TIDSKADD    SET D/A                                      
         MVI   MYNAMEH,24          SET FIELD LENGTH                             
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',TLCASSN),MYNAMEH                      
         MVC   CALNAME(16),MYNAME                                               
         OI    STATUS,REREAD       SET WE NEED TO RE-READ SYSIO'S KEY           
         SPACE 1                                                                
         L     R2,APLINE           A(HEADER OF NEXT LINE TO PRINT ON)           
         OI    6(R2),X'80'         TRASNMIT                                     
         ZIC   R1,0(R2)            BUMP PAST LINE FIELD                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            BUMP PAST SEL FIELD                          
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            BUMP PAST AUTO SKIP FIELD                    
         AR    R2,R1                                                            
         ST    R2,APLINE           NEXT LINE TO PRINT ON                        
         SPACE 1                                                                
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
LCX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SELECT CAST MEMBERS                                   
         SPACE 1                                                                
SELECTC  NTR1                                                                   
         MVI   LISTED,C'N'                                                      
         MVI   LSTPAGE,C'N'                                                     
         LA    R2,SCOSELH          START AT FIRST SEL FIELD                     
         LA    R4,DATABLE          D/A TABLE                                    
         LA    R5,SCOLSTH                                                       
         LA    R3,SELTAB                                                        
         LA    R1,STABLNQ(R3)      END OF TABLE                                 
         SPACE 1                                                                
SC10     OC    0(4,R3),0(R3)       FIND LAST ENTRY IN TABLE                     
         BZ    SC20                                                             
         LA    R3,4(R3)                                                         
         CR    R3,R1                                                            
         BNH   SC10                                                             
         DC    H'0'                TABLE TOO SMALL                              
         SPACE 1                                                                
SC20     CR    R2,R5               IF END OF SCREEN - EXIT                      
         BNL   SCX                                                              
         CLI   SCOSCST,C'N'                                                     
         BE    SC25                                                             
         CLI   5(R2),0             ACCEPT ANY INPUT                             
         BE    SC30                                                             
SC25     MVC   8(3,R2),SPACES      CLEAR FIELD FROM INPUT                       
         OI    6(R2),X'80'                                                      
         MVC   0(4,R3),0(R4)       SAVE SELECTED D/A                            
         LA    R3,4(R3)            BUMP SELECTED D/A TABLE                      
         ZIC   R1,NSEL             INCREMENT NUMBER SELECTED                    
         LA    R1,1(R1)                                                         
         STC   R1,NSEL                                                          
         SPACE 1                                                                
SC30     LA    R4,4(R4)            BUMP D/A TABLE                               
         ZIC   R1,0(R2)            BUMP PAST SEL,DISPLAY & AUTO SKIP            
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     SC20                                                             
         SPACE 1                                                                
SCX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GOES THROUGH SELECTED RECS TO ADD THEM TO FILE           
         SPACE 1                                                                
ADDSEL   NTR1                                                                   
         BAS   RE,COPY             ADD NEW COMMERCIAL                           
         SPACE 1                                                                
         LA    R4,SELTAB           SELECTED D/A'S                               
         LA    R2,STABLNQ(R4)      END OF TABLE                                 
         SPACE 1                                                                
AS10     OC    0(4,R4),0(R4)       ANY D/A'S LEFT                               
         BZ    ASX                                                              
         CR    R4,R2               REACHED END OF TABLE                         
         BH    ASX                                                              
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         XC    KEY,KEY                                                          
         MVC   TLDRDA,0(R4)        SET D/A                                      
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              & GET SELECTED CAST RECORD                   
         SPACE 1                                                                
         L     R3,AIO                                                           
         BAS   RE,NEWCAST          ADD A NEW CAST RECORD TO THE FILE            
         LA    R4,4(R4)            BUMP TO NEXT D/A                             
         B     AS10                                                             
         SPACE 1                                                                
ASX      LA    RE,SELTAB           CLEAR STORAGE                                
         LH    RF,=AL2(STABLNQ)                                                 
         XCEFL                                                                  
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'B4',TOCOM)                               
         BE    *+6                                                              
         DC    H'00'                                                            
         BRAS  RE,SETNXTC          UPDATE COMMERCIAL'S NEXT CAST                
         GOTO1 PUTREC              SEQUENCE NUMBER                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ADDS A NEW CAST RECORD TO THE FILE                       
         SPACE 1                                                                
         USING TLCAD,R3            R3=A(CAST RECORD)                            
NEWCAST  NTR1                                                                   
         BRAS  RE,TRNOVA           TRANSFER OV AMTS FOR PERCYC/TRAN             
         SPACE 1                                                                
         MVC   TLCACOM,TOCOM       SET NEW INTERNAL COMMERCIAL NUMBER           
         SPACE 1                                                                
         MVC   TLCASEQ,CASTSEQ     SET NEW INPUT SEQUENCE NUMBER                
         SPACE 1                                                                
         ZICM  RE,CASTSEQ,2                                                     
         AHI   RE,1                BUMP INPUT SEQUENCE NUMBER                   
         STCM  RE,3,CASTSEQ                                                     
         SPACE 1                                                                
         MVI   ELCODE,TAHFELQ      DELETE HOLDING FEE NOTICE EL.                
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAHFD,R4                                                         
         MVI   TAHFEL,TAHFELQ      BUILD AND                                    
         MVI   TAHFLEN,TAHFLNQ                                                  
         GOTO1 ADDELEM             ADD NEW ONE                                  
         SPACE 1                                                                
         MVI   ELCODE,TACRELQ      DELETE APPLIED CREDIT HISTORY ELS.           
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTVER))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTWEB))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTOWB))                                     
         SPACE 1                                                                
         MVI   ELCODE,TACEELQ      DELETE CAST ERRORS ELEMENTS                  
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         BRAS  RE,SETFFC           SET FFC FOR PERCYCLE/TRANSFER                
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0       ADD NEW ACTIVITY ELEMENT                     
         SPACE 1                                                                
         BAS   RE,TESTANN          TEST IF ANNOUNCER                            
         BAS   RE,TESTLIFT         TEST IF PERFORMER ON LIFT                    
         SPACE 1                                                                
         BAS   RE,MYADDREC         ADD THE RECORD TO THE FILE                   
         SPACE 1                                                                
         L     RE,ASVPBLK                                                       
         XC    0(256,RE),0(RE)                                                  
         GOTO1 ADDPTRS,DMCB,(X'08',ASVPBLK),AADPBLK    ADD PASSIVE PTRS         
         SPACE 1                                                                
         BRAS  RE,UPDGUAR          UPDATE GUARANTEE RECORD FOR PC/TRAN          
         SPACE 1                                                                
         OI    STATUS,REREAD       SET WE NEED TO RE-READ SYSIO'S KEY           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS ADDING NEW CAST RECORD TO FILE                  
         SPACE 1                                                                
         USING TLRCD,R3            R3=A(RECORD)                                 
MYADDREC NTR1                                                                   
         LA    R4,KEY              R4=A(DIRECTORY KEY)                          
         USING TLDRD,R4                                                         
         MVC   TLDRKEY,TLRCKEY     MOVE KEY TO DIRECTORY                        
         SPACE 1                                                                
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                LOOK FOR RECORD ALREADY ON FILE              
         SPACE 1                                                                
         CLC   TLDRKEY,KEYSAVE     TEST WE FOUND RECORD                         
         BNE   MYADR4                                                           
         TM    TLDRSTAT,X'80'      IT HAD BETTER BE DELETED                     
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    TLDRSTAT,X'3F'      TURN OFF CAST DELETED BITS                   
         GOTO1 WRITE               AND WRITE IT BACK                            
         SPACE 1                                                                
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         MVC   AIO,AIO3            SET ALT. I/O AREA                            
         GOTO1 GETREC              GET THE RECORD SO THAT WE CAN ...            
         ST    R3,AIO                                                           
         GOTO1 PUTREC              WRITE NEW ONE BACK OVER DELETED ONE          
         B     MYADRX                                                           
         SPACE 1                                                                
MYADR4   MVC   TLDRKEY,KEYSAVE                                                  
         GOTO1 ADDREC              OK TO ADD THE RECORD                         
         SPACE 1                                                                
MYADRX   NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* THIS ROUTINE INSPECTS THE RECORD IN AIO AND DETERMINES HOW ITS                
* CATEGORY WILL EFFECT THE ANNOUNCER-ONLY-NON-MUSICIAN STATUS FOR               
* THE COMMERCIAL.                                                               
*                                                                               
TESTANN  NTR1                                                                   
         L     R3,AIO              LOOK UP CATTAB FOR THIS RECORD               
         USING TLCAD,R3                                                         
         GOTO1 CATVAL,DMCB,TLCACAT                                              
*                                                                               
         CLI   TGCAEQU,CTANN       IF CAST MEMBER IS AN ANNOUNCER               
         BNE   *+16                                                             
         L     R1,NUMANN                                                        
         LA    R1,1(R1)            INCREMENT NUMBER OF ANNOUNCERS               
         ST    R1,NUMANN                                                        
*                                                                               
*        TM    TGCAUNI,AFM         IF CAST MEMBER IS AN NON-MUSICIAN            
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BO    *+16                                                             
         L     R1,NUMNONM          THEN INC NUMBER OF NON-MUSICIANS             
         LA    R1,1(R1)                                                         
         ST    R1,NUMNONM                                                       
*                                                                               
TAX      B     XIT                                                              
         SPACE 2                                                                
* THIS ROUTINE INSPECTS THE RECORD IN AIO AND DETERMINES IF THE                 
* CAST MEMBER IS ON THE LIFT OR NOT.                                            
*                                                                               
TESTLIFT NTR1                                                                   
         L     R4,AIO                                                           
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   TLX                                                              
*                                                                               
         TM    TACASTAT,TACASTLF   IF CAST MEMBER IS ON LIFT                    
         BNO   TLX                                                              
         L     R1,NUMLIFT                                                       
         LA    R1,1(R1)            INCREMENT N'PERFORMERS ON LIFT               
         ST    R1,NUMLIFT                                                       
*                                                                               
TLX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO UPDATE NEW COMMERCIAL IF NECESSARY                    
         SPACE 1                                                                
UPDCOMML NTR1                                                                   
         OC    NSEL,NSEL           IF NOTHING SELECTED                          
         BZ    UCX                 EXIT                                         
         MVC   KEY+TLDRDA-TLDRD(4),COMDA  SET D/A OF NEW COMMERCIAL             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              READ RECORD FOR UPDATE                       
         SPACE 1                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      GET COMML DETAILS EL.                        
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,TACOSTA2       SAVE CURRENT STATUS BYTE                     
         CLC   NUMANN,=F'1'        IF ANNOUNCER IS ONLY NON-MUSICIAN            
         BNE   UC10                                                             
         CLC   NUMNONM,=F'1'                                                    
         BNE   UC10                                                             
         OI    TACOSTA2,TACOSANO   THEN SET BIT                                 
         B     UC20                                                             
         SPACE 1                                                                
UC10     NI    TACOSTA2,ALL-TACOSANO                                            
         SPACE 1                                                                
UC20     OC    NUMLIFT,NUMLIFT     ARE THERE CAST MEMBERS ON LIFT               
         BZ    UC30                                                             
         OI    TACOSTA2,TACOSLFT   THEN SET BIT                                 
         B     UC40                                                             
         SPACE 1                                                                
UC30     NI    TACOSTA2,ALL-TACOSLFT                                            
         SPACE 1                                                                
UC40     CLC   BYTE,TACOSTA2       IF STATUS BYTE CHANGED                       
         BE    UCX                                                              
         GOTO1 PUTREC              UPDATE RECORD                                
         SPACE 1                                                                
UCX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE COPIES COMMERCIAL'S COMMENT RECORD                       
         SPACE 1                                                                
CPYCMT   NTR1                                                                   
         TM    STATUS,CLICOPY      EXIT IF DOING A CLIENT COPY                  
         BO    CCMTX                                                            
         SPACE 1                                                                
         TM    STATUS,COPYCMT      EXIT IF NOT COPYING COMMERCIAL'S             
         BZ    CCMTX               COMMENT RECORDS                              
         SPACE 1                                                                
         MVI   CCMTPASS,1                                                       
         SPACE 1                                                                
CCMT10   MVI   RDUPDATE,C'Y'                                                    
         SPACE 1                                                                
         USING TLCMD,R4                                                         
         LA    R4,KEY              BUILD KEY FOR "FROM COMMERCIAL'              
         XC    KEY,KEY             COMMENT RECORD WITH:                         
         MVI   TLCMCD,TLCMCDQ      COMMENT RECORD CODE                          
         MVC   TLCMAGY,FROMAGY     FROM AGENCY CODE                             
         MVI   TLCMTYP,TLCMTCOM    COMMENT TYPE COMMERCIAL                      
         MVC   TLCMCID,FROMCID     FROM COMMERCIAL ID                           
         MVC   TLCMICOM,FROMCOM    FROM INTERNAL COMMERCIAL NUMBER              
         GOTO1 HIGH                                                             
         CLC   TLCMKEY(TLCMVER-TLCMD),KEYSAVE                                   
         BNE   CCMTX               EXIT IF NO COMMENTS EXIT                     
         SPACE 1                                                                
         TM    STATUS,COPYHIST     IF NOT COPYING HISTORY                       
         BNZ   CCMT30                                                           
         CLI   CCMTPASS,1          AND ON SECOND PASS                           
         BE    CCMT30                                                           
         GOTO1 SEQ                 LOOK FOR TP TYPE COMMENT                     
         CLC   TLCMKEY(TLCMVER-TLCMD),KEYSAVE                                   
         BNE   CCMTX                                                            
         SPACE 1                                                                
CCMT30   MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC              GET THE COMMENT RECORD                       
         SPACE 1                                                                
         TM    STATUS,COPYHIST                                                  
         BZ    CCMT40                                                           
         L     R4,AIO              MARK RECORD DELETED                          
         OI    TLCMSTAT,X'80'                                                   
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TLDRD,R4                                                         
         LA    R4,KEY              MARK KEY DELETED                             
         OI    TLDRSTAT,X'80'                                                   
         GOTO1 WRITE                                                            
         MVI   TLDRSTAT,0                                                       
         DROP  R4                                                               
         SPACE 1                                                                
         USING TLCMD,R4                                                         
CCMT40   MVC   TLCMAGY,TOAGY       CHANGE KEY TO NEW AGENCY                     
         MVC   TLCMCID,TOCID       COMMERCIAL ID                                
         TM    STATUS,COPYHIST                                                  
         BO    *+14                                                             
         MVC   TLCMICOM,TOCOM      AND INTERNAL COMMERCIAL NUMBER               
         MVI   TLCMVER,0                                                        
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVC   TLCMAGY,TOAGY       CHANGE RECORD TO NEW AGENCY                  
         MVC   TLCMCID,TOCID       COMMERCIAL ID                                
         TM    STATUS,COPYHIST                                                  
         BO    *+14                                                             
         MVC   TLCMICOM,TOCOM      AND INTERNAL COMMERCIAL NUMBER               
         MVI   TLCMVER,0                                                        
         MVI   TLCMSTAT,0                                                       
         LR    R3,R4                                                            
         BAS   RE,MYADDREC                                                      
         SPACE 1                                                                
         ZIC   RE,CCMTPASS                                                      
         AHI   RE,1                                                             
         STC   RE,CCMTPASS                                                      
         CLI   CCMTPASS,2                                                       
         BNH   CCMT10                                                           
         SPACE 1                                                                
CCMTX    OI    STATUS,REREAD       SET WE NEED TO RE-READ SYSIO'S KEY           
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORCE BOTTOM OF REPORT                                
         SPACE 1                                                                
BXBOT    NTR1                                                                   
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE/BOX ROUTINES (HEADHOOK)                                 
         SPACE 1                                                                
HOOK     NTR1                                                                   
         L     R3,ABOX             NOW HANDLE BOXES                             
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXROWS+7,C'T'      SET ROWS                                     
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+66,C'B'                                                  
         SPACE 1                                                                
         LA    R2,HEAD4            HEADLINES                                    
         MVC   14(L'TGAGY,R2),FRCLAGY                                           
         MVC   21(36,R2),FRCLAGN                                                
         MVC   85(L'TGAGY,R2),TOCLAGY                                           
         MVC   92(36,R2),TOCLAGN                                                
         SPACE 1                                                                
         LA    R2,HEAD5                                                         
         MVC   14(L'TGCLI,R2),FRCLCLI                                           
         MVC   21(36,R2),FRCLCLN                                                
         MVC   85(L'TGCLI,R2),TOCLCLI                                           
         MVC   92(36,R2),TOCLCLN                                                
         SPACE 1                                                                
         LA    R2,BOXCOLS          SET DEFAULT COLUMNS                          
         USING LINED,R2                                                         
         CLI   RCSUBPRG,1                                                       
         BNE   HK10                                                             
         MVI   LLHS,C'L'                                                        
         MVI   LCOL1,C'C'                                                       
         MVI   LCOL2,C'C'                                                       
         MVI   LRHS,C'R'                                                        
         B     HKX                                                              
         SPACE 1                                                                
HK10     CLI   RCSUBPRG,0          IF COPYING ONLY ONE PRODUCT                  
         BNE   *+16                                                             
         MVI   L2LHS,C'L'                                                       
         MVI   L2RHS,C'R'                                                       
         B     HK30                                                             
         SPACE 1                                                                
         MVI   L3LHS,C'L'                                                       
         MVI   L3COL1,C'C'                                                      
         MVI   L3RHS,C'R'                                                       
         CLI   RCSUBPRG,2          IF COPYING ONLY ONE PRODUCT                  
         BNE   HKX                                                              
         SPACE 1                                                                
HK30     LA    R2,HEAD6            PUT PRODUCT IN HEADLINES                     
         MVC   14(L'TGPRD,R2),FRCLPRD                                           
         MVC   21(36,R2),FRCLPRN                                                
         MVC   85(L'TGPRD,R2),TOCLPRD                                           
         MVC   92(36,R2),TOCLPRN                                                
HKX      B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 1                                                                
                                                                                
SELINF   CLI   SCOSCST,C'Y'        IF COPYING HISTORY AND                       
         BNE   PFKMSG                                                           
         MVI   MYMSGNO1,3          CAST DISPLAYED - SELECT AS DESIRED           
         LA    R2,SCOSELH                                                       
         B     INFEND                                                           
         SPACE 1                                                                
LSTINF   CLI   RECNUM,PC           IF PERCYCLE/TRANSFER                         
         BNE   LSTINF2                                                          
         TM    PCYSTAT,PCSELIG     AND NO ELIGIBLE CAST WAS FOUND               
         BO    LSTINF2                                                          
         LA    R2,SCOFCIDH         RETURN THE REASON WHY ...                    
         MVC   MYMSGNO,=Y(ERRNTPRI)                                             
         TM    PCYSTAT,PCSPRIC     IS COMMERCIAL A PRIMARY FOR ANY              
         BZ    ERREND              GUARANTEES?                                  
         MVC   MYMSGNO,=Y(ERRELSUB)                                             
         TM    PCYSTAT,PCSESUB     DOES GUARANTEE HAVE ELIGIBLE                 
         BO    ERREND              SUBSIDIARIES?                                
         MVC   MYMSGNO,=Y(ERRNOSUB)                                             
         TM    PCYSTAT,PCSNSUB     DOES GUARANTEE HAVE ANY ACTIVE               
         BO    ERREND              SUBSIDIARIES?                                
         MVC   MYMSGNO,=Y(ERRNGELI)                                             
         B     ERREND              OR IS GUARANTEE JUST INELIGIBLE?             
         SPACE 1                                                                
LSTINF2  MVI   LSTPAGE,C'Y'                                                     
         CLI   SCOSCST,C'Y'        IF COPYING HISTORY                           
         BNE   PFKMSG                                                           
         OI    TRNSTAT,OKINTPFK    SET PF13 TO OK COPY                          
         OI    STATUS,PFKPEND      SET PFKEY PENDING                            
         MVI   MYMSGNO1,10         NO MORE CAST TO DISPLAY - SELECT             
         LA    R2,SCOSELH                                                       
         B     INFEND                                                           
         SPACE 1                                                                
HISTINV  LA    R2,SCOHISTH                                                      
         B     FLDINV                                                           
         SPACE 1                                                                
CASTINV  LA    R2,SCOCASTH                                                      
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
ERRNOGLB MVI   ERROR,ERNOGLOB      NO GLOBAL CLIENT/PRODUCT                     
         B     THEEND                                                           
         SPACE 1                                                                
MISSERR  MVI   ERROR,MISSING       MISSING INPUT                                
         B     THEEND                                                           
         SPACE 1                                                                
RECXIST  MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         B     THEEND                                                           
         SPACE 1                                                                
PFKMSG   MVI   MYMSGNO1,35         SET HIT PF13 TO PROCESS REQUEST              
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,CONRECH                                                       
         OI    TRNSTAT,OKINTPFK    SET PF13 TO OK COPY                          
         OI    STATUS,PFKPEND      SET PFKEY PENDING                            
         B     THEEND                                                           
         SPACE 1                                                                
COPIED   MVI   MYMSGNO1,44         SET COMPLETION MESSAGE                       
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,CONRECH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
CLIERR   MVI   ERROR,ERCOPYC       SET CLIENT ERROR MESSAGE                     
         LA    R2,SCOTCIDH                                                      
         B     THEEND                                                           
         SPACE 1                                                                
PRDERR   MVI   ERROR,ERCOPYP       SET PRODUCT ERROR MESSAGE                    
         LA    R2,SCOTCIDH                                                      
         B     THEEND                                                           
         SPACE 1                                                                
WEBERR   LA    R2,SCOTCIDH                                                      
         MVC   MYMSGNO,=Y(ERUSEWEB)                                             
         B     ERREND                                                           
         SPACE 1                                                                
ERRFRPCY LA    R2,SCOFCIDH         FROM COMMERCIAL CANNOT BE PERCYCLE           
         B     FLDINV              FOR PERCYCLE/TRANSFER                        
         SPACE 1                                                                
ERRPCYCH LA    R2,SCOHISTH                                                      
         MVC   MYMSGNO,=Y(ERRCHPCY) MUST COPY HISTORY                           
         B     ERREND                                                           
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERPPLSI2 MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
*                                                                               
ERREGON  MVC   MYMSGNO,=Y(534)        FROM AGY/CLIENT REG STATUS ON             
         J     ERREND                                                           
*                                                                               
ERREGOFF MVC   MYMSGNO,=Y(535)        FROM AGY/CLIENT REG STATUS OFF            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
         SPACE 1                                                                
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SPECS                                                            
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SPROG 0,1,2,3                                                          
         SSPEC H1,2,RUN                                                         
         SSPEC H1,93,REPORT                                                     
         SSPEC H2,93,REQUESTOR                                                  
         SSPEC H2,108,PAGE                                                      
*                                                                               
         SSPEC H1,54,C'Client Copy'                                             
         SSPEC H2,54,11X'BF'                                                    
*                                                                               
         SSPEC H4,2,C'From Agency'                                              
         SSPEC H5,2,C'     Client'                                              
*                                                                               
         SSPEC H4,75,C'To Agency'                                               
         SSPEC H5,75,C'   Client'                                               
*                                                                               
         SPROG 0,2                                                              
         SSPEC H6,2,C'     Product'                                             
         SSPEC H6,75,C'   Product'                                              
*                                                                               
         SPROG 0                                                                
         SSPEC H9,53,C'Commercial ID'                                           
*                                                                               
         SPROG 1                                                                
         SSPEC H9,26,C'Product'                                                 
         SSPEC H9,51,C'Product Name'                                            
         SSPEC H9,82,C'Commercial ID'                                           
*                                                                               
         SPROG 2,3                                                              
         SSPEC H9,2,C'Estimate Change Summary'                                  
         SSPEC H10,2,23X'BF'                                                    
         SSPEC H9,41,C'Estimate Code'                                           
         SSPEC H9,70,C'Estimate Name'                                           
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
                                                                                
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 1                                                                
VALOPT   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SCLOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         SPACE 1                                                                
VOPT4    DS    0H                                                               
         MVC   ERRDISP,SCDISP1     SET DISP. INTO FLD OF LHS FOR ERRORS         
         SPACE 1                                                                
         CLC   =C'TRACE',SCDATA1   TRACE                                        
         BNE   *+12                                                             
         OI    STATUS,TRACING                                                   
         B     VOPT8                                                            
         SPACE 1                                                                
         DS    0H                                                               
         J     FLDINV                                                           
         SPACE 1                                                                
VOPT8    LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT4            AND CONTINUE                                 
         MVI   ERRDISP,0                                                        
         SPACE 1                                                                
VOPTX    XIT                                                                    
         EJECT                                                                  
*              COPY CAST RECORDS FOR THIS COMMERCIAL                            
         SPACE 1                                                                
CCAST    NTR1  BASE=*,LABEL=*                                                   
         LH    RF,=Y(TIEND-TASYSIOD)                                            
         XCEFL TASYSIOD                                                         
         MVC   TIQSKEY,CONTKEY     SET CONTINUE KEY                             
         LA    R2,IOHOOK           SET A(I/O HOOK) FOR SYSIO                    
         ST    R2,TIHOOK                                                        
         MVC   TIUSERID,TWAORIG                                                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVC   TIACOMFC,ACOMFACS                                                
         SPACE 1                                                                
         MVI   TIREAD,TLCACDQ      SET TO READ CAST RECORDS                     
         MVC   TIFCOM,FROMCOM      SET TO FILTER ON INTERNAL COMML NO.          
         OI    TIQFLAGS,TIQFUPRC                                                
         SPACE 1                                                                
         MVC   CASTSEQ,=H'1'       START SEQUENCE OFF AT 1                      
         SPACE 1                                                                
         TM    STATUS,SELCAST      IF SELECTING CAST                            
         BNO   CC10                                                             
         LA    R1,SCOLINEH         SET LIST LINE                                
         ST    R1,APLINE                                                        
         XC    DATABLE(36),DATABLE    CLEAR D/A TABLE                           
         TWAXC SCOSELH,SCOLSTH,PROT=Y      CLEAR SCREEN                         
         SPACE 1                                                                
CC10     GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO READ RECORDS              
         TM    STATUS,SELCAST      IF SELECTING CAST                            
         BNO   CC20                                                             
         CLI   KEY,X'90'           & IF SYSIO STOPPED (NOT FORCED)              
         BE    CC20                                                             
         XC    CONTKEY,CONTKEY     CLEAR CONTINUE KEY TO START OVER             
         SPACE 1                                                                
CC20     MVI   COUNTER,0                                                        
         MVI   LISTED,C'Y'                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              ROUTINE COPIES ADDITIONAL COMMERCIAL RECORDS                     
         SPACE 1                                                                
CPYCOMS  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
                                                                                
         XC    SVCOKEY,SVCOKEY                                                  
         MVC   SVCOKEY(L'TLCOKEY),SVKEY                                         
                                                                                
         USING TLCOD,R3                                                         
CCOM10   LA    R3,SVCOKEY                                                       
         ZIC   RE,TLCOVER                                                       
         AHI   RE,1                                                             
         CHI   RE,TLCOV250                                                      
         BH    CCOMX                                                            
         STC   RE,TLCOVER                                                       
         DROP  R3                                                               
                                                                                
         LA    R3,KEY                                                           
         MVC   KEY,SVCOKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),SVCOKEY                                           
         BNE   CCOM10                                                           
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         GOTO1 SAVPTRS,DMCB,ASVPBLK                                             
                                                                                
         USING TLCOD,R3                                                         
         L     R3,AIO2                                                          
         OI    TLCOSTAT,X'80'                                                   
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'E8',ASVPBLK),AADPBLK                             
                                                                                
         MVC   TLCOKEY,SVCOKEY                                                  
         MVC   TLCOAGY,TOAGY                                                    
         MVC   TLCOCLI,TOCLI                                                    
         MVC   TLCOPRD,TOPRD                                                    
         MVC   TLCOCID,TOCID                                                    
         NI    TLCOSTAT,X'7F'                                                   
         DROP  R3                                                               
                                                                                
         BRAS  RE,MYADDREC                                                      
                                                                                
         L     RE,ASVPBLK                                                       
         XC    0(255,RE),0(RE)                                                  
         GOTO1 ADDPTRS,DMCB,(X'08',ASVPBLK),AADPBLK                             
         B     CCOM10                                                           
                                                                                
CCOMX    OI    STATUS,REREAD       SET WE NEED TO RE-READ SYSIO'S KEY           
         MVC   AIO,AIO1                                                         
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PERFORM SCREEN SETUP                              *         
***********************************************************************         
                                                                                
SETSCRN  NTR1  BASE=*,LABEL=*                                                   
         CLI   TWASCR,SCR8E                                                     
         JE    XIT                                                              
         CLC   TWALREC,RECNUM                                                   
         JNE   SS10                                                             
         CLC   TWALACT,ACTNUM                                                   
         JNE   SS10                                                             
         GOTO1 FLDVAL,DMCB,(X'80',SCOFAYH),(X'80',SCOTCIDH)                     
         JNE   XIT                                                              
*                                                                               
SS10     NI    SCOFAYH+4,X'FF'-X'20'                                            
*                                                                               
         GOTO1 FLDVAL,DMCB,(2,SCOCASHH),SCOLSTH                                 
         GOTO1 FLDVAL,DMCB,SCOCCMHH,(8,SCOCCOMH)                                
*                                                                               
         OI    SCOCASHH+1,X'08'                                                 
         NI    SCOCASHH+1,X'FB'                                                 
         NI    SCOCASTH+1,X'DF'                                                 
         OI    SCOHISHH+1,X'08'                                                 
         NI    SCOHISHH+1,X'FB'                                                 
         NI    SCOHISTH+1,X'DF'                                                 
         NI    SCOCMUHH+1,X'FB'                                                 
         NI    SCOCMUSH+1,X'DF'                                                 
         OI    SCOCCMHH+1,X'08'                                                 
         NI    SCOCCMHH+1,X'FB'                                                 
         NI    SCOCCOMH+1,X'DF'                                                 
         GOTO1 FLDVAL,DMCB,(1,SCOCASTH),(X'90',SCOCCOMH)                        
*                                                                               
SS20     CLI   RECNUM,PC                                                        
         JNE   SS30                                                             
         GOTO1 FLDVAL,DMCB,(X'0A',SCOCASHH),(8,SCOHISTH)                        
         GOTO1 FLDVAL,DMCB,(X'0A',SCOCMUHH),(8,SCOCMUSH)                        
         GOTO1 FLDVAL,DMCB,(X'0A',SCOCCMHH),(8,SCOCCOMH)                        
         MVI   SCOCAST,C'Y'                                                     
         MVI   SCOCASTH+5,1                                                     
         MVI   SCOHIST,C'N'                                                     
         MVI   SCOHISTH+5,1                                                     
         MVI   SCOCMUS,C'N'                                                     
         MVI   SCOCMUSH+5,1                                                     
         MVI   SCOCCOM,C'Y'                                                     
         MVI   SCOCCOMH+5,1                                                     
         J     XIT                                                              
*                                                                               
SS30     CLI   TGCTSTTY,TASTTYPP   IF THIS IS NOT A PROGRAMMER                  
         JE    SS40                OR LEVEL 2, HIDE COPY COMMENT KEY            
         CLI   TGCTSTTY,TASTTYP2                                                
         JNE   SS40                                                             
SS40     GOTO1 FLDVAL,DMCB,SCOCCMHH,(X'10',SCOCCOMH)                            
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PERFORM SELECT FIELD SETUP                        *         
***********************************************************************         
                                                                                
SETSELS  NTR1  BASE=*,LABEL=*                                                   
         CLI   TWASCR,SCR8E                                                     
         JE    XIT                                                              
*                                                                               
         GOTO1 FLDVAL,DMCB,(8,SCOSELH),SCOLSTH                                  
         CLI   SCOSCST,C'Y'                                                     
         JNE   SSELS20                                                          
         LA    R2,SCOSELH                                                       
         LA    R3,SCOLSTH                                                       
SSELS10  CR    R2,R3                                                            
         JH    SSELS20                                                          
         NI    1(R2),X'DF'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         J     SSELS10                                                          
*                                                                               
SSELS20  GOTO1 FLDVAL,DMCB,SCOSELH,(X'10',SCOLSTH)                              
*                                                                               
         CLI   RECNUM,PC                                                        
         JNE   XIT                                                              
         CLI   SCOSCSTH+5,0                                                     
         JNE   SSEL30                                                           
         MVI   SCOSCST,C'N'                                                     
         MVI   SCOSCSTH+5,1                                                     
         OI    SCOSCSTH+6,X'80'                                                 
SSEL30   CLI   SCOSCST,C'Y'                                                     
         JE    XIT                                                              
         GOTO1 FLDVAL,DMCB,SCOSELH,(X'08',SCOLSTH)                              
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DETERMINES IF CAST IS ELIGIBLE FOR PER CYCLE TRANSFER*         
*        ON ENTRY ... R2=A(CURRENT SCREEN LINE)                       *         
*                     R3=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
         USING LLINED,R2                                                        
PCELIG   NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,PC           ONLY CHECK IS RECORD/ACTION IS               
         JNE   YES                 PERCYCLE/TRANSFER                            
                                                                                
         BRAS  RE,READGRT          READ GRT RECORD INTO AIO3                    
         JNE   NO                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO3             R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         CLC   TAGUCOM,FROMCOM     FROM COMMERCIAL MUST BE THE                  
         JNE   NO                  PRIMARY COMMERCIAL                           
         DROP  R4                                                               
                                                                                
         OI    PCYSTAT,PCSPRIC     SET PRIMARY COMM'L FOR GUARANTEE             
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY           READ ALL CHECK RECORDS FOR THIS                
         MVI   TLCKPCD,TLCKHCDQ  PERFORMER FOR THIS COMMERCIAL                  
         MVC   TLCKHCOM,FROMCOM                                                 
         MVC   TLCKHSSN,TGSSN                                                   
         GOTO1 HIGH                                                             
         J     PCE20                                                            
PCE10    GOTO1 SEQ                                                              
PCE20    CLC   KEY(TLCKHCAT-TLCKPCD),KEYSAVE                                    
         JNE   PCE40                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TACDD,R4                                                         
         L     R4,AIO            R4=A(CHECK RECORD)                             
         MVI   ELCODE,TACDELQ    GET CHECK DETAILS ELEMENT                      
         BRAS  RE,GETEL                                                         
         JNE   PCE10                                                            
         OC    TACDDTE,TACDDTE   ONLY CONSIDER CHECKS THAT HAVE                 
         JNZ   PCE10             NOT PROCESSED YET                              
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO            R4=A(CHECK RECORD)                             
         MVI   ELCODE,TACAELQ    GET CAST DETAIL ELEMENT                        
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         XC    TACAGUA,HEXFFS                                                   
         CLC   TACAGUA,TGGUA     ONLY CONSIDER CHECKS WITH GUARANTEE            
         JNE   PCE10             CODE THAT MATCHES CURRENT PAYMENT              
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO            R4=A(CHECK RECORD)                             
         MVI   ELCODE,TAPDELQ    GET PAYMENT DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    *+6               ONLY CONSIDER PER CYCLE PAYMENTS               
         DC    H'00'                                                            
         GOTO1 USEVAL,DMCB,(X'40',TAPDUSE)                                      
         TM    TGUSSTA2,APPREUSE                                                
         JZ    PCE10                                                            
         DROP  R4                                                               
                                                                                
         MVC   PCEKEY,KEY        SAVE CHECK KEY                                 
                                                                                
         MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR' RESET TO READ TALENT DIRECTORY               
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO            R4=A(CHECK RECORD)                             
                                                                                
         USING TLINPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLINPCD,TLINBCDQ  CHECK IF INVOICE HAS ALREADY BEEN              
         MVC   TLINBAGY,TLCKAGY  BILLED                                         
         MVC   TLINBINV,TLCKINV                                                 
         XC    TLINBINV,HEXFFS                                                  
         GOTO1 HIGH                                                             
         CLC   TLINPKEY,KEYSAVE  IF NOT, CANNOT SWITCH THE PRIMARY              
         JNE   PCE30                                                            
         TM    TLINBST2,TAINSBIL                                                
         JZ    NO                                                               
         DROP  R3,R4                                                            
                                                                                
PCE30    MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         MVC   KEY,PCEKEY          RESTORE CHECK READ SEQUENCE                  
         GOTO1 HIGH                                                             
         J     PCE10                                                            
                                                                                
PCE40    MVC   SYSDIR,=CL8'TALDIR' RESTORE FILE SETTING                         
         MVC   SYSFIL,=CL8'TALFIL' AND INDICATE THAT NO PER CYCLE               
                                                                                
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'A0',0)                                    
         JE    *+6                                                              
         DC    H'00'               RE-READ GUARANTEE RECORD                     
                                                                                
         XC    LPCYSTRT,LPCYSTRT   INITIALIZE LAST PER CYCLE START DATE         
         XR    R0,R0               AND # OF MISMATCHED CYCLES FOUND             
                                                                                
         USING TAGCD,R4                                                         
         L     R4,AIO              R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGCELQ      SAVE LATEST PER CYCLE PAYMENT                
         BRAS  RE,GETEL            START DATE                                   
         J     PCE60                                                            
PCE50    BRAS  RE,NEXTEL                                                        
PCE60    JNE   PCE70                                                            
         MVC   LPCYSTRT,TAGCSTRT                                                
         J     PCE50                                                            
         DROP  R4                                                               
                                                                                
PCE70    OC    LPCYSTRT,LPCYSTRT   CANNOT TRANSFER IF NO PER CYCLE              
         JZ    NO                  PAYMENTS HAVE BEEN MADE                      
                                                                                
         MVI   BYTE,0                                                           
                                                                                
         USING TLCAPD,R3                                                        
         LA    R3,KEY              READ ALL CAST KEYS ATTACHED                  
         XC    KEY,KEY             TO THIS GUARANTEE                            
         MVI   TLCAPCD,TLCAGCDQ                                                 
         MVC   TLCAGSSN,TGSSN                                                   
         MVC   TLCAGGUA,TGGUA                                                   
         XC    TLCAGGUA,HEXFFS                                                  
         GOTO1 HIGH                                                             
         J     PCE90                                                            
PCE80    GOTO1 SEQ                                                              
PCE90    CLC   KEY(TLCAGCOM-TLCAPCD),KEYSAVE                                    
         JNE   PCE150                                                           
                                                                                
         CLC   TLCAGCOM,FROMCOM    SKIP CAST RECORD FOR PRIMARY COMM'L          
         JE    PCE80                                                            
                                                                                
         MVC   PCEKEY,KEY          SAVE CAST KEY                                
         MVC   TGCOM,TLCAGCOM                                                   
         DROP  R3                                                               
                                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)                                   
         JE    *+6                 READ COMMERCIAL RECORD FOR                   
         DC    H'00'               POSSIBLE PRIMARY                             
                                                                                
         MVC   KEY,PCEKEY          RESTORE CAST READ SEQUENCE                   
         GOTO1 HIGH                                                             
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JE    *+6                 MAY NOT BE LOCKED, RELEASED OR               
         DC    H'00'               SET UP FOR CANADIAN RATES                    
         TM    TACOSTAT,TACOSTLO+TACOSTRL+TACOSCRT                              
         JNZ   PCE80                                                            
         CLI   TACOMED,TACOMEDT    MEDIA MUST BE TELEVISION                     
         JE    PCE100                                                           
         CLI   TACOMED,TACOMEDI    INTERNET                                     
         JE    PCE100                                                           
         CLI   TACOMED,TACOMEDN    OR NEW MEDIA                                 
         JNE   PCE80                                                            
PCE100   MVC   LFTRSTRT,TACOFCYC   INITIALIZE LATEST FTRACK DATE                
         DROP  R4                                                               
                                                                                
         GOTO1 GETREC              GET CAST RECORD                              
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO              R4=A(CAST RECORD)                            
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACALAST,TACALAST   SKIP LAST SERVICED CAST                      
         JNZ   PCE80                                                            
         OC    TACAFCYC,TACAFCYC   IF FIRST FIXED CYCLE AT THIS LEVEL           
         JZ    *+10                                                             
         MVC   LFTRSTRT,TACAFCYC   SAVE AS LATEST FTRACK DATE                   
         DROP  R4                                                               
                                                                                
         MVI   BYTE,1                                                           
                                                                                
         USING TACRD,R4                                                         
         L     R4,AIO              R4=A(CAST RECORD)                            
         MVI   ELCODE,TACRELQ      READ ALL APPLIED CREDIT ELEMENTS             
         BRAS  RE,GETEL                                                         
         J     PCE120                                                           
PCE110   BRAS  RE,NEXTEL                                                        
PCE120   JNE   PCE130                                                           
         MVC   LFTRSTRT,TACRSTRT   SAVE LATEST START DATE                       
         J     PCE110                                                           
         DROP  R4                                                               
                                                                                
PCE130   CLC   LPCYSTRT,LFTRSTRT   IF LATEST PER CYCLE PAYMENT MATCHES          
         JNE   PCE140              CAST'S LATEST FTRACK, COMMERCIAL             
         OI    PCYSTAT,PCSESUB     IS ELIGIBLE TO BE PRIMARY                    
         J     NO                                                               
PCE140   AHI   R0,1                ELSE ADD TO MISMATCHED COUNT                 
         J     PCE80                                                            
                                                                                
PCE150   CLI   BYTE,0              IF NO ACTIVE SUBSIDIARIES WERE               
         JNE   PCE160              FOUND                                        
         OI    PCYSTAT,PCSNSUB     SET STATUS                                   
                                                                                
PCE160   LTR   R0,R0               IF AT LEAST 1 MISMATCHED CYCLE               
         JZ    NO                  FOUND                                        
         MVC   CAGUA,TGGUA         DISPLAY GUARANTEE CODE                       
         XC    CAGUA,HEXFFS                                                     
         OI    PCYSTAT,PCSELIG     AND SET ELIGIBLE CAST FOUND                  
         J     YES                                                              
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TRANSFERS OVERSCALE AMOUNTS FOR PERCYCLE/TRANSFER    *         
*        ON ENTRY ... R3=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
TRNOVA   NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,PC           IF REC/ACT IS PERCYCLE/TRANSFER              
         JNE   XIT                                                              
                                                                                
         USING TAOAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAOAELQ    SAVE CAST OVERSCALE AMOUNT ELEMENT             
         BRAS  RE,GETEL          FROM ORIGINAL PRIMARY COMMERCIAL               
         JNE   XIT                                                              
         MVC   WORK,TAOAD                                                       
         GOTO1 REMELEM           THEN DELETE IT                                 
                                                                                
         LA    R4,ELEMENT        INITIALIZE NEW CAST OVERSCALE AMOUNT           
         XC    ELEMENT,ELEMENT   ELEMENT FOR OLD PRIMARY COMMERCIAL             
         MVI   TAOAEL,TAOAELQ                                                   
         MVI   TAOALEN,TAOALNQ                                                  
                                                                                
         LA    RE,WORK+TAOAUSE-TAOAD   RE=A(INITIAL OVERSCALE AMOUNTS)          
         ZIC   RF,WORK+TAOANUM-TAOAD   RF=# OF OVERSCALE AMOUNTS                
                                                                                
         LA    R1,TAOAUSE        R1=A(NEW OVERSCALE AMOUNTS)                    
                                                                                
TOVA10   CLC   =C'HLD',0(RE)     STRIP HOLDING FEE TYPES                        
         JE    TOVA20            OUT OF THE INITIAL OVERSCALE AMOUNTS           
         CLC   =C'SHL',0(RE)                                                    
         JE    TOVA20                                                           
         CLC   =C'ADH',0(RE)                                                    
         JE    TOVA20                                                           
                                                                                
         ZIC   R0,TAOALEN                                                       
         AHI   R0,L'TAOASBEL                                                    
         STC   R0,TAOALEN        BUMP ELEMENT LENGTH                            
                                                                                
         ZIC   R0,TAOANUM                                                       
         AHI   R0,1                                                             
         STC   R0,TAOANUM        BUMP # OF OVERSCALE AMOUNTS                    
                                                                                
         MVC   0(L'TAOASBEL,R1),0(RE)                                           
                                                                                
         LA    R1,L'TAOASBEL(R1)                                                
                                                                                
TOVA20   LA    RE,L'TAOASBEL(RE)                                                
         BCT   RF,TOVA10                                                        
                                                                                
         CLI   TAOANUM,0         IF ANY OVERSCALE AMOUNTS IN ELEMENT            
         JE    TOVA30                                                           
         GOTO1 ADDELEM           ADD NEW OVERSCALE AMOUNTS ELEMENT              
TOVA30   GOTO1 PUTREC            AND PUT CAST RECORD                            
                                                                                
         GOTO1 REMELEM           REMOVE ELEMENT                                 
                                                                                
         LA    R4,ELEMENT        INITIALIZE CAST OVERSCALE AMOUNT               
         XC    ELEMENT,ELEMENT   ELEMENT FOR NEW PRIMARY COMMERCIAL             
         MVI   TAOAEL,TAOAELQ                                                   
         MVI   TAOALEN,TAOALNQ                                                  
                                                                                
         LA    RE,WORK+TAOAUSE-TAOAD   RE=A(INITIAL OVERSCALE AMOUNTS)          
         ZIC   RF,WORK+TAOANUM-TAOAD   RF=# OF OVERSCALE AMOUNTS                
                                                                                
         LA    R1,TAOAUSE        R1=A(NEW OVERSCALE AMOUNTS)                    
                                                                                
TOVA40   CLC   =C'HLD',0(RE)     STRIP HOLDING FEE TYPES                        
         JE    TOVA50            OUT OF THE INITIAL OVERSCALE AMOUNTS           
         CLC   =C'SHL',0(RE)                                                    
         JE    TOVA50                                                           
         CLC   =C'ADH',0(RE)                                                    
         JNE   TOVA60                                                           
                                                                                
TOVA50 ZIC     R0,TAOALEN                                                       
         AHI   R0,L'TAOASBEL                                                    
         STC   R0,TAOALEN        BUMP ELEMENT LENGTH                            
                                                                                
         ZIC   R0,TAOANUM                                                       
         AHI   R0,1                                                             
         STC   R0,TAOANUM        BUMP # OF OVERSCALE AMOUNTS                    
                                                                                
         MVC   0(L'TAOASBEL,R1),0(RE)                                           
                                                                                
         LA    R1,L'TAOASBEL(R1)                                                
                                                                                
TOVA60 LA      RE,L'TAOASBEL(RE)                                                
         BCT   RF,TOVA40                                                        
                                                                                
         CLI   TAOANUM,0         IF ANY OVERSCALE AMOUNTS IN ELEMENT            
         JE    XIT                                                              
         GOTO1 ADDELEM           ADD NEW OVERSCALE AMOUNTS ELEMENT              
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS CAST LEVEL FFC FOR PERCYCLE/TRANSFER            *         
*        ON ENTRY ... R3=A(CAST RECORD)                               *         
*                     AIO=A(GUARANTEE RECORD)                         *         
***********************************************************************         
                                                                                
SETFFC   NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,PC           IF REC/ACT IS PERCYCLE/TRANSFER              
         JNE   XIT                                                              
                                                                                
         BRAS  RE,READGRT          READ GRT RECORD INTO AIO3                    
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TAGCD,R4                                                         
         L     R4,AIO3             R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGCELQ      SAVE LATEST PER CYCLE PAYMENT                
         BRAS  RE,GETEL            START DATE                                   
         J     SFFC20                                                           
SFFC10   BRAS  RE,NEXTEL                                                        
SFFC20   JNE   SFFC30                                                           
         MVC   LPCYSTRT,TAGCSTRT                                                
         J     SFFC10                                                           
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
SFFC30   LR    R4,R3               R4=A(CAST RECORD)                            
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TACAFCYC,LPCYSTRT   SAVE FIRST FIXED CYCLE DATE                  
         MVC   TGGUA,TACAGUA       AND GUARANTEE CODE                           
         DROP  R4                                                               
                                                                                
         OC    EPCYSTRT,EPCYSTRT                                                
         JZ    SFFC40                                                           
         CLC   EPCYSTRT,LPCYSTRT   IF THIS IS EARLIEST ENCOUNTERED              
         JNH   XIT                                                              
SFFC40   MVC   EPCYSTRT,LPCYSTRT   SAVE IT                                      
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE READS GUARANTEE RECORD INTO AIO3                     *         
*        ON ENTRY ... R3=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
         USING TLCAD,R3                                                         
READGRT  NTR1  BASE=*,LABEL=*                                                   
         MVC   TGSSN,TLCASSN       SAVE PERFORMER SS#                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         LR    R4,R3               R4=A(CAST RECORD)                            
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAGUA,TACAGUA     PERFORMER MUST BE ON A GUARANTEE             
         JZ    NO                                                               
         MVC   TGGUA,TACAGUA       SAVE GUARANTEE CODE                          
         XC    TGGUA,HEXFFS        READ GUARANTEE RECORD INTO AIO3              
         DROP  R4                                                               
                                                                                
         OI    STATUS,REREAD       SET WE NEED TO RE-READ SYSIO'S KEY           
                                                                                
         MVC   FULL,AIO                                                         
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'A0',0)                                    
         MVC   AIO,FULL                                                         
         JE    YES                                                              
         J     NO                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE UPDATES GUARANTEE RECORD WITH NEW PRIMARY COMMERCIAL *         
*        FOR PER CYCLE TRANSFER                                       *         
*        ON ENTRY ... AIO=A(CAST RECORD)                              *         
***********************************************************************         
                                                                                
UPDGUAR  NTR1  BASE=*,LABEL=*                                                   
         USING TLCAD,R4                                                         
         L     R4,AIO              R4=A(CAST RECORD)                            
         MVC   TGSSN,TLCASSN       SAVE SOCIAL SECURITY NUMBER                  
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         OC    TACAGUA,TACAGUA     IF PERFORMER IS ON GUARANTEE                 
         JZ    XIT                                                              
         MVC   TGGUA,TACAGUA       SAVE GUARANTEE CODE                          
         DROP  R4                                                               
                                                                                
         XC    TGGUA,HEXFFS        READ GUARANTEE RECORD                        
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'B0',0)                                    
         JNE   XIT                                                              
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO              R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         BRAS  RE,UPDGAC           UPDATE GUARANTEE'S AGENCY/CLIENT             
                                                                                
         CLI   RECNUM,PC           IF RUNNING PER CYCLE TRANSFER                
         JNE   UGUAR10                                                          
         MVC   TAGUCOM,TOCOM       INSERT NEW PRIMARY COMMERCIAL                
                                                                                
UGUAR10  GOTO1 PUTREC              AND WRITE BACK THE GUARANTEE                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE UPDATES GUARANTEE RECORD'S VALID AGENCY/CLIENTS      *         
*        WITH NEW AGENCY/CLIENT                                       *         
*        ON ENTRY ... R4=A(GUARANTEE DETAILS ELEMENT)                 *         
***********************************************************************         
                                                                                
         USING TAGUD,R4                                                         
UPDGAC   NTR1  BASE=*,LABEL=*                                                   
         OC    TAGUAGY,TAGUAGY     IF PRIMARY AGENCY/CLIENT IS NOT              
         JZ    XIT                 DEFINED, DO NOT UPDATE AGY/CLI               
                                                                                
         CLC   TAGUAGY,TOAGY       IF "TO" AGENCY IS ALREADY SET AS             
         JNE   UGAC10              PRIMARY                                      
         OC    TAGUCLI,TAGUCLI     WITH NO CLIENT LIMITS                        
         JZ    XIT                                                              
         CLC   TAGUCLI,TOCLI       OR "TO" CLIENT IS ALREADY SET AS             
         JE    XIT                 PRIMARY, DO NOT UPDATE AGY/CLI               
                                                                                
         USING GACBLKD,R3                                                       
UGAC10   LA    R3,ADDGAC                                                        
         MVC   GACAGY,TOAGY        PREPARE TO ADD NEW AGENCY/CLIENT             
         MVC   GACCLI,TOCLI        TO GUARANTEE                                 
         MVI   GACLNQ(R3),X'FF'                                                 
                                                                                
         CLI   RECNUM,PC           IF RUNNING PER CYCLE TRANSFER                
         JE    UGAC30              SKIP AHEAD                                   
                                                                                
         TM    STATUS,CLICOPY      IF RUNNING CLIENT COPY                       
         JZ    UGAC20                                                           
         CLC   TAGUAGY,FROMAGY     AND "FROM" AGENCY                            
         JNE   UGAC20                                                           
         CLC   TAGUCLI,FROMCLI     AND "FROM" CLIENT ARE THE PRIMARIES          
         JNE   UGAC20                                                           
         XC    ADDGAC,ADDGAC                                                    
         MVI   ADDGAC,X'FF'                                                     
         MVC   TAGUAGY,TOAGY       MAKE "TO" AGENCY/CLIENT THE PRIMARY          
         MVC   TAGUCLI,TOCLI                                                    
         J     UGAC40                                                           
                                                                                
UGAC20   TM    STATUS,COPYHIST+CLICOPY                                          
         JZ    UGAC40              OR RUNNING CLIENT COPY OR COPYING            
         CLC   TAGUCOM,TGCOM       HISTORY                                      
         JNE   UGAC40              AND THIS COMMERCIAL IS THE PRIMARY           
UGAC30   MVC   GACAGY,TAGUAGY      COMMERCIAL                                   
         MVC   GACCLI,TAGUCLI      STRIP OLD PRIMARY AGY/CLI OF PRIMARY         
         MVC   TAGUAGY,TOAGY       STATUS                                       
         MVC   TAGUCLI,TOCLI       ADD NEW AGENCY/CLIENT AS PRIMARY             
                                                                                
UGAC40   L     R3,AGACBLK          R3=A(GUARANTEE AGENCY/CLIENT TABLE)          
         BAS   RE,CLRGTBL          CLEAR TABLE                                  
         BAS   RE,BLDGTBL          AND BUILD IT                                 
                                                                                
UGAC50   CLI   0(R3),X'FF'         SEARCH THROUGH GUARANTEE'S ORIGINAL          
         JE    UGAC90              AGENCY/CLIENT BLOCK                          
                                                                                
         CLC   GACAGY,TOAGY        IF "TO" AGENCY IS FOUND                      
         JNE   UGAC80                                                           
         OC    GACCLI,GACCLI       WITHOUT A CLIENT RESTRICTION ...             
         JNZ   UGAC70                                                           
         CLI   RECNUM,PC                                                        
         JE    UGAC60                                                           
         TM    STATUS,COPYHIST+CLICOPY ... AND RUNNING CLIENT/COPY              
         JZ    XIT                         OR COPYNG HISTORY                    
         MVI   ADDGAC,X'FF'        NO NEED TO ADD AGENCY/CLIENT                 
                                                                                
         TM    STATUS,CLICOPY      IF RUNNING CLIENT COPY                       
         JZ    UGAC90              AND "TO" AGENCY IS FOUND                     
         OC    GACCLI,GACCLI       WITH NO CLIENT RESTRICTIONS                  
         JNZ   UGAC90                                                           
         XC    TAGUCLI,TAGUCLI     CLEAR THE PRIMARY CLIENT                     
         XC    GACBLKD(GACLNQ),GACBLKD   AND CLEAR ORIGINAL BLOCK               
         J     UGAC90                                                           
*                                  ... AND RUNNING PER CYCLE TRANSFER           
UGAC60   XC    TAGUCLI,TAGUCLI     CLEAR PRIMARY COMMERCIAL'S CLIENT            
         XC    GACBLKD(GACLNQ),GACBLKD                                          
         J     UGAC90                                                           
                                                                                
UGAC70   CLC   GACCLI,TOCLI        IF "TO" AGENCY AND CLIENT ARE FOUND          
         JNE   UGAC80                                                           
         CLC   GACAGY,TAGUAGY      AND AGENCY                                   
         JNE   XIT                                                              
         CLC   GACCLI,TAGUCLI      AND CLIENT MATCH THE PRIMARY COMM'L          
         JNE   UGAC80              CLEAR THEM FROM ORIGINAL BLOCK               
         XC    GACBLKD(GACLNQ),GACBLKD                                          
         J     UGAC90                                                           
                                                                                
UGAC80   LA    R3,GACLNQ(R3)       BUMP TO NEXT ORIGINAL AGENCY/CLIENT          
         J     UGAC50                                                           
         DROP  R4                                                               
                                                                                
UGAC90   MVI   ELCODE,TAVAELQ      REMOVE OLD AGENCY/CLIENT LIMITS              
         GOTO1 REMELEM                                                          
                                                                                
         BAS   RE,BLDGACEL         BUILD AGENCY/CLIENT LIMITS                   
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO CLEAR GUARANTEE'S AGENCY/CLIENT TABLE             *         
*        ON ENTRY ... R3=A(GUARANTEE'S AGENCY/CLIENT TABLE)           *         
***********************************************************************         
                                                                                
CLRGTBL  NTR1                                                                   
         LR    RE,R3               RE=(GUARANTEE AGENCY/CLIENT TABLE)           
         LR    RF,R3                                                            
         AHI   RF,GACBLKNQ         RF=(END OF GRT AGENCY/CLIENT TABLE)          
CGTBL10  XC    0(250,RE),0(RE)                                                  
         LA    RE,250(RE)                                                       
         CR    RE,RF                                                            
         JL    CGTBL10                                                          
         MVI   0(R3),X'FF'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO BUILD GUARANTEE'S AGENCY/CLIENT TABLE             *         
*        ON ENTRY ... R3=A(AGENCY/CLIENT TABLE AREA)                            
***********************************************************************         
                                                                                
         USING GACBLKD,R3                                                       
BLDGTBL  NTR1                                                                   
         USING TAVAD,R4                                                         
         L     R4,AIO              READ ALL SUBSIDIARY AGENCY/CLIENT            
         MVI   ELCODE,TAVAELQ      ELEMENTS                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
BGTBL10  BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
                                                                                
         XR    RE,RE                                                            
         ZIC   RF,TAVALEN          CALCULATE NUMBER OF CLIENTS                  
         SHI   RF,TAVALNQ          IN ELEMENT                                   
         LTR   RF,RF                                                            
         JNZ   BGTBL20                                                          
         LHI   RF,TAVALNQ                                                       
BGTBL20  D     RE,=A(L'TAVACLI)                                                 
                                                                                
         LR    R2,RF               R2=(NUMBER OF CLIENTS IN ELEMENT)            
         LA    R5,TAVACLI          RE=A(CURRENT CLIENT IN ELEMENT)              
                                                                                
BGTBL30  CLI   TAVALEN,TAVALNQ                                                  
         JE    BGTBL40                                                          
                                                                                
         MVC   GACCLI-GACBLKD(L'GACCLI,R3),0(R5)                                
                                                                                
BGTBL40  MVC   0(GACCLI-GACBLKD,R3),TAVAAGY                                     
         LA    R3,GACLNQ(R3)                                                    
         MVI   0(R3),X'FF'         ADD CURRENT CLIENT TO TABLE                  
                                                                                
         LA    R5,L'TAVACLI(R5)                                                 
         BCT   R2,BGTBL30          BUMP TO NEXT CLIENT IN ELEMENT               
         J     BGTBL10                                                          
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO BUILD GUARANTEE'S AGENCY/CLIENT LIMIT ELEMENTS    *         
***********************************************************************         
                                                                                
BLDGACEL NTR1                                                                   
         USING GACBLKD,R2                                                       
         L     R2,AGACBLK        R2=A(INITIAL TABLE)                            
         LA    R3,ADDGAC         R3=A(NEW AGENCY/CLIENT TABLE)                  
                                                                                
         USING TAVAD,R4                                                         
BGACE10  LA    R4,ELEMENT        R4=A(TAVA ELEMENT TO BUILD)                    
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAVAEL,TAVAELQ    INITIALIZE AGENCY/CLIENT LIMIT ELEMENT         
         MVI   TAVALEN,TAVALNQ                                                  
                                                                                
         LA    R5,TAVACLI        R5=A(WHERE TO SAVE NXT CLI IN ELEMENT)         
                                                                                
BGACE20  OC    GACBLKD(GACLNQ),GACBLKD                                          
         JNZ   BGACE30                                                          
         LA    R2,GACLNQ(R2)     IF AT DELETED ENTRY FROM INITIAL TABLE         
         J     BGACE20           BUMP TO NEXT INITIAL ENTRY                     
                                                                                
BGACE30  CLI   0(R2),X'FF'       IF AT THE END OF BOTH TABLES                   
         JNE   BGACE40           GO ADD THE FINAL AGY/CLI ELEMENT               
         CLI   0(R3),X'FF'                                                      
         JE    BGACE110                                                         
                                                                                
BGACE40  CLI   0(R2),X'FF'       IF AT END OF INITIAL TABLE                     
         JE    BGACE60           GO ADD FROM ADDITIONS TABLE                    
                                                                                
         CLI   0(R3),X'FF'       IF AT END OF ADDITIONS TABLE                   
         JE    BGACE50           GO ADD ENTRY FROM INITAL TABLE                 
                                                                                
         CLC   0(GACLNQ,R2),0(R3) COMPARE ENTRIES FROM INITIAL AND              
         JH    BGACE60           ADDITIONS - ADD THE ALPHA LOWER                
                                                                                
BGACE50  LR    R1,R2             SET TO ADD FROM INITIAL TABLE                  
         LA    R2,GACLNQ(R2)     AND BUMP TO NEXT INITIAL ENTRY                 
         J     BGACE70                                                          
                                                                                
BGACE60  LR    R1,R3             SET TO ADD FROM ADDITIONS TABLE                
         LA    R3,GACLNQ(R3)     AND BUMP TO NEXT ADDITIONS ENTRY               
                                                                                
BGACE70  TM    STATUS,CLICOPY    IF CLIENT COPY, FROM AGY/CLI SHOULD            
         JZ    BGACE80                                                          
         CLI   SCLFPRDH+5,0      AND NOT COPYING AT PRODUCT LEVEL               
         JNE   BGACE80           FROM AGY/CLI IS REMOVED FROM GRT               
         CLC   FROMAGY,GACAGY-GACBLKD(R1)                                       
         JNE   BGACE80                                                          
         CLC   FROMCLI,GACCLI-GACBLKD(R1)                                       
         JE    BGACE20                                                          
                                                                                
BGACE80  OC    TAVAAGY,TAVAAGY   IF ELEMENT BUILDING IN PROGRESS                
         JZ    BGACE90                                                          
         CLC   TAVAAGY,GACAGY-GACBLKD(R1)                                       
         JE    BGACE90           BUT CURRENT AGENCY DOES NOT MATCH              
         GOTO1 ADDELEM           THE ELEMENT, GO ADD THE ELEMENT                
         XC    ELEMENT,ELEMENT   AND INITIALIZE THE NEW ELEMENT                 
         MVI   TAVAEL,TAVAELQ                                                   
         MVI   TAVALEN,TAVALNQ                                                  
         LA    R5,TAVACLI                                                       
                                                                                
BGACE90  MVC   TAVAAGY,GACAGY-GACBLKD(R1)                                       
                                                                                
         OC    GACCLI-GACBLKD(L'GACCLI,R1),GACCLI-GACBLKD(R1)                   
         JZ    BGACE20                                                          
         MVC   0(L'TAVACLI,R5),GACCLI-GACBLKD(R1)                               
         ZIC   RE,TAVALEN                                                       
         AHI   RE,L'TAVACLI     IF NEW CLIENT IS BEING ADDED                    
         STC   RE,TAVALEN       ADD CLIENT AND BUMP UP ELEMENT LENGTH           
                                                                                
         CLI   TAVALEN,255      IF ELEMENT IS NOW AT MAXIMUM LENGTH             
         JL    BGACE100                                                         
         GOTO1 ADDELEM          ADD ELEMENT TO RECORD                           
         J     BGACE10          AND REINITIALIZE THE ELEMENT                    
                                                                                
BGACE100 LA    R5,L'GACCLI(R5)  BUMP TO SPOT IN ELEMENT FOR THE                 
         J     BGACE20          NEXT CLIENT                                     
         DROP  R2                                                               
                                                                                
BGACE110 OC    TAVAAGY,TAVAAGY  WHEN END OF BOTH TABLES IS REACHED              
         JZ    XIT                                                              
         GOTO1 ADDELEM          ADD THE FINAL ELEMENT                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
HEXFFS   DC    6X'FF'                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS HISTORY COMMENT FOR PERCYCLE/TRANSFER           *         
***********************************************************************         
                                                                                
ADHSTCMT NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,PC           IF RUNNING PER CYCLE TRANSFER                
         JNE   XIT                                                              
                                                                                
         MVC   PCTDATE,TGTODAY8    MOVE DATE AND STAFF INTO LITERAL             
         MVC   PCTSTAF,TGCTSTAF                                                 
                                                                                
         MVC   AIO,AIO2                                                         
                                                                                
         USING TLHCD,R3                                                         
         L     R3,AIO              BUILD HISTORY COMMENT KEY                    
         XC    0(250,R3),0(R3)                                                  
         MVI   TLHCCD,TLHCCDQ                                                   
         MVC   TLHCCOM,TOCOM                                                    
         MVI   TLHCSEQ,X'80'                                                    
         DROP  R3                                                               
                                                                                
         USING TACMD,R4                                                         
         LA    R4,ELEMENT          ADD HISTORY COMMENT ELEMENT                  
         XC    ELEMENT,ELEMENT                                                  
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMLEN,TACMLNQ+PCTMLNQ                                          
         MVI   TACMTYPE,TACMTYPH                                                
         MVC   TACMCOMM(PCTMLNQ),PCTMSG                                         
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTO1 ADDREC              ADD HISTORY COMMENT RECORD                   
         XC    WORK,WORK           AND PASSIVE POINTERS                         
         GOTO1 ADDPTRS,DMCB,(X'08',WORK),AADPBLK                                
                                                                                
         MVC   PCTAGY,FROMAGY      MOVE "FROM:" AGENCY AND                      
         MVC   PCTCID,FROMCID      COMMERCIAL ID INTO LITERAL                   
                                                                                
         USING TLHCD,R3                                                         
         L     R3,AIO              BUILD HISTORY COMMENT KEY                    
         XC    0(250,R3),0(R3)                                                  
         MVI   TLHCCD,TLHCCDQ                                                   
         MVC   TLHCCOM,TOCOM                                                    
         MVI   TLHCSEQ,X'7F'                                                    
         DROP  R3                                                               
                                                                                
         USING TACMD,R4                                                         
         LA    R4,ELEMENT          ADD HISTORY COMMENT ELEMENT                  
         XC    ELEMENT,ELEMENT                                                  
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMLEN,TACMLNQ+PCTM2LNQ                                         
         MVI   TACMTYPE,TACMTYPH                                                
         MVC   TACMCOMM(PCTM2LNQ),PCTMSG2                                       
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTO1 ADDREC              ADD HISTORY COMMENT RECORD                   
         XC    WORK,WORK           AND PASSIVE POINTERS                         
         GOTO1 ADDPTRS,DMCB,(X'08',WORK),AADPBLK                                
                                                                                
         OI    STATUS,REREAD       SET WE NEED TO RE-READ SYSIO'S KEY           
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
PCTMSG   DC    C'PERCYCLE/TRANSFER '                                            
PCTSTAF  DC    CL8'        '                                                    
         DC    CL1' '                                                           
PCTDATE  DC    CL8'        '                                                    
PCTMLNQ  EQU   *-PCTMSG                                                         
                                                                                
PCTMSG2  DC    C'TRANSFERRED FROM '                                             
PCTAGY   DC    CL6'        '                                                    
         DC    CL1'/'                                                           
PCTCID   DC    CL12'        '                                                   
PCTM2LNQ EQU   *-PCTMSG2                                                        
         EJECT                                                                  
*              OUTPUT N'CAST MEMBERS SELECTED                                   
         SPACE 1                                                                
OUTSEL   NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,PC                                                        
         JNE   OSEL10                                                           
         CLI   SCOSCST,C'Y'                                                     
         JNE   XIT                                                              
         CLI   NSEL,0                                                           
         JE    ERROSEL                                                          
         SPACE 1                                                                
OSEL10   EDIT  NSEL,(3,SCONSEL),ZERO=NOBLANK                                    
         NI    SCONSELH+1,X'F3'    TURN TO HIGH INTENSITY                       
         OI    SCONSELH+6,X'80'                                                 
         J     XIT                                                              
         SPACE 1                                                                
ERROSEL  LA    R2,SCOFCIDH                                                      
         NI    4(R2),X'FF'-X'20'                                                
         MVC   MYMSGNO,=Y(ERRSLONE)                                             
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
*              CLEAR BOTTOM SCREEN - COMMERCIAL COPY - CAST SELECT              
         SPACE 1                                                                
CLRSCR   NTR1  BASE=*,LABEL=*                                                   
         TWAXC SCOSELH,SCOLSTH,PROT=Y      CLEAR SCREEN                         
         OI    SCOTTL1H+1,X'0C'    TURN HEADINGS TO LOW INTENSITY               
         OI    SCOTTL1H+6,X'80'                                                 
         OI    SCOTTL2H+1,X'0C'                                                 
         OI    SCOTTL2H+6,X'80'                                                 
         OI    SCOTTL3H+1,X'0C'                                                 
         OI    SCOTTL3H+6,X'80'                                                 
         OI    SCONSELH+1,X'0C'                                                 
         OI    SCONSELH+6,X'80'                                                 
         J     XIT                                                              
         SPACE 2                                                                
*              CLEAR SCREEN NAMES FOR COMMERCIAL COPY                           
         SPACE 1                                                                
CLRSCR1  NTR1  BASE=*,LABEL=*                                                   
         MVC   SCOFAYN,SPACES      PRE-CLEAR NAMES                              
         OI    SCOFAYNH+6,X'80'    TRANSMIT                                     
         MVC   SCOFCIN,SPACES                                                   
         OI    SCOFCINH+6,X'80'                                                 
         MVC   SCOTAYN,SPACES                                                   
         OI    SCOTAYNH+6,X'80'                                                 
         MVC   SCOTCLN,SPACES                                                   
         OI    SCOTCLNH+6,X'80'                                                 
         MVC   SCOTPRN,SPACES                                                   
         OI    SCOTPRNH+6,X'80'                                                 
         J     XIT                                                              
         SPACE 3                                                                
*              CLEAR SCREEN NAMES FOR CLIENT COPY                               
         SPACE 1                                                                
CLRSCR2  NTR1  BASE=*,LABEL=*                                                   
         MVC   SCLFAYN,SPACES      PRE-CLEAR NAMES                              
         OI    SCLFAYNH+6,X'80'    TRANSMIT                                     
         MVC   SCLFCLN,SPACES                                                   
         OI    SCLFCLNH+6,X'80'                                                 
         MVC   SCLFPRN,SPACES                                                   
         OI    SCLFPRNH+6,X'80'                                                 
         MVC   SCLTAYN,SPACES                                                   
         OI    SCLTAYNH+6,X'80'                                                 
         MVC   SCLTCLN,SPACES                                                   
         OI    SCLTCLNH+6,X'80'                                                 
         MVC   SCLTPRN,SPACES                                                   
         OI    SCLTPRNH+6,X'80'                                                 
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADJUSTS COMMERCIAL'S VERSION RECORDS                 *         
***********************************************************************         
                                                                                
CPYVER   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
                                                                                
         USING TLVRD,R3                                                         
         LA    R3,KEY              BUILD KEY FOR "FROM COMMERCIAL'              
         XC    KEY,KEY             VERSION RECORD WITH:                         
         MVI   TLVRCD,TLVRCDQ      VERSION RECORD CODE                          
         MVC   TLVRCOM,FROMCOM     FROM INTERNAL COMMERCIAL NUMBER              
         GOTO1 HIGH                                                             
         J     CVER20                                                           
CVER10   GOTO1 SEQ                                                              
CVER20   CLC   TLVRKEY(TLVRVER-TLVRD),KEYSAVE                                   
         JNE   CVERX                                                            
         MVC   SVVRKEY,KEY                                                      
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              GET THE VERSION RECORD                       
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPBLK                                             
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      PUT NEW CID IN COMML DETAILS                 
         BRAS  RE,GETEL            ELEMENT                                      
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   TACOCLG,TOCLG                                                    
                                                                                
         CLC   TOAGY,FROMAGY       IF AGENCY IS CHANGING                        
         JE    CVER30                                                           
         XC    TACOATT,TACOATT     CLEAR ATTENTION CODE                         
         DROP  R4                                                               
                                                                                
         TM    STATUS2,DELPMREF    IF DELETING PMUSIC REFERENCES                
         JZ    CVER30                                                           
         MVI   ELCODE,TACPELQ      AND DELETE ANY PUBLISHED MUSIC               
         GOTO1 REMELEM             ELEMENTS                                     
                                                                                
CVER30   MVI   ELCODE,TAFNELQ                                                   
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTAGY))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTCLI))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTPRD))                                     
                                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     ADD AGENCY CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TOAGY                                          
         MVI   TAFNTYPE,TAFNTAGY                                                
         MVC   TAFNNAME(L'TOAGY),TOAGY                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         XC    ELEMENT,ELEMENT     ADD CLIENT CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TOCLI                                          
         MVI   TAFNTYPE,TAFNTCLI                                                
         MVC   TAFNNAME(L'TOCLI),TOCLI                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         OC    TOPRD,TOPRD                                                      
         JZ    CVER40                                                           
         XC    ELEMENT,ELEMENT     ADD PRODUCT CODE ELEMENT                     
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TOPRD                                          
         MVI   TAFNTYPE,TAFNTPRD                                                
         MVC   TAFNNAME(L'TOPRD),TOPRD                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
CVER40   GOTO1 ACTVIN,DMCB,0                                                    
                                                                                
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'08',ASVPBLK),AADPBLK                             
         OI    STATUS,REREAD       SET WE NEED TO RE-READ SYSIO'S KEY           
                                                                                
         MVC   KEY,SVVRKEY                                                      
         GOTO1 HIGH                                                             
         J     CVER10                                                           
                                                                                
CVERX    MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE FILTERS CAST RECORDS FROM SYSIO                      *         
***********************************************************************         
                                                                                
FILTER   NTR1  BASE=*,LABEL=*                                                   
         USING TARLD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TARLELQ      CAST RELEASED STATUS ELEMENT                 
         BRAS  RE,GETEL                                                         
         JNE   FILTER10                                                         
         CLI   TARLSTAT,C' '       RELEASED, IF STATUS = A,B,C,D                
         JH    NO                                                               
         DROP  R4                                                               
                                                                                
         USING TLCAD,R4                                                         
FILTER10 L     R4,TIAREC                                                        
         TM    TLCASORT,X'80'      IF PERFORMER IS A MUSICIAN                   
         JZ    YES                                                              
         TM    STATUS2,CPYMUS      REJECT IF NOT COPYING MUSIC                  
         JZ    NO                                                               
         DROP  R4                                                               
                                                                                
         BAS   RE,ELIMNV1T         REJECT IF NOT ON VERSION 1                   
         JNE   NO                                                               
                                                                                
         TM    STATUS,SELCAST      IF SELECTING CAST                            
         JZ    YES                                                              
         CLI   ENDLIST,C'Y'        AND ALL CAST HAVE ALREADY BEEN               
         JNE   NO                  DISPLAYED                                    
                                                                                
         LA    R3,SELTAB                                                        
         LA    R1,STABLNQ(R3)                                                   
FILTER20 OC    0(4,R3),0(R3)       FIND FIRST EMPTY ENTRY IN                    
         JZ    FILTER30            SELECT TABLE                                 
         LA    R3,4(R3)                                                         
         CR    R3,R1                                                            
         JNH   FILTER20                                                         
         DC    H'00'               TABLE TOO SMALL                              
                                                                                
FILTER30 MVC   0(4,R3),TIDSKADD    SAVE SELECTED D/A                            
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ELIMINATES ANY NON-VERSION 1 TRACKS FROM CAST RECORD *         
*        ON ENTRY ... TIAREC=A(CAST RECORD)                           *         
***********************************************************************         
                                                                                
ELIMNV1T NTR1                                                                   
         USING TATRD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TATRELQ      IF MUSICIAN IS ASSOCIATED TO                 
         BRAS  RE,GETEL            A TRACK                                      
         JNE   YES                                                              
                                                                                
         USING TATRTABD,R2                                                      
ENV1T10  LA    R2,TATRTAB          R2=A(TRACKS ON VERSION 1)                    
ENV1T20  CLI   0(R2),X'FF'                                                      
         JE    ENV1T40                                                          
         CLC   TATRCOM,TTCOM       SEE IF MUSICIAN'S TRACKS ARE ON              
         JNE   ENV1T30             VERSION 1                                    
         CLC   TATRTRK,TTTRK                                                    
         JE    ENV1T50                                                          
ENV1T30  LA    R2,TTLNQ(R2)                                                     
         J     ENV1T20                                                          
         DROP  R2                                                               
                                                                                
ENV1T40  MVI   TATREL,X'FF'        SET TO DELETE ANY THAT ARE NOT               
ENV1T50  BRAS  RE,NEXTEL           ON VERSION 1                                 
         JE    ENV1T10                                                          
         DROP  R4                                                               
                                                                                
         MVC   TGFULL,AIO                                                       
         MVC   AIO,TIAREC                                                       
         MVI   ELCODE,X'FF'        WHEN ALL TRACKS ARE PROCESSED,               
         GOTO1 REMELEM             CHECK IF ANY TRACKS REMAIN ON                
         MVC   AIO,TGFULL          CAST RECORD                                  
         L     R4,TIAREC           IF NOT, REJECT IT                            
         MVI   ELCODE,TATRELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS START KEY BASED ON COPY TYPE                    *         
***********************************************************************         
                                                                                
KEYINIT  NTR1  BASE=*,LABEL=*                                                   
         TM    STATUS,CLICOPY      IF THIS IS COMML COPY                        
         JO    KEYI10                                                           
         MVC   KEY,SVKEY           RE-READ FROM COMMERCIAL                      
         MVI   LENKEY,L'TLCOKEY-1                                               
         J     XIT                                                              
                                                                                
         USING TLCOD,R4                                                         
KEYI10   LA    R4,KEY                                                           
         XC    KEY,KEY             ELSE SET START KEY                           
         MVI   TLCOCD,TLCOCDQ      COMMERCIAL RECORD CODE                       
         MVC   TLCOAGY,FRCLAGY     AGENCY                                       
         MVC   TLCOCLI,FRCLCLI     CLIENT                                       
         MVI   LENKEY,TLCOPRD-TLCOD-1                                           
                                                                                
         OC    FRCLPRD,FRCLPRD                                                  
         JZ    *+14                                                             
         MVC   TLCOPRD,FRCLPRD     PRODUCT                                      
         MVI   LENKEY,TLCOCID-TLCOD-1                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE HANDLES RECORD TRACES                                *         
***********************************************************************         
                                                                                
MYTRACE  NTR1  BASE=*,LABEL=*                                                   
         TM    STATUS,TRACING                                                   
         JZ    XIT                                                              
         L     R2,0(R1)                                                         
         ZIC   R3,0(R1)                                                         
         MVC   WORK,0(R2)          MOVE LITERAL TO WORK                         
         LA    R4,WORK(R3)                                                      
         MVC   0(7,R4),=C' (AIO?)' ADD I/O AREA LITERAL                         
         LA    R3,7(R3)            BUMP L'LITERAL                               
         CLC   AIO,AIO1                                                         
         JNE   *+8                                                              
         MVI   5(R4),C'1'          SET CURRENT I/O AREA LITERAL                 
         CLC   AIO,AIO2                                                         
         JNE   *+8                                                              
         MVI   5(R4),C'2'                                                       
         CLC   AIO,AIO3                                                         
         JNE   *+8                                                              
         MVI   5(R4),C'3'                                                       
         CLC   AIO,TIAREC                                                       
         JNE   *+10                                                             
         MVC   1(6,R4),=C'TIAREC'                                               
         GOTO1 TRACE,DMCB,AIO,0,WORK,(R3)                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS DELETE PMUSIC REFERENCES STATUS                 *         
***********************************************************************         
                                                                                
SETPMUS  NTR1  BASE=*,LABEL=*                                                   
         USING TLMUD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLMUCD,TLMUCDQ      IF THE FIRST PMUSIC RECORD ON FILE           
         GOTO1 HIGH                HAS AGENCY POPULATED                         
         CLI   KEY,TLMUCDQ         SET TO DELETE ALL PMUSIC REFERENCES          
         JNE   XIT                                                              
         OC    TLMUAGY,TLMUAGY                                                  
         JZ    XIT                                                              
         OI    STATUS2,DELPMREF                                                 
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SET NEXT CAST SEQUENCE NUMBER FOR COMMERCIAL         *         
*        ON ENTRY ... AIO=A(COMMERCIAL RECORD)                        *         
***********************************************************************         
                                                                                
SETNXTC  NTR1  BASE=*,LABEL=*                                                   
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSEQ))                                     
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R4,TGELEM                                                        
         MVC   TANUNXTC,CASTSEQ                                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE GETS NEW INTERNAL COMMERCIAL NUMBER                  *         
***********************************************************************         
                                                                                
GETCOM   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'B4',0)  GET SYSTEM REC FOR UPDATE         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TASYD,R4            R4=A(SYSTEM DETAILS ELEMENT)                 
                                                                                
         ICM   R1,15,TASYLCOM      GET NEXT INTERNAL COMMERCIAL NUMBER          
         LA    R1,1(R1)                                                         
         STCM  R1,15,TASYLCOM                                                   
                                                                                
         STCM  R1,15,TOCOM         SAVE FOR COPY                                
                                                                                
         GOTO1 PUTREC              WRITE BACK SYSTEM RECORD                     
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET TO CLIENT GROUP FROM CLIENT RECORD                
         SPACE 1                                                                
GETCLG   NTR1  BASE=*,LABEL=*                                                   
         XC    CGROUP,CGROUP                                                    
         L     R4,AIO              R4=A(CLIENT RECORD)                          
         MVI   ELCODE,TACIELQ                                                   
         USING TACID,R4                                                         
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   CGROUP,TACICLG                                                   
         J     XIT                                                              
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAMQHFR                                                        
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
LINED    DSECT                                                                  
         DS    CL21                                                             
LLHS     DS    CL1                                                              
         DS    CL3                                                              
LFPRD    DS    CL6                 FROM PRODUCT                                 
         DS    CL3                                                              
LCOL1    DS    CL1                                                              
         DS    CL3                                                              
LFPRDN   DS    CL36                FROM PRODUCT NAME                            
         DS    CL3                                                              
LCOL2    DS    CL1                                                              
         DS    CL3                                                              
LFCID    DS    CL12                COMMERCIAL ID                                
         DS    CL3                                                              
LRHS     DS    CL1                                                              
         SPACE 1                                                                
         ORG   LINED                                                            
         DS    CL48                                                             
L2LHS    DS    CL1                                                              
         DS    CL3                                                              
L2FCID   DS    CL12                COMMERCIAL ID                                
         DS    CL3                                                              
L2RHS    DS    CL1                                                              
         SPACE 1                                                                
         ORG   LINED                                                            
         DS    CL36                                                             
L3LHS    DS    CL1                                                              
L3EST    DS    CL20                ESTIMATE CODE                                
L3COL1   DS    CL1                                                              
L3ESTN   DS    CL36                ESTIMATE NAME                                
L3RHS    DS    CL1                                                              
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 1                                                                
LLINED   DSECT                                                                  
CASSN    DS    CL9                 SS NUMBER                                    
         DS    CL1                                                              
CALNAME  DS    CL16                PERFORMER NAME                               
         DS    CL1                                                              
CACAT    DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
CACAM    DS    CL3                 ON/OFF CAMERA                                
         DS    CL1                                                              
CATAX    DS    CL3                 TAX UNIT                                     
         DS    CL1                                                              
CAAGNT   DS    CL4                 AGENT                                        
         DS    CL1                                                              
CAUNI    DS    CL3                 UNION                                        
         DS    CL1                                                              
CALCL    DS    CL3                 LOCAL                                        
         DS    CL1                                                              
CAYR     DS    CL2                 CONTRACT YEAR                                
         DS    CL1                                                              
CALIFT   DS    CL1                 LIFT                                         
         DS    CL2                                                              
CACORP   DS    CL1                 CORP CODE                                    
         DS    CL2                                                              
CAGUA    DS    CL4                 GUARANTEE                                    
         EJECT                                                                  
*              DSECT TO COVER TEMPORARY STORAGE                                 
         SPACE 1                                                                
TMPD     DSECT                                                                  
SVPTRBLK DS    CL((320*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS                      
ADPTRBLK DS    CL((320*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS FOR ADDPTRS          
TMPLNQ   EQU   *-TMPD                                                           
         SPACE 3                                                                
*              DSECT TO COVER LOCAL W/S IN TWAHOLE                              
         SPACE 1                                                                
LCLWSD   DSECT                                                                  
LOCALD   DS    0D                                                               
*                                                                               
STATUS   DS    XL1                 STATUS - SEE EQUATES                         
PFKPEND  EQU   X'80'               PFKEY PENDING                                
REREAD   EQU   X'40'               NEED TO RE-READ SYSIO'S KEY                  
CLICOPY  EQU   X'20'               COPY ENTIRE CLIENT                           
COPYCAST EQU   X'10'               COPY CAST RECORDS TOO                        
COPYHIST EQU   X'08'               COPY HISTORY RECORDS TOO                     
TRACING  EQU   X'04'               TRACE ACTIVE                                 
SELCAST  EQU   X'02'               SELECT CAST                                  
COPYCMT  EQU   X'01'               COPY COMMENT RECORD                          
*                                                                               
STATUS2  DS    XL1                 STATUS - SEE EQUATES                         
CPYMUS   EQU   X'80'               COPY MUSIC TOO                               
DELPMREF EQU   X'40'               DELETE PMUSIC REFERENCES                     
*                                                                               
LENKEY   DS    XL1                                                              
CASTSEQ  DS    H                                                                
COMLCTR  DS    XL2                 COMMERCIAL COUNTER                           
ESTCTR   DS    XL2                 ESTIMATE COUNTER                             
ATHSCLI  DS    A                   A(CURRENT EST. CLIENT EL.)                   
COMDA    DS    XL4                 NEW COMMERCIAL D/A                           
SVKEY    DS    CL(L'TLDRREC)                                                    
SVCAKEY  DS    CL(L'TLCAKEY)                                                    
PCEKEY   DS    CL(L'TLDRREC)                                                    
VERSION  DS    C                                                                
CCMTPASS DS    C                                                                
         SPACE 1                                                                
*              COMMERCIAL COPY FIELDS                                           
         SPACE 1                                                                
FROMAGY  DS    CL(L'TGAGY)         FROM AGENCY                                  
FROMCID  DS    XL(L'TGCID)         FROM COMMERCIAL ID                           
FROMCOM  DS    XL(L'TGCOM)         FROM INTERNAL COMMERCIAL NUMBER              
FROMCLI  DS    XL(L'TGCLI)         FROM CLIENT                                  
FROMPRD  DS    XL(L'TGPRD)         FROM PRODUCT                                 
FROMCLG  DS    XL(L'TGCLG)         FROM CLIENT GROUP                            
FROMWEB  DS    CL1                 FROM WEB APPLICATION?                        
FROMCOTY DS    XL(L'TACOTYPE)      FROM COMMERCIAL TYPE                         
TATRTAB  DS    XL(7*TTLNQ)         FROM TRACK TABLE                             
TOAGY    DS    CL(L'TGAGY)         TO AGENCY                                    
TOCID    DS    CL(L'TGCID)         TO COMMERCIAL ID                             
TOCOM    DS    XL(L'TGCOM)         TO INTERNAL COMMERCIAL NUMBER                
TOCLI    DS    XL(L'TGCLI)         TO 'NEW' CLIENT                              
TOPRD    DS    XL(L'TGPRD)         TO 'NEW' PRODUCT                             
TOCLG    DS    XL(L'TGCLG)         TO CLIENT GROUP                              
         SPACE 1                                                                
*              CLIENT COPY FIELDS                                               
         SPACE 1                                                                
FRCLAGY  DS    CL(L'TGAGY)         FROM AGENCY                                  
FRCLAGN  DS    CL36                     NAME                                    
FRCLCLI  DS    XL(L'TGCLI)         FROM CLIENT                                  
FRCLCLN  DS    CL36                     NAME                                    
FRCLPRD  DS    XL(L'TGPRD)         FROM PRODUCT                                 
FRCLPRN  DS    CL36                     NAME                                    
FRCL     EQU   *-FRCLAGY                                                        
         SPACE 1                                                                
TOCLAGY  DS    CL(L'TGAGY)         TO 'NEW' AGENCY                              
TOCLAGN  DS    CL36                     NAME                                    
TOCLCLI  DS    XL(L'TGCLI)         TO 'NEW' CLIENT                              
TOCLCLN  DS    CL36                     NAME                                    
TOCLPRD  DS    XL(L'TGPRD)         TO 'NEW' PRODUCT                             
TOCLPRN  DS    CL36                     NAME                                    
TOCLCLG  DS    XL(L'TGCLG)         TO 'NEW' CLIENT GROUP                        
         SPACE 1                                                                
LASTPRD  DS    CL6                 LAST PRODUCT READ                            
PRDNHEDH DS    CL8                 PRODUCT NAME + HEADER                        
PRDNHEAD DS    CL36                                                             
         SPACE 1                                                                
CGROUP   DS    CL6                 CLIENT GROUP                                 
COUNTER  DS    X                   COUNTER - LINES PER PAGE                     
MYNAMEH  DS    CL8                 FAKE HEADER                                  
MYNAME   DS    CL16                                                             
APLINE   DS    A                   A(NEXT LINE TO LIST)                         
DATABLE  DS    9F                  D/A TABLE                                    
CONTKEY  DS    XL(L'KEY)           SAVED KEY FOR PAGING SEL CAST                
         SPACE 1                                                                
AYAYSTA6 DS    X                   AGENCY 6TH STATUS                            
AYBRSTAT DS    X                   AGENCY BILLING STATUS                        
FAYSTA6  DS    X                                                                
FCISTA2  DS    X                                                                
TCISTA2  DS    X                                                                
LOCALLNQ EQU   *-LOCALD                                                         
         SPACE 1                                                                
*        THIS STORAGE SHOULDN'T BE CLEARED EACH TIME                            
         SPACE 1                                                                
NSEL     DS    X                   NUMBER OF CAST MEMBERS SELECTED              
ENDLIST  DS    C                   FLAG FOR VKEY - END OF LIST                  
LISTED   DS    C                   CAST LISTED ALREADY                          
LSTPAGE  DS    C                   SELECT FROM LAT PAGE                         
NUMANN   DS    F                   N'ANNOUNCERS                                 
NUMNONM  DS    F                   N'NON MUSICIANS                              
NUMLIFT  DS    F                   N'PERFORMERS ON LIFT                         
AGACBLK  DS    A                   A(GUARANTEE ORIGINAL AGY/CLI BLOCK)          
         ORG   AGACBLK                                                          
ASVPBLK  DS    A                   A(SAVED PTR BLOCK)                           
AADPBLK  DS    A                   A(ADDED PTR BLOCK)                           
SVCOKEY  DS    CL(L'KEY)           SAVED COMMERCIAL KEY                         
SVVRKEY  DS    CL(L'KEY)           SAVED VERSION KEY                            
PCYSTAT  DS    X                   PERCYCLE/TRANSFER STATUS                     
PCSELIG  EQU   X'80'               ELIGIBLE CAST FOUND                          
PCSPRIC  EQU   X'40'               PRIMARY COMM'L FOR AT LEAST 1 GUAR           
PCSESUB  EQU   X'20'               GRT HAS ELIGIBLE SUBSIDIARIES                
PCSNSUB  EQU   X'10'               GRT HAS NO ACTIVE SUBSIDIARIES               
EPCYSTRT DS    XL(L'TAGCSTRT)      EARLIEST PER CYCLE PAYMENT ST.DATE           
LPCYSTRT DS    XL(L'TAGCSTRT)      LAST PER CYCLE PAYMENT START DATE            
LFTRSTRT DS    XL(L'TACRSTRT)      LAST FTRACK START DATE                       
ADDGAC   DS    XL(GACLNQ+1)        NEW AGENCY/CLIENT LIMITATION                 
SELTAB   DS    300F                SELECTED D/A TABLE                           
STABLNQ  EQU   *-SELTAB                                                         
         SPACE 1                                                                
LCLWSLNQ EQU   *-LOCALD                                                         
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR8FD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR8ED                                                       
         ORG   SCOWORK                                                          
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA     (MUST FOLLOW LAST SCREEN)                                        
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**********************************************************************          
*        DSECT FOR GUARANTEE'S AGENCY/CLIENT TABLE                   *          
**********************************************************************          
                                                                                
GACBLKD  DSECT                                                                  
GACAGY   DS    CL6                                                              
GACCLI   DS    CL6                                                              
GACLNQ   EQU   *-GACBLKD                                                        
GACBLKNQ EQU   (GACLNQ*100)+1                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097TAGEN8F   05/29/15'                                      
         END                                                                    
