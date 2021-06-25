*          DATA SET SPTRA61    AT LEVEL 161 AS OF 07/15/16                      
*PHASE T21661B                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE ENDOFMOD                                                               
         TITLE 'T21661 NETWORK PATTERN REC DIS/CHG/ADD/DEL/LIST'                
                                                                                
*********************************************************                       
* AS OF NOV/08 - COMMERCIALS LIVE ON SPTDIR AS 0A21 RECORDS                     
*                PATTERNS    LIVE ON XSPDIR AS 0A61 RECORDS                     
*********************************************************                       
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - USED IN VCML TO READ COMMERCIAL RECS                       
*                    ALSO IN DCML TO CHECK FOR DELETED COMMERCIAL RECS          
*                    READ COMMERCIAL RECORD IN PGROUP FTR ROUTINE               
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE REG                                                   
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
*                                                                     *         
*  LEV 66 MNAS 29SEP06 FIX FILE UPDATING WHEN ERRORS EXIST            *         
*  LEV 66 MNAS 21NOV06 FIX UNIT PASSIVES WITH ZERO DISC ADDRESSES     *         
*  LEV 67 SMUR 10AUG07 FIX UNABLE TO DEL 1ST CML RECORD ON PATTERN    *         
*  LEV 68 SMUR 19OCT07 POINT TO XSPOT AFTER VALICLT FOR MORE BRNDS CLT*         
*  LEV 69 SMUR 24OCT07 FIX SEQ READ WHEN ALL CLIENT REQUEST           *         
*  LEV 70 SMUR 14MAR08 FIX P/B BUG                                    *         
*  LEV 71 MNAS 07JUL08 NEW TRAFFIC TYPE ON MASTER RECORD              *         
*  LEV141 SMUR 26MAR10 FIX TO STOP DUPLICATE PASSIVE KEYS             *         
*  LEV142 MHER ???     TIME ON PATTERN                                *         
*  LEV143 SMUR   SEP10 FIX ADID ON *DEL* AND *TIM* MESSAGE            *         
*  LEV150 MNAS   OCT11 FIX BUG ON DEL/RESTORE COMBINATION THAT WILL   *         
*              ALLOW THE RESTORED PATTERN TO BE A DUPLICATE           *         
*  LEV155 MNAS 11OCT13 PROFILE TO INCLUDE/EXCLUDE DIGITAL             *         
*  LEV157 SMUR 09JAN14 ALLOW SAME PRODUCT PIGGYBACKS                  *         
*  LEV158 SMUR 11JAN16 TURN OFF PAT CHANGED IN OPTICA FLAG            *         
*                      FIX VALIDATION FOR COMML W/NETWORK RESTRICTION *         
*  LEV159 SCHT 23JUN16 FIX DOWNLOAD                                   *         
*  LEV160 SMUR 15JUL16 FIX COMMERCIAL DELETE LOGIC                    *         
*                                                                     *         
*                     ********TERMINUS********                        *         
***********************************************************************         
         TITLE 'T21661 NETWORK PATTERN REC DISPLAY/CHANGE/ADD/DELETE'           
T21661   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NPAT**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         LHI   RE,SVNETLST-T216FFD                                              
         AR    RE,RA               POINT TO NETWORK LIST SAVE AREA              
         ST    RE,ASVNET                                                        
         LHI   RE,SVOLDLST-T216FFD                                              
         AR    RE,RA                                                            
         ST    RE,ASVOLD                                                        
*                                                                               
         CLI   TRLSTPGM,X'61'                                                   
         BE    *+12                                                             
         NI    CONRECH+4,X'DF'                                                  
         MVI   TRLSTPGM,X'61'                                                   
*                                                                               
         TM    CONRECH+4,X'20'     TEST REC FIELD HAS CHANGED                   
         BO    PAT02               NO - SO BEEN HERE BEFORE                     
                                                                                
         OI    CONRECH+4,X'20'     SET FLAG TO DETECT CHANGE                    
         LA    R0,CLRSTART         AND CLEAR LOCAL STORAGE                      
         LHI   R1,CLREND-CLRSTART                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
PAT02    ST    R2,SPTR61RR                                                      
         CLI   MYSCREEN,0                                                       
         BNE   *+8                                                              
         MVI   MYSCREEN,X'81'      INITIALIZE MYSCREEN                          
*                                                                               
         MVC   TRACEIT+6(1),MODE                                                
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',TRACEIT                        
*                                                                               
         XC    DLCB,DLCB           CLEAR DLCB                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         L     R0,=V(DLFLD)                                                     
         A     R0,SPTR61RR                                                      
         ST    R0,VDLFLD                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   PAT06                                                            
*                                                                               
         CLI   MYSCREEN,X'81'      TEST BASE SCREEN                             
         BE    PAT04               YES                                          
         CLI   ACTNUM,ACTADD       ACTION ADD MUST BE BASE SCR                  
         BNE   PAT04                                                            
*                                                                               
         MVI   MYSCREEN,X'81'                                                   
         BRAS  RE,GETSCR                                                        
*                                                                               
PAT04    CLI   WHEN,X'20'      SOON?                                            
         JNE   PAT05                                                            
         CLC   =C'DOWN',CONOUT                                                  
         JNE   PAT05                                                            
         MVC   QCRDCODE,=C'TH'                                                  
*                                                                               
PAT05    BRAS  RE,VK                                                            
         LA    R2,TRADESCH                                                      
         OI    6(R2),X'40'         SET CURSOR POSN                              
         J     EXIT                                                             
*                                                                               
PAT06    CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
PATDR    BRAS  RE,DR                                                            
         J     EXIT                                                             
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   PAT10                                                            
*                                                                               
PATVR    DS    0H                                                               
         OC    MYSUBLIN,MYSUBLIN   TEST VERSIONING ACTIVE                       
         BNZ   PATDR               IF YES, CAN'T CHANGE RECORD                  
         BRAS  RE,VREC                                                          
         CLI   ACTNUM,ACTADD                                                    
         JE    EXIT                                                             
         B     PATDR                                                            
*                                                                               
PAT10    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
*                                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BNE   *+12                                                             
         BRAS  RE,LISTR                                                         
         J     EXIT                                                             
*                                                                               
         CLI   MODE,PRINTREP       REPORT                                       
         BNE   *+12                                                             
         BRAS  RE,PR                                                            
         J     EXIT                                                             
*                                                                               
         CLI   MODE,RECDEL         BEFORE DELETE RECORD (INVALID PAT)           
         BNE   PAT12                                                            
         MVI   ERROR,INVACT        DELETE IS INVALID                            
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         B     TRAPERR                                                          
*                                                                               
PAT12    CLI   MODE,XRECADD        AFTER ADD RECORD                             
         BNE   PAT14                                                            
         BRAS  RE,XAAREC           YES, ADD RECORD TO XFILE                     
         B     PATDR               AND THEN REDISPLAY                           
*                                                                               
PAT14    CLI   MODE,SETFILE        SET FILES FOR XFILE                          
         BNE   *+12                                                             
         BRAS  RE,INITXSP          SWITCH TO XFILE                              
         B     EXIT                                                             
                                                                                
*=============================================================                  
* CODE BELOW IS NEEDED BECAUSE OF PASSIVE LIST POINTERS                         
* LAST KEY ON SCREEN IS SAVED AND RESTORED HERE                                 
* OTHERWISE DISPLAY BEGINS AT THE RECORD LAST DISPLAYED                         
* AND THAT JUST DOESN'T WORK                                                    
*=============================================================                  
                                                                                
         CLI   MODE,LISTKEY        TEST SET KEY FOR LIST                        
         BNE   PAT16                                                            
         OC    SVMYKEY,SVMYKEY     TEST HAVE A KEY                              
         BZ    PAT16                                                            
         MVC   KEY,SVMYKEY                                                      
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
PAT16    CLI   MODE,PROCPFK                                                     
         BNE   EXIT                                                             
*                                                                               
         LR    RE,RA                GET TWA ADDRESS                             
         AHI   RE,THISLSEL-T216FFD  POINT TO FIELD                              
         CLI   0(RE),C'C'           TEST THIS IS CHANGE ACTION                  
         BNE   PATDR                                                            
         MVC   KEY(32),TWAKEYSV                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         B     PATVR                                                            
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
TRACEIT  DC    X'07',C'MODE= '                                                  
         EJECT                                                                  
*============================================================                   
* DISPLAY KEY ROUTINE                                                           
*============================================================                   
                                                                                
DK       L     R4,AIO                                                           
         USING NPTXKEY,R4                                                       
         MVC   SVXNKEY,0(R4)       SAVE THIS RECORD KEY                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,NPTXR3F                                                     
         ST    RE,BREFK                                                         
         ST    RE,BREF                                                          
         XC    BREF,=X'00FFFFFF'                                                
*                                                                               
         CLC   TWAKEYSV(32),0(R4)  TEST SAME RECORD                             
         BE    DK01                                                             
         MVI   MYSCREEN,X'81'      RESTORE PATTRN SCREEN                        
         BRAS  RE,GETSCR                                                        
         BRAS  RE,GETVRCNT                                                      
*                                                                               
DK01     MVC   BAGYMD,NPTXAM                                                    
         MVC   KEY(32),NPTXKEY                                                  
         CLC   BCLT,NPTXCLT        CLIENT CHANGE                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   QPRD,NPTXPRD         PRD                                         
         MVC   BSLN,NPTXSLN         SLN                                         
         MVC   QPRD2,NPTXPRD2       PRD2                                        
         MVC   BSLN2,NPTXSLN2       SLN2                                        
*                                                                               
         MVC   SVPROG,NPTXPROG                                                  
         CLC   NETWORK,NPTXNET                                                  
         BE    DK04                                                             
         CLI   NPTXNET,X'00'       TEST FOR PATTERN BY MEDIA                    
         BNE   DK02                                                             
         CLI   NPTXNET+2,X'FF'                                                  
         BNE   DK02                                                             
         MVC   NETWORK,NPTXNET                                                  
         B     DK04                                                             
*                                                                               
DK02     XC    NETWORK,NETWORK                                                  
         MVI   NETWORK,C'$'                                                     
         CLI   NPTXNET,C'$'        TEST NETWORK LIST                            
         BE    DK04                                                             
         DROP  R4                                                               
*                                                                               
         MVI   ELEM,C'0'                                                        
         MVC   ELEM+1(16),ELEM      PRE-FILL THE KEY WITH ZEROES                
         LA    R4,ELEM                                                          
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),KEY+NPTXNET-NPTXKEY                                  
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         CLI   STAKCALL,C'$'                                                    
         BE    DK04                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',ELEM,AIO3                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO3                                                          
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,NETMKT                                                        
*                                                                               
         L     R4,AIO                                                           
         MVC   NETWORK,KEY+NPTXNET-NPTXKEY                                      
         MVC   SVPROG,KEY+NPTXPROG-NPTXKEY                                      
         B     DK04                                                             
         DROP  R4                                                               
*                                                                               
         USING NPTXKEY,R4                                                       
DK04     MVC   PROGRAM,SVPROG                                                   
         MVI   QDPT,0                                                           
         XC    QDPT2,QDPT2                                                      
*                                                                               
         CLI   SVPROG,X'FF'        THIS DAYPART                                 
         BNE   DK06                                                             
         XC    PROGRAM,PROGRAM                                                  
         MVC   FEED,SVPROG+2                                                    
*                                                                               
         MVC   QDPT,SVPROG+1                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
*                                                                               
         OC    NPTDPART,NPTDPART   ANY DAYPART                                  
         BZ    DK06                                                             
*                                                                               
         MVC   QDPT2,NPTDPART      DAYPART CODE                                 
*                                                                               
DK06     MVC   TRACLT(3),QCLT                                                   
         OI    TRACLTH+6,X'80'                                                  
                                                                                
* DISPLAY NETWORK *                                                             
                                                                                
         CLI   NETWORK,0           TEST FOR PATTERN BY MEDIA                    
         BNE   DK08                                                             
         CLI   NETWORK+2,X'FF'                                                  
         BNE   DK08                                                             
         MVC   TRANET(2),=C'M='                                                 
         MVC   TRANET+2(1),NETWORK+1                                            
         B     DK10                                                             
*                                                                               
DK08     MVC   TRANET,NETWORK                                                   
*                                                                               
DK10     OI    TRANETH+6,X'80'                                                  
                                                                                
* DISPLAY PROGRAM OR CODE FIELDS *                                              
                                                                                
         XC    TRACODE,TRACODE                                                  
         MVC   TRACODE(1),QDPT                                                  
         OC    QDPT2,QDPT2         DAYPART                                      
         BZ    *+10                                                             
         MVC   TRACODE(2),QDPT2                                                 
         OI    TRACODEH+6,X'80'                                                 
*                                                                               
         MVC   TRAPROG,PROGRAM                                                  
         OI    TRAPROGH+6,X'80'                                                 
*                                                                               
         MVC   TRAFEED,FEED                                                     
         OI    TRAFEEDH+6,X'80'                                                 
*                                                                               
         XC    FLD,FLD                                                          
         MVC   FLD(3),NPTXPRD                                                   
         LA    R3,NPTXSLN                                                       
         BRAS  RE,FMTSLN           PRINT SLN                                    
         MVC   TRAPRLN,FLD                                                      
         OI    TRAPRLNH+6,X'80'                                                 
*                                                                               
         MVC   TRAPTLN,SPACES                                                   
         OI    TRAPTLNH+6,X'80'                                                 
*                                                                               
         OC    NPTXPRD2,NPTXPRD2                                                
         BZ    DK16                                                             
*                                                                               
DK14     XC    FLD,FLD                                                          
         MVC   FLD(3),NPTXPRD2                                                  
         LA    R3,NPTXSLN2                                                      
         BRAS  RE,FMTSLN                                                        
         MVC   TRAPTLN,FLD                                                      
*                                                                               
DK16     SR    R0,R0                                                            
         ICM   R0,7,KEY+NPTXR3F-NPTXKEY                                         
         X     R0,=X'00FFFFFF'                                                  
         ST    R0,BREF                                                          
         EDIT  (R0),(5,TRAREF),ALIGN=LEFT                                       
         OI    TRAREFH+6,X'80'                                                  
                                                                                
* PRINT OUT ANY FILTERS                                                         
                                                                                
         XC    ELEM,ELEM                                                        
         OC    FILTERS,FILTERS     ANY FILTERS                                  
         BZ    DK46                NO                                           
*                                                                               
         LA    R3,ELEM             OUTPUT AREA                                  
         LR    R4,R3               COMPARAND                                    
*                                                                               
         CLI   DATEFTR1,0          DATE FILTER                                  
         BE    DK20                                                             
         MVI   0(R3),C'D'                                                       
         CLI   DATESFTR,0          ANY GREATER/LESS THAN                        
         BE    *+14                NO                                           
         MVC   1(1,R3),DATESFTR                                                 
         LA    R3,1(,R3)                                                        
         MVI   1(R3),C'='                                                       
         GOTO1 DATCON,DMCB,(3,DATEFTR1),(5,2(R3))                               
         LA    R3,10(,R3)                                                       
*                                                                               
         CLI   DATEFTR2,0          SECOND DATE FILTER                           
         BE    DK20                                                             
         MVI   0(R3),C'-'                                                       
         GOTO1 (RF),(R1),(3,DATEFTR2),(5,1(R3))                                 
         LA    R3,9(,R3)                                                        
*                                                                               
DK20     TM    FLAGFTR,FLAGDEL     DELETED RECS FILTER                          
         BZ    DK30                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(3,R3),=C'DEL'                                                  
         LA    R3,3(R3)                                                         
*                                                                               
DK30     OC    CMLFTR,CMLFTR       ANY CML FILTERS                              
         BZ    DK40                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         MVC   0(2,R3),=CL3'CML'                                                
         MVI   2(R3),C'='                                                       
         MVC   3(8,R3),CMLFTR                                                   
         CLI   CMLFTRADI,C'Y'      IS IT AN ADID                                
         BNE   DK32                                                             
         GOTO1 VTRPACK,DMCB,(C'U',CMLFTR),3(R3)                                 
*                                                                               
DK32     LA    R3,14(R3)                                                        
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         LA    R3,2(R3)                                                         
*                                                                               
DK40     OC    PERFTR,PERFTR       PERIOD FILTER                                
         BZ    DK42                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'P'                                                       
         MVI   1(R3),C'='                                                       
         LA    R2,PERFTR                                                        
         BRAS  RE,PPER                                                          
         MVC   2(17,R3),WORK                                                    
         LA    R3,19(R3)                                                        
*                                                                               
DK42     TM    FLAGFTR,X'40'       WAS SORT ENTERED                             
         BZ    DK46                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(8,R3),=C'SORT=REF'                                             
         TM    FLAGFTR,X'20'                                                    
         BZ    DK44                                                             
         MVC   5(4,R3),=C'DATE'                                                 
         LA    R3,1(R3)                                                         
*                                                                               
DK44     LA    R3,8(R3)                                                         
*                                                                               
DK46     MVC   TRAFLTR,ELEM                                                     
         OI    TRAFLTRH+6,X'80'                                                 
         J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*====================================================================           
* READ THROUGH PATTERNS TO GET VERSION COUNT                                    
*====================================================================           
                                                                                
GETVRCNT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    MYSUBLIN,MYSUBLIN   CLEAR VERSION                                
         XC    SVADDWHN,SVADDWHN   CLEAR RECORD ADDED DATE                      
*                                                                               
         BRAS  RE,INITXSP          SET FOR XFILE                                
         SR    R4,R4               COUNT AVAILABLE VERSIONS                     
         XC    KEY,KEY                                                          
         MVC   KEY(23),SVXNKEY     MOVE ALL BUT REFNUM                          
         GOTO1 HIGH                                                             
*                                                                               
GETVR2   CLC   KEY(23),KEYSAVE                                                  
         BNE   GETVRX                                                           
*                                                                               
         LA    RE,KEY+NPTXR3F-NPTXKEY   POINT TO REFNUM                         
         CLI   0(RE),X'A0'              TEST SAVED VERSION                      
         BH    GETVR4                   NO                                      
*                                                                               
         LA    RE,KEY+NPTXOR3G-NPTXKEY  POINT TO OLD REFNUM                     
         OC    0(3,RE),0(RE)                                                    
         BZ    GETVR6                   MUST BE PRE-VERSIONING                  
*                                                                               
GETVR4   CLC   0(3,RE),BREFK+1          TEST SAME PATTERN                       
         BNE   GETVR6                                                           
*                                                                               
         LA    R4,1(R4)            BUMP NUMBER OF VERSIONS                      
         OC    SVADDWHN,SVADDWHN   TEST HAVE ADD DATE ALREADY                   
         BNZ   GETVR6                                                           
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETVR6                                                           
*                                                                               
         LA    RE,ACTVADDT-ACTVD(R6)                                            
         MVC   SVADDWHN,0(RE)      SAVE ORIGINAL ADD DATE                       
*                                                                               
GETVR6   GOTO1 SEQ                                                              
         B     GETVR2                                                           
*                                                                               
GETVRX   STH   R4,LATEST           SET VERSION COUNT                            
         MVC   KEY,SVXNKEY         AND RESTORE KEY                              
         GOTO1 HIGH                RESTORE XSPDIR                               
         GOTO1 GETREC              AND XSPFIL                                   
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*====================================================                           
* REPORT - ONLINE, SOON, OV                                                     
*====================================================                           
                                                                                
PR       NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,INITXSP          SET FOR XFILE                                
         MVC   AIO,AIO1                                                         
         CLC   =C'DOWN',CONOUT     SUPPRESS DATE SEQ FOR DOWNLOAD               
         BNE   PR02                                                             
         NI    FLAGFTR,X'FF'-X'20'                                              
*                                                                               
PR02     XC    SORTCNT,SORTCNT                                                  
         MVI   CMLPRT,0            INIT CMLS PRINTED ON ERROR REPORT            
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
* 'WHEN' VALUES ARE X40=NOW X20=SOON X10=OV                                     
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    PR04                                                             
*                                                                               
         XC    DMCB(24),DMCB       ONLINE STORAGE ALLOCATION                    
         L     RE,=V(ENDOFMOD)                                                  
         A     RE,SPTR61RR                                                      
         ST    RE,DMCB                                                          
         MVC   DMCB+4(4),=X'D902168C'  LOAD T2168C                              
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,0(R1)            GET PHASE ADDRESS                            
         LA    RE,16(RE)           CAN USE FROM +16                             
         MVC   0(8,RE),=C'SORTAREA'                                             
         LA    RE,8(RE)                                                         
*                                                                               
         ST    RE,STRTSORT                                                      
         ST    RE,NEXTSORT                                                      
         AHI   RE,12000                                                         
         ST    RE,NEXTMAX                                                       
         B     PR06                                                             
*                                                                               
PR04     OC    STRTSORT,STRTSORT   INITIALIZE ONCE ONLY                         
         BNZ   PR06                                                             
         L     R0,SORTSIZE                                                      
         GETMAIN R,LV=(0),LOC=(ANY,ANY)                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,STRTSORT                                                      
         ST    R1,NEXTSORT                                                      
         A     R1,SORTSIZE                                                      
         ST    R1,NEXTMAX                                                       
         B     PR06                                                             
SORTSIZE DC    F'800000'                                                        
*                                                                               
PR06     LAY   RE,HEADING                                                       
         ST    RE,SPECS                                                         
         LAY   RE,HDHK                                                          
         ST    RE,HEADHOOK                                                      
*                                                                               
PR10     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPTXKEY,R4                                                       
*                                                                               
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM,BAGYMD      A-M                                           
         OC    BCLT,BCLT                                                        
         BNZ   PR12                                                             
         GOTO1 HIGH               READ FIRST CLIENT ON FILE                     
         MVC   SVXNKEY,KEY                                                      
         B     PR42                                                             
*                                                                               
PR12     MVC   NPTXCLT,BCLT       CLT                                           
*                                                                               
         OC    SVSTAGRP,SVSTAGRP   DO NOT PUT STAGRP IN KEY                     
         BNZ   PR14                                                             
         CLC   TRANET,SPACES                                                    
         BNH   PR14                                                             
         MVC   NPTXNET,TRANET                                                   
*                                                                               
         CLC   TRAPROG,SPACES                                                   
         BNH   PR14                                                             
         MVC   NPTXPROG,TRAPROG                                                 
*                                                                               
PR14     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     PR16                                                             
*                                                                               
PRSEQ    MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PR16     CLC   KEY(3),KEYSAVE      SAME TYPE/A-M                                
         BNE   PR30                                                             
*                                                                               
         OC    KEY+5(27),KEY+5     THIS A PTN SEQ REC                           
         BZ    PRSEQ                                                            
*                                                                               
         CLI   KEY+23,X'A0'        THIS SAVED PTN                               
         BL    PRSEQ                                                            
*                                                                               
         CLC   KEY(5),KEYSAVE      TYPE/A-M/CLT                                 
         BNE   PR30                                                             
*                                                                               
         BRAS  RE,FLIST            FILTER AGAINST KEY FIELDS                    
         BNE   PRSEQ                                                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         TM    NPTSTAT,NPTS_ADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         BRAS  RE,FTR              FILTER AGAINST DATES, DELETED, ETC           
         BNE   PRSEQ                                                            
*                                                                               
         CLC   =C'DOWN',CONOUT                                                  
         BNE   *+12                                                             
         BRAS  RE,PRDOWN                                                        
         B     PRSEQ                                                            
*                                                                               
         TM    FLAGFTR,X'20'       TEST SORT TO DATE ORDER                      
         BO    PR20                                                             
         BRAS  RE,PRTREC           ELSE PRINT RECORD NOW                        
         B     PRSEQ                                                            
*                                                                               
PR20     BRAS  RE,BLDSRT                                                        
         B     PRSEQ                                                            
*                                                                               
PR30     MVC   SVXNKEY,KEY         SAVE KEY THAT CAUSED BREAK                   
*                                                                               
         TM    FLAGFTR,X'20'       TEST DATE SEQUENCE                           
         BZ    PR40                NO                                           
*                                                                               
         ICM   R0,15,SORTCNT                                                    
         BZ    PR40                                                             
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A50')  QSORT                          
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 (RF),DMCB,STRTSORT,(R0),L'SRTREC,L'SRTKEY,0                      
*                                                                               
         L     R2,STRTSORT         A(SORT BUFFER)                               
         L     R3,SORTCNT                                                       
*                                                                               
PR32     XC    KEY,KEY                                                          
         MVC   KEY+36(4),SRTDSKAD-SRTREC(R2)                                    
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AIO                                                           
         MVC   KEY(32),0(RE)       MOVE THIS KEY TO KEY                         
         MVC   NPTXNET-NPTKEY+KEY(4),SRTNET-SRTREC(R2)  MOVE NETWORK            
         MVC   NPTXPSSV-NPTKEY+KEY,SRTNPSSV-SRTREC(R2)  SET PSSV FLAG           
*                                                                               
         BRAS  RE,PRTREC                                                        
*                                                                               
         LA    R2,L'SRTREC(R2)                                                  
         BCT   R3,PR32                                                          
*                                                                               
PR40     TM    FLAGFTR,FLREPORT    TEST ERROR ONLY REPORT                       
         BZ    PR42                                                             
*                                                                               
         CLI   CMLPRT,C'Y'         TEST ANY PATTERNS PRINTED                    
         BE    PR42                YES-                                         
         MVI   P,0                 PRINT NO ACTIVITY MESSAGE                    
         MVC   P2(L'NOACTIV1),NOACTIV1                                          
         MVC   P3(L'NOACTIV2),NOACTIV2                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PR42     CLC   =C'ALL',TRACLT      TEST ALL CLIENT REQUEST                      
         BNE   PRX                 NO - DONE                                    
         MVI   FORCEHED,C'Y'                                                    
         MVC   KEY,SVXNKEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
PR44     CLC   KEY(2),=X'0A61'                                                  
         BNE   PRX                                                              
         CLC   KEY+2(1),BAGYMD                                                  
         BNE   PRX                                                              
*                                                                               
         MVC   BCLT,KEY+3          SET NEW CLIENT IN PROCESS                    
         BRAS  RE,FCLT             TEST TO PROCESS THIS CLIENT                  
         BE    PR02                YES!                                         
*                                                                               
         MVC   KEY,SVXNKEY                                                      
         LA    RE,KEY+NPTXNET-NPTXKEY   POINT TO FIELD AFTER CLT                
         SR    R0,R0                                                            
         BCTR  R0,0                R0 = -1                                      
         STCM  R0,15,0(RE)         SET NTWK TO FFF...                           
         STCM  R0,15,4(RE)         SET PROG TO FFFF...'                         
         GOTO1 HIGH                                                             
         B     PR44                                                             
*                                                                               
PRX      CLC   =C'DOWN',CONOUT                                                  
         BNE   PRX2                                                             
         MVI   KEY,X'FF'                                                        
         BRAS  RE,PRDOWN                                                        
*                                                                               
PRX2     XC    KEY,KEY                                                          
         J     EXIT                                                             
         EJECT                                                                  
*========================================================                       
* PRINT A RECORD                                                                
*========================================================                       
                                                                                
PRTREC   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R0,AIO3             CLEAR THE PRINT BUFFER                       
         LHI   R1,6000                                                          
         SR    RE,RE                                                            
         LA    RF,X'40'            FILL BUFFER WITH SPACES                      
         SLL   RF,24                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         L     R4,AIO3             PRINT BUFFER ADDRESS                         
         USING PRTLINE,R4                                                       
         ST    R4,PDATAST          SET PRINT BUFFER START ADDR                  
*                                                                               
         LA    RE,NPTXPRD-NPTXKEY+KEY                                           
         MVC   FLD(3),0(RE)                                                     
         LA    R3,NPTXSLN-NPTXKEY+KEY                                           
         BRAS  RE,FMTSLN                                                        
         MVC   PPRLN,FLD                                                        
*                                                                               
         LA    RE,NPTXPRD2-NPTXKEY+KEY                                          
         OC    0(3,RE),0(RE)                                                    
         BZ    PRTR02                                                           
*                                                                               
         MVC   FLD(3),0(RE)                                                     
         LA    R3,NPTXSLN2-NPTXKEY+KEY                                          
         BRAS  RE,FMTSLN                                                        
         MVC   PPTLN,FLD                                                        
*                                                                               
PRTR02   L     R6,AIO                                                           
         MVI   ELCODE,X'10'        DATA ELEMENT                                 
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         TM    NPTSTAT,NPTS_ADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         LA    R1,KEY                                                           
         USING NPTXKEY,R1                                                       
*                                                                               
         CLI   NPTXNET,0           TEST FOR PATTERN BY MEDIA                    
         BNE   PRTR04                                                           
         CLI   NPTXNET+2,X'FF'                                                  
         BNE   PRTR04                                                           
         MVC   PNET(2),=C'M='                                                   
         MVC   PNET+2(1),NPTXNET+1                                              
         B     *+10                                                             
*                                                                               
PRTR04   MVC   PNET,NPTXNET                                                     
         CLI   NPTXPSSV,0          TEST PASSIVE KEY                             
         BE    PRTR06              NO                                           
         MVI   PPROG,C'$'                                                       
         B     PRTR10                                                           
*                                                                               
PRTR06   CLI   NPTXPROG,X'FF'                                                   
         BE    PRTR10                                                           
         MVC   PPROG,NPTXPROG                                                   
         B     PRTR12X                                                          
*                                                                               
PRTR10   OC    NPTDPART,NPTDPART                                                
         BZ    PRTR12                                                           
         MVC   PCODE,NPTDPART      DAYPART CODE                                 
         B     *+10                                                             
*                                                                               
PRTR12   MVC   PCODE(1),NPTXPROG+1                                              
         MVC   PFEED,NPTXPROG+2                                                 
         DROP  R1                                                               
*                                                                               
PRTR12X  MVC   STRTPAT(6),NPTSTART   SAVE START/END DATES                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,NPTSTART),(5,PPERIOD)                             
         MVI   PPERIOD+8,C'-'                                                   
         CLC   NPTEND,=XL3'FFFFFF'                                              
         BNE   PRTR14                                                           
         MVC   PPERIOD+9(3),=CL3'UFN'                                           
         B     PRTR16                                                           
*                                                                               
PRTR14   MVI   PPERIOD+8,C'-'                                                   
         GOTO1 (RF),(R1),(3,NPTEND),(5,PPERIOD+9)                               
*                                                                               
PRTR16   SR    R0,R0                                                            
         ICM   R0,7,NPTXR3F-NPTXKEY+KEY                                         
         X     R0,=XL4'00FFFFFF'                                                
         EDIT  (R0),(5,PREF)                                                    
*                                                                               
         LA    R5,PDESC            SET OUTPUT POSN                              
*                                                                               
         TM    NPTSTAT,NPTS_TIME   TEST TIME IN ELEMENT                         
         BZ    PRTR18                                                           
         OC    NPTSTIM,NPTSTIM     TEST TIME PRESENT                            
         BZ    PRTR18                                                           
         GOTO1 UNTIME,DMCB,NPTSTIM,(R5)                                         
         LA    R5,110(R5)                                                       
*                                                                               
PRTR18   MVC   0(L'PDESC,R5),NPTDESC                                            
*                                                                               
         TM    NPTSTAT,X'80'       SOFT DELETE                                  
         BZ    PRTR20                                                           
         MVC   0(L'PDESC,R5),SPACES                                             
         MVC   0(9,R4),=C'*DELETED*'                                            
         DROP  R4                                                               
*                                                                               
PRTR20   MVI   ELCODE,X'30'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    PRTR30                                                           
         LA    R1,=CL34'- PATTERN HAS NO COMMERCIALS'                           
         B     PRTR82                                                           
                                                                                
* FIND CMML PCT ELEM *                                                          
                                                                                
PRTR30   XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'34'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    PRTR32                                                           
*                                                                               
         MVI   ELCODE,X'36'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   PRTR34                                                           
*                                                                               
PRTR32   MVC   ELEM,0(R6)          MOVE PCTEL TO ELEM                           
                                                                                
PRTR34   MVI   ELCODE,X'30'        FIND CMML ELEMENT                            
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PRTR94                                                           
*                                                                               
         USING NPTCMLEL,R6                                                      
         LLC   R2,NPTCMLLN                                                      
         SRL   R2,4                GIVES NUMBER OF CMMLS IN R2                  
         LA    R6,NPTCML           START OF LIST                                
         DROP  R6                                                               
*                                                                               
         MVI   ANYPIG,C'N'                                                      
         LR    R1,R6                                                            
         LR    R0,R2                                                            
*                                                                               
PRTR36A  OC    8(8,R1),8(R1)       TEST P/B                                     
         BZ    PRTR36B                                                          
         MVI   ANYPIG,C'Y'                                                      
         B     PRTR36C                                                          
*                                                                               
PRTR36B  LA    R1,16(R1)                                                        
         BCT   R0,PRTR36A                                                       
*                                                                               
PRTR36C  LAY   R5,ALPHATAB                                                      
*                                                                               
         L     R4,PDATAST          START ON FIRST OUTPUT LINE                   
         LA    R4,PDATA-PRTLINE(R4)                                             
         MVC   0(11,R4),=C'COMMERCIALS'                                         
         B     PRTR42                                                           
*                                                                               
PRTR40   BAS   RE,NEXTPDTA         POINT R4 TO NEXT PDATA                       
*                                                                               
PRTR42   LA    R4,12(R4)           FIRST COMMERCIAL POSN                        
         LA    R0,2                SET FOR 2 CMMLS/LINE                         
         CLI   ANYPIG,C'Y'                                                      
         BNE   *+8                                                              
         LA    R0,1                PRINT P/B 1 CMML/LINE                        
*                                                                               
PRTR44   MVC   0(1,R4),0(R5)       MOVE ALPHA CHAR                              
         MVI   1(R4),C'='                                                       
         MVC   2(8,R4),0(R6)                                                    
         CLC   =X'5C00',0(R6)                                                   
         BE    PRTR44A                                                          
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PRTR44A                                                          
         GOTO1 VTRPACK,DMCB,(C'U',(R6)),2(R4)                                   
*                                                                               
PRTR44A  LA    R4,15(R4)                                                        
         OC    8(8,R6),8(R6)       PIGGY-BACK CMML                              
         BZ    PRTR46              NO                                           
         MVI   0(R4),C'-'                                                       
         MVC   1(8,R4),8(R6)                                                    
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PRTR44B                                                          
         GOTO1 VTRPACK,DMCB,(C'U',8(R6)),1(R4)                                  
*                                                                               
PRTR44B  LA    R4,15(R4)                                                        
*                                                                               
PRTR46   BAS   RE,GETPCT                                                        
*                                                                               
         AHI   R2,-1               DECREMENT REMAINING COUNT                    
         BNP   PRTR50                                                           
         LA    R4,6(R4)                                                         
         LA    R5,1(R5)                                                         
         LA    R6,16(R6)                                                        
         BCT   R0,PRTR44                                                        
         B     PRTR40                                                           
*                                                                               
PRTR50   BAS   RE,NEXTPDTA                                                      
*                                                                               
         MVI   ELCODE,X'32'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    PRTR52                                                           
*                                                                               
         MVI   ELCODE,X'34'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PRTR54                                                           
*                                                                               
         USING NPTPTNEL,R6                                                      
PRTR52   MVC   0(8,R4),=C'ROTATION'                                             
         LLC   R1,NPTPTNLN                                                      
         AHI   R1,-3               GET ROT LEN -1                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R4),2(R6)                                                   
*                                                                               
         BAS   RE,NEXTPDTA                                                      
*                                                                               
PRTR54   CLI   NPTXPSSV-NPTKEY+KEY,0   TEST PASSIVE KEY                         
         BNE   PRTR60                  YES - NO NETWORK LIST                    
*                                                                               
         MVI   ELCODE,X'5B'        NETWORK LIST ELEM                            
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PRTR60                                                           
         LR    RE,R4                                                            
         AHI   RE,-5               BACK UP                                      
         MVC   0(5,RE),=C'NETS='                                                
*                                                                               
PRTR56   TM    6(R6),X'80'         TEST DELETED                                 
         BO    PRTR57                                                           
*                                                                               
         MVC   0(4,R4),2(R6)       MOVE NETWORK                                 
         LA    R4,3(R4)            POINT TO LAST CHAR                           
         CLI   0(R4),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
*                                                                               
PRTR57   BRAS  RE,NEXTEL                                                        
         BNE   PRTR58                                                           
*                                                                               
         LA    R0,60               MAX CHARS FOR NETWORKS                       
         AHI   R0,PDATA-PRTLINE    DSPL TO FIRST CHAR                           
         A     R0,PDATAST                                                       
         CR    R4,R0               TEST ROOM FOR MORE THIS LINE                 
         BL    PRTR56                                                           
         BAS   RE,NEXTPDTA                                                      
         B     PRTR56                                                           
*                                                                               
PRTR58   BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
*                                                                               
         BAS   RE,NEXTPDTA                                                      
*                                                                               
PRTR60   MVI   ELCODE,X'50'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PRTR62                                                           
*                                                                               
         MVC   0(5,R4),=C'TEXT='                                                
         MVC   12(7,R4),2(R6)                                                   
         BAS   RE,NEXTPDTA                                                      
*                                                                               
PRTR62   MVI   ELCODE,X'40'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PRTR70                                                           
*                                                                               
         USING NPTCMTEL,R6                                                      
PRTR64   MVC   0(8,R4),=C'COMMENTS'                                             
*                                                                               
PRTR66   MVC   12(1,R4),NPTCMTNO                                                
         OI    12(R4),X'F0'                                                     
         MVI   13(R4),C'.'                                                      
         LLC   R1,NPTCMTLN                                                      
         AHI   R1,-4                                                            
         DROP  R6                                                               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   15(0,R4),3(R6)                                                   
*                                                                               
         BAS   RE,NEXTPDTA                                                      
         BRAS  RE,NEXTEL                                                        
         BE    PRTR66                                                           
         EJECT                                                                  
*==========================================================                     
* CHECK FOR ANY COMMERCIAL ERRORS AND FOR CONFLICTS BETWEEN                     
* PATTERN AND COMMERCIALS                                                       
*==========================================================                     
                                                                                
PRTR70   MVI   CMLFLAG1,0           INIT FLAGS                                  
         MVI   CMLFLAG,0                                                        
*                                                                               
         LA    R1,KEY                                                           
         USING NPTXKEY,R1                                                       
         MVC   QPRD,NPTXPRD         PRD                                         
         MVC   BSLN,NPTXSLN         SLN                                         
         MVC   QPRD2,NPTXPRD2       PRD2                                        
         MVC   BSLN2,NPTXSLN2       SLN2                                        
         DROP  R1                                                               
*                                                                               
PRTR72   MVI   ELCODE,X'30'        CHECK FOR PATTERN/CMML ERRORS                
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVMYKEY,KEY                                                      
         LLC   R0,1(R6)                                                         
         SRL   R0,4                GIVES NUMBER OF ENTRIES                      
         LA    R6,2(R6)                                                         
*                                                                               
PRTR74   XC    SVTYPE1(7),SVTYPE1  TYPE(4)/SLN(1)/SOLO(1)/FLAG(1)               
         XC    SVTYPE2(7),SVTYPE2                                               
*                                                                               
         CLC   =X'5C0000',0(R6)    TEST DELETED                                 
         BE    PRTR78                                                           
         MVC   WORK(8),0(R6)       MOVE CMML1                                   
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PRTR74A                                                          
         GOTO1 VTRPACK,DMCB,(C'U',(R6)),WORK   NEED UNPACKED TO VCML            
*                                                                               
PRTR74A  LA    R1,SVTYPE1                                                       
         BRAS  RE,VCML                                                          
         CLI   CMLFLAG,0                                                        
         BNE   PRTR80                                                           
         CLI   CMLFLAG1,0                                                       
         BNE   PRTR80                                                           
*                                                                               
         OC    8(8,R6),8(R6)       CHECK FOR CMML2                              
         BZ    PRTR76                                                           
         CLC   =X'5C0000',0(R6)    TEST DELETED                                 
         BE    PRTR78                                                           
         MVC   WORK(8),8(R6)       MOVE CMML1                                   
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PRTR74B                                                          
         GOTO1 VTRPACK,DMCB,(C'U',8(R6)),WORK   NEED UNPACKED TO VCML           
*                                                                               
PRTR74B  LA    R1,SVTYPE2                                                       
         BRAS  RE,VCML                                                          
         CLI   CMLFLAG,0                                                        
         BNE   PRTR80                                                           
         CLI   CMLFLAG1,0                                                       
         BNE   PRTR80                                                           
*                                                                               
PRTR76   BRAS  RE,CHKCMLS          GO CHECK FOR INCONSISTENCIES                 
*                                                                               
         CLI   CMLFLAG,0                                                        
         BNE   PRTR80                                                           
         CLI   CMLFLAG1,0                                                       
         BNE   PRTR80                                                           
*                                                                               
PRTR78   LA    R6,16(R6)           NEXT CMML ELEMENT                            
         BCT   R0,PRTR74                                                        
                                                                                
* REPORT RAN WITHOUT ERRORS                                                     
                                                                                
         TM    FLAGFTR,FLREPORT    TEST ERROR ONLY REPORT                       
         BZ    PRTR90                                                           
         B     PRTRX               NO ERRORS - DON'T PRINT                      
*                                                                               
PRTR80   LA    R1,=CL34'CML REL DATE AFTER PAT START'                           
         TM    CMLFLAG,CMLDTSSW                                                 
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'PAT/CML SPOT LENGTH DIFFERENT'                          
         TM    CMLFLAG,CMLPATLN                                                 
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'CML RECALL DATE BEFORE PAT START'                       
         TM    CMLFLAG,CMLDTESW                                                 
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'CML PIGGY/SOLO MUST MATCH USAGE'                        
         TM    CMLFLAG,PGSOSW                                                   
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'PATTERN PROD NOT IN CML'                                
         TM    CMLFLAG,CMLPRDSW                                                 
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'P/B CMML TYPES NOT THE SAME'                            
         TM    CMLFLAG,TYPESW                                                   
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'NETWORK NOW EXCLUDED'                                   
         TM    CMLFLAG,X'20'       EXCLUDED NETWORK                             
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'CML NETWORK SPECIFIED'                                  
         TM    CMLFLAG,X'80'       CML IS NETWORK SPECIFIC                      
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'PRD/LEN NOT ON CML'                                     
         TM    CMLFLAG,X'40'       BAD PRODUCT                                  
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'CLIENT APPROVAL DATE MISSING'                           
         TM    CMLFLAG1,NOAPDTE                                                 
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'COMMERCIAL NOT APPROVED TO AIR'                         
         TM    CMLFLAG1,NOAPAIR                                                 
         BO    PRTR82               NO                                          
*                                                                               
         LA    R1,=CL34'COMMERCIALS DELETED'                                    
         TM    CMLFLAG1,DELCMLSW                                                
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'INVALID CMML IN PATTERN'                                
         TM    CMLFLAG1,CMLINVAL                                                
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'>>> CMML/PTTN TIME CONFLICT'                            
         TM    CMLFLAG1,CMLINVTM                                                
         BO    PRTR82                                                           
*                                                                               
         LA    R1,=CL34'PATTERN VERY SICK'                                      
*                                                                               
PRTR82   MVI   CMLPRT,C'Y'         CMLS PRINTED ON ERROR REPORT                 
         BAS   RE,NEXTPDTA                                                      
         AHI   R4,-6               BACK UP TO UNDER REF                         
         MVC   0(13,R4),=C'====> ERROR -'                                       
         MVC   14(34,R4),0(R1)                                                  
                                                                                
*=================================================================              
* NOW PRINT THE LINES OUT 4 AT A TIME                                           
* REMEMBER PRINT LINES IN BUFFER ARE 110 BYTES                                  
*=================================================================              
                                                                                
PRTR90   L     R4,AIO3                                                          
         SR    R6,R6                                                            
*                                                                               
PRTR92   CLC   0(110,R4),SPACES                                                 
         BE    PRTR94                                                           
         LA    R4,110(R4)                                                       
         BCT   R6,PRTR92                                                        
*                                                                               
PRTR94   LPR   R6,R6                                                            
         STC   R6,ALLOWLIN                                                      
         CHI   R6,10                                                            
         BNH   *+8                                                              
         MVI   ALLOWLIN,10                                                      
*                                                                               
         L     R4,AIO3                                                          
         MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
*                                                                               
PRTR96   LA    R0,4                                                             
         CR    R0,R6                                                            
         BNH   *+6                                                              
         LR    R0,R6                                                            
         LA    R1,P                                                             
*                                                                               
PRTR98   MVC   0(110,R1),0(R4)     BUFFER HAS 110 CHARS                         
         LA    R1,132(R1)          P HAS 132 CHARS                              
         LA    R4,110(R4)                                                       
         BCT   R0,PRTR98                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AHI   R6,-4                                                            
         BP    PRTR96                                                           
*                                                                               
PRTRX    BRAS  RE,INITXSP          READ XSPFILE AGAIN                           
         MVC   KEY,SVMYKEY                                                      
         GOTO1 HIGH                                                             
         J     EXIT                                                             
         EJECT                                                                  
*===========================================================                    
* ON ENTRY R5 POINTS TO ALPHATAB ENTRY                                          
* FORMAT PCT AT 1(R4)                                                           
*===========================================================                    
                                                                                
GETPCT   NTR1                                                                   
         CLI   ELEM,0              TEST NO PCTS                                 
         BE    GETPCTX                                                          
         LA    R6,ELEM+2                                                        
*                                                                               
GETPCT2  CLC   0(1,R5),0(R6)       MATCH LETTER                                 
         BE    GETPCT4                                                          
         BL    GETPCTX             CMML MUST BE DELETED                         
         LA    R6,3(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   GETPCT2                                                          
         B     GETPCTX                                                          
*                                                                               
GETPCT4  LA    RE,1(R4)                                                         
         MVI   0(RE),C'('                                                       
         LLC   R0,2(R6)                                                         
         EDIT  (R0),(3,1(RE)),ALIGN=LEFT                                        
         AR    RE,R0                                                            
         MVI   1(RE),C')'                                                       
*                                                                               
GETPCTX  J     EXIT                                                             
                                                                                
NEXTPDTA L     R4,PDATAST                                                       
         CLC   0(110,R4),SPACES                                                 
         JE    *+12                                                             
         LA    R4,110(R4)                                                       
         ST    R4,PDATAST                                                       
         LA    R4,PDATA-PRTLINE(R4)                                             
         BR    RE                                                               
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
* RESET FILES TO SPOT *                                                         
*                                                                               
INITSPT  MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVI   LSTATUS+1,1                                                      
         BRAS  R1,INITALL                                                       
         DC    CL4'SPT'                                                         
*                                                                               
* RESET FILES TO NET *                                                          
*                                                                               
INITNET  MVI   DATADISP+1,27       SET FROM SPOT TO NET                         
         MVI   LKEY+1,20                                                        
         MVI   LSTATUS+1,1                                                      
         BRAS  R1,INITALL                                                       
         DC    CL4'UNT'                                                         
*                                                                               
* RESET FILES TO XFILE *                                                        
*                                                                               
INITXSP  MVI   DATADISP+1,42       SET FROM SPOT TO XSP                         
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         BRAS  R1,INITALL                                                       
         DC    CL4'XSP'                                                         
*                                                                               
INITALL  MVC   SYSDIR(3),0(R1)                                                  
         MVC   SYSFIL(3),0(R1)                                                  
         XC    FILENAME,FILENAME                                                
         BR    RE                                                               
*                                                                               
CMLSEQER MVC   GERROR,=Y(CMMLSQER) COMMERCIAL SEQUENCE ERROR                    
ERREXIT2 GOTO1 VTRAERR                                                          
         EJECT                                                                  
CMLTYPER DS    0H                                                               
         CLI   MODE,PRINTREP       PRINTING REPORT                              
         BNE   *+12                                                             
         OI    CMLFLAG1,TYPESW     PIGGY-BACK COMML TYPES UNEQUAL               
         J     EXIT                                                             
*                                                                               
         MVI   ERROR,UNMCMLTP      PIGGY-BACK CML TYPES UNEQUAL                 
         J     TRAPERR                                                          
*                                                                               
PTPRDERR MVC   GERROR,=Y(PATPRDER) PAT PROD NOT IN CML                          
*                                                                               
         USING NPTXKEY,R4                                                       
         L     R4,AIO1                                                          
*                                                                               
         MVC   FLD(3),NPTXPRD                                                   
         TM    PRDMATSW,PRDMATPR   WAS 1ST PRD FOUND                            
         BZ    *+10                 NO                                          
         MVC   FLD(3),NPTXPRD2                                                  
         DROP  R4                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         STCM  R3,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R3),7             L'SUBST TEXT + 1                             
         MVC   1(6,R3),FLD                                                      
         J     ERREXIT2                                                         
*                                                                               
AFFILER  MVI   ERROR,INVAFFIL      AFFIL MUST BE ABC, CBS, NBC                  
         J     TRAPERR                                                          
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         J     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
INVALERR MVI   ERROR,INVALID                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         LTORG                                                                  
*                                                                               
ALPHATAB DC    CL12'ABCDEFGHIJKL'  12 LETTERS FOR 12 COMMERCIALS                
*                                                                               
NOACTIV1 DC    CL50'** REPORT RAN AS A RESULT OF A CML RECORD CHANGE.'          
NOACTIV2 DC    CL75'** PATTERN RECORDS ARE NOT AFFECTED BY THE CHANGE(SC        
               ). NO ACTION NECESSARY.'                                         
*                                                                               
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
PPER     NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(3,(R2)),(5,WORK)                                    
         MVI   WORK+8,C'-'                                                      
         CLC   =XL3'FFFFFF',3(R2)                                               
         BNE   PPER10                                                           
         MVC   WORK+9(3),=CL3'UFN'                                              
         J     EXIT                                                             
*                                                                               
PPER10   GOTO1 (RF),(R1),(3,3(R2)),(5,WORK+9)                                   
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
* PRINT SPOT LEN AT 0(R3) AFTER PRODUCT IN FLD                                  
                                                                                
FMTSLN   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,FLD                                                           
         LA    R5,2(R5)                                                         
         CLI   0(R5),C' '                                                       
         BNH   FMTSLN10                                                         
         LA    R5,1(R5)                                                         
*                                                                               
FMTSLN10 MVI   0(R5),C'-'                                                       
         MVC   1(4,R5),SPACES                                                   
*                                                                               
         LLC   R0,0(R3)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R5),DUB                                                      
         CHI   R0,100                                                           
         BL    *+10                                                             
         UNPK  1(3,R5),DUB                                                      
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
BLDSRT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,NEXTSORT                                                      
         USING SORTREC,R3                                                       
*                                                                               
         L     R6,AIO                                                           
         USING NPTXKEY,R6                                                       
*                                                                               
         XC    SRTREC,SRTREC                                                    
         MVC   SRTCLT,NPTXCLT                                                   
         MVC   SRTNET,NPTXNET                                                   
         MVC   SRTPROG,NPTXPROG                                                 
         MVC   SRTPRD,NPTXPRD                                                   
         MVC   SRTSLN,NPTXSLN                                                   
*                                                                               
         CLI   NPTXPSSV-NPTKEY+KEY,0       TEST PASSIVE KEY                     
         BE    BLDSRT1                     NO                                   
         MVC   SRTNET,NPTXNET-NPTKEY+KEY   MOVE NET FROM KEY                    
         MVI   SRTNPSSV,C'$'               AND SET PASSIVE KEY FLAG             
*                                                                               
BLDSRT1  OC    NPTXPRD2,NPTXPRD2   IF NO PRD2                                   
         BZ    BLDSRT2             LEAVE ZERO                                   
*                                                                               
         MVC   SRTPRD2,NPTXPRD2                                                 
         MVC   SRTSLN2,NPTXSLN2                                                 
         MVC   SRTREFS,NPTXR3F                                                  
*                                                                               
BLDSRT2  MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,NPTSTART),(2,SRTDTS)                              
         GOTO1 (RF),(R1),(3,NPTEND),(2,SRTDTS+2)                                
*                                                                               
         MVC   SRTDSKAD,KEY+36                                                  
*                                                                               
         LA    R3,L'SRTREC(R3)                                                  
         C     R3,NEXTMAX                                                       
         BL    BLDSRT12                                                         
*                                                                               
         LA    R2,CONWHENH                                                      
         TM    WHEN,X'40'          IF NOW (ONLINE), MUST RUN OFFLINE            
         BZ    BLDSRT10                                                         
         MVC   GERROR,=Y(TOOBIG)                                                
         GOTO1 VTRAERR                                                          
*                                                                               
BLDSRT10 MVI   SIZEFLAG,C'Y'                                                    
         B     BLDSRTX                                                          
*                                                                               
BLDSRT12 ST    R3,NEXTSORT                                                      
         L     R1,SORTCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,SORTCNT                                                       
*                                                                               
BLDSRTX  J     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
VREC     NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VR01                                                             
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VR01     MVI   CHGESW,0            SET CHGESW OFF                               
         MVC   SVXNKEY,KEY         SAVE XFILE KEY                               
         BRAS  RE,INITXSP          SET FOR XFILE                                
*                                                                               
         CLI   ACTNUM,5             TEST THIS IS SELECT ACTION                  
         BNE   *+8                                                              
         OI    GENSTAT2,RETEQSEL    STAY ON THIS SELECTION                      
*                                                                               
         L     R4,AIO                                                           
         USING NPTXKEY,R4                                                       
*                                                                               
         CLC   NPTXCLT,BCLT                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BCLT,NPTXCLT                                                     
         MVC   QPRD,NPTXPRD         PRD                                         
         MVC   BSLN,NPTXSLN         SLN                                         
         MVC   QPRD2,NPTXPRD2       PRD2                                        
         MVC   BSLN2,NPTXSLN2       SLN2                                        
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,NPTXR3F                                                     
         ST    RE,BREFK                                                         
         ST    RE,BREF                                                          
         XC    BREF,=X'00FFFFFF'                                                
*                                                                               
         MVI   COMMLCT,0                                                        
         MVI   PRDMATSW,0                                                       
*                                                                               
         CLI   MYSCREEN,X'5A'      TEST COMMENT SCREEN                          
         BE    VR50                                                             
*                                                                               
         LA    R2,TRADESCH         DESCRIPTION                                  
         GOTO1 ANY                                                              
*                                                                               
         CLI   ACTNUM,ACTADD       IF AN ADD, NO DELETE/RESTORE                 
         BE    VR10                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
         CLC   =C'DELETE',WORK     SOFT DELETE THIS PATTERN                     
         BNE   VR02                NO                                           
         OI    NPTSTAT,X'80'       SET ON SOFT DELETE BIT                       
         B     VR60                JUST DELETE PATTERN                          
*                                                                               
VR02     CLC   =C'RESTORE',WORK    IS THIS A RESTORE SOFT DEL                   
         BNE   VR10                NO                                           
*                                                                               
         TM    NPTSTAT,X'80'       WAS PAT SOFT DELETED                         
         BZ    INVALER1             NO                                          
*                                                                               
         CLI   TRANET,C'$'         DOING NETWORK LIST                           
         BNE   VR08                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VR10                                                             
*                                                                               
VR04     TM    6(R6),X'80'         TEST THIS NETWORK IS DELETED                 
         BO    VR06                                                             
         MVC   SVXNKEY+5(4),2(R6)  MOVE THIS NETWORK TO KEY                     
         BRAS  RE,CHKOV                                                         
*                                                                               
VR06     BRAS  RE,NEXTEL                                                        
         BE    VR04                                                             
         B     VR08X                                                            
                                                                                
* CHECK FOR ANY OTHER PATTERNS FOR THIS CLIENT PROD WITH SAME                   
* MARKET/STATION LIST AND OVERLAPPING DATES                                     
                                                                                
VR08     BRAS  RE,CHKOV            CHECK FOR OVERLAPPING DATES                  
*                                                                               
VR08X    NI    NPTSTAT,X'FF'-X'80' SET OFF SOFT DELETE                          
         B     VR60                JUST RESTORE PATTERN                         
*                                                                               
VR10     MVI   ELCODE,X'10'        PATTERN DATA ELEMENT                         
         GOTO1 REMELEM             REMOVE DATA ELEM                             
         MVC   SVELEM,ELEM         AND SAVE IT                                  
*                                                                               
         LA    R6,ELEM                                                          
         USING NPTDTAEL,R6                                                      
         MVI   NPTDTAEL,X'10'                                                   
         MVI   ELEM+1,NPTDTAX-NPTDTAEL                                          
         OI    NPTSTAT,NPTS_TIME   SET FLAG FOR TIME FIELD IN ELEM              
         MVC   NPTDESC,WORK                                                     
*                                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL       CHANGE SELECT ?                              
         BNE   *+8                                                              
         NI    NPTSTAT,X'FF'-NPTOPCHG TURN OFF PATTERN CHAGED IN OPTICA         
*                                                                               
         LA    R2,TRAPERH          PERIOD                                       
         BRAS  RE,VPER                                                          
*                                                                               
         XC    NPTSTIM(4),NPTSTIM                                               
         LA    R2,TRASTIMH         PATTERN START TIME                           
         CLI   5(R2),0             TEST ENTERED                                 
         BNE   VR12                YES                                          
         CLI   TRAETIMH+5,0        ELSE MUST BE NO END TIME                     
         JNE   MISSERR                                                          
         B     VR30                                                             
*                                                                               
VR12     LA    R2,TRAETIMH         MUST HAVE END TIME TOO                       
         CLC   ENDPAT,=X'FFFFFF'   TEST UFN PATTERN                             
         BNE   VR14                                                             
         CLI   5(R2),0             MUST NOT HAVE END TIME FOR UFN               
         JNE   INVALERR                                                         
         BE    VR16                                                             
         J     INVALERR                                                         
*                                                                               
VR14     CLI   TRAETIMH+5,0                                                     
         JE    MISSERR                                                          
*                                                                               
VR16     LA    R2,TRASTIMH                                                      
         BAS   RE,GOTIMVAL                                                      
         MVC   NPTSTIM,FULL                                                     
*                                                                               
         CLC   ENDPAT,=X'FFFFFF'   TEST UFN PATTERN                             
         BE    VR18                                                             
*                                                                               
         LA    R2,TRAETIMH                                                      
         BAS   RE,GOTIMVAL                                                      
         CLC   FULL(2),=H'2400'   MUST END BEFORE MIDNIGHT                      
         JNL   INVALERR                                                         
         MVC   NPTETIM,FULL                                                     
*                                                                               
VR18     OC    NPTSTIM(4),NPTSTIM  TEST TIME ENTERED                            
         BZ    VR20                NO                                           
         CLI   NPTXPROG,0          TEST PROG OR DPT IN KEY                      
         BE    VR20                                                             
         MVC   GERROR,=Y(NOHAVTIM) CAN'T HAVE TIME IF DPT OR PGM                
         LA    R2,TRASTIMH                                                      
         GOTO1 VTRAERR                                                          
                                                                                
VR20     CLC   NPTSTART,NPTEND     PATTERN FOR ONE DAY                          
         BNE   VR30                                                             
*                                                                               
         LH    R0,NPTSTIM          YES - MUST START BEFORE IT ENDS              
         B     *+16   <------      NOP FOR CALENDAR DAYS                        
         CHI   R0,599                                                           
         BH    *+8                                                              
         AHI   R0,2400                                                          
*                                                                               
         CHI   R0,2400             TEST FOR MIDNIGHT                            
         BNE   *+6                                                              
         XR    R0,R0               MAKE IT A LOW TIME                           
*                                                                               
         LH    R1,NPTETIM                                                       
         B     *+16   <------      NOP FOR CALENDAR DAYS                        
         CHI   R1,599                                                           
         BH    *+8                                                              
         AHI   R1,2400                                                          
*                                                                               
         CR    R0,R1               TEST START BEFORE END                        
         BNH   VR30                                                             
         LA    R2,TRASTIMH                                                      
         J     INVALERR                                                         
*                                                                               
GOTIMVAL NTR1                                                                   
         LLC   R0,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R0),8(R2)),FULL                                    
         CLI   0(R1),X'FF'                                                      
         JE    INVALERR                                                         
         J     EQXIT                                                            
*                                                                               
VR30     LA    RE,NPTSTIM-NPTDTAEL+SVELEM                                       
         CLC   NPTSTIM(4),0(RE)    TEST TIME HAS CHANGED                        
         BE    *+8                                                              
         MVI   CHGESW,1            FORCE NEW SUBL IF NEW TIME                   
*                                                                               
         CLI   NPTXPROG,X'FF'      TEST DAYPART IN KEY                          
         BNE   VR32                NO                                           
         CLI   NPTXPROG+1,0        ANY DAYPART                                  
         BE    VR32                NO                                           
*                                                                               
         MVC   SVMYKEY(32),KEY     SAVE XFILE KEY                               
         GOTO1 VALIDPT,DMCB,(X'01',NPTXPROG+1)  GET 2 CHAR DAYPART              
         MVC   KEY(32),SVMYKEY     RESTORE KEY                                  
         MVC   NPTDPART,QDPT2      MOVE IN 2 CHAR DAYPART CODE                  
*                                                                               
VR32     GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,TRANET1H         TEST ANY NETWORK LIST INPUT                  
         CLI   5(R2),0                                                          
         BNE   VR34                                                             
         LA    R2,TRANET2H                                                      
         CLI   5(R2),0                                                          
         BNE   VR34                                                             
         LA    R2,TRANET1H                                                      
         CLI   TRANET,C'$'                                                      
         BNE   VR40                                                             
         CLI   ACTNUM,ACTADD       ADD MUST HAVE AT LEAST 1 NET                 
         JE    MISSERR                                                          
         B     VR36                ELSE MAYBE THEY DELETED ALL                  
*                                                                               
VR34     CLI   TRANET,C'$'         TEST NETWORK LIST KEY                        
         JNE   INVALERR            IF NOT, SHOULD HAVE NO INPUT                 
*                                                                               
VR36     BRAS  RE,VALNLIST         VALIDATE NETWORK LIST                        
*                                                                               
VR40     BRAS  RE,VCMMLS           VALIDATE CMMLS/PCTS                          
*                                                                               
         CLI   TRANET,C'$'         DOING NETWORK LIST                           
         BNE   VR44                                                             
         CLI   SVNETNEW,C'Y'       TEST LIST IS NEW                             
         BNE   VR44X               NO                                           
         L     R4,ASVNET                                                        
         CLI   0(R4),0                                                          
         BE    VR42X                                                            
*                                                                               
VR42     MVC   SVXNKEY+5(4),0(R4)  MOVE THIS NETWORK TO KEY                     
         BRAS  RE,CHKOV                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   VR42                                                             
*                                                                               
VR42X    MVC   SVXNKEY+5(4),TRANET RESTORE PATTERN REC KEY                      
         BRAS  RE,UPDNETS          UPDATE NETWORK ELEMENTS                      
         CLI   ACTNUM,ACTADD       IF ADD, POINTERS ADDED LATER                 
         BE    VR44X                                                            
         MVC   KEY,SVXNKEY         ELSE REREAD DIRECTORY NOW                    
         GOTO1 HIGH                                                             
         B     VR44X                                                            
*                                                                               
VR44     BRAS  RE,CHKOV            CHECK FOR OVERLAPPING PATTERNS               
*                                                                               
VR44X    CLI   ACTNUM,ACTADD       TEST ADD                                     
         BNE   VR48                                                             
         CLI   SVTN3PR0,C'Y'       TEST TO COPY COMMENTS ON ADD                 
         BNE   VR48                                                             
         OC    SVPRVDA,SVPRVDA     TEST OK TO COPY PRV CLT                      
         BZ    VR48                                                             
*                                                                               
* GET THE PREVIOUS PATTERN RECORD                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+36(4),SVPRVDA                                                
         L     R0,AIO              SAVE PREVIOUS                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         ST    R0,AIO              RESTORE PREVIOUS                             
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'40'        FIND COMMENTS                                
         BRAS  RE,GETEL                                                         
         BNE   VR48                                                             
*                                                                               
VR46     MVC   ELEM,0(R6)          MOVE TO ELEM                                 
         GOTO1 ADDELEM             AND COPY TO NEW RECORD                       
         BRAS  RE,NEXTEL                                                        
         BE    VR46                                                             
*                                                                               
VR48     B     VR60                                                             
                                                                                
*================================================================               
* VALIDATE COMMENTS                                                             
*================================================================               
                                                                                
VR50     BRAS  RE,VCMT                                                          
         BRAS  RE,VHIAT                                                         
         EJECT                                                                  
*================================================================               
* FINAL RECORD VALIDATION                                                       
*================================================================               
                                                                                
VR60     MVC   KEY(L'SVXNKEY),SVXNKEY    XFILE KEY                              
*                                                                               
         CLI   ACTNUM,ACTADD       ADD RECORD                                   
         BE    VR62                 YES                                         
         CLI   CHGESW,0            HAVE DATES, COMMLS, OR ROTATION CHGD         
         BE    VR62                 NO                                          
*                                                                               
         BAS   RE,NEWREF           GO MOVE PTN TO NEW REF NUMBER                
*                                                                               
VR62     DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR70                                                             
         BRAS  RE,NGETSEQ          ADD REC TO XSPFILE                           
         B     VRXIT                                                            
*                                                                               
VR70     DS    0H                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL       CHANGE SELECT ?                              
         BNE   VRDIE                                                            
*                                                                               
VRXIT    BRAS  RE,INITXSP                                                       
         XIT1                                                                   
*                                                                               
VRDIE    DC    H'0'                WHAT ?                                       
         EJECT                                                                  
CMLMISSR LA    R2,TRACMLAH         FIRST COMMERCIAL FIELD                       
         MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
INVDELER MVI   ERROR,INVCMLDL      CAN'T DELETE AN EMPTY FLD                    
         J     TRAPERR                                                          
*                                                                               
EQCMLER  MVI   ERROR,DUPLCMML      2 PIGGY-BACK COMMERCIALS EQUAL               
         J     TRAPERR                                                          
*                                                                               
CMLTPERR MVI   ERROR,UNMCMLTP      PIGGY-BACK CML TYPES UNEQUAL                 
         CLI   MODE,PRINTREP                                                    
         JNE   TRAPERR                                                          
         OI    CMLFLAG1,TYPESW     PIGGY-BACK COMML TYPES UNEQUAL               
         J     VRXIT                                                            
*                                                                               
INVALER1 MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
*                                                                               
PTPRODER MVC   GERROR,=Y(PATPRDER) PAT PROD NOT IN CML                          
*                                                                               
         USING NPTXKEY,R4                                                       
         L     R4,AIO1                                                          
*                                                                               
         MVC   FLD(3),NPTXPRD                                                   
         TM    PRDMATSW,PRDMATPR   WAS 1ST PRD FOUND                            
         BZ    *+10                 NO                                          
         MVC   FLD(3),NPTXPRD2                                                  
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         STCM  R3,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R3),7             L'SUBST TEXT + 1                             
         MVC   1(6,R3),FLD                                                      
         B     ERREXITX                                                         
*                                                                               
CMLSQERR MVC   GERROR,=Y(CMMLSQER) COMMERCIAL SEQUENCE ERROR                    
ERREXITX GOTO1 VTRAERR                                                          
         EJECT                                                                  
*==================================================================             
* GET A NEW REF/SEQ NUMBER FROM SEQNUM RECORD                                   
* ADD THE OLD PATTERN RECORD WITH A NEW REF NUM (< XA00000)                     
* THEN PUT A NEW DISK ADDRESS IN THE OLD PASSIVE SEQNUM POINTER                 
* THEN CREATE A NEW SEQNUM POINTER FOR THE NEW RECORD (THE ONE                  
* WHICH IS ABOUT TO BE CHANGED)                                                 
*==================================================================             
                                                                                
NEWREF   NTR1                                                                   
         BRAS  RE,INITXSP           PATTERNS LIVE ON XSPFILE                    
                                                                                
*=======================================================                        
* READ SEQNUM RECORD TO GET NEXT AVAILABLE SEQNUM                               
*=======================================================                        
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(5),SVXNKEY       0A61/A-M/CLT/000000 IS SEQNUM REC           
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDATA,R6                                                       
*                                                                               
         MVC   HOLDSEQ,NPTS3QNO                                                 
         SR    R1,R1                                                            
         ICM   R1,7,NPTS3QNO       UPDATE 'NEXT' SEQNUM                         
         LA    R1,1(R1)                                                         
         STCM  R1,7,NPTS3QNO                                                    
*                                                                               
         MVC   HOLDREF,NPTR3FNO                                                 
         SR    R1,R1                                                            
         ICM   R1,7,NPTR3FNO       AND REFNUM                                   
         LA    R1,1(R1)                                                         
         STCM  R1,7,NPTR3FNO                                                    
*                                                                               
         MVI   BYTE,C'C'           CHANGE F1 ELEM                               
         BRAS  RE,AACTELEM                                                      
         GOTO1 PUTREC              AND WRITE SEQNUM RECORD                      
                                                                                
*================================================================               
* NOW REREAD THE OLD PATTERN RECORD                                             
* AND ADD IT WITH THE NEW >> SEQNUM << AS THE REF NUM                           
*================================================================               
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(32),SVXNKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   NPTXR3F-NPTXKEY(3,R6),HOLDSEQ   MOVE NEW REF TO KEY              
         MVC   NPTXOR3G-NPTXKEY(3,R6),BREFK+1  MOVE OLD REF TO KEY              
*                                                                               
         MVI   BYTE,0                                                           
         BRAS  RE,AACTELEM         ADD F1 ELEM                                  
*                                                                               
         GOTO1 ADDREC                                                           
         DROP  R6                                                               
*                                                                               
NEWREF06 MVC   SVDSKAD,KEY         SAVE THE NEW DISK ADDRESS                    
         EJECT                                                                  
*&&DO                                                                           
*---> I DON'T THINK WE NEED THIS                                                
*---> HISTORY IS RECORDED ONLY UNDER THE $ RECORD                               
         CLI   NPTXNET-NPTKEY+KEY,C'$'    TEST A NETWORK LIST RECORD            
         BNE   NEWREF10                                                         
                                                                                
* NEED A NEW PASSIVE 0A61 REC FOR EACH NETWORK                                  
                                                                                
         XC    KEY,KEY                                                          
         L     R6,AIO                                                           
         MVC   KEY(32),0(R6)               MOVE KEY FROM RECORD                 
         MVI   NPTXPSSV-NPTXKEY+KEY,C'$'   SET PASSIVE FLAG                     
         MVC   KEY+36(4),SVDSKAD                                                
*                                                                               
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         BNE   NEWREF10                                                         
*                                                                               
NEWREF08 MVC   NPTXNET-NPTKEY+KEY(4),2(R6)                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'XSPDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    NEWREF08                                                         
*&&                                                                             
         EJECT                                                                  
*================================================================               
* NOW POINT THE 0AE1...(OLDSEQNUM) TO THE RECORD JUST ADDED                     
*================================================================               
                                                                                
NEWREF10 L     R6,AIO              FIND 10 ELEM IN OLD RECORD                   
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDATA,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AE1'       GET OLD PASSIVE                            
         MVC   KEY+2(3),SVXNKEY+2     A/M & BCLT                                
         MVC   KEY+5(3),NPTS3QNO   OLD SEQNUM                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+36(4),SVDSKAD   MOVE IN NEW DISK ADDR                        
         GOTO1 WRITE                                                            
                                                                                
* =============================================================                 
* NOW ADD AN 0AE1...(NEWSEQNUM) TO POINT TO OLD RECORD                          
* =============================================================                 
                                                                                
         USING NPTDATA,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AE1'       GET OLD PASSIVE                            
         MVC   KEY+2(3),SVXNKEY+2     A/M & BCLT                                
         MVC   KEY+5(3),HOLDSEQ        NEW SEQ NO                               
         MVC   KEY+36(4),SVXNKEY+36    MOVE IN "OLD" DISK ADDR                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'XSPDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
*===================================================================            
* AND NOW REREAD THE RECORD BEING UPDATED INTO AIO3                             
* TO LEAVE NEW VERSION IN AIO1                                                  
*===================================================================            
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(32),SVXNKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
*===================================================================            
* AND LAST - PUT THE NEW REFERENCE NUMBER IN THE OLD RECORD                     
*===================================================================            
                                                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NPTS3QNO,HOLDSEQ                                                 
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
*===============================================================                
* CHECK FOR ANY OTHER PATTERNS FOR THIS CLT/PRD WITH SAME                       
* MARKET/STATION LIST AND OVERLAPPING DATES                                     
*===============================================================                
                                                                                
CHKOV    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
*        TM    NPTSTAT,X'80'       IS THIS PATTERN DELETED                      
*        BO    CHKOVX              YES, NO DATE OVERLAP CK NEEDED               
         LA    R2,NPTSTART         R2 = THIS REC DATES                          
         MVC   MYPTNSTM(4),NPTSTIM   SAVE START/END TIMES                       
*                                                                               
         LA    R3,KEY                                                           
         USING NPTXKEY,R3                                                       
         MVC   NPTXKEY,SVXNKEY                                                  
         MVI   NPTXR3F,X'A0'       START PAST SAVED PTNS                        
         GOTO1 HIGH                                                             
*                                                                               
CHKOV02  CLC   NPTXKEY(NPTXR3F-NPTXKEY),KEYSAVE SAME THRU PRDS                  
         BNE   CHKOVX                           NO                              
*                                                                               
         L     R4,AIO1                                                          
**NOP**  CLC   KEY(32),0(R4)                                                    
         CLC   NPTXR3F,NPTXR3F-NPTXKEY(R4)   TEST SAME XREF                     
         BNE   CHKOV10             NO                                           
*                                                                               
CHKOV4   GOTO1 SEQ                                                              
         B     CHKOV02                                                          
*                                                                               
CHKOV10  L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLI   NPTXNET-NPTKEY(R6),C'$' TEST A LIST RECORD                       
         BNE   CHKOV16                                                          
                                                                                
* NEED TO FIND NETEL TO TEST FOR DELETED NET                                    
                                                                                
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CHKOV16                                                          
*                                                                               
CHKOV12  CLC   NPTXNET-NPTKEY+KEY(4),2(R6) MATCH NETWORK                        
         BE    CHKOV14                                                          
         BRAS  RE,NEXTEL                                                        
         BE    CHKOV12                                                          
         B     CHKOV16                                                          
*                                                                               
CHKOV14  TM    6(R6),X'80'         TEST DELETED                                 
         BO    CHKOV30                                                          
*                                                                               
CHKOV16  L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
         TM    NPTSTAT,X'80'       IS THIS PATTERN DELETED                      
         BO    CHKOV30             YES, NO DATE OVERLAP CK NEEDED               
         LA    R5,NPTSTART         SAVE IF ERROR                                
         CLC   0(3,R5),3(R2)       THIS START AFTER OTHER END                   
         BH    CHKOV30             CAN'T OVERLAP                                
         CLC   3(3,R5),0(R2)       THIS END BEFORE OTHER START                  
         BL    CHKOV30                                                          
                                                                                
* NEED TO CHECK START/END TIMES (IF THEY'RE IN BOTH RECORDS)                    
                                                                                
         TM    NPTSTAT,NPTS_TIME   TEST OLD PATTERN HAS TIME IN IT              
         BZ    DTOVERR             NO - SO IT DOES OVERLAP                      
         OC    NPTSTIM(4),NPTSTIM  TEST OLD HAS TIME                            
         BZ    DTOVERR                                                          
*                                                                               
         MVC   MYADJSTM(4),MYPTNSTM   MOVE MY START/END TIMES                   
         MVC   MYADJSDT(6),0(R2)      MOVE MY START/END DATES                   
*                                                                               
         MVC   PTADJSTM(4),NPTSTIM    MOVE FILE START/END TIMES                 
         MVC   PTADJSDT(6),0(R5)      MOVE FILE START/END DATES                 
*                                                                               
         LA    R0,4                                                             
         LA    R1,MYADJSTM                                                      
         LA    RF,MYADJSDT                                                      
*                                                                               
         BAS   RE,ADJTIM              ADJUST TO 24 HR CLOCK                     
         LA    R1,2(R1)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,*-12                                                          
*                                                                               
CHKOV20  CLC   MYADJSDT,PTADJEDT      MY START DATE = FILE END DATE             
         BE    CHKOV22                                                          
         CLC   MYADJEDT,PTADJSDT      MY END DATE= FILE START DATE              
         BNE   DTOVERR                NO - THEN PATTERNS OVERLAP                
*                                                                               
CHKOV22  CLC   MYADJSDT,PTADJEDT      MY START DATE = FILE END DATE             
         BNE   CHKOV24                NO                                        
         CLC   MYADJSTM,PTADJETM      THEN I SHOULD START AFTER                 
         BH    CHKOV30                                                          
         CLC   NPTSTART,NPTEND        TEST PATTERN ALL ON 1 DAY                 
         BNE   DTOVERR                                                          
         CLC   MYADJETM,PTADJSTM      THEN IT'S OK TO END BEFORE                
         BL    CHKOV30                                                          
         B     DTOVERR                                                          
*                                                                               
CHKOV24  CLC   MYADJEDT,PTADJSDT      MY END DATE = FILE START DATE             
         BNE   CHKOV26                                                          
         CLC   MYADJETM,PTADJSTM      THEN I SHOULD END BEFORE                  
         BL    CHKOV30                                                          
         CLC   NPTSTART,NPTEND        TEST PATTERN ALL ON 1 DAY                 
         BNE   DTOVERR                                                          
         CLC   MYADJSTM,PTADJETM      THEN IT'S OK TO START AFTER               
         BL    CHKOV30                                                          
         B     DTOVERR                                                          
*                                                                               
CHKOV26  CLC   0(6,R2),0(R5)       DATES SHOULD NOT BE THE SAME                 
         BE    DTOVERR                                                          
*                                                                               
CHKOV30  GOTO1 SEQ                                                              
         B     CHKOV02                                                          
*                                                                               
CHKOVX   MVC   AIO,AIO1            RESTORE ORIGINAL IOA ADDRESS                 
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
DTOVERR  MVC   GERROR,=Y(DATOVLAP)                                              
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         STCM  R3,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R3),5             L'SUBST TEXT + 1                             
         L     R4,AIO                                                           
         USING NPTXKEY,R4                                                       
         SR    R1,R1                                                            
         ICM   R1,7,NPTXR3F                                                     
         DROP  R4                                                               
         X     R1,=XL4'00FFFFFF'                                                
         EDIT  (R1),(4,1(R3))                                                   
         LA    R3,5(R3)                                                         
*                                                                               
         MVI   0(R3),9                                                          
         GOTO1 DATCON,DMCB,(3,(R5)),(5,1(R3)) START DATE                        
         LA    R3,9(R3)                                                         
*                                                                               
         CLI   3(R5),X'FF'         IS THIS A UFN DATE                           
         BNE   DTOVERR2                                                         
         MVI   0(R3),4                                                          
         MVC   1(3,R3),=CL3'UFN'                                                
         B     DTOVERRA                                                         
*                                                                               
DTOVERR2 MVI   0(R3),9                                                          
         GOTO1 (RF),(R1),(3,3(R5)),(5,1(R3)) END DATE                           
*                                                                               
DTOVERRA LA    R2,TRAPERH          POINT TO ERROR FIELD                         
         GOTO1 VTRAERR                                                          
*                                                                               
ADJTIM   NTR1                                                                   
         LH    R0,0(R1)                                                         
         CHI   R0,2400                                                          
         BL    *+6                                                              
         XR    R0,R0               MIDNIGHT BECOMES 0                           
         STH   R0,0(R1)                                                         
         B     ADJTIMX             NOP FOR CALENDAR DAYS                        
*                                                                               
         LH    R0,0(R1)  <<NOP>>                                                
* NOW NEED TO BACK UP DATE TO PREVIOUS DAY                                      
         LR    R2,RF                  SAVE ADDRESS OF DATE FIELD                
         GOTO1 DATCON,DMCB,(3,(R2)),WORK                                        
         LHI   R0,-1                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,DMCB,WORK+6,(3,(R2))                                      
ADJTIMX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
VNET     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         CLI   FMEDIA,C'Y'         JUST FIND MEDIA                              
         BE    VNET12                                                           
*                                                                               
         XC    NETWORK,NETWORK                                                  
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VNET2                                                            
                                                                                
* CHECK FOR STATION GROUP FOR ACTION LIST                                       
                                                                                
         CLI   8(R2),C'A'                                                       
         BL    VNET2                                                            
         CLI   8(R2),C'Z'                                                       
         BH    VNET2                                                            
         LLC   R0,5(R2)                                                         
         AHI   R0,-1               GIVES REMAINING CHARS                        
         BNP   VNET2                                                            
         LA    R1,9(R2)                                                         
*                                                                               
VNET1    CLI   0(R1),C'0'                                                       
         BL    VNET2                                                            
         CLI   0(R1),C'9'                                                       
         BH    VNET2                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VNET1                                                         
         B     VNET30              IT'S A STATION GROUP!                        
*                                                                               
VNET2    CLC   =C'M=',8(R2)        PATTERN BY MEDIA REC?                        
         BNE   VNET5                                                            
         CLI   5(R2),3                                                          
         BNE   BADMED                                                           
         CLI   TRAPROGH+5,0        ANY PROGRAM                                  
         BNE   *+12                 YES                                         
         CLI   TRAFEEDH+5,0        ANY FEED                                     
         BE    *+14                 NO                                          
         MVC   GERROR,=Y(NOPGMFED)                                              
         B     VNETEREX2                                                        
*                                                                               
*MNV                                                                            
         CLI   10(R2),C'V'         DIGITAL                                      
         BNE   VNET2A                                                           
         TM    FLAGFTR,FLAGDIGI    OPTION TO INCLUDE DIGITAL                    
         BO    VNET3                                                            
         CLI   SVTN2PRO+14,C'Y'                                                 
         BNE   VNET3                                                            
         MVC   GERROR,=Y(BDNETMED)                                              
         B     VNETEREX2                                                        
*MNV                                                                            
VNET2A   CLI   10(R2),C'N'         NETWORK                                      
         BE    VNET3                                                            
         CLI   10(R2),C'D'         NETWORK RADIO                                
         BE    VNET3                                                            
         CLI   10(R2),C'H'         HISPANIC                                     
         BE    VNET3                                                            
         CLI   10(R2),C'C'                                                      
         BE    VNET3                                                            
         CLI   10(R2),C'S'         SYNDICATION                                  
         BE    VNET3                                                            
         CLI   10(R2),C'O'         OTHER                                        
         BNE   BADMED                                                           
*                                                                               
* PUT MEDIA IN NETWORK PART OF KEY AS  X'00MMFF00' TO FORCE SORT                
* ORDER OF 1) PATTERNS BY 'ALL' NET                                             
*          2) PATTERNS BY MEDIA                                                 
*          3) PATTERNS BY SPECIFIC NET                                          
*                                                                               
VNET3    DS    0H                                                               
         MVC   NETWORK+1(1),10(R2)                                              
         MVI   NETWORK+2,X'FF'                                                  
         B     VNETX                                                            
*                                                                               
VNET5    CLI   5(R2),0             ANY INPUT                                    
         BNE   VNET10                                                           
         CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VNETX                                                            
         CLI   TRAPROGH+5,0        PROGRAM                                      
         BNE   MISNETER                                                         
         CLI   TRACODEH+5,0        DAYPART CODE                                 
         BNE   MISNETER                                                         
         CLI   TRAFEEDH+5,0        FEED                                         
         BNE   MISNETER                                                         
         B     VNETX                                                            
*                                                                               
VNET10   CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VNET15              INVALID NETWORK                              
         GOTO1 ANY                                                              
         MVC   NETWORK,WORK                                                     
*                                                                               
VNET12   MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    *+14                                                             
VNET15   MVC   GERROR,=Y(NONET)                                                 
         B     VNETEREX2                                                        
*                                                                               
         L     R4,AIO                                                           
         CLI   FMEDIA,C'Y'         JUST FIND MEDIA                              
         BE    VNET20                                                           
*                                                                               
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,NETMKT                                                        
*MNV     B     VNETX                                                            
*                                                                               
VNET20   DS    0H                                                               
         MVC   SVMEDIA,STRTYPE                                                  
         B     VNETX                                                            
         EJECT                                                                  
*===========================================================                    
* VALIDATE STATION GROUP FOR ACTION LIST                                        
*===========================================================                    
                                                                                
VNET30   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D05'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(1),8(R2)      DATA IS LIKE Q1234                           
         MVC   WORK(4),=C'0000'                                                 
         LLC   RE,5(R2)            GET INPUT LEN                                
         BCTR  RE,0                                                             
         BCTR  RE,0                GIVES DIGITS-1                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),9(R2)                                                    
         PACK  DUB(3),WORK(5)      PACK F1F2F3F4XY --> 1234YX                   
         MVC   KEY+4(2),DUB                                                     
         MVC   SVSTAGRP,KEY+3      SAVE REQUESTED GROUP                         
*                                                                               
         BRAS  RE,INITSPT                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VNETX                                                            
         MVC   GERROR,=Y(NOSTAGRP)                                              
         B     VNETEREX2                                                        
*                                                                               
VNETX    XIT1                                                                   
*                                                                               
BADMED   MVC   GERROR,=Y(BADMEDIA)                                              
         B     VNETEREX2                                                        
*                                                                               
MISNETER MVC   GERROR,=Y(NETREQD)                                               
*                                                                               
VNETEREX2 GOTO1 VTRAERR                                                         
*                                                                               
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE PROGRAM *                                                            
*                                                                               
VPR      NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         XC    PROGRAM,PROGRAM                                                  
         XC    SVPROG,SVPROG                                                    
         CLI   5(R2),0                                                          
         BE    VPRX                                                             
*                                                                               
         OC    NETWORK,NETWORK                                                  
         BNZ   *+16                                                             
         LA    R2,TRANETH                                                       
         MVI   ERROR,MISSING                                                    
         B     VPRERX                                                           
*                                                                               
         GOTO1 ANY                                                              
         MVC   PROGRAM,WORK                                                     
*                                                                               
* GET PROGRAM INFO                                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,PROGRAM                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+14                                                             
         MVC   GERROR,=Y(NOPROG)                                                
         B     VPRER2X                                                          
*                                                                               
         BRAS  RE,INITNET                                                       
*                                                                               
         XC    KEY,KEY                                                          
         USING PGEKEY,R4                                                        
         MVI   PGEKID,X'24'                                                     
         MVC   PGEKAM(3),BAGYMD                                                 
         MVC   PGEKNET,NETWORK                                                  
         MVC   PGEKPRG,PROGRAM                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(14),KEYSAVE                                                  
         BNE   *+14                                                             
         MVC   GERROR,=Y(NOEQUIV)                                               
         B     VPRER2X                                                          
*                                                                               
         BRAS  RE,INITSPT                                                       
*                                                                               
VPRX     XIT1                                                                   
VPRERX   GOTO1 ERREX                                                            
VPRER2X  GOTO1 VTRAERR                                                          
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE DAYPART CODE *                                                       
*                                                                               
VCC      NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         MVI   QDPT,0              INIT DAYPART CODE                            
         XC    QDPT2,QDPT2                                                      
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VCCX                                                             
*                                                                               
VCC30    CLI   5(R2),2                                                          
         BNH   *+14                                                             
         MVC   GERROR,=Y(INVDPT)                                                
         B     VCCERX                                                           
*                                                                               
         OC    PROGRAM,PROGRAM     CAN'T HAVE BOTH DAYPART AND PROGRAM          
         BZ    *+14                                                             
         MVC   GERROR,=Y(NOPGMCOD)                                              
         B     VCCERX                                                           
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         MVC   SVMYKEY,KEY         SAVE KEY                                     
         GOTO1 VALIDPT             VALIDATE DAYPART CODE                        
         MVC   KEY(L'SVMYKEY),SVMYKEY                                           
*                                                                               
*NOP     MVC   CODE,WORK                                                        
VCCX     XIT1                                                                   
*                                                                               
VCCERX   GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE FEED CODE *                                                          
*                                                                               
VFD      NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         XC    FEED,FEED                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFDX                                                             
*                                                                               
         CLI   5(R2),4                                                          
         BNH   *+14                                                             
         MVC   GERROR,=Y(BADFEED)                                               
         B     VFDERX                                                           
*                                                                               
         OC    PROGRAM,PROGRAM     CAN'T HAVE BOTH FEED AND PROGRAM             
         BZ    *+14                                                             
         MVC   GERROR,=Y(NOFEDPGM)                                              
         B     VFDERX                                                           
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
* VALIDATE FEED EXISTS *                                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING FEEDKEY,R4                                                       
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,BAGYMD                                                   
         MVC   FEEDKNET,NETWORK                                                 
         MVC   FEEDKCLT,BCLT                                                    
         MVC   FEEDKFD,WORK                                                     
         LA    RE,FEEDKFD+3                                                     
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         MVI   0(RE),C' '                                                       
         BCT   RE,*-12                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VFD10                                                            
         MVC   KEY,KEYSAVE                                                      
         LA    R4,KEY                                                           
         XC    FEEDKCLT,FEEDKCLT                                                
         DROP  R4                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+14                                                             
         MVC   GERROR,=Y(NOFEED)                                                
         B     VFDERX                                                           
*                                                                               
VFD10    MVC   FEED,WORK                                                        
VFDX     XIT1                                                                   
*                                                                               
VFDERX   GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE FILTERS *                                                            
*                                                                               
VFTR     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         XC    FILTERS,FILTERS                                                  
         OI    FLAGFTR,X'20'                                                    
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,3                                                             
         B     VFTR04                                                           
VFTR02   LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VFTR08                                                           
VFTR06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRHELP),FTRHELP                                       
         GOTO1 ERREX2                                                           
*                                                                               
VFTR08   GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(7,BLOCK+64)                          
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         JZ    MISSERR             NO                                           
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
VFTR10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   *+12                NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
* GET ADDRESS OF FILTER VALIDATION RTN                                          
*                                                                               
         LA    RF,FLTTABLE                                                      
         EX    R1,FLTRCLC                                                       
         BE    FLTRGO                                                           
         LA    RF,L'FLTTABLE(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-16                                                             
         B     VFTR100                                                          
*                                                                               
FLTRCLC  CLC   12(0,R4),0(RF)                                                   
FLTRGO   L     RE,10(RF)                                                        
         A     RE,SPTR61RR                                                      
         BR    RE                                                               
         EJECT                                                                  
* FILTER BY DATE(S) *                                                           
*                                                                               
FLTDATE  LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         ICM   R5,15,DMCB          WAS DATE VALID                               
         JZ    DATERR              NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,DATEFTR1)                                
         CLM   R5,1,1(R4)          WAS SECOND DATE ENTERED                      
         BL    VFTR16              YES                                          
         MVC   DATESFTR,HOLDSIGN                                                
         B     VFTR90                                                           
VFTR16   LA    R5,23(R4,R5)                                                     
         GOTO1 DATVAL,(R1),(0,(R5)),DATE                                        
         OC    DMCB,DMCB           WAS DATE VALID                               
         JZ    DATERR              NO                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,DATEFTR2)                                
         CLC   DATEFTR1,DATEFTR2   1ST MUST BE LESS OR EQ TO 2ND                
         JH    DATERR                                                           
         B     VFTR90                                                           
*                                                                               
* FILTER BY PRODUCT GROUP                                                       
*                                                                               
FLTPGRP  DS    0H                                                               
*                                                                               
         CLI   TRAPRLNH+5,0        WAS PRODUCT ENTERED                          
         BNE   PRDPGRER            YES, ERROR                                   
*                                                                               
         BAS   RE,VPGRP            VALIDATE PGROUP                              
         B     VFTR90                                                           
*                                                                               
PRDPGRER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PRDPGRMS),PRDPGRMS                                     
         B     ERREXIT                                                          
PRDPGRMS DC    C'* ENTER PRODUCT OR PRODUCT GROUP BUT NOT BOTH *'               
*                                                                               
* FILTER BY COMMERCIAL *                                                        
*                                                                               
FLTCML   CLI   1(R4),8             CML ID MUST BE 8 CHAR                        
         BL    CMLLENER                                                         
         CLI   1(R4),12                                                         
         BH    CMLLENER                                                         
*                                                                               
* RESET FILES TO SPOT *                                                         
*                                                                               
         MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
*                                                                               
         MVC   CMLFTR,22(R4)                                                    
         MVC   CMLFTREBC,22(R4)   SAVE UNPACKED INPUT FOR CLC                   
         MVI   CMLFTRADI,C'N'                                                   
         GOTO1 VTRPACK,DMCB,(C'P',22(R4)),CMLFTR                                
         BNE   *+8                                                              
         MVI   CMLFTRADI,C'Y'                                                   
*                                                                               
         XC    KEY(13),KEY                                                      
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=X'0A21'                                                  
         CLI   CMLFTRADI,C'Y'                                                   
         BNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
         MVC   CMLKCML,CMLFTR                                                   
         DROP  R1                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   VCMLERR                                                          
         B     VFTR90                                                           
*                                                                               
* FILTER BY PERIOD *                                                            
*                                                                               
FLTPER   LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DATE                                        
         L     RE,DMCB             GET LENGTH OF FIELD                          
         LTR   RE,RE                                                            
         JZ    DATERR                                                           
         LA    R5,1(RE,R5)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,DATE),(3,PERFTR)                                  
         GOTO1 DATVAL,(R1),(0,(R5)),DATE                                        
         OC    DMCB(4),DMCB                                                     
         JZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,PERFTR+3)                                
         CLC   PERFTR(3),PERFTR+3                                               
         JH    DATERR                                                           
         B     VFTR90                                                           
*                                                                               
* FILTER ON SOFT DELETED RECORDS *                                              
*                                                                               
FLTDEL   OI    FLAGFTR,FLAGDEL                                                  
         B     VFTR90                                                           
*                                                                               
* FILTER ON BOTH DELETED RECORDS & UNDELETED PTNS *                             
*                                                                               
FLTALL   DS   0H                                                                
         OI    FLAGFTR,FLAGALL      SET ON BOTH UNDEL & DELTED                  
         B     VFTR90                                                           
*MNV                                                                            
*                                                                               
* INCLUDE DIGITAL PATTERNS                                                      
*                                                                               
FLTDIGI  DS   0H                                                                
         OI    FLAGFTR,FLAGDIGI     INCLUDE DIGITAL PATTERN                     
         B     VFTR90                                                           
*MNV                                                                            
*                                                                               
* DO ERROR REPORT ONLY *                                                        
*                                                                               
FLTERRSW TM    WHEN,X'80'          IMMEDIATE                                    
         BO    FLTERR              ERROR, ITS A REPORT ONLY FEATURE             
*                                                                               
         OI    FLAGFTR,FLREPORT                                                 
         B     VFTR90                                                           
*                                                                               
* SHOW PAT SEQ # *                                                              
*                                                                               
FLTSEQSW DS   0H                   IMMEDIATE                                    
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERM                              
         BNE   VFTR100                                                          
         OI    FLAGFTR,FLAGSEQ                                                  
         B     VFTR90                                                           
*                                                                               
FLTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTERRMSG),FTERRMSG                                     
ERREXIT  GOTO1 ERREX2                                                           
FTERRMSG DC    C'* ERROR * RUN IT NOW/SOON/OV'                                  
         EJECT                                                                  
* SORT ON DATES OR REF *                                                        
*                                                                               
FLTSORT  OI    FLAGFTR,X'40'                                                    
         CLC   =C'DATE',22(R4)                                                  
         BE    VFTR90                                                           
         CLC   =C'REF',22(R4)                                                   
         BNE   SORTERR                                                          
         NI    FLAGFTR,X'FF'-X'20'                                              
         B     VFTR90                                                           
*                                                                               
* FILTER ON DAYPART CODE *                                                      
*                                                                               
FLTCODE  LLC   R1,1(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CODEFTR(0),22(R4)                                                
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),22(R4)       DAYPART                                      
         LA    R1,1(R1)                                                         
         STC   R1,FLDH+5           DATA LEN                                     
         LA    R2,FLDH                                                          
*                                                                               
         MVC   SVMYKEY,KEY         SAVE KEY                                     
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALIDPT             VALIDATE DAYPART CODE                        
         MVC   KEY(L'SVMYKEY),SVMYKEY                                           
*                                                                               
         MVI   ERROPT,0                                                         
         OC    GERROR,GERROR                                                    
         BZ    VFTR90                                                           
*                                                                               
         LA    R2,TRAFLTRH         FILTERS                                      
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         STCM  R1,7,GASUBST                                                     
         MVI   DUB,13              L'SUBST TEXT + 1                             
         MVC   DUB+1(12),=C'DAYPART CODE'                                       
         GOTO1 VTRAERR                                                          
*                                                                               
FLTSEND  CLI   1(R4),3             SEND=ALL, DOWN=ALL, OR =UPDATE               
         BNE   FLTSEND2                                                         
         CLC   =C'ALL',22(R4)                                                   
         BNE   FLTSEND2                                                         
         OI    FLAGFTR,FLAGDOWN    SET TO DO UPDATES ONLY                       
         B     VFTR90                                                           
*                                                                               
FLTSEND2 LLC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R4),=C'UPDATE'                                              
         BNE   FLTSEND4                                                         
         B     VFTR90                                                           
*                                                                               
FLTSEND4 MVC   GERROR,=Y(BADSEND)                                               
         GOTO1 VTRAERR                                                          
*                                                                               
VFTR90   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
         BRAS  RE,INITXSP          SET FOR XFILE                                
VFTRX    XIT1                                                                   
*                                                                               
VFTR100  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG+L'FTRHELP),FTRMSG                               
         B     VFTRERX                                                          
*                                                                               
SORTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SORTERMS),SORTERMS                                     
*                                                                               
VFTRERX  GOTO1 ERREX2                                                           
*                                                                               
CMLLENER MVC   GERROR,=Y(NOT812)     CMML MUST BE 8-12 CHAR                     
         GOTO1 VTRAERR                                                          
*                                                                               
FTRMSG   DC    C'* ERROR *'                                                     
FTRHELP  DC    C'FILTERS-DELETED,DATE/CODE/CML/PER/SORT=/PGR'                   
SORTERMS DC    C'* ERROR * SORT = DATE OR REF'                                  
FLTTABLE DS    0CL14                                                            
         DC    CL10'ALL       ',AL4(FLTALL)                                     
         DC    CL10'DATE      ',AL4(FLTDATE)                                    
         DC    CL10'CML       ',AL4(FLTCML)                                     
         DC    CL10'CMML      ',AL4(FLTCML)                                     
         DC    CL10'PER       ',AL4(FLTPER)                                     
         DC    CL10'PGROUP    ',AL4(FLTPGRP)                                    
         DC    CL10'DELETED   ',AL4(FLTDEL)                                     
         DC    CL10'SORT      ',AL4(FLTSORT)                                    
         DC    CL10'CODE      ',AL4(FLTCODE)                                    
         DC    CL10'ERROR     ',AL4(FLTERRSW)                                   
         DC    CL10'SEQ       ',AL4(FLTSEQSW)                                   
         DC    CL10'SEND      ',AL4(FLTSEND)                                    
         DC    CL10'DOWN      ',AL4(FLTSEND)                                    
*MNV                                                                            
         DC    CL10'DIGITAL   ',AL4(FLTDIGI)                                    
*MNV                                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* VALIDATE PRODUCT GROUP *                                                      
*                                                                               
VPGRP    NTR1                                                                   
         MVI   BYTE,C'2'           SET TN2 PROFILE ONLY                         
         BRAS  RE,FPRO                                                          
*                                                                               
         LA    R0,PGRLIST          PGROUP PRODUCT LIST                          
         LA    R1,PGRLISTX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   22(R4),C'A'         TEST SCHEME ENTERED                          
         BL    VPGR02                                                           
         CLI   22(R4),C'Z'                                                      
         BH    VPGR02                                                           
         MVC   QPGR,22(R4)         SAVE PROD GROUP SCHEME                       
         MVC   SVTN2PR1,22(R4)     SAVE FOR VALIPGR                             
*                                                                               
         IC    R0,1(R4)            GET INPUT STRING LEN                         
         BCTR  R0,0                ADJUST FOR SCHEME CODE                       
         STC   R0,1(R4)                                                         
         ICM   R1,15,23(R4)        GET REMAINING STRING INPUT                   
         MVC   22(10,R4),SPACES                                                 
         STCM  R1,15,22(R4)        AND SET AS INPUT                             
*                                                                               
VPGR02   GOTO1 VALIPGR                                                          
*                                                                               
         GOTO1 HIGH                ON RETURN, KEY HAS FIRST PRDGRP              
*                                                                               
         LA    R0,NPGRLIST                                                      
         LA    R5,PGRLIST                                                       
*                                                                               
VPGR25   MVC   SVMYKEY,KEY         SAVE KEY                                     
*                                                                               
         LA    R2,ELEM             FAKE VALIPRD                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0B01000184030001'                                     
         MVC   ELEM+8(3),KEY+8                                                  
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BE    *+6                  NO                                          
         DC    H'0'                INVALID PRODUCT                              
*                                                                               
         MVC   KEY(L'SVMYKEY),SVMYKEY  RESTORE KEY                              
         GOTO1 HIGH                DUMMY HI FOR SEQ                             
*                                                                               
         LA    RE,WORK             3 CHAR PROD                                  
         MVC   0(3,R5),0(RE)       MOVE 3 CHAR PROD TO TABLE                    
         LA    R5,3(,R5)                                                        
         BCT   R0,VPGR44                                                        
         DC    H'0'                TOO MANY PRODUCTS FOR TABLE                  
*                                                                               
VPGR44   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(8),KEYSAVE                                                   
         BE    VPGR25                                                           
         B     VFTRX                                                            
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE REFERENCE NUMBER                                                     
*                                                                               
VREF     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         XC    BREF,BREF                                                        
         XC    BREFK,BREFK                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VREF10               YES                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VREFX                                                            
         CLI   ACTNUM,ACTADD       OR ADD                                       
         JNE   MISSERR              NO, MUST BE                                 
*                                                                               
VREF10   BRAS  RE,INITXSP          XFILE                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPTXKEY,R4                                                       
*                                                                               
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM(3),BAGYMD    A/M,CLT                                      
         MVC   NPTXNET,NETWORK                                                  
*                                                                               
         CLI   QDPT,0              THIS REALLY DAYPART                          
         BNE   VREF12               YES                                         
         OC    FEED,FEED           OR FEED                                      
         BNZ   VREF12               YES                                         
         MVC   NPTXPROG,PROGRAM                                                 
         B     VREF14                                                           
*                                                                               
VREF12   MVI   NPTXPROG,X'FF'                                                   
         MVC   NPTXPROG+1(1),QDPT                                               
         MVC   NPTXPROG+2(4),FEED                                               
         OC    PROGRAM,PROGRAM                                                  
         BZ    VREF14                                                           
         DC    H'0'                                                             
*                                                                               
VREF14   MVC   NPTXPRD,QPRD        MOVE IN 3 CHAR PROD                          
         MVC   NPTXSLN,BSLN        PRD LENGTH                                   
         CLC   QPRD2,SPACES                                                     
         BNH   *+16                                                             
         MVC   NPTXPRD2,QPRD2      3 CHAR PARTNER PRD                           
         MVC   NPTXSLN2,BSLN2      PARTNER LENGTH                               
*                                                                               
VREF20   CLI   ACTNUM,ACTADD       IS THIS ADD                                  
         BNE   VREF30                                                           
*                                                                               
         MVI   NPTXR3F,X'A0'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',KEYSAVE,KEY, C        
               DMWORK                                                           
         CLC   KEY(23),KEYSAVE                                                  
         BE    VREF62                                                           
         B     VREF61                                                           
*                                                                               
VREF30   TM    4(R2),X'08'         WAS IT NUMERIC                               
         BZ    NUMERRB                                                          
         GOTO1 ANY                                                              
         CLI   5(R2),6             ALLOW 6 DIGITS                               
         BH    NUMERRB                                                          
         LLC   R1,5(R2)            GET DATA LENGTH                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   R0,DUB                                                           
         ST    R0,BREF                                                          
         X     R0,=XL4'00FFFFFF'                                                
         ST    R0,BREFK                                                         
         STCM  R0,7,NPTXR3F                                                     
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',KEYSAVE,KEY, C        
               DMWORK                                                           
         CLC   KEY(32),KEYSAVE                                                  
         BNE   NOTFNDER                                                         
*                                                                               
         OI    4(R2),X'20'         SET ON VALIDATED                             
         B     VREFX                                                            
*                                                                               
VREF61   LA    R0,1                                                             
         ST    R0,BREF                                                          
         B     VREF64                                                           
*                                                                               
VREF62   SR    R1,R1                                                            
         ICM   R1,7,KEY+NPTXR3F-NPTKEY    GET THIS REFNUM                       
         X     R1,=XL4'00FFFFFF'                                                
         LA    R1,1(R1)            ADD 1                                        
         ST    R1,BREF                                                          
*                                                                               
VREF64   MVC   BREFK,BREF                                                       
         XC    BREFK,=X'00FFFFFF'                                               
         L     R1,BREF                                                          
         EDIT  (R1),(5,TRAREF),ALIGN=LEFT                                       
         OI    TRAREFH+6,X'80'                                                  
*                                                                               
VREFX    XIT1                                                                   
*                                                                               
NOTFNDER MVI   ERROR,NOTFOUND                                                   
         J     TRAPERR                                                          
*                                                                               
BIGERR   MVI   ERROR,INVREFSZ      REF NUMBER TOO LARGE                         
         J     TRAPERR                                                          
*                                                                               
NUMERRB  MVI   ERROR,NOTNUM                                                     
         J     TRAPERR                                                          
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* ONLINE LIST LOGIC                                                             
*===========================================================                    
                                                                                
LISTR    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITXSP          SET FOR XFILE                                
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    KEY,KEY             1ST TIME                                     
         BZ    LISTR02             YES                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     LISTR06                                                          
*                                                                               
LISTR02  LA    R4,KEY                                                           
         USING NPTXKEY,R4                                                       
*                                                                               
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM(3),BAGYMD    A/M,CLT                                      
*                                                                               
         CLC   NETWORK,SPACES                                                   
         BNH   LISTR04                                                          
         MVC   NPTXNET,NETWORK                                                  
*                                                                               
         CLC   TRAPROG,SPACES                                                   
         BNH   LISTR04                                                          
         MVC   NPTXPROG,TRAPROG                                                 
*                                                                               
         CLC   TRAPRLN,SPACES                                                   
         BNH   LISTR04                                                          
         MVC   NPTXPRD,QPRD                                                     
         MVC   NPTXSLN,BSLN                                                     
*                                                                               
LISTR04  MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     LISTR06                                                          
*                                                                               
LISTRSEQ MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
LISTR06  CLC   KEY(3),KEYSAVE      SAME TYPE/A-M                                
         BNE   LISTRX                                                           
*                                                                               
         OC    KEY+5(27),KEY+5     THIS A PTN SEQ REC                           
         BZ    LISTRSEQ                                                         
*                                                                               
         CLI   KEY+23,X'A0'        THIS SAVED PTN                               
         BL    LISTRSEQ                                                         
*                                                                               
         CLC   KEY(5),KEYSAVE      TYPE/A-M/CLT                                 
         BNE   LISTRX                                                           
*                                                                               
         BRAS  RE,FLIST            FILTER AGAINST KEY FIELDS                    
         BNE   LISTRSEQ                                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,FTR              FILTER AGAINST DATES, DELETED, ETC           
         BNE   LISTRSEQ                                                         
*                                                                               
*MNV                                                                            
         L     R4,AIO                                                           
                                                                                
         TM    FLAGFTR,FLAGDIGI    OPTION TO INCLUDE DIGITAL                    
         BO    LIST08                                                           
         CLI   SVTN2PRO+14,C'Y'    PROFILE TO EXCLUDE DIGITAL                   
         BNE   LIST08                                                           
                                                                                
         CLC   NPTXNET,=XL4'00'    NO NETWORK ON PATTERN                        
         BE    LIST08                                                           
                                                                                
         CLI   NPTXNET,C'$'        MEDIA PATTERN                                
         BE    LIST08                                                           
                                                                                
         CLI   NPTXNET+2,X'FF'     MEDIA PATTERN                                
         BNE   *+14                                                             
         MVC   SVMEDIA,NPTXNET+1                                                
         B     LIST07                                                           
                                                                                
         MVC   WORK(L'NPTXNET),NPTXNET    SET NETWORK                           
         MVI   FMEDIA,C'Y'                ONLY NEED MEDIA FROM MASTER           
         BRAS  RE,INITNET          POINT TO NET FILE                            
         XC    SVXNKEY,SVXNKEY     CLEAR AND SAVE OFF CURRENT                   
         MVC   SVXNKEY(L'NPTXKEY),NPTXKEY    PATTERN KEY                        
         BRAS  RE,VNET             GET TRAFFIC TYPE FROM MASTER RECORD          
         BRAS  RE,INITXSP          POINT TO XSPOT FILE                          
         MVC   KEY,SVXNKEY         RESTORE KEY                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                RESTORE READ                                 
         CLC   KEY(L'NPTXKEY),SVXNKEY                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET CURRENT PATTERN RECORD                   
                                                                                
LIST07   CLI   SVMEDIA,C'V'                                                     
         BE    LISTRSEQ                                                         
                                                                                
LIST08   DS    0H                                                               
*MNV                                                                            
         CLC   =C'DOWN',CONOUT                                                  
         BNE   *+12                                                             
         BRAS  RE,PRDOWN                                                        
         B     LISTRSEQ                                                         
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   FLD(3),NPTXPRD                                                   
         LA    R3,NPTXSLN                                                       
         BRAS  RE,FMTSLN                                                        
         MVC   LPRLN,FLD                                                        
*                                                                               
         OC    NPTXPRD2,NPTXPRD2                                                
         BZ    LISTR10                                                          
*                                                                               
         MVC   FLD(3),NPTXPRD2                                                  
         LA    R3,NPTXSLN2                                                      
         BRAS  RE,FMTSLN                                                        
         MVC   LPTLN,FLD                                                        
*                                                                               
LISTR10  CLI   NPTXNET,0           TEST FOR PATTERN BY MEDIA                    
         BNE   LISTR12                                                          
         CLI   NPTXNET+2,X'FF'                                                  
         BNE   LISTR12                                                          
*                                                                               
         MVC   LNET(2),=C'M='                                                   
         MVC   LNET+2(1),NPTXNET+1                                              
         B     LISTR13                                                          
*                                                                               
LISTR12  MVC   LNET,NPTXNET                                                     
         CLI   NPTXPSSV-NPTKEY+KEY,0          TEST PASSIVE KEY                  
         BE    LISTR13                        NO                                
         MVC   LNET,NPTXNET-NPTKEY+KEY        SHOW PASSIVE NETWORK              
         MVI   LPROG,C'$'                                                       
*                                                                               
LISTR13  SR    R0,R0                                                            
         ICM   R0,7,NPTXR3F                                                     
         X     R0,=XL4'00FFFFFF'                                                
         EDIT  (R0),(5,LREF),ALIGN=LEFT                                         
*                                                                               
LISTR14  MVI   ELCODE,X'10'                                                     
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         TM    NPTSTAT,NPTS_ADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         MVI   HASTIME,C'N'                                                     
         TM    NPTSTAT,NPTS_TIME   TEST TIME IN ELEMENT                         
         BZ    LISTR16                                                          
         OC    NPTSTIM(4),NPTSTIM                                               
         BZ    *+8                                                              
         MVI   HASTIME,C'Y'                                                     
*                                                                               
LISTR16  CLI   NPTXPROG,C' '       DON'T MOVE PROGRAM IF NONE                   
         BNH   *+10                                                             
         MVC   LPROG,NPTXPROG      MOVE PROGRAM NAME                            
*                                                                               
         CLI   NPTXPROG,X'FF'      TEST FOR DPT OR FEED                         
         BNE   LISTR30             NO                                           
*                                                                               
         MVC   LPROG,SPACES                                                     
         LA    R1,LPROG            SET DISPLAY AREA FOR DPT/FEED                
*                                                                               
         CLI   NPTXPROG+1,0               ANY DAYPART                           
         BNE   LISTR22                    YES                                   
*                                                                               
         OC    NPTXPROG+2(4),NPTXPROG+2   ANY FEED                              
         BZ    *+10                                                             
         MVC   0(4,R1),NPTXPROG+2                                               
         B     LISTR30                                                          
*                                                                               
LISTR22  CLI   NPTXPSSV-NPTKEY+KEY,0          TEST PASSIVE KEY                  
         BE    *+12                                                             
         MVI   0(R1),C'$'                                                       
         LA    R1,1(R1)                                                         
         MVC   0(1,R1),NPTXPROG+1  MOVE DPT FROM KEY                            
         LA    R1,1(R1)                                                         
         OC    NPTDPART,NPTDPART   TEST 2-CHAR DPT IN ELEM                      
         BZ    LISTR24                                                          
         BCTR  R1,0                                                             
         MVC   0(2,R1),NPTDPART    DAYPART CODE                                 
         LA    R1,2(R1)                                                         
*                                                                               
LISTR24  OC    NPTXPROG+2(4),NPTXPROG+2 ANY FEED                                
         BZ    LISTR30                                                          
         MVI   0(R1),C'/'               SET SEPERATOR                           
         MVC   1(4,R1),NPTXPROG+2       AND MOVE FEED                           
*                                                                               
LISTR30  GOTO1 DATCON,DMCB,(3,NPTSTART),(5,LPER)                                
         MVI   LPER+8,C'-'                                                      
         MVC   LPER+9(3),=CL3'UFN'                                              
         CLC   NPTEND,=XL3'FFFFFF'                                              
         BE    LISTR32                                                          
         GOTO1 (RF),(R1),(3,NPTEND),(5,LPER+9)                                  
*                                                                               
LISTR32  MVC   LDESC,NPTDESC                                                    
*                                                                               
         TM    NPTSTAT,NPTS_TIME   TEST TIME IN ELEMENT                         
         BZ    LISTR34                                                          
         OC    NPTSTIM,NPTSTIM     TEST TIME PRESENT                            
         BZ    LISTR34                                                          
         GOTO1 UNTIME,DMCB,NPTSTIM,LDELETE                                      
*                                                                               
LISTR34  TM    NPTSTAT,X'80'       SOFT DELETE                                  
         BZ    *+10                                                             
         MVC   LDELETE(10),=CL10'DELETED'                                       
*                                                                               
         L     RE,AIO1                                                          
         CLC   KEY(NPTXOR3G-NPTXKEY),0(RE)  KEY/REC MATCH KEYS                  
         BE    LISTR36                                                          
         MVI   LPROG,C'$'          SET LIST FLAG                                
*                                                                               
         BRAS  RE,CKPATLST         SEE IF NETWORK IS DELETED                    
         BE    LISTR36                                                          
         MVC   LDELETE(10),=CL10'DELETED'                                       
*                                                                               
LISTR36  TM    FLAGFTR,FLAGSEQ     SHOW PAT SEQ NO?                             
         BZ    LISTR40                                                          
         SR    R0,R0                                                            
         ICM   R0,7,NPTS3QNO                                                    
         EDIT  (R0),(4,LDESC+16),ZERO=NOBLANK                                   
*                                                                               
LISTR40  MVI   ELCODE,X'30'        CHECK FOR DELETED COMMERCIALS                
         BRAS  RE,NEXTEL                                                        
         BE    LISTR42                                                          
         MVC   LDELETE(10),=CL10'NO CMMLS'                                      
         B     LISTR50                                                          
*                                                                               
LISTR42  MVC   SVMYKEY,KEY                                                      
         BRAS  RE,CHKDEL           CHECK FOR DELETED COMMERCIALS                
         BE    LISTR50                                                          
         TM    CMLFLAG,X'20'+X'80' NETWORK SPECIFIC/EXCLUDED NETS               
         BZ    *+14                 NO                                          
         MVC   LDESC(14),=C'** CMML ERR **'                                     
         B     LISTR50                                                          
*                                                                               
         TM    CMLFLAG,X'40'       PRODUCT ERROR                                
         BZ    *+10                 NO                                          
         MVC   LDESC(13),=C'** PRD ERR **'                                      
*                                                                               
*                                                                               
LISTR50  MVC   KEY,SVMYKEY         RESTORE SEQ AFTER CHKDEL ROUTINE             
         GOTO1 HIGH                                                             
*                                                                               
         MVC   DMDSKADD,KEY+36     SAVE DISK ADDR FOR LISTDIR                   
         MVI   NLISTS,14                                                        
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LISTRSEQ                                                         
*                                                                               
LISTRX   CLC   =C'DOWN',CONOUT                                                  
         BNE   LISTRX2                                                          
         MVI   KEY,X'FF'                                                        
         BRAS  RE,PRDOWN                                                        
*                                                                               
LISTRX2  XC    KEY,KEY                                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* SEE IF NETWORK IN KEY+6 IS DELETED IN A 5B PATTERN LIST ELEM                  
* OR IF THE WHOLE FREAKING PATTERN IS DELETED! AAAAAAAAARGH!                    
*================================================================               
                                                                                
CKPATLST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    NPTSTAT-NPTDATA(R6),X'80'      SOFT DELETE                       
         JO    NEQXIT                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CKPAT2   BRAS  RE,NEXTEL                                                        
         JNE   NEQXIT                                                           
*                                                                               
         CLC   NPTXNET-NPTXKEY+KEY(4),2(R6)   RIGHT NETWORK ELEM                
         BNE   CKPAT2                                                           
*                                                                               
         TM    6(R6),X'80'         TEST NETWORK DELETED                         
         JO    NEQXIT                                                           
         J     EQXIT                                                            
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* FILTER KEY AGAINST ENTERED VALUES SAVED IN COMPKEY                            
* EXIT WITH CC EQ (PROCESS) OR NEQ (IGNORE)                                     
*============================================================                   
                                                                                
FLIST    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    COMPKEY+3(2),COMPKEY+3   CLIENT ENTERED                          
         BZ    *+14                     NO                                      
         CLC   COMPKEY+3(2),KEY+3       AT END OF CLIENT                        
         JNZ   NEQXIT                   YES                                     
*                                                                               
         OC    COMPKEY+5(4),COMPKEY+5   NETWORK ENTERED                         
         BZ    *+14                     NO                                      
         CLC   KEY+5(4),COMPKEY+5                                               
         JNE   NEQXIT                                                           
*                                                                               
         OC    COMPKEY+9(6),COMPKEY+9   PROGRAM ENTERED                         
         BZ    FLIST04                  NO                                      
*                                                                               
         CLI   COMPKEY+9,X'FF'          CODE/FEED                               
         BE    FLIST02                                                          
         CLC   KEY+9(6),COMPKEY+9                                               
         JNE   NEQXIT                                                           
         B     FLIST04                                                          
*                                                                               
FLIST02  CLI   KEY+9,X'FF'                                                      
         JNE   NEQXIT                                                           
*                                                                               
         CLI   COMPKEY+10,0             CODE                                    
         BE    *+14                                                             
         CLC   COMPKEY+10(1),KEY+10                                             
         JNE   NEQXIT                                                           
*                                                                               
         OC    COMPKEY+11(4),COMPKEY+11 FEED ENTERED                            
         BZ    FLIST04                  NO                                      
         CLC   COMPKEY+11(4),KEY+11                                             
         JNE   NEQXIT                                                           
*                                                                               
FLIST04  OC    COMPKEY+15(3),COMPKEY+15  PRD ENTERED                            
         BZ    *+14                      NO                                     
         CLC   KEY+15(3),COMPKEY+15                                             
         JNE   NEQXIT                                                           
*                                                                               
         CLI   COMPKEY+18,0              SLN ENTERED                            
         BE    *+14                      NO                                     
         CLC   KEY+18(1),COMPKEY+18                                             
         JNE   NEQXIT                                                           
*                                                                               
         OC    COMPKEY+19(3),COMPKEY+19  PRD2 ENTERED                           
         BZ    *+14                      NO                                     
         CLC   KEY+19(3),COMPKEY+19                                             
         JNE   NEQXIT                                                           
*                                                                               
         CLI   COMPKEY+22,0              SLN2 ENTERED                           
         BE    *+14                      NO                                     
         CLC   KEY+22(1),COMPKEY+22                                             
         JNE   NEQXIT                                                           
*                                                                               
         OC    COMPKEY+23(2),COMPKEY+23  REF ENTERED                            
         BZ    *+14                      NO                                     
         CLC   KEY+23(2),COMPKEY+23                                             
         JNE   NEQXIT                                                           
*                                                                               
         OC    SVSTAGRP,SVSTAGRP                                                
         BNZ   FLIST10                                                          
*                                                                               
FLISTX   J     EQXIT                                                            
                                                                                
* TEST THIS NETWORK IN STATION GROUP                                            
                                                                                
FLIST10  MVC   SVMYKEY,KEY               SAVE CURRENT KEY                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D85'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(4),SVMYKEY+NPTXNET-NPTXKEY                                 
         MVC   KEY+7(2),SPACES                                                  
*                                                                               
         CLC   KEY(7),SVGRPKEY     TEST SAME KEY AS PREVIOUS                    
         BE    FLIST22                                                          
*                                                                               
         MVI   SVGRPFLG,C'N'       ASSUME NOT IN GROUP                          
         OC    KEY+3(4),KEY+3      IF NO NETWORK                                
         BZ    FLIST22             THEN CAN'T BE IN STAGRP                      
         CLI   KEY+5,X'FF'         PATTERN BY MEDIA                             
         BE    FLIST22             YES - NO STAGRP                              
*                                                                               
         MVC   KEY+9(3),SVSTAGRP                                                
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+8                                                              
         MVI   SVGRPFLG,C'Y'       SET FOR EQ ON EXIT                           
*                                                                               
FLIST20  MVC   KEY,SVMYKEY         RESTORE CURRENT KEY (ON XSPDIR)              
         GOTO1 HIGH                RESTORE DIR                                  
         B     FLIST24                                                          
*                                                                               
FLIST22  MVC   KEY,SVMYKEY         USED IF NO KEY READ                          
*                                                                               
FLIST24  CLI   SVGRPFLG,C'Y'                                                    
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* GET PTN SEQ NO AND/OR CREATE NEW PTN SEQ REC FOR XFILE                        
*=============================================================                  
                                                                                
NGETSEQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   NEWSQXSW,0                                                       
         BRAS  RE,INITXSP                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPTXKEY,R4                                                       
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM(3),BAGYMD     A-M/CLT                                     
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO              USE AIO2!                                    
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   XGETSQ10                                                         
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   BYTE,ELCODE                                                      
         MVI   ELCODE,X'10'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDATA,R6                                                       
*                                                                               
         MVC   HOLDSEQ,NPTS3QNO                                                 
         B     XGETSQ30                                                         
*                                                                               
XGETSQ10 DS    0H                                                               
         XC    0(256,R6),0(R6)                                                  
*                                                                               
         MVI   NEWSQXSW,1          SET ON ADDED PTN SEQ SW                      
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   0(32,R6),KEY                                                     
         MVI   33(R6),84                                                        
*                                                                               
         MVC   42(2,R6),=X'102A'                                                
         MVC   42+2(18,R6),=CL18'PATTERN SEQ RECORD'                            
*                                                                               
         MVI   42+NPTS3QNO+1-NPTDATA(R6),2                                      
*                                                                               
         LA    R0,1                                                             
         STCM  R0,7,HOLDSEQ                                                     
*                                                                               
         MVI   BYTE,0                                                           
         BRAS  RE,AACTELEM         ADD F1 ELEM                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'XSPFIL',KEY,AIO2,DMWORK               
*                                                                               
XGETSQ30 L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   ELCODE,X'10'        NOW GET DATA ELEM                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
         MVC   NPTS3QNO,HOLDSEQ      AND PUT COMMERCIAL SEQ # IN IT             
*                                                                               
         MVC   KEY(L'SVNKEY),SVXNKEY  RESTORE KEY                               
*                                                                               
XGETSQX  XIT1                                                                   
         EJECT                                                                  
* ADD F1 ACTIVITY ELEMENT                                                       
*                                                                               
AACTELEM NTR1  BASE=*,LABEL=*                                                   
         CLI   BYTE,C'C'           CHANGE                                       
         BE    CHAACTIV             YES                                         
*                                                                               
         MVI   ELCODE,X'F1'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R6,ELEM                                                          
         USING ACTVD,R6                                                         
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET DATE                                     
         MVC   ACTVCHDT,BTODAY                                                  
         LA    R3,ACTVADID                                                      
         B     ACTIV6                                                           
*                                                                               
CHAACTIV DS    0H                                                               
         LA    R6,ELEM                                                          
*                                                                               
         MVI   ELCODE,X'F1'                                                     
         GOTO1 REMELEM                                                          
         CLI   ELEM,0                                                           
         BNE   CHAACT2                                                          
         MVI   ACTVEL,X'F1'        REPAIRING IF NO ELEMENT YET                  
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET DATE                                     
         MVC   ACTVADID,TWAORIG    AND ID                                       
         MVC   ACTVCHDT,BTODAY                                                  
*                                                                               
CHAACT2  LA    R3,ACTVCHID                                                      
         CLC   ACTVCHDT,BTODAY     WAS RECORD CHANGED TODAY                     
         BNE   ACTIV3                                                           
         CLI   ACTVCHNM,0          (ADDED TODAY)                                
         BNE   ACTIV6                                                           
*                                                                               
ACTIV3   MVC   ACTVCHDT,BTODAY     NO SO SET TODAYS DATE                        
         AI    ACTVCHNM,1          UP THE CHANGE NUMBER                         
         B     ACTIV6                                                           
*                                                                               
ACTIV6   MVC   0(2,R3),TWAORIG     MOVE IN ID                                   
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         BZ    ACTIV8                                                           
         MVC   0(2,R3),FAPASSWD    YES SO USE THIS ID                           
         OI    2(R3),X'80'                                                      
         DROP  R1                                                               
*                                                                               
ACTIV8   OC    ASECBLK,ASECBLK     IS NEW SECURITY ACTIVE                       
         BZ    ACTIV10                                                          
         L     R1,ASECBLK          NEW SECURITY BLOCK                           
         USING SECD,R1                                                          
         MVC   ACTVSCID,SECPID     USER'S PERSONAL ID                           
         MVI   ACTVLEN,ACTVLENQ    NEW LENGTH                                   
         DROP  R1                                                               
*                                                                               
ACTIV10  GOTO1 ADDELEM                                                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
* AFTER ADDED RECORD - ADD PASSIVE KEY                                          
* UPDATE SEQUENCE NUMBER (FOR XFILE)                                            
*                                                                               
XAAREC   NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         MVI   ERROR,0                                                          
         MVC   SVDSKAD,KEY        SAVE DISK ADDR OF ADDED REC                   
*                                                                               
         CLI   NEWSQXSW,1          DID WE JUST ADD PTN SEQ SW                   
         BE    XAAR10               YES                                         
*                                                                               
         MVC   AIO,AIO2            UPDATE PATTERN SEQNUM RECORD                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPTXKEY,R4                                                       
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM(3),BAGYMD     A-M/CLT                                     
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JUST ADDED RECORD, MUST BE THERE             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,INITXSP                                                       
*                                                                               
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
         MVC   HOLDSEQ,NPTS3QNO                                                 
         SR    R1,R1                                                            
         ICM   R1,7,NPTS3QNO       GET SEQ                                      
         LA    R1,1(,R1)           AND ADD 1                                    
         STCM  R1,7,NPTS3QNO       FOR ADDED REC                                
*                                                                               
         L     R6,AIO                                                           
         MVI   BYTE,C'C'           CHANGE F1 ELEM                               
         BRAS  RE,AACTELEM                                                      
*                                                                               
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XAAR10   MVC   KEY(36),SVXNKEY     RESTORE PATTERN KEY                          
*                                                                               
         CLI   NPTXNET,C'$'        TEST A NETWORK LIST                          
         BNE   XAAR20                                                           
         CLI   ACTNUM,ACTADD       TEST THIS IS A REAL ADDREC                   
         BNE   XAAR20              NO                                           
*                                                                               
         MVC   KEY+36(4),SVDSKAD                                                
         MVI   NPTXPSSV,C'$'       SET PASSIVE FLAG                             
                                                                                
* ADD A PASSIVE 0A61 POINTER FOR EACH NETWORK                                   
                                                                                
         L     R5,ASVNET                                                        
*                                                                               
XAAR12   CLI   0(R5),0                                                          
         BE    XAAR20                                                           
*                                                                               
         MVC   NPTXNET,0(R5)                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'XSPDIR',KEY,KEY                        
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,4(R5)                                                         
         B     XAAR12                                                           
*                                                                               
XAAR20   XC    NPTXKEY,NPTXKEY                                                  
         MVC   NPTPXID,=X'0AE1'                                                 
         MVC   NPTPXAM(3),BAGYMD         A-M/CLT                                
         MVC   NPTPXS3Q,HOLDSEQ                                                 
*                                                                               
         MVC   KEY+36(4),SVDSKAD         MOVE IN SAVED DISK ADDR                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'XSPDIR',KEY,KEY                        
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    XAAR30                                                           
         DC    H'0'                                                             
*                                                                               
XAAR30   CLI   MYPFKEY,2           TEST USER WANTS TO ADD COMMENTS              
         BNE   XAAR32                                                           
*                                                                               
         MVI   PFKEY,2             TELL DOPFK USER HIT PF2                      
         MVC   CONACT(3),=C'CHA'                                                
         OI    CONACTH+6,X'80'     XMT                                          
         MVI   TWALACT,ACTCHA      LIE!                                         
         MVI   ACTNUM,ACTCHA                                                    
         MVI   ACTEQU,ACTCHA                                                    
*                                                                               
XAAR32   XC    SVPRVDA,SVPRVDA                                                  
         CLI   SVTN3PR0,C'Y'       TEST TO COPY COMMENTS THIS CLT               
         BNE   *+10                                                             
         MVC   SVPRVDA,SVDSKAD     THEN DISK ADDRESS IS HERE!                   
*                                                                               
         BRAS  RE,INITXSP                                                       
         MVC   KEY(32),SVXNKEY     RESTORE KEY                                  
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TWAKEYSV,KEY        AND SAVE IT WHERE GENCON WANTS IT            
*                                                                               
         MVC   AIO,AIO             RESTORE AIO FOR GETREC                       
         GOTO1 GETREC                                                           
         XIT1                                                                   
         DROP  R4,R6                                                            
         EJECT                                                                  
*==============================================================                 
* VALIDATE PERIOD                                                               
*==============================================================                 
                                                                                
         USING NPTDTAEL,R6                                                      
VPER     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         JE    MISSERR             NO                                           
*                                                                               
VPER10   LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),DATE                                            
         L     RE,DMCB             GET LENGTH OF FIELD                          
         LTR   RE,RE                                                            
         BZ    DATERRD                                                          
         LA    R3,1(RE,R3)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,DATE),(3,STRTPAT)                                 
*                                                                               
         CLC   0(3,R3),=CL3'UFN'                                                
         BNE   VPER20                                                           
*                                                                               
         CLI   SVTN1PR1,C'Y'       TN1 PROFILE PROHIBITS UFN                    
         BNE   *+14                                                             
         MVC   GERROR,=Y(NOUFN)                                                 
         B     VPERER                                                           
*                                                                               
         MVC   ENDPAT,=XL3'FFFFFF'                                              
         B     VPER30                                                           
*                                                                               
VPER20   GOTO1 DATVAL,(R1),(R3),DATE                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRD                                                          
         GOTO1 DATCON,(R1),(0,DATE),(3,ENDPAT)                                  
         CLC   STRTPAT,ENDPAT                                                   
         BH    DATERRD                                                          
*                                                                               
VPER30   CLI   MODE,VALREC                                                      
         BNE   VPERX                                                            
*                                                                               
         CLC   NPTSTART(6),PATDTS  HAVE DATES CHANGED                           
         BE    VPER34               NO                                          
*                                                                               
         MVI   CHGESW,2            SET CHGESW ON                                
*                                                                               
VPER34   DS    0H                                                               
         MVC   NPTSTART(6),PATDTS  SAVE DATES FOR CK TO CMML                    
*                                                                               
         OC    PROGRAM,PROGRAM     WAS PROGRAM ENTERED                          
         BZ    VPERX                NO                                          
*                                                                               
         BRAS  RE,INITSPT                                                       
*                                                                               
* GET PROGRAM INFO                                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,PROGRAM                                                 
         GOTO1 DATCON,DMCB,(3,STRTPAT),(2,NPGKEND)                              
         CLC   ENDPAT,=XL3'FFFFFF'                                              
         BE    VPER40                                                           
         GOTO1 DATCON,DMCB,(3,ENDPAT),(2,NPGKEND)                               
*                                                                               
VPER40   DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+14                                                             
         MVC   GERROR,=Y(PGMUNCOV)                                              
         B     VPERER                                                           
         BRAS  RE,INITXSP          SET FOR XFILE                                
*                                                                               
VPERX    XIT1                                                                   
*                                                                               
VPERER   GOTO1 VTRAERR                                                          
*                                                                               
DATERRD  MVI   ERROR,INVDATE                                                    
         J     TRAPERR                                                          
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* DISPLAY NETWORKS FOR NETWORK LIST PATTERN                                     
*=================================================================              
                                                                                
DNETS    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    TRANET1,TRANET1                                                  
         OI    TRANET1H+6,X'80'                                                 
         XC    TRANET2,TRANET2                                                  
         OI    TRANET2H+6,X'80'                                                 
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DNETX                                                            
         B     DNET4                                                            
*                                                                               
DNET2    BRAS  RE,NEXTEL                                                        
         BNE   DNET10                                                           
*                                                                               
DNET4    TM    6(R6),X'80'         TEST NETWORK DELETED                         
         BO    DNET2                                                            
*&&DO                                                                           
         BZ    *+12                                                             
         MVI   0(R4),C'*'                                                       
         LA    R4,1(R4)                                                         
*&&                                                                             
         MVC   0(4,R4),2(R6)                                                    
*                                                                               
         LA    R4,3(R4)            POINT TO LAST CHAR                           
         CLI   0(R4),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         B     DNET2                                                            
*                                                                               
DNET10   LA    R1,ELEM                                                          
         AHI   R1,L'TRANET1-1      POINT TO LAST OUTPUT CHAR                    
         CLI   0(R1),C','          TEST SEPARATOR                               
         BE    *+8                                                              
         BCT   R1,*-8                                                           
         BCTR  R1,0                DON'T MOVE COMMA                             
*                                                                               
DNET12   LR    RE,R1                                                            
         LA    RF,ELEM                                                          
         SR    RE,RF               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TRANET1(0),ELEM                                                  
*                                                                               
         LA    R1,2(R1)            POINT TO NEXT CHAR TO MOVE                   
         CLI   0(R1),C' '          TEST THERE ARE ANY                           
         BNH   DNETX                                                            
         MVC   TRANET2,0(R1)       MOVE REMAINING CHARS                         
         LA    R1,TRANET2+L'TRANET2  NOW GET RID OF LAST COMMA                  
         CLI   0(R1),C','                                                       
         BE    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   0(R1),0                                                          
*                                                                               
DNETX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* DISPLAY CMMLS/PCTS/ROTATIONS                                                  
* SINCE CHKDEL READS CMMLS, SAVE KEY IN SVXNKEY AND RESTORE AT EXIT             
*=================================================================              
                                                                                
DCMMLS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         TM    NPTSTAT,NPTS_ADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         MVC   STRTPAT(6),NPTSTART   SAVE START/END DATES FOR VCML              
*                                                                               
         MVI   HASTIME,C'N'        SET PATTERN HAS TIME FLAG                    
         TM    NPTSTAT,NPTS_TIME   TEST TIME IN ELEMENT                         
         BZ    DCM1                                                             
         OC    NPTSTIM(4),NPTSTIM                                               
         BZ    *+8                                                              
         MVI   HASTIME,C'Y'                                                     
*                                                                               
DCM1     L     R6,AIO                                                           
         MVI   ELCODE,X'36'        LOOK FOR ABSURD ROT ELEM                     
         BRAS  RE,GETEL                                                         
         BE    DCM2                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'34'        LOOK FOR NORMAL ROT ELEM                     
         BRAS  RE,GETEL                                                         
         BE    DCM2                                                             
*                                                                               
         SR    R3,R3               INDICATE NO ROT ELEM                         
         B     DCM4                                                             
*                                                                               
DCM2     LA    R3,NPTPCTLT-NPTPCTEL(R6)  POINT TO FIRST PCT LETTER              
*                                                                               
DCM4     L     R6,AIO              COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    DCM5                                                             
         DC    H'0'                                                             
*                                                                               
DCM5     CLI   DELFLAG,C'N'        TEST DO NOT DISPLAY DELETES                  
         BE    *+8                                                              
         BRAS  RE,CHKDEL           DISPLAY ANY DELETED CMLS                     
*                                                                               
         USING NPTCMLEL,R6                                                      
         LA    R2,TRACMLAH         1ST SCREEN FIELD                             
         LLC   R5,NPTCMLLN                                                      
         SRL   R5,4                GIVES NUMBER OF CMMLS                        
         LA    R6,NPTCML           FIRST CMML                                   
         DROP  R6                                                               
*                                                                               
DCM6     XC    8(L'TRACMLA,R2),8(R2)      CLEAR FIELD                           
         OI    6(R2),X'80'                AND XMT                               
         LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         XC    8(L'TRAPCTA,RE),8(RE)    CLEAR PCT FIELD                         
         OI    6(RE),X'80'              AND XMT                                 
*                                                                               
DCM8     CLC   =X'5C00',0(R6)        TEST DELETED                               
         BNE   DCM10                                                            
         CLI   DELFLAG,C'N'          TEST DO NOT DISPLAY DELETES                
         BE    DCM24                                                            
         MVI   8(R2),C'*'                                                       
         MVI   5(R2),1                                                          
         AHI   R2,TRACMLBH-TRACMLAH  POINT TO NEXT CMML                         
         B     DCM24                                                            
*                                                                               
DCM10    MVC   8(8,R2),0(R6)       MOVE FIRST CMML                              
         OC    8(8,R6),8(R6)       TEST FOR SECOND CMML                         
         BZ    *+14                NO                                           
         MVI   16(R2),C'-'                                                      
         MVC   17(8,R2),8(R6)                                                   
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   DCM12                                                            
         XC    8(25,R2),8(R2)      CLEAR IT ALL AND START AGAIN!                
         GOTO1 VTRPACK,DMCB,(C'U',0(R6)),8(R2)                                  
         OC    8(8,R6),8(R6)                                                    
         BZ    DCM12                                                            
*                                                                               
         LA    R4,8(R2)                                                         
DCM10A   CLI   0(R4),C' '                                                       
         BNH   DCM10B                                                           
         LA    R4,1(R4)                                                         
         B     DCM10A                                                           
*                                                                               
DCM10B   MVI   0(R4),C'-'                                                       
         GOTO1 VTRPACK,DMCB,(C'U',8(R6)),1(R4)                                  
*                                                                               
DCM12    LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         BCTR  RE,0                POINT TO LAST CHAR IN FIELD                  
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R2                                                            
         AHI   RE,-7                                                            
         STC   RE,5(R2)                                                         
*                                                                               
DCM20    LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO PCT FIELD                           
         OI    6(R2),X'80'                                                      
*                                                                               
         LTR   R3,R3               TEST HAVE PCTS                               
         BZ    DCM22                                                            
         ICM   R0,3,1(R3)          GET PCT                                      
         EDIT  (R0),(3,8(R2)),0,ALIGN=LEFT                                      
*                                                                               
         MVI   5(R2),3                                                          
         CLI   10(R2),C' '                                                      
         BH    DCM22                                                            
         MVI   5(R2),2                                                          
         CLI   9(R2),C' '                                                       
         BH    DCM22                                                            
         MVI   5(R2),1                                                          
*                                                                               
DCM22    LLC   R0,0(R2)            POINT TO NEXT LETTER                         
         AR    R2,R0                                                            
         LLC   R0,0(R2)            POINT TO NEXT CMML FIELD                     
         AR    R2,R0                                                            
*                                                                               
         LTR   R3,R3               TEST HAVE PCT ELEMENT                        
         BZ    *+8                                                              
         LA    R3,3(R3)                                                         
*                                                                               
DCM24    LA    R6,16(R6)                                                        
         BCT   R5,DCM6                                                          
         LA    R0,TRAPCTLH                                                      
         CR    R2,R0               TEST PAST LAST CMML FIELD                    
         BH    DCM30                                                            
                                                                                
* CLEAR REMAINING CMMLS/PCTS                                                    
                                                                                
DCM26    XC    8(L'TRACMLA,R2),8(R2)      CLEAR FIELD                           
         OI    6(R2),X'80'                AND XMT                               
         LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         XC    8(L'TRAPCTA,RE),8(RE)    CLEAR PCT FIELD                         
         OI    6(RE),X'80'              AND XMT                                 
*                                                                               
DCM28    AHI   R2,TRACMLBH-TRACMLAH                                             
         LA    R0,TRAPCTLH         LAST PCT FIELD                               
         CR    R2,R0               TEST DONE ALL                                
         BL    DCM26                                                            
         EJECT                                                                  
*=======================================================                        
* DISPLAY LETTER ROTATION ELEMENT                                               
*=======================================================                        
                                                                                
DCM30    XC    TRAROT,TRAROT                                                    
         OI    TRAROTH+6,X'80'                                                  
         XC    TRADROT,TRADROT                                                  
         OI    TRADROTH+6,X'80'                                                 
*                                                                               
         LA    R2,TRADROTH         IF HAVE PCTS, SHOW DERIVED ROT               
         MVI   ELCODE,X'34'        CHECK FOR PCT ELEM                           
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R2,TRAROTH          IF NO PCT, SHOW ROT                          
*                                                                               
         MVI   ELCODE,X'32'        FIND ROTATION ELEMENT                        
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    DCM34                                                            
         DC    H'0'                                                             
*                                                                               
         USING NPTPTNEL,R6                                                      
DCM34    LLC   R1,NPTPTNLN                                                      
         AHI   R1,-3                                                            
         EX    R1,*+8              MOVE IN ROTATION                             
         B     *+10                                                             
         MVC   8(0,(R2)),NPTPTN-NPTPTNEL(R6)                                    
                                                                                
*==========================================================                     
* NOW CHECK STATUS OF ALL COMMMERCIALS                                          
*==========================================================                     
                                                                                
DCM40    L     R6,AIO              COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    DCM42                                                            
         DC    H'0'                                                             
*                                                                               
         USING NPTCMLEL,R6                                                      
DCM42    LA    R2,TRACMLAH         1ST SCREEN FIELD                             
         LLC   R4,NPTCMLLN                                                      
         SRL   R4,4                GIVES NUMBER OF CMMLS                        
         LA    R5,NPTCML           FIRST CMML                                   
*                                                                               
DCM44    CLC   =X'5C00',0(R5)        TEST DELETED                               
         BE    DCM48                                                            
*                                                                               
DCM46    MVC   WORK(8),0(R5)       MOVE CMML1                                   
         CLI   ADIDFLAG,C'Y'       TEST CMMLS ARE ADIDS                         
         BNE   DCM46A                                                           
         GOTO1 VTRPACK,DMCB,(C'U',(R5)),WORK                                    
*                                                                               
DCM46A   LA    R1,SVTYPE1                                                       
         BRAS  RE,VCML                                                          
*                                                                               
         OC    8(8,R5),8(R5)       CHECK FOR CMML2                              
         BZ    DCM48                                                            
*                                                                               
         MVC   WORK(8),8(R5)       MOVE CMML2                                   
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   DCM46B                                                           
         GOTO1 VTRPACK,DMCB,(C'U',8(R5)),WORK                                   
*                                                                               
DCM46B   LA    R1,SVTYPE2                                                       
         BRAS  RE,VCML                                                          
*                                                                               
DCM48    AHI   R2,TRACMLBH-TRACMLAH  POINT TO NEXT CMML ON SCREEN               
         LA    R5,16(R5)           NEXT CMML IN ELEMENT                         
         BCT   R4,DCM44                                                         
*                                                                               
DCMX     MVC   KEY,SVXNKEY                                                      
         BRAS  RE,INITXSP                                                       
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* CHECK FOR DELETED COMMERCIALS (ON SPTDIR)                                     
*============================================================                   
                                                                                
CHKDEL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,AIO                                                           
         CLI   NPTXNET-NPTXKEY(RE),0   TEST FOR PATTERN BY MEDIA                
         BE    *+10                                                             
         MVC   NETCML,NPTXNET-NPTXKEY(RE)                                       
*                                                                               
         USING NPTCMLEL,R6                                                      
CHKDEL2  LLC   R3,NPTCMLLN         GET ELEM LEN                                 
         SRL   R3,3                DIV BY 8=NO OF CMML PRS (DROPS ODD)          
         LA    R4,NPTCML           1ST CMML                                     
         DROP  R6                                                               
*                                                                               
         LA    R2,LDELETE                                                       
         CLI   MODE,LISTRECS                                                    
         BE    CHKDEL10                                                         
*                                                                               
         LA    R2,TRADCML                                                       
         XC    TRADCML,TRADCML                                                  
         LA    R2,4(R2)            START AT +4                                  
         OI    TRADCMLH+6,X'80'                                                 
*                                                                               
CHKDEL10 OC    0(8,R4),0(R4)       IS THIS A CML                                
         BZ    CHKDEL30                                                         
         CLC   =X'5C00',0(R4)      DELETED CML                                  
         BE    CHKDEL30                                                         
         MVC   WORK(8),0(R4)                                                    
*                                                                               
         BRAS  RE,FNDCML            GO FIND COMMERCIAL                          
         BE    CHKDEL12                                                         
         MVC   LDELETE(7),=C'!!BAD!!'                                           
         B     CHKDELX                                                          
*                                                                               
CHKDEL12 L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       CMML DELETED                                 
         BZ    CHKDEL20            NO                                           
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BNE   CHKDEL14                                                         
*                                                                               
         MVC   LDELETE(7),=C'DEL-CML'                                           
         B     CHKDELX                                                          
*                                                                               
CHKDEL14 LA    R0,TRADCML+63                                                    
         LA    RF,16(R2)                                                        
         CR    RF,R0               ROOM FOR ONE MORE CML?                       
         BNL   CHKDEL22                                                         
*                                                                               
         MVC   0(4,R2),=C'DEL*'                                                 
         MVC   4(8,R2),0(R4)                                                    
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   CHKDEL18                                                         
         GOTO1 VTRPACK,DMCB,(C'U',0(R4)),4(R2)                                  
*                                                                               
         LA    RF,16(R2)                                                        
CHKDEL16 CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,CHKDEL16                                                      
         LA    R2,2(RF)                                                         
         B     CHKDEL30                                                         
*                                                                               
CHKDEL18 LA    R2,13(R2)                                                        
         B     CHKDEL30                                                         
*                                                                               
CHKDEL20 CLI   HASTIME,C'Y'        TEST PATTERN HAS TIME                        
         BNE   CHKDEL30            NO - SKIP CMML TIME TEST                     
*                                                                               
         MVI   ELCODE,X'B0'        GET CMML MATCHING ELEMENT                    
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   CHKDEL30                                                         
*                                                                               
         USING CMLMATEL,R6                                                      
         OC    CMLMSTIM,CMLMSTIM   TEST CMML HAS TIME                           
         BZ    CHKDEL30                                                         
         LA    R0,TRADCML+63                                                    
         LA    RF,16(R2)                                                        
         CR    RF,R0               ROOM FOR ONE MORE CML?                       
         BL    CHKDEL24                                                         
*                                                                               
CHKDEL22 MVC   0(8,R2),=C'MORE....'                                             
         B     CHKDELX                                                          
*                                                                               
CHKDEL24 MVC   0(4,R2),=C'TIM*'                                                 
         MVC   4(8,R2),0(R4)                                                    
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   CHKDEL30                                                         
         GOTO1 VTRPACK,DMCB,(C'U',0(R4)),4(R2)                                  
*                                                                               
         LA    RF,16(R2)                                                        
CHKDEL26 CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,CHKDEL26                                                      
         LA    R2,2(RF)                                                         
*                                                                               
CHKDEL30 LA    R4,8(R4)                                                         
         BCT   R3,CHKDEL10                                                      
         DROP  R6                                                               
*                                                                               
CHKDELX  BRAS  RE,INITXSP          RESTORE XSPFILE PARAMS                       
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BE    CHKDELX2                                                         
         OC    TRADCML,TRADCML     TEST ANY ERRORS                              
         BZ    *+10                                                             
         MVC   TRADCML(3),=C'==>'                                               
*                                                                               
CHKDELX2 J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* VALIDATE CMMLS AND PCTS FOR NORMAL PATTERN                                    
*===============================================================                
                                                                                
VCMMLS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   HASTIME,C'N'        SET PATTERN HAS TIME FLAG                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
         OC    NPTSTIM(4),NPTSTIM                                               
         BZ    *+8                                                              
         MVI   HASTIME,C'Y'                                                     
                                                                                
* NEED TO SEE IF ALL CMMLS ARE VALID ADIDS                                      
                                                                                
         LA    R2,TRACMLAH                                                      
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
*                                                                               
VCMML2   CLI   5(R2),0                                                          
         BE    VCMML6                                                           
*                                                                               
VCMML4   BRAS  RE,MYSCAN           SCANNER CAN'T DO 12 CHAR FIELDS!             
*                                                                               
         CLC   =C'DELETE',WORK+44                                               
         BE    VCMML6                                                           
         CLI   WORK+44,C'*'                                                     
         BNE   *+16                                                             
         CLI   WORK+32,1                                                        
         BE    VCMML6                                                           
         J     INVALERR                                                         
                                                                                
         GOTO1 VTRPACK,DMCB,(C'P',WORK+44),WORK+12                              
         BNE   VCMML8                                                           
         CLI   WORK+33,0           TEST SECOND CMML INPUT                       
         BE    VCMML6                                                           
         GOTO1 VTRPACK,DMCB,(C'P',WORK+56),WORK+12                              
         BNE   VCMML8                                                           
*                                                                               
VCMML6   AHI   R2,TRACMLBH-TRACMLAH                                             
         LA    R0,TRACMLLH         LAST CMML FIELD                              
         CR    R2,R0                                                            
         BL    VCMML2                                                           
         MVI   ADIDFLAG,C'Y'                                                    
         B     VCMML10                                                          
*                                                                               
VCMML8   MVI   ADIDFLAG,C'N'       DO NOT USE ADIDS THIS REC                    
*                                                                               
VCMML10  MVC   AIO,AIO1                                                         
*                                                                               
         XC    PCTTBL,PCTTBL       CLEAR PCT BUILD AREA                         
         LA    RE,PCTTBL                                                        
         ST    RE,PCTNXT           SET A(NEXT ENTRY)                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
         MVC   STRTPAT,NPTSTART    PASS THESE TO VCMML                          
         MVC   ENDPAT,NPTEND                                                    
         DROP  R6                                                               
*                                                                               
         LA    R2,TRACMLAH         FIRST INPUT CMML                             
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING NPTCMLEL,R6                                                      
*                                                                               
VCMML12  MVI   CMMLCT,0            CLEAR COUNTERS                               
         MVI   DELCT,0                                                          
         MVI   PCTCT,0                                                          
         MVI   PCTTOT,0                                                         
         MVI   PRDMATSW,0                                                       
*                                                                               
         LA    R3,NPTCML                                                        
*                                                                               
VCMML14  LR    RE,R2                                                            
         BCTR  RE,0                BACK UP TO CMML LETTER                       
         IC    R0,0(RE)                                                         
         L     RE,PCTNXT                                                        
         STC   R0,0(RE)            SAVE LETTER                                  
*                                                                               
VCMML20  CLI   5(R2),0             TEST NO INPUT                                
         BNE   VCMML22                                                          
         MVI   0(RE),0             CLEAR LAST LETTER W/O PCT                    
*                                                                               
         LR    RE,R2               SHOULD BE NO MORE CMMLS                      
VCMML21  AHI   RE,TRACMLBH-TRACMLAH                                             
         LA    R0,TRACMLLH                                                      
         CR    RE,R0                                                            
         BH    VCMML66                                                          
         CLI   5(RE),0                                                          
         JNE   MISSERR             THERE SHOULD BE INPUT AT 0(R2)               
         B     VCMML21                                                          
*                                                                               
VCMML22  BRAS  RE,MYSCAN                                                        
*                                                                               
         XC    SVTYPE1(7),SVTYPE1  TYPE(4)/SLN(1)/SOLO(1)/FLAG(1)               
         XC    SVTYPE2(7),SVTYPE2                                               
*                                                                               
         CLI   WORK+33,0           IF NO CMML2                                  
         BNE   VCMML26                                                          
*                                                                               
         CLI   WORK+32,6           IF INPUT IS 6 CHARS                          
         BNE   *+14                                                             
         CLC   WORK+44(6),=C'DELETE' THEY CAN INPUT 'DELETE'                    
         BE    VCMML24                                                          
*                                                                               
         CLI   WORK+32,1           OR IF INPUT IS ONE CHAR                      
         BNE   VCMML26                                                          
         CLI   WORK+44,C'*'        IT SHOULD BE A *                             
         BNE   VCMML26                                                          
*                                                                               
VCMML24  XC    0(16,R3),0(R3)                                                   
         AHI   R2,TRACMLBH-TRACMLAH  POINT TO NEXT CMML                         
*                                                                               
         CLI   ACTNUM,ACTADD       DON'T INSERT DELETE IN RECORD                
         BE    VCMML64                                                          
*                                                                               
         MVI   0(R3),X'5C'                                                      
         LLC   R1,DELCT                                                         
         LA    R1,1(R1)            BUMP DELETED COUNT                           
         STC   R1,DELCT                                                         
         LA    R3,16(R3)                                                        
         B     VCMML64                                                          
*                                                                               
VCMML26  MVC   WORK(12),WORK+44    MOVE CMML1                                   
         CLI   WORK+32,8           MUST HAVE AT LEAST 8 CHARS                   
         JL    VCMLERR                                                          
         CLI   ADIDFLAG,C'Y'                                                    
         BE    *+12                                                             
         CLI   WORK+32,8                                                        
         JNE   VCMLERR                                                          
*                                                                               
         LA    R1,SVTYPE1                                                       
         BRAS  RE,VCML             VALIDATE CMML INPUT                          
         MVC   0(8,R3),WORK+12     MOVE COMPRESSED CODE TO ELEM                 
*                                                                               
         CLI   WORK+33,0           TEST CMML2 INPUT                             
         BE    VCMML30             NO                                           
         OC    QPRD2,QPRD2         TEST 2 PRODUCTS IN HL                        
         JZ    INVALERR                                                         
*                                                                               
VCMML28  CLI   WORK+33,8                                                        
         JL    VCMLERR                                                          
         CLI   ADIDFLAG,C'Y'                                                    
         BE    *+12                                                             
         CLI   WORK+33,8                                                        
         JL    VCMLERR                                                          
*                                                                               
         MVC   WORK(12),WORK+56     VALIDATE CMML2                              
         LA    R1,SVTYPE2                                                       
         BRAS  RE,VCML             VALIDATE CMML2 INPUT                         
         MVC   8(8,R3),WORK+12                                                  
*                                                                               
         CLC   SVTYPE1,SVTYPE2     ARE BOTH CMML TYPES THE SAME                 
         JNE   CMLTYPER            NO ERROR                                     
*                                                                               
         CLC   0(8,R3),8(R3)       BOTH CMLS SHOULD NOT BE EQUAL                
         JNZ   VCMML30                                                          
         MVC   GERROR,=Y(PBCMLSAM) PIGGY BACK CMMLS SAME                        
         GOTO1 VTRAERR                                                          
*                                                                               
VCMML30  BRAS  RE,CHKCMLS          CHECK PRDS/SLNS                              
         TM    CMLFLAG,X'A0'       TEST INVALID THIS PATTERN                    
         BZ    VCMML40                                                          
         LHI   R0,NETCMML                                                       
         STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
                                                                                
*================================================================               
* CHECK FOR DUPLICATE INPUT                                                     
*================================================================               
                                                                                
VCMML40  LA    R1,NPTCML           FIRST COMMERCIAL                             
*                                                                               
VCMML42  CR    R1,R3               TEST THIS IS US                              
         BE    VCMML50             YES - DONE                                   
         CLC   0(16,R1),0(R3)      TEST EQUAL                                   
         JE    EQCMLER                                                          
         LA    R1,16(R1)           POINT TO NEXT CMML(S)                        
         B     VCMML42                                                          
*                                                                               
VCMML50  LLC   R1,CMMLCT                                                        
         LA    R1,1(R1)                                                         
         STC   R1,CMMLCT                                                        
*                                                                               
VCMML52  LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO PCT FIELD                           
         CLI   5(R2),0             TEST INPUT                                   
         BE    VCMML54             NO                                           
*                                                                               
         GOTO1 VALINUM                                                          
         LLC   R0,PCTTOT                                                        
         LLC   RE,ACTUAL                                                        
         AR    R0,RE                                                            
         STC   R0,PCTTOT                                                        
*                                                                               
         L     RE,PCTNXT                                                        
         MVC   2(1,RE),ACTUAL      SAVE PCT                                     
         LA    RE,3(RE)                                                         
         ST    RE,PCTNXT                                                        
*                                                                               
         LLC   RE,PCTCT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,PCTCT                                                         
         B     VCMML60                                                          
*                                                                               
VCMML54  CLI   PCTTOT,0            TEST PCTS PREVIOUSLY INPUT                   
         JNE   MISSERR                                                          
*                                                                               
VCMML60  LA    R3,16(R3)                                                        
*                                                                               
VCMML62  LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT LETTER                         
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT CMML                           
*                                                                               
VCMML64  LA    R0,TRAPCTLH         LAST PCT FIELD                               
         CR    R2,R0                                                            
         BL    VCMML14                                                          
*                                                                               
VCMML66  CLI   CMMLCT,0           ANY ENTRIES FOUND                             
         JE    CMLMISER            NO                                           
*                                                                               
         CLI   PCTCT,0             IF NO PCTS INPUT, OK                         
         BE    VCMML70                                                          
         CLC   PCTCT,CMMLCT        ELSE MUST HAVE 1 FOR EACH                    
         BE    VCMML70                                                          
*                                                                               
         LA    R2,TRAPCTAH         FIND FIRST ONE WITH NO INPUT                 
VCML67   CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         AHI   R2,TRAPCTBH-TRAPCTAH                                             
         B     VCML67                                                           
*                                                                               
VCMML70  BRAS  RE,INITXSP                                                       
         MVI   ELEM,X'30'          SET ELEMENT CODE                             
         LA    R0,ELEM                                                          
         SR    R3,R0               GIVE ELEMENT LENGTH                          
         STC   R3,ELEM+1                                                        
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         CLI   ADIDFLAG,C'Y'       ARE NEW CMMLS ADIDS                          
         BNE   VCMML71             NO - WE'RE OK THEN                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    NPTSTAT-NPTDATA(R6),NPTS_ADID   TEST OLD CMMLS ARE ADIDS         
         BO    VCMML71                         THEN THEY'RE THE SAME            
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VCMML72             IF NO OLD ELEM, JUST ADD NEW ELEMENT         
* CONVERT TO ADIDS                                                              
         LLC   R0,1(R6)                                                         
         SRDL  R0,4                DIVIDE BY 16                                 
         LA    R6,2(R6)                                                         
*                                                                               
VCMML70A MVC   WORK(12),SPACES                                                  
         MVC   WORK(8),0(R6)                                                    
         GOTO1 VTRPACK,DMCB,(C'P',WORK),0(R6)                                   
*                                                                               
         OC    8(8,R6),8(R6)       TEST P/B                                     
         BZ    VCMML70B                                                         
*                                                                               
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(8),8(R6)                                                    
         GOTO1 (RF),(R1),,8(R6)                                                 
*                                                                               
VCMML70B LA    R6,16(R6)                                                        
         BCT   R0,VCMML70A                                                      
                                                                                
        L     R6,AIO              SET FLAG THAT CMMLS ARE ADIDS                 
        MVI   ELCODE,X'10'                                                      
        BRAS  RE,GETEL                                                          
        BE    *+6                                                               
        DC    H'0'                                                              
        USING NPTDTAEL,R6                                                       
                                                                                
        OI    NPTSTAT,NPTS_ADID   SET CMMLS ARE ADIDS                           
*                                                                               
VCMML71  L     R6,AIO                                                           
         MVI   ELCODE,X'30'        GET OLD CMML ELEM                            
         BRAS  RE,GETEL                                                         
         BNE   VCMML72                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R6)            GET OLD ELEM LENGTH                          
         BCTR  RE,0                                                             
         EX    RE,VCMMLCLC         COMPARE X'30' ELEMS                          
         BE    VCMML74                                                          
         MVI   CHGESW,3            SET TO ADD NEW REF/SUBL ON CHG               
         GOTO1 VRECUP,DMCB,(C'T',AIO),(R6)   DELETE OLD ELEMENT                 
*                                                                               
VCMML72  GOTO1 ADDELEM                                                          
*                                                                               
         L     R6,AIO              SET FLAG THAT CMMLS ARE ADIDS                
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
*                                                                               
         OI    NPTSTAT,NPTS_ADID   SET CMMLS ARE ADIDS                          
         CLI   ADIDFLAG,C'Y'                                                    
         BE    *+8                                                              
         NI    NPTSTAT,X'FF'-NPTS_ADID  UNSET FLAG                              
         B     VCMML74                                                          
*                                                                               
VCMMLCLC CLC   0(0,R6),ELEM                                                     
                                                                                
*===========================================================                    
* GET RID OF 32/34/36 ELEMENTS                                                  
* SAVE 34 OR 36 IF THEY EXIST, OTHERWISE 32                                     
* THEN COMPARE ELEMENT CODES AND DATA TO SEE IF CHANGED                         
*===========================================================                    
                                                                                
VCMML74  XC    SVELEM,SVELEM                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'32'        LOOK FOR 32 FIRST                            
         BRAS  RE,GETEL                                                         
         BNE   VCMML74A                                                         
         MVC   SVELEM,0(R6)                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VCMML74A L     R6,AIO                                                           
         MVI   ELCODE,X'36'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VCMML74B                                                         
         MVC   SVELEM,0(R6)                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VCMML74B L     R6,AIO                                                           
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VCMML74X                                                         
         MVC   SVELEM,0(R6)                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VCMML74X CLI   PCTTOT,0            TEST ANY PCTS INPUT                          
         BE    VCMML80             NO                                           
         CLI   PCTTOT,99                                                        
         BE    *+12                                                             
         CLI   PCTTOT,100                                                       
         JNE   ROTPCERR            ERROR MUST BE 99/100 PCT                     
         CLI   CMMLCT,1            TEST EXACTLY 1 CMML                          
         BNE   VCMML80             NO                                           
         MVI   PCTTOT,100                                                       
         MVC   ELEM(5),=X'3405C10064'   SET A=100                               
         XC    TRAROT,TRAROT            AND IGNORE ROTATION INPUT               
         MVI   TRAROTH+5,0                                                      
         OI    TRAROTH+6,X'80'                                                  
*                                                                               
VCMML76X SR    RE,RE                                                            
         IC    RE,ELEM+1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   ELEM(0),SVELEM      TEST SAME ELEM/DATA                          
         BE    *+8                                                              
         MVI   CHGESW,4            SET TO ADD NEW REF/SUBL                      
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VCMML80  LA    R2,TRAROTH          ROTATION PATTERN ENTRIES                     
         BRAS  RE,VROT                                                          
*                                                                               
VCMMLX   J     EXIT                                                             
         EJECT                                                                  
*                                                                               
CMLMISER LA    R2,TRACMLAH         FIRST COMMERCIAL FIELD                       
         MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
CMLTIMER LHI   R0,CMLHASTM         CMML HAS TIME                                
         CLI   MODE,PRINTREP                                                    
         JNE   ERREXIT1                                                         
         OI    CMLFLAG1,CMLINVTM   CMML HAS TIME AND SO DOES PTTN               
         J     VCMLX                                                            
*                                                                               
CMLDELER LHI   R0,CMLISDEL       COMMERCIAL IS DELETED                          
         CLI   MODE,PRINTREP                                                    
         JNE   ERREXIT1                                                         
         OI    CMLFLAG1,CMLINVAL   SET INVALID CMML IN PATTER                   
         J     VCMLX                                                            
*                                                                               
MATPRDER MVI   ERROR,CMLPRDER                                                   
         CLI   MODE,PRINTREP                                                    
         JNE   TRAPERR                                                          
         OI    CMLFLAG,X'40'       SET PRD ERR                                  
         J     VCMLX                                                            
*                                                                               
ROTPCERR LHI   R0,ROTPCT          PCTS MUST ADD UP TO 99 OR 100                 
*                                                                               
ERREXIT1 STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
FNDCML   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITSPT                                                       
*                                                                               
         MVC   AIO,AIO2            USE I/O 2                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
*                                                                               
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,WORK                                                     
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* CHECK THAT COMMERCIALS AND PATTERN PRDS/SLNS AGREE                            
*==========================================================                     
                                                                                
CHKCMLS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITSPT                                                       
         BRAS  RE,NCML             CHECK EXCLUDED NETWORKS                      
         BRAS  RE,INITNET                                                       
*                                                                               
         OC    QPRD2,QPRD2         TEST P/B PATTERN                             
         BNZ   CHKCML20            YES                                          
                                                                                
*================================================                               
* PATTERN IS FOR ONE PRODUCT ONLY                                               
*================================================                               
                                                                                
         CLI   SVSOLO1,C'P'        TEST FOR P/B ONLY                            
         JE    PGSOERR             YES - THEN ERROR                             
         CLI   SVSOLO2,C'P'                                                     
         JE    PGSOERR                                                          
*                                                                               
         CLI   SVSLN2,0            TEST 2 CMMLS INPUT                           
         BNE   CHKCML10            YES                                          
                                                                                
* ONE CMML INPUT - NON P/B                                                      
                                                                                
         TM    SVFLAG1,X'01'       PRD IN CMML                                  
         JZ    MATPRDER            NO - ERROR                                   
         CLC   BSLN,SVSLN1         RIGHT SLN                                    
         JNE   INVSLNER            NO                                           
         J     EXIT                                                             
                                                                                
* TWO CMMLS INPUT - NON P/B                                                     
                                                                                
CHKCML10 CLI   SVSOLO1,C'S'        TEST SOLO ONLY                               
         JE    PGSOERR             YES - THEN ERROR                             
         CLI   SVSOLO2,C'S'        TEST SOLO ONLY                               
         JE    PGSOERR                                                          
*                                                                               
         CLI   SVFLAG1,1           TEST PRD IN BOTH CMMLS                       
         JNE   MATPRDER                                                         
         CLI   SVFLAG2,1                                                        
         JNE   MATPRDER                                                         
*                                                                               
         LLC   R0,SVSLN1                                                        
         LLC   R1,SVSLN2                                                        
         AR    R0,R1               GET TOTAL SLN                                
         CLM   R0,1,BSLN           RIGHT SLN                                    
         JNE   INVSLNER            NO                                           
         J     EXIT                                                             
                                                                                
*============================================================                   
* PATTERN IS FOR PIGGYBACK                                                      
*============================================================                   
                                                                                
CHKCML20 CLI   SVSOLO1,C'S'        TEST FOR SOLO ONLY                           
         JE    PGSOERR             YES - THEN ERROR                             
         CLI   SVSOLO2,C'S'                                                     
         JE    PGSOERR                                                          
*                                                                               
         CLI   SVSLN2,0            TEST TWO CMMLS                               
         BNE   CHKCML30                                                         
                                                                                
* ONE CMML INPUT - P/B                                                          
                                                                                
         TM    SVFLAG1,X'02'+X'01' TEST BOTH PRDS IN CMML                       
         JNO   MATPRDER            NO - ERROR                                   
         LLC   R0,SVSLN1                                                        
         SRL   R0,1                                                             
         CLM   R0,1,BSLN                                                        
         JNE   INVSLNER                                                         
         CLM   R0,1,BSLN2                                                       
         JNE   INVSLNER                                                         
         J     EXIT                                                             
                                                                                
* TWO CMMLS INPUT- P/B                                                          
                                                                                
CHKCML30 TM    SVFLAG1,X'01'       TEST PRD1 IN CMML1                           
         BO    CHKCML32            YES                                          
         TM    SVFLAG2,X'01'       TEST PRD1 IN CMML2                           
         JZ    MATPRDER                                                         
*                                                                               
CHKCML32 TM    SVFLAG1,X'02'       TEST PRD2 IN CMML1                           
         BO    CHKCML40                                                         
         TM    SVFLAG2,X'02'       TEST PRD2 IN CMML2                           
         JZ    MATPRDER                                                         
                                                                                
* FULL+0=PRD1/CMML1, FULL+1=PRD2/CMML2                                          
* FULL+2=PRD1/CMML2, FULL+3=PRD2/CMML1                                          
                                                                                
CHKCML40 XC    FULL,FULL                                                        
         TM    SVFLAG1,X'01'       TEST PRD1 IN CMML 1                          
         BZ    CHKCML42                                                         
         MVI   FULL,X'10'          SET FLAG                                     
         CLC   SVSLN1,BSLN         MATCH PRD1 SLN                               
         BNE   *+8                                                              
         MVI   FULL,X'11'          SET PRD1/SLN1 MATCH CMML1                    
*                                                                               
CHKCML42 TM    SVFLAG1,X'02'       TEST PRD2 IN CMML1                           
         BZ    CHKCML44                                                         
         MVI   FULL+3,X'10'                                                     
         CLC   SVSLN2,BSLN2        MATCH PRD2 SLN                               
         BNE   *+8                                                              
         MVI   FULL+3,X'11'                                                     
*                                                                               
CHKCML44 TM    SVFLAG2,X'01'       TEST PRD1 IN CMML2                           
         BZ    CHKCML46                                                         
         MVI   FULL+2,X'10'                                                     
         CLC   SVSLN1,BSLN         MATCH PRD1 SLN                               
         BNE   *+8                                                              
         MVI   FULL+2,X'11'                                                     
*                                                                               
CHKCML46 TM    SVFLAG2,X'02'       TEST PRD2 IN CMML2                           
         BZ    CHKCML50                                                         
         MVI   FULL+1,X'10'                                                     
         CLC   SVSLN2,BSLN2        MATCH PRD2 SLN                               
         BNE   *+8                                                              
         MVI   FULL+1,X'11'                                                     
*                                                                               
CHKCML50 CLI   FULL,X'11'          TEST PRD1/SLN1 MATCH CMML1                   
         BNE   CHKCML52            NO                                           
         CLI   FULL+1,X'11'        TEST PRD2/SLN2 MATCH CMML2                   
         BE    CHKCMLX             YES - ALL IS GOOD                            
*                                                                               
CHKCML52 CLI   FULL+2,X'11'        TEST PRD1/SLN1 MATCH CMML2                   
         JNE   INVSLNER            NO - ERROR                                   
         CLI   FULL+3,X'11'        TEST PRD2/SLN2 MATCH CMML1                   
         JNE   INVSLNER            NO  - ERROR                                  
*                                                                               
CHKCMLX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* SPLIT THE COMMERCIAL FIELD AT 8(R2)                                           
* CMML1 RETURNED AT WORK+44(12), CMML2 AT WORK+56(12)                           
*==============================================================                 
                                                                                
MYSCAN   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK+32(12),WORK+32                                              
         MVC   WORK+44(24),SPACES                                               
         MVC   WORK+32(1),5(R2)    SET FIELD 1 LENGTH                           
*                                                                               
         CLI   5(R2),12                                                         
         BH    MYSCAN2                                                          
*                                                                               
         LLC   RE,5(R2)            GET INPUT FIELD LENGTH                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     EQXIT                                                            
MYMVCLHS MVC   WORK+44(0),8(R2) *EXECUTED*                                      
*                                                                               
MYSCAN2  LA    R1,8(R2)                                                         
         LLC   R0,5(R2)                                                         
*                                                                               
MYSCAN4  CLI   0(R1),C'-'                                                       
         BE    MYSCAN10                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,MYSCAN4                                                       
         J     INVALERR            TOO MANY CHARS                               
*                                                                               
MYSCAN10 LA    R4,1(R1)            SAVE POINTER TO RHS DATA                     
         LA    RE,8(R2)                                                         
         SR    R1,RE               GIVES NUM CHARS UP TO -                      
         STC   R1,WORK+32          SET AS LENGTH OF LHS                         
         BCTR  R1,0                                                             
         EX    R1,MYMVCLHS         MOVE LHS DATA                                
*                                                                               
         AHI   R0,-1               ADJUST REMAINING COUNT                       
         JNP   INVALERR            SHOULD BE POSITIVE                           
         STC   R0,WORK+33          SET AS LEN OF RHS                            
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,MYMVCRHS         MOVE RHS DATA                                
         J     EQXIT                                                            
MYMVCRHS MVC   WORK+56(0),0(R4) *EXECUTED*                                      
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* VALIDATE COMMERCIAL                                                           
* ON ENTRY WORK(12) CONTAINS INPUT CMML                                         
*          R1 POINTS TO SVTYPE(4),SVSLN                                         
* ON EXIT  WORK+12(8) CONTAINS 8 BYTE VERSION                                   
*================================================================               
                                                                                
VCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R5,R1               SAVE POINTER                                 
         BRAS  RE,INITSPT          CMMLS LIVE ON SPTDIR                         
*                                                                               
         MVC   AIO,AIO2            USE IO2 FOR CMMLS                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
*                                                                               
         CLI   ADIDFLAG,C'Y'       TEST CAN USE ADIDS                           
         BE    VCML2               YES                                          
         MVC   KEY+5(8),WORK       USE EBCDIC CMML                              
         MVC   WORK+12(8),WORK     SET AS TRUNPK OUTPUT                         
         B     VCML4                                                            
*                                                                               
VCML2    MVC   KEY(2),=X'0AC1'                                                  
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK+12                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+5(8),WORK+12                                                 
*                                                                               
VCML4    MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   VCMLERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       CMML DELETED                                 
         JNZ   CMLDELER                                                         
*                                                                               
         CLC   CMLRLSE,STRTPAT     SEE IF THIS CMML WILL START IN TIME          
         JH    CMLDTERA            NO, CMLRLSE AFTER PAT START                  
*                                                                               
         CLC   CMLRCL,ENDPAT       SEE IF THIS CMML WILL LAST THRU PAT          
         BNL   VCML10              YES, OK                                      
*                                                                               
         CLC   ENDPAT,=XL3'FFFFFF' IS PAT UFN                                   
         JNE   CMLDTERB                                                         
*                                                                               
VCML10   MVC   SVCMLRCL,CMLRCL     SAVE COMMERCIAL RECALL DATE                  
*                                                                               
         MVC   0(4,R5),CMLTYPE     SAVE COMMERCIAL TYPE                         
         MVC   4(1,R5),CMLSLN      SAVE SPOT LEN                                
         MVC   5(1,R5),CMLSOLO                                                  
*                                                                               
         LA    R4,QPRD                                                          
         BAS   RE,CHKPRD                                                        
         BNE   *+8                                                              
         OI    6(R5),X'01'       SET PRD1 IN CMML                               
*                                                                               
         LA    R4,QPRD2                                                         
         CLI   0(R4),0                                                          
         BE    VCML12                                                           
         BAS   RE,CHKPRD                                                        
         BNE   *+8                                                              
         OI    6(R5),X'02'       SET PRD2 IN CMML                               
*                                                                               
VCML12   TM    6(R5),X'03'       TEST EITHER PRD IN THIS CMML                   
         JZ    MATPRDER                                                         
*                                                                               
         CLI   HASTIME,C'Y'        TEST PATTERN HAS TIME                        
         BNE   VCML14                                                           
*                                                                               
         MVI   ELCODE,X'B0'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   VCML14                                                           
         USING CMLMATEL,R6                                                      
         OC    CMLMSTIM,CMLMSTIM   TEST TIME IN CMML                            
         BZ    VCML14                                                           
         J     CMLTIMER                                                         
         EJECT                                                                  
*============================================================                   
* FURTHER VALIDATE CML (APPROVALS AND DATES IF ANY)                             
*============================================================                   
                                                                                
VCML14   MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEMENT                   
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    VCML20                                                           
         CLI   SVT1PROF+12,C'Y'    IS THIS BRAND AGENCY                         
         JNE   VCMLX                                                            
         J     NOAIRERR            YES, NOT APPROVED TO AIR                     
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
VCML20   DS    0H                                                               
         CLI   CMLBBBAG,C'N'       IS BRAND AGY=N                               
         JE    VCMLX                                                            
         CLI   CMLBBBAG,C'Y'                                                    
         BE    *+12                                                             
         CLI   SVT1PROF+12,C'Y'    IS THIS BRAND AGENCY                         
         JNE   VCMLX                                                            
*                                                                               
         OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                             
         BZ    VCML30                                                           
         CLC   SVCMLRCL,CMLBBMXD   COMPARE RECALL DATE TO MAX USE DTE           
         BNH   VCML30                                                           
         MVC   SVCMLRCL,CMLBBMXD   SAVE EARLIER DATE                            
*                                                                               
         CLC   SVCMLRCL,STRTPAT    IS CML RECALL BEFORE PAT START               
         JL    CMLDTERB             YES, ERROR                                  
         CLC   SVCMLRCL,ENDPAT     SEE IF THIS CMML WILL LAST THRU PAT          
         BNL   VCML30               YES, OK                                     
         CLC   ENDPAT,=XL3'FFFFFF' IS PAT UFN                                   
         JNE   CMLDTERB                                                         
*                                                                               
VCML30   DS    0H                                                               
         CLI   CMLBBBAG,0          LEO CML?                                     
         BNE   *+12                                                             
         CLI   SVT1PROF+12,C'Y'    IS THIS BRAND AGENCY                         
         BE    *+12                                                             
         CLI   CMLBBBAG,C'Y'       LEO B. CML                                   
         JNE   VCMLX                NO, DONE                                    
*                                                                               
         OC    CMLBBCAD,CMLBBCAD   ANY CLIENT APPROVAL DATE?                    
         JZ    CADTERR              NO, ERROR                                   
*                                                                               
         CLI   CMLBBAPR,C'Y'       BROADCAST BUSINESS APPROVAL?                 
         BE    VCML32              YES                                          
*                                                                               
         CLI   CMLATAIR,C'Y'       APPROVED TO AIR                              
         JNE   NOAIRERR            NOT APPROVED TO AIR                          
         J     VCMLX                YES, DONE                                   
*                                                                               
VCML32   OC    CMLBBREF,CMLBBREF   ANY CML REFERENCE?                           
         JZ    NOAIRERR             NO, ERROR                                   
*                                                                               
* CHECK IF CML FROM REFERENCE FIELD IS APPROVED TO AIR                          
*                                                                               
         MVC   WORK(8),CMLBBREF                                                 
         BRAS  RE,FNDCML            GO FIND COMMERCIAL                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(8),SVCML       RESTORE ORIGINAL CML                         
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         JNE   NOAIRERR                                                         
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
         CLI   CMLATAIR,C'Y'                                                    
         JNE   NOAIRERR                                                         
*                                                                               
VCMLX    BRAS  RE,INITXSP          RESTORE XSPFILE PARAMS                       
         MVC   AIO,AIO1            RESTORE AIO                                  
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
         USING CMLMPREL,R6                                                      
CHKPRD   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'29'        GET PRDLIST ELEMENT                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CMLMPRS,X'FF'       IS THIS CMML PRD=ALL                         
         JE    EQXIT               YES, COVERS ALL PRODUCTS                     
*                                                                               
         LLC   RF,CMLMPRLN                                                      
         AHI   RF,-2                                                            
         SR    RE,RE                                                            
         D     RE,=F'3'                                                         
         LA    RE,CMLMPRS          START OF PROD LIST                           
*                                                                               
CHKPRD2  CLC   0(3,RE),0(R4)       MATCH PRD                                    
         JE    EQXIT                YES                                         
         LA    RE,3(RE)                                                         
         BCT   RF,CHKPRD2                                                       
         J     NEQXIT                                                           
         EJECT                                                                  
*===================================================                            
* SEE IF CML IS NETWORK SPECIFIC                                                
*===================================================                            
                                                                                
NCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    CMLFLAG,X'FF'-X'20' INIT EXCLUDED NETWORK                        
         NI    CMLFLAG,X'FF'-X'80' INIT INCLUDE NETWORK                         
         MVI   BYTE,0              INIT EXCLUDED NETS                           
*                                                                               
         MVI   ELCODE,X'22'                                                     
         L     R6,AIO2                                                          
         BRAS  RE,GETEL                                                         
         BNE   NCMLX                                                            
*                                                                               
         USING CMLNETEL,R6                                                      
NCML10   DS    0H                                                               
         CLI   CMLNETLN,6          OLD REC?                                     
         BE    NCML30               YES                                         
*                                                                               
         TM    CMLFLG,CMLEXNET     EXCLUDE THIS NETWORK?                        
         BZ    NCML30                                                           
         MVI   BYTE,1              EXCLUDED NETS                                
         OC    NETWORK,NETWORK     IF ALL NETWORK PATTERN                       
         BZ    *+14                THEN CAN NOT HAVE EXCLUDED NETWORKS          
         CLC   NETWORK,CMLNET                                                   
         BNE   NCML40                                                           
         OI    CMLFLAG,X'20'                                                    
         B     NCMLX                                                            
*                                                                               
NCML30   DS    0H                                                               
         CLC   NETWORK,CMLNET       COMPARE NETWORK SPECIFIC CML                
         BE    NCMLX                                                            
*                                                                               
NCML40   BRAS  RE,NEXTEL                                                        
         BE    NCML10                                                           
*                                                                               
         CLI   BYTE,1              EXCLUDED NETS?                               
         BE    NCMLX                                                            
*                                                                               
         OI    CMLFLAG,X'80'                                                    
NCMLX    XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* VALIDATE COMMERCIAL ROTATION                                                  
*===============================================================                
                                                                                
VROT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* BUILD A TABLE IN VALTBL OF VALID COMMERCIAL LETTERS                           
* X'00'= INVALID X'01'=OK C'D'=DELETED                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        GET CMML ELEM                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R0,1(R6)                                                         
         SRL   R0,4                GIVES NUMBER OF CMMLS PRESENT                
         LA    R6,2(R6)                                                         
*                                                                               
         XC    VALTBL,VALTBL                                                    
         LA    R1,VALTBL                                                        
*                                                                               
VROT2    MVI   0(R1),1             ASSUME VALID                                 
         CLC   =X'5C00',0(R6)      TEST DELETED CMML                            
         BNE   *+8                                                              
         MVI   0(R1),C'D'                                                       
         LA    R6,16(R6)           NEXT CMML                                    
         LA    R1,1(R1)                                                         
         BCT   R0,VROT2                                                         
                                                                                
VROT10   MVC   GERROR,=Y(ROTORPCT) EITHER ROTATION OR PCTS                      
         LA    R2,TRAROTH                                                       
         CLI   5(R2),0             TEST ANY ROTATION INPUT                      
         BE    VROT11              NO                                           
         CLI   PCTTOT,0            HAVE ROT - TEST ANY PCTS INPUT               
         BE    VROT12              NO - GO VALIDATE ROTATION                    
         GOTO1 VTRAERR             ELSE SHOULD NOT INPUT BOTH                   
*                                                                               
VROT11   CLI   PCTTOT,0            NO ROT INPUT, SHOULD HAVE PCTS               
         BNE   VROT30              GO BUILD ROTATION ELEMENT                    
         GOTO1 VTRAERR                                                          
*                                                                               
VROT12   LLC   R0,5(R2)                                                         
         LA    R1,8(R2)                                                         
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM+2                                                        
*                                                                               
VROT14   LLC   RE,0(R1)            GET INPUT CHAR                               
         AHI   RE,-192             A=192 =1                                     
         CHI   RE,9                                                             
         BNH   *+8                                                              
         AHI   RE,-7               J=209 = 10                                   
         LA    RF,VALTBL-1(RE)                                                  
*                                                                               
         CLI   0(RF),0             IF 0, INVALID CHAR                           
         BE    VROTERR                                                          
         CLI   0(RF),C'D'          TEST DELETED                                 
         BE    VROT16                                                           
*                                                                               
         LLC   RE,0(RF)            GET VALID CMML FLAG                          
         LA    RE,1(RE)            BUMP USAGE COUNT                             
         STC   RE,0(RF)                                                         
         MVC   0(1,R4),0(R1)       MOVE LETTER TO ELEM                          
         LA    R4,1(R4)            BUMP OUTPUT POSN                             
*                                                                               
VROT16   LA    R1,1(R1)                                                         
         BCT   R0,VROT14                                                        
*                                                                               
* MAKE SURE ALL CMMLS HAVE BEEN INPUT                                           
*                                                                               
         LA    R1,VALTBL                                                        
         LA    R0,15                                                            
VROT20   CLI   0(R1),1             TABLE SHOULD NOT BE 1 ANYMORE                
         JE    ROTERR                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VROT20                                                        
*                                                                               
         LA    R6,ELEM             INSERT ROTATION ELEMENT IN RECORD            
         USING NPTPTNEL,R6                                                      
*                                                                               
         MVI   NPTPTNEL,X'32'                                                   
         LA    R0,ELEM                                                          
         SR    R4,R0                                                            
         STC   R4,NPTPTNLN                                                      
*                                                                               
         XC    TRAROT,TRAROT     MOVE DATA TO SCREEN                            
         MVC   TRAROT,ELEM+2                                                    
         OI    TRAROTH+6,X'80'                                                  
*                                                                               
         XC    TRADROT,TRADROT     DO NOT SHOW DERIVED ROTATION                 
         OI    TRADROT+6,X'80'                                                  
         B     VROT80                                                           
         EJECT                                                                  
*================================================================               
* BUILD ROTATION ELEMENT FROM INPUT PERCENTAGES                                 
*================================================================               
                                                                                
* FIND MIN/MAX PCTS                                                             
VROT30   LA    R0,255                                                           
         ST    R0,MINPCT                                                        
         XC    MAXPCT,MAXPCT                                                    
*                                                                               
         LA    R1,PCTTBL           POINT TO FIRST PCT                           
         LA    R0,15               MAX NUMBER OF PCT                            
*                                                                               
VROT32   OC    0(3,R1),0(R1)                                                    
         BZ    VROT34                                                           
         SR    RE,RE                                                            
         ICM   RE,3,1(R1)                                                       
         C     RE,MINPCT                                                        
         BNL   *+8                                                              
         ST    RE,MINPCT                                                        
         C     RE,MAXPCT                                                        
         BNH   *+8                                                              
         ST    RE,MAXPCT                                                        
         LA    R1,3(R1)                                                         
         BCT   R0,VROT32                                                        
*                                                                               
VROT34   BAS   RE,DIV              GET DIVISOR                                  
         BE    VROT40                                                           
*                                                                               
         CLI   SVTN2PRO+9,C'Y'                                                  
         BNE   CALROTER                                                         
*                                                                               
         BAS   RE,ABSEL                                                         
*                                                                               
VROT40   MVI   ELCODE,X'34'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'34'                                                       
*                                                                               
         L     RE,PCTNXT           GET A(NEXT ENTRY)                            
         LA    R0,PCTTBL                                                        
         SR    RE,R0               GIVES LENGTH OF ENTRIES                      
         AHI   RE,2                                                             
         STC   RE,ELEM+1           SET ELEM LENGTH                              
         MVC   ELEM+2(45),PCTTBL   MOVE TABLE TO ELEM                           
*                                                                               
         CLC   ELEM(1),SVELEM      SAME ELEMENT ID                              
         BNE   VROT46                                                           
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   ELEM(0),SVELEM      TEST SAME ELEM/DATA                          
         BE    *+8                                                              
         MVI   CHGESW,5            SET TO ADD NEW REF/SUBL                      
*                                                                               
VROT46   GOTO1 ADDELEM                                                          
*                                                                               
* NOW BUILD TABLE OF LETTERS AND NUMBERS TO BUILD ROTATION *                    
*                                                                               
         XC    WORK,WORK                                                        
         LLC   R0,PCTCT                                                         
         LA    R3,PCTTBL                                                        
         LA    R4,WORK+2                                                        
         L     R6,COMDIV                                                        
         SR    R1,R1                                                            
         SR    RF,RF                                                            
*                                                                               
VROT60   MVC   0(1,R4),0(R3)       SAVE LETTER                                  
         ICM   RF,3,1(R3)                                                       
         SR    RE,RE                                                            
         DR    RE,R6                                                            
         LTR   RE,RE               IF A REMAINDER, NG                           
         BZ    VROT66                                                           
         DC    H'0'                                                             
*                                                                               
VROT66   STCM  RF,3,1(R4)                                                       
         AR    R1,RF                                                            
         LA    R3,3(,R3)                                                        
         LA    R4,3(,R4)                                                        
         BCT   R0,VROT60                                                        
*                                                                               
         STH   R1,WORK             ROTATION TABLE SIZE                          
         CHI   R1,60               MAX ROT LEN                                  
         BNL   CALROTER                                                         
*                                                                               
VROT68   LR    R3,R1                                                            
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'32'                                                       
         LA    R0,2(,R3)                                                        
         STC   R0,ELEM+1                                                        
         LA    R6,ELEM+2                                                        
*                                                                               
VROT70   LLC   R0,PCTCT            GET NUMBER OF ENTRIES                        
         LA    R1,WORK+2                                                        
*                                                                               
VROT74   SR    RE,RE                                                            
         ICM   RE,3,1(R1)                                                       
         BZ    VROT76                                                           
         MVC   0(1,R6),0(R1)                                                    
         BCTR  RE,0                                                             
         STCM  RE,3,1(R1)                                                       
         LA    R6,1(R6)                                                         
*                                                                               
VROT76   LA    R1,3(R1)                                                         
         BCT   R0,VROT74                                                        
         BCT   R3,VROT70                                                        
*                                                                               
VROT80   GOTO1 ADDELEM                                                          
*                                                                               
         CLC   ELEM(1),SVELEM      SAME ELEMENT ID                              
         BNE   VROTX               DIFFERENT ELEMS, DONE                        
*                                                                               
         LLC   RE,SVELEM+1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   ELEM(0),SVELEM                                                   
         BE    *+8                                                              
         MVI   CHGESW,6                                                         
*                                                                               
VROTX    XIT1                                                                   
*                                                                               
VROTERR  XC    DUB,DUB                                                          
         LA    RE,DUB                                                           
         STCM  RE,7,GASUBST        A(SUBST TEXT)                                
         MVI   DUB,2                                                            
         MVC   DUB+1(1),0(R1)                                                   
         MVC   GERROR,=Y(BADLTTR)                                               
         GOTO1 VTRAERR                                                          
         DROP  R6                                                               
         EJECT                                                                  
*=============================================================                  
* DEVELOP DIVISOR TO BUILD ROTATION TABLE                                       
*=============================================================                  
                                                                                
DIV      NTR1                                                                   
         SR    RE,RE               SEE IF MAX DIVIDES BY MIN                    
         L     RF,MAXPCT                                                        
         D     RE,MINPCT                                                        
         MVC   COMDIV,MINPCT                                                    
         LTR   RE,RE               IF A REMAINDER, NG                           
         BNZ   DIV10                                                            
         BAS   RE,CKD              CK ALL ENTRIES                               
         JE    EQXIT                                                            
*                                                                               
* SEE IF MAX DIVIDES BY MIN/2 *                                                 
*                                                                               
DIV10    TM    COMDIV+3,1          IF ODD NUMBER, BYPASS                        
         BO    DIV14                                                            
         SR    RE,RE                                                            
         L     RF,MAXPCT                                                        
         L     R0,COMDIV                                                        
         SRL   R0,1                DIVIDE BY 2                                  
         ST    R0,COMDIV                                                        
         DR    RE,R0                                                            
         LTR   RE,RE               IF A REMAINDER, NG                           
         BNZ   DIV14                                                            
         BAS   RE,CKD              CK ALL ENTRIES                               
         JE    EQXIT                                                            
*                                                                               
* DIVIDE BY SOME PRIME NOS & SEE IF RESULT IS COMMON DIVISOR *                  
*                                                                               
DIV14    LA    R2,PRIMETBL         TRY LIMITED PRIME NUMBERS                    
*                                                                               
DIV20    LH    R0,0(R2)                                                         
         ST    R0,COMDIV                                                        
         BAS   RE,CKD              CK ALL ENTRIES                               
         BE    DIV30                                                            
         LA    R2,2(R2)                                                         
         CLI   0(R2),255                                                        
         BNE   DIV20                                                            
         J     NEQXIT                                                           
*                                                                               
* NOW DOUBLE DIVISOR AND TRY AGAIN *                                            
*                                                                               
DIV30    L     R0,COMDIV                                                        
         AR    R0,R0                                                            
         ST    R0,COMDIV                                                        
         BAS   RE,CKD              CK ALL ENTRIES                               
         BE    DIV30                                                            
         L     R0,COMDIV                                                        
         SRL   R0,1                                                             
         ST    R0,COMDIV                                                        
         J     EQXIT                                                            
         EJECT                                                                  
*===================================================================            
* FIRST BUILD ABSURD PERCENT ELEMENT                                            
* BUILD AND ADD PERCENTAGE ROTATION ELEMENT                                     
* DEVELOP DIVISOR TO BUILD REAL ROTATION TABLE FROM ABSURD PERCENT              
*                                                                               
* METHOD - DIVIDE EACH ENTRY BY 2, 3, 5, 11, ADDING REMAINDERS                  
* DIVIDE TOTAL REMAINDER BY PRIME, SAVE Q & R, USE SMALLEST #                   
*===================================================================            
                                                                                
ABSEL    NTR1                                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'36'                                                       
         L     RE,PCTNXT           GET A(NEXT ENTRY)                            
         MVI   0(RE),0             MARK END OF LIST                             
         LA    R0,PCTTBL                                                        
         SR    RE,R0               GIVES LENGTH OF ENTRIES                      
         AHI   RE,2                                                             
         STC   RE,ELEM+1           SET ELEM LENGTH                              
         MVC   ELEM+2(45),PCTTBL   MOVE TABLE TO ELEM                           
*                                                                               
         CLI   SVELEM,X'36'        COMPARE ONLY IF X'36'                        
         BNE   ABSEL05             WILL COMPARE X'34' INSTEAD                   
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ELEM+1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   ELEM(0),SVELEM      TEST SAME ELEM/DATA                          
         BE    *+8                                                              
         MVI   CHGESW,7            SET TO ADD NEW REF/SUBL                      
*                                                                               
ABSEL05  GOTO1 ADDELEM                                                          
*                                                                               
         LA    R0,PRCTABLN                                                      
         LA    R1,PRCTAB                                                        
         LA    R2,RELTAB                                                        
*                                                                               
ABSEL10  LLC   R3,PCTCT            GET NUMBER OF PCTS                           
         SR    R4,R4                                                            
         LA    R5,PCTTBL           AND POINT TO FIRST                           
*                                                                               
ABSEL20  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,1(R5)                                                       
         D     RE,0(R1)                                                         
         AR    R4,RE               ADD REMAINDER                                
         LA    R5,3(R5)                                                         
         BCT   R3,ABSEL20                                                       
*                                                                               
         SR    RE,RE                                                            
         LR    RF,R4               GET TOTAL REMAINDERS                         
         D     RE,0(R1)                                                         
         STC   RE,1(R2)            REMAINDER                                    
         STC   RF,0(R2)            QUOTIENT                                     
         LA    R1,4(R1)                                                         
         LA    R2,2(R2)                                                         
         BCT   R0,ABSEL10                                                       
*                                                                               
         LA    R0,PRCTABLN                                                      
         LA    R1,PRCTAB                                                        
         LA    R2,RELTAB                                                        
         LA    R3,2047                                                          
         SR    R4,R4                                                            
*                                                                               
ABSEL30  CLM   R3,3,0(R2)                                                       
         BNH   ABSEL34                                                          
         LR    R4,R1               SAVE ADDRESS OF LOWEST Q/R                   
         ICM   R3,3,0(R2)                                                       
*                                                                               
ABSEL34  LA    R1,4(R1)                                                         
         LA    R2,2(R2)                                                         
         BCT   R0,ABSEL30                                                       
*                                                                               
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   COMDIV,0(R4)                                                     
*                                                                               
         LLC   R0,PCTCT                                                         
         LA    R2,PCTTBL                                                        
         LA    R3,REMTBL                                                        
*                                                                               
ABSEL40  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,1(R2)                                                       
         D     RE,0(R4)                                                         
         STCM  RF,3,1(R2)          STORE QUOTIENT                               
         ST    RE,0(R3)            & REMAINDER                                  
         LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ABSEL40                                                       
*                                                                               
* NOW MULTIPLY EACH BY PRIME #                                                  
*                                                                               
         LLC   R0,PCTCT                                                         
         LA    R2,PCTTBL                                                        
ABSEL50  SR    RE,RE                                                            
         ICM   RE,3,1(R2)                                                       
         MH    RE,2(R4)                                                         
         STCM  RE,3,1(R2)          STORE RESULT                                 
         LA    R2,3(R2)                                                         
         BCT   R0,ABSEL50                                                       
*                                                                               
ABSEL60  LLC   R0,PCTCT            SEE IF SUM 99 OR 100                         
         LA    R2,PCTTBL                                                        
         SR    RF,RF                                                            
*                                                                               
ABSEL64  SR    RE,RE                                                            
         ICM   RE,3,1(R2)                                                       
         AR    RF,RE                                                            
         LA    R2,3(,R2)                                                        
         BCT   R0,ABSEL64                                                       
*                                                                               
         CHI   RF,100                                                           
         JE    EQXIT                                                            
         CHI   RF,99                                                            
         JE    EQXIT                                                            
*                                                                               
         LLC   R0,PCTCT                                                         
         LA    R2,PCTTBL                                                        
         LA    R3,REMTBL                                                        
         SR    R1,R1                                                            
*                                                                               
ABSEL70  C     R1,0(R3)                                                         
         BNL   ABSEL74                                                          
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         L     R1,0(R3)                                                         
*                                                                               
ABSEL74  LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ABSEL70                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(RE)                                                       
         A     R0,0(R4)                                                         
         STCM  R0,3,1(RE)                                                       
         XC    0(4,RF),0(RF)                                                    
         B     ABSEL60                                                          
*                                                                               
PRCTAB   DC    F'2',F'3',F'5',F'11'                                             
PRCTABLN EQU   (*-PRCTAB)/4                                                     
         EJECT                                                                  
*=========================================================                      
* CHECK COMMON DIVISOR AGAINST ALL ENTRIES                                      
*=========================================================                      
                                                                                
CKD      LLC   RF,PCTCT                                                         
         LA    R3,PCTTBL           GET A(FIRST PCT IN ELEM)                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                                                               
CKD10    ICM   R1,3,1(R3)                                                       
         D     R0,COMDIV                                                        
         LTR   R0,R0                                                            
         BNZR  RE                                                               
         LA    R3,3(R3)                                                         
         BCT   RF,CKD10                                                         
         CR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
VROTMVC  MVC   ELEM+2(0),PCTTBL                                                 
         USING NPTPTNEL,R6                                                      
VROTMVCA MVC   NPTPTN(0),WORK                                                   
         DROP  R6                                                               
*                                                                               
CALROTER MVC   GERROR,=Y(LIST2LNG) LIST WOULD BE TOO LONG TO CREATE             
         J     ERREXITD                                                         
*                                                                               
MIXPCTER MVC   GERROR,=Y(NOMIXPCT) MUST HAVE ALL PERCENTS OR NONE               
         J     ERREXITD                                                         
*                                                                               
ROTPCTER MVC   GERROR,=Y(ROTPCT)  ALL PERCENTS MUST ADD UP TO 99 OR 100         
         J     ERREXITD                                                         
*                                                                               
PCTFMTER MVC   GERROR,=Y(INVPCT)  PERCENTS MUST BE 1 OR 2 DIGITS                
         J     ERREXITD                                                         
*                                                                               
ZEROPER  MVC   GERROR,=Y(ZEROPCT) PERCENTS MUST BE NON-ZERO                     
         J     ERREXITD                                                         
*                                                                               
NOPCTER  MVC   GERROR,=Y(NOPCT1CM)  NO PERCENTAGE ALLOWED FOR 1 COMML           
*                                                                               
ERREXITD GOTO1 VTRAERR                                                          
*                                                                               
ROTERR   MVI   ERROR,INVCMLRT      MISSING/EXTRA ROTATION CHAR                  
         LA    R2,TRAROTH                                                       
         GOTO1 ERREX                                                            
*                                                                               
PRIMETBL DC    H'5',H'3',H'2',H'7',H'11',H'13',H'17',H'19',H'23',H'29'          
         DC    H'31',H'37'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*&&DO                                                                           
*=============================================================                  
* FURTHER VALIDATE CML (APPROVALS AND DATES IF ANY)                             
*=============================================================                  
                                                                                
VCMLAPR  NTR1                                                                   
*                                                                               
         MVC   SVCML,WORK          SAVE ORIGINAL CML                            
*                                                                               
         NI    CMLFLAG1,X'FF'-(NOAPDTE+NOAPAIR) INIT NO APRVLS SWITCH           
*                                                                               
         MVI   ELCODE,X'90'        BORADCAST BUSINESS ELEMENT                   
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    VAPR10                                                           
*                                                                               
         CLI   SVT1PROF+12,C'Y'    IS THIS BRAND AGENCY                         
         JNE   VCMLX                                                            
         B     NOAIRERR            YES, NOT APPROVED TO AIR                     
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
VAPR10   DS    0H                                                               
         CLI   CMLBBBAG,C'N'       IS BRAND AGY=N                               
         JE    VCMLX                                                            
         CLI   CMLBBBAG,C'Y'                                                    
         BE    *+12                                                             
         CLI   SVT1PROF+12,C'Y'       IS THIS BRAND AGENCY                      
         JNE   VCMLX                                                            
*                                                                               
         OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                             
         BZ    VAPR20                                                           
*                                                                               
         CLC   SVCMLRCL,CMLBBMXD   COMPARE RECALL DATE TO MAX USE DTE           
         BNH   VAPR20                                                           
         MVC   SVCMLRCL,CMLBBMXD   SAVE EARLIER DATE                            
*                                                                               
         CLC   SVCMLRCL,STRTPAT    IS CML RECALL BEFORE PAT START               
         JL    CMLDTERB             YES, ERROR                                  
         CLC   SVCMLRCL,ENDPAT     SEE IF THIS CMML WILL LAST THRU PAT          
         BNL   VAPR20               YES, OK                                     
         CLC   ENDPAT,=XL3'FFFFFF' IS PAT UFN                                   
         JNE   CMLDTERB                                                         
*                                                                               
VAPR20   DS    0H                                                               
         CLI   CMLBBBAG,0          LEO CML?                                     
         BNE   *+12                                                             
         CLI   SVT1PROF+12,C'Y'    IS THIS BRAND AGENCY                         
         BE    *+12                                                             
         CLI   CMLBBBAG,C'Y'       LEO B. CML                                   
         JNE   VCMLX                NO, DONE                                    
*                                                                               
         OC    CMLBBCAD,CMLBBCAD   ANY CLIENT APPROVAL DATE?                    
         BZ    CADTERR              NO, ERORR                                   
*                                                                               
         CLI   CMLBBAPR,C'Y'       BROADCAST BUSINESS APPROVAL?                 
         BNE   VAPR40              NOT APPROVED TO AIR                          
*                                                                               
         OC    CMLBBREF,CMLBBREF   ANY CML REFERENCE?                           
         JZ    NOAIRERR             NO, ERROR                                   
*                                                                               
* CHECK IF CML FROM REFERENCE FIELD IS APPROVED TO AIR                          
*                                                                               
         MVC   WORK(8),CMLBBREF                                                 
         BRAS  RE,FNDCML           GO FIND COMMERCIAL                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(8),SVCML       RESTORE ORIGINAL CML                         
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         BNE   NOAIRERR                                                         
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
         CLI   CMLATAIR,C'Y'                                                    
         BNE   NOAIRERR                                                         
*                                                                               
VAPR30   MVC   SVANET,CMLANET      SAVE NET APPROVAL BITS                       
*                                                                               
         BAS   RE,GETSTA                                                        
         B     VAPRX                                                            
*                                                                               
VAPR40   CLI   CMLATAIR,C'Y'       APPROVED TO AIR                              
         JNE   NOAIRERR                                                         
         B     VAPR30                                                           
*                                                                               
VAPRX    J     VCMLX                                                            
         EJECT                                                                  
*=============================================================                  
* GET STATIONS APPROVAL RECORDS FOR THIS PATTERN                                
* SEE IF CML HAS BEEN APPROVED                                                  
*=============================================================                  
                                                                                
GETSTA   NTR1                                                                   
         MVC   SVCMLKEY,KEY        SAVE CML KEY                                 
         MVC   SVWORK,WORK                                                      
*                                                                               
         BRAS  RE,INITNET                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STATRECD,R4         STATION RECORD                               
         MVI   STATKID,X'29'       RECORD ID                                    
         MVC   STATKAM,BAGYMD      AGY/MED                                      
*                                                                               
         CLI   NETWORK,0                                                        
         BE    *+16                ALL NETWORK PATTERN                          
         MVC   STATKNET,NETWORK    VALIDATE THIS NETWORK ONLY                   
         OC    STATKNET,SPACES                                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     GETST08                                                          
*                                                                               
GETST05  DS    0H                                                               
         CLI   NETWORK,0           ANY NETWORK                                  
         BNE   GETSTX              NETWORK SPECIFIC                             
         GOTO1 SEQ                                                              
*                                                                               
GETST08  CLC   KEY(2),KEYSAVE                                                   
         BNE   GETSTX                                                           
*                                                                               
         CLI   NETWORK,0           ALL NETWORK/MEDIA PATTERN                    
         BE    GETST10                                                          
         CLC   KEY(7),KEYSAVE      STATION RECORD FOR THIS NETWORK              
         BNE   GETSTX                                                           
         B     GETST15                                                          
*                                                                               
GETST10  CLI   NETWORK+2,X'FF'     MEDIA PATTERN                                
         BNE   GETST15                                                          
*                                                                               
         MVI   FMEDIA,C'Y'         FIND MEDIA                                   
         MVC   WORK(L'STATKNET),STATKNET                                        
         BRAS  RE,VNET                                                          
*                                                                               
         CLC   SVMEDIA,NETWORK+1   SAME MEDIA                                   
         BNE   GETST05             NO, GET NEXT                                 
*                                                                               
GETST15  L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    GETST20                                                          
         DC    H'0'                                                             
*                                                                               
         USING STADATEL,R6                                                      
*                                                                               
GETST20  CLC   STAIDATE,=X'FFFFFF' ANY INACTIVE DATE                            
         BE    GETST30              NO                                          
*                                                                               
         MVC   IDATE,STAIDATE                                                   
         XC    IDATE,=X'FFFFFF'    INVERT INACTIVE DATE                         
*                                                                               
         CLC   IDATE,STRTPAT       IF INACTIVE IS BEFORE PAT START              
         BL    GETST05             DONE WITH THIS RECORD                        
*                                                                               
GETST30  DS    0H                                                               
         CLC   STAADATE,ENDPAT     ACTIVE DATE BEFORE PAT END DATE              
         BH    GETST05             YES                                          
         DROP  R6                                                               
*                                                                               
         L     R6,AIO2                                                          
GETST40  MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING STACODEL,R6                                                      
*                                                                               
         MVC   SVCODE,STACODE      SAVE STATION CODE                            
*                                                                               
         MVC   WORK(8),SVANET      APPROVED NETWORKS                            
         NC    WORK(8),SVCODE      AGAINST THIS NETWORK CODE                    
         XC    SVCODE,WORK                                                      
         BNZ   NOAIRERR            NOT APPROVED TO AIR                          
         B     GETST05                                                          
*                                                                               
GETSTX   DS    0H                                                               
         BRAS  RE,INITXSP          SET FOR XFILE                                
*                                                                               
         MVC   KEY(L'SVCMLKEY),SVCMLKEY RESTORE KEY                             
         GOTO1 HIGH                DUMMY READ HI                                
*                                                                               
         MVC   WORK,SVWORK         RESTORE WORK                                 
*                                                                               
         XIT1                                                                   
         DROP  R6                                                               
*&&                                                                             
*                                                                               
CADTERR  DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+12                                                             
         OI    CMLFLAG1,NOAPDTE     NO CLIENT APPROVAL DATE                     
         J     VCMLX                                                            
*                                                                               
         MVC   GERROR,=Y(NOCLADTE) NO CLIENT APPROVAL DATE                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,9                                                           
         MVC   ELEM+1(8),WORK                                                   
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
*                                                                               
         J     ERREXITC                                                         
*                                                                               
NOAIRERR DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BZ    *+12                                                             
         OI    CMLFLAG1,NOAPAIR     NOT APPROVED TO AIR                         
         J     VCMLX                                                            
*                                                                               
         MVC   GERROR,=Y(NAPRTAIR)  NOT APPROVED TO AIR                         
         XC    ELEM,ELEM                                                        
         MVI   ELEM,9                                                           
         MVC   ELEM+1(8),SVCML                                                  
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
*                                                                               
         B     ERREXITC                                                         
*                                                                               
PTPRDERC DS    0H                                                               
         MVC   GERROR,=Y(PATPRDER) PAT PRD NOT IN CML                           
         CLI   MODE,PRINTREP                                                    
         BNE   *+12                                                             
         OI    CMLFLAG,CMLPRDSW    PATTERN PROD NOT IN CML                      
         J     VCMLX                                                            
*                                                                               
         USING NPTXKEY,R4                                                       
         L     R4,AIO1                                                          
         LA    R3,NPTXPRD                                                       
         TM    PRDMATSW,PRDMATPR   WAS 1ST PRD FOUND                            
         BZ    PTPRC10              NO                                          
         LA    R3,NPTXPRD2                                                      
*                                                                               
PTPRC10  LA    R5,FLD              ADDRESS OF OUTPUT AREA                       
         MVC   FLD,SPACES                                                       
         CLI   0(R3),0             ANY PRODUCT CODE                             
         BE    PPRD16C             NO,DONE                                      
         MVC   FLD(3),0(R3)                                                     
         B     PPRD16C                                                          
*                                                                               
PPRD16C  DS   0H                                                                
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         STCM  R3,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R3),7             L'SUBST TEXT + 1                             
         MVC   1(6,R3),FLD                                                      
         B     ERREXITC                                                         
*                                                                               
PGSOERR  MVC   GERROR,=Y(INVPGSO) COMML PIGGY/SOLO CODE MUST MATCH USG          
         CLI   MODE,PRINTREP                                                    
         JNE   ERREXITC                                                         
         OI    CMLFLAG,PGSOSW     COMML PIGGY/SOLO MUST MATCH USAGE             
         J     VCMLX                                                            
*                                                                               
ERREXITC GOTO1 VTRAERR                                                          
*                                                                               
INVSLNER MVI   ERROR,PATCMLER      PAT/CML SPOT LENS DIFFERENT                  
         CLI   MODE,PRINTREP                                                    
         JNE   TRAPERR                                                          
         OI    CMLFLAG,CMLPATLN    PAT/CML SPOT LENS DIFFERENT                  
         J     VCMLX                                                            
*                                                                               
CMLDTERA MVI   ERROR,BDCMLDTS      CML RELEASE DTE AFTER PAT START              
         CLI   MODE,PRINTREP                                                    
         JNE   TRAPERR                                                          
         OI    CMLFLAG,CMLDTSSW    CML REL DATE AFTER PAT START                 
         J     VCMLX                                                            
*                                                                               
CMLDTERB MVI   ERROR,BDCMLDTE      CML RECALL DTE BEFORE PAT END                
         CLI   MODE,PRINTREP                                                    
         JNE   TRAPERR                                                          
         OI    CMLFLAG,CMLDTESW    CML RCL DATE BEFORE PAT START                
         J     VCMLX                                                            
*                                                                               
VCMLERR  MVI   ERROR,INVCOMM       NO SUCH CMML FOR CLT                         
         CLI   MODE,PRINTREP                                                    
         JNE   TRAPERR                                                          
         OI    CMLFLAG1,CMLINVAL   INVALID COMMERCIAL                           
         J     VCMLX                                                            
*                                                                               
EXNETERR XC    CONHEAD,CONHEAD                                                  
         BRAS  R1,*+44                                                          
         DC    CL40'EXCLUDED NETWORKS IN COMMERCIAL RECORDS'                    
         MVC   CONHEAD(40),0(R1)                                                
         GOTO1 ERREX2                                                           
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
* FILTER PATTERNS FOR LIST FUNCTION                                             
*                                                                               
FTR      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         USING NPTDTAEL,R6                                                      
*                                                                               
         TM    FLAGFTR,FLAGALL     SHOW BOTH DEL & UNDEL                        
         BO    FTR06                YES                                         
*                                                                               
         L     RE,AIO                                                           
         CLI   NPTXPSSV-NPTKEY+KEY,0  TEST NETWORK PASSIVE POINTER              
         BE    FTR02                  NO                                        
*                                                                               
         TM    FLAGFTR,FLAGDEL     ANY DELETE FILTER                            
         BO    FTR01               NO                                           
* SHOW ONLY NON-DELETED NETWORKS                                                
         BRAS  RE,CKPATLST         SEE IF THIS NETWORK IS DELETED               
         BNZ   FTRNO               SKIP IF DELETED                              
         B     FTR06                                                            
* SHOW ONLY DELETED NETWORKS                                                    
FTR01    BRAS  RE,CKPATLST         SEE IF THIS NETWORK IS DELETED               
         BZ    FTRNO               SKIP IF **NOT** DELETED                      
         B     FTR06                                                            
*                                                                               
FTR02    TM    FLAGFTR,FLAGDEL     ANY DELETE FILTER                            
         BZ    FTR04               NO                                           
         TM    NPTSTAT,X'80'       SOFT DELETE                                  
         BZ    FTRNO                                                            
         B     FTR06                                                            
*                                                                               
FTR04    TM    NPTSTAT,X'80'                                                    
         BO    FTRNO                                                            
*                                                                               
FTR06    OC    FILTERS,FILTERS     ANY FILTERS                                  
         BZ    FTRYES              NO, SO EVERYTHING PASSES                     
         CLI   DATEFTR1,0          DATE FILTER                                  
         BE    FTR16                                                            
         CLI   DATEFTR2,0          SECOND DATE FILTER                           
         BE    FTR10                                                            
         CLC   DATEFTR1,NPTEND                                                  
         BH    FTRNO                                                            
         CLC   DATEFTR2,NPTSTART                                                
         BL    FTRNO                                                            
         B     FTR16                                                            
*                                                                               
FTR10    CLI   DATESFTR,0                                                       
         BE    FTR14                                                            
         CLI   DATESFTR,X'4C'      GREATER THAN                                 
         BNE   FTR12               MUST BE LESS THAN                            
         CLC   DATEFTR1,NPTEND     FILTER TO RECALL                             
         BNH   FTRNO               BYPASS                                       
         B     FTR16               CK NEXT FILTER                               
*                                                                               
FTR12    CLC   DATEFTR1,NPTEND                                                  
         BNL   FTRNO                                                            
         B     FTR16                                                            
*                                                                               
FTR14    CLC   DATEFTR1,NPTSTART                                                
         BL    FTRNO                                                            
         CLC   DATEFTR1,NPTEND                                                  
         BH    FTRNO                                                            
*                                                                               
FTR16    OC    PERFTR,PERFTR       PERIOD FILTER                                
         BZ    FTR20                                                            
         CLC   PERFTR(3),NPTSTART  PERIOD START TO PAT START                    
         BNE   FTRNO                                                            
         CLC   PERFTR+3(3),NPTEND  PERIOD END TO PAT END                        
         BNE   FTRNO                                                            
*                                                                               
FTR20    OC    CODEFTR,CODEFTR     IS THERE A CODE FILTER                       
         BZ    FTR40               NO                                           
*                                                                               
         CLI   KEY+NPTXPROG-NPTXKEY,X'FF'                                       
         BNE   FTRNO                                                            
         CLC   QDPT,KEY+NPTXPROG+1-NPTXKEY                                      
         BNE   FTRNO                                                            
*                                                                               
FTR40    OC    CMLFTR,CMLFTR       IS THERE A CML ID FILTER                     
         BZ    FTR50               NO                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FTRNO                                                            
         USING NPTCMLEL,R6                                                      
*                                                                               
         LLC   R0,NPTCMLLN                                                      
         SRL   R0,4                DIVIDE BY 16 = NUMBER OF CML'S               
         LA    R4,NPTCML           START OF CML LIST                            
*                                                                               
FTR42    CLC   0(8,R4),CMLFTR                                                   
         BE    FTR50                                                            
         CLC   8(8,R4),CMLFTR                                                   
         BE    FTR50                                                            
         CLI   CMLFTRADI,C'Y'      IF FILTER IS PACKED                          
         BNE   FTR44                                                            
         CLC   0(8,R4),CMLFTREBC   TRY MATCHING UNPACKED                        
         BE    FTR50                                                            
         CLC   8(8,R4),CMLFTREBC                                                
         BE    FTR50                                                            
FTR44    LA    R4,16(R4)                                                        
         BCT   R0,FTR42                                                         
         B     FTRNO                                                            
*                                                                               
FTR50    DS    0H                                                               
         OC    QPGR,QPGR           IS THERE A PGROUP FILTER                     
         BZ    FTRYES               NO                                          
*                                                                               
         L     R4,AIO                                                           
         USING NPTXKEY,R4                                                       
*                                                                               
         LA    R2,NPGRLIST                                                      
         LA    R3,PGRLIST          LIST OF PRODUCTS                             
*                                                                               
FTR52    DS    0H                                                               
         CLC   NPTXPRD,0(R3)       SAME PROD                                    
         BE    FTRYES                                                           
         CLC   NPTXPRD2,0(R3)                                                   
         BE    FTRYES                                                           
         LA    R3,3(R3)            BUMP IN PRD LIST                             
         CLI   0(R3),0                                                          
         BE    FTRNO                                                            
         BCT   R2,FTR52                                                         
         B     FTRNO                                                            
*                                                                               
FTRYES   J     EQXIT               SET COND CODE FILTERED OK                    
*                                                                               
FTRNO    J     NEQXIT              SET COND CODE NO FILTER                      
         DROP  R4,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* VALIDATE COMMENT(S)                                                           
*==========================================================                     
                                                                                
VCMT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,1                COMMENT NUMBER                               
         LA    R5,4                MAX POSSIBLE COMMENT LINES                   
         MVI   ELCODE,X'40'                                                     
         GOTO1 REMELEM             WILL REMOVE ALL X'40' ELEMENTS               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING NPTCMTEL,R6                                                      
         MVI   NPTCMTEL,X'40'                                                   
         LA    R2,PCMCMT1H                                                      
*                                                                               
VCMT10   CLI   5(R2),0                                                          
         BE    VCMT20                                                           
         CLI   5(R2),52            MAX COMMENT LENGTH IS 52                     
         BNH   *+14                                                             
         MVC   GERROR,=Y(COMLENER)                                              
         B     VCMTERR                                                          
*                                                                               
         GOTO1 ANY                                                              
         LLC   R1,5(R2)            LENGTH                                       
         LA    R0,3(,R1)                                                        
         STC   R0,NPTCMTLN                                                      
         STC   R3,NPTCMTNO         COMMENT NUMBER                               
         BCTR  R1,0                                                             
         EX    R1,VCMTMVC                                                       
         GOTO1 ADDELEM                                                          
         LA    R3,1(,R3)           INCREMENT COMMENT NUMBER                     
*                                                                               
VCMT20   LLC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               POINT TO NEXT FIELD                          
         BCT   R5,VCMT10                                                        
         XIT1                                                                   
*                                                                               
VCMTMVC  MVC   NPTCMT(0),WORK                                                   
         DROP  R6                                                               
VCMTERR  GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* VALIDATE HIATUS DATA                                                          
*==========================================================                     
                                                                                
VHIAT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ELCODE,X'5C'                                                     
         GOTO1 REMELEM             WILL REMOVE ALL X'5C' ELEMENTS               
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING NPTHIAEL,R6                                                      
         MVI   NPTHIAEL,X'5C'                                                   
         MVI   NPTHIALN,NPTHIAX-NPTHIAEL                                        
         LA    R2,PCMHIA1H                                                      
*                                                                               
VHIA10   CLI   5(R2),0                                                          
         BE    VHIA60                                                           
*                                                                               
* TEST INPUT IS VALID DAYS, FORMAT IS MO-FR,1000A-1100P                         
*                                                                               
         MVC   WORK(20),8(R2)      MOVE INPUT                                   
         LA    RE,WORK             POINT TO FIRST CHAR                          
         LA    RF,20                                                            
VHIA12   CLI   0(RE),C','          FIND A TIME DELIMITER                        
         BE    VHIA14                                                           
         CLI   0(RE),C' '                                                       
         BNH   VHIA14                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,VHIA12                                                        
*                                                                               
VHIA14   MVI   0(RE),C' '          MAKE DELIMITER A SPACE                       
         LA    R4,WORK                                                          
         SR    R4,RE                                                            
         LPR   R4,R4               SAVE DAYS LENGTH IN R4                       
*                                                                               
         GOTO1 DAYVAL,DMCB,((R4),WORK),FULL,NPTHIASE                            
         CLI   FULL,0              IF 0, INVALID                                
         BE    VHIA20              SO TRY FOR DATES                             
         MVC   NPTHIADT+2(1),FULL                                               
         CLI   FULL,X'7F'          TEST ENTERED MO-SU                           
         BNE   VHIA30                                                           
         MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
*                                                                               
* INPUT MUST BE VALID DATES                                                     
* FORMAT IS JAN01/12,1000A-1000P                                                
*                                                                               
VHIA20   MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
*                                                                               
         MVI   NEEDYEAR,C'Y'                                                    
         OC    NPTEND,NPTEND       TEST PATTERN IS UFN                          
         BZ    VHIA22                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,NPTSTART),WORK                                    
         GOTO1 (RF),(R1),(3,NPTEND),WORK+6                                      
*                                                                               
         MVC   DUB(3),NPTSTART     MOVE PATTERN START DATE                      
         LLC   RE,DUB                                                           
         LA    RE,1(RE)                                                         
         STC   RE,DUB              ADVANCE ONE YEAR                             
         CLC   DUB(3),NPTEND       IS DATE AFTER END DATE                       
         BL    VHIA22              NO - NEED TO INPUT YEAR                      
         MVI   NEEDYEAR,C'N'       ELSE DON'T NEED YEAR                         
*                                                                               
VHIA22   LA    R6,ELEM                                                          
         USING NPTHIAEL,R6                                                      
*                                                                               
VHIA24   CLC   =C'DEL',8(R2)       TEST TO DELETE THIS ONE                      
         BE    VHIA60                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,8(R2),DUB  VALIDATE FOR M/D/Y                        
         ICM   R4,15,0(R1)            TEST DATE VALID                           
         BNZ   VHIA26                 YES                                       
         CLI   NEEDYEAR,C'Y'                                                    
         BE    VHIAERR2                                                         
         GOTO1 (RF),(R1),(1,8(R2))    VALIDATE FOR M/D                          
         ICM   R4,15,0(R1)            TEST VALID                                
         BZ    VHIAERR2                                                         
*                                                                               
         MVC   DUB(2),WORK         MOVE PAT START YEAR                          
         CLC   DUB+2(4),WORK+2     INPUT MMDD TO PATSTART MMDD                  
         BNL   *+10                                                             
         MVC   DUB(2),WORK+6       IF LOW USE PAT END YEAR                      
*                                                                               
VHIA26   CLC   DUB(6),WORK         TEST BEFORE PATTERN START                    
         BL    VHIAERR6                                                         
         CLC   DUB(6),WORK+6       OR AFTER PATTERN END                         
         BH    VHIAERR6                                                         
         GOTO1 DATCON,DMCB,DUB,(3,NPTHIADT)                                     
*                                                                               
VHIA30   LLC   R5,5(R2)            GET FIELD INPUT LENGTH                       
         SR    R5,R4               GIVES REMAINING LENGTH IN R5                 
         BZ    VHIA32              IF NO TIME, DONE                             
*                                                                               
* THERE'S MORE INPUT, SO VALIDATE TIME                                          
*                                                                               
         LA    RE,8(R2,R4)         POINT TO CHAR AFTER DATE                     
         CLI   0(RE),C','          SHOULD BE A COMMA!                           
         BNE   VHIAERR3            IF NOT, TELL THEM HOW                        
         BCTR  R5,0                                                             
         GOTO1 TIMVAL,DMCB,((R5),1(RE)),DUB                                     
         OC    DUB+2(2),DUB+2      MAKE SURE 2 TIMES                            
         BZ    VHIAERR2                                                         
         CLC   DUB(2),DUB+2        MAKE SURE NOT THE SAME                       
         BE    VHIAERR2                                                         
         MVC   NPTHIATM,DUB                                                     
                                                                                
*============================================================                   
* SEE IF NEW ELEM OVERLAPS ANY ELEM ALREADY IN RECORD                           
* AND DON'T ALLOW BOTH DAYS AND DATES                                           
*============================================================                   
                                                                                
VHIA32   CLI   NPTHIADT+1,0        TEST NEW ELEM IS DAYS                        
         BNE   VHIA40              NO                                           
*                                                                               
         L     R6,AIO              CHECK FOR DAYS OVERLAP                       
         MVI   ELCODE,X'5C'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VHIA50                                                           
*                                                                               
VHIA34   CLI   NPTHIADT+1,0        TEST ELEM FROM REC IS DATE                   
         BNE   VHIAERR7            YES - ERROR                                  
*                                                                               
         OC    NPTHIATM,NPTHIATM   TEST ELEM FROM REC HAS TIME                  
         BNZ   VHIA36                                                           
         LA    RE,NPTHIATM-NPTHIAEL+ELEM TEST NEW ELEM HAS TIME                 
         OC    0(2,RE),0(RE)                                                    
         BZ    VHIAERR1                  NO - ERROR                             
*                                                                               
VHIA36   LLC   R0,NPTHIADT+2       GET DAYS FROM RECORD ELEM                    
         LLC   RE,NPTHIADT+2-NPTHIAEL+ELEM  GET NEW DAYS                        
         NR    R0,RE                                                            
         BZ    VHIA38              NO DAYS IN COMMON - GOOD                     
* AT LEAST ONE DAY IN COMMON                                                    
         OC    NPTHIATM,NPTHIATM   TEST ELEM IN REC HAS TIME                    
         BZ    VHIAERR4            NO - SHOULD NOT OVERLAP                      
         LA    RE,NPTHIATM-NPTHIAEL+ELEM                                        
         OC    0(4,RE),0(RE)       TEST NEW ELEM HAS TIME                       
         BZ    VHIAERR4             NO - SHOULD NOT OVERLAP                     
*                                                                               
         BAS   RE,CHKTIMOV         SEE IF TIMES OVERLAP                         
         BNZ   VHIAERR4                                                         
*                                                                               
VHIA38   BRAS  RE,NEXTEL                                                        
         BE    VHIA34                                                           
         B     VHIA50                                                           
*                                                                               
VHIA40   L     R6,AIO              CHECK FOR DAYS OVERLAP                       
         MVI   ELCODE,X'5C'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VHIA50                                                           
*                                                                               
VHIA42   CLI   NPTHIADT+1,0        TEST ELEM IN REC IS DAYS                     
         BE    VHIAERR7            YES - ERROR                                  
         LA    RE,NPTHIADT-NPTHIAEL+ELEM                                        
         CLC   NPTHIADT,0(RE)      TEST SAME DATE                               
         BNE   VHIA44                                                           
         OC    NPTHIATM,NPTHIATM   BOTH ELS SHOULD HAVE TIME                    
         BZ    VHIAERR5                                                         
         LA    RE,NPTHIATM-NPTHIAEL+ELEM                                        
         OC    0(4,RE),0(RE)                                                    
         BZ    VHIAERR5                                                         
*                                                                               
         BAS   RE,CHKTIMOV                                                      
         BNZ   VHIAERR5                                                         
*                                                                               
VHIA44   BRAS  RE,NEXTEL                                                        
         BE    VHIA42                                                           
*                                                                               
VHIA50   LA    R6,ELEM             RESTORE POINTER TO NEW ELEM                  
         GOTO1 ADDELEM                                                          
         XC    ELEM+2(16),ELEM+2   CLEAR DATA FROM ELEM                         
*                                                                               
VHIA60   LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,PCMHIAXH                                                      
         CR    R2,R0                                                            
         BH    VHIAX                                                            
         CLI   5(R2),0                                                          
         BE    VHIA60                                                           
         B     VHIA10              GO EDIT NEXT ENTRY WITH DATA                 
*                                                                               
VHIAX    XIT1                                                                   
*                                                                               
CHKTIMOV NTR1                                                                   
         MVC   DUB(4),NPTHIATM                                                  
         LA    RE,NPTHIATM-NPTHIAEL+ELEM                                        
         MVC   DUB+4(4),0(RE)                                                   
*                                                                               
         LA    R1,DUB                                                           
         LA    R0,4                                                             
*                                                                               
CHKTIM2  CLC   0(2,R1),=H'2400'                                                 
         BNE   *+10                                                             
         XC    0(2,R1),0(R1)                                                    
         LA    R1,2(R1)                                                         
         BCT   R0,CHKTIM2                                                       
*                                                                               
         CLC   DUB(2),DUB+6        ONE START AFTER OTHER ENDS                   
         JH    EQXIT                                                            
         CLC   DUB+2(2),DUB+4      OR END BEFORE OTHER STARTS                   
         JL    EQXIT                                                            
         J     NEQXIT                                                           
         DROP  R6                                                               
*                                                                               
VHIAERR1 MVC   GERROR,=Y(DAYSONCE)                                              
         B     VHIAERR                                                          
*                                                                               
VHIAERR2 MVC   GERROR,=Y(INPUTYY)                                               
         B     VHIAERR                                                          
*                                                                               
VHIAERR3 MVC   GERROR,=Y(BADDATE)                                               
         B     VHIAERR                                                          
*                                                                               
VHIAERR4 MVC   GERROR,=Y(DAYSOVLP)                                              
         B     VHIAERR                                                          
*                                                                               
VHIAERR5 MVC   GERROR,=Y(DATEOVLP)                                              
         B     VHIAERR                                                          
*                                                                               
VHIAERR6 MVC   GERROR,=Y(NOTINPAT)                                              
         B     VHIAERR                                                          
*                                                                               
VHIAERR7 MVC   GERROR,=Y(DAYSNDTS)                                              
*                                                                               
VHIAERR  GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* READ CLIENT HEADER TO TEST IF ACCESS AUTHORIZED                               
*=================================================================              
                                                                                
FCLT     NTR1  BASE=*,LABEL=*                                                   
         MVC   SVMYKEY,KEY                                                      
*                                                                               
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         BRAS  RE,INITXSP                                                       
         MVC   KEY,SVMYKEY         RESTORE MY KEY                               
         GOTO1 HIGH                                                             
*                                                                               
         CLI   ERROR,0             ANY ERRORS                                   
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE KEY ROUTINE *                                                        
*                                                                               
VK       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   ACTNUM,ACTDEL       DELETE IS INVALID ACTION                     
         BNE   VK5                                                              
         MVI   ERROR,INVACT        DELETE IS INVALID                            
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         GOTO1 ERREX                                                            
*                                                                               
VK5      BRAS  RE,INITSPT                                                       
*                                                                               
         LA    R2,ELEM             FAKE VALIDATE MEDIA                          
         MVC   ELEM,=X'0A01000184010001'                                        
         MVI   ELEM+8,C'N'                                                      
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT(6),BCLT        CLEAR BCLT,BSLN,BSLN2                        
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK12                                                             
         TM    WHEN,X'78'          IF OFFLINE, NOT NEEDED                       
         BZ    VK12                 NO                                          
*                                                                               
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
*                                                                               
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
*                                                                               
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
*                                                                               
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPER1                                                          
*                                                                               
         MVI   ERROR,0                                                          
         B     VK20                                                             
*                                                                               
VK12     GOTO1 VALICLT                                                          
*                                                                               
         OI    4(R2),X'20'                                                      
*                                                                               
         MVI   BYTE,0              SET FOR ALL PROFILES                         
         BRAS  RE,FPRO             GO GET PROFILE REC                           
*                                                                               
VK20     LA    R2,TRANETH          NETWORK                                      
         CLI   8(R2),C'$'          TEST NETWORK LIST PATTERN                    
         BNE   VK22                                                             
*                                                                               
         XC    NETWORK,NETWORK                                                  
         XC    PROGRAM,PROGRAM                                                  
         XC    SVPROG,SVPROG                                                    
*                                                                               
         CLI   5(R2),1             TEST INPUT LEN=1                             
         JNE   INVALERR                                                         
         MVI   NETWORK,C'$'                                                     
         LA    R2,TRAPROGH                                                      
         CLI   5(R2),0             NO PROGRAM ALLOWED                           
         JNE   INVALERR                                                         
*                                                                               
         LA    R2,TRACODEH                                                      
         BRAS  RE,VCC              VALIDATE DAYPART IF ANY                      
*                                                                               
         LA    R2,TRAFEEDH                                                      
         CLI   5(R2),0                                                          
         JNE   INVALERR                                                         
         LA    R2,TRANETH                                                       
         B     VK24                                                             
*                                                                               
VK22     MVI   FMEDIA,C'N'                                                      
         BRAS  RE,VNET                                                          
*MNV                                                                            
         CLC   =C'ALL',TRANET                                                   
         BE    VK23                                                             
         CLC   =C'M=',TRANET                                                    
         BE    VK23                                                             
         CLI   TRANET,C'$'                                                      
         BE    VK23                                                             
         CLC   TRANET,SPACES                                                    
         BNH   VK23                                                             
         TM    FLAGFTR,FLAGDIGI    INCLUDE DIGITAL PATTERNS                     
         BO    VK23                                                             
         CLI   SVTN2PRO+14,C'Y'                                                 
         BNE   VK23                                                             
         CLI   SVMEDIA,C'V'                                                     
         BNE   VK23                                                             
         MVC   GERROR,=Y(BDNETMED)                                              
         GOTO1 VTRAERR                                                          
                                                                                
VK23     DS    0H                                                               
*MNV                                                                            
*                                                                               
         LA    R2,TRAPROGH                                                      
         BRAS  RE,VPR                                                           
*                                                                               
         LA    R2,TRACODEH         DAYPART CODE                                 
         BRAS  RE,VCC                                                           
*                                                                               
         LA    R2,TRAFEEDH         FEED                                         
         BRAS  RE,VFD                                                           
*                                                                               
VK24     LA    R2,TRAPRLNH         PRODUCT                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK30                 YES                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST                                  
         BE    VK40                                                             
         MVI   ERROR,MISSING                                                    
*                                                                               
TRAPER1  GOTO1 ERREX                                                            
*                                                                               
VK30     GOTO1 VALIPRD                                                          
*                                                                               
         MVC   QPRD,WORK                                                        
         CLI   WORK+4,0            VALID SPOT LEN                               
         BNE   VK34                YES                                          
         CLI   ACTNUM,ACTLIST      NOT NEEDED FOR LIST                          
         BE    VK34                                                             
*                                                                               
         MVI   WORK+4,30           DEFAULT SPOT LENGTH IS 30 SEC                
         LA    R0,L'TRAPRLN                                                     
         LA    R1,TRAPRLN                                                       
         CLI   0(R1),C' '                                                       
         BNH   *+16                                                             
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
         B     BADPRDER                                                         
         MVC   0(3,R1),=C'-30'                                                  
         OI    TRAPRLNH+6,X'80'    FORCE TRANSMIT                               
VK34     MVC   BPRD(2),WORK+3      GET BIN PROD AND SPOT LENGTH                 
*                                                                               
VK40     LA    R2,TRAPTLNH         PRODUCT PARTNER                              
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK50                NO                                           
         GOTO1 VALIPRD                                                          
*                                                                               
         CLI   WORK+4,0            VALID SPOT LEN                               
         BNE   VK44                YES                                          
         CLI   ACTNUM,ACTLIST      NOT NEEDED FOR LIST                          
         BE    VK44                                                             
         MVI   WORK+4,30           DEFAULT SPOT LENGTH IS 30 SEC                
         LA    R0,L'TRAPTLN                                                     
         LA    R1,TRAPTLN                                                       
         CLI   0(R1),C' '                                                       
         BNH   *+16                                                             
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
         B     BADPRDER                                                         
         MVC   0(3,R1),=C'-30'                                                  
         OI    TRAPRLNH+6,X'80'    FORCE TRANSMIT                               
*                                                                               
VK44     CLC   QPRD,WORK           COMPARE 2 PRODUCTS                           
         BH    PRDSQERR            MUST BE LOWER FIRST                          
*NOP     BE    EQPRDERR            CAN NOT BE 2 EQUAL                           
         B     VK46                                                             
*                                                                               
PRDSQERR MVI   ERROR,INVPRDSQ      PRODS OUT OF SEQ                             
         B     *+12                                                             
*                                                                               
EQPRDERR MVI   ERROR,INVEQPRD      PROD/PARTNER PROD EQ                         
         LA    R2,TRAPRLNH         POINT TO PROD-SPOT LEN                       
         GOTO1 ERREX                                                            
*                                                                               
VK46     MVC   QPRD2,WORK                                                       
         MVC   BSLN2,WORK+4        SPOT LENGTH 2                                
*                                                                               
VK50     LA    R2,TRAREFH          REFERENCE NUMBER                             
         BRAS  RE,VREF                                                          
*                                                                               
* VALIDATE FEED HERE *                                                          
*                                                                               
         LA    R2,TRAFLTRH         FILTERS                                      
         BRAS  RE,VFTR                                                          
*                                                                               
VK100    XC    KEY,KEY             BUILD XSPFILE KEY                            
         LA    R4,KEY                                                           
         USING NPTXKEY,R4                                                       
*                                                                               
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM(3),BAGYMD    A/M,CLT                                      
         MVC   NPTXNET,NETWORK                                                  
*                                                                               
         CLI   QDPT,0              THIS REALLY DAYPART                          
         BNE   VK110                YES                                         
         OC    FEED,FEED           OR FEED                                      
         BNZ   VK110                YES                                         
         MVC   NPTXPROG,PROGRAM                                                 
         B     VK120                                                            
*                                                                               
VK110    MVI   NPTXPROG,X'FF'                                                   
         MVC   NPTXPROG+1(1),QDPT                                               
         MVC   NPTXPROG+2(4),FEED                                               
         OC    PROGRAM,PROGRAM                                                  
         BZ    VK120                                                            
         DC    H'0'                                                             
*                                                                               
VK120    MVC   NPTXPRD,QPRD        MOVE IN 3 CHAR PROD                          
         MVC   NPTXSLN,BSLN        PRD LENGTH                                   
         CLC   QPRD2,SPACES                                                     
         BNH   *+16                                                             
         MVC   NPTXPRD2,QPRD2      3 CHAR PARTNER PRD                           
         MVC   NPTXSLN2,BSLN2      PARTNER LENGTH                               
*                                                                               
         MVC   NPTXR3F,BREFK+1                                                  
         MVC   COMPKEY,KEY                                                      
         MVC   SVXNKEY,KEY           SAVE KEY JUST BUILT                        
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    *+14                                                             
         CLC   TWAKEYSV(32),SVXNKEY  TEST SAME RECORD                           
         BE    VKX                                                              
*                                                                               
         MVC   MYPFKEY,PFKEY       SAVE REAL PFKEY                              
         BRAS  RE,GETVRCNT         GET VERSION COUNT                            
*                                                                               
         MVC   KEY,SVXNKEY         RESTORE NEW RECORD KEY!                      
         XC    SVMYKEY,SVMYKEY     CLEAR LAST LIST RECORD KEY                   
*                                                                               
VKX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
* GET PROFILE REC(S)                                                            
*                                                                               
FPRO     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ TN1 PROFILE *                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,X'A2'                                                       
         MVC   WORK+1(3),=C'TN2'   GET TN2 PROFILE                              
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF CLIENT OFFICE                                
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    FPRO10                                                           
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    FPRO10                                                           
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
                                                                                
* ALWAYS GET TN2 PROFILE FIRST                                                  
                                                                                
FPRO10   MVI   WORK+3,C'2'         TN2 PROFILE                                  
         GOTO1 GETPROF,DMCB,WORK,SVTN2PRO,DATAMGR                               
         CLI   BYTE,C'2'           TEST TN2 PROFILE ONLY                        
         JE    EXIT                                                             
*                                                                               
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         MVI   WORK+3,C'3'         TN3 PROFILE                                  
         GOTO1 (RF),(R1),,SVTN3PR0                                              
         J     EXIT                                                             
*                                                                               
BADPRDER MVC   GERROR,=Y(PRDPRDLN)                                              
         GOTO1 VTRAERR                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
DR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVXNKEY,KEY                                                      
*                                                                               
         TM    SECFLAG,BLSSW       TEST BRAND LEVEL SECURITY                    
         BZ    *+8                                                              
         BAS   RE,DRSEC            GO CHECK SECURITY                            
*                                                                               
         BRAS  RE,INITXSP          XFILE                                        
*                                                                               
         BAS   RE,DOPFK            TEST PFK INPUT                               
*                                                                               
         CLI   MYSCREEN,X'5A'      TEST PTTN/COMMENT                            
         BE    DR60                                                             
*                                                                               
         CLI   NPTXNET-NPTXKEY+SVXNKEY,C'$'   TEST NTWK LIST                    
         BNE   *+8                                                              
         BRAS  RE,DNETS                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                 HAS TO BE DATA ELEM                          
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
         XC    WORK,WORK                                                        
         TM    NPTSTAT,X'80'       SOFT DELETE                                  
         BZ    *+10                                                             
         MVC   WORK(9),=C'*DELETED*'                                            
*                                                                               
         CLC   TRADEL,WORK                                                      
         BE    *+14                                                             
         MVC   TRADEL,WORK                                                      
         OI    TRADELH+6,X'80'                                                  
*                                                                               
         MVC   TRADESC(L'NPTDESC),NPTDESC                                       
         OI    TRADESCH+6,X'80'                                                 
*                                                                               
         LA    R2,NPTSTART                                                      
         BRAS  RE,PPER                                                          
         MVC   TRAPER,WORK                                                      
         OI    TRAPERH+6,X'80'                                                  
*                                                                               
         XC    DUB,DUB                                                          
         TM    NPTSTAT,NPTS_TIME   TEST TIMES IN ELEMENT                        
         BZ    *+10                                                             
         MVC   DUB(4),NPTSTIM                                                   
*                                                                               
         LA    R2,TRASTIMH                                                      
         LA    R4,DUB                                                           
         BAS   RE,GOUNTIME                                                      
*                                                                               
         LA    R2,TRAETIMH                                                      
         LA    R4,DUB+2                                                         
         BAS   RE,GOUNTIME                                                      
         B     DR50                                                             
*                                                                               
GOUNTIME NTR1                                                                   
         XC    8(5,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         OC    0(2,R4),0(R4)       TEST NO TIME PRESENT                         
         JZ    EQXIT                                                            
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R4)                                                    
         XC    WORK,WORK                                                        
         GOTO1 UNTIME,DMCB,FULL,WORK                                            
         MVC   8(5,R2),WORK                                                     
         J     EQXIT                                                            
*                                                                               
DR50     L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         MVC   BYTE,NPTSTAT        SAVE STATUS BYTE                             
*                                                                               
         BRAS  RE,DCMMLS           DISPLAY COMMERCIALS                          
*                                                                               
         XC    TRAPTYP,TRAPTYP                                                  
         OI    TRAPTYPH+6,X'80'                                                 
         LA    R4,TRAPTYP                                                       
*                                                                               
         L     R6,AIO              SEE IF ANY COMMENTS IN RECORD                
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR52A                                                            
         MVC   0(9,R4),=C'COMMENTS!'                                            
         XC    1(7,R4),SPACES     MAKE LOWERCASE                                
         LA    R4,10(R4)                                                        
*                                                                               
DR52A    L     R6,AIO              SEE IF HIATUS IN RECORD                      
         MVI   ELCODE,X'5C'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR52B                                                            
         MVC   0(7,R4),=C'HIATUS!'                                              
         XC    1(5,R4),SPACES     MAKE LOWERCASE                                
         LA    R4,8(R4)                                                         
*                                                                               
DR52B    TM    BYTE,X'10'          BYTE HAS PATSTAT FROM ABOVE                  
         BZ    DR52C                                                            
         MVC   0(10,R4),=CL20'COPYCD=EST'                                       
         XC    1(5,R4),SPACES     MAKE LOWERCASE                                
         XC    8(2,R4),SPACES                                                   
         LA    R4,11(R4)                                                        
*                                                                               
DR52C    L     R6,AIO                                                           
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR52X                                                            
         USING ACTVD,R6                                                         
*                                                                               
         LA    R0,ACTVADDT                                                      
         OC    SVADDWHN,SVADDWHN   USE VERSION 1 ADD DATE                       
         BZ    *+8                 IF IT'S AROUND                               
         LA    R0,SVADDWHN                                                      
         MVC   0(5,R4),=C'ADDED'                                                
         XC    1(4,R4),SPACES     MAKE LOWERCASE                                
         LA    R4,6(R4)                                                         
         GOTO1 DATCON,DMCB,(3,(R0)),(8,(R4))                                    
         LA    R4,9(R4)                                                         
*                                                                               
         LA    R0,ACTVCHDT                                                      
         OC    ACTVCHDT,ACTVCHDT    IF CHANGE DATE PRESENT                      
         BNZ   DR52D                USE IT                                      
         CLC   LATEST,=H'1'         ELSE CHECK THIS IS VRSN 1                   
         BNH   DR52E                WHICH MAY NOT HAVE A CHANGE DATE            
         LA    R0,ACTVADDT          SO ADD DATE IS CHANGE DATE                  
*                                                                               
DR52D    MVC   0(7,R4),=C'CHANGED'                                              
         XC    1(6,R4),SPACES      MAKE LOWERCASE                               
         LA    R4,8(R4)                                                         
         GOTO1 DATCON,DMCB,(3,(R0)),(8,(R4))                                    
         LA    R4,9(R4)                                                         
*                                                                               
DR52E    CLI   1(R6),20            TEST ID IN ELEMENT                           
         BNH   DR52X                                                            
         MVC   0(2,R4),=C'BY'                                                   
         XI    1(R4),X'40'                                                      
         MVC   3(8,R4),ACTVSCID                                                 
DR52X    B     DR90                                                             
*                                                                               
DR60     BRAS  RE,BLDINFO          DISPLAY DATA ON COMMENT SCREEN               
         MVC   PCMINFO,WORK                                                     
         OI    PCMINFOH+6,X'80'                                                 
*                                                                               
         LA    R3,4                MAX COMMENTS LINES                           
         LA    R2,PCMCMT1H                                                      
         MVI   ELCODE,X'40'        COMMENT ELEMENT(S)                           
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DR62     BRAS  RE,NEXTEL                                                        
         BNE   DR66                                                             
         USING NPTCMTEL,R6                                                      
         LLC   R1,NPTCMTLN         GET COMMENT ELEM LEN                         
         AHI   R1,-4               GET ACTUAL COMMENT LEN-1                     
         LLC   RF,0(R2)            FLD LEN                                      
         AHI   RF,-9                                                            
         EX    RF,DRXC             CLEAR FIELD BEFORE MOVE                      
         CR    RF,R1               INSURE                                       
         BNL   DR64                NO FLDH                                      
         LR    R1,RF               CLOBBER                                      
*                                                                               
DR64     EX    R1,DRMVC                                                         
         OI    6(R2),X'80'                                                      
         LLC   R0,0(R2)            GET FLD LENGTH                               
         AR    R2,R0                                                            
         BCT   R3,DR62                                                          
*                                                                               
         BRAS  RE,NEXTEL           SEE IF 5TH COMMENT                           
         BNE   DR70                NO, OK                                       
         DC    H'0'                                                             
*                                                                               
DR66     LTR   R3,R3                                                            
         BZ    DR70                                                             
*                                                                               
DR68     LLC   RF,0(R2)                                                         
         AHI   RF,-9               GET FLD LEN -1                               
         EX    RF,DROC                                                          
         BZ    *+12                                                             
         EX    RF,DRXC                                                          
         OI    6(R2),X'80'                                                      
         LA    R2,9(RF,R2)                                                      
         BCT   R3,DR68                                                          
*                                                                               
DR70     LA    R2,PCMHIA1H         CLEAR THE FIELDS                             
*                                                                               
DR72     OC    8(PCMHIA2H-PCMHIA1H-8,R2),8(R2)                                  
         BZ    DR72X                                                            
         XC    8(PCMHIA2H-PCMHIA1H-8,R2),8(R2)                                  
         OI    6(R2),X'80'                                                      
*                                                                               
DR72X    LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,PCMHIAXH                                                      
         CR    R2,R0                                                            
         BL    DR72                                                             
*                                                                               
         LA    R2,PCMHIA1H                                                      
         MVI   ELCODE,X'5C'        HIATUS  ELEMENT(S)                           
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DR74     BRAS  RE,NEXTEL                                                        
         BNE   DR90                                                             
         USING NPTHIAEL,R6                                                      
         OI    6(R2),X'80'         SET TRANSMIT                                 
*                                                                               
DR76     CLI   NPTHIADT+1,0        TEST MONTH 0                                 
         BNE   DR78                NO - SO IT'S A DATE                          
* DISPLAY DAYS ENTRY                                                            
         GOTO1 CALLOV,DMCB,0,X'D9000A0F'  GET DAYUNPK ADDRESS                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,NPTHIADT+2,8(R2)                                       
         B     DR80                                                             
*                                                                               
DR78     GOTO1 DATCON,DMCB,(3,NPTHIADT),(5,8(R2))                               
*                                                                               
DR80     OC    NPTHIATM,NPTHIATM   TEST TIME PRESENT                            
         BZ    DR82                                                             
*                                                                               
         LA    R5,8+19(R2)         POINT TO LAST POSSIBLE CHAR                  
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),NPTHIATM,(R5)                                          
*                                                                               
DR82     LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,PCMHIAXH                                                      
         CR    R2,R0                                                            
         BNH   DR74                                                             
*                                                                               
DR90     LA    R2,TRAXXXXH                                                      
         SR    R0,R0                                                            
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO FIRST DATA FIELD                    
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    *-10                                                             
         OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
         BRAS  RE,SETPFTXT                                                      
*                                                                               
         OC    MYSUBLIN,MYSUBLIN   TEST VERSIONING ACTIVE                       
         BZ    DR92                                                             
         BRAS  RE,SETFLDS          SET PROT/UNP AS REQUIRED                     
         J     EXIT                                                             
*                                                                               
DR92     CLI   MODE,XRECADD        AFTER ADD RECORD                             
         BE    DR94                YES - DON'T CLEAR PREV DISK ADDR             
*                                                                               
         XC    SVPRVDA,SVPRVDA                                                  
         CLI   SVT3PROF,C'Y'        TEST TO COPY COMMENTS THIS CLT              
         BNE   DR94                                                             
         CLI   ACTNUM,ACTADD                                                    
         BE    DR94                MUST SAVE AFTER ADDREC                       
         MVC   KEY(13),TWAKEYSV    KEY OF CURRENT RECORD                        
         CLC   =X'0A61',KEY        MAKE SURE IT'S A PATTERN REC                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVPRVDA,KEY+36      THEN SAVE THIS DISK ADDR                     
*                                                                               
DR94     CLI   MODE,VALREC         VALIDATE RECORD                              
         JNE   EXIT                NO, JUST DISPLAY REC                         
         CLI   ACTNUM,ACTADD       ADD RECORD                                   
         JE    EXIT                 YES                                         
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         J     EXIT                                                             
DRMVC    MVC   8(0,R2),NPTCMT-NPTCMTEL(R6)                                      
DRXC     XC    8(0,R2),8(R2)                                                    
DROC     OC    8(0,R2),8(R2)                                                    
         DROP  R6                                                               
         EJECT                                                                  
*===========================================================                    
* PRDCHK - CHECK BRAND LEVEL SECURITY                                           
*===========================================================                    
                                                                                
DRSEC    NTR1                                                                   
         MVC   SVMYKEY,KEY         SAVE KEY                                     
*                                                                               
         LA    R2,ELEM             FAKE VALIPRD                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0B01000184030001'                                     
         MVC   ELEM+8(3),QPRD                                                   
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         MVC   KEY(L'SVMYKEY),SVMYKEY RESTORE KEY                               
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BE    DRSEC02              NO                                          
*                                                                               
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         J     TRAPERR                                                          
*                                                                               
DRSEC02  CLC   QPRD2,SPACES        ANY PARTNER PRODUCT                          
         BNH   DRSECX                                                           
*                                                                               
         MVC   SVMYKEY,KEY         SAVE KEY                                     
*                                                                               
         LA    R2,ELEM             FAKE VALIPRD                                 
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0B01000184030001'                                     
         MVC   ELEM+8(3),QPRD2                                                  
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         MVC   KEY(L'SVMYKEY),SVMYKEY  RESTORE KEY                              
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BE    DRSECX               NO                                          
*                                                                               
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         J     TRAPERR                                                          
*                                                                               
DRSECX   J     EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* PROCESS PFKEYS                                                                
*                                                                               
* CURRENT SCREEN   PFKEY    NEW SCREEN                                          
*                                                                               
* PATTERN (81)       2       5A COMMENTS                                        
*                  5/6/7     NEXT/PREV/CURRENT  <<<NOP>>>                       
*                                                                               
* PTTN/COMMENT (5A)  2       81 PATTERN                                         
*=============================================================                  
                                                                                
* ENTRIES ARE RECORD (P), FROM SCREEN, PFKEY, TO SCREEN                         
*                                                                               
PFKEYTAB DC    X'D781025A'         PATT TO PATT/COMM                            
         DC    X'D75A0281'         PATT/COMM TO PATT                            
PFKEYTBX DC    X'FF'                                                            
                                                                                
DOPFK    NTR1                                                                   
         OI    CONSERVH+1,X'01'    FORCE MODIFIED FOR PFKEYS                    
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         CLI   PFKEY,0                                                          
         BE    DOPFK20                                                          
*                                                                               
DOPFK2   CLI   ACTNUM,ACTDIS                                                    
         BE    DOPFK4                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BE    DOPFK4                                                           
         OC    MYSUBLIN,MYSUBLIN   IS CURRENT VERSION ACTIVE                    
         BZ    DOPFK10             YES                                          
         XC    MYSUBLIN,MYSUBLIN   SET TO GET CURRENT VERSION                   
         B     DOPFK10                                                          
         CLI   ACTNUM,ACTSEL                                                    
         BE    DOPFK4                                                           
         OC    MYSUBLIN,MYSUBLIN   IS CURRENT VERSION ACTIVE                    
         BZ    DOPFK10             YES                                          
         XC    MYSUBLIN,MYSUBLIN   SET TO GET CURRENT VERSION                   
         B     DOPFK6                                                           
                                                                                
DOPFK4   CLI   PFKEY,6             TEST PFKEY FOR PREV VERSION                  
         BE    DOPFK6                                                           
         CLI   PFKEY,7             OR CURR VERSION                              
         BE    DOPFK6                                                           
         CLI   PFKEY,5             OR FOR NEXT VERSION                          
         BNE   DOPFK10                                                          
*                                                                               
DOPFK6   BRAS  RE,GETVRSN                                                       
         B     DOPFK20                                                          
*                                                                               
DOPFK10  LA    R4,PFKEYTAB         TABLE OF PFKEYS                              
*                                                                               
DOPFK12  CLC   CONREC(1),0(R4)     MATCH RECORD TYPE (P OR B)                   
         BNE   DOPFK14                                                          
         CLC   MYSCREEN,1(R4)      MATCH CURRENT SCREEN                         
         BNE   DOPFK14                                                          
         CLC   PFKEY,2(R4)         MATCH PFKEY                                  
         BE    DOPFK16                                                          
*                                                                               
DOPFK14  LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DOPFK12                                                          
         MVI   PFKEY,0             PFKEY IS INVALID - IGNORE IT                 
         B     DOPFK20                                                          
*                                                                               
DOPFK16  MVC   MYSCREEN,3(R4)      SET NEW SCREEN                               
*                                                                               
         BRAS  RE,GETSCR                                                        
         MVI   GENPFOPT,C'Y'       SET TO STAY ON THIS SELECTION                
*                                                                               
         OC    MYSUBLIN,MYSUBLIN   TEST CURRENT VERSION                         
         BZ    DOPFK20                                                          
         IC    R0,PFKEY            SET PFKEY=0 TO FIX SCRN                      
         MVI   PFKEY,0                                                          
         BRAS  RE,GETVRSN          WITH PFKEY=0, JUST PROT/UNP FIELDS           
         STC   R0,PFKEY            AND RESTORE PFKEY VALUE                      
*                                                                               
DOPFK20  LA    R2,CONHEADH                                                      
         L     R6,AIO                                                           
*                                                                               
         CLI   MODE,PROCPFK        MAY NEED TO GET RECORD                       
         BNE   DOPFK24                                                          
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
DOPFK24  L     R6,AIO                                                           
         MVI   ELCODE,X'10'        FIND STATUS ELEM                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ADIDFLAG,C'Y'                                                    
         USING NPTDTAEL,R6                                                      
         TM    NPTSTAT,NPTS_ADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
         DROP  R6                                                               
*                                                                               
DOPFKX   J     EXIT                                                             
*                                                                               
         EJECT                                                                  
GETSCR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,BLOCK                                                         
         LHI   R1,TRAXXXXH-TRAKEYH                                              
         LA    RE,TRAKEYH                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE KEY FIELDS                              
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D90216'                                             
         MVC   DMCB+7(1),MYSCREEN                                               
*                                                                               
         GOTO1 CALLOV,DMCB,TRAKEYH                                              
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,BLOCK                                                         
         LHI   R1,TRAXXXXH-TRAKEYH                                              
         LA    RE,TRAKEYH                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE KEY FIELDS                           
*                                                                               
         LA    R1,TRAXXXXH         FIND EOS                                     
         SR    R0,R0                                                            
*                                                                               
GETSCR10 IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   GETSCR10                                                         
         MVC   0(3,R1),=X'000101'  ERASE BEFORE/AFTER                           
*                                                                               
         OI    CONSERVH+1,X'01'    FORCE MODIFIED FOR PFKEYS                    
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* READ PREVIOUS/NEXT VERSION OF THIS PATTERN                                    
* CURRENT 0, PREVIOUS IS 1, ... WORK IN POSITIVE NUMBERS                        
*==================================================================             
                                                                                
GETVRSN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PFKEY,0             TEST REQUEST TO CLEAR VRSN SCRN              
         BE    GETVRS20                                                         
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+8                                                              
         MVI   GENPFOPT,C'Y'       IF LIST SELECT, STAY ON THIS REC             
*                                                                               
         LA    R1,CONHEADH                                                      
         SR    R0,R0                                                            
*                                                                               
GETVRS2  IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0             FIND E-O-S                                   
         BNE   GETVRS2                                                          
*                                  NOW SAVE OLD SCREEN IN TIA                   
         L     R0,ATWA                                                          
         SR    R1,R0               GIVES LEN TO EOS                             
*                                  NOW SAVE OLD SCREEN IN TIA                   
         L     RE,ATIA                                                          
         LA    RF,1(R1)                                                         
         MVCL  RE,R0                                                            
*                                                                               
         CLI   PFKEY,7             TEST 'LATEST' REQUEST                        
         BNE   GETVRS4                                                          
         SR    R0,R0                                                            
         B     GETVRS10                                                         
*                                                                               
GETVRS4  CLI   PFKEY,6             TEST GO BACK 1                               
         BNE   GETVRS6                                                          
         LH    R0,MYSUBLIN                                                      
         AHI   R0,1                                                             
         B     GETVRS10                                                         
*                                                                               
GETVRS6  CLI   PFKEY,5             TEST GO FORWARD 1                            
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    MYSUBLIN,MYSUBLIN   TEST AT CURRENT                              
         BZ    GETVRSE2                                                         
         LH    R0,MYSUBLIN                                                      
         AHI   R0,-1                                                            
                                                                                
*=================================================================              
* BUILD A LIST OF ALL THE VERSIONS IN IOAREA3                                   
* CURRENT IS 0, PREVIOUS IS 1                                                   
*=================================================================              
                                                                                
GETVRS10 STH   R0,MYSUBLIN         SAVE REQUESTED REL VRSN                      
*                                                                               
         L     RE,AIO3             USE AS BUILD AREA                            
         LA    RF,2000                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R4,AIO3                                                          
         SR    R5,R5               CLEAR COUNTER                                
         XC    KEY,KEY                                                          
         MVC   KEY(23),TWAKEYSV    DO NOT MOVE REFNUM                           
         GOTO1 HIGH                                                             
*                                                                               
GETVRS12 CLC   KEY(23),KEYSAVE     TEST SAME UP TO REFNUM                       
         BNE   GETVRS16                                                         
*                                                                               
         LA    RE,KEY+NPTXR3F-NPTXKEY POINT TO REFNUM                           
         CLI   0(RE),X'A0'         REAL REFNUMS ARE > XA00000                   
         BH    GETVRS13            GO SEE IF THIS IS US                         
         LA    RE,KEY+NPTXOR3G-NPTXKEY POINT TO OLD REFNUM                      
         OC    0(3,RE),0(RE)           IF NONE, SKIP RECORD                     
         BZ    GETVRS14                                                         
*                                                                               
GETVRS13 CLC   0(3,RE),BREFK+1         TEST RIGHT REFNUM                        
         BNE   GETVRS14                                                         
*                                                                               
         MVC   0(4,R4),KEY+36      SAVE DISK ADDRESS                            
         LA    R4,4(R4)                                                         
         LA    R5,1(R5)            BUMP COUNTER                                 
*                                                                               
GETVRS14 GOTO1 SEQ                                                              
         B     GETVRS12                                                         
*                                                                               
GETVRS16 LH    R0,MYSUBLIN         GET REQUESTED REFNUM                         
         AHI   R0,1                FOR ARITHMETIC                               
         CR    R0,R5               ARE THERE THAT MANY VERSIONS                 
         BH    GETVRSE1            NO - ERROR                                   
                                                                                
* NOW POINT TO CORRECT DISK ADDRESS MOVING BACKWARDS FROM 0(R4)                 
                                                                                
GETVRS18 AHI   R4,-4               BACK UP TO PREVIOUS ENTRY                    
         BCT   R0,GETVRS18                                                      
         MVC   KEY+36(4),0(R4)     MOVE DISK ADDRESS FOR GETREC                 
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   KEY,0(R6)           SET CURRENT KEY FROM RECORD                  
                                                                                
*===================================================================            
* NOW FIX THE SCREEN TO PROTECT ALL FIELDS IF NOT CURRENT VERSION               
*===================================================================            
                                                                                
GETVRS20 LA    R4,TRA81TAB                                                      
         CLI   MYSCREEN,X'81'                                                   
         BE    GETVRS22                                                         
*                                                                               
         LA    R4,TRA5ATAB                                                      
         CLI   MYSCREEN,X'5A'                                                   
         BE    GETVRS22                                                         
         J     EXIT                                                             
*                                                                               
GETVRS22 LA    RE,T216FFD          START OF TWA                                 
         AH    RE,0(R4)            POINT TO THE FLDHDR                          
*                                                                               
         NI    1(RE),X'FF'-X'2C'   SET TO UNP/NORMAL                            
         OC    MYSUBLIN,MYSUBLIN   TEST CURRENT VERSION                         
         BZ    GETVRS26                                                         
         OI    1(RE),X'20'         SET PROTECTED                                
*                                                                               
GETVRS26 LA    R4,2(R4)                                                         
         CLC   0(2,R4),=X'FFFF'                                                 
         BNE   GETVRS22                                                         
*                                                                               
GETVRS30 NI    CONACTH+1,X'FF'-X'20'  UNPROTECT ACTION FIELD                    
         OC    MYSUBLIN,MYSUBLIN      TEST CURRENT VERSION                      
         BZ    *+12                                                             
         OI    CONACTH+1,X'20'        PROTECT ACTION FIELD                      
         OI    CONRECH+4,X'20'        SET FLAG SO KNOW IF REC CHANGES           
*                                                                               
         SR    R0,R0                                                            
GETVRS32 IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             FIND EOS                                     
         BNE   GETVRS32                                                         
         MVC   0(3,RE),=X'000101'  FORCE XMT ALL                                
*                                                                               
         BRAS  RE,SETPFTXT                                                      
         J     EXIT                                                             
*                                                                               
GETVRSE1 MVC   GERROR,=Y(NOOLDER)                                               
         B     GETVRSEX                                                         
*                                                                               
GETVRSE2 MVC   GERROR,=Y(NONEWER)                                               
*                                                                               
GETVRSEX LA    R2,TRACODEH                                                      
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
*==============================================================                 
* SET PFKEY LINE AS REQUIRED                                                    
*==============================================================                 
                                                                                
SETPFTXT NTR1  BASE=*,LABEL=*                                                   
         NC    PF2PTN+3(6),=7X'BF' MAKE LOWERCASE AS NEEDED                     
         NC    PFKCMT+3(7),=7X'BF' MAKE LOWERCASE AS NEEDED                     
         NC    PFKCMT+12(5),=7X'BF'                                             
         NC    PFKVRSN+3(9),=9X'BF'                                             
         NC    PFKVRSN+16(9),=9X'BF'                                            
         NC    PFKVRSN+29(6),=9X'BF'                                            
*                                                                               
         CLI   MYSCREEN,X'81'      TEST PATTERN BASE                            
         BNE   SETPF2                                                           
         XC    TRAPFK,TRAPFK                                                    
         LA    R2,TRAPFKH                                                       
         LA    R4,8(R2)                                                         
         MVC   0(2,R4),=C'PF'                                                   
         LA    R4,2(R4)                                                         
*                                                                               
         MVC   0(L'PFKCMT,R4),PFKCMT    2=COMMENTS                              
         LA    R4,L'PFKCMT+1(R4)                                                
         B     SETPF10                                                          
*                                                                               
SETPF2   CLI   MYSCREEN,X'5A'      TEST PATTERN COMMENT                         
         BNE   SETPF10                                                          
         XC    PCMPFK,PCMPFK                                                    
         LA    R2,PCMPFKH                                                       
         LA    R4,8(R2)                                                         
         MVC   0(2,R4),=C'PF'                                                   
         LA    R4,2(R4)                                                         
         MVC   0(L'PF2PTN,R4),PF2PTN    2=PATTERN                               
         LA    R4,L'PF2PTN+1(R4)                                                
*                                                                               
SETPF10  OI    6(R2),X'80'         SET FIELD TO XMT                             
*                                                                               
         CLC   LATEST,=H'1'        TEST ONLY ONE VERSION                        
         BNH   SETPF12                                                          
*                                                                               
         CLI   ACTNUM,ACTDIS                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   SETPF12                                                          
         MVC   0(L'PFKVRSN,R4),PFKVRSN                                          
         LA    R4,L'PFKVRSN+1(R4)                                               
*                                                                               
SETPF12  DS    0H                                                               
SETPFX   J     EQXIT                                                            
*                                                                               
PFKCMT   DC    C'2=COMMENTS/HIATUS'                                             
PF2PTN   DC    C'2=PATTERN'                                                     
PFKVRSN  DC    C'5=NEWER VRSN 6=OLDER VRSN 7=CURRENT'                           
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* WHEN VERSIONING ACTIVE, SET CHANGED FIELDS TO HIGH INTENSITY                  
*================================================================               
                                                                                
SETFLDS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R4,TRA81TAB                                                      
         CLI   MYSCREEN,X'81'                                                   
         BE    SETFLD2                                                          
*                                                                               
         LA    R4,TRA5ATAB                                                      
*                                                                               
SETFLD2  LA    RE,T216FFD          START OF TWA                                 
         AH    RE,0(R4)            POINT TO THE FLDHDR                          
*                                                                               
         L     RF,ATIA             POINT TO SAVED TWA                           
         AH    RF,0(R4)            POINT TO FLDHDR                              
*                                                                               
         SR    R1,R1                                                            
         IC    R1,0(RE)            GET FIELD LEN                                
         AHI   R1,-9               DECREMENT FOR FLDHDR + EX                    
         TM    1(RE),X'02'         TEST FOR EXTENDED FLDHDR                     
         BZ    *+8                                                              
         AHI   R1,-8                                                            
*                                                                               
         EX    R1,OC8RE                                                         
         EX    R1,OC8RF                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,RE),8(RF)       TEST FIELD CHANGED                           
         BE    SETFLD10                                                         
*                                                                               
         OI    1(RE),X'08'         SET HIGH INTENSITY                           
         B     SETFLD10                                                         
*                                                                               
OC8RE    OC    8(0,RE),SPACES                                                   
OC8RF    OC    8(0,RF),SPACES                                                   
*                                                                               
SETFLD10 LA    R4,2(R4)                                                         
         CLC   0(2,R4),=X'FFFF'                                                 
         BNE   SETFLD2                                                          
                                                                                
*====================================================                           
* MYSUBLIN IS 0 FOR CURRENT VERSION                                             
* LATEST IS NUMBER OF VRSNS                                                     
* BACK ONE VERSION  IS 1 WHICH SHOWS AS -1                                      
* BACK TWO VERSIONS IS 2 WHICH SHOWS AS -2                                      
*====================================================                           
                                                                                
         LA    R4,CONHEAD                                                       
         MVC   0(17,R4),=C'CURRENT VERSION -'                                   
         LA    R4,17(R4)                                                        
         LH    R0,MYSUBLIN                                                      
         EDIT  (R0),(3,(R4)),0,ALIGN=LEFT                                       
         AR    R4,R0                                                            
         MVC   0(4,R4),=C' OF '                                                 
         LA    R4,4(R4)                                                         
         LH    R0,LATEST                                                        
         BCTR  R0,R0                                                            
         EDIT  (R0),(3,(R4)),0,ALIGN=LEFT                                       
         AR    R4,R0                                                            
         MVC   0(10,R4),=C' DISPLAYED'                                          
         OI    GENSTAT2,USMYOK     TELL GENCON I DID MESSAGE                    
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         DS    0H                                                               
TRA81TAB DC    AL2(TRANET1H-T216FFD)                                            
         DC    AL2(TRANET2H-T216FFD)                                            
         DC    AL2(TRADESCH-T216FFD)                                            
         DC    AL2(TRAPERH-T216FFD)                                             
         DC    AL2(TRASTIMH-T216FFD)                                            
         DC    AL2(TRAETIMH-T216FFD)                                            
         DC    AL2(TRACMLAH-T216FFD)                                            
         DC    AL2(TRAPCTAH-T216FFD)                                            
         DC    AL2(TRACMLBH-T216FFD)                                            
         DC    AL2(TRAPCTBH-T216FFD)                                            
         DC    AL2(TRACMLCH-T216FFD)                                            
         DC    AL2(TRAPCTCH-T216FFD)                                            
         DC    AL2(TRACMLDH-T216FFD)                                            
         DC    AL2(TRAPCTDH-T216FFD)                                            
         DC    AL2(TRACMLEH-T216FFD)                                            
         DC    AL2(TRAPCTEH-T216FFD)                                            
         DC    AL2(TRACMLFH-T216FFD)                                            
         DC    AL2(TRAPCTFH-T216FFD)                                            
         DC    AL2(TRACMLGH-T216FFD)                                            
         DC    AL2(TRAPCTGH-T216FFD)                                            
         DC    AL2(TRACMLHH-T216FFD)                                            
         DC    AL2(TRAPCTHH-T216FFD)                                            
         DC    AL2(TRACMLIH-T216FFD)                                            
         DC    AL2(TRAPCTIH-T216FFD)                                            
         DC    AL2(TRACMLJH-T216FFD)                                            
         DC    AL2(TRAPCTJH-T216FFD)                                            
         DC    AL2(TRACMLKH-T216FFD)                                            
         DC    AL2(TRAPCTKH-T216FFD)                                            
         DC    AL2(TRACMLLH-T216FFD)                                            
         DC    AL2(TRAPCTLH-T216FFD)                                            
         DC    AL2(TRAROTH-T216FFD)                                             
         DC    X'FFFF'                                                          
*                                                                               
TRA5ATAB DC    AL2(PCMCMT1H-T216FFD)                                            
         DC    AL2(PCMCMT2H-T216FFD)                                            
         DC    AL2(PCMCMT3H-T216FFD)                                            
         DC    AL2(PCMCMT4H-T216FFD)                                            
         DC    X'FFFF'                                                          
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* VALIDATE ENTRIES IN NETWORK LIST                                              
*=============================================================                  
                                                                                
VALNLIST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ASVNET                                                        
         XC    0(SVNETLSX-SVNETLST,R6),0(R6)                                    
*                                                                               
         LA    R2,TRANET1H         POINT TO FIRST FLDHDR                        
         CLI   5(R2),0                                                          
         BE    VNL08                                                            
*                                                                               
VNL01    L     RE,AIO3             CLEAR THE SCANNER BLOCK                      
         LA    RF,1024                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(20,AIO3)                                      
         LLC   R0,DMCB+4           GET NUMBER OF BLOCKS                         
         L     R5,AIO3             ADDRESS OF FIRST ENTRY                       
*                                                                               
VNL02    CLI   12(R5),C'*'         TEST DELETED                                 
         BNE   VNL04               NO                                           
         CLI   ACTNUM,ACTADD       CANNOT ADD DELETED NETWORK                   
         BE    VNLERR3                                                          
         B     VNL06                                                            
*                                                                               
VNL04    CLI   0(R5),3                                                          
         BL    VNLERR1                                                          
         CLI   0(R5),4                                                          
         BH    VNLERR1                                                          
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),12(R5)                                               
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO2                     
         CLI   8(R1),0                                                          
         BNE   VNLERR2                                                          
*                                                                               
         MVC   0(4,R6),12(R5)      SAVE NETWORK                                 
         LA    R6,4(R6)                                                         
*                                                                               
VNL06    LA    R5,32(R5)           NEXT SCAN FIELD                              
         CLI   0(R5),0             TEST MORE DATA                               
         BNE   VNL02                                                            
*                                                                               
VNL08    LA    R0,TRANET2H                                                      
         CR    R2,R0               DONE BOTH FIELDS?                            
         BE    VNL10                                                            
         LA    R2,TRANET2H                                                      
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VNL01                                                            
*                                                                               
VNL10    L     R1,ASVNET                                                        
         SR    R0,R0                                                            
*                                                                               
VNL12    CLI   0(R1),0             COUNT LIST ITEMS                             
         BE    VNL14                                                            
         LA    R1,4(R1)                                                         
         BCT   R0,VNL12                                                         
*                                                                               
VNL14    LPR   R0,R0                                                            
         BZ    VNLERR4                                                          
         CHI   R0,32               LIMIT FOR NOW IS 32 ACTIVE NETS              
         BH    VNLERR6                                                          
         GOTO1 XSORT,DMCB,ASVNET,(R0),4,4,0                                     
                                                                                
* CHECK FOR DUPLICATES                                                          
                                                                                
         L     RE,ASVNET                                                        
*                                                                               
VNL16    CLC   0(4,RE),4(RE)                                                    
         BE    VNLERR5                                                          
         LA    RE,4(RE)                                                         
         BCT   R0,VNL16                                                         
                                                                                
* NOW SEE IF THIS IS SAME AS LIST IN RECORD                                     
                                                                                
VNL20    L     R5,ASVOLD           BUILD LIST FROM NETWORK ELEMENTS             
         XC    0(SVOLDLSX-SVOLDLST,R5),0(R5)                                    
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VNL32                                                            
*                                                                               
VNL30    TM    6(R6),X'80'         TEST NETWORK DELETED                         
         BO    *+14                                                             
         MVC   0(4,R5),2(R6)       MOVE NETWORK TO LIST                         
         LA    R5,4(R5)                                                         
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    VNL30                                                            
*                                                                               
VNL32    MVI   SVNETNEW,C'N'       SET NOT NEW                                  
         L     RE,ASVNET                                                        
         L     RF,ASVOLD                                                        
         CLC   0(SVNETLSX-SVNETLST,RE),0(RF)   TEST LISTS SAME                  
         BE    VNLX                                                             
         MVI   SVNETNEW,C'Y'                                                    
*                                                                               
VNLX     J     EQXIT                                                            
*                                                                               
VNLERR1  MVC   GERROR,=Y(BADNLST)                                               
         B     VNLERRX                                                          
*                                                                               
VNLERR2  MVC   GERROR,=Y(NONLST)                                                
         B     VNLERRX                                                          
*                                                                               
VNLERR3  MVC   GERROR,=Y(NODELNET)                                              
         B     VNLERRX2                                                         
*                                                                               
VNLERR4  MVC   GERROR,=Y(NEED1NET)                                              
         LA    R2,TRANET1H                                                      
         B     VNLERRX2                                                         
*                                                                               
VNLERR5  MVC   GERROR,=Y(DUPNET)                                                
         LA    R2,TRANET1H                                                      
         LR    R5,RE                                                            
         XC    4(8,R5),4(R5)       CLEAR END OF ENTRY                           
         AHI   R5,-12              POINT TO DUP -12 FOR SUBST                   
         B     VNLERRX                                                          
*                                                                               
VNLERR6  MVC   GERROR,=Y(MSMAXLST)                                              
         LA    R2,TRANET1H                                                      
         B     VNLERRX2                                                         
*                                                                               
VNLERRX  XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         STCM  R3,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R3),7             L'SUBST TEXT + 1                             
         MVC   1(6,R3),12(R5)                                                   
*                                                                               
VNLERRX2 GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* UPDATE NETWORK ELEMENTS IN PATTERN RECORD                                     
* SVNETLST HAS NEW NETWORK LIST                                                 
* SVOLDLST HAS OLD NETWORK LIST. IF NETWORK IS IN OLD LIST BUT                  
* NOT IN NEW LIST, FLAG IT AS DELETED IN THE ELEMENT                            
* IF IT'S IN THE NEW LIST BUT NOT IN THE RECORD, ADD NEW POINTER                
* IF IT'S IN THE NEW LIST AND FLAGGED DELETED, UNSET DELETE BIT                 
*=============================================================                  
                                                                                
UPDNETS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ELCODE,X'5B'                                                     
         L     R6,AIO1                                                          
         SR    R5,R5                                                            
         BRAS  RE,GETEL            COUNT 5B ELEMENTS                            
         B     *+8                                                              
*                                                                               
UPDN1    BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BCT   R5,UPDN1                                                         
         LPR   R5,R5                                                            
         LA    R0,(SVNETLSX-SVNETLST)/4                                         
         CR    R5,R0                                                            
         BH    UPDERR1                                                          
*                                                                               
         L     R4,ASVNET                                                        
         CLI   0(R4),0                                                          
         BE    UPDN20                                                           
*                                                                               
UPDN2    L     R6,AIO1             FIND ELEMENT IN RECORD                       
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         BNE   UPDN8               GO ADD NETWORK ELEM                          
*                                                                               
UPDN4    CLC   0(4,R4),2(R6)       SAME NETWORK                                 
         BE    UPDN6                                                            
         BL    UPDN8               IF NET IN REC IS HIGH, ADD NOW               
         BRAS  RE,NEXTEL                                                        
         BNE   UPDN8                                                            
         B     UPDN4                                                            
*                                                                               
UPDN6    NI    6(R6),X'7F'         MARK UNDELETED                               
         B     UPDN10                                                           
*                                                                               
UPDN8    XC    ELEM,ELEM                                                        
         MVI   ELEM,X'5B'                                                       
         MVI   ELEM+1,7                                                         
         MVC   ELEM+2(4),0(R4)                                                  
         MVI   ELEM+6,0                                                         
         GOTO1 ADDELEM                                                          
                                                                                
* ADD A DIRECTORY POINTER TO THIS NEW NETWORK                                   
                                                                                
*                                                                               
         CLI   ACTNUM,ACTADD       IF REAL ADDREC, ADD PTRS IN XAAREC           
         BE    UPDN10                                                           
         MVC   KEY,SVXNKEY                  COPY KEY                            
         MVC   NPTXNET-NPTXKEY+KEY(4),0(R4) MOVE NETWORK TO KEY                 
         MVI   NPTXPSSV-NPTXKEY+KEY,C'$'    SET PASSIVE FLAG                    
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'XSPDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDN10   LA    R4,4(R4)            NEXT NETWORK                                 
         CLI   0(R4),0                                                          
         BNE   UPDN2                                                            
                                                                                
* NOW NEED TO FLAG DISAPPEARED NETWORKS DELETED                                 
* IF ENTRY IS IN OLD LIST BUT NOT IN NEW, FLAG IT DELETED                       
                                                                                
UPDN20   L     R4,ASVOLD                                                        
*                                                                               
UPDN22   CLI   0(R4),0                                                          
         BE    UPDNX                                                            
*                                                                               
         L     RE,ASVNET                                                        
         LA    RF,(SVNETLSX-SVNETLST)/4                                         
*                                                                               
UPDN24   CLI   0(RE),0                                                          
         BE    UPDN26                                                           
         CLC   0(4,R4),0(RE)       ENTRIES MATCH                                
         BE    UPDN32                                                           
         LA    RE,4(RE)                                                         
         BCT   RF,UPDN24                                                        
                                                                                
* DOESN'T MATCH - SO NOT IN NEW LIST - FLAG DELETED                             
                                                                                
UPDN26   L     R6,AIO1                                                          
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDN28   CLC   0(4,R4),2(R6)                                                    
         BE    UPDN30                                                           
         BRAS  RE,NEXTEL                                                        
         BE    UPDN28                                                           
         DC    H'0'                                                             
*                                                                               
UPDN30   OI    6(R6),X'80'         FLAG NETWORK DELETED                         
*                                                                               
UPDN32   LA    R4,4(R4)            NEXT NET IN OLD LIST                         
         B     UPDN22                                                           
*                                                                               
UPDNX    J     EQXIT                                                            
*                                                                               
UPDERR1  MVC   GERROR,=Y(MSMAXLST)                                              
         LA    R2,TRANET1H                                                      
         GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
BLDINFO  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(24),NPTDESC                                                 
*                                                                               
         LA    R4,WORK+26                                                       
         GOTO1 DATCON,DMCB,(3,NPTSTART),(8,0(R4))                               
         MVC   8(4,R4),=C'-UFN'                                                 
         CLC   NPTEND,=X'FFFFFF'                                                
         BE    BLDINF2                                                          
         GOTO1 (RF),(R1),(3,NPTEND),(8,9(R4))                                   
*                                                                               
BLDINF2  LA    R4,WORK+46                                                       
         TM    NPTSTAT,NPTS_TIME   TEST TIME IN ELEMENT                         
         JZ    EXIT                                                             
         OC    NPTSTIM(4),NPTSTIM                                               
         JZ    EXIT                                                             
         GOTO1 UNTIME,DMCB,NPTSTIM,0(R4)                                        
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* HEADING ROUTINE FOR OFF-LINE REPORTS                                          
*=============================================================                  
                                                                                
HDHK     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         MVC   H3+50(10),=CL10'DATE ORDER'                                      
         TM    FLAGFTR,X'20'                                                    
         BO    HDHK10                                                           
         MVC   H3+50(4),=CL4' REF'                                              
*                                                                               
HDHK10   MVC   H2+8(L'QMED),QMED                                                
         MVC   H2+13(L'MEDNM),MEDNM                                             
         MVC   H3+8(L'QCLT),QCLT                                                
         MVC   H3+13(L'CLTNM),CLTNM                                             
         TM    FLAGFTR,FLREPORT       ERROR REPORT                              
         BZ    *+10                                                             
         MVC   H4+49(12),=C'ERROR REPORT'                                       
*                                                                               
         CLC   CONWHEN(7),=C'DDS,T/A' IS THIS A TURNAROUND REQ                  
         BNE   HDHKX                  NO                                        
         MVC   H4+49(12),=C'TURN-AROUND '                                       
         TM    FLAGFTR,FLREPORT       ERROR REPORT                              
         BZ    HDHKX                                                            
         MVC   H4+41(28),=C'**TURN-AROUND ERROR REPORT**'                       
*                                                                               
HDHKX    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,1,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,43,C'P A T T E R N   L I S T'                                 
         SSPEC H2,43,C'-----------------------'                                 
         SSPEC H1,71,AGYNAME                                                    
         SSPEC H2,71,AGYADD                                                     
         SSPEC H2,1,C'MEDIA'                                                    
         SSPEC H3,1,C'CLIENT'                                                   
         SSPEC H4,71,REPORT                                                     
         SSPEC H4,83,RUN                                                        
         SSPEC H5,71,REQUESTOR                                                  
         SSPEC H5,101,PAGE                                                      
         SSPEC H8,1,C'PRD-LEN'                                                  
         SSPEC H8,10,C'DPT'                                                     
         SSPEC H8,14,C'FEED'                                                    
         SSPEC H8,20,C'NET'                                                     
         SSPEC H8,26,C'PROGRAM'                                                 
         SSPEC H8,35,C'REF'                                                     
         SSPEC H8,42,C'DATA'                                                    
*                                                                               
         SSPEC H9,1,C'PTR-LEN'                                                  
         SSPEC H9,10,C'PERIOD'                                                  
         SSPEC H9,42,C'--------------------------------------'                  
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
*=================================================================              
* DOWNLOAD PATTERN DATA                                                         
*=================================================================              
                                                                                
D        USING DLCBD,DLCB                                                       
                                                                                
PRDOWN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,KEY                                                           
         USING NPTRECD,R6                                                       
*                                                                               
         CLI   KEY,X'FF'           TEST RUNLAST CALL                            
         BE    PRDOWNX                                                          
*                                                                               
         OC    SORTCNT,SORTCNT     TEST FIRST TIME                              
         BNZ   PRD10                                                            
*                                                                               
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         XC    MIDHOOK,MIDHOOK                                                  
         XC    FOOTHOOK,FOOTHOOK                                                
*                                                                               
         CLC   =C'NOW',CONWHEN                                                  
         BNE   PRD05                                                            
         MVC   P(20),=C'PATTERN DOWNLOADABLE'                                   
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,99                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRD05    XC    D.DLCBD(DLCBL),D.DLCBD                                           
         MVI   D.DLCBACT,DLCBINIT         DOWNLOAD ACTION IS START              
         LA    RF,DWNHOOK                 DUMMY HOOK                            
         ST    RF,D.DLCBAPR                                                     
         LA    RF,P                                                             
         ST    RF,D.DLCBAPL                                                     
         MVI   D.DLCXMAXL+1,L'P                                                 
         MVI   D.DLCXDELC,C' '            DELIMITER                             
         MVI   D.DLCXEOTC,C'"'            TEXT DELIMITER                        
         MVI   D.DLCXEOLC,X'5E'           SEMI-COLON, END-OF-LINE               
         MVI   D.DLCXEORC,C':'            END-OF-REPORT                         
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
         MVC   D.DLCBFLD,SPACES                                                 
         MVI   D.DLCBFLX,C' '                                                   
         MVC   D.DLCBFLX+1(L'DLCBFLX-1),D.DLCBFLX                               
         MVC   P,SPACES                                                         
*                                                                               
         BRAS  RE,BLDHDR                                                        
         MVC   D.DLCBFLX(PDHDRX-PDHDR),ELEM                                     
         LA    R0,1                                                             
         ST    R0,SORTCNT          SET FLAG THERE IS DATA                       
*                                                                               
         MVI   D.DLCBACT,DLCBPUT     ACTION IS PUT                              
         MVI   D.DLCBTYP,DLCBTXT     TYPE IS TEXT                               
         OI    D.DLCBFLG1,DLCBFXFL   USE EXTENDED FOR TEXT                      
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
         MVI   D.DLCBACT,DLCBEOL          END OF LINE                           
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
PRD10    MVC   D.DLCBFLD,SPACES           MUST CLEAR FIRST TIME                 
         MVI   D.DLCBFLX,C' '                                                   
         MVC   D.DLCBFLX+1(L'DLCBFLX-1),D.DLCBFLX                               
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
*                                                                               
         L     R0,AIO3                                                          
         LHI   R1,PDDATAX-PDDATA                                                
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24               CLEAR TO SPACES                              
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIO3             POINT TO BUFFER                              
         USING PDDATA,R4                                                        
*                                                                               
         MVC   PDITM,=CL4'ITM1'                                                 
         MVI   PDMED,C'N'                     MEDIA                             
         MVC   PDNET,NPTXNET                  NETWORK                           
*                                                                               
         CLI   NPTXNET+2,X'FF'                                                  
         BNE   PRD12                                                            
         MVC   PDNET(2),=C'M='                                                  
         MVC   PDNET+2(1),NPTXNET+1                                             
*                                                                               
PRD12    GOTO1 CLUNPK,DMCB,NPTXCLT,PDCLI      CLIENT                            
*                                                                               
         MVC   PDPROG,NPTXPROG                                                  
         CLI   PDPROG,X'FF'                                                     
         BNE   PRD14                                                            
         MVC   PDPROG,SPACES                                                    
         MVC   PDCODE,NPTXPROG+1                                                
         MVC   PDFEED,NPTXPROG+2                                                
*                                                                               
PRD14    SR    R0,R0                                                            
         ICM   R0,7,NPTXR3F                                                     
         X     R0,=X'00FFFFFF'                                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PDREF,DUB                                                        
*                                                                               
         MVC   PDPRD(3),NPTXPRD                                                 
         LLC   R0,NPTXSLN                                                       
         LA    R1,PDSLN                                                         
         BAS   RE,GETSLN                                                        
*                                                                               
         CLI   NPTXPRD2,0                                                       
         BE    PRD20                                                            
*                                                                               
         MVC   PDPTR(3),NPTXPRD2                                                
         LLC   R0,NPTXSLN2                                                      
         LA    R1,PDSLN2                                                        
         BAS   RE,GETSLN                                                        
         B     PRD20                                                            
         DROP  R6                                                               
*                                                                               
GETSLN   CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R1),DUB                                                      
         BR    RE                                                               
*                                                                               
PRD20    MVC   PDSTATUS,=C'NEW'                                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        FIND ACTIVITY ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   PRD24                                                            
         LR    R7,R6               SAVE ACTIVITY ELEM POINTER                   
         USING ACTVD,R7                                                         
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'95'        FIND DOWNLOAD ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   PRD24               IF NO ELEM, ALWAYS SEND                      
*                                                                               
         USING NPTDEL,R6                                                        
         MVC   PDSTATUS,SPACES                                                  
*                                                                               
         OC    ACTVCHDT,ACTVCHDT   HAS RECORD EVER CHANGED                      
         BZ    *+14                NO                                           
         CLC   ACTVCHDT,NPTDDATE   CHANGED AFTER LAST DOWNLOAD                  
         BH    PRD22               YES                                          
*                                                                               
         TM    FLAGFTR,FLAGDOWN    NOT CHANGED - TEST SEND ALL                  
         JZ    EXIT                EXIT IF UPDATES ONLY                         
         B     PRD24               ELSE SEND STATUS OF BLANK                    
*                                                                               
PRD22    MVC   PDSTATUS,=C'CHA'    ONLY CHANGES AFTER DNLD GET 'CHA'            
         DROP  R6                                                               
*                                                                               
PRD24    L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING NPTDATA,R6                                                       
*                                                                               
         MVC   PDDESC,SPACES                                                    
         MVC   PDDESC(L'NPTDESC),NPTDESC      DESCRIPTION                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,NPTSTART),(5,PDPERIOD)                            
         MVI   PDPERIOD+8,C'-'                                                  
         MVC   PDPERIOD+9(10),=C'UFN       '  PERIOD                            
         CLC   NPTEND,=XL3'FFFFFF'                                              
         JE    PRD40                                                            
         GOTO1 DATCON,DMCB,(3,NPTEND),(5,PDPERIOD+9)                            
*                                                                               
         TM    NPTSTAT,NPTS_TIME     TEST ELEM HAS START TIME                   
         BZ    PRD26                                                            
         XC    FULL,FULL                                                        
         OC    NPTSTIM,NPTSTIM                                                  
         BZ    PRD26                                                            
         MVC   FULL(2),NPTSTIM                                                  
         GOTO1 UNTIME,DMCB,FULL,NPTSTIM                                         
         MVC   FULL(2),NPTETIM                                                  
         GOTO1 (RF),(R1),,NPTETIM                                               
*                                                                               
PRD26    CLI   KEY+NPTXPROG-NPTKEY,X'FF'   TEST DPT IN RECORD                   
         BNE   PRD28                                                            
         OC    NPTDPART,NPTDPART   TEST 2-BYTE DPT IN ELEMENT                   
         BZ    *+10                                                             
         MVC   PDCODE,NPTDPART                                                  
*                                                                               
PRD28    MVI   ELCODE,X'32'        FIND ROTATION ELEMENT                        
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   PRD40                                                            
*                                                                               
         USING NPTPTNEL,R6                                                      
PRD30    LLC   R1,NPTPTNLN                                                      
         AHI   R1,-3               GET ROT LEN -1                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDROT(0),2(R6)                                                   
*                                                                               
PRD40    L     R6,AIO                                                           
         USING NPTRECD,R6                                                       
*                                                                               
         MVI   ELCODE,X'10'        FIND STATUS ELEM                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ADIDFLAG,C'Y'                                                    
         USING NPTDTAEL,R6                                                      
         TM    NPTSTAT,NPTS_ADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRD60                                                            
         USING NPTCMLEL,R6                                                      
*                                                                               
         LLC   R0,1(R6)                                                         
         SRL   R0,4                                                             
         LA    R3,PDCMLA                                                        
         LA    R6,NPTCML                                                        
*                                                                               
PRD52    ST    R3,FULL                                                          
         MVC   0(25,R3),SPACES                                                  
         MVC   0(8,R3),0(R6)       MOVE ISCI CMML                               
*                                                                               
         CLC   =X'5C00',0(R3)      TEST DELETED                                 
         BE    PRD52A                                                           
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PRD52A                                                           
         GOTO1 VTRPACK,DMCB,(C'U',0(R6)),0(R3)                                  
*                                                                               
PRD52A   CLI   8(R6),C' '          TEST FOR SECOND CMML                         
         BNH   PRD52X              NO                                           
*                                                                               
         LA    R3,13(R3)           BACK UP TO LAST CHAR                         
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
*                                                                               
         MVI   1(R3),C'-'                                                       
         MVC   2(8,R3),8(R6)                                                    
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PRD52X                                                           
         GOTO1 VTRPACK,DMCB,(C'U',8(R6)),2(R3)                                  
*                                                                               
PRD52X   L     R3,FULL             GET START OF FIELD ADDRESS                   
         LA    R3,PDCMLB-PDCMLA(R3)                                             
         LA    R6,16(R6)                                                        
         BCT   R0,PRD52                                                         
*                                                                               
         L     R6,AIO              PERCENTAGE ELEMENT                           
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         JNE   PRD60                                                            
         MVC   PDROT,SPACES        IF PCTS, NO ROTATION                         
*                                                                               
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'3'                                                         
         LR    R0,R1                                                            
         LA    R6,2(R6)            FIRST ENTRY                                  
*                                                                               
PRD54    LLC   RE,0(R6)            GET CML LETTER                               
         AHI   RE,-193             A=X'C1'=193 IS +0                            
         CHI   RE,9                                                             
         BNH   *+8                                                              
         AHI   RE,-7               J=X'D1'=209 IS +9                            
         MHI   RE,PDPCTB-PDPCTA    GIVES DISTANCE BETWEEN PCTS                  
         LA    RE,PDPCTA(RE)                                                    
         SR    R1,R1                                                            
         ICM   R1,3,1(R6)          GET PCT                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,RE),DUB                                                      
         LA    R6,3(R6)            NEXT PCT ENTRY                               
         BCT   R0,PRD54                                                         
                                                                                
* COMMENTS *                                                                    
                                                                                
PRD60    LA    R0,4                MAX COMMENTS LINES                           
         LA    R2,PDCMT1                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'        COMMENT ELEMENT(S)                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PRD62    BRAS  RE,NEXTEL                                                        
         BNE   PRD70                                                            
         USING NPTCMTEL,R6                                                      
         LLC   R1,NPTCMTLN         GET COMMENT ELEM LEN                         
         AHI   R1,-4               GET DATA LEN -1                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),3(R6)                                                    
         LA    R2,PDCMT2-PDCMT1(R2)                                             
         BCT   R0,PRD62                                                         
         EJECT                                                                  
*===============================================================                
* ALL FIELDS NOW IN OUTPUT BUFFERS                                              
* CALL DLFLD FOR EACH DATA FIELD                                                
*===============================================================                
                                                                                
PRD70    LA    RE,SORTCNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,SORTCNT                                                       
*                                                                               
         LA    R6,DOWNTAB                                                       
PRD72    MVI   D.DLCBACT,DLCBPUT     ACTION IS PUT                              
         MVI   D.DLCBTYP,DLCBTXT     TYPE IS TEXT                               
         OI    D.DLCBFLG1,DLCBFXFL   USE EXTENDED FOR TEX                       
*                                                                               
         L     RE,AIO3               DATA BUFFER                                
         AH    RE,0(R6)              ADD DSPL TO BUFFER START                   
*                                                                               
         LLC   RF,2(R6)              GET DATA LEN                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D.DLCBFLX(0),0(RE)                                               
*                                                                               
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
         LA    R6,3(R6)                                                         
         CLI   0(R6),X'FF'                                                      
         BNE   PRD72                                                            
*                                                                               
         MVI   D.DLCBACT,DLCBEOL          END OF LINE                           
         GOTO1 VDLFLD,DLCB                                                      
                                                                                
* NOW UPDATE PATTERN RECORD WITH LAST DOWNLOAD DATE                             
                                                                                
         OI    GENSTAT1,RDUPAPPL   TELL GENCON TO ALLOW RD FOR UPD              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              FIRST GET RECORD FOR UPDATE                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'95'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRD74                                                            
         GOTO1 REMELEM                                                          
*                                                                               
PRD74    LA    R7,ELEM                                                          
         USING NPTDEL,R7                                                        
         XC    ELEM,ELEM                                                        
         MVI   NPTDEL,X'95'                                                     
         MVI   NPTDLN,NPTDELX-NPTDEL                                            
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,DUB)                                        
         MVC   NPTDDATE,DUB                                                     
*                                                                               
         TIME  DEC                 R0=HHMMSSHH                                  
         STCM  R0,12,NPTDTIME                                                   
*                                                                               
         MVC   NPTDWHO(3),REMUSER                                               
*                                                                               
         GOTO1 ADDELEM             ADD NEW DOWNLOAD ELEMENT                     
         GOTO1 PUTREC              AND WRITE PATTERN RECORD                     
         J     EXIT                                                             
*                                                                               
PRDOWNX  DS    0H                                                               
         OC    SORTCNT,SORTCNT     TEST ANY DATA                                
         JZ    EXIT                                                             
         MVI   D.DLCBACT,DLCBEOR          END OF REPORT                         
         GOTO1 VDLFLD,DLCB                                                      
         J     EXIT                                                             
*                                                                               
DWNHOOK  NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
         EJECT                                                                  
*===========================================================                    
* BUILD DOWNLOAD HEADER IN ELEM                                                 
*===========================================================                    
                                                                                
BLDHDR   NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM             POINT TO HEADER BUILD AREA                   
         USING PDHDRD,R5                                                        
         MVC   PDAGYNAM(33),SVUSER  REQUESTING AGENCY NAME                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,DUB)  GET YYYYMMDD                         
         MVC   PDCRDATE(4),DUB+4                                                
         MVC   PDCRDATE+4(4),DUB                                                
*                                                                               
         THMS  DDSTIME=Y                                                        
         ST    R0,FULL             STORE DDS TIME CORRECTION                    
         ST    R1,DUB              STORE TIME                                   
         AP    FULL,DUB(4)         AND ADD PACKED!                              
         MVI   PDCRTIME+4,C'A'                                                  
         CP    FULL,=P'120000'                                                  
         BNH   *+14                                                             
         MVI   PDCRTIME+4,C'P'                                                  
         SP    FULL,=P'120000'                                                  
         UNPK  DUB,FULL                                                         
         MVC   PDCRTIME(4),DUB+2  MOVE HHMMSS                                   
*                                                                               
         XC    FLD,FLD                                                          
         CLI   OFFLINE,C'Y'                                                     
         BNE   BLDHDR2                                                          
*                                                                               
         L     RE,TWAMASTC                                                      
         L     RE,MCAEXTRA-MASTD(RE)   POINT TO EXTRA DATA AREA                 
         USING MCEXTRA,RE                                                       
         MVC   PDBYWHOM(18),MCLNAME LAST NAME                                   
         MVC   PDBYWHOM+19(9),MCFNAME FIRST NAME                                
         B     BLDHDRX                                                          
         DROP  RE                                                               
*                                                                               
BLDHDR2  L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         XC    DMCB,DMCB                                                        
         GOTO1 (RF),DMCB                                                        
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         TM    FATFLAG,X'08'       IF NO PID, EXIT!                             
         BZ    BLDHDRX                                                          
         MVC   FLD(2),FAPASSWD     SAVE PID                                     
         MVC   FLD+2(2),FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R1                                                               
*                                                                               
         XC    WORK(25),WORK                                                    
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(2),FLD+2     MOVE SECURITY AGENCY                         
         CLC   FLD+2(2),SPACES                                                  
         BH    *+10                                                             
         MVC   WORK+1(2),AGENCY                                                 
         MVC   WORK+23(2),FLD      MOVE PID                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO3                     
         L     R6,AIO3                                                          
         CLC   WORK(25),0(R6)                                                   
         BNE   BLDHDR20                                                         
         LA    R6,28(R6)                                                        
*                                                                               
BLDHDR4  CLC   =X'0318',0(R6)                                                   
         BE    BLDHDR12                                                         
*                                                                               
         CLC   =X'C30A',0(R6)        NEW SECURITY - PERSON ELEMENT              
         BE    BLDHDR6                                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   BLDHDR4                                                          
         B     BLDHDR20                                                         
*                                                                               
BLDHDR6  MVC   PDPID,2(R6)         PID                                          
*                                                                               
         XC    WORK(25),WORK                                                    
         MVI   WORK,C'F'           GET PERSON RECORD                            
         MVI   WORK+1,X'04'                                                     
         MVC   WORK+13(2),FLD+2    SET SECURITY AGENCY                          
         CLC   FLD+2(2),SPACES                                                  
         BH    *+10                                                             
         MVC   WORK+13(2),AGENCY                                                
         MVC   WORK+15(8),2(R6)                                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO3                     
         L     R6,AIO3                                                          
         CLC   WORK(23),0(R6)                                                   
         BNE   BLDHDRX                                                          
         LA    R6,28(R6)                                                        
*                                                                               
BLDHDR10 CLI   0(R6),X'C5'                                                      
         BE    BLDHDR12                                                         
*                                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   BLDHDR10                                                         
         B     BLDHDR20                                                         
*                                                                               
         USING SANAMD,R6                                                        
BLDHDR12 LA    RE,SANAMES                                                       
         TM    SANAMIND,X'80'      TEST FIRST NAME PRESENT                      
         BZ    BLDHDR14                                                         
         MVC   PDBYWHOM(1),1(RE)   FIRST INITIAL                                
         LLC   RF,0(RE)                                                         
         LA    RE,1(RE,RF)         POINT TO NEXT NAME                           
*                                                                               
BLDHDR14 TM    SANAMIND,X'40'      TEST MIDDLE NAME PRESENT                     
         BZ    BLDHDR16                                                         
         LLC   RF,0(RE)            SKIP MIDDLE NAME                             
         LA    RE,1(RE,RF)         POINT TO NEXT NAME                           
*                                                                               
BLDHDR16 TM    SANAMIND,X'20'      TEST LAST NAME PRESENT                       
         BZ    BLDHDR20                                                         
         LLC   RF,0(RE)            GET NAME LENGTH                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PDBYWHOM+2(0),1(RE)                                              
         B     BLDHDRX                                                          
*                                                                               
BLDHDR20 MVC   PDBYWHOM(20),=C'SORRY - I DON''T KNOW!'                          
         B     BLDHDRX                                                          
*                                                                               
BLDHDRX  OC    ELEM,SPACES         MAKE SURE NO X'00'                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*DOWNTAB'                                                    
                                                                                
* 2 BYTE DSPL FROM PDDATA (IF BYTE3=0)                                          
                                                                                
DOWNTAB  DS    0D                                                               
         DC    AL2(PDITM-PDDATA),AL1(L'PDITM)                                   
         DC    AL2(PDMED-PDDATA),AL1(L'PDMED)                                   
         DC    AL2(PDCLI-PDDATA),AL1(L'PDCLI)                                   
         DC    AL2(PDNET-PDDATA),AL1(L'PDNET)                                   
         DC    AL2(PDPROG-PDDATA),AL1(L'PDPROG)                                 
         DC    AL2(PDCODE-PDDATA),AL1(L'PDCODE)                                 
         DC    AL2(PDPRD-PDDATA),AL1(L'PDPRD)                                   
         DC    AL2(PDSLN-PDDATA),AL1(L'PDSLN)                                   
         DC    AL2(PDPTR-PDDATA),AL1(L'PDPTR)                                   
         DC    AL2(PDSLN2-PDDATA),AL1(L'PDSLN2)                                 
         DC    AL2(PDFEED-PDDATA),AL1(L'PDFEED)                                 
         DC    AL2(PDSTATUS-PDDATA),AL1(L'PDSTATUS)                             
         DC    AL2(PDREF-PDDATA),AL1(L'PDREF)                                   
*                                                                               
         DC    AL2(PDDESC-PDDATA),AL1(L'PDDESC)                                 
         DC    AL2(PDPERIOD-PDDATA),AL1(L'PDPERIOD)                             
         DC    AL2(PDSTIM-PDDATA),AL1(L'PDSTIM)                                 
         DC    AL2(PDETIM-PDDATA),AL1(L'PDETIM)                                 
         DC    AL2(PDCMLA-PDDATA),AL1(L'PDCMLA)                                 
         DC    AL2(PDPCTA-PDDATA),AL1(L'PDPCTA)                                 
         DC    AL2(PDCMLB-PDDATA),AL1(L'PDCMLB)                                 
         DC    AL2(PDPCTB-PDDATA),AL1(L'PDPCTB)                                 
         DC    AL2(PDCMLC-PDDATA),AL1(L'PDCMLC)                                 
         DC    AL2(PDPCTC-PDDATA),AL1(L'PDPCTC)                                 
         DC    AL2(PDCMLD-PDDATA),AL1(L'PDCMLD)                                 
         DC    AL2(PDPCTD-PDDATA),AL1(L'PDPCTD)                                 
         DC    AL2(PDCMLE-PDDATA),AL1(L'PDCMLE)                                 
         DC    AL2(PDPCTE-PDDATA),AL1(L'PDPCTE)                                 
         DC    AL2(PDCMLF-PDDATA),AL1(L'PDCMLF)                                 
         DC    AL2(PDPCTF-PDDATA),AL1(L'PDPCTF)                                 
         DC    AL2(PDCMLG-PDDATA),AL1(L'PDCMLG)                                 
         DC    AL2(PDPCTG-PDDATA),AL1(L'PDPCTG)                                 
         DC    AL2(PDCMLH-PDDATA),AL1(L'PDCMLH)                                 
         DC    AL2(PDPCTH-PDDATA),AL1(L'PDPCTH)                                 
         DC    AL2(PDCMLI-PDDATA),AL1(L'PDCMLI)                                 
         DC    AL2(PDPCTI-PDDATA),AL1(L'PDPCTI)                                 
         DC    AL2(PDCMLJ-PDDATA),AL1(L'PDCMLJ)                                 
         DC    AL2(PDPCTJ-PDDATA),AL1(L'PDPCTJ)                                 
         DC    AL2(PDCMLK-PDDATA),AL1(L'PDCMLK)                                 
         DC    AL2(PDPCTK-PDDATA),AL1(L'PDPCTK)                                 
         DC    AL2(PDCMLL-PDDATA),AL1(L'PDCMLL)                                 
         DC    AL2(PDPCTL-PDDATA),AL1(L'PDPCTL)                                 
         DC    AL2(PDCMLM-PDDATA),AL1(L'PDCMLM)                                 
         DC    AL2(PDPCTM-PDDATA),AL1(L'PDPCTM)                                 
         DC    AL2(PDCMLN-PDDATA),AL1(L'PDCMLN)                                 
         DC    AL2(PDPCTN-PDDATA),AL1(L'PDPCTN)                                 
         DC    AL2(PDCMLO-PDDATA),AL1(L'PDCMLO)                                 
         DC    AL2(PDPCTO-PDDATA),AL1(L'PDPCTO)                                 
         DC    AL2(PDROT-PDDATA),AL1(L'PDROT)                                   
         DC    AL2(PDCMT1-PDDATA),AL1(L'PDCMT1)                                 
         DC    AL2(PDCMT2-PDDATA),AL1(L'PDCMT2)                                 
         DC    AL2(PDCMT3-PDDATA),AL1(L'PDCMT3)                                 
         DC    AL2(PDCMT4-PDDATA),AL1(L'PDCMT4)                                 
         DC    X'FFFF'                                                          
         EJECT                                                                  
       ++INCLUDE SPTRNPAT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNEQPRG                                                     
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         EJECT                                                                  
       ++INCLUDE SPTRNFEED                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA81D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA5AD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
*                                                                               
SVTN3PR0 EQU   SVT3PROF                                                         
         ORG                                                                    
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
CLRSTART DS    0C                                                               
SPTR61RR DS    A                                                                
VTRPACK  DS    A                                                                
VDLFLD   DS    A                                                                
PCTNXT   DS    A                                                                
ASVNET   DS    A                                                                
ASVOLD   DS    A                                                                
BREF     DS    F                   REF NUM                                      
BREFK    DS    F                   REF NUM COMPLEMENTED                         
         DS    0D                                                               
PCTTBL   DS    XL48                                                             
         ORG   PCTTBL                                                           
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
SVELEM   DS    XL256                                                            
*                                                                               
VALTBL   DS    XL16                                                             
LATEST   DS    H                                                                
MYSUBLIN DS    H                                                                
SVSTAGRP DS    XL3                STATION GROUP                                 
SVGRPKEY DS    XL7                                                              
SVGRPFLG DS    CL1                                                              
HASTIME  DS    CL1                                                              
DELFLAG  DS    CL1                                                              
NEWSUBSW DS    CL1                                                              
*                                                                               
MAXPCT   DS    F                                                                
MINPCT   DS    F                                                                
COMDIV   DS    F                                                                
RELTAB   DS    XL8       4 ENTRIES OF 1 BYTE QUOTIENT, 1 BYTE REMAINDER         
PRCNTSW  DS    XL1                                                              
CMMLCT   DS    XL1                                                              
DELCT    DS    XL1                                                              
PCTCT    DS    XL1                                                              
PCTTOT   DS    XL1                                                              
SIZEFLAG DS    CL1                 RAN OUT OF SPACE FOR SORT LIST               
REF      DS    XL2                                                              
FEED     DS    CL4                                                              
*                                                                               
PATDTS   DS    0XL6                                                             
STRTPAT  DS    XL3                 SAVED PATSTART FOR COMP TO CMML'S            
ENDPAT   DS    XL3                 SAVED PATEND FOR COMP TO CMML'S              
*                                                                               
DATE     DS    CL6                                                              
PRINTPTR DS    H                                                                
NETMKT   DS    H                                                                
NETCML   DS    CL4                 NETWORK SPECIFIC CML                         
NETWORK  DS    CL4                                                              
PROGRAM  DS    CL6                                                              
SVPROG   DS    CL6                 SAVE PROGRAM                                 
COMMLCT  DS    XL1                 NUMBER OF COMMERCIALS ENTERED                
COMMLDEL DS    XL1                 NUMBER OF COMMERCIALS DELETED                
CMLPRT   DS    XL1                 CMLS PRINTED ON ERROR REPORT (0,Y)           
SVCML    DS    XL8                 SAVE ORIGINAL CML                            
SVCMLRCL DS    XL3                 SAVE CML RECALL DATE                         
*                                                                               
HOLDSEQ  DS    XL3                                                              
HOLDREF  DS    XL3                                                              
*                                                                               
SVDSKAD  DS    XL4                 SAVED DISK ADDR FROM ADDREC                  
*                                                                               
PRDMATSW DS    XL1                 USED WITH VCML ROUTINE                       
*                                                                               
PRDMATPR EQU   X'80'               80 - PRODUCT FOUND IN COMMERCIAL             
PRDMATPT EQU   X'40'               40 - PARTNER FOUND IN CML                    
PRDMATBO EQU   X'C0'               C0 - BOTH PRODUCTS FOUND                     
PRDMATEQ EQU   X'08'               08 - PRD/PTR SPOT LENS=CML LEN               
*                                       -CML IS FOR PIGGYBACK PAIR              
PRDMATPP EQU   X'04'               04 - LIST - VALIDATE PRD/PTR                 
*                                                                               
*                                                                               
* IF SIGNIFICANT CHANGES ARE MADE, SAVE OLD PTN WITH LOW REF,                   
* BUT SAME PTN SEQ NO.                                                          
* SIGNIFICANT CHANGES ARE DATE, CMMLS, ROTATION, TIMES                          
*                                                                               
CHGESW   DS    XL1                 A CHANGE HAS BEEN MADE, FORCE SAVE           
*                                                                               
NEWSEQSW DS    XL1                 NEW PATTERN SEQ ADDED FOR THIS CLT           
NEWSQXSW DS    XL1                   "   "      "    "  ON XSPOT                
*                                                                               
COMPKEY  DS    CL32                COMPARE KEY FOR ONLINE LIST                  
SORTCNT DS     F                   CT OF PAT RECS IF SORT=DATE                  
FILTERS  DS    0CL31                                                            
FLAGFTR  DS    CL1                                                              
FLAGDEL  EQU   X'80'               LIST DELETES (X00=DON'T LIST)                
FLAGSORT EQU   X'40'                                                            
FLAGDATE EQU   X'20'               SORT BY DATE (X00 = REF ORDER)               
FLREPORT EQU   X'10'               DO ERROR REPORT ONLY                         
FLAGDOWN EQU   X'08'               DOWNLOAD ALL RECORDS                         
*MNV                                                                            
FLAGDIGI EQU   X'04'               INCLUDE DIGITAL PATTERNS                     
*MNV                                                                            
FLAGSEQ  EQU   X'02'               SHOW SEQ #                                   
FLAGALL  EQU   X'01'               SHOW DEL & UNDEL                             
*                                                                               
DATEFTR1 DS    CL3                                                              
DATEFTR2 DS    CL3                                                              
DATESFTR DS    CL1                                                              
CMLFTR   DS    CL8                                                              
CMLFTREBC DS   CL8                                                              
CMLFTRADI DS   CL1                                                              
CODEFTR  DS    CL2                                                              
HOLDSIGN DS    CL1                                                              
PERFTR   DS    CL6                 1ST 3 START DATE, 2ND 3 END DATE             
QPGR     DS    CL4                 PGROUP FILTER FOR DISPLAY                    
PRGRFTR  DS    XL2                 PGROUP NUMBER                                
NEEDYEAR DS    XL1                                                              
*                                                                               
         DS    0D                                                               
PGRLIST  DS    XL420               PGROUP LIST (140 PRDS 3 BYTES EACH)          
PGRLISTX EQU   *                                                                
NPGRLIST EQU   (PGRLISTX-PGRLIST)/3                                             
*                                                                               
SVCMLKEY DS    XL13                                                             
SVNKEY   DS    XL40                                                             
SVXNKEY  DS    XL40                                                             
SVMYKEY  DS    XL40                REPLACES SVKEY (CL25)                        
*                                                                               
PDATAST  DS    A                                                                
STRTSORT DS    A                                                                
NEXTSORT DS    A                                                                
NEXTMAX  DS    A                                                                
ANYPIG   DS    CL1                                                              
CMLFLAG  DS    CL1                 COMMERCIAL FLAG                              
*                                  80 - NETWORK SPEC CML                        
*                                  40 - PRODUCT ERROR                           
*                                  20 - EXCLUDED NETWORK                        
CMLDTSSW EQU   X'10'               CML REL DATE AFTER PAT START                 
CMLPATLN EQU   X'08'               PAT/CML SPOT LENGTH DIFFERENT                
CMLDTESW EQU   X'04'               CML RCL DATE AFTER PAT START                 
PGSOSW   EQU   X'02'               COMML PIGGY/SOLO MUST MATCH USAGE            
CMLPRDSW EQU   X'01'               PATTERN PROD NOT IN CML                      
*                                                                               
CMLFLAG1 DS    CL1                 COMMERCIAL FLAG                              
TYPESW   EQU   X'80'               P/B CMML TYPES NOT EQUAL                     
DELCMLSW EQU   X'40'               DELETED CML SWITCH                           
NOAPDTE  EQU   X'10'               NO CLIENT APPROVAL DATE                      
NOAPAIR  EQU   X'08'               NOT APPROVED TO AIR                          
CMLINVAL EQU   X'04'               INVALID CMML IN PATTERN                      
CMLINVTM EQU   X'02'               CMML MUST NOT HAVE TIME                      
*                                                                               
SVPRD2   DS    CL1                 SAVE PROD PARTNER                            
MYPFKEY  DS    CL1                                                              
MYKEY    DS    CL(L'KEY)                                                        
MYKEYSV  DS    CL(L'KEYSAVE)                                                    
*                                                                               
PBFLAG   DS    CL1                 P/B COMMERCIAL FLAG                          
FMEDIA   DS    CL1                 FIND MEDIA FOR THE NETWORK                   
SVMEDIA  DS    CL1                 SAVE MEDIA                                   
SVWORK   DS    CL64                SAVE WORK                                    
IDATE    DS    XL3                 INACTIVE DATE                                
SVCODE   DS    D                   SAVE STATION CODE                            
SVANET   DS    D                   SAVE APPROVED NETWORK BITS                   
*                                                                               
SVTYPE1  DS    CL4                                                              
SVSLEN1  DS    XL1                                                              
SVSLN1   EQU   SVSLEN1                                                          
SVSOLO1  DS    CL1                                                              
SVFLAG1  DS    XL1                                                              
SVTYPE2  DS    CL4                                                              
SVSLEN2  DS    XL1                                                              
SVSLN2   EQU   SVSLEN2                                                          
SVSOLO2  DS    CL1                                                              
SVFLAG2  DS    XL1                                                              
         DS    0D                                                               
REMTBL   DS    CL60                                                             
         ORG   REMTBL                                                           
MYPTNSTM DS    XL2                                                              
MYPTNETM DS    XL2                                                              
MYADJSTM DS    XL2                                                              
MYADJETM DS    XL2                                                              
PTADJSTM DS    XL2                                                              
PTADJETM DS    XL2                                                              
MYADJSDT DS    XL3                                                              
MYADJEDT DS    XL3                                                              
PTADJSDT DS    XL3                                                              
PTADJEDT DS    XL3                                                              
*                                                                               
         DS    0D                                                               
DLCB     DS    XL256                                                            
*                                                                               
CLREND   EQU   *                                                                
         EJECT                                                                  
* SORT RECORD FOR DATE SORT *                                                   
*                                                                               
SORTREC  DSECT                                                                  
SRTREC   DS    0CL32                                                            
SRTKEY   DS    0CL27                                                            
SRTCLT   DS    XL2                                                              
SRTNET   DS    CL4                                                              
SRTPROG  DS    CL6                                                              
SRTPRD   DS    CL3                                                              
SRTSLN   DS    XL1                                                              
SRTPRD2 DS     CL3                                                              
SRTSLN2  DS    XL1                                                              
SRTDTS   DS    XL4                                                              
SRTREFS  DS    XL3                                                              
SRTNPSSV DS    XL1                 NON-ZERO FOR PASSIVE KEY                     
SRTDSKAD DS    XL4                                                              
*                                                                               
* OFFLINE PRINT                                                                 
*                                                                               
PRTLINE  DSECT                                                                  
PPRLN    DS    CL7                                                              
         DS    CL2                                                              
PPTLN    EQU   PPRLN+110,7                                                      
*                                                                               
PCODE    DS    CL2                                                              
         DS    CL2                                                              
PFEED    DS    CL4                                                              
         DS    CL2                                                              
PNET     DS    CL4                                                              
         DS    CL2                                                              
PPROG    DS    CL6                                                              
         DS    CL2                                                              
PREF     DS    CL5                                                              
         DS    CL4                                                              
PDATA    DS    CL66                                                             
         ORG   PCODE+110                                                        
PPERIOD  DS    CL17                                                             
         ORG   PPERIOD+110                                                      
PDESC    DS    CL24                                                             
         ORG   PPERIOD+220                                                      
PTIME    DS    CL11                                                             
         EJECT                                                                  
* ONLINE LIST                                                                   
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LNET     DS    CL4                                                              
         DS    CL1                                                              
LPROG    DS    CL6                                                              
         DS    CL1                                                              
LPRLN    DS    CL7                                                              
         DS    CL1                                                              
LPTLN    DS    CL7                                                              
         DS    CL1                                                              
LREF     DS    CL3                                                              
         DS    CL1                                                              
LPER     DS    CL17                                                             
         DS    CL1                                                              
LDESC    DS    CL14                                                             
         DS    CL1                                                              
LDELETE  DS    CL10                                                             
         EJECT                                                                  
*========================================================                       
* DSECT FOR PATTERN DOWNLOAD DATA                                               
*========================================================                       
PDHDRD   DSECT                                                                  
PDHDR    DS    0C                                                               
PDCRDATE DS    CL8                                                              
PDCRTIME DS    CL5                                                              
PDPID DS       CL8                                                              
PDAGYNAM DS    CL40                                                             
PDBYWHOM DS    CL30                                                             
PDHDRX   EQU   *                                                                
*                                                                               
PDDATA   DSECT                                                                  
PDITM    DS    CL4                                                              
PDMED    DS    CL1                                                              
PDCLI    DS    CL3                                                              
PDNET    DS    CL4                                                              
PDPROG   DS    CL6                                                              
PDCODE   DS    CL2                                                              
PDPRD    DS    CL3                 PRD-LEN                                      
PDSLN    DS    CL3                                                              
PDPTR    DS    CL3                 PARTNER-LEN                                  
PDSLN2   DS    CL3                                                              
PDFEED   DS    CL4                                                              
PDSTATUS DS    CL3                 CHA/NEW                                      
PDREF    DS    CL6                                                              
PDDESC   DS    CL17                                                             
PDPERIOD DS    CL20                                                             
PDSTIM   DS    CL5                                                              
PDETIM   DS    CL5                                                              
PDDPT    DS    CL2                                                              
PDTEXT   DS    CL7                                                              
PDT      DS    CL1                                                              
PDT2     DS    CL72                                                             
PDT3     DS    CL72                                                             
PDT4     DS    CL72                                                             
PDROT    DS    CL69                                                             
*                                                                               
PDCMLA   DS    CL25                                                             
PDPCTA   DS    CL3                                                              
PDCMLB   DS    CL25                                                             
PDPCTB   DS    CL3                                                              
PDCMLC   DS    CL25                                                             
PDPCTC   DS    CL3                                                              
PDCMLD   DS    CL25                                                             
PDPCTD   DS    CL3                                                              
PDCMLE   DS    CL25                                                             
PDPCTE   DS    CL3                                                              
PDCMLF   DS    CL25                                                             
PDPCTF   DS    CL3                                                              
PDCMLG   DS    CL25                                                             
PDPCTG   DS    CL3                                                              
PDCMLH   DS    CL25                                                             
PDPCTH   DS    CL3                                                              
PDCMLI   DS    CL25                                                             
PDPCTI   DS    CL3                                                              
PDCMLJ   DS    CL25                                                             
PDPCTJ   DS    CL3                                                              
PDCMLK   DS    CL25                                                             
PDPCTK   DS    CL3                                                              
PDCMLL   DS    CL25                                                             
PDPCTL   DS    CL3                                                              
PDCMLM   DS    CL25                                                             
PDPCTM   DS    CL3                                                              
PDCMLN   DS    CL25                                                             
PDPCTN   DS    CL3                                                              
PDCMLO   DS    CL25                                                             
PDPCTO   DS    CL3                                                              
*                                                                               
PDCMT1   DS    CL72                                                             
PDCMT2   DS    CL72                                                             
PDCMT3   DS    CL72                                                             
PDCMT4   DS    CL72                                                             
*                                                                               
PDDATAX  EQU   *                                                                
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE SEACSFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'161SPTRA61   07/15/16'                                      
         END                                                                    
