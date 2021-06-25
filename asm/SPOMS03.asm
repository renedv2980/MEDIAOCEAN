*          DATA SET SPOMS03    AT LEVEL 123 AS OF 03/16/16                      
*PHASE T23403A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T23403 - MAKEGOOD LIST AND STATUS                                     
*                                                                               
*  CALLED FROM: ADDS CONTROLLER (T23400), WHICH CALLS                           
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS SPOMSFE (T234FE) -- MAKEGOOD LIST                            
*                  SPOMSF3 (T234F3) -- MGSTATUS                                 
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
T23403   TITLE 'SPOMS03 - LIST OF MAKEGOOD GROUPS'                              
T23403   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T23403*,R7,RR=R3                                              
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
*                                                                               
         NI    GENSTAT3,X'FF'-OKVALSEL                                          
         CLI   ACTEQU,ACTSTATS     ACTION STATUS?                               
         BNE   MAIN10              NO, THEN MG                                  
         NI    CTLRFLG1,X'FF'-CF1CKLST                                          
         GOTO1 INITIAL,DMCB,SPFTABLE                                            
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         B     XIT                                                              
*                                                                               
MAIN10   DS    0H                                                               
         BRAS  RE,SETMFKYS                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE               
         GOTO1 DATCON,DMCB,(5,0),(0,DATEFLTR)                                   
         GOTO1 ADDAY,DMCB,DATEFLTR,DATEFLTR,F'-14'                              
         GOTO1 DATCON,DMCB,(0,DATEFLTR),(19,DATEFLT1)                           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
         BRAS  RE,VK                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       TM    MISCFLG1,MF1KYCHG   IF ANY OF THE KEYS CHANGED                   
         BZ    *+14                                                             
         XC    SVLSTKEY,SVLSTKEY                                                
         B     LR                  THEN DISPLAY FROM BEGINNING                  
*                                                                               
         TM    GOTGLOB,GGLBDAR     COMING BACK FROM BUY?                        
         BZ    *+14                                                             
         OI    MISCFLG1,MF1BACK    YES                                          
         MVC   SVSELSID,TMPSLSID                                                
*                                                                               
         LA    R2,ORMSEL1H         CHECK ALL THE SELECT FIELDS                  
         USING MKGDLIND,R2                                                      
         LA    R4,KEYTABLE         R4 = A(1ST ENTRY IN OUR TABLE)               
         USING KYTABLED,R4                                                      
*                                                                               
VRSELLP  LA    R0,ORMPFLNH                                                      
         CR    R2,R0                                                            
         BNL   VRSELX                                                           
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VRSELNX             YES                                          
         OI    MISCFLG1,MF1BACK    SET TO REDISPLAY LIST LATER                  
*                                                                               
         OC    MGORDER,MGORDER                                                  
         BNZ   VR10                                                             
VRINVLD  XC    8(L'ORMSEL1,R2),8(R2)   CLEAR SELECT FIELD                       
         OI    4(R2),X'20'             VALIDATE IT FOR NEXT TIME                
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'             TRANSMIT                                 
         B     INVLFLD                                                          
*                                                                               
VR10     GOTOR BINORDR,DMCB,MGORDER  GET BINARY ORDER NUMBER                    
         CLI   MGSTATN,C'0'          CABLE MAKEGOOD?                            
         BNL   VR20                  YES, WE HAVE DIFF KEYS                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                GET THE MAKEGOOD NOTICE RECORD             
         USING DAREMGND,R6                                                      
         MVI   MNKTYPE,MNKTYPQ       X'0D' - TYPE EQUATE                        
         MVI   MNKSUBTY,MNKSTYPQ     X'36' - SUBTYPE EQUATE                     
         MVC   MNKAGMD,BAGYMD                                                   
         MVC   MNKBYR,MGBUYER                                                   
         OC    MNKBYR,SPACES                                                    
         MVC   MNKORDER,BINORDER                                                
         MVC   MNKGROUP,MGGRPCD                                                 
         OC    MNKGROUP,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'MNKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,MNSTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    VR30                                                             
         DC    H'0'                                                             
*                                                                               
VR20     XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY             GET THE MAKEGOOD NOTICE RECORD             
         USING MNXKEY,R6                                                        
         MVI   MNXKTYPE,MNXKTYPQ   X'0D' - TYPE EQUATE                          
         MVI   MNXKSBTY,MNXKSBTQ   X'36' - SUBTYPE EQUATE                       
         MVC   MNXKAGMD,BAGYMD                                                  
         MVC   MNXKORDR,BINORDER                                                
         MVC   MNXKGRP,MGGRPCD                                                  
         OC    MNXKGRP,SPACES                                                   
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVC   BIGKEYSV,BIGKEY     PASS BACK DELETED RECORDS                    
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',BIGKEY,      +        
               BIGKEY                                                           
         CLC   BIGKEY(L'MNXKEY),BIGKEYSV                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'XSPFIL',BIGKEY+36,   +        
               AIO,DMWORK                                                       
         L     R6,AIO                                                           
         LA    R6,MNXFRST-MNXKEY(R6) CAN'T USE NORMAL GETEL AS THAT IS          
         MVI   ELCODE,MNSTELQ          SETUP FOR 'SPTFIL' NOT 'XSPFIL'          
         BAS   RE,FIRSTEL                                                       
         BE    VR30                                                             
         DC    H'0'                                                             
*                                                                               
         USING MNSTELD,R6          CHECK THE STATUS                             
VR30     CLI   MNSTSTAT,MNSTDELV   DELIVERED STATUS                             
         BNE   VR50                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VR30                                                             
*                                                                               
VR50     CLC   =C'E97',8(R2)       ERROR 97?                                    
         BNE   VRSEL00                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   INVLFLD             NOT VALID FOR NON-DDS TERMINALS              
         CLI   MNSTSTAT,MNSTERR    ERROR STATUS?                                
         BNE   *+14                                                             
         CLC   MNSTERRN,=H'97'     ERROR 97?                                    
         BE    INVLFLD             YES, CAN'T HAVE ANOTHER ONE                  
*                                                                               
         XC    ELEM,ELEM                                                        
         LR    R0,R6               SAVE A(ELEMENT) IN RECORD                    
         LA    R6,ELEM                                                          
         MVI   MNSTEL,MNSTELQ                                                   
         MVI   MNSTLEN,MNSTELNQ                                                 
         GOTO1 DATCON,DMCB,(5,0),(19,MNSTDATE)                                  
         MVI   MNSTSTAT,MNSTERR                                                 
         MVC   MNSTERRN,=H'97'                                                  
         LR    R6,R0               RESTORE A(1ST STATUS ELEM)                   
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
*                                                                               
         GOTO1 PUTREC                                                           
         B     VRSEL100                                                         
*                                                                               
VRSEL00  CLI   8(R2),C'A'          SELECTED TO APPLY ON HOLD MKGD?              
         BNE   VRSEL10                                                          
         CLI   MNSTSTAT,MNSTHOLD   STATUS MUST BE "ON HOLD" FOR THIS            
         BE    VRSEL20                 SELECT CODE                              
*                                                                               
         CLI   MNSTSTAT,MNSTGOIN   IF OKAYED, CHECK DATE/TIME                   
         BNE   INVLFLD                                                          
         GOTO1 DATCON,DMCB,(5,0),(19,WORK)                                      
         THMS  DDSTIME=YES                                                      
         STCM  R1,15,PACKOF4B                                                   
         STCM  R0,15,FULL                                                       
         AP    PACKOF4B,FULL       DDS TIME IS OFFSET BY 6AM                    
         AP    PACKOF4B,=P'15'       ADD 15 MINUTES                             
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         JL    VRSEL05                                                          
         SP    PACKOF4B,=P'240000'    YUP, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   THE TIME                             
         GOTO1 ADDAY,DMCB,DUB,DUB,F'1'                                          
         GOTO1 DATCON,DMCB,(0,DUB),(19,WORK)                                    
*                                                                               
VRSEL05  ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,WORK+3         CURRENT TIME PWOS                            
         CLC   MNSTDATE,WORK       OKAYED BEFORE TODAY?                         
         BL    VRSEL20                                                          
         CLC   MNSTTIME,WORK+3                                                  
         BNH   VRSEL20                                                          
         B     INVLFLD                                                          
*                                                                               
VRSEL10  CLI   8(R2),C'B'          SELECT TO GO TO BUY PROGRAM?                 
         BNE   VRSEL15                                                          
         CLI   MNSTSTAT,MNSTCAN    STATUS IS "CANCELLED"?                       
         BE    MGCANREP                                                         
         CLI   MNSTSTAT,MNSTCANM   STATUS IS "CANCEL W/ MORE TO FOLLOW"         
         BE    MGCANMOR                                                         
         CLI   MNSTSTAT,MNSTERR    ERROR STATUS?                                
         BNE   VRSEL20                                                          
         CLC   MNSTERRN,=H'97'     ERROR 97?                                    
         BE    INVLFLD             YES, CAN'T GO TO THE BUY WITH THIS           
         B     VRSEL20                                                          
*                                                                               
VRSEL15  CLI   8(R2),C'C'          SELF APPLY MAKEGOOD?                         
         BNE   VRSEL100            NO                                           
*                                                                               
         CLI   MNSTSTAT,MNSTNEW    NEW STATUS                                   
         BE    VRSEL20                                                          
         CLI   MNSTSTAT,MNSTAPP    APPROVED STATUS                              
         BE    VRSEL20                                                          
         CLI   MNSTSTAT,MNSTAMND   AMEND STATUS                                 
         BNE   INVLFLD                                                          
         DROP  R6                                                               
*                                                                               
VRSEL20  DS    0H                                                               
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    VRSEL25                                                          
         TM    OMBASFL2,OM2DACT    AGENCY DEACTIVATED?                          
         BNZ   AGYDACT              - YUP, ERROR                                
*                                                                               
VRSEL25  CLI   MGSTATN,C'0'          CABLE MAKEGOOD?                            
         BNL   VRSEL30               YES, WE HAVE DIFF KEYS                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY              GET THE MAKEGOOD OFFER RECORD                
         USING MOKEY,R6                                                         
         MVI   MOKTYPE,MOKTYPQ     X'0D' - TYPE EQUATE                          
         MVI   MOKSUBTY,MOKSTYPQ   X'37' - SUBTYPE EQUATE                       
         MVC   MOKAGMD,BAGYMD                                                   
         MVC   MOKORDER,BINORDER                                                
         MVC   MOKMGCD,MGGRPCD                                                  
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'MOKEY),KEYSAVE                                             
         BE    VRSEL35                                                          
         B     INVLFLD                                                          
*                                                                               
VRSEL30  XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY           GET THE MAKEGOOD OFFER RECORD                
         USING MOXKEY,R6                                                        
         MVI   MOXKTYPE,MOXKTYPQ   X'0D' - TYPE EQUATE                          
         MVI   MOXKSBTY,MOXKSBTQ   X'37' - SUBTYPE EQUATE                       
         MVC   MOXKAGMD,BAGYMD                                                  
         MVC   MOXKORDR,BINORDER                                                
         MVC   MOXKMGCD,MGGRPCD                                                 
         OC    MOXKMGCD,SPACES                                                  
*                                                                               
         MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',BIGKEY,      +        
               BIGKEY                                                           
         CLC   BIGKEY(L'MOXKEY),BIGKEYSV                                        
         BNE   INVLFLD                                                          
*                                                                               
VRSEL35  CLI   8(R2),C'A'          CHECK IF ESTIMATE LOCKED IF APPLY            
         BE    VRSEL40                                                          
         CLI   8(R2),C'C'                                                       
         BNE   VRSEL60                                                          
*                                                                               
VRSEL40  LR    R0,R2                                                            
         MVC   FAKEFLDH,0(R2)                                                   
         MVC   FAKEFLD(L'MGCLIENT),MGCLIENT                                     
         MVI   FAKEFLDH+5,3                                                     
         CLI   MGCLIENT+2,C' '                                                  
         BNE   *+8                                                              
         MVI   FAKEFLDH+5,2                                                     
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALICLT                                                          
         LR    R2,R0                                                            
*                                                                               
         CLI   8(R2),C'C'                                                       
         BNE   VRSEL45                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'sDAR'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
*                                                                               
         CLI   CLTOFFCE,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFFCE                                              
                                                                                
         GOTO1 GETPROF,DMCB,WORK,PROFDAR,DATAMGR                                
*                                                                               
         CLI   PDARSAPP,C'N'       USE SELF APPLY MAKEGOODS?                    
         BE    INVLFLD                                                          
*                                                                               
VRSEL45  PACK  DUB,MGESTMTE                                                     
         CVB   R1,DUB                                                           
         STC   R1,BEST                                                          
*                                                                               
         MVI   BYTE,0                                                           
         LA    R6,KEY                                                           
         USING EKEY,R6                                                          
VRSEL50  XC    KEY,KEY                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
*                                                                               
         CLI   BYTE,0                                                           
         BNE   *+14                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         B     VRSEL55                                                          
*                                                                               
         CLI   BYTE,1                                                           
         BNE   *+14                                                             
         MVC   EKEYPRD,MGPRDCTS                                                 
         B     VRSEL55                                                          
*                                                                               
         CLI   BYTE,2                                                           
         BNE   *+10                                                             
         MVC   EKEYPRD,MGPRDCTS+4                                               
*                                                                               
VRSEL55  OC    EKEYPRD,EKEYPRD     CAN'T HAVE A NULL PRODUCT                    
         BZ    VRSEL60                                                          
*                                                                               
         MVC   EKEYEST,BEST                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(ELEN-EKEY),KEYSAVE                                           
         BNE   *+12                                                             
         TM    KEY+L'EKEY,X'0C'    HELD OR LOCKED?                              
         BNZ   VRSELELK            YES, ESTIMATE LOCKED                         
*                                                                               
         CLI   BYTE,2              TESTED ALL POSSIBLE PRD COMBOS?              
         BNL   VRSEL60                                                          
*                                                                               
         ZIC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         B     VRSEL50                                                          
         DROP  R6                                                               
*                                                                               
VRSEL60  L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',ORMMEDH,,GLVSPMD                              
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC            CAN'T SWITCH TO BUY PROGRAM                  
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTD',MGBUYER,L'MGBUYER,GLVSPBYR                    
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
         MVC   WORK(L'MGCLIENT),MGCLIENT                                        
         OC    WORK(L'MGCLIENT),SPACES                                          
         GOTO1 (RF),DMCB,=C'PUTD',WORK,L'MGCLIENT,GLVSPCLT                      
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
         TM    KTBLFLG1,KTF1POLE   IS IT POL ESTIMATE?                          
         BZ    VRSEL65                                                          
         GOTO1 (RF),DMCB,=C'PUTD',=C'POL',3,GLVSPPRD  YES, PUT POL OUT          
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
         B     VRSEL80                                                          
*                                                                               
VRSEL65  MVC   WORK(L'MGPRDCTS),MGPRDCTS                                        
         OC    WORK(L'MGPRDCTS),SPACES                                          
         GOTO1 (RF),DMCB,=C'PUTD',WORK,3,GLVSPPRD  NO, 1ST PRODUCT              
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
         LA    R3,MGPRDCTS         CHECK IF THERE'S A PIGGYBACK                 
         LA    RE,L'MGPRDCTS                                                    
VRSEL70  CLI   0(R3),C'-'                                                       
         BE    VRSEL75                                                          
         LA    R3,1(R3)                                                         
         BCT   RE,VRSEL70                                                       
         B     VRSEL80                                                          
*                                                                               
VRSEL75  LA    R3,1(R3)                                                         
         MVC   WORK(L'MGPRDCTS),0(R3)                                           
         OC    WORK(L'MGPRDCTS),SPACES                                          
         GOTO1 (RF),DMCB,=C'PUTD',WORK,3,GLVSPPR2  2ND PRODUCT                  
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
VRSEL80  GOTO1 (RF),DMCB,=C'PUTD',MGESTMTE,L'MGESTMTE,GLVSPEST                  
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',BYTE,L'BYTE,GLVSPEFL                          
         CLI   DMCB+8,0                                                         
         BNE   VRSEL85                                                          
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPEFL                                    
*                                                                               
VRSEL85  CLC   MGFLIGHT,=C'00'                                                  
         BNH   VRSEL90                                                          
         PACK  DUB,MGFLIGHT                                                     
         CVB   R1,DUB                                                           
         STC   R1,BYTE                                                          
         GOTO1 (RF),DMCB,=C'PUTD',BYTE,L'BYTE,GLVSPEFL                          
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
VRSEL90  GOTO1 (RF),DMCB,=C'PUTD',MGSTATN,L'MGSTATN,GLVSPSTA                    
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTD',MGORDER,L'MGORDER,GLVSPORD                    
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTD',MGGRPCD,L'MGGRPCD,GLVSPMKG                    
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTD',MGPRDCTS,L'MGPRDCTS,GLVDRPRD                  
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC            CAN'T SWITCH TO BUY PROGRAM                  
*                                                                               
         XC    BLOCK(12),BLOCK                                                  
         LA    R1,BLOCK                                                         
         MVC   0(6,R1),=C'SPODAR'  FROM THE SPOT SYSTEM                         
*                                                                               
         OI    6(R1),X'80'         HEADLINE CHANGE IN DARE                      
         CLI   8(R2),C'A'          SELECTED TO APPLY ON HOLD MKGD?              
         BNE   *+8                                                              
         OI    6(R1),X'40'         MGEACC, ALSO                                 
*                                                                               
         CLI   MGCSHTRD,C'T'       SELECTED A TRADE ORDER MAKEGOOD?             
         BNE   *+8                                                              
         OI    6(R1),X'20'         TRADE                                        
         CLI   MGCSHTRD,C'C'       SELECTED A CASH ORDER MAKEGOOD?              
         BNE   *+8                                                              
         OI    6(R1),X'10'         CASH THAT HAS TRADE COUNTERPART              
*                                                                               
         CLI   8(R2),C'C'          SELECTED TO SELF APPLY MKGD?                 
         BNE   *+8                                                              
         OI    6(R1),X'08'                                                      
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTD',BLOCK,12,GLVNOTE                              
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
         XC    BLOCK(GLVXLENQ),BLOCK                                            
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'    FROM THE SPOT SYSTEM                         
         MVC   GLVXFRPR,=C'DAR'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'SPO'    TO THE SPOT SYSTEM                           
         MVC   GLVXTOPR,=C'BUY'    BUY PROGRAM                                  
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
*                                                                               
         OC    SVSELSID,SVSELSID   CAN WE GOTO A SPECIFIC SESSION?              
         BZ    VRSEL95             NO                                           
         OI    GLVXFLG1,GLV1SIDR+GLV1SIDE  IDS OF CALLER/EE                     
         MVC   GLVXSESR(2),SVSELSID                                             
         DROP  R1                                                               
*                                                                               
VRSEL95  GOTO1 (RF),DMCB,=C'PUTD',BLOCK,GLVXLENQ,GLVXCTL                        
         CLI   DMCB+8,0                                                         
         BNE   CANTSWTC                                                         
*                                                                               
         XC    8(L'ORMSEL1,R2),8(R2)   CLEAR SELECT FIELD                       
         OI    4(R2),X'20'             VALIDATE IT FOR NEXT TIME                
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'             TRANSMIT                                 
*                                                                               
         MVC   CONHEAD(23),=CL23'** BACK TO SPOT/DARE **'                       
         OI    CONHEADH+6,X'80'                                                 
         B     XIT                 SO MONITOR CAN TRANSFER CONTROL              
*                                                                               
VRSEL100 XC    8(L'ORMSEL1,R2),8(R2)   CLEAR SELECT FIELD                       
         OI    4(R2),X'20'             VALIDATE IT FOR NEXT TIME                
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'             TRANSMIT                                 
*                                                                               
VRSELNX  LA    R2,MKGDLNNX-MKGDLIND(R2)                                         
         LA    R4,KTBLLENQ(R4)                                                  
         B     VRSELLP                                                          
         DROP  R2,R4                                                            
*                                                                               
VRSELX   DS    0H                                                               
         TM    MISCFLG1,MF1BACK    CAME BACK FROM BUY?                          
         BZ    VRX                 NOPE                                         
         MVC   SVLSTKEY,KEYTABLE                                                
*                                                                               
         SR    R0,R0                                                            
         LA    R1,ORMPFLNH                                                      
VRSELX00 ICM   R0,1,0(R1)                                                       
         BZ    VRSELX10                                                         
         AR    R1,R0                                                            
         B     VRSELX00                                                         
VRSELX10 MVC   1(2,R1),=X'0101'    RETRANSMIT SCREEN                            
*                                                                               
VRX      B     LR                  CONTINUE DISPLAY                             
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       DS    0H                                                               
         XC    SVSELSID,SVSELSID   NEW LIST, NO SELECT CODES LEFT               
         NI    MISCFLG2,X'FF'-MF2RCNTF                                          
*                                                                               
         XR    RE,RE               ** MODIFIED TWAXC TO VALIDATE FIELD          
         LA    R1,ORMSEL1H                                                      
         LA    RF,ORMCLRLH                                                      
LR10     IC    RE,0(R1)                                                         
         AHI   RE,-9                                                            
         TM    1(R1),X'02'                                                      
         BZ    *+8                                                              
         AHI   RE,-8                                                            
         LTR   RE,RE                                                            
         BM    LR20                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         OI    4(R1),X'20'         <----  THIS IS THE ADDITIONAL INSTR          
         OI    6(R1),X'80'                                                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,LR10          ** MODIFIED TWAXC TO VALIDATE FIELD          
*                                                                               
LR20     LA    RE,KEYTABLE                                                      
         LA    RF,KYTBLLNQ                                                      
         XCEFL                                                                  
         XC    DKEYTABL,DKEYTABL                                                
*                                                                               
         XC    IO_COUNT,IO_COUNT   CLEAR ANY IO COUNTS                          
         NI    MISCFLG1,X'FF'-MF1CNTIO                                          
         TM    OPTFLAG,OPNSTATS+OPNSTATN   ANY OPTION FILTERS?                  
         BNZ   LR25                YES, NEED TO COUNT I/OS                      
         CLC   SVBUYRCD,SPACES     ANY BUYER CODE FILTER?                       
         BH    *+8                 YES                                          
LR25     OI    MISCFLG1,MF1CNTIO   NO, NEED TO COUNT I/O'S                      
*                                                                               
         LA    R2,ORMSEL1H         R2 = A(1ST LIST LINE'S DATA)                 
         USING MKGDLIND,R2                                                      
         XC    KEY,KEY                                                          
*                                                                               
         OC    SVLSTKEY,SVLSTKEY   DID WE START THE LIST PREVIOUSLY?            
         BNZ   LR30                                                             
         LA    R4,KEY              NO, START FROM FIRST FOR THE BUYER           
         USING DAREMGND,R4                                                      
         MVI   MNKTYPE,MNKTYPQ                                                  
         MVI   MNKSUBTY,MNKSTYPQ                                                
         MVC   MNKAGMD,BAGYMD                                                   
         MVC   MNKBYR,SVBUYRCD                                                  
         OC    MNKBYR,SPACES                                                    
         B     LRHIGH                                                           
*                                                                               
LR30     CLC   =X'FFFFFF',SVLSTKEY  ARE WE UPTO CABLE MAKEGOODS?                
         BE    LR100                                                            
         MVC   KEY(L'SVLSTKEY),SVLSTKEY  YES, START FROM LAST ONE               
*                                                                               
LRHIGH   MVI   RDUPDATE,C'N'                                                    
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         MVC   SVLSTKEY,KEY                                                     
*                                                                               
         BRAS  RE,BMPIOCNT         COUNT I/O'S                                  
         BE    LR40                                                             
LRMAXIO  B     LR200                                                            
*                                                                               
LR40     DS    0H                                                               
         TM    MNKSTAT,X'C0'       DELETED AND DON'T SHOW THIS?                 
         BO    LRSEQ               THAT'S RIGHT, NEXT RECORD                    
*********                                                                       
* FILTERING BY BUYER AND CHECKING IF BASE KEY IS A MATCH                        
*********                                                                       
         LA    R1,MNKBYR-MNKEY                                                  
         CLI   ORMBUYRH+5,0                                                     
         BE    *+8                                                              
         LA    R1,3(R1)                                                         
         BCTR  R1,0                                                             
         EX    R1,LR40CLC                                                       
         BE    LR45                                                             
*                                                                               
         MVC   SVLSTKEY,=X'FFFFFF' NO, SIGNAL TO DO CABLE MAKEGOODS             
         B     LR100                                                            
*                                                                               
LR40CLC  CLC   KEY(0),KEYSAVE                                                   
LR45CLC  CLC   ORMORDR(0),FAKEFLD                                               
LR50CLC  CLC   ORMGRP(0),MNKGROUP                                               
*********                                                                       
* FILTERING BY ORDER NUMBER                                                     
*********                                                                       
LR45     LA    R4,KEY                                                           
         CLI   ORMORDRH+5,0                                                     
         BE    LR50                                                             
         GOTOR SHWORDER,DMCB,MNKORDER,FAKEFLD                                   
         XR    R1,R1                                                            
         IC    R1,ORMORDRH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,LR45CLC                                                       
         BNE   LRSEQ                                                            
*********                                                                       
* FILTERING BY GROUP                                                            
*********                                                                       
LR50     CLI   ORMGRPH+5,0                                                      
         BE    LR60                                                             
         ZIC   R1,ORMGRPH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,LR50CLC                                                       
         BNE   LRSEQ                                                            
         DROP  R4                                                               
*********                                                                       
* FILTERING BY STATION                                                          
*********                                                                       
LR60     TM    OPTFLAG,OPNSTATN    FILTER ON STATION?                           
         BNO   LR65                NO                                           
         MVC   SAVEKEY,KEY         READ ORDER KEY FOR THE STATION               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DAREORDD,R4                                                      
         MVI   DOKTYPE,DOKTYPQ                                                  
         MVI   DOKSUBTY,DOKSTYPQ                                                
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,SAVEKEY+MNKORDER-MNKEY                                  
*                                                                               
         NI    DMINBTS,X'FF'-X'08'   DON'T PASS BACK DELETED ORDERS             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         OI    DMINBTS,X'08'         AS IT MIGHT BE AFTER CALL SWITCH           
         BRAS  RE,BMPIOCNT                                                      
         BNE   LRMAXIO                                                          
*                                                                               
         XC    WORK,WORK                                                        
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BNE   LR61                                                             
         MVC   WORK+2(L'DOKSTA),DOKSTA                                          
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,WORK+9                                   
                                                                                
LR61     MVI   RDUPDATE,C'N'                                                    
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         BRAS  RE,BMPIOCNT                                                      
         BNE   LRMAXIO                                                          
*                                                                               
         CLC   KEY(L'MNKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    WORK+9(L'SAVSTATN),WORK+9   DID WE FIND AN ORDER?                
         BZ    LRSEQ                       NO, NEXT                             
         CLC   SAVSTATN,WORK+9     COMPARE THE STATION                          
         BNE   LRSEQ                                                            
*                                                                               
LR65     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BRAS  RE,BMPIOCNT                                                      
         BNE   LRMAXIO                                                          
*                                                                               
         L     R6,AIO                                                           
         LR    R4,R6                                                            
*                                                                               
         USING MNMSELD,R6                                                       
         MVC   STATUS,SPACES                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,MNSTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DLVRDFLG,0                                                       
*                                                                               
         USING MNSTELD,R6                                                       
LR69     LA    R3,STATABLE         CHECK TABLE FOR STATUS                       
LR70     CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),MNSTSTAT                                                 
         BE    *+12                                                             
         LA    R3,10(R3)                                                        
         B     LR70                                                             
         MVC   STATUS,1(R3)                                                     
*                                                                               
         CLI   MNSTSTAT,MNSTDELV   DELIVERED STATUS?                            
         BNE   LR73                                                             
         ST    R6,ADELNOT                                                       
         OI    DLVRDFLG,GOTDLVRD   GOT DELIVERY NOTICE                          
         TM    OPTFLAG,OPNSTATS    FILTERING ON STATUS?                         
         BNO   LR71                NO, SKIP DIRECTLY TO NEXT STAT ELEM          
         ZIC   R1,STATLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   STATUS(0),SAVSTAT   YES, CHECK FILTERING FOR DELIVERED?          
         BE    LR73                     YES, DON'T SKIP DELIVERED STAT          
LR71     BAS   RE,NEXTEL                                                        
         BE    LR69                                                             
         DC    H'0'                                                             
*                                                                               
LR73     CLI   MNSTSTAT,MNSTCAN    STATUS IS "CANCELLED"?                       
         BNE   LR75                                                             
         L     R1,AIO                                                           
         TM    MNRSTAT-MNKEY(R1),X'80'   DELETED ALREADY?                       
         BNZ   LR80                      YES, DISPLAY IT                        
********                                                                        
         MVC   SVLSTKEY,KEY                                                     
**** DON'T WANT THIS PROGRAM DELETING KEYS AND RECS ANYMORE                     
         B     LR80                                                             
*&&DO                                                                           
         BRAS  RE,CHKSTA           YES, CHECK TO DELETE                         
         BE    LR80                                                             
*&&                                                                             
**** DON'T WANT THIS PROGRAM DELETING KEYS AND RECS ANYMORE                     
         CLC   =X'FFFF',IO_COUNT                                                
         BE    LRMAXIO                                                          
******** BE    LRSEQ               DELETED, READ NEXT RECORD                    
         B     LR80                                                             
*                                                                               
LR75     CLI   MNSTSTAT,MNSTOKAY   STATUS OKAY?                                 
         BNE   LR75A010                                                         
         XC    GROUPCD,GROUPCD                                                  
         L     R1,AIO                                                           
         TM    MNRSTAT-MNKEY(R1),X'80'   DELETED ALREADY?                       
         BNZ   LR80                      YES, DISPLAY IT                        
**** DON'T WANT THIS PROGRAM DELETING KEYS AND RECS ANYMORE                     
         B     LR80                                                             
*&&DO                                                                           
         BRAS  RE,CHKSTA           YES, CHECK TO DELETE                         
         BE    LRSEQ               DELETED, READ NEXT RECORD                    
*&&                                                                             
**** DON'T WANT THIS PROGRAM DELETING KEYS AND RECS ANYMORE                     
         CLC   =X'FFFF',IO_COUNT                                                
         BE    LRMAXIO                                                          
         OC    GROUPCD,GROUPCD                                                  
         BZ    LR80                                                             
         MVI   STATUS+6,C'/'                                                    
         MVC   STATUS+7,GROUPCD                                                 
*                                                                               
LR75A010 DS    0H                                                               
         CLI   MNSTSTAT,MNSTREJ    STATUS REJECT?                               
         BE    LR75A020                                                         
         CLI   MNSTSTAT,MNSTAPP    STATUS APPROVED?                             
         BNE   LR80                                                             
*                                                                               
LR75A020 DS    0H                                                               
         TM    DLVRDFLG,GOTDLVRD   GOT DELIVERY NOTICE?                         
         BNZ   LR80                YES:SHOW CORRECT STATUS                      
         MVI   STATUS,C'*'                                                      
         MVC   STATUS+1(L'STATUS-1),1(R3)                                       
*********                                                                       
* FILTERING BY STATUS                                                           
*********                                                                       
LR80     TM    OPTFLAG,OPNSTATS    FILTER ON STATUS?                            
         BNZ   LR80A               YES                                          
         CLI   MNSTSTAT,MNSTCAN                                                 
         BE    *+12                                                             
         CLI   MNSTSTAT,MNSTOKAY                                                
         BNE   LR85                                                             
         CLC   MNSTDATE,DATEFLT1                                                
         BNL   LR85                                                             
         B     LRSEQ                                                            
*                                                                               
LR80A    ZIC   R1,STATLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   STATUS(0),SAVSTAT   YES, CHECK SAME STATUS                       
         BNE   LRSEQ                                                            
*                                                                               
LR85     MVC   SVLSTKEY,0(R4)      SAVE LAST KEY THAT PASSED OUR TESTS          
*                                                                               
         LA    R0,ORMPFLNH         IF NO MORE LINES AVAILABLE ON SCREEN         
         CR    R2,R0                                                            
         BNL   LR200               THEN EXIT                                    
*                                                                               
         MVC   MGSTATN(L'SAVSTATN),SAVSTATN                                     
         CLI   STATUS,C'*'                                                      
         BNE   *+14                                                             
         MVC   MGSTATS1(L'STATUS),STATUS                                        
         B     *+10                                                             
         MVC   MGSTATUS(L'STATUS),STATUS                                        
         MVC   MGBUYER,MNKBYR-MNKEY(R4)                                         
         GOTOR SHWORDER,DMCB,MNKORDER-MNKEY(R4),MGORDER                         
         MVC   MGGRPCD,MNKGROUP-MNKEY(R4)                                       
*                                                                               
         CLI   MNSTSTAT,MNSTERR    STATUS ERROR?                                
         BNE   LR90                                                             
         MVC   MGACTDAT(8),=C'CALL DDS'                                         
         B     LR95                                                             
*                                                                               
LR90     TM    DLVRDFLG,GOTDLVRD   GOT DELIVERY NOTICE?                         
         BNO   *+8                                                              
         L     R6,ADELNOT                                                       
         GOTO1 DATCON,DMCB,(8,MNSTDATE),(11,MGACTDAT)                           
         GOTO1 HEXOUT,DMCB,MNSTTIME,WORK,L'MNSTTIME                             
         MVC   MGACTDAT+9(2),WORK                                               
         MVI   MGACTDAT+11,C':'                                                 
         MVC   MGACTDAT+12(2),WORK+2                                            
*                                                                               
LR95     DS    0H                                                               
         L     R6,AIO                                                           
         GOTOR MKGDCLR,DMCB,(R6)                                                
         MVC   MGCOLOR,BYTE                                                     
*                                                                               
LR95A    LH    R1,DKEYTABL         R1 = A(KEY TABLE ENTRY)                      
         LA    R0,KEYTABLE                                                      
         AR    R1,R0                                                            
         USING KYTABLED,R1                                                      
         MVC   KTBLKEY(L'MNKEY),0(R4)  SAVE KEY INTO THE TABLE ENTRY            
         DROP  R1                                                               
*                                                                               
         LA    R1,KTBLLENQ(R1)                                                  
         SR    R1,R0                                                            
         STH   R1,DKEYTABL         NEW DISP TO KEY TABLE ENTRY                  
*                                                                               
         LA    R2,MKGDLNNX-MKGDLIND(R2)  R2 = A(NEXT LIST LINE)                 
*                                                                               
         DROP  R6                                                               
*                                                                               
LRSEQ    MVI   RDUPDATE,C'N'       CHECK NEXT NOTICE RECORD                     
         OI    DMINBTS,X'08' PASS BACK DELETED RECORDS                          
         GOTO1 SEQ                                                              
         MVC   SVLSTKEY,KEY                                                     
*                                                                               
         BRAS  RE,BMPIOCNT         COUNT I/O'S                                  
         BNE   LRMAXIO                                                          
*                                                                               
LRSEQ1   LA    R4,KEY              RE-ESTABLISH KEY POINTER                     
         B     LR40                                                             
***********************************************************************         
* CABLE MAKEGOODS                                                               
***********************************************************************         
LR100    XC    BIGKEY,BIGKEY                                                    
*                                                                               
LR110    OC    SVLSTXKY,SVLSTXKY   DID WE LIST CABLE MAKEGOODS PREV?            
         BNZ   LR130               YES                                          
         LA    R4,BIGKEY           NO, START FROM FIRST CABLE MKGD              
         USING DAREMGND,R4                                                      
         MVI   MNXKTYPE,MNXKTYPQ   X'0D' - DARE MAKEGOOD                        
         MVI   MNXKSBTY,MNXKSBTQ   X'36' -               NOTICE RECORD          
         MVC   MNXKAGMD,BAGYMD     NO BUYER FOR CABLE MAKEGOODS                 
         B     LRXRDHI                                                          
*                                                                               
LR130    MVC   BIGKEY(L'SVLSTXKY),SVLSTXKY  START FROM LAST ONE                 
*                                                                               
LRXRDHI  MVI   RDUPDATE,C'N'                                                    
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',BIGKEY,      +        
               BIGKEY                                                           
*                                                                               
         BRAS  RE,BMPIOCNT         COUNT I/O'S                                  
         BE    LR140                                                            
         B     LR200                                                            
*                                                                               
LR145CLC CLC   ORMORDR(0),FAKEFLD                                               
LR150CLC CLC   ORMGRP(0),MNXKGRP                                                
*                                                                               
LR140END XC    SVLSTKEY,SVLSTKEY   NO, START FROM BEG NXT TIME                  
         XC    SVLSTXKY,SVLSTXKY                                                
         OI    MISCFLG1,MF1FIRST   NEED 'HIT ENTER FOR FIRST' MESSAGE           
         B     LR200                                                            
*                                                                               
LR140    DS    0H                                                               
         CLC   BIGKEY(MNXKORDR-MNXKEY),BIGKEYSV  DONE WITH CABLE MKGDS?         
         BNE   LR140END                          YES                            
*                                                                               
         OC    MNXKSTTN,MNXKSTTN   ANY STATION?                                 
         BNZ   LRXRDSQ             YES, THEN IT IS NOT THE STATUS REC           
         TM    MNXKSTAT,X'C0'      DELETED AND DON'T SHOW THIS?                 
         BO    LRXRDSQ             THAT'S RIGHT, NEXT RECORD                    
         MVC   SVLSTXKY,BIGKEY                                                  
*********                                                                       
* FILTERING BY ORDER NUMBER                                                     
*********                                                                       
LR145    LA    R4,BIGKEY                                                        
         CLI   ORMORDRH+5,0                                                     
         BE    LR150                                                            
         GOTOR SHWORDER,DMCB,MNXKORDR,FAKEFLD                                   
         XR    R1,R1                                                            
         IC    R1,ORMORDRH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,LR145CLC                                                      
         BNE   LRXRDSQ                                                          
*********                                                                       
* FILTERING BY GROUP                                                            
*********                                                                       
LR150    CLI   ORMGRPH+5,0                                                      
         BE    LR160                                                            
         ZIC   R1,ORMGRPH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,LR150CLC                                                      
         BNE   LRXRDSQ                                                          
         DROP  R4                                                               
*********                                                                       
* FILTERING BY STATION                                                          
*********                                                                       
LR160    TM    OPTFLAG,OPNSTATN    FILTER ON STATION?                           
         BNO   LR165               NO                                           
         XC    KEY,KEY             YES, WE NEED TO READ THE ORDER               
         LA    R4,KEY                                                           
         USING DAREORDD,R4                                                      
         MVI   DOKTYPE,DOKTYPQ                                                  
         MVI   DOKSUBTY,DOKSTYPQ                                                
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BIGKEY+MNXKORDR-MNXKEY                                  
*                                                                               
         NI    DMINBTS,X'FF'-X'08'   DON'T PASS BACK DELETED ORDERS             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         OI    DMINBTS,X'08'         AS IT MIGHT BE AFTER CALL SWITCH           
         BRAS  RE,BMPIOCNT                                                      
         BNE   LRMAXIO                                                          
*                                                                               
         XC    WORK,WORK                                                        
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BNE   LR161                                                            
         MVC   WORK+2(L'DOKSTA),DOKSTA                                          
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,WORK+9                                   
*                                                                               
LR161    OC    WORK+9(L'SAVSTATN),WORK+9    DID WE FIND AN ORDER?               
         BZ    LRXRDSQ                      NO, NEXT                            
         CLC   SAVSTATN,WORK+9     COMPARE THE STATION                          
         BNE   LRXRDSQ                                                          
*                                                                               
LR165    MVI   RDUPDATE,C'N'                                                    
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'XSPFIL',BIGKEY+36,   +        
               AIO,DMWORK                                                       
         BRAS  RE,BMPIOCNT                                                      
         BNE   LRMAXIO                                                          
*                                                                               
         L     R6,AIO                                                           
         LR    R4,R6                                                            
*                                                                               
         USING MNMSELD,R6                                                       
         MVC   STATUS,SPACES                                                    
         L     R6,AIO                                                           
         LA    R6,MNXFRST-MNXKEY(R6)   CAN USE NORMAL GETEL AS THAT IS          
         MVI   ELCODE,MNSTELQ            SET FOR 'SPTFIL' NOT 'XSPFIL'          
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DLVRDFLG,0                                                       
*                                                                               
         USING MNSTELD,R6                                                       
LR169    LA    R3,STATABLE         CHECK TABLE FOR STATUS                       
LR170    CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),MNSTSTAT                                                 
         BE    *+12                                                             
         LA    R3,10(R3)                                                        
         B     LR170                                                            
         MVC   STATUS,1(R3)                                                     
*                                                                               
         CLI   MNSTSTAT,MNSTDELV   DELIVERED STATUS?                            
         BNE   LR173                                                            
         ST    R6,ADELNOT                                                       
         OI    DLVRDFLG,GOTDLVRD   GOT DELIVERY NOTICE                          
         TM    OPTFLAG,OPNSTATS    FILTERING ON STATUS?                         
         BNO   LR171                NO, SKIP TO NEXT STAT ELEM                  
         ZIC   R1,STATLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   STATUS(0),SAVSTAT   YES, CHECK FILTERING FOR DELIVERED?          
         BE    LR173                    YES, DON'T SKIP DELIVERED STAT          
LR171    BAS   RE,NEXTEL                                                        
         BE    LR169                                                            
         DC    H'0'                                                             
*                                                                               
LR173    CLI   MNSTSTAT,MNSTCAN    STATUS IS "CANCELLED"?                       
         BNE   LR175                                                            
         L     R1,AIO                                                           
         TM    MNXRSTAT-MNXKEY(R1),X'80' DELETED ALREADY?                       
         BNZ   LR180                     YES, DISPLAY IT                        
********                                                                        
         MVC   SVLSTXKY,BIGKEY                                                  
**** DON'T WANT THIS PROGRAM DELETING KEYS AND RECS ANYMORE                     
         B     LR180                                                            
*&&DO                                                                           
         BRAS  RE,CHKSTA           YES, CHECK TO DELETE                         
         BE    LR180                                                            
*&&                                                                             
**** DON'T WANT THIS PROGRAM DELETING KEYS AND RECS ANYMORE                     
         CLC   =X'FFFF',IO_COUNT                                                
         BE    LRMAXIO                                                          
******** BE    LRXRDSQ             DELETED, READ NEXT RECORD                    
         B     LR180                                                            
*                                                                               
LR175    CLI   MNSTSTAT,MNSTOKAY   STATUS OKAY?                                 
         BNE   LR175A10                                                         
         XC    GROUPCD,GROUPCD                                                  
         L     R1,AIO                                                           
         TM    MNXRSTAT-MNXKEY(R1),X'80' DELETED ALREADY?                       
         BNZ   LR180                     YES, DISPLAY IT                        
**** DON'T WANT THIS PROGRAM DELETING KEYS AND RECS ANYMORE                     
         B     LR180                                                            
*&&DO                                                                           
         BRAS  RE,CHKSTA           YES, CHECK TO DELETE                         
         BE    LRXRDSQ             DELETED, READ NEXT RECORD                    
*&&                                                                             
**** DON'T WANT THIS PROGRAM DELETING KEYS AND RECS ANYMORE                     
         CLC   =X'FFFF',IO_COUNT                                                
         BE    LRMAXIO                                                          
         OC    GROUPCD,GROUPCD                                                  
         BZ    LR180                                                            
         MVI   STATUS+6,C'/'                                                    
         MVC   STATUS+7,GROUPCD                                                 
*                                                                               
LR175A10 DS    0H                                                               
         CLI   MNSTSTAT,MNSTREJ    STATUS REJECT?                               
         BE    LR175A20                                                         
         CLI   MNSTSTAT,MNSTAPP    STATUS APPROVED?                             
         BNE   LR180                                                            
*                                                                               
LR175A20 DS    0H                                                               
         TM    DLVRDFLG,GOTDLVRD   GOT DELIVERY NOTICE?                         
         BNZ   LR180               YES:SHOW CORRECT STATUS                      
         MVI   STATUS,C'*'                                                      
         MVC   STATUS+1(L'STATUS-1),1(R3)                                       
*********                                                                       
* FILTERING BY STATUS                                                           
*********                                                                       
LR180    TM    OPTFLAG,OPNSTATS    FILTER ON STATUS?                            
         BNZ   LR180A               YES                                         
         CLI   MNSTSTAT,MNSTCAN                                                 
         BE    *+12                                                             
         CLI   MNSTSTAT,MNSTOKAY                                                
         BNE   LR185                                                            
         CLC   MNSTDATE,DATEFLT1                                                
         BNL   LR185                                                            
         B     LRXRDSQ                                                          
*                                                                               
LR180A   ZIC   R1,STATLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   STATUS(0),SAVSTAT   YES, CHECK SAME STATUS                       
         BNE   LRXRDSQ                                                          
*                                                                               
LR185    MVC   SVLSTXKY,0(R4)      SAVE LAST KEY THAT PASSED OUR TESTS          
*                                                                               
         LA    R0,ORMPFLNH         IF NO MORE LINES AVAILABLE ON SCREEN         
         CR    R2,R0                                                            
         BNL   LR200               THEN EXIT                                    
*                                                                               
         MVC   MGSTATN(L'SAVSTATN),SAVSTATN                                     
         CLI   STATUS,C'*'                                                      
         BNE   *+14                                                             
         MVC   MGSTATS1(L'STATUS),STATUS                                        
         B     *+10                                                             
         MVC   MGSTATUS(L'STATUS),STATUS                                        
         GOTOR SHWORDER,DMCB,MNXKORDR-MNXKEY(R4),MGORDER                        
         MVC   MGGRPCD,MNXKGRP-MNXKEY(R4)                                       
*                                                                               
         CLI   MNSTSTAT,MNSTERR    STATUS ERROR?                                
         BNE   LR190                                                            
         MVC   MGACTDAT(8),=C'CALL DDS'                                         
         B     LR195                                                            
*                                                                               
LR190    TM    DLVRDFLG,GOTDLVRD   GOT DELIVERY NOTICE?                         
         BNO   *+8                                                              
         L     R6,ADELNOT                                                       
         GOTO1 DATCON,DMCB,(8,MNSTDATE),(11,MGACTDAT)                           
         GOTO1 HEXOUT,DMCB,MNSTTIME,WORK,L'MNSTTIME                             
         MVC   MGACTDAT+9(2),WORK                                               
         MVI   MGACTDAT+11,C':'                                                 
         MVC   MGACTDAT+12(2),WORK+2                                            
*                                                                               
LR195    DS    0H                                                               
         L     R6,AIO                                                           
         GOTOR MKGDCLR,DMCB,(R6)                                                
         MVC   MGCOLOR,BYTE                                                     
*                                                                               
LR195A   LH    R1,DKEYTABL         R1 = A(KEY TABLE ENTRY)                      
         LA    R0,KEYTABLE                                                      
         AR    R1,R0                                                            
         USING KYTABLED,R1                                                      
         MVC   KTBLKEY,0(R4)       SAVE THE KEY INTO THE TABLE ENTRY            
         DROP  R1                                                               
*                                                                               
         LA    R1,KTBLLENQ(R1)                                                  
         SR    R1,R0                                                            
         STH   R1,DKEYTABL         NEW DISP TO KEY TABLE ENTRY                  
*                                                                               
         LA    R2,MKGDLNNX-MKGDLIND(R2)  R2 = A(NEXT LIST LINE)                 
         DROP  R6                                                               
*                                                                               
LRXRDSQ  MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'XSPDIR',BIGKEY,      +        
               BIGKEY                                                           
*                                                                               
         BRAS  RE,BMPIOCNT         COUNT I/O'S                                  
         BNE   LRMAXIO                                                          
*                                                                               
LRXRDSQ1 LA    R4,BIGKEY           RE-ESTABLISH KEY POINTER                     
         B     LR140                                                            
***************                                                                 
* SECOND PASS - NEED TO DISPLAY THE CLIENT,PRD(S),EST, & STATION                
***************                                                                 
LR200    NI    DMINBTS,X'FF'-X'08'   DON'T NEED DELETED RECS NOW                
         LA    R4,KEYTABLE                                                      
         USING KYTABLED,R4                                                      
         LA    R2,ORMSEL1H                                                      
         LA    R3,ORMPFLNH                                                      
*                                                                               
         L     R6,AIO              SO WE KNOW FIRST TIME IN 2ND PASS            
         XC    0(L'DOKEY,R6),0(R6)                                              
*                                                                               
LR210    OC    0(KTBLLENQ,R4),0(R4)   HIT END OF THE TABLE?                     
         BZ    LR200X                 YES                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DOKEY,R6                                                         
         MVI   DOKTYPE,DOKTYPQ                                                  
         MVI   DOKSUBTY,DOKSTYPQ                                                
*                                                                               
         LA    RE,KTBLKEY                                                       
         USING MNKEY,RE                                                         
         OC    MNXKSPAR,MNXKSPAR      CABLE MAKEGOOD?                           
         BZ    LR213                  YES                                       
         MVC   DOKAGMD,MNKAGMD                                                  
         MVC   DOKORDER,MNKORDER                                                
         B     LR216                                                            
*                                                                               
LR213    MVC   DOKAGMD,MNXKAGMD       AGMD AND ORDER # HAS DIFF                 
         MVC   DOKORDER,MNXKORDR       DISPL THAN BROADCAST                     
         DROP  RE                                                               
*                                                                               
LR216    L     R1,AIO                                                           
         CLC   DOKEY(DOKSTA-DOKEY),0(R1)       SAME ORDER?                      
         BE    LR230                           YES                              
*                                                                               
LR220    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                       WE DON'T WANT OLD CALLS               
*                                                                               
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE   DON'T CARE ABOUT STATION             
         BNE   LR221                                                            
         CLI   DOKCMT,0                    NOT A COMMENT                        
         BE    LR222               ORDER RECORD NOT FOUND                       
LR221    OI    MISCFLG2,MF2RCNTF                                                
         MVC   MGACTDAT(8),=C'CALL DDS'                                         
         B     LR250                                                            
*                                                                               
LR222    MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
LR230    L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,DOIDELQ      ALL ORDER HAD BETTER HAVE THIS               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING DOIDELD,R6                                                       
         OC    MNXKSPAR-MNKEY(L'MNXKSPAR,R4),MNXKSPAR-MNKEY(R4)                 
         BNZ   LR230M                                                           
         MVC   MGBUYER,DOIDBYR                                                  
         B     LR230X                                                           
*                                                                               
LR230M   CLC   DOIDBYR,MNKBYR-MNKEY(R4)                                         
         BE    LR230X                                                           
********                                                                        
****  SOME ERROR CODE NEEDED HERE TO SAY BUYER IS NOT THE SAME                  
********                                                                        
*                                                                               
LR230X   GOTO1 CLUNPK,DMCB,DOIDCLT,MGCLIENT   SHOW THE CLIENT                   
         OC    MGCLIENT,SPACES                                                  
         MVC   MGPRDCTS(2),DOIDPRD                                              
         EDIT  (B1,DOIDEST),(3,MGESTMTE),FILL=0                                 
*                                                                               
         CLI   DOIDFLTN,0                          ANY FLIGHT NUMBER?           
         BE    LR235                                                            
         EDIT  (B1,DOIDFLTN),(2,MGFLIGHT),FILL=0   SHOW IT THEN                 
*                                                                               
LR235    MVC   MGSTATN(L'SAVSTATN),SAVSTATN                                     
         TM    OPTFLAG,OPNSTATN    FILTERED ON STATION?                         
         BO    LR240               YES                                          
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,MGSTATN                                  
*                                                                               
LR240    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LR250                                                            
         USING DOSPELD,R6                                                       
         TM    DOSPFLG1,DOSPTRDE   TRADE ORDER?                                 
         BZ    *+12                                                             
         MVI   MGCSHTRD,C'T'                                                    
         B     LR250                                                            
*                                                                               
         CLI   DOSPTMTH,0          THIS IS CASH, ANY METHOD FOR TRADE?          
         BE    LR250                                                            
         MVI   MGCSHTRD,C'C'                                                    
         DROP  R6                                                               
*                                                                               
LR250    LA    R4,KTBLLENQ(R4)     NEXT ENTRY                                   
         LA    R2,MKGDLNNX-MKGDLIND(R2)   R2 = A(NEXT LIST LINE)                
         CR    R2,R3                                                            
         BL    LR210                                                            
*                                                                               
LR200X   DS    0H                  END OF 2ND PASS                              
***************                                                                 
* THIRD PASS - RESOLVE THE EBCDIC PRODUCTS                                      
***************                                                                 
LR300    LA    R4,KEYTABLE                                                      
         LA    R2,ORMSEL1H                                                      
*                                                                               
         XC    QCLT,QCLT           SO WE KNOW FIRST TIME IN 3RD PASS            
*                                                                               
LR310    OC    0(KTBLLENQ,R4),0(R4)   HIT END OF THE TABLE?                     
         BZ    LR300X                 YES                                       
*                                                                               
         OC    MGCLIENT,MGCLIENT   DID WE FIND THE ORDER?                       
         BZ    LR340               NO                                           
         CLC   MGCLIENT,QCLT                                                    
         BE    LR330                                                            
*                                                                               
         L     R1,ATIOB            SET CURSOR SO ERROR POINTS TO THE            
         USING TIOBD,R1               CLIENT                                    
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVI   TIOBCURI,MGCLIENT-MGSTATUS                                       
         DROP  R1                                                               
*                                                                               
         MVC   FAKEFLD(3),MGCLIENT    SET FAKEFLDH SO IT SIMULATES A            
         MVI   FAKEFLDH+5,3              CLIENT FIELD                           
         CLI   FAKEFLD+2,C' '                                                   
         BNE   *+8                                                              
         MVI   FAKEFLDH+5,2                                                     
*                                                                               
         LR    R0,R2                                                            
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALICLT                                                          
         LR    R2,R0                                                            
         MVC   QCLT,MGCLIENT                                                    
*                                                                               
LR330    MVC   BPRD,MGPRDCTS                                                    
         MVC   BPRD2,MGPRDCTS+1                                                 
*                                                                               
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVC   KTBLFLG1,CPROF+0    COPY THE BARND/POL TRNDS FROM CPROF          
         GOTO1 CLUNPK,DMCB,(CPROF+6,BCLT),QCLT                                  
         MVC   MGCLIENT,QCLT                                                    
         DROP  R6                                                               
*                                                                               
         BRAS  RE,SHWPRDCT                                                      
         MVC   MGPRDCTS(L'QPRD),QPRD                                            
         CLI   BPRD2,0                                                          
         BE    LR340                                                            
         MVI   MGPRDCTS+L'QPRD,C'-'                                             
         MVC   BPRD,BPRD2                                                       
         BRAS  RE,SHWPRDCT                                                      
         MVC   MGPRDCTS+L'QPRD+1(L'QPRD),QPRD                                   
*                                                                               
LR340    LA    R4,KTBLLENQ(R4)     NEXT ENTRY                                   
         LA    R2,MKGDLNNX-MKGDLIND(R2)   R2 = A(NEXT LIST LINE)                
         CR    R2,R3                                                            
         BL    LR310                                                            
*                                                                               
LR300X   L     R1,ATIOB            DON'T NEED TO SET CURSOR FOR ERROR           
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                                                               
***************                                                                 
* FOURTH PASS - SEE IF POL ESTIMATE (REALLY FOR THE BUY PROGRAM)                
***************                                                                 
LR400    LA    R4,KEYTABLE                                                      
         LA    R2,ORMSEL1H                                                      
*                                                                               
LR410    OC    0(KTBLLENQ,R4),0(R4)   HIT END OF THE TABLE?                     
         BZ    LR400X                 YES                                       
*                                                                               
         CLI   KTBLFLG1,C'0'       BRAND POOL?                                  
         MVI   KTBLFLG1,0          CLEAR IT ANYWAY                              
         BNE   LR420X              YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ESTHDRD,R3                                                       
         MVC   EKEYAM,BAGYMD                                                    
         GOTO1 CLPACK,DMCB,MGCLIENT,EKEYCLT                                     
         MVC   EKEYPRD,=C'POL'                                                  
         PACK  DUB,MGESTMTE                                                     
         CVB   R1,DUB                                                           
         STC   R1,EKEYEST                                                       
         DROP  R3                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BNE   *+8                                                              
         OI    KTBLFLG1,KTF1POLE   WE GOT A POL ESTIMATE                        
*                                                                               
LR420X   LA    R4,KTBLLENQ(R4)     NEXT ENTRY                                   
         LA    R2,MKGDLNNX-MKGDLIND(R2)   R2 = A(NEXT LIST LINE)                
         LA    R0,ORMPFLNH                                                      
         CR    R2,R0                                                            
         BL    LR410                                                            
*                                                                               
LR400X   DS    0H                  END OF 4TH PASS                              
*                                                                               
         DROP  R2,R4                                                            
*                                                                               
         OI    GENSTAT2,DISTHSPG   SET DISPLAY SAME PAGE ON RETURN              
LRXNTFD  TM    MISCFLG2,MF2RCNTF   WAS ALL THE ORDER RECORDS FOUND?             
         BO    RECNTFD             NO, EXIT WITH ERROR                          
         TM    MISCFLG1,MF1FIRST   NEED 'HIT ENTER FOR FIRST'?                  
         BNZ   FORFIRST            YES                                          
         LA    R4,KEYTABLE                                                      
         AHI   R4,KYTBLLNQ-KTBLLENQ   R4 = VERY LAST ENTRY IN TABLE             
         OC    0(KTBLLENQ,R4),0(R4)                                             
         BNZ   FORNEXT                                                          
         B     TOOMNYIO            MAX NUMBER OF I/O'S EXCEEDED                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
RECNTFD  MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
FORFIRST MVI   GERROR1,ENDOFLST                                                 
         B     INFCURSR                                                         
*                                                                               
FORNEXT  MVI   GERROR1,LSTDISPL                                                 
*                                                                               
INFCURSR L     R1,ATIOB            SET UP ERROR MESSAGE                         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LA    R0,ORMMEDH                                                       
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
         MVI   TIOBCURI,0                                                       
         DROP  R1                                                               
*                                                                               
         B     INFEXIT                                                          
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
AGYDACT  LHI   RF,1328             NOT ALL SPOTS ALLOCATED                      
         STCM  RF,3,GERROR                                                      
         MVI   GETMSYS,2           SPOT MESSAGES                                
         B     ERREXIT                                                          
*                                                                               
***********                                                                     
INVLBYR  MVI   GERROR1,NOASSBYR    ASSISTANT BUYER NOT FOUND                    
         B     ERREXIT                                                          
*                                                                               
CANTSWTC MVI   GERROR1,NOBUYPRG    CAN'T SWITCH TO THE BUY PROGRAM              
         B     ERREXIT                                                          
*                                                                               
MGCANREP MVI   GERROR1,MKGCANRP    MAKEGOOD HAS BEEN CANCELLED BY REP           
         B     ERREXIT                                                          
*                                                                               
MGCANMOR MVI   GERROR1,MKGCANMR    MAKEGOOD CANCELLED MORE TO FOLLOW            
         B     ERREXIT                                                          
*                                                                               
VRSELELK MVI   GERROR1,ESTLCKED    ESTIMATE IS LOCKED                           
         B     ERREXIT                                                          
*                                                                               
TOOMNYIO MVI   GERROR1,MAXNMIOS    MAXIMUM NUMBER OF I/O'S EXCEEDED             
         L     R1,ATIOB            SOUND AN ALARM                               
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBALRM                                                
         DROP  R1                                                               
         B     MYINFXIT                                                         
*                                                                               
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                        MG PF TABLE                                  *         
***********************************************************************         
MPFTABLE DS    0C                                                               
*                                                                               
         DC    AL1(MPF02X-*,02,0,0,0,PFTRETRN)                                  
         DC    CL3'S',CL8' ',CL8' '                                             
MPF02X   EQU   *                                                                
*                                                                               
         DC    AL1(MPF04X-*,04,0,0,0,PFTRETRN)                                  
         DC    CL3'L',CL8' ',CL8' '                                             
MPF04X   EQU   *                                                                
*                                                                               
         DC    AL1(MPF25X-*,25,0,0,0,PFTRETRN)                                  
         DC    CL3'A',CL8' ',CL8' '                                             
MPF25X   EQU   *                                                                
*                                                                               
         DC    AL1(MPF26X-*,26,0,0,0,PFTRETRN)                                  
         DC    CL3'B',CL8' ',CL8' '                                             
MPF26X   EQU   *                                                                
*                                                                               
         DC    AL1(MPF27X-*,27,0,0,0,PFTRETRN)                                  
         DC    CL3'C',CL8' ',CL8' '                                             
MPF27X   EQU   *                                                                
*                                                                               
* PF12 = RETURN CALLER                                                          
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(MPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
MPF12X   EQU   *                                                                
*                                                                               
* ACTUAL MGSTATUS                                                               
*                                                                               
MPF14A   DC    AL1(MPF14X-*,14,PFTCPROG,0,(MPF14X-MPF14)/KEYLNQ,0)              
         DC    CL3' ',CL8'OFFER',CL8'STATUS'                                    
MPF14    DC    AL1(KEYTYTWA,L'ORMMED-1),AL2(ORMMED-T234FFD)                     
         DC    AL1(KEYTYCUR,L'MGBUYER-1),AL2(MGBUYER-MGSTATS1)                  
         DC    AL1(KEYTYCUR,L'MGORDER-1),AL2(MGORDER-MGSTATS1)                  
         DC    AL1(KEYTYCUR,L'MGGRPCD-1),AL2(MGGRPCD-MGSTATS1)                  
MPF14X   EQU   *                                                                
*                                                                               
* ACTUAL ORDER LIST                                                             
*                                                                               
         DC    AL1(MPF16X-*,16,PFTCPROG,0,(MPF16X-MPF16)/KEYLNQ,0)              
         DC    CL3' ',CL8'ORDER',CL8'LIST'                                      
MPF16    DC    AL1(KEYTYTWA,L'ORMMED-1),AL2(ORMMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'ORMBUYR-1),AL2(ORMBUYR-T234FFD)                   
MPF16X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 5                                                                
***********************************************************************         
*                       MGSTATUS PFTABLE                              *         
***********************************************************************         
SPFTABLE DS    0C                                                               
* PF12 = RETURN CALLER                                                          
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(SPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF12X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
STATABLE DS    0H                                                               
         DC    AL1(MNSTNEW),CL9'NEW      '                                      
         DC    AL1(MNSTAPP),CL9'APPROVED '                                      
         DC    AL1(MNSTREJ),CL9'REJECTED '                                      
         DC    AL1(MNSTERR),CL9'ERROR    '                                      
         DC    AL1(MNSTCAN),CL9'CANCELLED'                                      
         DC    AL1(MNSTOKAY),CL9'OKAYED   '                                     
         DC    AL1(MNSTAMND),CL9'AMENDED  '                                     
         DC    AL1(MNSTCANM),CL9'SELRCL   '                                     
         DC    AL1(MNSTGOIN),CL9'*OKAYED  '                                     
         DC    AL1(MNSTHOLD),CL9'ON HOLD'                                       
         DC    AL1(MNSTDELV),CL9'DELIVERED'                                     
         DC    AL1(MNSTSAPP),CL9'SELF-APP '                                     
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* COLOR TABLES                                                                  
***********************************************************************         
MGBLKTBL DS    0H                                                               
         DC    AL1(MNSTOKAY)                                                    
         DC    AL1(MNSTCAN)                                                     
         DC    X'FF'                                                            
*                                                                               
MGREDTBL DS    0H                                                               
         DC    AL1(MNSTCANM)                                                    
         DC    AL1(MNSTSAPP)                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       NTR1  BASE=*,LABEL=*                                                   
         NI    MISCFLG1,X'FF'-MF1KYCHG-MF1FIRST-MF1BACK                         
*                                                                               
         TM    GOTGLOB,GGLBDAR     COMING BACK FROM BUY?                        
         BZ    VKMED00                                                          
         OI    MISCFLG1,MF1BACK    YES                                          
         MVC   SVSELSID,TMPSLSID                                                
*********                                                                       
* MEDIA                                                                         
*********                                                                       
VKMED00  OC    ORMMED,SPACES                                                    
         LA    R2,ORMMEDH          VALIDATE MEDIA                               
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
*                                                                               
VKMEDX   OI    4(R2),X'20'                                                      
*********                                                                       
* BUYER ID                                                                      
*********                                                                       
VKBYR00  LA    R2,ORMBUYRH         VALIDATE BUYER ID                            
         OC    ORMBUYR,SPACES                                                   
         CLC   SVBUYRCD,8(R2)                                                   
         BE    *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
         MVC   SVBUYRCD,8(R2)                                                   
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKBYRX                                                           
         CLI   ACTEQU,ACTSTATS     REQUIRED FOR MGSTATUS SCREEN                 
         BE    MISSFLD                                                          
VKBYRX   DS    0H                                                               
*********                                                                       
* ORDER NUMBER                                                                  
*********                                                                       
VKODR00  LA    R2,ORMORDRH         VALIDATE ORDER NUMBER                        
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   ACTEQU,ACTSTATS     REQUIRED FOR MGSTATUS SCREEN                 
         BNE   VKODRX                                                           
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   5(R2),8             YJJJSSSS - YEAR,JULIAN,SEQ                   
         BNE   INVLFLD                                                          
         TM    4(R2),X'08'         HAS TO BE NUMERIC?                           
         BZ    INVLFLD                                                          
         GOTOR BINORDR,DMCB,8(R2)  GET BINARY ORDER NUMBER                      
*                                                                               
VKODRX   OI    4(R2),X'20'                                                      
*********                                                                       
* MAKEGOOD GROUP CODE                                                           
*********                                                                       
VKGRP00  LA    R2,ORMGRPH          VALIDATE MAKEGOOD GROUP CODE                 
         OC    ORMGRP,SPACES                                                    
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   ACTEQU,ACTSTATS     REQUIRED FOR MGSTATUS SCREEN                 
         BNE   VKGRPX                                                           
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         OI    4(R2),X'20'                                                      
         BRAS  RE,MGSTA                                                         
         B     XIT                                                              
*                                                                               
VKGRPX   OI    4(R2),X'20'                                                      
*********                                                                       
* OPTION                                                                        
*********                                                                       
VKOPN00  LA    R2,ORMOPTNH         VALIDATE OPTION                              
         TM    4(R2),X'20'                                                      
         BNZ   VKOPNX                                                           
         NI    OPTFLAG,X'FF'-OPNSTATS-OPNSTATN                                  
         CLI   5(R2),0                                                          
         BE    VKOPN40                                                          
                                                                                
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),BLOCK                                          
         ZICM  R4,4(R1),1          NUMBER OF OPTIONS ENTERED                    
         BZ    INVLFLD                                                          
                                                                                
         LA    R3,BLOCK                                                         
VKOPN10  CLI   1(R3),0             LENGTH OF SECOND HALF OF FIELD               
         BNE   VKOPN20                                                          
         ZICM  RE,0(R3)            LENGTH OF INPUT FIELD                        
         BCTR  RE,0                -1                                           
         LA    R1,STATABLE         A(STATUS TABLE)                              
*                                                                               
VKOPN12  CLI   0(R1),X'FF'                                                      
         BE    VKOPN14             CHECK *APPROVED/*REJECTED                    
         EX    RE,*+8                                                           
         BE    VKOPN16                                                          
         CLC   12(0,R3),1(R1)                                                   
         LA    R1,10(R1)                                                        
         B     VKOPN12                                                          
*                                                                               
VKOPN14  DS    0H                                                               
         EX    RE,*+8                                                           
         BE    VKOPN16                                                          
         CLC   12(0,R3),=C'*APPROVED'                                           
         EX    RE,*+8                                                           
         BNE   INVLFLD                                                          
         CLC   12(0,R3),=C'*REJECTED'                                           
*                                                                               
VKOPN16  TM    OPTFLAG,OPNSTATS    MAKE SURE ONLY ONE STATUS FILTER             
         BO    INVLFLD                                                          
         OI    OPTFLAG,OPNSTATS                                                 
         MVC   SAVSTAT,12(R3)      SAVE THE FILTER STATUS                       
         MVC   STATLEN,0(R3)       SAVE FILTER STATUS LENGTH                    
         B     VKOPN30                                                          
                                                                                
VKOPN20  CLC   =C'STA',12(R3)      OTHERWISE, MUST BE A STATION                 
         BNE   INVLFLD                                                          
         TM    OPTFLAG,OPNSTATN    MAKE SURE ONLY 1 STATION FILTER              
         BO    INVLFLD                                                          
         OI    OPTFLAG,OPNSTATN                                                 
         MVC   SAVSTATN,22(R3)     SAVE THE STATION                             
                                                                                
VKOPN30  LA    R3,32(R3)                                                        
         BCT   R4,VKOPN10                                                       
                                                                                
VKOPN40  OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
VKOPNX   OI    4(R2),X'20'                                                      
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAYS THE ORDER NUMBER IN 8 BYTE EBCDIC FORM                               
*                                                                               
* ON ENTRY:    PARAM 1             A(ORDER NUMBER AS STORED IN RECORD)          
*              PARAM 2             A(WHERE TO DISPLAY THE ORDER NUMBER)         
***********************************************************************         
SHWORDER NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)                                                         
         L     R2,4(R1)                                                         
*                                                                               
         MVC   FULL,0(R3)          SHOW THE ORDER NUMBER                        
         XC    FULL,=4X'FF'                                                     
*                                                                               
         TM    FULL,X'80'                 NEW STYLE?                            
         BNZ   SHWO20                                                           
*                                                                               
         L     R1,FULL                                                          
         AHI   R1,1                                                             
         ZICM  R1,FULL,2                                                        
         CVD   R1,DUB                                                           
         UNPK  0(4,R2),DUB                                                      
         OI    3(R2),X'F0'                                                      
*                                                                               
         ZICM  R3,FULL+2,2                                                      
         EDIT  (R3),(4,4(R2)),0,FILL=0    SEQUENCE NUMBER                       
         B     SHWORDX                                                          
*                                                                               
SHWO20   NI    FULL,X'FF'-X'80'                                                 
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         AP    DUB,=P'04000000'                                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  0(8,R2),DUB                                                      
*                                                                               
SHWORDX  B     XIT                                                              
         SPACE 5                                                                
***********************************************************************         
* CONVERTS EBCDIC ORDER NUMBER TO BINARY ORDER NUMBER                           
*                                                                               
* ON ENTRY:    PARAM 1             A(EBCDIC ORDER NUMBER)                       
* ON EXIT:     BINORDER            BINARY ORDER NUMBER                          
***********************************************************************         
BINORDR  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
*                                                                               
         CLI   1(R2),C'3'                                                       
         BNH   BNORDR10                                                         
         PACK  DUB,0(8,R2)         NEW STYLE ORDER NUMBER                       
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,BINORDER                                                   
         OI    BINORDER,X'80'                                                   
         XC    BINORDER,=4X'FF'                                                 
         J     XIT                                                              
*                                                                               
BNORDR10 GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE               
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,(R2),FAKEFLD,8   SAVE AS IF ENTRY WAS HEX             
         MVC   PACKOF4B,FAKEFLD    CONVERT IT TO PACK                           
         OI    PACKOF4B+3,X'0F'                                                 
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),FAKEFLD   STICK IN DAYS IN YEAR                    
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=X'FFFF'                                                
*                                                                               
         PACK  DUB,4(4,R2)         SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=X'FFFF'                                                
         B     XIT                                                              
***********************************************************************         
* DISPLAYS THE EBCDIC PRODUCT FOR THE CLIENT THAT IS IN AIO                     
*                                                                               
* ON ENTRY:    BPRD                BINARY PRODUCT CODE                          
*                                                                               
* ON EXIT:     (CC)                CONDITION CODE                               
*              QPRD                EBCDIC PRODUCT CODE                          
***********************************************************************         
SHWPRDCT NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         LA    R4,CLIST                                                         
         DROP  R6                                                               
*                                                                               
SPRDLP   CLI   0(R4),0                                                          
         BNE   *+8                                                              
         J     NO                  RETURN 'NO' TO CALLER IF EOT                 
         CLC   BPRD,3(R4)                                                       
         BE    SPRDYES                                                          
         LA    R4,4(R4)                                                         
         B     SPRDLP                                                           
*                                                                               
SPRDYES  MVC   QPRD,0(R4)          GOT THE EBCDIC PRODUCT CODE                  
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RETURNS THE COLOR IN BYTE                                                     
* ON ENTRY: PARAM1 = A(DARE MG NOTICE RECORD)                                   
*                                                                               
* ON EXIT : BYTE IS SET TO THE COLOR                                            
*                                                                               
***********************************************************************         
MKGDCLR  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,C'G'           DEFAULT GREEN                                
         L     RE,DMCB                                                          
         LA    R6,MNRFRST-MNKEY(RE)                                             
         OC    MNXKSPAR-MNXKEY(L'MNXKSPAR,RE),MNXKSPAR-MNXKEY(RE)               
         BNZ   MGCLR005               NOT CABLE                                 
         LA    R6,MNXFRST-MNXKEY(RE)                                            
MGCLR005 CLI   0(R6),MNSTELQ       MUST HAVE STATUS                             
         JNE   *+2                                                              
         MVI   ELCODE,MNSTELQ                                                   
         USING MNSTELD,R6                                                       
MGCLR010 CLI   MNSTSTAT,MNSTDELV                                                
         BNE   MGCLR020                                                         
         OI    DLVRDFLG,GOTDLVRD   GOT DELIVERY NOTICE                          
         BRAS  RE,NEXTEL                                                        
         BE    MGCLR010                                                         
         DC    H'0'                                                             
*                                                                               
MGCLR020 LA    R1,MGBLKTBL                                                      
MGCLR030 CLI   0(R1),X'FF'                                                      
         BE    MGCLR050                                                         
         CLC   MNSTSTAT,0(R1)                                                   
         BE    MGCLR040                                                         
         AHI   R1,1                                                             
         B     MGCLR030                                                         
*                                                                               
MGCLR040 DS    0H                                                               
         MVI   BYTE,C'K'                                                        
         B     MGCLRXIT                                                         
*                                                                               
MGCLR050 DS    0H                                                               
         CLI   MNSTSTAT,MNSTAPP                                                 
         BE    *+12                                                             
         CLI   MNSTSTAT,MNSTREJ                                                 
         BNE   MGCLR060                                                         
         TM    DLVRDFLG,GOTDLVRD   GOT DELIVERY NOTICE                          
         BNZ   MGCLR080                                                         
         B     MGCLRXIT                                                         
*                                                                               
MGCLR060 LA    R1,MGREDTBL                                                      
MGCLR070 DS    0H                                                               
         CLI   0(R1),X'FF'                                                      
         BE    MGCLRXIT                                                         
         CLC   MNSTSTAT,0(R1)                                                   
         BE    MGCLR080                                                         
         AHI   R1,1                                                             
         B     MGCLR070                                                         
*                                                                               
MGCLR080 DS    0H                                                               
         MVI   BYTE,C'R'                                                        
*                                                                               
MGCLRXIT B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUMP THE I/O COUNTER AND CHECK                                                
***********************************************************************         
BMPIOCNT NTR1  BASE=*,LABEL=*                                                   
         TM    MISCFLG1,MF1CNTIO   DO WE NEED TO COUNT I/O'S                    
         JZ    BMPIOYES                                                         
*                                                                               
         LH    RF,IO_COUNT                                                      
         CHI   RF,300              CHECK EVERY 300 I/O'S                        
         JL    BMPIO20                                                          
*                                                                               
         GOTO1 GETFACT,DMCB,(X'80',0),F#SSBD                                    
         L     R1,0(R1)                                                         
         USING F@SSBD,R1                                                        
         MVC   FULL(2),F@SMAXIO        STANDARD MAX I/O'S                       
         DROP  R1                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,(X'80',0),F#TCBD                                    
         L     R1,0(R1)                                                         
         USING F@TCBD,R1                                                        
         LH    RF,FULL                                                          
         SHI   RF,2500             GIVE OURSELVES SOME ROOM                     
         CLM   RF,7,F@BIOCNA       DID WE SURPASS STANDARD MAX I/O'S?           
         JH    BMPIO19             NO, CONTINUE UNTIL NEXT 300 I/O'S            
         MVC   IO_COUNT,=X'FFFF'   KILL THIS NUMBER                             
         J     BMPIONO             SURPASSED THE SAFE POINT                     
         DROP  R1                                                               
*                                                                               
BMPIO19  SR    RF,RF               RESET SO WE CAN COUNT FOR 300 AGAIN          
BMPIO20  AHI   RF,1                                                             
         STH   RF,IO_COUNT                                                      
*                                                                               
BMPIOYES J     YES                                                              
*                                                                               
BMPIONO  J     NO                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET THE MFKEY INFORMATION                                                     
***********************************************************************         
SETMFKYS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,MPFTABLE         YES, USE LIST MPFKEY TABLE                   
         OI    CTLRFLG1,CF1CKLST                                                
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
***************                                                                 
* SETUP THE MFKEY LINE                                                          
***************                                                                 
         XC    ORMPFLN,ORMPFLN                                                  
         MVC   ORMPFLN(23),=C'PF2=MGstatus  4=OrdList'                          
         OI    ORMPFLNH+6,X'80'                                                 
*                                                                               
         TM    GOTGLOB,GGLBDAR     DID I CALL GLOBBER?                          
         BZ    SETMF05                                                          
         MVC   SVSELSID,TMPSLSID                                                
*                                                                               
SETMF05  CLI   CALLSP,0            RETURN TO ORDER LIST SCREEN?                 
         BE    SETMF10             NO                                           
         MVC   ORMPFLN+25(9),=C'12=RETURN'                                      
*                                                                               
         CLI   PFKEY,0                                                          
         BE    STMFX                                                            
         CLI   PFKEY,12                                                         
         BE    STMFX                                                            
         CLI   PFKEY,4                                                          
         BNE   SETMF10                                                          
         MVI   PFKEY,12                                                         
         B     STMFX                                                            
*                                                                               
SETMF10  DS    0H                                                               
         CLI   PFKEY,0             PFKEYING SOMEWHERE?                          
         BE    STMFX                                                            
         CLI   PFKEY,24            NEW SELECT CODES W/O PFKEYS                  
         BH    STMFX                                                            
*                                                                               
         MVC   SVLSTKEY,KEYTABLE                                                
*                                                                               
         ZIC   R0,PFKEY                                                         
         AHI   R0,12                                                            
         STC   R0,PFKEY                                                         
*                                                                               
         CLI   PFKEY,14                                                         
         BNE   STMF45                                                           
         LA    R1,MPF14A                                                        
         USING PFTABD,R1                                                        
         OI    PFTSTAT,PFTCPROG    DEFAULT TO SAVE TWA & WRKING STO             
         CLI   CALLSP,0            COME FROM SOMEWHERE?                         
         BE    STMF45              NO, GO AHEAD                                 
         CLI   CALLSTCK,X'E9'      YES, COME FROM ORDER LIST?                   
         BNE   STMF45                   NO, IGNORE WHERE WE CAME FROM           
         NI    PFTSTAT,X'FF'-PFTCPROG  DON'T SAVE SO RETURN TO LIST             
         DROP  R1                                                               
*                                                                               
STMF45   LA    R2,MPFTABLE         YES, USE LIST MPFKEY TABLE                   
         OI    CTLRFLG1,CF1CKLST+CF1TSELQ                                       
*                                  DON'T TEST THE SEL CODES IN TESTSEL          
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
STMFX    B     XIT                                                              
***********************************************************************         
* SHOW RECORD MGSTATUS HISTORY                                                  
***********************************************************************         
MGSTA    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    MISCFLG1,MF1KYCHG                                                
         BZ    *+10                                                             
         XC    RECLENB4,RECLENB4                                                
*                                                                               
         NI    RECFLAG,X'FF'-REJFND                                             
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    MGSTA10             NO                                           
*                                                                               
         MVC   ORSPFLN(11),=C'PF12=RETURN'                                      
         OI    ORSPFLNH+6,X'80'                                                 
*                                                                               
*****    CLI   TWAOFFC,C'*'        ONLY DDS TERMINALS USE $OM NOW               
*****    BNE   MGSTA10                                                          
MGSTA10  BRAS  RE,SHOWDAS          SHOW DISK ADDRESSES ON PFKEY LINE            
*                                                                               
         LA    R2,ORSMEDH                                                       
         OC    DISKADDR,DISKADDR   VALID D/A SET BY SHOWDAS?                    
         BZ    RECNTFD             NO, THEN WE DID NOT FIND OUR RECORD          
*                                                                               
         MVI   ELCODE,MNSTELQ      AT LEAST ONE STATUS ELEMENT                  
         OC    ELEMDISP,ELEMDISP   START FROM THE BEGINNING?                    
         BZ    MGSTA20                                                          
         L     R6,AIO                                                           
         USING DAREMGND,R6                                                      
         CLC   RECLENB4,MNRLEN     IF RECORD LENGTHS DON'T MATCH                
         BNE   MGSTA25             WE COULD BE POINTING ANYWHERE                
         L     R6,AIO                                                           
         AH    R6,ELEMDISP         POINT TO WHERE WE'RE SUPPOSED TO BE          
         B     MGSTA30                                                          
*                                                                               
MGSTA20  XC    ELEMDISP,ELEMDISP   SO WE'LL START FROM BEGINNING AGAIN          
*                                                                               
         L     R6,AIO                                                           
         USING MNSTELD,R6                                                       
         TM    MISCFLG2,MF2CBLOR   CABLE ORDER?                                 
         BNZ   MGSTA25                                                          
         BRAS  RE,GETEL                                                         
         BE    MGSTA30                                                          
         DC    H'0'                                                             
*                                                                               
MGSTA25  AHI   R6,MNXFRST-MNXKEY   CAN'T USE GETEL, BUT WE CAN USE              
         BRAS  RE,FIRSTEL            FIRSTEL                                    
         BE    MGSTA30                                                          
         DC    H'0'                                                             
*                                                                               
MGSTA30  BRAS  RE,CLRSCRN          CLEAR THE SCREEN                             
*                                                                               
         LA    R2,ORSMGSFH         START DISPLAY AT 1ST LINE                    
         USING MGSTAD,R2                                                        
*                                                                               
MGSTA40  DS    0H                  CHECK TABLE TO EXPAND STATUS                 
         LA    R1,ORSMGSLH                                                      
         LA    R3,STATABLE                                                      
         CR    R2,R1               ANY MORE LINES?                              
         BNH   MGSTA50             YES                                          
MGSTA45  L     R3,AIO                                                           
         SR    R6,R3                                                            
         STH   R6,ELEMDISP                                                      
         MVC   RECLENB4,MNRLEN-MNKEY(R3)                                        
         B     MGSTAX                                                           
*                                                                               
MGSTA50  CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                DIE: STATUS NOT DEFINED IN TABLE             
         CLC   0(1,R3),MNSTSTAT                                                 
         BE    MGS50A10                                                         
         LA    R3,10(R3)                                                        
         B     MGSTA50                                                          
*                                                                               
MGS50A10 DS    0H                                                               
         CLI   MNSTSTAT,MNSTDELV                                                
         BNE   MGS50A30                                                         
         LR    R1,R6                                                            
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   MNSTSTAT,MNSTAPP                                                 
         BNE   MGS50A20                                                         
         MVC   MGSTSTAT(17),=CL17'APPROVE DELIVERED'                            
         LR    R6,R1                                                            
         B     MGS50A60                                                         
*                                                                               
MGS50A20 DS    0H                                                               
         CLI   MNSTSTAT,MNSTREJ                                                 
         BNE   MGS50A25                                                         
         MVC   MGSTSTAT(16),=CL16'REJECT DELIVERED'                             
         LR    R6,R1                                                            
         B     MGS50A60                                                         
MGS50A25 LR    R6,R1                                                            
*                                                                               
MGS50A30 DS    0H                                                               
         CLI   MNSTSTAT,MNSTGOIN                                                
         BNE   MGS50A40                                                         
         MVC   MGSTSTAT(16),=CL16'OKAYED BY SELLER'                             
         B     MGS50A60                                                         
*                                                                               
MGS50A40 DS    0H                                                               
         CLI   MNSTSTAT,MNSTOKAY                                                
         BNE   MGS50A50                                                         
         MVC   MGSTSTAT(13),=CL13'OKAY APPLIED'                                 
         B     MGS50A60                                                         
*                                                                               
MGS50A50 MVC   MGSTSTAT,1(R3)                                                   
*                                                                               
MGS50A60 GOTO1 DATCON,DMCB,(8,MNSTDATE),(11,MGSTDATE)                           
*                                                                               
         GOTO1 HEXOUT,DMCB,MNSTTIME,WORK,L'MNSTTIME                             
         MVC   MGSTTIME(2),WORK                                                 
         MVI   MGSTTIME+2,C':'                                                  
         MVC   MGSTTIME+3(2),WORK+2                                             
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   MNSTSTAT,MNSTERR    ERROR?                                       
         BNE   MGSTA80                                                          
         EDIT  MNSTERRN,SAVERROR,0,FILL=0                                       
         LA    R3,ERRORLST                                                      
MGSTA60  CLI   0(R3),0                                                          
         BE    MGSTA100                                                         
         ZIC   R1,0(R3)                                                         
         CLC   SAVERROR,1(R3)      SAME ERROR CODE?                             
         BE    MGSTA70                                                          
         AR    R3,R1                                                            
         B     MGSTA60                                                          
*                                                                               
MGSTA70  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   MGSTCMNT,=C'ERROR:  '                                            
         SHI   R1,5                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGSTCMT(0),4(R3)                                                 
         OI    6(R2),X'80'                                                      
         B     MGSTA100                                                         
*                                                                               
MGSTA80  CLI   MNSTSTAT,MNSTREJ    REJECTED?                                    
         BNE   MGSTA100                                                         
         TM    RECFLAG,REJFND      ALREADY FOUND A REJECTED STATUS              
         BO    MGSTA100                                                         
         LR    R3,R6               SAVE ADDRESS OF LAST STATUS ELEMENT          
         OI    RECFLAG,REJFND                                                   
         MVI   ELCODE,MNMRJCQ      GET REJECTION COMMENT                        
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   MGSTA90                                                          
*                                                                               
         LA    R1,ORSMGSLH                                                      
         CR    R2,R1               CAN WE DISPLAY THE COMMENT?                  
         BL    MGSTA85             YES                                          
         LR    R6,R3               NO, SHOW STATUS/COMMENT ON NEXT PAGE         
         XC    ORSMGSL,ORSMGSL                                                  
         B     MGSTA45                                                          
*                                                                               
MGSTA85  ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         USING MNMRJCD,R6                                                       
         MVC   MGSTCMNT,=C'COMMENT:'                                            
         ZIC   R1,MNMRLEN          LENGTH OF REJECTION COMMENT                  
         SHI   R1,MNMROVRH+1       GET LENGTH OF COMMENT-1                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGSTCMT(0),MNMRTEXT                                              
         OI    6(R2),X'80'                                                      
*                                                                               
MGSTA90  MVI   ELCODE,MNSTELQ      AT LEAST ONE STATUS ELEMENT                  
         LR    R6,R3               RESTORE W/ ADDRESS OF LAST STATUS EL         
*                                                                               
MGSTA100 ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BRAS  RE,NEXTEL                                                        
         BE    MGSTA40                                                          
         XC    ELEMDISP,ELEMDISP   START FROM BEGINNING ON RETURN               
*                                                                               
MGSTAX   J     XIT                                                              
         DROP  R2,R6                                                            
*                                                                               
*====================================================================*          
* CLEAR THE SCREEN                                                              
*====================================================================*          
CLRSCRN  NTR1                                                                   
         LA    R2,ORSMGSFH                                                      
         LA    R3,ORSMGSLH         LAST FIELD                                   
*                                                                               
CLSC10   CR    R2,R3               FINISHED CLEARING THE SCREEN?                
         BH    CLRSCRNX            YES                                          
*                                                                               
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         LR    R4,R1                                                            
         SHI   R1,8                SUBTRACT LENGTH OF HEADER                    
         TM    1(R2),X'02'         EXTENSION PRESENT?                           
         BNO   *+8                                                              
         SHI   R1,8                SUBTRACT LENGTH OF EXTENSION                 
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         AR    R2,R4                                                            
         B     CLSC10                                                           
*                                                                               
CLRSCRNX B     XIT                                                              
         SPACE                                                                  
*====================================================================*          
* SHOW DISK ADDRESSES FOR ORDER/NOTICE/OFFER RECORDS                            
*====================================================================*          
SHOWDAS  NTR1                                                                   
         LA    R4,ORSPFLN          POINT TO OUTPUT AREA                         
         USING DISPFLD,R4                                                       
         XC    DISKADDR,DISKADDR                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DOKEY,R6                                                         
*                                                                               
         MVI   DOKTYPE,DOKTYPQ     X'0D' - TYPE EQUATE                          
         MVI   DOKSUBTY,DOKSTYPQ   X'34' - SUBTYPE EQUATE                       
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER                                                
         GOTO1 HIGH                                                             
*                                                                               
         MVC   DDSKORDT,=C'ORD='                                                
         MVC   DDSKORDD,=C'NOTFOUND'                                            
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE   TEST THROUGH ORDER ONLY              
         BNE   SHOWDAX                                                          
         GOTO1 HEXOUT,DMCB,KEY+14,DDSKORDD,4                                    
*                                                                               
         NI    MISCFLG2,X'FF'-MF2CBLOR                                          
         CLI   DOKSTA,X'E8'        IS THIS A CABLE ORDER?                       
         BNL   SHWDA50             YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY              READ OFFER RECORD KEY                        
         USING MOKEY,R6                                                         
         MVI   MOKTYPE,MOKTYPQ     X'0D' - TYPE EQUATE                          
         MVI   MOKSUBTY,MOKSTYPQ   X'37' - SUBTYPE EQUATE                       
         MVC   MOKAGMD,BAGYMD                                                   
         MVC   MOKORDER,BINORDER                                                
         MVC   MOKMGCD,ORSGRP                                                   
         OC    MOKMGCD,SPACES                                                   
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
*                                                                               
         MVC   DDSKOFRT,=C'OFR='                                                
         MVC   DDSKOFRD,=C'NOTFOUND'                                            
         CLC   KEY(L'MOKEY),KEYSAVE                                             
         BNE   SHWDA10                                                          
         GOTO1 HEXOUT,DMCB,MOKDSKAD,DDSKOFRD,4                                  
*                                                                               
SHWDA10  XC    KEY,KEY                                                          
         LA    R6,KEY              READ NOTICE RECORD                           
         USING MNKEY,R6                                                         
         MVI   MNKTYPE,MNKTYPQ     X'0D' - TYPE EQUATE                          
         MVI   MNKSUBTY,MNKSTYPQ   X'36' - SUBTYPE EQUATE                       
         MVC   MNKAGMD,BAGYMD                                                   
         MVC   MNKBYR,ORSBUYR                                                   
         OC    MNKBYR,SPACES                                                    
         MVC   MNKORDER,BINORDER                                                
         MVC   MNKGROUP,ORSGRP                                                  
         OC    MNKGROUP,SPACES                                                  
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
*                                                                               
         MVC   DDSKNOTT,=C'NOT='                                                
         MVC   DDSKNOTD,=C'NOTFOUND'                                            
         CLC   KEY(L'MNKEY),KEYSAVE                                             
         BNE   SHOWDAX                                                          
         GOTO1 HEXOUT,DMCB,MNKDSKAD,DDSKNOTD,4                                  
         MVC   DISKADDR,MNKDSKAD                                                
*                                                                               
         GOTO1 GETREC              READ THE NOTICE RECORD                       
         B     SHOWDAX                                                          
***************                                                                 
* FOR CABLE MAKEGOODS                                                           
***************                                                                 
SHWDA50  MVC   DCABLETX,=C'*CABLE*'                                             
         XC    BIGKEY,BIGKEY                                                    
         OI    MISCFLG2,MF2CBLOR                                                
         LA    R6,BIGKEY           READ OFFER RECORD KEY                        
         USING MOXKEY,R6                                                        
         MVI   MOXKTYPE,MOXKTYPQ   X'0D' - TYPE EQUATE                          
         MVI   MOXKSBTY,MOXKSBTQ   X'37' - SUBTYPE EQUATE                       
         MVC   MOXKAGMD,BAGYMD                                                  
         MVC   MOXKORDR,BINORDER                                                
         MVC   MOXKMGCD,ORSGRP                                                  
         OC    MOXKMGCD,SPACES                                                  
*                                                                               
         MVC   BIGKEYSV,BIGKEY     PASS BACK DELETED RECORDS                    
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',BIGKEY,      +        
               BIGKEY                                                           
*                                                                               
         MVC   DDSKOFRT,=C'XSO='                                                
         MVC   DDSKOFRD,=C'NOTFOUND'                                            
         CLC   BIGKEY(L'MOXKEY),BIGKEYSV                                        
         BNE   SHWDA60                                                          
         GOTO1 HEXOUT,DMCB,MOXKDA,DDSKOFRD,4                                    
*                                                                               
SHWDA60  XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY           READ NOTICE RECORD                           
         USING MNXKEY,R6                                                        
         MVI   MNXKTYPE,MNXKTYPQ   X'0D' - TYPE EQUATE                          
         MVI   MNXKSBTY,MNXKSBTQ    X'36' - SUBTYPE EQUATE                      
         MVC   MNXKAGMD,BAGYMD                                                  
         MVC   MNXKORDR,BINORDER                                                
         MVC   MNXKGRP,ORSGRP                                                   
         OC    MNXKGRP,SPACES                                                   
*                                                                               
         MVC   BIGKEYSV,BIGKEY     PASS BACK DELETED RECORDS                    
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',BIGKEY,      +        
               BIGKEY                                                           
*                                                                               
         MVC   DDSKNOTT,=C'XSN='                                                
         MVC   DDSKNOTD,=C'NOTFOUND'                                            
         CLC   BIGKEY(L'MNXKEY),BIGKEYSV                                        
         BNE   SHOWDAX                                                          
         GOTO1 HEXOUT,DMCB,MNXKDA,DDSKNOTD,4                                    
         MVC   DISKADDR,MNXKDA                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'XSPFIL',BIGKEY+36,   +        
               AIO,DMWORK                                                       
         B     SHOWDAX                                                          
*                                                                               
SHOWDAX  XIT1                                                                   
         DROP  R4,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPOMSERROR                                                     
         EJECT                                                                  
***********************************************************************         
* CHECK STATUS & FIND GROUP CODE                                                
* R6 HAS ADDRESS OF MOST RECENT STATUS ELEMENT                                  
* IF OKAY STATUS IS DATED MORE THAN 2 DAYS BEFORE TODAY AND EVERY               
* BUYLINE ELEMENT DOES NOT HAVE AN ERROR STATUS THEN DELETE THE OFFER           
* AND NOTICE RECORDS                                                            
* IF STATUS IS "CANCEL", CHECK IF IT WAS SELFAPPLIED                            
*  IF IT WASN'T, THEN ALWAY DELETE,                                             
*  IF IT WAS, THEN DELETE AFTER 14 DAYS                                         
***********************************************************************         
CHKSTA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MNSTELD,R6                                                       
         GOTO1 DATCON,DMCB,(8,MNSTDATE),(0,WORK)                                
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY)   TODAY'S DATE                       
         CLI   MNSTSTAT,MNSTCAN    STATUS IS "CANCELLED"?                       
         BNE   CHKSTA05            NO                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   MNSTSTAT,MNSTSAPP   YES, CHECK IF IT WAS SELFAPPLIED?            
         BNE   CHKSTA07            NO                                           
*                                                                               
CHKSTA05 GOTO1 GETDAY,DMCB,WORK,WORK+6            GET DAY OF WEEK               
         LA    R0,3                                                             
         CLI   MNSTSTAT,MNSTSAPP   YES, CHECK IF IT WAS SELFAPPLIED?            
         BNE   *+8                 NO                                           
         LA    R0,14                                                            
         CLI   0(R1),2                            IF NOT MONDAY OR TUES         
         BNH   *+8                                                              
         AHI   R0,1                               ADD EXTRA DAY FOR SUN         
         GOTO1 ADDAY,DMCB,WORK,WORK,(R0)          ADD 3/4 DAYS                  
         CLC   WORK(L'TODAY),TODAY                                              
         BL    *+8                                                              
         OI    RECFLAG,OKAYNTEX    OKAYED BUT NOT EXPIRED                       
*                                                                               
CHKSTA07 MVC   SAVEKEY(L'SAVEKEY),KEY             SAVE THE NOTICE KEY           
         XC    KEY,KEY                            AND READ THE OFFER            
         LA    R4,KEY                             RECORD                        
         USING DAREMGOD,R4                                                      
         MVI   MOKTYPE,MOKTYPQ                                                  
         MVI   MOKSUBTY,MOKSTYPQ                                                
         MVC   MOKAGMD,BAGYMD                                                   
         MVC   MOKORDER,MNKORDDT-MNKEY+SAVEKEY                                  
         MVC   MOKMGCD,MNKGROUP-MNKEY+SAVEKEY                                   
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         BRAS  RE,BMPIOCNT                                                      
         BNE   CHKSTAMX                                                         
*                                                                               
         CLC   KEY(MOKSEQ-MOKEY),KEYSAVE                                        
         BE    CHKSTA08                                                         
         L     R6,AIO              OFFER DOESN'T EXIST, LOOK AT NOTICE          
         MVI   ELCODE,MNSTELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MNSTELD,R6                                                       
         CLI   MNSTSTAT,MNSTCAN    CANCELLED MAKEGOOD                           
         BE    CHKSTA45                                                         
         CLI   MNSTSTAT,MNSTOKAY   OKAYED MAKEGOOD?                             
         BE    CHKSTA45                                                         
         DC    H'0'                                                             
*                                                                               
CHKSTA08 TM    RECFLAG,CANCELD                    STATUS "CANCEL"               
         BO    CHKSTA35                                                         
*                                                                               
CHKSTA10 MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BRAS  RE,BMPIOCNT                                                      
         BNE   CHKSTAMX                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,MNUMELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   CHKSTA30                                                         
*                                                                               
         USING MNUMELD,R6          CHECK THE STATUS                             
CHKSTA20 TM    RECFLAG,OKAYNTEX    OKAYED BUT NOT EXPIRED                       
         BNO   CHKSTA25                                                         
         CLI   MNUMSTAT,MNUMBUY    STATUS IS BUY?                               
         BNE   CHKSTA27                                                         
         NI    RECFLAG,X'FF'-OKAYNTEX                                           
         MVC   GROUPCD,MNUMCD                                                   
         B     CHKSTA50                                                         
*                                                                               
CHKSTA25 CLI   MNUMSTAT,MNUMERR    STATUS IS ERROR?                             
         BE    CHKSTA50                                                         
CHKSTA27 BAS   RE,NEXTEL                                                        
         BE    CHKSTA20                                                         
*                                                                               
CHKSTA30 TM    RECFLAG,OKAYNTEX    OKAYED BUT NOT EXPIRED                       
         BO    CHKSTA50                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         BRAS  RE,BMPIOCNT                                                      
         BNE   CHKSTAMX                                                         
*                                                                               
         CLC   KEY(MOKSEQ-MOKEY),KEYSAVE                                        
         BE    CHKSTA10                                                         
*                                                                               
* IF I GET PAST HERE, THEN I DELETE OFFER/NOTICE                                
*                                                                               
CHKSTA35 XC    KEY,KEY                                                          
         MVC   KEY(L'MOKEY),KEYSAVE  FIRST OFFER                                
CHKSTA37 MVI   RDUPDATE,C'Y'       READ FOR UPDATE!!!                           
         GOTO1 HIGH                                                             
         BRAS  RE,BMPIOCNT                                                      
*        BNE   CHKSTAMX           DON'T EXIT, NEED TO DELETE EVERYTHING         
*                                                                               
         CLC   KEY(MOKSEQ-MOKEY),KEYSAVE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKSTA40 MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         BRAS  RE,BMPIOCNT                                                      
*        BNE   CHKSTAMX           DON'T EXIT, NEED TO DELETE EVERYTHING         
*                                                                               
         L     R6,AIO              DELETE THE OFFER RECORD                      
         USING DAREMGOD,R6                                                      
         TM    KEY+L'MOKEY,X'80'   ALREADY DELETED?                             
         BNZ   CHKSTA43            YES, DON'T DO THIS AGAIN FOR NOTHING         
         OI    KEY+L'MOKEY,X'80'                                                
         GOTO1 WRITE                                                            
         BRAS  RE,BMPIOCNT                                                      
*        BNE   CHKSTAMX           DON'T EXIT, NEED TO DELETE EVERYTHING         
*                                                                               
         TM    MORSTAT,X'80'       ALREADY DELETED?                             
         BNZ   CHKSTA43            YES, DON'T DO THIS AGAIN FOR NOTHING         
         OI    MORSTAT,X'80'                                                    
         GOTO1 PUTREC                                                           
*                                                                               
CHKSTA43 MVI   RDUPDATE,C'N'       IN CASE THERE ARE MULTIPLE (MOKSEQ)          
         GOTO1 SEQ                                                              
         BRAS  RE,BMPIOCNT                                                      
*        BNE   CHKSTAMX           DON'T EXIT, NEED TO DELETE EVERYTHING         
*                                                                               
         CLC   KEY(MOKSEQ-MOKEY),KEYSAVE                                        
         BE    CHKSTA37            READ HIGH FOR UPDATE!!!                      
*                                                                               
CHKSTA45 XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY   DELETE THE NOTICE RECORD                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         BRAS  RE,BMPIOCNT                                                      
*        BNE   CHKSTAMX           DON'T EXIT, NEED TO DELETE EVERYTHING         
*                                                                               
         CLC   KEY(L'MNKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         BRAS  RE,BMPIOCNT                                                      
*        BNE   CHKSTAMX           DON'T EXIT, NEED TO DELETE EVERYTHING         
*                                                                               
         L     R6,AIO                                                           
         USING DAREMGND,R6                                                      
         TM    KEY+L'MNKEY,X'80'   ALREADY DELETED?                             
         BNZ   CHKSTA48                                                         
         OI    KEY+L'MNKEY,X'80'   DON'T WASTE IO'S                             
         GOTO1 WRITE                                                            
         BRAS  RE,BMPIOCNT                                                      
*        BNE   CHKSTAMX           DON'T EXIT, NEED TO DELETE EVERYTHING         
*                                                                               
         TM    MNRSTAT,X'80'       ALREADY DELETED?                             
         BNZ   CHKSTA48                                                         
         OI    MNRSTAT,X'80'       DON'T WASTE IO'S                             
         GOTO1 PUTREC                                                           
         BRAS  RE,BMPIOCNT                                                      
*        BNE   CHKSTAMX           DON'T EXIT, NEED TO DELETE EVERYTHING         
*                                                                               
CHKSTA48 NI    RECFLAG,X'FF'-CANCELD                                            
         B     YES                                                              
*                                                                               
CHKSTA50 MVC   KEY(L'SAVEKEY),SAVEKEY    REREAD NOTICE RECORD                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         BRAS  RE,BMPIOCNT                                                      
*        BNE   CHKSTAMX           DON'T EXIT, NEED TO DELETE EVERYTHING         
*                                                                               
         CLC   KEY(L'MNKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BRAS  RE,BMPIOCNT                                                      
         BNE   CHKSTAMX                                                         
*                                                                               
         B     NO                                                               
*                                                                               
CHKSTAMX MVC   IO_COUNT,=X'FFFF'                                                
         B     NO                                                               
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPOMSDSCTS                                                     
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* SPGENAGY                                                                      
* SPGENCLT                                                                      
* SPGENMKT                                                                      
* SPOMSFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSFED          (OUR MAKEGOOD LIST SCREEN)                   
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSF3D          MG SCREEN                                    
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMKN                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMKO                                                     
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
IO_COUNT DS    H                   I/O COUNTER                                  
*                                                                               
BIGKEYSV DS    XL(L'BIGKEY)        FOR XSPDIR                                   
*                                                                               
OPTFLAG  DS    XL1                 OPTION FLAGS                                 
OPNSTATN EQU   X'80'               FILTER ON STATION                            
OPNSTATS EQU   X'40'               FILTER ON STATUS                             
*                                                                               
RECFLAG  DS    XL1                 RECORD FLAGS                                 
REJFND   EQU   X'80'               REJECTION STATUS ELEMENT FOUND               
OKAYNTEX EQU   X'40'               OKAYED BUT NOT EXPIRED, GET GROUP CD         
CANCELD  EQU   X'20'               CANCELED STATUS                              
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAG BITS                      
MF1KYCHG EQU   X'80'               - KEY WAS CHANGED                            
MF1CNTIO EQU   X'40'               - NEED TO COUNT I/O'S                        
MF1FIRST EQU   X'20'               - NEED 'HIT ENTER FOR FIRST' MESSAGE         
MF1BACK  EQU   X'10'               - REDISPLAY CURRENT LIST SCREEN              
*                                                                               
MISCFLG2 DS    XL1                 MISCELLANEOUS FLAGS BIT 2                    
MF2RCNTF EQU   X'80'               - ORDER RECORD NOT FOUND                     
MF2CBLOR EQU   X'40'               - SHOWDAS FOUND A CABLE ORDER                
*                                                                               
DLVRDFLG DS    XL1                 DELIVERY NOTICE FLAG                         
GOTDLVRD EQU   X'80'               GOT DELIVERY NOTICE                          
*                                                                               
RECLENB4 DS    H                   RECORD LENGTH B4 IN THE LAST TRANS           
ELEMDISP DS    H                   ELEMENT DISPLACEMENT (USED FOR DIS)          
*                                                                               
SVBUYRCD DS    CL4                 BUYER CODE                                   
GROUPCD  DS    CL2                 GROUP CODE                                   
DKEYTABL DS    H                   D(KEY TABLE ENTRY)                           
*                                                                               
SVLSTKEY DS    XL(L'MNKEY)         LAST MAKEGOOD NOTICE KEY EXAMINED            
SAVEKEY  DS    XL(L'MNKEY)                                                      
SVLSTXKY DS    XL(L'MNXKEY)        LAST CABLE MKGD NOTICE KEY XAMINED           
DISKADDR DS    XL4                 DISK ADDRESS                                 
*                                                                               
FAKEFLDH DS    CL8                 FAKE FIELD HEADER                            
FAKEFLD  DS    CL60                FAKE FIELD                                   
SAVSTATN DS    CL5                 SAVE THE FILTER STATION                      
SAVSTAT  DS    CL9                 SAVE THE FILTER STATUS                       
SAVERROR DS    CL3                 SAVE THE 3 BYTE ERROR CODE                   
STATLEN  DS    X                   FILTER STATUS LENGTH                         
STATUS   DS    CL9                                                              
TODAY    DS    CL6                 TODAY'S DATE EBCDIC                          
*                                                                               
BINORDER DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                     DATE PORTION                             
BINORDSQ DS    XL2                     SEQUENCE PORTION                         
*                                                                               
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
DATEFLTR DS    CL6                                                              
DATEFLT1 DS    XL3                 TODAYS PWOS DATE MINUS 10                    
ADELNOT  DS    A                                                                
*                                                                               
PROFDAR  DS    CL16                DARE PROFILE                                 
PDARSAPP EQU   PROFDAR+13            USE SELF APPLY MAKEGOODS                   
*                                                                               
KEYTABLE DS    ((ORMSELLH-ORMSEL1H)/(ORMSEL2H-ORMSEL1H)+1)XL(KTBLLENQ)          
KYTBLLNQ EQU   *-KEYTABLE                                                       
         EJECT                                                                  
*                                                                               
* KEY TABLE BREAKDOWN                                                           
*                                                                               
KYTABLED DSECT                                                                  
KTBLKEY  DS    CL(L'MNXKEY)        MAKEGOOD NOTICE KEY                          
KTBLFLG1 DS    XL1                 FLAG                                         
KTF1POLE EQU   X'80'                - POL ESTIMATE                              
KTBLLENQ EQU   *-KYTABLED                                                       
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
MGSTAD   DSECT                                                                  
         DS    CL8                 FOR HEADER                                   
MGSTSTAT DS    CL9                                                              
         DS    CL17                                                             
MGSTDATE DS    CL8                                                              
         DS    CL2                                                              
MGSTTIME DS    CL5                                                              
         ORG   MGSTSTAT+1                                                       
MGSTCMNT DS    CL8                                                              
         DS    C                                                                
MGSTCMT  DS    CL68                                                             
*                                                                               
* OFFER/STATUS PFKEY DISKADDRESS                                                
*                                                                               
DISPFLD  DSECT                                                                  
DPFLPF12 DS    CL11             PF12=RETURN                                     
         DS    CL1                                                              
DCABLETX DS    CL7              *CABLE*                                         
         DS    CL1                                                              
DDSKOFRT DS    CL4                                                              
DDSKOFRD DS    CL8                                                              
         DS    CL1                                                              
DDSKNOTT DS    CL4                                                              
DDSKNOTD DS    CL8                                                              
         DS    CL1                                                              
DDSKORDT DS    CL4                                                              
DDSKORDD DS    CL8                                                              
*                                                                               
MKGDLIND DSECT                                                                  
MGSELECH DS    CL8                                                              
MGSELECT DS    CL3                                                              
         DS    CL8                                                              
MGSTATS1 DS    C                                                                
MGSTATUS DS    CL9                                                              
         DS    CL1                                                              
MGORDER  DS    CL8                                                              
MGCSHTRD DS    CL1                                                              
         DS    CL1                                                              
MGGRPCD  DS    CL3                                                              
         DS    CL1                                                              
MGTYPCD  DS    CL2                                                              
         DS    CL1                                                              
MGSTATN  DS    CL7                                                              
         DS    CL1                                                              
MGCLIENT DS    CL3                                                              
         DS    CL1                                                              
MGPRDCTS DS    CL7                                                              
         DS    CL1                                                              
MGESTMTE DS    CL3                                                              
         DS    CL1                                                              
MGFLIGHT DS    CL2                                                              
         DS    CL1                                                              
MGACTDAT DS    CL14                                                             
         DS    CL1                                                              
MGBUYER  DS    CL3                                                              
MGCOLORH DS    CL8                                                              
MGCOLOR  DS    C                                                                
MKGDLNNX DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'123SPOMS03   03/16/16'                                      
         END                                                                    
