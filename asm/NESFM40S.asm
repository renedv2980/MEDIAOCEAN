*          DATA SET NESFM40S   AT LEVEL 029 AS OF 05/01/02                      
*PHASE T31C40A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T31C40 - MAINTENANCE/LIST OF PRODUCT GROUP ASSIGNMENTS                
*                                                                               
*  COMMENTS: MAINTAINS PRODUCT GROUP ASSIGNMENTS                                
*                                                                               
*  CALLED FROM: NET SFM CONTROLLER (T31C40), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS NESFMD3 (T31CD3) -- MAINTENANCE                              
*                  NESFMD5 (T31CD5) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW BUYERS                                               
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - SECOND BASE                                                     
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
         TITLE 'NESFM40 - PRODUCT GROUP ASSIGNMENTS'                            
T31C40   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NETPGA                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,2048(RB)                                                      
         LA    R5,2048(R5)                                                      
         USING T31C40,RB,R5                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         BAS   RE,SECURITY                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
*        CLI   MODE,RECDEL         DELETE RECORDS                               
*        BE    DELERR                                                           
         CLI   MODE,XRECADD        AFTER RECORD HAS BEEN ADDED                  
         BE    XU                                                               
         CLI   MODE,XRECPUT        AFTER RECORD HAS BEEN PUT                    
         BE    XU                                                               
         CLI   MODE,XRECDEL        AFTER RECORD HAS BEEN PUT                    
         BE    XD                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                  VALIDATE KEY                                 
         XC    SVKEY,SVKEY                                                      
         LA    R3,SVKEY                                                         
         USING PRGRECD,R3                                                       
         MVC   PRGKTYP,=X'0D01'                                                 
*                                                                               
         LA    R2,SFMMEDH                                                       
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY??                       
         BNZ   VK03                                                             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         VALIDATED                                    
VK03     MVC   PRGKAGMD,BAGYMD                                                  
*                                                                               
         LA    R2,SFMCLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK05                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST AND REPORT                 
         BE    VK06                                                             
         CLI   ACTNUM,ACTREP                                                    
         BE    VK06                                                             
         B     MISSFLD                                                          
*                                                                               
VK05     MVC   QCLT,SFMCLT         GET CLIENT CODE                              
         OC    QCLT,SPACES                                                      
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   INVLFLD                                                          
         MVC   PRGKCLT,BCLT                                                     
*                                                                               
VK06     LA    R2,SFMIDH                                                        
         MVI   IDFILTER,C'N'                                                    
         CLI   5(R2),0                                                          
         BNE   VK08                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST AND REPORT                 
         BE    VKX                                                              
         CLI   ACTNUM,ACTREP                                                    
         BE    VKX                                                              
         B     MISSFLD                                                          
*                                                                               
VK08     MVC   PRGKID,SFMID                                                     
         MVI   IDFILTER,C'Y'                                                    
         OC    SFMCLT,SFMCLT       LIST/REP FILTER ON ID ONLY                   
         BZ    VKX                                                              
*                                                                               
VK09     MVC   KEY,SVKEY           GET DEFINITION REC                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'PRGKEY),KEYSAVE                                            
         BNE   PDEFERR                                                          
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PRGEL01,R6          GET GROUP BREAK DESCRIPTION ELEMENT          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,PRGBK1LN                                                      
         ZIC   R4,PRGBK2LN                                                      
         AR    R1,R4                                                            
         LA    R1,1(R1)            COMPENSATE FOR LETTER CODE                   
         STC   R1,WORK                                                          
         MVC   BYTE,5(R2)          IN ID                                        
         CLC   BYTE,WORK                                                        
         BE    VK10                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK10                                                             
         CLI   ACTNUM,ACTREP                                                    
         BNE   INVLFLD                                                          
*                                                                               
VK10     XC    WORK,WORK                                                        
         MVC   WORK(3),9(R2)       LET ALIGNED PWOS                             
         PACK  WORK+10(3),WORK(5)                                               
         MVC   PRGKGRP,WORK+10                                                  
*                                                                               
         CLI   ACTNUM,ACTADD       TEST ADD                                     
         BNE   VKX                                                              
*                                                                               
         MVC   SFMBK1(12),PRGBK1                                                
         OI    SFMBK1H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SFMBK2(12),PRGBK2                                                
         OI    SFMBK2H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SVBKLNS+0(1),PRGBK1LN                                            
         MVC   SVBKLNS+1(1),PRGBK2LN                                            
         MVC   SVBKLNS+2(1),PRGBK3LN                                            
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                  DISPLAY KEY                                  
         L     R3,AIO                                                           
         MVC   KEY(L'PRGKEY),0(R3)                                              
         MVC   SAVEKEY,KEY                                                      
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         MVI   SFMMED,C'N'                                                      
         OI    SFMMEDH+6,X'80'     XMIT                                         
         GOTO1 CLUNPK,DMCB,PRGKCLT,SFMCLT                                       
         OI    SFMCLTH+6,X'80'     XMIT                                         
         MVC   SFMID(1),PRGKID                                                  
         XC    FULL,FULL                                                        
         MVC   FULL(2),PRGKGRP                                                  
         UNPK  WORK(5),FULL(3)                                                  
*                                                                               
         XC    PRGKGRP,PRGKGRP                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'PRGKEY),KEYSAVE                                            
         BNE   PDEFERR                                                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRGEL01,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,PRGBK1LN                                                      
         ZIC   R4,PRGBK2LN                                                      
         AR    R1,R4               GET LENGTH OF ID                             
         STC   R1,IDLEN            SO WE CAN DISPLAY ID CORRECTLY               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFMID+1(0),WORK                                                  
         OI    SFMIDH+6,X'80'      XMIT                                         
*                                                                               
DKX      MVC   KEY,SAVEKEY                                                      
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE ALL '0D81' RECORDS BELONGING TO SELECTED PRODUCT GROUP                 
***********************************************************************         
DELREC   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DR05                                                             
         MVC   PREVKEY,KEY                                                      
         MVI   PREVFLAG,C'Y'                                                    
*                                                                               
DR05     MVC   SAVEKEY,KEY                                                      
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         XC    PRGKGRP,PRGKGRP     GET DEFINITION RECORD                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'PRGKEY),KEYSAVE                                            
         BNE   PDEFERR                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRGEL01,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SFMBK1(12),PRGBK1                                                
         OI    SFMBK1H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SFMBK2(12),PRGBK2                                                
         OI    SFMBK2H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SVBKLNS+0(1),PRGBK1LN                                            
         MVC   SVBKLNS+1(1),PRGBK2LN                                            
         MVC   SVBKLNS+2(1),PRGBK3LN                                            
         DROP  R6                                                               
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    DR25                                                             
*                                                                               
         FOUT  SFMNM1H,SPACES,24                                                
*                                                                               
         FOUT  SFMNM2H,SPACES,24                                                
*                                                                               
         FOUT  SFMUSERH,SPACES,3                                                
*                                                                               
         FOUT  SFMADD1H,SPACES,30                                               
         FOUT  SFMADD2H,SPACES,30                                               
         FOUT  SFMADD3H,SPACES,30                                               
         FOUT  SFMADD4H,SPACES,30                                               
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'PRGKEY),KEYSAVE                                            
         BNE   PASSERR                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRGEL10,R6                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SFMNM1,PRGNAM1                                                   
         OI    SFMNM1H+6,X'80'         XMIT                                     
*                                                                               
         MVC   SFMNM2,PRGNAM2                                                   
         OI    SFMNM2H+6,X'80'         XMIT                                     
         DROP  R6                                                               
*                                                                               
DR10     ZIC   R1,1(R6)                                                         
         AR    R6,R1               POINT TO NEXT ELEMENT                        
         CLI   0(R6),0                                                          
         BE    DR15                                                             
         CLI   0(R6),X'20'         ADDRESS ELEMENT                              
         BNE   DR10                                                             
                                                                                
         USING PRGEL20,R6                                                       
         LA    R2,SFMADD1H                                                      
         FOUT  (R2),PRGADDR1,24                                                 
         OI    4(R2),X'20'                                                      
         LA    R2,SFMADD2H                                                      
         FOUT  (R2),PRGADDR2,24                                                 
         OI    4(R2),X'20'                                                      
         LA    R2,SFMADD3H                                                      
         FOUT  (R2),PRGADDR3,24                                                 
         OI    4(R2),X'20'                                                      
         LA    R2,SFMADD4H                                                      
         FOUT  (R2),PRGADDR4,24                                                 
         OI    4(R2),X'20'                                                      
         DROP  R6                                                               
*                                                                               
DR15     L     R6,AIO                                                           
         USING PRGEL30,R6                                                       
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR25                                                             
*                                                                               
         LA    R2,SFMUSERH                                                      
         FOUT  (R2),PRGUSER,3                                                   
         OI    4(R2),X'20'                                                      
         DROP  R6                                                               
*                                                                               
DR25     TWAXC SFMPRD1H,SFMPRDXH   CLEAR ADDED PRODUCT FIELDS                   
         TWAXC SFMLST1H,SFMLSTXH,PROT=Y    CLEAR PRODUCT LIST                   
*                                                                               
         XC    KEY,KEY             READ PASSIVE KEY AND DISPLAY LIST            
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,SAVEKEY+2  AGY/MED                                      
         MVC   PRGPCLT,SAVEKEY+3   CLIENT                                       
         MVC   PRGPID(3),SAVEKEY+5 GROUP ID                                     
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,SFMLST1H                                                      
*                                                                               
         LA    R4,SFMLST1                                                       
         ZAP   HALF,=P'18'         SET FOR 18 PRDS PER LINE                     
         B     DR40                                                             
*                                                                               
DR30     GOTO1 SEQ                                                              
*                                                                               
DR40     CLC   KEY(8),KEYSAVE      TEST SAME THRU PRDGRP                        
         BNE   DR50                                                             
*                                                                               
         MVC   0(3,R4),PRGPPRD                                                  
         LA    R4,4(R4)                                                         
         SP    HALF,=P'1'          ADJUST COUNTER                               
         BP    DR30                CONTINUE IF MORE ROOM                        
         OI    6(R2),X'80'         XMIT LINE                                    
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST E-O-S                                   
         BE    DR50                                                             
         LA    R4,8(R2)                                                         
         ZAP   HALF,=P'18'                                                      
         B     DR30                                                             
*                                                                               
DR50     OI    6(R2),X'80'         XMIT LINE                                    
         MVC   KEY,SAVEKEY                                                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   DRX                                                              
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(54),=C'RECORD ADDED, ENTER MORE PRODUCTS OR ENTEX        
               R NEXT REQUEST'                                                  
         OI    CONHEADH+6,X'80'    XMIT                                         
         XC    CONACT,CONACT                                                    
         MVC   CONACT(3),=C'CHA'                                                
         OI    CONACT+6,X'80'      XMIT                                         
         LA    R2,SFMPRD1H                                                      
         GOTO1 ERREX2                                                           
*                                                                               
DRX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         CLI   ACTNUM,ACTADD       TEST ADD                                     
         BE    VR10                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR10     DS    0H                                                               
         LA    R6,ELEM                                                          
         USING PRGEL10,R6                                                       
         MVI   PRGEL10,X'10'       ELEMENT CODE                                 
         MVI   PRGEL10+1,74        ELEMENT LENGTH                               
         LA    R2,SFMNM1H                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         MVC   PRGNAM1,SFMNM1                                                   
         OC    PRGNAM1,SPACES                                                   
*                                                                               
         LA    R2,SFMNM2H                                                       
         CLI   SVBKLNS+1,0                                                      
         BNE   VR20                                                             
         CLI   5(R2),0                                                          
         BNE   INVLFLD                                                          
         B     VR30                                                             
*                                                                               
VR20     CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         MVC   PRGNAM2,SFMNM2                                                   
         OC    PRGNAM2,SPACES                                                   
         DROP  R6                                                               
*                                                                               
VR30     L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR40     L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING PRGEL20,R4                                                       
         MVI   ELEM,X'20'                                                       
         MVI   ELEM+1,122                                                       
         MVI   BYTE,C'N'                                                        
*                                                                               
         LA    R2,SFMADD1H                                                      
         CLI   5(R2),0                                                          
         BE    VR40A                                                            
         MVC   PRGADDR1,SFMADD1                                                 
         OC    PRGADDR1,SPACES                                                  
         MVI   BYTE,C'Y'                                                        
*                                                                               
VR40A    LA    R2,SFMADD2H                                                      
         CLI   5(R2),0                                                          
         BE    VR40B                                                            
         MVC   PRGADDR2,SFMADD2                                                 
         OC    PRGADDR2,SPACES                                                  
         MVI   BYTE,C'Y'                                                        
*                                                                               
VR40B    LA    R2,SFMADD3H                                                      
         CLI   5(R2),0                                                          
         BE    VR40C                                                            
         MVC   PRGADDR3,SFMADD3                                                 
         OC    PRGADDR3,SPACES                                                  
         MVI   BYTE,C'Y'                                                        
*                                                                               
VR40C    LA    R2,SFMADD4H                                                      
         CLI   5(R2),0                                                          
         BE    VR40X                                                            
         MVC   PRGADDR4,SFMADD4                                                 
         OC    PRGADDR4,SPACES                                                  
         MVI   BYTE,C'Y'                                                        
*                                                                               
VR40X    CLI   BYTE,C'Y'                                                        
         BNE   VR60                                                             
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR60     LA    R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SFMUSERH         VALIDATE MASTER PRD USER FIELD               
         CLI   5(R2),0                                                          
         BE    VRX                                                              
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PRGEL30,R6                                                       
         MVI   ELEM,X'30'                                                       
         MVI   ELEM+1,5                                                         
         MVC   ELEM+2(3),SFMUSER                                                
         OC    ELEM+2(3),=XL3'404040'                                           
         DROP  R6                                                               
*                                                                               
* VALIDATE PRD ON FILE                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),SFMUSER                                                 
         OC    KEY+4(3),SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLFLD                                                          
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM             NOW VALID UNLESS CHANGED AGAIN               
*                                                                               
VRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* AFTER RECORD ADDED/PUT, UPDATE PRODUCT RECORDS/PASSIVE KEYS                   
***********************************************************************         
XU       DS    0H                  UPDATE PRODUCT RECORD                        
         LA    R2,SFMPRD1H                                                      
         B     XU40                                                             
*                                                                               
XU10     DS    0H                  VALIDATE PRD ON FILE                         
         OI    4(R2),X'20'         SET VALID                                    
         XC    KEY,KEY             BUILD PRODUCT KEY                            
         LA    R3,KEY                                                           
         USING PRDRECD,R3                                                       
         MVC   PKEYAM,SAVEKEY+2    MOVE AGY/MED                                 
         MVC   PKEYCLT,SAVEKEY+3   MOVE CLT                                     
         MVC   PKEYPRD,8(R2)                                                    
         OC    PKEYPRD,SPACES                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'PKEY),KEYSAVE                                              
         BNE   INVLPRD                                                          
*                                                                               
         MVC   DASAVE,KEY+14       SAVE PRDHDR DISK ADDRESS                     
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         USING PRDRECD,R3                                                       
         XC    ELEM,ELEM           PUT ALL PGROUP ASSGNS IN ELEM                
         LA    R6,ELEM             ONE LEVEL NOW                                
         MVC   0(9,R6),PGRP1                                                    
         MVC   9(6,R6),PGRP4                                                    
         LA    RF,5                MAX 5 CLIENTS                                
*                                                                               
XU14     DS    0H                                                               
         XC    GROUP,GROUP                                                      
         CLC   0(1,R6),SAVEKEY+5   IF PROD ALREADY ASSIGNED TO ANOTHER          
         BNE   XU16                GRP WITH SAME SCHEME, MOVE PROD              
         MVC   GROUP,0(R6)         TO THIS SCHEME                               
         B     XU18                                                             
*                                                                               
XU16     OC    0(3,R6),0(R6)       ROOM TO ADD NEW ASSIGNMENT??                 
         BZ    XU18                                                             
         LA    R6,3(R6)                                                         
         BCT   RF,XU14                                                          
         B     PFULLERR            CLIENT FULL, NO ROOM TO ADD                  
*                                                                               
XU18     MVC   0(3,R6),SAVEKEY+5   MOVE NEW PRDGRP INTO ELEM                    
         MVC   PGRP1(9),ELEM       RESTORE PGROUP ASSGNS                        
         MVC   PGRP4(6),ELEM+9     RESTORE PGROUP ASSGNS                        
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=CL8'SPTFILE',KEY+14,AIO,MYDMWRK         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         XC    KEY,KEY             ADD/RESTORE PASSIVE KEY                      
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,SAVEKEY+2                                               
         MVC   PRGPCLT,SAVEKEY+3                                                
         MVC   PRGPID(3),SAVEKEY+5                                              
         MVC   PRGPPRD,8(R2)       MOVE IN CLIENT CODE                          
         OC    PRGPPRD,SPACES                                                   
*                                  SEE IF ON FILE                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         MVI   DMINBTS,0           RESET                                        
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XU20                                                             
         TM    KEY+13,X'80'                                                     
         BNO   INVLPRD                                                          
         NI    KEY+13,X'FF'-X'80'  RESTORE DELETED PASSIVE KEY                  
         MVC   KEY+14(4),DASAVE    INSERT DISK ADDR                             
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XU25                                                             
*                                                                               
XU20     MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY+14(4),DASAVE    INSERT DISK ADDR                             
         GOTO1 ADD                 ADD NEW PASSIVE POINTER                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XU25     DS    0H                                                               
         OC    GROUP,GROUP         ARE WE MOVING THE PRODUCT FROM               
         BZ    XU30                ONE GROUP TO ANOTHER??                       
         XC    KEY,KEY             DELETE PASSIVE KEY FROM THE GROUP            
         LA    R3,KEY              WHICH PRODUCT HAS MOVED FROM                 
         USING PRGRECD,R3                                                       
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,SAVEKEY+2                                               
         MVC   PRGPCLT,SAVEKEY+3                                                
         MVC   PRGPID(3),GROUP                                                  
         MVC   PRGPPRD,8(R2)       MOVE IN PRODUCT CODE                         
         OC    PRGPPRD,SPACES                                                   
*                                  SEE IF ON FILE                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     NOT THERE, SO LEAVE                          
         BNE   XU30                                                             
         OI    KEY+13,X'80'        ELSE DELETE PASSIVE KEY                      
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XU30     ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         LA    RF,SFMPRDXH                                                      
         CR    R2,RF               LAST INPUT FIELD??                           
         BH    XUX                                                              
*                                                                               
XU40     CLI   5(R2),0             TEST DATA                                    
         BE    XU30                NO-TRY NEXT                                  
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BO    XU30                                                             
         B     XU10                GO EDIT                                      
*                                                                               
*XUX      B     EXIT                                                            
XUX      MVC   KEY,SAVEKEY                                                      
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* AFTER RECORD DELETED DELETE PRODUST RECORDS/ PASSIVE KEYS                     
***********************************************************************         
XD       DS    0H                  UPDATE PRODUCT RECORD                        
*                                                                               
XD10     DS    0H                  VALIDATE PRD ON FILE                         
         XC    KEY,KEY             BUILD PRODUCT KEY                            
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,SAVEKEY+2  AGY/MED                                      
         MVC   PRGPCLT,SAVEKEY+3   CLIENT                                       
         MVC   PRGPID(3),SAVEKEY+5 GROUP ID                                     
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
XD12     CLC   KEY(8),KEYSAVE                                                   
         BNE   XDX                                                              
*  DELETE POINTER                                                               
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
         DROP  R3                                                               
*                                                                               
*  REMOVE GROUP INFORMATION FROM PRODUCT RECORDS                                
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         USING PRDHDR,R3                                                        
         LA    RE,PGRP1                                                         
         CLC   0(3,RE),SAVEKEY+5                                                
         BE    XD14                                                             
         LA    RE,3(RE)                                                         
         CLC   0(3,RE),SAVEKEY+5                                                
         BE    XD14                                                             
         LA    RE,3(RE)                                                         
         CLC   0(3,RE),SAVEKEY+5                                                
         BE    XD14                                                             
         LA    RE,PGRP4                                                         
         CLC   0(3,RE),SAVEKEY+5                                                
         BE    XD14                                                             
         LA    RE,3(RE)                                                         
         CLC   0(3,RE),SAVEKEY+5                                                
         BE    XD14                                                             
         DC    H'0'                                                             
*                                                                               
XD14     DS    0H                                                               
         XC    0(3,RE),0(RE)                                                    
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=CL8'SPTFILE',KEY+14,AIO,MYDMWRK         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*  GET NEXT KEY                                                                 
         GOTO1 SEQ                                                              
         B     XD12                                                             
*                                                                               
XDX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS                                                                  
***********************************************************************         
LR       DS    0H                                                               
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         LA    R7,LISTAR                                                        
         USING PLINED,R7                                                        
*                                                                               
LR02     MVI   NLISTS,14                                                        
         CLI   PREVFLAG,C'Y'                                                    
         BE    LR03                                                             
         OC    KEY,KEY                                                          
         BNZ   LR05                                                             
         MVC   KEY(3),SVKEY                                                     
         B     LR05                                                             
*                                                                               
LR03     MVC   KEY,PREVKEY                                                      
         MVI   PREVFLAG,C'N'                                                    
*                                                                               
LR05     GOTO1 HIGH                                                             
         B     LR10                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR10     CLC   KEY(3),SVKEY                                                     
         BNE   LRX                                                              
         OC    SFMCLT,SFMCLT       FILTER ON CLIENT??                           
         BZ    LR12                                                             
         CLC   PRGKCLT,BCLT                                                     
         BNE   LRSEQ                                                            
*                                                                               
LR12     OC    SFMID,SFMID         FILTER ON ID???                              
         BZ    LR13                                                             
         CLC   PRGKID,SFMID                                                     
         BNE   LRSEQ                                                            
         CLI   IDFILTER,C'N'                                                    
         BE    LR13                                                             
         OC    PRGKGRP(7),PRGKGRP  CHK IF DEFINITION RECORD                     
         BNZ   LRSEQ                                                            
         MVI   IDFILTER,C'N'                                                    
         B     LR14                                                             
*                                                                               
LR13     OC    PRGKGRP(7),PRGKGRP  CHK IF DEFINITION RECORD                     
         BNZ   LR15                                                             
*                                                                               
LR14     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PRGEL01,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,PRGBK1LN                                                      
         ZIC   R4,PRGBK2LN                                                      
         DROP  R6                                                               
         AR    R1,R4                                                            
         STC   R1,IDLEN                                                         
         B     LRSEQ                                                            
*                                                                               
LR15     DS    0H                                                               
         CLI   SFMIDH+5,1          FILTER ON MORE THAN LETTER                   
         BNH   LR20                                                             
         CLC   PRGKID,SFMID                                                     
         BNE   LRSEQ                                                            
         XC    FULL,FULL                                                        
         MVC   FULL(2),PRGKGRP                                                  
         UNPK  WORK(5),FULL(3)     YES/UNPACK ID FIELD                          
         ZIC   R1,SFMIDH+5         GET LENGTH OF COMPARE                        
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SFMID+1                                                  
         BNE   LRSEQ                                                            
*                                                                               
LR20     GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PRGEL10,R6                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LISTAR,SPACES                                                    
         GOTO1 CLUNPK,DMCB,PRGKCLT,PLCLT                                        
         MVC   PLGRPID(1),PRGKID                                                
         XC    FULL,FULL                                                        
         MVC   FULL(2),PRGKGRP                                                  
         UNPK  WORK(5),FULL(3)                                                  
         ZIC   R1,IDLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLGRPID+1(0),WORK                                                
         MVC   PLLEV1,PRGNAM1                                                   
         MVC   PLLEV2,PRGNAM2                                                   
         GOTO1 LISTMON                                                          
         B     LRSEQ                                                            
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R3,R7                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       DS    0H                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HHOOK                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         LA    R4,P                                                             
         USING RLINED,R4                                                        
         MVI   PLFLAG,C'N'                                                      
         OC    KEY,KEY                                                          
         BNZ   PR08                                                             
         MVC   KEY(3),SVKEY                                                     
*                                                                               
PR08     GOTO1 HIGH                                                             
         B     PR10                                                             
*                                                                               
PRSEQ    GOTO1 SEQ                                                              
*                                                                               
PR10     CLC   KEY(3),SVKEY                                                     
         BNE   PR80                                                             
         OC    SFMCLT,SFMCLT       FILTER ON CLIENT??                           
         BZ    PR11                                                             
         CLC   PRGKCLT,BCLT                                                     
         BNE   PRSEQ                                                            
*                                                                               
PR11     OC    SFMID,SFMID                                                      
         BZ    PR12                                                             
         CLC   PRGKID,SFMID                                                     
         BNE   PRSEQ                                                            
         CLI   IDFILTER,C'N'                                                    
         BE    PR12                                                             
         OC    PRGKGRP(7),PRGKGRP  CHK IF DEFINITION RECORD                     
         BNZ   PRSEQ                                                            
         MVI   IDFILTER,C'N'                                                    
         B     PR13                                                             
*                                                                               
PR12     OC    PRGKGRP(7),PRGKGRP  CHK IF DEFINITION RECORD                     
         BNZ   PR20                                                             
*                                                                               
PR13     DS    0H                                                               
         CLI   PLFLAG,C'N'                                                      
         BE    PR14                                                             
         OC    ABOX,ABOX           PRINT BOTTOM OF BOX                          
         BZ    PR15                                                             
         L     R7,ABOX                                                          
         USING BOXD,R7                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
         DROP  R7                                                               
         BAS   RE,PRTLINE                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PR14     DS    0H                                                               
         GOTO1 CLUNPK,DMCB,PRGKCLT,H2+60                                        
         MVC   H3+60(1),PRGKID                                                  
*                                                                               
PR15     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PRGEL01,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,PRGBK1LN                                                      
         ZIC   RE,PRGBK2LN                                                      
         AR    R1,RE                                                            
         STC   R1,IDLEN                                                         
*                                                                               
         BAS   RE,PRTLINE                                                       
         BAS   RE,PRTLINE                                                       
         MVC   RCODE,=C'NO. '                                                   
         MVC   RBREAK1(12),PRGBK1                                               
*        MVI   P+19,C'('                                                        
*        EDIT  (B1,PRGBK1LN),(1,P+20)                                           
*        MVI   P+21,C')'                                                        
         CLI   PRGBK2LN,0                                                       
         BE    PR18                                                             
*                                                                               
         MVC   RBREAK2(12),PRGBK2                                               
*        MVI   P+44,C'('                                                        
*        EDIT  (B1,PRGBK2LN),(1,P+45)                                           
*        MVI   P+46,C')'                                                        
*                                                                               
PR18     MVC   RPRD(8),=C'PRODUCTS'                                             
         MVC   SVRLINE,P           SAVE REPORT HEADER                           
         BAS   RE,PRTLINE                                                       
         MVI   PLFLAG,C'Y'                                                      
         B     PRSEQ                                                            
*                                                                               
PR20     DS    0H                                                               
         CLI   SFMIDH+5,1          FILTER ON MORE THAN LETTER                   
         BNH   PR30                                                             
         CLC   PRGKID,SFMID                                                     
         BNE   PRSEQ                                                            
         XC    FULL,FULL                                                        
         MVC   FULL(2),PRGKGRP                                                  
         UNPK  WORK(5),FULL(3)     YES/UNPACK ID FIELD                          
         ZIC   R1,SFMIDH+5         GET LENGTH OF COMPARE                        
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SFMID+1                                                  
         BNE   PRSEQ                                                            
*                                                                               
PR30     DS    0H                                                               
         CLI   LINE,55                                                          
         BL    PRML                                                             
*                                                                               
         OC    ABOX,ABOX           PRINT BOTTOM OF BOX                          
         BZ    PR35                                                             
         L     R7,ABOX                                                          
         USING BOXD,R7                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
         DROP  R7                                                               
         BAS   RE,PRTLINE          PAGE BREAK, PRODUCT TOO LONG                 
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 CLUNPK,DMCB,PRGKCLT,H2+60                                        
         MVC   H3+60(1),PRGKID                                                  
         BAS   RE,PRTLINE                                                       
         BAS   RE,PRTLINE                                                       
         MVC   P,SVRLINE                                                        
         BAS   RE,PRTLINE                                                       
*                                                                               
PRML     OC    ABOX,ABOX           PRINT MIDDLE OF BOX                          
         BZ    PR35                                                             
         L     R7,ABOX                                                          
         USING BOXD,R7                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         DROP  R7                                                               
         BAS   RE,PRTLINE                                                       
*                                                                               
PR35     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PRGEL10,R6                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),PRGKGRP                                                  
         UNPK  WORK(5),FULL(3)                                                  
         ZIC   R1,IDLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RCODE(0),WORK                                                    
         MVC   RBREAK1,PRGNAM1                                                  
         MVC   RBREAK2,PRGNAM2                                                  
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY             READ PASSIVE KEY AND PRINT LIST              
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,SAVEKEY+2    AGY/MED                                    
         MVC   PRGPCLT,SAVEKEY+3     CLIENT                                     
         MVC   PRGPID(3),SAVEKEY+5   GROUP ID                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         LA    R7,RPRD                                                          
         ZAP   HALF,=P'18'         SET FOR 18 CLTS PER LINE                     
         MVI   PLFLAG,C'Y'                                                      
         B     PR50                                                             
*                                                                               
PR40     GOTO1 SEQ                                                              
*                                                                               
PR50     CLC   KEY(8),KEYSAVE      TEST SAME THRU CLTGRP                        
         BNE   PR60                                                             
         MVI   PLFLAG,C'Y'                                                      
*                                                                               
         MVC   0(3,R7),PRGPPRD                                                  
         LA    R7,4(R7)                                                         
         SP    HALF,=P'1'          ADJUST COUNTER                               
         BP    PR40                CONTINUE IF MORE ROOM                        
*                                                                               
         BAS   RE,PRTLINE                                                       
         ZAP   HALF,=P'18'         CAN FIT 18 CLIENT CODES PER LINE             
         LA    R7,RPRD                                                          
         MVI   PLFLAG,C'N'                                                      
         B     PR40                                                             
*                                                                               
PR60     DS    0H                                                               
         CLI   PLFLAG,C'N'                                                      
         BE    PR75                                                             
*                                                                               
PR70     DS    0H                                                               
         BAS   RE,PRTLINE                                                       
*                                                                               
PR75     MVI   PLFLAG,C'Y'                                                      
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         B     PRSEQ                                                            
*                                                                               
PR80     DS    0H                                                               
         OC    ABOX,ABOX           PRINT BOTTOM OF BOX                          
         BZ    PRX                                                              
         L     R7,ABOX                                                          
         USING BOXD,R7                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
         DROP  R7                                                               
         BAS   RE,PRTLINE                                                       
*                                                                               
PRX      B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT LINE                                                                    
***********************************************************************         
PRTLINE  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
***********************************************************************         
* HEADER ROUTINES                                                               
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK PRODUCT GROUP RECORDS'                           
         SSPEC H2,54,C'CLIENT'                                                  
         SSPEC H3,58,C'ID'                                                      
         SSPEC H4,46,C'-----------------------------'                           
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
HHOOK    NTR1                                                                   
         L     R3,ABOX             A(BOX DSECT)                                 
         LTR   R3,R3                                                            
         BZ    HHOOKX                                                           
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+7,C'T'                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+5,C'C'                                                   
         MVI   BOXCOLS+30,C'C'                                                  
         MVI   BOXCOLS+55,C'C'                                                  
         MVI   BOXCOLS+131,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
HHOOKX   B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SECURITY ACCESS:                                                              
*   SAME ACCESS AS FILE PROGRAM X'19'                                           
*   200F AND 600F DISPLAY AND LIST ONLY                                         
*   F10F DISPLAY ONLY                                                           
*   ALL OTHERS HAVE ALL ACTIONS                                                 
***********************************************************************         
SECURITY NTR1                                                                   
         CLI   TERMINFO,C'Y'                                                    
         BE    SEC35                                                            
         MVI   TERMINFO,C'Y'                                                    
         GOTO1 GETFACT             GET TERMINAL INFO                            
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
*                                                                               
         XC    KEY,KEY             GET TERIMINAL RECORD                         
         LA    R6,KEY                                                           
         USING CTTREC,R6                                                        
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKTID,FASYM                                                    
         DROP  R1,R6                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R6,AIO                                                           
         USING CTTREC,R6                                                        
         CLC   CTTKEY,KEY                                                       
         BE    SEC10                                                            
         DC    H'0',C'TERM REC NOT FOUND'                                       
*                                                                               
SEC10    DS    0H                                                               
         MVC   DATADISP,=Y(CTTDATA-CTTREC)                                      
         DROP  R6                                                               
         MVI   ELCODE,X'21'        SYSTEM AUTHORIZATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   SECX                                                             
         USING CTSYSD,R6                                                        
SEC15    CLI   CTSYSNUM,X'03'      NETWORK                                      
         BE    SEC20                                                            
         BAS   RE,NEXTEL                                                        
         BNE   SECX                                                             
         B     SEC15                                                            
*                                                                               
SEC20    DS    0H                                                               
         CLI   CTSYSLEN,16         ANY AUTHORIZATION LEVELS??                   
         BNH   SECX                                                             
         LA    R7,CTSYSL1Q(R6)                                                  
SEC25    CLI   0(R7),X'19'         FILE PROGRAM                                 
         BE    SEC30                                                            
         LA    R7,L'CTSYSPGM(R7)                                                
         LR    R4,R7                                                            
         SR    R4,R6                                                            
         STC   R4,COUNT                                                         
         CLC   COUNT,CTSYSLEN                                                   
         BNL   SECX                                                             
         B     SEC25                                                            
*                                                                               
SEC30    DS    0H                                                               
         MVC   AUTHCODE,1(R7)                                                   
SEC35    CLC   =X'200F',AUTHCODE   DISPLAY AND LIST ONLY                        
         BE    SECDL                                                            
         CLC   =X'600F',AUTHCODE   DISPLAY AND LIST ONLY                        
         BE    SECDL                                                            
         CLC   =X'F10F',AUTHCODE   DISPLAY ONLY                                 
         BE    SECD                                                             
         B     SECX                                                             
*                                                                               
SECDL    DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    SECX                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   SECD                                                             
         CLI   MODE,VALREC         ONLY SELECT FROM LIST                        
         BE    INVLACT                                                          
         CLI   MODE,RECDEL         ONLY SELECT FROM LIST                        
         BE    INVLACT                                                          
         B     SECX                                                             
SECD     CLI   ACTNUM,ACTDIS                                                    
         BE    SECX                                                             
         CLI   ACTNUM,ACTREP                                                    
         BNE   INVLACT                                                          
*                                                                               
SECX     MVC   DATADISP,=H'24'     RESET DATADISP FOR SPOT FILE                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVLFLD  DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
INVLACT  DS    0H                                                               
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
MISSFLD  DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
DELERR   DS    0H                                                               
         MVI   ERROR,INVACT        DELETE NOT ALLOWED                           
         B     TRAPERR                                                          
PFULLERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'CANNOT ASSIGN, PRODUCT RECORD FULL'               
         GOTO1 ERREX2                                                           
PDEFERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'DEFINITION RECORD NOT FOUND'                      
         GOTO1 ERREX2                                                           
PASSERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'ASSIGNMENT RECORD NOT FOUND'                      
         GOTO1 ERREX2                                                           
INVLPRD  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(15),=C'INVALID PRODUCT'                                  
         GOTO1 ERREX2                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMD3D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMD5D                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
         PRINT ON                                                               
*                           *******  T31C40 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
PREVFLAG DS    CL1                                                              
SVBKLNS  DS    CL6                                                              
DASAVE   DS    CL4                                                              
PREVKEY  DS    CL48                                                             
SAVEKEY  DS    CL48                                                             
IDFILTER DS    CL1                                                              
IDLEN    DS    CL1                                                              
PLFLAG   DS    CL1                                                              
SVRLINE  DS    CL132                                                            
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
TERMINFO DS    C                   Y/N                                          
COUNT    DS    X                                                                
AUTHCODE DS    CL2                                                              
GROUP    DS    CL3                                                              
WORKEND  EQU   *                                                                
*                                                                               
PLINED   DSECT                                                                  
         DS    CL2                                                              
PLCLT    DS    CL3                                                              
         DS    CL2                                                              
PLGRPID  DS    CL4                                                              
         DS    CL3                                                              
PLLEV1   DS    CL24                                                             
         DS    CL2                                                              
PLLEV2   DS    CL24                                                             
*                                                                               
RLINED   DSECT                                                                  
         DS    CL1                                                              
RCODE    DS    CL4                                                              
         DS    CL1                                                              
RBREAK1  DS    CL24                                                             
         DS    CL1                                                              
RBREAK2  DS    CL24                                                             
         DS    CL1                                                              
RPRD     DS    CL70                                                             
         DS    CL6                                                              
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
PRDRECD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE CTGENFILE                                                      
 END                                                                            
