*          DATA SET NESFM12S   AT LEVEL 188 AS OF 05/01/02                      
*PHASE T31C12A                                                                  
T31C12   TITLE 'NESFM06 - REP RECORDS'                                          
***********************************************************************         
T31C12   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C12,R7,RR=R8                                                
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         B     MAIN15                                                           
RELO     DS    A                                                                
*                                                                               
MAIN15   BAS   RE,SETUP                                                         
         CLI   MODE,SETFILE        SET FILE NAME                                
         BE    SF                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
DLCHK    CLI   MODE,RECDEL         DELETE RECORDS                               
         BNE   LRCHK                                                            
         BAS   RE,DL                                                            
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         B     XIT                                                              
**************************************                                          
* SET FILE                                                                      
SF       DS    0H                                                               
         OI    GENSTAT4,NODELLST                                                
         BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF           SET FILENAME & OTHER DEFAULTS                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
DL       NTR1                                                                   
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
********************************************************************            
* VALIDATE KEY                                                                  
********************************************************************            
VK       XC    SVQMED,SVQMED                                                    
*                                                                               
         LA    R2,REPMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 VALIMED                                                          
         MVC   SVQMED,QMED                                                      
*                                                                               
         LA    R2,REPREPH                                                       
         CLI   5(R2),0                                                          
         BNE   VK30                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+14                                                             
         MVC   SVQREP,ZEROES                                                    
         B     VK50                                                             
         B     MISSERR                                                          
*                                                                               
VK30     TM    4(R2),X'08'                                                      
         BZ    MISSERR                                                          
*                                                                               
         CLI   5(R2),3                                                          
         BNE   INVERR                                                           
         MVC   SVQREP,8(R2)                                                     
*                                                                               
*   BUILD THE KEY                                                               
VK50     LA    R4,KEY                                                           
         USING REPRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   REPKEY,ZEROES                                                    
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,QMED                                                     
         MVC   REPKREP,SVQREP                                                   
         MVC   REPKAGY,AGENCY                                                   
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF                                                        
*                                                                               
VKX      B     XIT                                                              
         DROP  R4                                                               
***************************************************************                 
*================ VAL REC ====================================*                 
VR       L     R6,AIO                                                           
         USING REPRECD,R6                                                       
*                                                                               
         LA    R2,REPNAMEH                                                      
         MVC   RNAME,8(R2)                                                      
*                                                                               
         LA    R2,REPSTADH                                                      
         MVC   R1LINE,8(R2)                                                     
*                                                                               
         LA    R2,REPCITYH                                                      
         MVC   R2LINE,8(R2)                                                     
*                                  STATE CODE NOW AFTER ZIP LOGIC               
VR40     XC    RZIP,RZIP                                                        
         LA    R2,REPCOUNH                                                      
         CLI   8(R2),C'C'                                                       
         BNE   EDT4D                                                            
         MVC   RZIP,=C'CANAD'                                                   
*                                                                               
EDT4D    LA    R2,REPSTATH                                                      
         TM    4(R2),X'04'          ALPHA                                       
         BZ    INVERR                                                           
         CLC   RZIP,=C'CANAD'      SEE IF CANADIAN STATION                      
         BE    EDT4F               ACCEPT 2 OR 3 CHARS                          
         CLI   5(R2),2                                                          
         BNE   INVERR                                                           
         B     EDT4X                                                            
*                                                                               
EDT4F    CLI   5(R2),2                                                          
         BL    INVERR                                                           
         CLI   5(R2),3                                                          
         BH    INVERR                                                           
EDT4X    MVC   R3LINE(3),8(R2)                                                  
         OC    R3LINE,SPACES                                                    
*                                                                               
EDT5     LA    R2,REPBZIPH                                                      
         CLI   5(R2),0                                                          
         BE    INVERR                                                           
         MVC   RBIGZIP,8(R2)                                                    
*                                                                               
EDT6     LA    R2,REPNTWKH                                                      
         MVI   RUNWNET,C'N'                                                     
         CLI   8(R2),C'Y'                                                       
         BNE   *+10                                                             
         MVC   RUNWNET,8(R2)                                                    
*                                                                               
VRX      MVC   REPRLEN,=H'144'                                                  
         B     DR                                                               
***********************************************************                     
*      DISPLAY REC                                                              
***********************************************************                     
DR       L     R6,AIO                                                           
         USING REPRECD,R6                                                       
*  REP NAME                                                                     
         LA    R2,REPNAMEH                                                      
         MVC   REPNAME,RNAME                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,REPSTADH                                                      
         MVC   REPSTAD,R1LINE                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,REPCITYH                                                      
         MVC   REPCITY,R2LINE                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,REPSTATH                                                      
         MVC   REPSTAT,R3LINE                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,REPBZIPH                                                      
         MVC   REPBZIP,RBIGZIP                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,REPCOUNH                                                      
         MVI   8(R2),C'U'                                                       
         CLC   RZIP,=C'CANAD'                                                   
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,REPNTWKH                                                      
         MVI   8(R2),C'N'                                                       
         CLI   RUNWNET,C'Y'                                                     
         BNE   *+8                                                              
         MVI   8(R2),C'Y'                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
DR220    BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF                                                        
                                                                                
DRX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
***********************************************************************         
* ================   DISP KEY    ====================================*          
***********************************************************************         
DK       L     R6,AIO                                                           
         USING REPRECD,R6                                                       
*                                                                               
         XC    REPMED,REPMED                                                    
         MVC   REPMED,REPKMED                                                   
         OI    REPMEDH+6,X'80'                                                  
*                                                                               
         XC    REPREP,REPREP                                                    
         MVC   REPREP,REPKREP                                                   
         OI    REPREPH+6,X'80'                                                  
*                                                                               
         B     XIT                                                              
***********************************************************************         
* ================   LIST RECS   ====================================*          
***********************************************************************         
*                                                                               
LR       OC    KEY,KEY                                                          
         BNZ   LR10                                                             
*                                                                               
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
LR30     L     R6,AIO                                                           
         USING REPRECD,R6                                                       
         CLC   0(2,R6),KEYSAVE       SAME MEDIA?                                
         BNE   LRX                                                              
         CLC   REPKAGY,AGENCY       SAME AGENCY?                                
         BNE   LR20                                                             
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTREP,REPKREP                                                  
         MVC   LISTREPN,RNAME                                                   
         MVC   LISTSTRT,R1LINE                                                  
         MVC   LISTCITY,R2LINE                                                  
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR20                                                             
LRX      B     XIT                                                              
         DROP  R6                                                               
*                                                                               
******************************************************                          
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DEL ALLOWED                               
         OI    GENSTAT2,DISTHSPG                                                
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
                                                                                
SAVEDEF  DS    0H                  SAVE DEFINITION BEFORE SETDEF                
         MVC   MYSYSDIR,SYSDIR                                                  
         MVC   MYSYSFIL,SYSFIL                                                  
         MVC   MYUSEIO,USEIO                                                    
         MVC   MYACELOP,ACTELOPT                                                
         MVC   MYLKEY,LKEY                                                      
         BR    RE                                                               
*                                                                               
SETDEF   MVC   SYSDIR,=C'STATION '      SET TO READ STATION FILE                
         MVC   SYSFIL,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVI   ACTELOPT,C'N'            NO ACTIVITY ELEMENTS                    
         MVC   LKEY,=H'15'              SET LENGTH OF STATION KEY               
         BR    RE                                                               
*                                                                               
RSTRDEF  DS    0H                  RESTORE DEFINITION AFTER SETDEF              
         MVC   SYSDIR,MYSYSDIR                                                  
         MVC   SYSFIL,MYSYSFIL                                                  
         MVC   USEIO,MYUSEIO                                                    
         MVC   ACTELOPT,MYACELOP                                                
         MVC   LKEY,MYLKEY                                                      
         BR    RE                                                               
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
NODAYERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=C'ENTER (Y) OR LEAVE BLANK'                         
         B     MSGERR                                                           
*                                                                               
WTSHRERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'ENTER VALUE (1 - 99.99) OR LEAVE BLANK'           
         B     MSGERR                                                           
*                                                                               
NUMERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'NUMERIC DATA ONLY IN THIS FIELD'                  
         B     MSGERR                                                           
*                                                                               
ALPMERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'ALPHABETIC DATA ONLY IN THIS FIELD'               
         B     MSGERR                                                           
*                                                                               
MKTINV   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(14),=C'INVALID MARKET'                                   
         B     MSGERR                                                           
*                                                                               
DUPMERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'DUPLICATE MARKET ERROR'                           
         B     MSGERR                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
ADDERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  OC    ERRDISP,ERRDISP     DO I NEED TO OVERRIDE CURSOR POS.            
         BZ    TRAPEND                                                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         MVC   TIOBCURI,ERRDISP+1  DISPLACMENT INTO FIELD                       
*                                                                               
TRAPEND  DS    0H                                                               
         MVI   ERROPT,0            NEVER TO RETURN                              
         GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
         PRINT GEN                                                              
                                                                                
         DS    0F                                                               
ZEROES   DC    20C'0'                                                           
ERRDISP  DS    H                                                                
         LTORG                                                                  
                                                                                
         DROP  R7,RB                                                            
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*        RETURN NETWORK IN WORK+20                                              
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMF5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMF6D                                                       
         EJECT                                                                  
*                                                                               
                                                                                
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
CBLRECD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSLST                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
* CTGENFILE (NEED CTDMREC)                                                      
       ++INCLUDE CTGENFILE                                                      
                                                                                
* SPDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
         SPACE 1                                                                
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
GEND      DSECT                                                                 
          ORG   LISTAR                                                          
          DS    CL1                                                             
LISTREP   DS    CL3                                                             
          DS    CL2                                                             
LISTREPN  DS    CL20                                                            
          DS    CL2                                                             
LISTSTRT  DS    CL24                                                            
          DS    CL2                                                             
LISTCITY  DS    CL15                                                            
*                                                                               
***********************************************************************         
*===================== NESFM06 (T31C12) SAVE AREA ====================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
*                                                                               
SAVEKEY  DS    CL(L'MKTKEY)                                                     
*                                                                               
SVQMED   DS    CL1                                                              
SVQREP   DS    CL3                                                              
TEMP     DS    X                                                                
SCANTBL  DS    XL256                                                            
NLINES   DS    X                                                                
FNDX     DS    X                                                                
MYKEY    DS    CL(L'KEY)                                                        
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'188NESFM12S  05/01/02'                                      
         END                                                                    
