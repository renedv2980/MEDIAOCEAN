*          DATA SET NESFM12    AT LEVEL 207 AS OF 10/31/05                      
*PHASE T31C12A                                                                  
T31C12   TITLE 'NESFM12 - REP RECORDS'                                          
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
*                                                                               
         GOTO1 VTERMACC            CHECK FOR DISP/LIST ONLY TERMINALS           
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    MAIN15                                                           
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    MAIN15                                                           
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    NOTAUTH                                                          
         CLI   ACTNUM,ACTCHA       ACTION CHANGE?                               
         BE    NOTAUTH                                                          
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         BNE   MAIN15                                                           
         B     NOTAUTH                                                          
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
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         LA    R5,HEDSPECS                                                      
         ST    R5,SPECS                                                         
         B     LR                                                               
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
         BZ    INVERR                                                           
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
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   RNAME,8(R2)                                                      
*                                                                               
         LA    R2,REPSTADH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   R1LINE,8(R2)                                                     
*                                                                               
         LA    R2,REPCITYH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   R2LINE,8(R2)                                                     
*                                  STATE CODE NOW AFTER ZIP LOGIC               
VR40     XC    RZIP,RZIP                                                        
         LA    R2,REPCOUNH                                                      
         CLI   8(R2),C'C'                                                       
         BNE   VR50                                                             
         MVC   RZIP,=C'CANAD'                                                   
*                                                                               
VR50     LA    R2,REPSTATH                                                      
         TM    4(R2),X'04'          ALPHA                                       
         BZ    INVERR                                                           
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLC   RZIP,=C'CANAD'      SEE IF CANADIAN STATION                      
         BE    VR60                ACCEPT 2 OR 3 CHARS                          
         CLI   5(R2),2                                                          
         BNE   INVERR                                                           
         B     VR70                                                             
*                                                                               
VR60     CLI   5(R2),2                                                          
         BL    INVERR                                                           
         CLI   5(R2),3                                                          
         BH    INVERR                                                           
VR70     MVC   R3LINE(3),8(R2)                                                  
         OC    R3LINE,SPACES                                                    
*                                                                               
VR80     LA    R2,REPBZIPH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   RBIGZIP,8(R2)                                                    
*                                                                               
VR90     LA    R2,REPNTWKH                                                      
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
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    DK10                                                             
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    DK10                                                             
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BE    NOTAUTH                                                          
*                                                                               
DK10     XC    REPMED,REPMED                                                    
         MVC   REPMED,REPKMED                                                   
         OI    REPMEDH+6,X'80'                                                  
*                                                                               
         XC    REPREP,REPREP                                                    
         MVC   REPREP,REPKREP                                                   
         OI    REPREPH+6,X'80'                                                  
*                                                                               
DKX      B     XIT                                                              
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
         CLI   MODE,PRINTREP                                                    
         BNE   LR220                                                            
         MVC   P,SPACES                                                         
         MVC   PREP,REPKREP                                                     
         MVC   PREPN,RNAME                                                      
         MVC   PSTRT,R1LINE                                                     
         MVC   PCITY,R2LINE                                                     
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR20                                                             
*                                                                               
LR220    GOTO1 LISTMON                                                          
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
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'REP LIST'                                                
         SSPEC H2,30,C'--------'                                                
         SPACE 1                                                                
         SSPEC H4,2,C'REP'                                                      
         SSPEC H5,2,C'---'                                                      
         SSPEC H4,17,C'REP NAME'                                                
         SSPEC H5,11,C'----------------------'                                  
         SSPEC H4,53,C'ADDRESS'                                                 
         SSPEC H5,40,C'---------------------------------------'                 
         DC    X'00'                                                            
         EJECT                                                                  
                                                                                
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
NUMERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'NUMERIC DATA ONLY IN THIS FIELD'                  
         B     MSGERR                                                           
*                                                                               
ALPMERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'ALPHABETIC DATA ONLY IN THIS FIELD'               
         B     MSGERR                                                           
*                                                                               
NOTAUTH  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(21),=C'ACTION NOT AUTHORIZED'                            
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
RELO     DS    A                                                                
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
SPOOLD    DSECT                                                                 
          ORG   P                                                               
          DS    CL1                                                             
PREP      DS    CL3                                                             
          DS    CL6                                                             
PREPN     DS    CL20                                                            
          DS    CL9                                                             
PSTRT     DS    CL24                                                            
          DS    CL2                                                             
PCITY     DS    CL15                                                            
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
SAVEKEY  DS    CL(L'REPKEY)                                                     
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
**PAN#1  DC    CL21'207NESFM12   10/31/05'                                      
         END                                                                    
