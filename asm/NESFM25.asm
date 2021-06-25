*          DATA SET NESFM25    AT LEVEL 022 AS OF 10/31/05                      
*PHASE T31C25A                                                                  
T31C25   TITLE 'NESFM25 - ADDRESS RECORDS'                                      
***********************************************************************         
T31C25   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C25,R7,RR=R8                                                
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
VRCHK    CLI   MODE,VALREC         VALIDATE RECORD                              
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
*                                                                               
VK       LA    R2,ADMMEDIH          MEDIA                                       
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLI   8(R2),C'*'                                                       
         BE    VK10                                                             
         GOTO1 VALIMED                                                          
         MVC   SVQMED,QMED                                                      
*                                                                               
VK10     LA    R2,ADMSTATH                                                      
         CLI   5(R2),0                                                          
         BNE   VK30                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+14                                                             
         MVC   SVQSTA,SPACES                                                    
         B     VK50                                                             
         B     MISSERR                                                          
*                                                                               
VK30     CLI   5(R2),4                                                          
         BH    INVERR                                                           
         CLI   5(R2),3                                                          
         BL    INVERR                                                           
*                                                                               
*                                                                               
*  ADDRESS BOX IS A REGULAR ADDRESS, ADDRESS BOXA-Z,1-9 IS A REMITANCE          
         CLI   5(R2),3                CHECK 3 CAHARACTER INPUT                  
         BNH   VK40                                                             
         CLC   =C'BOX',8(R2)          STATION BOX REQS MED *                    
         BNE   VK40                                                             
         CLI   ADMMEDI,C'*'                                                     
         BE    VK45                                                             
         B     INVERR                                                           
VK40     CLI   ADMMEDI,C'*'                                                     
         BNE   VK43                                                             
         BE    INVERR                                                           
*                                                                               
VK43     TM    4(R2),X'04'            ALPHANUMERIC?                             
         BZ    INVERR                                                           
VK45     XC    SVQSTA,SVQSTA                                                    
         MVC   SVQSTA,8(R2)                                                     
         OC    SVQSTA,SPACES                                                    
*                                                                               
*   BUILD THE KEY                                                               
*                                                                               
VK50     CLI   ADMMEDI,C'*'                                                     
         BNE   VK55                                                             
         MVI   QMED,C'T'                                                        
*                                                                               
VK55     LA    R4,KEY                                                           
         USING ADDRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   ADDRKEY,ZEROES                                                   
         MVI   ADDKTYPE,C'A'                                                    
         MVC   ADDKMED,QMED                                                     
         OC    ADDKCALL,SPACES                                                  
         MVC   ADDKCALL(L'SVQSTA),SVQSTA                                        
         MVC   ADDKCALL+4(L'QMED),QMED                                          
         MVC   ADDKAGY,AGENCY                                                   
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
*        BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF                                                        
*                                                                               
VKX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R4                                                               
***************************************************************                 
*================ VAL REC ====================================*                 
***************************************************************                 
VR       L     R6,AIO                                                           
         USING ADDRECD,R6                                                       
*                                                                               
         LA    R2,ADMNAMEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   ANAME,8(R2)                                                      
*                                                                               
         LA    R2,ADMADDRH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   A1LINE,8(R2)                                                     
*                                                                               
         LA    R2,ADMCITYH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVC   A2LINE,8(R2)                                                     
*                                                                               
EDT4     XC    AZIP,AZIP                                                        
         LA    R2,ADMCTRYH                                                      
         CLI   8(R2),C'C'                                                       
         BNE   EDT4D                                                            
         MVC   AZIP,=C'CANAD'                                                   
*                                                                               
EDT4D    LA    R2,ADMSTTEH                                                      
         TM    4(R2),X'04'          ALPHA                                       
         BZ    INVERR                                                           
*                                                                               
         CLC   AZIP,=C'CANAD'      SEE IF CANADIAN STATION                      
         BE    EDT4F               ACCEPT 2 OR 3 CHARS                          
         CLI   5(R2),2                                                          
         BNE   INVERR                                                           
         B     EDT4X                                                            
*                                                                               
EDT4F    CLI   5(R2),2                                                          
         BL    INVERR                                                           
         CLI   5(R2),3                                                          
         BH    INVERR                                                           
EDT4X    MVC   A3LINE(3),8(R2)                                                  
         OC    A3LINE,SPACES                                                    
         B     EDT5                                                             
EDT5     LA    R2,ADMBZIPH                                                      
         CLI   5(R2),0                                                          
         BE    INVERR                                                           
         MVC   ABIGZIP,8(R2)                                                    
*                                                                               
EDTX     MVC   ARECL,=H'144'                                                    
         B     DR                                                               
         DROP  R6                                                               
*                                                                               
***********************************************************                     
*      DISPLAY REC                                                              
***********************************************************                     
DR       L     R6,AIO                                                           
         USING ADDRECD,R6                                                       
*  REP NAME                                                                     
         LA    R2,ADMNAMEH                                                      
         MVC   ADMNAME,ANAME                                                    
         OI    6(R2),X'80'                                                      
*  STREET                                                                       
         LA    R2,ADMADDRH                                                      
         MVC   ADMADDR,A1LINE                                                   
         OI    6(R2),X'80'                                                      
*  CITY                                                                         
         LA    R2,ADMCITYH                                                      
         MVC   ADMCITY,A2LINE                                                   
         OI    6(R2),X'80'                                                      
*  STATE                                                                        
         LA    R2,ADMSTTEH                                                      
         MVC   ADMSTTE,A3LINE                                                   
         OI    6(R2),X'80'                                                      
*  ZIP CODE                                                                     
         LA    R2,ADMBZIPH                                                      
         MVC   ADMBZIP,ABIGZIP                                                  
         OI    6(R2),X'80'                                                      
*  COUNTRY                                                                      
         LA    R2,ADMCTRYH                                                      
         MVI   ADMCTRY,C'U'                                                     
         CLC   AZIP,=C'CANAD'                                                   
         BNE   *+8                                                              
         MVI   ADMCTRY,C'C'                                                     
         OI    6(R2),X'80'                                                      
*                                                                               
*R220    BAS   RE,SAVEDEF                                                       
         BAS   RE,SETDEF                                                        
                                                                                
DRX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
***********************************************************************         
* ================   DISP KEY    ====================================*          
***********************************************************************         
DK       L     R6,AIO                                                           
         USING ADDRECD,R6                                                       
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    DK10                                                             
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    DK10                                                             
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BE    NOTAUTH                                                          
*                                                                               
DK10     XC    ADMMEDI,ADMMEDI                                                  
         MVC   ADMMEDI,ADDKMED                                                  
         OI    ADMMEDIH+6,X'80'                                                 
*                                                                               
         XC    ADMSTAT,ADMSTAT                                                  
         MVC   ADMSTAT(L'ADDKCALL-1),ADDKCALL                                   
         OI    ADMSTATH+6,X'80'                                                 
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
******************************************************                          
* =============  LISTRECS ===========================*                          
******************************************************                          
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
         USING ADDRECD,R6                                                       
         CLC   0(2,R6),KEYSAVE       SAME MEDIA?                                
         BNE   LRX                                                              
         CLC   ADDKAGY,AGENCY       SAME AGENCY?                                
         BNE   LR20                                                             
*                                                                               
         CLI   ADLMED,C'*'                                                      
         BNE   LR40                                                             
         CLC   =C'BOX',ADDKCALL                                                 
         BNE   LR20                                                             
         CLI   ADDKCALL+3,X'40'                                                 
         BNH   LR20                                                             
         B     LR45                                                             
*                                                                               
LR40     CLC   =C'BOX',ADDKCALL                                                 
         BNE   LR45                                                             
         CLI   ADDKCALL+3,X'40'                                                 
         BH    LR20                                                             
LR45     MVC   LISTAR,SPACES                                                    
         MVC   LSTSTAT(L'ADDKCALL),ADDKCALL                                     
         MVI   LSTSTAT+4,C' '                                                   
         MVC   LSTNAME,ANAME                                                    
         MVC   LSTCITY,A2LINE                                                   
         MVC   LSTSTTE,A3LINE                  STATE                            
         MVC   LSTZIP,ABIGZIP                  ZIP CODE                         
         CLC   ABIGZIP,SPACES                                                   
         BH    LR50                                                             
         CLC   AZIP,=C'CANAD'                                                   
         BNE   *+10                                                             
         MVC   LSTZIP,=C'CANAD'                                                 
*                                                                               
LR50     CLI   MODE,PRINTREP                                                    
         BNE   LR80                                                             
         MVC   P,SPACES                                                         
         MVC   PSTAT(L'ADDKCALL),ADDKCALL                                       
         MVI   PSTAT+4,C' '                                                     
         MVC   PNAME,ANAME                                                      
         MVC   PCITY,A2LINE                                                     
         MVC   PSTTE,A3LINE                  STATE                              
         MVC   PZIP,ABIGZIP                  ZIP CODE                           
         CLC   ABIGZIP,SPACES                                                   
         BH    LR70                                                             
         CLC   AZIP,=C'CANAD'                                                   
         BNE   *+10                                                             
         MVC   PZIP,=C'CANAD'                                                   
LR70     GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR20                                                             
*                                                                               
LR80     GOTO1 LISTMON                                                          
         B     LR20                                                             
LRX      B     XIT                                                              
         DROP  R6                                                               
***********************************************************                     
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
         SSPEC H1,30,C'ADDRESS LIST'                                            
         SSPEC H2,30,C'------------'                                            
         SPACE 1                                                                
         SSPEC H4,3,C'STATION'                                                  
         SSPEC H5,2,C'----------'                                               
         SSPEC H4,23,C'NAME'                                                    
         SSPEC H5,17,C'-------------------'                                     
         SSPEC H4,45,C'CITY'                                                    
         SSPEC H5,39,C'-----------------------'                                 
         SSPEC H4,66,C'STATE'                                                   
         SSPEC H5,66,C'-----'                                                   
         SSPEC H4,79,C'ZIP CODE'                                                
         SSPEC H5,78,C'----------'                                              
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*                                                                               
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
RELO     DS    A                                                                
ZEROES   DC    20C'0'                                                           
ERRDISP  DS    H                                                                
*        LTORG                                                                  
                                                                                
         DROP  R7,RB                                                            
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*        RETURN NETWORK IN WORK+20                                              
*        LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMB3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMB4D                                                       
         EJECT                                                                  
*                                                                               
ADDRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
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
LSTSTAT   DS    CL8                                                             
          DS    CL2                                                             
LSTNAME   DS    CL20                                                            
          DS    CL2                                                             
LSTCITY   DS    CL24                                                            
          DS    CL2                                                             
LSTSTTE   DS    CL3                                                             
          DS    CL2                                                             
LSTZIP    DS    CL10                                                            
*                                                                               
SPOOLD    DSECT                                                                 
          ORG   P                                                               
          DS    CL1                                                             
PSTAT     DS    CL8                                                             
          DS    CL7                                                             
PNAME     DS    CL20                                                            
          DS    CL2                                                             
PCITY     DS    CL24                                                            
          DS    CL3                                                             
PSTTE     DS    CL3                                                             
          DS    CL9                                                             
PZIP      DS    CL10                                                            
*                                                                               
***********************************************************************         
*===================== NESFM25 (T31C25) SAVE AREA ====================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
*                                                                               
SAVEKEY  DS    CL(L'ADDRKEY)                                                    
*                                                                               
SVQMED   DS    CL1                                                              
SVQSTA   DS    CL5                                                              
TEMP     DS    X                                                                
SCANTBL  DS    XL256                                                            
NLINES   DS    X                                                                
FNDX     DS    X                                                                
MYKEY    DS    CL(L'KEY)                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022NESFM25   10/31/05'                                      
         END                                                                    
***********************************************************************         
