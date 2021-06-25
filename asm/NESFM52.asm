*          DATA SET NESFM52    AT LEVEL 131 AS OF 10/31/05                      
*PHASE T31C52A,*                                                                
***********************************************************************         
*                                                                               
*  TITLE: T31C52 - MAINTENANCE/LIST OF NVTEXT                                   
*                                                                               
***********************************************************************         
         TITLE 'T31C52 NETWORK 12COM RECORD'                                    
T31C52   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NVTEXT,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC/PUTREC                 
*                                                                               
         GOTO1 VTERMACC            CHECK FOR DISP/LIST ONLY TERMINALS           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+12                                                             
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CHECK IF OFFICE CODE                                                          
***********************************************************************         
CHKOFF   NTR1                                                                   
         CLI   5(R2),2                                                          
         BNE   NO                                                               
         CLI   8(R2),C'*'                                                       
         BNE   NO                                                               
         CLI   9(R2),C'A'                                                       
         BL    INVLFLD                                                          
         CLI   9(R2),C'Z'                                                       
         BNH   CHKO10                                                           
         CLI   9(R2),C'0'                                                       
         BL    INVLFLD                                                          
         CLI   9(R2),C'9'                                                       
         BH    INVLFLD                                                          
*                                                                               
CHKO10   DS    0H                                                               
         MVC   SVCLT,8(R2)         OFFICE CODE IS VALID                         
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         XC    SVMED,SVMED                                                      
         XC    SVCLT,SVCLT                                                      
*                                                                               
         LA    R2,LNVMEDIH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALIMED                                                          
         MVC   SVMED,BAGYMD        MEDIA/AGY                                    
*                                                                               
         LA    R2,LNVCLTH                                                       
         CLI   5(R2),0                                                          
         BE    VK90                                                             
*                                                                               
         BAS   RE,CHKOFF           CHECK IF OFFICE CODE IS VALID                
         BE    VK90                                                             
         GOTO1 VALICLT                                                          
         MVC   SVCLT,BCLT                                                       
*                                                                               
VK90     DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING COMHDRD,R4                                                       
         MVC   COMKTYPE,=X'0D0C'                                                
         MVC   COMKAGY,SVMED                                                    
         MVI   COMCTYPE,C'N'                                                    
         MVC   COMKCLT,SVCLT                                                    
*                                                                               
VK100    DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(13),KEY                                                  
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
*                                                                               
DKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE RECORD                                                                 
***********************************************************************         
RDEL     DS    0H                                                               
*                                                                               
RDELX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
*                                                                               
VRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS *                                                                
***********************************************************************         
LR       DS    0H                                                               
         USING NVTLSD,R2                                                        
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGH                           
         BNZ   *+10                                                             
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         B     LR10                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR10     DS    0H                                                               
         LA    R6,KEY                                                           
         USING COMHDRD,R6                                                       
*                                                                               
         CLC   KEY(4),SAVEKEY                                                   
         BNE   LRX                                                              
*                                                                               
         OC    SVCLT,SVCLT                                                      
         BZ    LR30                                                             
         CLC   COMKCLT,SVCLT                                                    
         BNE   LRX                                                              
*                                                                               
LR30     DS    0H                                                               
         OC    COMKCLT,COMKCLT                                                  
         BZ    LR55                                                             
         GOTO1 CLUNPK,DMCB,COMKCLT,NVTCLT                                       
*                                                                               
LR55     DS    0H                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    LR60                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR70                                                             
*                                                                               
LR60     DS    0H                                                               
         MVC   NVTCOM,SPACES                                                    
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NVTCOM(0),2(R6)                                                  
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR65                                                             
         GOTO1 LISTMON                                                          
         MVC   LISTAR,SPACES                                                    
         B     LR70                                                             
*                                                                               
LR65     DS    0H                                                               
         GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR70     B     LRSEQ                                                            
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     DMYES                                                            
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     DMYES                                                            
*                                                                               
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     DMYES                                                            
*                                                                               
MYDIRADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     DMYES                                                            
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    DMNO                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
DMYES    SR    R1,R1                                                            
         B     *+8                                                              
DMNO     LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVLCLI  MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM91D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM92D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C45 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
MYDMWRK  DS    12D                                                              
PREVFLAG DS    CL1                                                              
PREVKEY  DS    CL48                                                             
SAVEKEY  DS    CL48                                                             
*                                                                               
SVMED    DS    XL1                                                              
SVCLT    DS    XL2                                                              
SVPRD1   DS    CL3                                                              
SVPRD2   DS    CL3                                                              
SVEST1   DS    XL1                                                              
SVEST2   DS    XL1                                                              
SVNET    DS    XL3                                                              
SVMOYR   DS    CL6                                                              
*                                                                               
*                                                                               
SYSSW    DS    CL1                                                              
SWDSYS   DS    CL1                                                              
POWCODE  DS    CL2                                                              
ACCOFF   DS    CL2                                                              
OFFLEN   DS    CL1                                                              
COMPCD   DS    CL1                                                              
OLDCOPT2 DS    CL1                                                              
GTFACTB  DS    CL88                                                             
WORKEND  EQU   *                                                                
         EJECT                                                                  
*                                                                               
LIOS     EQU   4000                                                             
*                                                                               
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
*                                                                               
       ++INCLUDE SPGENCLG                                                       
       ++INCLUDE SPGENAGY                                                       
*                                                                               
PRDRECD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
PRGRECD  DSECT                                                                  
       ++INCLUDE SPGENPRG                                                       
*                                                                               
NVTLSD   DSECT                                                                  
         DS    CL1                                                              
NVTCLT   DS    CL3                                                              
         DS    CL20                                                             
NVTCOM   DS    CL40                                                             
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'131NESFM52   10/31/05'                                      
         END                                                                    
