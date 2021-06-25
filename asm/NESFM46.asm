*          DATA SET NESFM46    AT LEVEL 122 AS OF 10/31/05                      
*PHASE T31C46A,*                                                                
***********************************************************************         
*                                                                               
*  TITLE: T31C46 - MAINTENANCE/LIST OF I2COM RECORDS                            
*                                                                               
***********************************************************************         
         TITLE 'T31C46 NETWORK 12COM RECORD'                                    
T31C46   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,I2COM,R7                                                       
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
         BAS   RE,SETXFIL          SET TO XSPDIR/XSPFIL                         
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
* SET FILE TO XSPDIR/XSPFIL                                                     
***********************************************************************         
SETXFIL  NTR1                                                                   
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SIZEIO,=F'3975'                                                  
         MVC   SYSFIL,=C'XSPFIL'                                                
         MVC   SYSDIR,=C'XSPDIR'                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET FILE TO SPTDIR/SPTFIL                                                     
***********************************************************************         
SETSFIL  NTR1                                                                   
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SIZEIO,=A(LIOS)                                                  
         MVC   SYSFIL,=C'SPTFIL'                                                
         MVC   SYSDIR,=C'SPTDIR'                                                
         B     EXIT                                                             
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
*                                                                               
         LA    R2,LI2PRDH                                                       
         CLI   5(R2),0                                                          
         BNE   INVLFLD             NO MORE FIELDS ALLOWED                       
         LA    R2,LI2ESTH                                                       
         CLI   5(R2),0                                                          
         BNE   INVLFLD                                                          
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK PRODUCT GROUP                                                           
***********************************************************************         
CHKPRDG  NTR1                                                                   
         BAS   RE,SETSFIL          SET TO SPOTFILE                              
*                                                                               
         CLI   5(R2),5                                                          
         BL    INVLFLD                                                          
*                                                                               
         BAS   RE,GETGRP                                                        
         MVC   SVPRD1,FULL                                                      
*                                                                               
         CLI   HALF+1,0                                                         
         BE    CPG10                                                            
         OC    FULL+1(2),FULL+1    TEST GROUP NUM ENTERED                       
         BZ    INVLFLD                                                          
         BAS   RE,RDPGRDEF                                                      
*                                                                               
CPG10    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),SVMED          A/M                                      
         MVC   KEY+3(2),SVCLT          CLT                                      
         MVC   KEY+5(3),SVPRD1         PGRP                                     
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE          TY/A-M/CLT/PGRPID                        
         BNE   INVLFLD                                                          
*                                                                               
         CLI   HALF+1,0                TEST ADDING DEFAULT                      
         BE    CHKPRDGX                                                         
         CLC   KEY(13),KEYSAVE         TY/A-M/CLT/PGRPID                        
         BNE   INVLFLD                                                          
*                                                                               
CHKPRDGX DS    0H                                                               
         BAS   RE,SETXFIL          SET BACK TO XSPFILE                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*    CHECK THAT NUMBER OF INPUT DIGITS = BREAK 1 DIGITS                         
*    HALF+1(1) HAS NUMBER OF DIGITS INPUT FOR GROUP                             
***********************************************************************         
RDPGRDEF NTR1                                                                   
         BAS   RE,SETSFIL                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),SVMED      A-M/CLT/PGRPID                               
         MVC   KEY+3(2),SVCLT                                                   
         MVC   KEY+5(1),FULL                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLFLD                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING PRGRECD,R4                                                       
         LA    R6,PRGEL                                                         
         USING PRGEL01,R6                                                       
*                                                                               
         CLC   PRGBK1LN(1),HALF+1                                               
         BNE   INVLFLD                                                          
*                                                                               
         MVC   AIO,AIO1                                                         
         BAS   RE,SETXFIL                                                       
*                                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GET GROUP CODE - FORMAT IS A999                                               
***********************************************************************         
GETGRP   NTR1                                                                   
         LA    R4,12(R2)           BUMP PAST PGR=                               
         XC    FULL,FULL                                                        
         XC    HALF,HALF                                                        
*                                                                               
         ZIC   R5,5(R2)                                                         
         SH    R5,=H'4'            SUBTRACT PGR=                                
         BZ    GGX                                                              
         CH    R5,=H'4'            MAX OF 3 CHARS                               
         BH    INVLFLD                                                          
         CLI   0(R4),C'A'                                                       
         BL    INVLFLD                                                          
         CLI   0(R4),C'Z'                                                       
         BH    INVLFLD                                                          
         MVC   FULL(1),0(R4)       MOVE GROUP ID                                
*                                                                               
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BZ    GGX                                                              
*                                                                               
         CLC   0(3,R4),=C'999'   MAY NOT ENTER ALL 9'S                          
         BE    INVLFLD                                                          
*                                                                               
         STC   R5,HALF+1           RETURN NUMBER OF DIGITS ENTERED              
         STM   R4,R5,WORK                                                       
*                                                                               
GG10     DS    0H                                                               
         CLI   0(R4),C'0'                                                       
         BL    INVLFLD                                                          
         CLI   0(R4),C'9'                                                       
         BH    INVLFLD                                                          
         LA    R4,1(R4)                                                         
         BCT   R5,GG10                                                          
*                                                                               
         LM    R4,R5,WORK                                                       
         XC    WORK,WORK                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4) *EXECUTED*                                         
         PACK  FULL+1(3),WORK(5)   GET DIGITS LEFT ALIGNED                      
*                                                                               
GGX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         XC    SVMED,SVMED                                                      
         XC    SVCLT,SVCLT                                                      
         XC    SVPRD1,SVPRD1                                                    
         XC    SVPRD2,SVPRD2                                                    
         XC    SVEST1,SVEST1                                                    
         XC    SVEST2,SVEST2                                                    
         XC    SVNET,SVNET                                                      
         XC    SVMOYR,SVMOYR                                                    
*                                                                               
         LA    R2,LI2MEDIH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         CLI   8(R2),C'C'                                                       
         BE    INVLFLD                                                          
*                                                                               
         GOTO1 VALIMED                                                          
         MVC   SVMED,BAGYMD        MEDIA/AGY                                    
*                                                                               
         LA    R2,LI2CLTH                                                       
         CLI   5(R2),0                                                          
         BE    VK50                                                             
*                                                                               
         BAS   RE,CHKOFF           CHECK IF OFFICE CODE IS VALID                
         BE    VK50                                                             
         GOTO1 VALICLT                                                          
         MVC   SVCLT,BCLT                                                       
*                                                                               
         XC    SVPRD1,SVPRD1                                                    
         XC    SVPRD2,SVPRD2                                                    
         XC    SVEST1,SVEST1                                                    
         XC    SVEST2,SVEST2                                                    
         XC    SVNET,SVNET                                                      
         XC    SVMOYR,SVMOYR                                                    
*                                                                               
         LA    R2,LI2PRDH                                                       
         CLI   5(R2),0                                                          
         BE    VK40                                                             
*                                                                               
         CLC   =C'PGR=',8(R2)                                                   
         BNE   *+12                                                             
         BAS   RE,CHKPRDG          CHECK PRODUCT GROUP                          
         B     VK50                                                             
*                                                                               
VK30     DS    0H                                                               
         GOTO1 VALIPRD                                                          
         CLC   =C'ALL',QPRD                                                     
         BE    *+10                                                             
         MVC   SVPRD1,QPRD                                                      
*                                                                               
         LA    R2,LI2ESTH                                                       
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK40                                                             
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         CH    RE,=H'1'            TEST IN RANGE 1-255                          
         BL    INVLFLD                                                          
         CH    RE,=H'255'                                                       
         BH    INVLFLD                                                          
         STC   RE,SVEST1           SET BINARY ESTIMATE                          
*                                                                               
VK40     DS    0H                                                               
         LA    R2,LI2STATH                                                      
         CLI   5(R2),0                                                          
         BE    VK50                                                             
*                                                                               
         GOTO1 VALINTWK                                                         
         MVC   SVNET,BMKTSTA+2                                                  
*                                                                               
VK50     DS    0H                                                               
         LA    R2,LI2MYRH                                                       
         CLI   5(R2),0                                                          
         BE    VK60                                                             
*                                                                               
         GOTO1 DATVAL,DMCB,(2,8(R2)),SVMOYR                                     
         CLC   =C'000000',SVMOYR                                                
         BE    INVLFLD                                                          
*                                                                               
VK60     DS    0H                                                               
         LA    R2,LI2PR2H                                                       
         CLI   5(R2),0                                                          
         BE    VK90                                                             
*                                                                               
         CLC   =C'PGR=',8(R2)                                                   
         BNE   VK65                                                             
         BAS   RE,CHKPRDG                                                       
         B     VK70                                                             
*                                                                               
VK65     DS    0H                                                               
         GOTO1 VALIPRD                                                          
         CLI   BPRD,X'FF'                                                       
         BNE   *+8                                                              
         MVI   BPRD,0                                                           
*                                                                               
         MVC   SVPRD2,QPRD                                                      
*                                                                               
VK70     DS    0H                                                               
         MVI   BEST,0                                                           
*                                                                               
         LA    R2,LI2ES2H                                                       
         CLI   5(R2),0                                                          
         BNE   VK75                                                             
*                                                                               
         CLC   =C'ALL',LI2CLT      IF NOT ALL CLIENTS                           
         BE    VK90                                                             
         CLC   =C'ALL',LI2PR2      OR NOT ALL PRODUCTS                          
         BE    VK90                                                             
         MVC   8(3,R2),=C'ALL'     THEN ASSUME 'ALL' ESTIMATE                   
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         B     VK90                                                             
*                                                                               
VK75     DS    0H                                                               
         CLC   =C'ALL',LI2CLT      IF ALL CLIENTS                               
         BE    INVLFLD             THEN NOTHING AFTER IS ALLOWED                
*                                                                               
         CLC   =C'ALL',8(R2)       IF ALL ESTIMATES                             
         BE    VK90                                                             
*                                                                               
         CLC   =C'ALL',LI2PR2      IF PRODUCT SPECIFIED                         
         BE    *+14                                                             
         CLC   =C'PGR=',LI2PR2     AND NOT PGROUP                               
         BNE   VK80                THEN VALIDATE THE ESTIMATE                   
*                                                                               
         TM    4(R2),X'08'         WE HAVE TO VALIDATE OURSELVES                
         BZ    INVLFLD                                                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'255'          ESTIMATE 1-255                               
         BH    INVLFLD                                                          
*                                                                               
         LTR   R1,R1                                                            
         BZ    INVLFLD                                                          
         STC   R1,BEST                                                          
         B     VK90                                                             
*                                                                               
VK80     DS    0H                                                               
         GOTO1 VALIEST             BEST NOW CONTAINS 2ND ESTIMATE               
         MVC   SVEST2,BEST                                                      
*                                                                               
VK90     DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING COMI2HD,R4          BUILD XSPOT KEY FOR I2COM                    
         MVC   COMI2K,=X'0D0C'                                                  
         MVC   COMI2KAM,SVMED                                                   
         MVI   COMI2KTY,C'I'                                                    
         MVC   COMI2KCL,SVCLT                                                   
         MVC   COMI2KPR,SVPRD1                                                  
         MVC   COMI2KES,SVEST1                                                  
         MVC   COMI2KP2,SVPRD2                                                  
         MVC   COMI2KE2,SVEST2                                                  
*                                                                               
         CLC   SVMOYR,=6X'0'                                                    
         BE    VK100                                                            
         GOTO1 DATCON,DMCB,(0,SVMOYR),(3,COMI2KYM)                              
         B     VK100                                                            
*                                                                               
VK100    DS    0H                                                               
         BAS   RE,SETXFIL                                                       
         GOTO1 HIGH                                                             
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(32),KEY                                                  
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
         USING I2LSD,R2                                                         
*                                                                               
         BAS   RE,SETXFIL                                                       
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGH                           
         BNZ   *+10                                                             
         MVC   KEY(32),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         B     LR10                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR10     DS    0H                                                               
         LA    R6,KEY                                                           
         USING COMI2HD,R6                                                       
*                                                                               
         CLC   KEY(COMI2KCL-COMI2K),SAVEKEY                                     
         BNE   LRX                                                              
*                                                                               
         OC    SVCLT,SVCLT                                                      
         BZ    LR30                                                             
         CLC   COMI2KCL,SVCLT                                                   
         BNE   LRX                                                              
*                                                                               
LR30     DS    0H                                                               
         OC    COMI2KCL,COMI2KCL                                                
         BZ    LR40                                                             
         GOTO1 CLUNPK,DMCB,COMI2KCL,I2CLT                                       
*                                                                               
         OC    COMI2KPR,COMI2KPR                                                
         BZ    *+10                                                             
         MVC   I2PRD,COMI2KPR       I2COM HAS EBCDIC PRD CODE                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,COMI2KES                                                    
         BZ    LR35                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  I2EST,DUB                                                        
*                                                                               
LR35     DS    0H                                                               
         OC    COMI2KST,COMI2KST                                                
         BZ    LR40                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+12(3),COMI2KST                                              
         GOTO1 VMSUNPK,DMCB,WORK+10,DUB,WORK                                    
         MVC   I2STA(4),WORK                                                    
*                                                                               
LR40     DS    0H                                                               
         OC    COMI2KP2,COMI2KP2                                                
         BZ    LR45                                                             
         MVC   I2PR2,COMI2KP2                                                   
*                                                                               
LR45     DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,1,COMI2KE2                                                    
         BZ    LR50                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  I2ES2,DUB                                                        
*                                                                               
LR50     DS    0H                                                               
         OC    COMI2KYM,COMI2KYM                                                
         BZ    LR55                                                             
         GOTO1 DATCON,DMCB,(3,COMI2KYM),(6,I2MYR)                               
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
         MVC   I2COM,SPACES                                                     
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   I2COM(0),2(R6)                                                   
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
       ++INCLUDE NESFM61D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM62D                                                       
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
COMI2HD  DSECT                                                                  
       ++INCLUDE SPGENXCOM                                                      
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
I2LSD    DSECT                                                                  
I2CLT    DS    CL3                                                              
         DS    CL1                                                              
I2PRD    DS    CL3                                                              
         DS    CL1                                                              
I2EST    DS    CL3                                                              
         DS    CL1                                                              
I2PR2    DS    CL3                                                              
         DS    CL1                                                              
I2ES2    DS    CL3                                                              
         DS    CL1                                                              
I2STA    DS    CL5                                                              
         DS    CL1                                                              
I2MYR    DS    CL6                                                              
         DS    CL2                                                              
I2COM    DS    CL40                                                             
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122NESFM46   10/31/05'                                      
         END                                                                    
