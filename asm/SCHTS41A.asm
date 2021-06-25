*          DATA SET SCHTS41A   AT LEVEL 025 AS OF 05/01/02                      
*PHASE T31C41A,+0,NOAUTO                                                        
***********************************************************************         
*                                                                               
*  TITLE: T31C41 - MAINTENANCE/LIST OF PRODUCT GROUP DEFINITIONS                
*                                                                               
*  COMMENTS: MAINTAINS PRODUCT GROUP DEFINITIONS                                
*                                                                               
*  CALLED FROM: NET SFM CONTROLLER (T31C40), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS NESFMD2 (T31CD2) -- MAINTENANCE                              
*                  NESFMD4 (T31CD4) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW BUYERS                                               
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - WORK                                                            
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
         TITLE 'NESFM41 - PRODUCT GROUP DEFINITIONS'                            
T31C41   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NETPGD                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
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
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE RECORDS                               
         BE    DELERR                                                           
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
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK10                                                             
         B     MISSFLD                                                          
*                                                                               
VK05     MVC   QCLT,SFMCLT                                                      
         OC    QCLT,SPACES                                                      
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   INVLCLI                                                          
         MVC   PRGKCLT,BCLT                                                     
*                                                                               
VK10     LA    R2,SFMIDH                                                        
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKX                                                              
         B     MISSFLD                                                          
*                                                                               
VK20     MVC   PRGKID,SFMID                                                     
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                  DISPLAY KEY                                  
         L     R3,AIO                                                           
         USING PRGRECD,R3                                                       
         MVI   SFMMED,C'N'                                                      
         OI    SFMMEDH+6,X'80'     XMIT                                         
         GOTO1 CLUNPK,DMCB,PRGKCLT,SFMCLT                                       
         OI    SFMCLTH+6,X'80'     XMIT                                         
         MVC   SFMID,PRGKID                                                     
         OI    SFMIDH+6,X'80'      XMIT                                         
*                                                                               
DKX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE KEY-NOT USED                                                           
***********************************************************************         
DELREC   DS    0H                                                               
         CLI   T31CFFD+1,C'*'                                                   
         BE    DELR05                                                           
         LA    R2,CONACTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(45),=C'ACTION DELETE - INVALID FOR NON-DDS TERMIX        
               NALS'                                                            
         OI    CONHEADH+6,X'80'    XMIT                                         
         GOTO1 ERREX2                                                           
*                                                                               
DELR05   DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    WORK2,WORK2                                                      
         MVC   KEY(PRGKGRP-PRGKTYP),DELKEY                                      
*                                                                               
         MVI   BYTE,0              DON'T PASS DELETES                           
         GOTO1 MYHIGH                                                           
         MVC   WORK2(20),KEY                                                    
         GOTO1 MYSEQ               SKIP DEFINITION RECORD                       
         CLC   KEY(PRGKGRP-PRGKTYP),DELKEY                                      
         BE    MYERR5              DEFINITION IN USE                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),WORK2                                                    
         OI    KEY+13,X'80'        DELETE DEFINITION RECORD                     
         GOTO1 MYDIRWRT                                                         
*                                                                               
         L     R5,AIO                                                           
         USING PRGRECD,R5                                                       
         MVI   BYTE,X'80'          READ FOR UPDATE                              
         GOTO1 MYGETREC                                                         
         OI    PRGCNTL,X'80'                                                    
         GOTO1 MYPUTREC            DELETE DA                                    
         DROP  R5                                                               
*                                                                               
         LA    R2,CONACTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(17),=C'SELECTION DELETED'                                
         OI    CONHEADH+6,X'80'    XMIT                                         
         GOTO1 ERREX2                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DR02                                                             
         MVC   PREVKEY,KEY         SAVE FOR LIST SELECT                         
         MVI   PREVFLAG,C'Y'                                                    
*                                                                               
DR02     L     R7,AIO                                                           
         USING PRGRECD,R7                                                       
         LA    R6,PRGEL                                                         
         USING PRGEL01,R6                                                       
*                                                                               
         MVC   SFMBK1,PRGBK1                                                    
         EDIT  (B1,PRGBK1LN),(1,SFMLN1)                                         
         MVC   SFMBK2,PRGBK2                                                    
         EDIT  (B1,PRGBK2LN),(1,SFMLN2)                                         
         OI    SFMBK1H+6,X'80'     XMIT                                         
         OI    SFMLN1H+6,X'80'     XMIT                                         
         OI    SFMBK2H+6,X'80'     XMIT                                         
         OI    SFMLN2H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SVBKLNS+0(1),PRGBK1LN                                            
         MVC   SVBKLNS+1(1),PRGBK2LN                                            
         MVC   SVBKLNS+2(1),PRGBK3LN                                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,PRGBK1LN                                                      
         SR    RE,RE                                                            
         IC    RE,PRGBK2LN                                                      
         AR    R0,RE                                                            
         IC    RE,PRGBK3LN                                                      
         AR    R0,RE                                                            
         STC   R0,BYTE2            SAVE TOTAL DIGITS                            
         B     DR4                                                              
         EJECT                                                                  
*                                                                               
DR4      DS    0H                  CLEAR DISPLAY LINES                          
         LA    R2,SFMLISTH                                                      
DR4A     OC    8(72,R2),8(R2)                                                   
         BZ    DR4B                                                             
         XC    8(72,R2),8(R2)                                                   
         OI    6(R2),X'80'         XMIT                                         
DR4B     SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    DR4A                                                             
*                                                                               
* DISPLAY PRODUCT GROUP NAMES                                                   
*                                                                               
         LA    R2,SFMLISTH                                                      
* READ HI FOR GRP DEF'N REC                                                     
         MVC   MYKEYSV,KEY         SAVE CURRENT KEY                             
         MVC   AIOSV,AIO           SAVE CURRENT AIO AREA                        
         MVC   KEY(6),0(R7)        IO AREA HAS PGRDEF REC                       
         MVI   BYTE,0              DON'T PASS DELETES                           
         GOTO1 MYHIGH                                                           
*                                                                               
DR6      GOTO1 MYSEQ                                                            
*                                                                               
DR8      CLC   KEY(PRGKGRP-PRGKEY),KEYSAVE  TEST SAME THRU GRP ID               
         BNE   DRX                                                              
*                                                                               
         MVC   AIO,AIO2            READ PROGRAM GROUP REC INTO AIO2             
         MVI   BYTE,0                                                           
         GOTO1 MYGETREC                                                         
         L     R7,AIO                                                           
*                                                                               
         LA    R6,PRGEL                                                         
         USING PRGEL10,R6                                                       
         LA    R4,8(R2)         POINT TO DISPLAY POSITION                       
         MVC   0(1,R4),PRGKID                                                   
         UNPK  DUB,PRGKGRP(3)                                                   
         IC    RE,BYTE2            GET TOTAL DIGITS                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),DUB+3                                                    
         MVC   6(24,R4),PRGNAM1                                                 
         CLI   SVBKLNS+1,0                                                      
         BE    DR9                                                              
         MVC   32(24,R4),PRGNAM2                                                
         EJECT                                                                  
*                                                                               
DR9      DS    0H                                                               
         OI    6(R2),X'80'         XMIT                                         
         SPACE 2                                                                
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNH   DRX                 END OF SCREEN                                
         B     DR6                                                              
*                                                                               
DRX      MVC   KEY,MYKEYSV         RESTORE KEY                                  
         MVC   AIO,AIOSV                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         L     R7,AIO                                                           
         CLI   ACTNUM,ACTADD       TEST ADD                                     
         BNE   VR4                                                              
* COUNT NUMBER OF PRDGRPS ON FILE                                               
         ZAP   HALF,=P'0'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(5),SVKEY        X'0D01'/A-M/CLT                              
         MVI   BYTE,0              DON'T COUNT DELETES                          
         GOTO1 MYHIGH                                                           
         BE    VR1B                                                             
         DC    H'0'                                                             
VR1A     MVC   KEY+6(2),=X'FFFF'                                                
         MVI   BYTE,0                                                           
         GOTO1 MYHIGH                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
VR1B     CLC   KEY(5),SVKEY                                                     
         BNE   VR1X                                                             
         AP    HALF,=P'1'                                                       
         CP    HALF,=P'25'         LIMIT OF 26 SCHEMES                          
         BNH   VR1A                                                             
         B     MYERR1              TOO MANY SCHEMES                             
VR1X     MVC   PRGKEY,SVKEY                                                     
         MVC   PRGLEN,=H'65'                                                    
         MVC   PRGAGYA,USERNAME                                                 
         MVI   PRGEL,X'01'                                                      
         MVI   PRGEL+1,41                                                       
         EJECT                                                                  
*                                                                               
VR4      LA    R6,PRGEL                                                         
         USING PRGEL01,R6                                                       
*                                                                               
         LA    R2,SFMBK1H                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         MVC   PRGBK1,8(R2)                                                     
         LA    R2,SFMLN1H                                                       
         MVC   BYTE,8(R2)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,3                                                           
         BH    INVLFLD                                                          
         CLI   BYTE,1                                                           
         BL    INVLFLD                                                          
         CLC   PRGBK1LN,BYTE                                                    
         BE    VR6                                                              
         CLI   ACTNUM,ACTADD                                                    
         BNE   MYERR3                                                           
         MVC   PRGBK1LN,BYTE                                                    
*                                                                               
VR6      LA    R2,SFMBK2H                                                       
         CLI   5(R2),0             IF NO NAME INPUT                             
         BNE   VR7                                                              
         LA    R2,SFMLN2H                                                       
         CLI   5(R2),0             LENGTH INPUT INVALID                         
         BE    VR10                                                             
         B     INVLFLD                                                          
         EJECT                                                                  
VR7      MVC   PRGBK2,8(R2)                                                     
         LA    R2,SFMLN2H                                                       
         MVC   BYTE,8(R2)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,3                                                           
         BH    INVLFLD                                                          
         CLI   BYTE,1                                                           
         BL    INVLFLD                                                          
         CLC   BYTE,PRGBK2LN                                                    
         BE    VR10                                                             
         CLI   ACTNUM,ACTADD                                                    
         BNE   MYERR3                                                           
         MVC   PRGBK2LN,BYTE                                                    
*                                                                               
         SR    R4,R4               CHECK IF SUM OF LENGTHS EXCEEDS 3            
         IC    R4,PRGBK1LN                                                      
         SR    R5,R5                                                            
         IC    R5,PRGBK2LN                                                      
         AR    R4,R5                                                            
         C     R4,=F'3'                                                         
         BH    MYERR4                                                           
*                                                                               
VR10     DS    0H                                                               
         LA    R2,SFMBK1H                                                       
         ZIC   R1,BYTE                                                          
         CH    R1,=H'3'                                                         
         BH    MYERR2              TOO MANY DIGITS                              
*                                                                               
VRX      B     EXIT                                                             
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* LIST RECORD                                                                   
***********************************************************************         
LR       DS    0H                                                               
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         LA    R2,LISTAR                                                        
         USING PLINED,R2                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
LR02     CLI   PREVFLAG,C'Y'                                                    
         BE    LR03                                                             
         OC    KEY,KEY                                                          
         BNZ   LR05                                                             
         MVC   KEY,SVKEY                                                        
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
LR10     CLC   KEY(3),KEYSAVE                                                   
         BNE   LRX                                                              
         OC    PRGKGRP(7),PRGKGRP  CHK IF DEFINITION RECORD                     
         BNZ   LRSEQ                                                            
         OC    SFMCLT,SFMCLT       FILTER ON CLIENT??                           
         BZ    LR13                                                             
         CLC   PRGKCLT,BCLT                                                     
         BNE   LRSEQ                                                            
*                                                                               
LR13     OC    SFMID,SFMID         FILTER ON ID??                               
         BZ    LR30                                                             
         CLC   PRGKID,SFMID                                                     
         BNE   LRSEQ                                                            
*                                                                               
LR30     GOTO1 GETREC                                                           
*                                                                               
         MVC   MYKEYSV,KEY         SAVE AWAY PROGRAM REC KEY                    
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
*                                                                               
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   WORK2(1),CPROF+6                                                 
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,MYKEYSV                                                      
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRGEL01,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 CLUNPK,DMCB,(WORK2,PRGKCLT),PLCLI                                
         MVC   PLGRPID,PRGKID                                                   
         MVC   PLLEV1,PRGBK1                                                    
         MVC   PLLEV2,PRGBK2                                                    
         CLI   MODE,PRINTREP                                                    
         BE    LR40                                                             
         GOTO1 LISTMON                                                          
         B     LRSEQ                                                            
*                                                                               
LR40     MVC   P+12(PLEND-PLCLI),PLCLI                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRSEQ                                                            
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
INVLCLI  MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
MYERR1   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'*** ERROR-LIMIT IS 5 SCHEMES'                     
         B     MYERR                                                            
MYERR2   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'*** ERROR-TOO MANY DIGITS   '                     
         B     MYERR                                                            
MYERR3   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'*** MAY NOT CHANGE BREAK LEN'                     
         B     MYERR                                                            
MYERR4   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'*** ERROR-SUM OF LENGTHS EXCEEDS 3'               
         B     MYERR                                                            
MYERR5   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'*** CANNOT DELETE-DEFINITION IN USE'              
         B     MYERR                                                            
DELERR   MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
MYERR    GOTO1 ERREX2                                                           
         B     EXIT                                                             
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*********************************************                                   
*                                                                               
*                                                                               
MYHIGH   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(BYTE,=C'DMRDHI'),=CL8'SPTDIR',KEY,KEY,0            
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
MYSEQ    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(BYTE,=C'DMRSEQ'),=CL8'SPTDIR',KEY,KEY,0            
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
MYGETREC NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(BYTE,=C'GETREC'),=C'SPTFILE ',            X        
               KEY+14,AIO,MYDMWRK                                               
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=CL8'SPTDIR',KEY,KEY                      
         B     EXIT                                                             
MYPUTREC NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=CL8'SPTFILE',KEY+14,AIO,MYDMWRK         
         B     EXIT                                                             
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'92'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
*                                                                               
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
***********************************************************************         
* HEADER AND HEAD HOOK ROUTINES                                                 
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK PROGRAM DEFENITION RECORDS'                      
         SSPEC H2,46,C'-----------------------------------'                     
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PLCLI(3),=C'CLT'                                                 
         MVC   PLCLI+132(3),=20C'-'                                             
         MVC   PLGRPID(2),=C'ID'                                                
         MVC   PLGRPID+132(2),=20C'-'                                           
         MVC   PLLEV1(7),=C'LEVEL 1'                                            
         MVC   PLLEV1+132(12),=20C'-'                                           
         MVC   PLLEV2(7),=C'LEVEL 2'                                            
         MVC   PLLEV2+132(12),=20C'-'                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SECURITY ACCESS:                                                              
*   SAME ACCESS AS FILE PROGRAM X'19'                                           
*   400F AND 600F DISPLAY AND LIST ONLY                                         
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
         LA    R5,CTSYSL1Q(R6)                                                  
SEC25    CLI   0(R5),X'19'         FILE PROGRAM                                 
         BE    SEC30                                                            
         LA    R5,L'CTSYSPGM(R5)                                                
         LR    R4,R5                                                            
         SR    R4,R6                                                            
         STC   R4,COUNT                                                         
         CLC   COUNT,CTSYSLEN                                                   
         BNL   SECX                                                             
         B     SEC25                                                            
*                                                                               
SEC30    DS    0H                                                               
         MVC   AUTHCODE,1(R5)                                                   
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
         BNE   INVLACT                                                          
*                                                                               
SECX     MVC   DATADISP,=H'24'     RESET FOR SPOT FILE                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*****************************                                                   
*        DDSPLWORKD                                                             
*        DDSPOOLD                                                               
*        DDFLDIND                                                               
*        INCLUDE NESFMFFD                                                       
*        INCLUDE NESFMD2D                                                       
*        INCLUDE NESFMD4D                                                       
******************************                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMD2D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMD4D                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
         PRINT ON                                                               
*                                                                               
*                                                                               
*                                                                               
*                           *******  T31C40 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
BYTE2    DS    CL1                                                              
PREVFLAG DS    CL1                                                              
SVBKLNS  DS    CL6                                                              
MYKEYSV  DS    CL48                                                             
DELKEY   DS    CL48                                                             
PREVKEY  DS    CL48                                                             
WORK2    DS    CL48                                                             
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
AIOSV    DS    F                                                                
TERMINFO DS    C                   Y/N                                          
COUNT    DS    X                                                                
AUTHCODE DS    CL2                                                              
WORKEND  EQU   *                                                                
*                                                                               
*                                                                               
*                                                                               
PLINED   DSECT                                                                  
         DS    CL2                                                              
PLCLI    DS    CL3                                                              
         DS    CL2                                                              
PLGRPID  DS    CL1                                                              
         DS    CL3                                                              
PLLEV1   DS    CL12                                                             
         DS    CL2                                                              
PLLEV2   DS    CL12                                                             
         DS    CL2                                                              
PLEND    EQU   *                                                                
         EJECT                                                                  
*SPGENPRG                                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE CTGENFILE                                                      
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
 END                                                                            
