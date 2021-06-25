*          DATA SET CTTST00    AT LEVEL 153 AS OF 08/22/00                      
*&&      SET   NOP=N                                                            
*PHASE TA0900A                                                                  
*INCLUDE SCRUMPY                                                                
         TITLE 'CTTST00 - TEST MODULE'                                          
CTTEST   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**TST**,RA,RR=R4                                     
         USING WORKD,RC                                                         
         ST    R4,RELO                                                          
         ST    RD,SAVERD                                                        
         MVC   SVPARMS,0(R1)       SAVE S/R PARM LIST                           
         L     R8,ACOMFACS                                                      
         USING COMFACSD,R8         R8=A(COMFACS)                                
         L     R9,ATWA                                                          
         USING CTTSTFFD,R9         R9=A(TWA)                                    
*                                                                               
         LA    R1,TSTACTH          SET INITIAL CURSOR POS                       
         ST    R1,CURSOR                                                        
         XC    XCTL,XCTL                                                        
*                                                                               
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIOAREA1                                                      
*                                                                               
         L     R1,=A(BUFF2-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ABUFF2                                                        
         MVC   ABUFF1,ATIA                                                      
*                                                                               
         GOTO1 CPROTOFF                                                         
         B     MAIN                                                             
*                                                                               
XMOD     L     R1,CURSOR           SET CURSOR POS                               
         OI    6(R1),X'40'                                                      
         XMOD1                                                                  
*                                                                               
XIT1     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM TEST I/P O/P BITS                     *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     EQU   *                                                                
         MVC   TSTMSG,=CL60'CONTROL TEST PROGRAM'                               
         OI    TSTMSGH+6,X'80'                                                  
         LA    R1,TSTIN1H                                                       
         BAS   RE,DISPFLD                                                       
         MVC   TSTOUT1,WORK1                                                    
         OI    TSTOUT1H+6,X'80'                                                 
         LA    R1,TSTIN2H                                                       
         BAS   RE,DISPFLD                                                       
         MVC   TSTOUT2,WORK1                                                    
         OI    TSTOUT2H+6,X'80'                                                 
         LA    R1,TSTIN3H                                                       
         BAS   RE,DISPFLD                                                       
         MVC   TSTOUT3,WORK1                                                    
         OI    TSTOUT3H+6,X'80'                                                 
         LA    R1,TSTIN4H                                                       
         BAS   RE,DISPFLD                                                       
         MVC   TSTOUT4,WORK1                                                    
         OI    TSTOUT4H+6,X'80'                                                 
         LA    R1,TSTIN5H                                                       
         BAS   RE,DISPFLD                                                       
         MVC   TSTOUT5,WORK1                                                    
         OI    TSTOUT5H+6,X'80'                                                 
*                                                                               
         L     R1,AUTL                                                          
         SR    RE,RE                                                            
         IC    RE,TRATE-UTLD(R1)                                                
         MVC   TSTHX1O+0(4),=C'RATE'                                            
         LA    RF,TSTHX1O+5                                                     
         EDIT  (RE),(5,0(RF))                                                   
         L     R1,AUTL                                                          
**       TM    TFLAG-UTLD(R1),TFLAGHLP                                          
**       BZ    *+6                                                              
**       DC    H'0'                                                             
         LH    R1,TTRCNT-UTLD(R1)                                               
         MVC   TSTHX1O+0(4),=C'TTRC'                                            
         LA    RF,TSTHX1O+5                                                     
         EDIT  (R1),(5,0(RF))                                                   
         MVC   TSTHX1O+20(4),=C'TIOB'                                           
         L     RF,ATIOB                                                         
         GOTO1 CHEXOUT,DMCB,(RF),TSTHX1O+25,TIOBL                               
         OI    TSTHX1OH+6,X'80'                                                 
         B     XMOD                                                             
         EJECT                                                                  
*************************************************************                   
*        TEST SUBROUTINES                                   *                   
*************************************************************                   
         SPACE 1                                                                
DISPFLD  NTR1                                                                   
         LR    RF,R1                                                            
         XC    WORK1,WORK1                                                      
         MVC   WORK1(16),=C'L=??,IP=????????'                                   
         EDIT  (B1,5(RF)),(2,WORK1+2),ZERO=NOBLANK,FILL=0                       
         MVC   BYTE,4(RF)                                                       
         BAS   RE,TITBITS                                                       
         MVC   WORK1+8(8),DUB                                                   
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        COMMON SUBROUTINES BITS IN BYTE TO DUB 1.1.1.1.    *                   
*************************************************************                   
         SPACE 1                                                                
TITBITS  ST    RE,SAVERE           SET BIT PATERN FROM BYTE IN DUB              
         LA    RE,DUB                                                           
         LA    R1,X'80'                                                         
TBITS1   MVI   0(RE),C'.'          DEFAULT TO . FOR CLEAR BIT                   
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0              TEST BIT                                     
         BNO   *+8                                                              
         MVI   0(RE),C'1'          SET TO 1 IF ON                               
         LA    RE,1(RE)                                                         
         SRL   R1,1                NEXT BIT                                     
         LTR   R1,R1                                                            
         BNZ   TBITS1                                                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        VALNUM R4=A(FIELD),R3=LEN  EXIT DUB=PACKED OR FF   *                   
*************************************************************                   
         SPACE 1                                                                
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
*************************************************************                   
*        SWITCH TO ACC AND MAKE UPDATES                     *                   
*************************************************************                   
         SPACE 1                                                                
ACCUPD   NTR1                                                                   
*                                                                               
         MVI   SENUM,X'66'         SWITCH TO ACC0                               
*                                                                               
         GOTO1 CSWITCH,DMCB,(SENUM,X'FFFFFFFF'),0                               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R3,R3               VALIDATE NUMBER IN T3                        
         IC    R3,TSTIN3H+5                                                     
         LA    R4,TSTIN3                                                        
         BAS   RE,VALNUM                                                        
         CLI   DUB,X'FF'                                                        
         BE    ACCUPX                                                           
*                                                                               
         XC    TSTIN3,TSTIN3       CLR AND XMIT                                 
         OI    TSTIN3H+6,X'80'                                                  
*                                                                               
         CVB   R4,DUB              WAIT THIS MANY TIMES                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(6),ACCKEY                                                    
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',DMRDHI),ACCDIR,KEY,KEY                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACCU010  XI    KEY+42,X'02'                                                     
         MVC   DA,KEY+50                                                        
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',GETREC),ACCMST,DA,AIOAREA1,IOWORK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIOAREA1                                                      
         XI    42(R1),X'02'                                                     
*                                                                               
         GOTO1 CDATAMGR,DMCB,PUTREC,ACCMST,DA,AIOAREA1,IOWORK                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BCT   R4,ACCU010                                                       
*                                                                               
         MVI   SENUM,X'0A'         SWITCH TO CON                                
*                                                                               
         GOTO1 CSWITCH,DMCB,(SENUM,X'FFFFFFFF'),0                               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACCUPX   XIT1                                                                   
*                                                                               
ACCKEY   DC    X'0241',C'VAAA'                                                  
         EJECT                                                                  
*************************************************************                   
*        SWITCH TO PER AND MAKE UPDATES                     *                   
*************************************************************                   
         SPACE 1                                                                
PERUPD   NTR1                                                                   
*                                                                               
         MVI   SENUM,X'0E'         SWITCH TO PER1                               
*                                                                               
         GOTO1 CSWITCH,DMCB,(SENUM,X'FFFFFFFF'),IOAREA                          
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ATICTOC,DMCB,C'WAIT',F'384000'                                   
*        DON'T UPDATE REALLY                                                    
*                                                                               
         MVI   SENUM,X'0A'         BACK TO CONTROL                              
         GOTO1 CSWITCH,DMCB,(SENUM,X'FFFFFFFF'),0                               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PERUPX   XIT1                                                                   
*                                                                               
PERKEY   DC    X'0241',C'VAAA'                                                  
         EJECT                                                                  
*************************************************************                   
*        ERROR & INFO EXITS                                 *                   
*************************************************************                   
         SPACE 1                                                                
ERR1     LA    R1,1                                                             
         B     ERRX                                                             
*                                                                               
ERRX     MVC   TSTMSG,SPACES                                                    
         MVC   TSTMSG(9),=C'ERROR XXX'                                          
         EDIT  (R1),(3,TSTMSG+6)                                                
         OI    TSTMSGH+6,X'80'                                                  
         L     RD,SAVERD                                                        
         B     XMOD                                                             
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LITERALS                               *                   
*************************************************************                   
         SPACE 1                                                                
SPACES   DC    80C' '                                                           
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
GENDIR   DC    CL8'GENDIR'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
GENFIL   DC    CL8'GENFIL'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
READ     DC    CL8'READ'                                                        
WRITE    DC    CL8'WRITE'                                                       
SEQ      DC    CL8'SEQ'                                                         
RANDOM   DC    CL8'RANDOM'                                                      
DMRSRV   DC    CL8'DMRSRV'                                                      
TEMPEST  DC    CL8'TEMPEST'                                                     
DMREAD   DC    CL8'DMREAD'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
INDEX    DC    CL8'INDEX'                                                       
WRKFIL   DC    CL8'WRKFIL'                                                      
GFILE    DC    CL8'GFILE'                                                       
*                                                                               
WORKKEY  DC    X'0026',C'TEMP',X'24',C'T',X'0004'                               
*                                                                               
PROGLIN  DC    C'----10----20----30----40----50----60----70'                    
         DC    C'----80----90----100%                  '                        
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DSECT TO COVER WORKING STORAGE                     *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
RELO     DS    A                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG1    DS    X                                                                
SENUM    DS    X                                                                
DMCB     DS    6F                                                               
SAVERE   DS    A                                                                
CURSOR   DS    A                                                                
XCTL     DS    CL24                                                             
FIRSTK   DS    XL3                                                              
SECONK   DS    XL3                                                              
WORK     DS    CL255                                                            
HEXWORK  DS    XL32                                                             
WORK1    DS    XL64                                                             
IOWORK   DS    12D                                                              
DA       DS    F                                                                
*                                                                               
AIOAREA1 DS    A                                                                
*                                                                               
         DS    0F                                                               
SVPARMS  DS    0CL32                                                            
ASYSFACS DS    A                                                                
ATIA     DS    A                                                                
AUTL     DS    A                                                                
ACOMFACS DS    A                                                                
ASELIST  DS    A                                                                
ATWA     DS    A                                                                
AMAP     DS    A                                                                
ATIOB    DS    A                                                                
*                                                                               
ATICTOC  DS    A                                                                
AGETCUR  DS    A                                                                
ABUFF1   DS    A                                                                
ABUFF2   DS    A                                                                
*                                                                               
WRKF     DS    CL8                                                              
KEY      DS    CL40                                                             
*                                                                               
DSPCSTOK DS    CL8                                                              
DSPCALET DS    F                                                                
*                                                                               
BUFF2    DS    CL14336                                                          
         ORG   BUFF2                                                            
IOAREA   DS    4096C                                                            
IOAREA1  DS    4096C                                                            
         ORG                                                                    
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        DSECT TO COVER TWA                                 *                   
*************************************************************                   
         SPACE 1                                                                
CTTSTFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE CTTSTFFD                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
* FADSECTS                                                                      
* FAFACTS                                                                       
* DDPARSNIPD                                                                    
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATCB                                                          
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE DMWRKFD                                                        
         PRINT ON                                                               
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLPFMD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'153CTTST00   08/22/00'                                      
         END                                                                    
         EJECT                                                                  
*************************************************************                   
*        SAVE ALL OLD TEST PROGRAMS DOWN HERE AFTER END     *                   
*************************************************************                   
*************************************************************                   
*        MAIN PROGRAM MAKE LOTS OF CONTROL UPDATES          *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     MVC   TSTMSG,=CL60'CONTROL TEST PROGRAM'                               
         OI    TSTMSGH+6,X'80'                                                  
*                                                                               
         MVC   TSTTAB3,PROGLIN                                                  
         OI    TSTTAB3H+6,X'80'                                                 
         OI    TSTTAB2H+1,X'08'                                                 
         MVI   BYTE,0                                                           
*                                                                               
         L     R5,ASYSFACS         GET A(TCB) INTO R5                           
         MVC   ATICTOC,VTICTOC-SYSFACD(R5)                                      
*                                                                               
         L     R5,VSSB-SYSFACD(R5)                                              
         L     R5,SSBTKADR-SSBD(R5)                                             
         USING TCBD,R5                                                          
*                                                                               
*        BAS   RE,PERUPD           UPDATE PERSON                                
*        CLI   TSTIN2,C'D'                                                      
*        BNE   MAIN001                                                          
*        DC    H'0'                                                             
*                                                                               
MAIN001  MVC   FIRSTK,=X'0000D4'                                                
         MVC   SECONK,=X'0000C5'                                                
         CLI   TSTIN3,C'R'                                                      
         BNE   *+16                                                             
         MVC   FIRSTK,=X'0000C5'                                                
         MVC   SECONK,=X'0000D4'                                                
*                                                                               
         SR    R3,R3               VALIDATE NUMBER IN T1                        
         IC    R3,TSTIN1H+5                                                     
         LA    R4,TSTIN1                                                        
         BAS   RE,VALNUM                                                        
         CLI   DUB,X'FF'                                                        
         BE    MAINX                                                            
*                                                                               
         CVB   R4,DUB              LOOP THIS MANY TIMES                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),FIRSTK       E FOR EXCHANGE                               
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',DMRDHI),GENDIR,KEY,KEY                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN010  XI    KEY+33,X'02'                                                     
         MVC   DA,KEY+36                                                        
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',GETREC),GENFIL,DA,AIOAREA1,IOWORK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIOAREA1                                                      
         XI    35(R1),X'02'                                                     
*                                                                               
         GOTO1 CDATAMGR,DMCB,PUTREC,GENFIL,DA,AIOAREA1,IOWORK                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',DMWRT),GENDIR,KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN090  XC    FULL,FULL                                                        
         MVC   FULL(3),TCBIOCNT                                                 
*                                                                               
MAIN091  GOTO1 CDATAMGR,DMCB,(X'80',DMRSEQ),GENDIR,KEY,KEY                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TCBIOCNT(3),FULL    READ UNTIL NEXT BLOCK                        
         BE    MAIN091                                                          
*                                                                               
         BAS   RE,WAIT                                                          
         BAS   RE,REPORT                                                        
*                                                                               
         BCT   R4,MAIN010                                                       
         CLI   TSTIN2,C'D'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,REPORT                                                        
*                                                                               
         BAS   RE,PERUPD           UPDATE PERSON                                
*                                                                               
         SR    R3,R3               VALIDATE NUMBER IN T1                        
         IC    R3,TSTIN1H+5                                                     
         LA    R4,TSTIN1                                                        
         BAS   RE,VALNUM                                                        
         CLI   DUB,X'FF'                                                        
         BE    MAINX                                                            
*                                                                               
         MVI   BYTE,0                                                           
         CVB   R4,DUB              LOOP THIS MANY TIMES                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),SECONK       M FOR MESSAGE                                
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',DMRDHI),GENDIR,KEY,KEY                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN110  XI    KEY+33,X'02'                                                     
         MVC   DA,KEY+36                                                        
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',GETREC),GENFIL,DA,AIOAREA1,IOWORK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIOAREA1                                                      
         XI    35(R1),X'02'                                                     
*                                                                               
         GOTO1 CDATAMGR,DMCB,PUTREC,GENFIL,DA,AIOAREA1,IOWORK                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',DMWRT),GENDIR,KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN190  XC    FULL,FULL                                                        
         MVC   FULL(3),TCBIOCNT                                                 
*                                                                               
MAIN191  GOTO1 CDATAMGR,DMCB,(X'80',DMRSEQ),GENDIR,KEY,KEY                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TCBIOCNT(3),FULL    READ UNTIL NEXT BLOCK                        
         BE    MAIN191                                                          
*                                                                               
         BAS   RE,WAIT                                                          
         BAS   RE,REPORT                                                        
*                                                                               
         BCT   R4,MAIN110                                                       
         BAS   RE,REPORT                                                        
*                                                                               
         CLI   TSTIN2,C'M'         MAXI DUMP REQUIRED                           
         BNE   DUMP                                                             
*                                                                               
         L     R1,ASYSFACS                                                      
         L     R1,VSSB-SYSFACD(R1)                                              
         XC    SSBTKADR-SSBD(4,R1),SSBTKADR-SSBD(R1)                            
         DC    H'0'                DUMP SYSTEM                                  
*                                                                               
DUMP     B     MAINX                                                            
*                                                                               
MAINX    L     R1,ASYSFACS                                                      
         L     R1,VSSB-SYSFACD(R1)                                              
         L     R1,SSBTKADR-SSBD(R1)                                             
         SR    RF,RF                                                            
         ICM   RF,7,TCBIOCNT-TCBD(R1)                                           
         EDIT  (RF),(5,TSTOUT1)                                                 
         OI    TSTOUT1H+6,X'80'                                                 
         B     XMOD                                                             
*                                                                               
WAIT     NTR1                                                                   
*                                                                               
         GOTO1 ATICTOC,DMCB,C'WAIT',F'38400'                                    
*                                                                               
WAITX    XIT1                                                                   
         EJECT                                                                  
************************************************************                    
*        REPORT TO USER ON PROGRESS                        *                    
************************************************************                    
         SPACE 1                                                                
REPORT   NTR1                                                                   
         B     REPORTX                                                          
         CVB   R1,DUB              R1 IS MAX NUMBER                             
         LR    RF,R1                                                            
         SR    RF,R4               R4 IS PROGRESS                               
         MH    RF,=H'10'                                                        
         SR    RE,RE                                                            
         DR    RE,R1               CALCULATE 10%TAGE PROGRESS                   
         CLM   RF,1,BYTE                                                        
         BE    REPORTX                                                          
         STC   RF,BYTE                                                          
*                                                                               
         XC    TSTTAB2,TSTTAB2                                                  
         MH    RF,=H'6'                                                         
REP010   LA    R1,TSTTAB2-1(RF)                                                 
         MVI   0(R1),C'*'          FILL UP BAR LINE                             
         BCT   RF,REP010                                                        
*                                                                               
         OI    TSTTAB2H+6,X'80'                                                 
         L     R1,CURSOR           SET CURSOR POS                               
         OI    6(R1),X'40'                                                      
         GOTO1 CGETFACT,DMCB,(X'80',WORK),F#WRITE                               
REPORTX  XIT1                                                                   
*                                                                               
         EJECT                                                                  
*********************************************************                       
*  TEST A NON INTERUPTABLE LOOP                         *                       
*********************************************************                       
         GOTO1 ATICTOC,DMCB,C'SSET'                                             
         L     R1,=X'7FFFFFFF'                                                  
LOOPY    SH    R1,=H'1'                                                         
         C     R1,=F'1'                                                         
         BH    LOOPY                                                            
         MVC   DUMP(2),=H'00'                                                   
         GOTO1 ATICTOC,DMCB,C'RSET'                                             
*************************************************************                   
*        MAIN PROGRAM TEST ALESERV                          *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     MVC   TSTMSG,=CL60'CONTROL TEST PROGRAM'                               
         OI    TSTMSGH+6,X'80'                                                  
*                                                                               
         CLI   TSTIN1,C'D'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,ASYSFACS                                                      
         L     R1,VSSB-SYSFACD(R1)                                              
         MVC   DSPCALET,SSBALET-SSBD(R1)                                        
*                                                                               
         XC    DSPCSTOK,DSPCSTOK                                                
         ALESERV EXTRACT,STOKEN=DSPCSTOK,ALET=DSPCALET                          
         DC    H'0'                                                             
*                                                                               
MAINX    B     XMOD                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM TEST GETFACT                          *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     MVC   TSTMSG,=CL60'CONTROL TEST PROGRAM'                               
         OI    TSTMSGH+6,X'80'                                                  
*                                                                               
         MVI   WORK,C'W'                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
*                                                                               
         GOTO1 CGETFACT,DMCB,(X'80',WORK),F#SEIND                               
*                                                                               
         LA    R1,WORK                                                          
         DC    H'0'                                                             
*                                                                               
MAINX    B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
*MAIN PROGRAM TO TRANSFER CONTROL TO DEBUG                            *         
***********************************************************************         
         SPACE 1                                                                
MAIN     CLC   TSTIN1(3),=C'XCTL'  TEST XFR CONTROL REQUIRED                    
         BNE   MAINPFM                                                          
         LA    R2,XCTL                                                          
         USING GLVXCTLD,R2                                                      
*                                                                               
         XC    XCTL,XCTL                                                        
         MVI   GLVXCODE,4                                                       
         MVI   GLVXLEN,24                                                       
         MVC   GLVXFRSY(6),=C'CONTES'                                           
         MVC   GLVXTOSY(6),=C'CONDEB'                                           
         CLI   TSTIN1+3,C'N'                                                    
         BNE   *+8                                                              
         OI    GLVXFLG1,GLV1SEPS                                                
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',XCTL+2,22,04                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAINX    B     XMOD                                                             
         EJECT                                                                  
MAINPFM  CLC   TSTIN1(3),=C'PFM'  TEST PFM GLOBAL REQUIRED                      
         BNE   XMOD                                                             
         XC    WORK1,WORK1                                                      
         LA    R2,WORK1                                                         
         USING GLPFMFIL,R2                                                      
         MVC   GLPFMFIL,=CL8'GENFIL'                                            
         MVC   GLPFMDA,=X'00010101'                                             
         MVC   GLPFMKEY(4),=C'*DFI'                                             
         GOTO1 CGLOBBER,DMCB,=C'PUTD',WORK1,52,08                               
*                                                                               
         CLC   TSTIN1(5),=C'PFMGO'                                              
         BNE   XMOD                                                             
         LA    R2,XCTL                                                          
         USING GLVXCTLD,R2                                                      
         XC    XCTL,XCTL                                                        
         MVI   GLVXCODE,4                                                       
         MVI   GLVXLEN,24                                                       
         MVC   GLVXFRSY(6),=C'CONTES'                                           
         MVC   GLVXTOSY(6),=C'CONPFM'                                           
         OI    GLVXFLG1,GLV1SEPS                                                
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',XCTL+2,22,04                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XMOD                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM CALL GETCUR                           *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     MVC   TSTMSG,=CL60'CONTROL TEST PROGRAM'                               
         OI    TSTMSGH+6,X'80'                                                  
*                                                                               
         CLI   TSTIN1,C'D'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TSTIN1,C'E'                                                      
         BNE   MAINX                                                            
*                                                                               
         MVC   KEY,DUMMY                                                        
*                                                                               
         GOTO1 CGETCUR,DMCB,(X'88',KEY),ACOMFACS                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DC    H'0'                                                             
*                                                                               
MAINX    B     XMOD                                                             
*                                                                               
DUMMY    DC    X'0000',C'ED1',X'04',C'FRFUSDB'                                  
         DC    XL6'FF',XL8'00',X'BD8A0000'                                      
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM TEST FLASHPOINT                       *                   
*************************************************************                   
         SPACE 1                                                                
*&&NOP                                                                          
MAIN     MVC   TSTMSG,=CL60'CONTROL TEST PROGRAM'                               
         OI    TSTMSGH+6,X'80'                                                  
*                                                                               
         CLI   TSTIN1H+5,0                                                      
         BE    MAINX                                                            
*                                                                               
         SR    R3,R3               VALIDATE NUMBER IN T1                        
         IC    R3,TSTIN1H+5                                                     
         LA    R4,TSTIN1                                                        
         BAS   RE,VALNUM                                                        
         CLI   DUB,X'FF'                                                        
         BE    ERR1                                                             
*                                                                               
         CVB   R1,DUB              SET IT IN TIOBAID                            
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         STC   R1,TIOBAID                                                       
         OI    TIOBINDS,TIOBSUBS                                                
         DROP  RF                                                               
MAINX    B     XMOD                                                             
*&&                                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM TEST GETCURA                          *                   
*************************************************************                   
         SPACE 1                                                                
*&&NOP                                                                          
MAIN     MVC   TSTMSG,=CL60'CONTROL TEST PROGRAM'                               
         OI    TSTMSGH+6,X'80'                                                  
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,TSTHX1IH+5                                                  
         BZ    MAINX                                                            
         GOTO1 CHEXIN,DMCB,TSTHX1I,HEXWORK,(R0)                                 
*                                                                               
         GOTO1 AGETCUR,DMCB,(X'08',HEXWORK),SVCFACS                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,0(R1)                                                       
         MVC   HEXWORK,0(RF)                                                    
         GOTO1 CHEXOUT,DMCB,HEXWORK,TSTHX1O,32,=C'TOG'                          
*                                                                               
         OI    TSTHX1OH+6,X'80'                                                 
         LA    R1,TSTIN2H                                                       
MAINX    B     XMOD                                                             
*&&                                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM TO CALL SCRUMPY                       *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+UKFILENO-UKRECD(2),=X'0002'                                  
         OI    KEY+UKFLAG-UKRECD,X'80'                                          
         MVC   KEY(2),WORKKEY                                                   
         GOTO1 =V(SCRUMPY),DMCB,ACOMFACS,ABUFF1,ABUFF2,KEY,RR=RELO              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAINX    B     XMOD                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM WAIT TEST                             *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     MVC   TSTMSG,=CL60'CONTROL TEST PROGRAM'                               
         OI    TSTMSGH+6,X'80'                                                  
*                                                                               
         CLI   TSTIN1H+5,0                                                      
         BE    MAINX                                                            
*                                                                               
         L     R1,AIOAREA1                                                      
         MVC   0(5,R1),=X'115C6F1DE8' SBA/23/80/HI/UNP                          
         MVC   0(18,R1),=C' HALFWAY THERE    '                                  
         LA    R1,18(R1)                                                        
         MVI   0(R1),ETX                                                        
         LA    R1,1(R1)                                                         
*                                                                               
         L     RE,AIOAREA1                                                      
         SH    RE,=H'8'            BACK UP TO BUFFER HEADER                     
         ST    R3,0(RE)            SET A(UTL ENTRY)                             
         S     R1,AIOAREA1                                                      
         ST    R1,4(RE)            SET 0000.LEN                                 
         MVI   5(RE),X'01'         SET ERASE WRITE                              
*                                                                               
         L     R2,=F'1000'                                                      
LOOP1    L     R1,=F'1000000'                                                   
         BCT   R1,*                                                             
*                                                                               
         GOTO1 CCALLOV,DMCB,IOAREA,X'D9010100'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         C     R2,=F'500'                                                       
         BNE   LOOPL                                                            
*                                                                               
         L     RF,ASYSFACS                                                      
         L     RF,VLCM-SYSFACD(RF)                                              
*                                                                               
         GOTO1 (RF),DMCB,1,AUTL,AIOAREA1                                        
*                                                                               
LOOPL    BCT   R2,LOOP1                                                         
*                                                                               
                                                                                
*                                                                               
MAINX    B     XMOD                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM CALL DADDS DIRECT                     *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     MVC   TSTMSG,=CL60'CONTROL TEST PROGRAM'                               
         OI    TSTMSGH+6,X'80'                                                  
*                                                                               
         CLI   TSTIN1,C'D'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN01   L     RF,ASYSFACS         GET A(DADDS) INTO RF                         
         L     RF,VDADDS-SYSFACD(RF)                                            
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+3,X'01'                                                     
         MVC   DMCB+4(4),AIOAREA1                                               
         MVC   DMCB+12(4),=X'00073198'                                          
         LA    R1,DISKAD                                                        
         ST    R1,DMCB+16                                                       
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         DC    H'0'                                                             
*                                                                               
*ISKAD   DC    X'002D0200'                                                      
DISKAD   DC    X'00010100'                                                      
*                                                                               
MAINX    B     XMOD                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM TEST WRKF                             *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     MVC   TSTMSG,=CL60'CONTROL TEST PROGRAM'                               
         OI    TSTMSGH+6,X'80'                                                  
*                                                                               
         CLI   TSTIN1H+5,0                                                      
         BE    MAINX                                                            
         CLI   TSTIN2H+5,0                                                      
         BE    MAINX                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING UKRECD,R2                                                        
         MVC   UKUSRID,=X'0026'    FORCE TO DDS1                                
*                                                                               
         GOTO1 CDATAMGR,DMCB,GFILE,WRKFIL,KEY,IOAREA,ATIA                       
         MVC   WRKF,UKUSRINF       EXTRACT FILENAME                             
*                                                                               
         XC    KEY,KEY                                                          
NXTINDX  GOTO1 CDATAMGR,DMCB,INDEX,WRKF,KEY,IOAREA,ATIA                         
         CLI   8(R1),0                                                          
         BNE   NXTEND                                                           
*                                                                               
         USING W_RECD,R2                                                        
         CLC   W_SYSPRG(4),TSTIN2                                               
         BNE   NXTINDX                                                          
         B     READF                                                            
*                                                                               
NXTEND   TM    8(R1),X'80'                                                      
         BE    MAINX                                                            
         DC    H'0'                ERROR                                        
*                                                                               
READF    GOTO1 CDATAMGR,DMCB,READ,WRKF,KEY,IOAREA,ATIA                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TSTIN1,C'H'                                                      
         BE    RRAND                                                            
         CLI   TSTIN1,C'N'                                                      
         BE    RRANDN                                                           
         CLI   TSTIN1,C'W'                                                      
         BE    RWRITE                                                           
         B     MAINX                                                            
*                                                                               
RRAND    XC    IOAREA(8),IOAREA    READ HEADER                                  
         MVC   IOAREA+4(4),=C'REC '                                             
         GOTO1 CDATAMGR,DMCB,RANDOM,WRKF,KEY,IOAREA,ATIA                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,IOAREA                                                        
         DC    H'0'                                                             
*                                                                               
RRANDN   XC    IOAREA(8),IOAREA    READ HEADER                                  
         MVC   IOAREA+0(4),=F'20'                                               
         MVC   IOAREA+4(4),=C'REC '                                             
         GOTO1 CDATAMGR,DMCB,RANDOM,WRKF,KEY,IOAREA,ATIA                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,IOAREA                                                        
         DC    H'0'                                                             
*                                                                               
RWRITE   GOTO1 CDATAMGR,DMCB,(X'80',READ),WRKF,KEY,IOAREA,ATIA                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,IOAREA                                                        
         MVC   14(8,RF),=C'BOLLOCKS'                                            
         GOTO1 CDATAMGR,DMCB,WRITE,WRKF,KEY,IOAREA,ATIA                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     MAINX                                                            
*                                                                               
         SR    R3,R3               VALIDATE NUMBER IN T1                        
         IC    R3,TSTIN1H+5                                                     
         LA    R4,TSTIN1                                                        
         BAS   RE,VALNUM                                                        
         CLI   DUB,X'FF'                                                        
         BE    ERR1                                                             
*                                                                               
         CVB   R1,DUB              SET IT IN TIOBAID                            
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         STC   R1,TIOBAID                                                       
         OI    TIOBINDS,TIOBSUBS                                                
         DROP  RF                                                               
MAINX    B     XMOD                                                             
*************************************************************                   
*        DSECT TO COVER WORKING STORAGE                     *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
RELO     DS    A                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG1    DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
SAVERE   DS    A                                                                
CURSOR   DS    A                                                                
AIO1     DS    A                                                                
*                                                                               
FACID    DS    XL4                                                              
*                                                                               
XCTL     DS    CL24                                                             
WORK     DS    CL255                                                            
HEXWORK  DS    XL32                                                             
WORK1    DS    XL64                                                             
*                                                                               
         DS    0F                                                               
SVPARMS  DS    0CL32                                                            
ASYSFACS DS    A                                                                
ATIA     DS    A                                                                
AUTL     DS    A                                                                
ACOMFACS DS    A                                                                
ASELIST  DS    A                                                                
ATWA     DS    A                                                                
AMAP     DS    A                                                                
ATIOB    DS    A                                                                
*                                                                               
PARBLK   DS    320C                                                             
IOAREA1  DS    4096C                                                            
*                                                                               
WORKX    EQU   *                                                                
***********************************************************************         
*TEST IF THIS IS A XCTL RETURN CALL                                   *         
***********************************************************************         
         SPACE 1                                                                
WHY      XC    XCTL,XCTL           TEST IF WE HAVE BEEN CALLED                  
         XC    WORK(18),WORK                                                    
         GOTO1 CGLOBBER,DMCB,=C'GETD',XCTL+2,22,04                              
         TM    DMCB+8,X'10'                                                     
         BO    WHYX                NO XFR CONTROL GLOBAL PRESENT                
         GOTO1 CGLOBBER,DMCB,=C'DELE'                                           
         LA    R2,XCTL                                                          
         USING GLVXCTLD,R2                                                      
         MVI   GLVXCODE,4                                                       
         MVI   GLVXLEN,24                                                       
         TM    GLVXFLG1,GLV1RETN   TEST IF RETURN CALL TO ME                    
         BO    WHY1                                                             
         TM    GLVXFLG1,GLV1IGN    TEST IF MY ORIGINAL CALL                     
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(18),=CL18'WE MADE IT BACK'                                  
         B     WHY2                                                             
*                                                                               
WHY1     GOTO1 CGLOBBER,DMCB,=C'GETD',WORK,18,50                                
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
WHY2     MVC   TSTIN1(8),=C'RECEIVED'                                           
         OI    TSTIN1H+6,X'80'                                                  
         MVC   TSTOUT1(18),WORK    DISPLAY RETURN TEXT                          
         OI    TSTOUT1H+6,X'80'                                                 
         LA    RE,TSTMSGH                                                       
         SR    RF,RF                                                            
WHY3     CLI   0(RE),0             RETRANSMIT ALL FIELDS                        
         BE    WHY4                                                             
         OI    6(RE),X'80'                                                      
         IC    RF,0(RE)                                                         
         AR    RE,RF                                                            
         B     WHY3                                                             
WHY4     MVI   1(RE),1             SET TO CLEAR SCREEN BEFORE                   
         B     XMOD                                                             
WHYX     XC    XCTL,XCTL                                                        
         DROP  R2                                                               
*                                                                               
**PAN#1  CSECT                                                                  
         END                                                                    
