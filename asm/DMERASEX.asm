*          DATA SET DMERASEX   AT LEVEL 003 AS OF 08/22/00                      
*PHASE DMERASEA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE DMDMGRL                                                                
         TITLE 'DMERASE - CREATE AND ERASE A DIRECT ACCESS FILE'                
* UPSI   1.... CARD INPUT                                                       
* UPSI   .1... DO NOT ASK FOR VERIFICATION                                      
*                                                                               
         PRINT NOGEN                                                            
DMERASE  CSECT                                                                  
         NBASE 0,DMERASE,WORK=A(DMERWORK)                                       
*&&DO                                                                           
DME0     L     R1,20               SAVE DOS COMRG ADDRESS                       
         SVC   33                                                               
         ST    R1,ACMRG                                                         
         MVC   UPSI,23(R1)                                                      
DME0X    EQU   *                                                                
*&&                                                                             
*&&OS                                                                           
DME0     ST    R1,ACMRG            SAVE MVS PARAM ADDRESS                       
         L     R1,0(R1)                                                         
         LH    R2,0(R1)                                                         
         LTR   R2,R2               R2=L'PARM DATA                               
         BZ    DME0X                                                            
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         LA    RF,UPSITAB                                                       
DME0A    CLI   0(R1),C'0'                                                       
         BE    DME0B                                                            
         CLI   0(R1),C'1'                                                       
         BNE   DME0X                                                            
         OC    UPSI,0(RF)                                                       
DME0B    LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,DME0A                                                         
         B     DME0X                                                            
UPSITAB  DC    X'8040201008040201'                                              
DME0X    EQU   *                                                                
*&&                                                                             
         EJECT                                                                  
DME1     TM    UPSI,X'80'          CARD INPUT                                   
         BZ    DME2                                                             
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'                                                   
         BE    DMEOJ                                                            
         CLC   CARD(4),=C'UPSI'                                                 
         BNE   DME3                                                             
         MVI   UPSI,0                                                           
         CLI   CARD+5,C'1'                                                      
         BNE   *+8                                                              
         OI    UPSI,X'80'                                                       
         CLI   CARD+6,C'1'                                                      
         BNE   *+8                                                              
         OI    UPSI,X'40'                                                       
         B     DME1                                                             
         SPACE 2                                                                
DME2     LA    R0,GETFILEL         CONSOLE INPUT                                
         GOTO1 =V(LOGIO),DMCB,1,((R0),GETFILE)                                  
DME2A    MVC   CARD,SPACES                                                      
         GOTO1 =V(LOGIO),DMCB,0,(16,CARD)                                       
         CLC   CARD(4),=C'EOJ '                                                 
         BE    DMEOJ                                                            
         CLC   CARD(4),=C'END '                                                 
         BE    DMEOJ                                                            
         SPACE 2                                                                
DME3     LA    RE,CARD             VALIDATE FILENAME & ACTION                   
         LA    R0,8                                                             
DME3A    CLI   0(RE),C' '                                                       
         BE    DME3B                                                            
         CLI   0(RE),C'='                                                       
         BE    DME3B                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,DME3A                                                         
         B     DME3E                                                            
*                                                                               
DME3B    LA    RF,8                FILE NAME 3 THRU 7 CHRS                      
         SR    RF,R0                                                            
         CH    RF,=H'3'                                                         
         BL    DME3E                                                            
*                                                                               
DME3C    MVI   TYPE,0              FILE=OLD (DEFAULT)                           
         CLC   0(8,RE),SPACES                                                   
         BE    DME3X                                                            
         CLC   0(8,RE),=CL8'=OLD'                                               
         BE    DME3X                                                            
         MVI   TYPE,1              FILE=VTOC (NEW FILE FOR VTOC ONLY)           
         CLC   0(8,RE),=CL8'=VTOC'                                              
         BE    DME3X                                                            
         MVI   TYPE,2              FILE=NEW (NEW FILE TO BE ERASED)             
         CLC   0(8,RE),=CL8'=NEW'                                               
         BE    DME3X                                                            
*                                                                               
DME3E    MVC   ERRFILE+33(16),CARD                                              
         LA    R0,ERRFILEL                                                      
         GOTO1 =V(LOGIO),DMCB,1,((R0),ERRFILE)                                  
         B     DME2A                                                            
*                                                                               
DME3X    MVC   0(8,RE),SPACES                                                   
         SPACE 2                                                                
DME4     TM    UPSI,X'40'          ASK FOR ERASE VERIFICATION                   
         BO    DME5                                                             
         MVC   CHKFILE+8(7),CARD                                                
         LA    R0,CHKFILEL                                                      
         GOTO1 =V(LOGIO),DMCB,1,((R0),CHKFILE)                                  
DME4A    MVC   ANSWER,SPACES                                                    
         GOTO1 =V(LOGIO),DMCB,0,(8,ANSWER)                                      
         CLC   ANSWER(5),=C'ERASE'                                              
         BE    DME5                                                             
         CLC   ANSWER(6),=C'IGNORE'                                             
         BE    DME1                                                             
         CLC   ANSWER(4),=C'EOJ '                                               
         BE    DMEOJ                                                            
         CLC   ANSWER(4),=C'END '                                               
         BE    DMEOJ                                                            
         LA    R0,ERRANSWL                                                      
         GOTO1 =V(LOGIO),DMCB,1,((R0),ERRANSW)                                  
         B     DME4A                                                            
         SPACE 2                                                                
DME5     LA    R3,FILE             POINT TO FILE                                
         MVI   21(R3),0            SET INPUT FILE                               
         CLI   TYPE,0                                                           
         BE    *+8                                                              
         OI    21(R3),X'80'        SET OUTPUT FILE                              
         MVC   22(7,R3),CARD                                                    
         XC    P1(24),P1                                                        
         L     RE,=A(DMERBUFF)                                                  
         ST    RE,P2                                                            
         MVI   P2,X'FF'                                                         
         ST    R3,P4                                                            
         LA    RE,P6                                                            
         ST    RE,P5                                                            
         SPACE 2                                                                
DME6     GOTO1 =V(DADDS),P1,A(DAOPEN)                                           
         CLI   TYPE,1                                                           
         BE    DME6X                                                            
*                                                                               
         GOTO1 =V(DADDS),P1,A(WTERASE)                                          
         NI    P3+1,X'FB'                                                       
*                                                                               
DME6X    GOTO1 =V(DADDS),P1,A(DACLOSE)                                          
         SPACE 2                                                                
DME7     OC    P3(2),P3                                                         
         BNZ   DME8                                                             
         MVC   ERSFILE+8(7),CARD                                                
         LA    R0,ERSFILEL                                                      
         GOTO1 =V(LOGIO),DMCB,1,((R0),ERSFILE)                                  
         B     DME1                                                             
         SPACE 2                                                                
DME8     MVC   ERRDISK+8(7),CARD                                                
         LA    R0,ERRDISKL                                                      
         GOTO1 =V(LOGIO),DMCB,1,((R0),ERRDISK)                                  
         B     DME1                                                             
         SPACE 2                                                                
DMEOJ    XBASE                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    C'**FILE**'                                                      
FILE     DMDA  DSKXTNT=16                                                       
         SPACE 2                                                                
BLDXTNT  DS    0C                                                               
SPACES   DC    CL80' '                                                          
CARD     DC    CL80' '                                                          
ANSWER   DC    CL8' '                                                           
UPSI     DC    X'00'                                                            
TYPE     DC    X'00'                                                            
         SPACE 2                                                                
DUB      DS    D                                                                
ACMRG    DS    A                                                                
DMCB     DS    6F                                                               
         DC    C'**PARM**'                                                      
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
         SPACE 2                                                                
GETFILE  DC    C'*ERASE* ENTER FILE NAME'                                       
GETFILEL EQU   *-GETFILE                                                        
CHKFILE  DC    C'*ERASE* XXXXXXX IS ABOUT TO BE ERASED - VERIFY'                
CHKFILEL EQU   *-CHKFILE                                                        
ERSFILE  DC    C'*ERASE* XXXXXXX ERASED'                                        
ERSFILEL EQU   *-ERSFILE                                                        
ERRFILE  DC    C'*ERASE* INVALID FILE - REINPUT - XXXXXXXXXXXXXXXX'             
ERRFILEL EQU   *-ERRFILE                                                        
ERRANSW  DC    C'*ERASE* INVALID REPLY - REINPUT REPLY'                         
ERRANSWL EQU   *-ERRANSW                                                        
ERRDISK  DC    C'*ERASE* XXXXXXX NOT ERASED DUE TO DISK ERROR'                  
ERRDISKL EQU   *-ERRDISK                                                        
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'**BUFF**'                                                      
DMERBUFF DS    200D                                                             
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'**WORK**'                                                      
DMERWORK DS    200D                                                             
         SPACE 2                                                                
DMERLAST DS    D                                                                
         SPACE 2                                                                
       ++INCLUDE DMGREQUS                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DMERASEX  08/22/00'                                      
         END                                                                    
