*          DATA SET PRTERM     AT LEVEL 007 AS OF 05/01/02                      
*PHASE PRTERM,*                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE SPRINT                                                                 
*INCLUDE REGSAVE                                                                
PRTERM   CSECT                                                                  
         TITLE 'PRTERM - AYER TERMINATION LISTING'                              
         PRINT NOGEN                                                            
         NBASE 0,*PRTERM*,=V(REGSAVE)                                           
         L     R3,=V(CPRINT)                                                    
         USING DPRINT,R3                                                        
*&&DO                                                                           
         OPEN  PRIN                                                             
*&&                                                                             
*&&OS                                                                           
         OPEN  (PRIN,(INPUT))                                                   
*&&                                                                             
         GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         MVC   C+2(2),C                                                         
         MVC   C(2),=C'84'                                                      
         MVC   C+4(2),=C'01'                                                    
         MVC   MID1(72),HDNGS                                                   
         GOTO1 =V(DATCON),DMCB,(0,C),(6,DATX)                                   
         MVC   TITLE(26),=C'TERMINATIONS FOR MONTH OF '                         
         MVC   TITLE+28(3),DATX                                                 
         MVC   P(24),=C'CONTROL MONTH REQUESTED='                               
         MVC   P+24(2),C+2                                                      
         BAS   RE,PRT                                                           
         ZAP   LINE,=P'75'                                                      
*&&DO                                                                           
GPR      GET   PRIN                                                             
*&&                                                                             
*&&OS                                                                           
GPR      GET   PRIN                                                             
         LR    R2,R1                                                            
*&&                                                                             
         CLI   228(R2),C'T'                                                     
         BNE   GPR                                                              
         CLC   199(2,R2),C+2                                                    
         BNE   GPR                                                              
         MVC   P(5),=C'NO DT'                                                   
         CLC   193(6,R2),SPACES                                                 
         BE    NO1                                                              
         MVC   C+40(2),197(R2)                                                  
         MVC   C+42(4),193(R2)                                                  
         GOTO1 =V(DATCON),DMCB,(0,C+40),(8,P)                                   
NO1      MVC   P+55(5),=C'NO DT'                                                
         CLC   199(6,R2),SPACES                                                 
         BE    NO2                                                              
         MVC   C+40(2),203(R2)                                                  
         MVC   C+42(4),199(R2)                                                  
         GOTO1 =V(DATCON),DMCB,(0,C+40),(8,P+55)                                
NO2      MVC   P+65(5),=C'NO DT'                                                
         CLC   187(6,R2),SPACES                                                 
         BE    NO3                                                              
         MVC   C+40(2),191(R2)                                                  
         MVC   C+42(4),187(R2)                                                  
         GOTO1 =V(DATCON),DMCB,(0,C+40),(8,P+65)                                
NO3      MVC   P+11(30),58(R2)                                                  
         MVC   P+42(10),=C'   -  -    '                                         
         MVC   P+42(3),178(R2)                                                  
         MVC   P+46(2),181(R2)                                                  
         MVC   P+49(4),183(R2)                                                  
         BAS   RE,PRT                                                           
         CLC   88(30,R2),SPACES                                                 
         BE    N1                                                               
         MVC   P+11(30),88(R2)                                                  
         BAS   RE,PRT                                                           
N1       CLC   118(30,R2),SPACES                                                
         BE    N2                                                               
         MVC   P+11(30),118(R2)                                                 
         BAS   RE,PRT                                                           
N2       CLC   148(30,R2),SPACES                                                
         BE    N3                                                               
         MVC   P+11(30),148(R2)                                                 
         BAS   RE,PRT                                                           
N3       BAS   RE,PRT                                                           
         B     GPR                                                              
PRT      NTR1                                                                   
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*&&DO                                                                           
EOD      CLOSE PRIN                                                             
*&&                                                                             
*&&OS                                                                           
EOD      CLOSE (PRIN,)                                                          
*&&                                                                             
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
         XBASE                                                                  
         EJECT                                                                  
DATX     DS    CL9                                                              
HDNGS    DC    C'HIRE DT.        NAME AND ADDRESS            '                  
         DC    C'S.S. NO.   TERM. DT.  D.O.B.'                                  
*&&DO                                                                           
PRIN     DTFSD BLKSIZE=720,EOFADDR=EOD,IOREG=(2)                                
*&&                                                                             
*&&OS                                                                           
PRIN     DCB   DDNAME=PRIN,                                            X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00720,                                            X        
               BLKSIZE=00720,          DOS BLKSIZE=00720               X        
               MACRF=GL,                                               X        
               EODAD=EOD                                                        
*&&                                                                             
C        DS    CL80                                                             
DMCB     DS    6F                                                               
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PRTERM    05/01/02'                                      
         END                                                                    
