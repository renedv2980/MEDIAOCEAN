*          DATA SET DDPVTM2    AT LEVEL 056 AS OF 05/01/02                      
*PHASE DDPVTM2                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PERVALA                                                                
         TITLE 'TEST PROGRAM FOR PERVALA INC EURO DATES'                        
TEST     CSECT                                                                  
         NBASE 0,**TEST**,WORK                                                  
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         LA    R9,RET                                                           
         USING PERVALD,R9                                                       
         MVC   TITLE(20),=CL20'PERVALA TEST MK3 L0'                             
         MVC   MID1+15(5),=C'INPUT'                                             
         MVC   MID1+39(17),=C'RC #DYS #WKS #MNS'                                
         MVC   MID1+63(6),=C'PERIOD'                                            
         MVC   MID1+75(5),=C'BIN/S'                                             
         MVC   MID1+82(5),=C'BIN/E'                                             
         MVC   MID1+89(9),=C'CP/S CP/E'                                         
         MVC   MID1+99(13),=C'PWOS/S PWOS/E'                                    
         MVC   MID1+113(13),=C'CHAR/S CHAR/E'                                   
         MVC   MID1+127(4),=C'FLGS'                                             
         MVC   MID2+15(5),=C'-----'                                             
         MVC   MID2+39(17),=C'-- ---- ---- ----'                                
         MVC   MID2+63(6),=C'------'                                            
         MVC   MID2+75(5),=C'-----'                                             
         MVC   MID2+82(5),=C'-----'                                             
         MVC   MID2+89(9),=C'---- ----'                                         
         MVC   MID2+99(13),=C'------ ------'                                    
         MVC   MID2+113(13),=C'------ ------'                                   
         MVC   MID2+127(4),=C'----'                                             
*                                                                               
START    EQU   *                                                                
         GOTO1 =V(CARDS),PARAM,C,=C'RE00'                                       
         CLC   C(2),=C'/*'                                                      
         BE    EXIT                                                             
         CLC   C(2),=C'**'                                                      
         BNE   INCHK0                                                           
         MVC   TITLE+19(1),C+7                                                  
         MVC   LANG,C+7                                                         
         NI    LANG,X'0F'                                                       
         B     START                                                            
*                                                                               
INCHK0   EQU   *                                                                
         LA    R2,30                                                            
         LA    R3,C+29                                                          
INCHK1   EQU   *                                                                
         CLI   0(R3),C' '                                                       
         BNE   INCHK2                                                           
         BCTR  R3,R0                                                            
         BCT   R2,INCHK1                                                        
         LA    R2,10               FAKE INP LENGTH FOR TESTING                  
         B     INCHK2              BLANK INTERPRETATION                         
INMV     MVC   IN(0),C                                                          
INCHK2   EQU   *                                                                
         STC   R2,INL                                                           
         MVI   IN,0                                                             
         MVC   IN+1(29),IN                                                      
         LTR   R2,R2                                                            
         BZ    INCHK3                                                           
         BCTR  R2,R0                                                            
         EX    R2,INMV                                                          
INCHK3   EQU   *                                                                
         MVC   OPT,C+34                                                         
         NI    OPT,X'0F'                                                        
         UNPK  WK(1),OPT(1)        FLIP NIBBLES                                 
         OC    WK,LANG                                                          
         MVC   P(38),IN                                                         
         MVC   P+34(1),C+34                                                     
         GOTO1 =V(PERVAL),PARAM,(INL,IN),(WK,RET)                               
         LA    R2,4(R1)                 RETURN CODE.                            
         LA    R3,P+39                                                          
         LA    R4,1                                                             
         BAS   R8,EXTR                                                          
         LA    R2,PVALNDYS              # OF DAYS.                              
         LA    R3,P+42                                                          
         LA    R4,2                                                             
         BAS   R8,EXTR                                                          
         LA    R2,PVALNWKS              # OF WEEKS.                             
         LA    R3,P+47                                                          
         LA    R4,2                                                             
         BAS   R8,EXTR                                                          
         LA    R2,PVALNMNS              # OF MONTHS.                            
         LA    R3,P+52                                                          
         LA    R4,2                                                             
         BAS   R8,EXTR                                                          
         MVC   P+57(17),PVALCPER        CHAR PERIOD.                            
         LA    R2,PVALBSTA              BINARY START.                           
         LA    R3,P+75                                                          
         LA    R4,3                                                             
         BAS   R8,EXTR                                                          
         LA    R2,PVALBEND              BINARY END.                             
         LA    R3,P+82                                                          
         LA    R4,3                                                             
         BAS   R8,EXTR                                                          
         LA    R2,PVALCSTA              CMPR START.                             
         LA    R3,P+89                                                          
         LA    R4,2                                                             
         BAS   R8,EXTR                                                          
         LA    R2,PVALCEND              CMPR END.                               
         LA    R3,P+94                                                          
         LA    R4,2                                                             
         BAS   R8,EXTR                                                          
         LA    R2,PVALPSTA              PWOS START.                             
         LA    R3,P+99                                                          
         LA    R4,3                                                             
         BAS   R8,EXTR                                                          
         LA    R2,PVALPEND              PWOS END.                               
         LA    R3,P+106                                                         
         LA    R4,3                                                             
         BAS   R8,EXTR                                                          
         MVC   P+113(6),PVALESTA        CHAR START.                             
         MVC   P+120(6),PVALEEND        CHAR END.                               
         LA    R2,PVALASSM              FLAG BYTES.                             
         LA    R3,P+127                                                         
         LA    R4,1                                                             
         BAS   R8,EXTR                                                          
PRINT01  EQU   *                                                                
         GOTO1  =V(PRINTER)                                                     
         B     START                                                            
EXTR     EQU   *                                                                
         SR    R6,R6                                                            
         LR    R7,R6                                                            
         IC    R6,0(R2)                                                         
         SRDL  R6,4                                                             
         CH    R6,=H'9'                                                         
         BH    EXTR01                                                           
         LA    R6,240(R6)                                                       
         B     EXTR02                                                           
EXTR01   EQU   *                                                                
         LA    R6,183(R6)                                                       
EXTR02   EQU   *                                                                
         STC   R6,0(R3)                                                         
         SRL   R7,28                                                            
         CH    R7,=H'9'                                                         
         BH    EXTR03                                                           
         LA    R7,240(R7)                                                       
         B     EXTR04                                                           
EXTR03   EQU   *                                                                
         LA    R7,183(R7)                                                       
EXTR04   EQU   *                                                                
         STC   R7,1(R3)                                                         
         LA    R2,1(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,EXTR                                                          
         BR    R8                                                               
*                                                                               
*                                                                               
EXIT     XBASE                                                                  
*                                                                               
ERROR    DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
PARAM    DS    6F                                                               
C        DS    CL80                                                             
IN       DS    CL80                                                             
INL      DS    CL1                                                              
LANG     DS    X                                                                
OPT      DS    X                                                                
WK       DS    X                                                                
RET      DS    CL100                                                            
WORK     DS    1000D                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         ORG   P+50                                                             
       ++INCLUDE DDPERVALD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056DDPVTM2   05/01/02'                                      
         END                                                                    
