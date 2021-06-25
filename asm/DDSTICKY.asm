*          DATA SET DDSTICKY   AT LEVEL 014 AS OF 05/01/02                      
*PHASE STICKY,*                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'STICKY - 2 UP LABEL PRINTING'                                   
STICKY   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**STIC**,=V(REGSAVE),RA                                        
         SPACE 1                                                                
         OPEN  (MARKIN,(INPUT))                                                 
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(255),SPACES                                             
         SPACE 1                                                                
         EJECT                                                                  
*              MAIN PROGRAM FLOW                                                
         SPACE 3                                                                
         GET   MARKIN,IO                                                        
         OC    IO,SPACES                                                        
         L     R2,=A(ABLOCK)                                                    
         BAS   RE,SCAN                                                          
         LA    R4,STACK                                                         
         LA    R5,2                                                             
         SPACE 1                                                                
READ     MVC   IO,SPACES                                                        
         GET   MARKIN,IO                                                        
         B     LABELS                                                           
************************************************************                    
         L     R2,=A(BBLOCK)                                                    
         BAS   RE,SCAN                                                          
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 GETWORD,DMCB,=C'FIRST',WORK                                      
         GOTO1 GETWORD,DMCB,=C'NAME',WORK+32                                    
         GOTO1 =V(SQUASHER),DMCB,WORK,64                                        
         MVC   0(30,R4),WORK                                                    
         GOTO1 GETWORD,DMCB,=C'ADDRESS1',30(R4)                                 
         GOTO1 GETWORD,DMCB,=C'ADDRESS2',60(R4)                                 
         MVC   WORK,SPACES                                                      
         GOTO1 GETWORD,DMCB,=C'CITY',WORK                                       
         GOTO1 GETWORD,DMCB,=C'STATE',WORK+20                                   
         GOTO1 GETWORD,DMCB,=C'ZIP',WORK+30                                     
         GOTO1 =V(SQUASHER),DMCB,WORK,64                                        
         MVC   90(30,R4),WORK                                                   
         GOTO1 GETWORD,DMCB,=C'TELEPHONE',120(R4)                               
* NOW PRINT IT                                                                  
         ZAP   DUB,=P'1'                                                        
         LA    R4,STACK                                                         
         LA    R5,5                                                             
COUNT    CLC   0(30,R4),SPACES                                                  
         BE    *+10                                                             
         AP    DUB,=P'1'                                                        
         LA    R4,30(R4)                                                        
         BCT   R5,COUNT                                                         
*                                                                               
         AP    DUB,LINE            ADD TO CURRENT LINE COUNT                    
         LA    R0,=C'BL01'         SET TO SKIP A LINE                           
         CP    DUB,=P'59'          TEST WILL GO OFF PAGE                        
         BNH   PRINT               NO                                           
         LA    R0,=C'BC01'         SET TO SKIP TO NEW PAGE                      
         ZAP   LINE,=P'0'          AND CLEAR COUNTER                            
*                                                                               
PRINT    MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PARA,P-1,(R0)                                          
         AP    LINE,=P'1'                                                       
*                                                                               
         LA    R4,STACK                                                         
         LA    R5,5                                                             
PRT2     MVC   P,SPACES                                                         
         MVC   P+2(30),0(R4)                                                    
         CLC   P(30),SPACES                                                     
         BNH   PRT4                                                             
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         AP    LINE,=P'1'                                                       
PRT4     LA    R4,30(R4)                                                        
         BCT   R5,PRT2                                                          
*                                                                               
         LA    R4,STACK                                                         
         MVC   STACK,SPACES                                                     
         B     READ                                                             
LINE     DC    PL2'0'                                                           
********************************************************                        
LABELS   OC    IO,SPACES           NO UPPER/LOWER CASE                          
         L     R2,=A(BBLOCK)                                                    
         BAS   RE,SCAN                                                          
         MVC   WORK,SPACES                                                      
         GOTO1 GETWORD,DMCB,=C'CITY',WORK                                       
         CLI   WORK,X'41'                                                       
         BL    READ                IGNORE THOSE WITHOUT CITIES                  
         SPACE 1                                                                
         MVC   WORK,SPACES                                                      
         GOTO1 GETWORD,DMCB,=C'FIRST',WORK                                      
         GOTO1 GETWORD,DMCB,=C'NAME',WORK+32                                    
         GOTO1 =V(SQUASHER),DMCB,WORK,64                                        
         MVC   0(30,R4),WORK                                                    
         GOTO1 GETWORD,DMCB,=C'ADDRESS1',30(R4)                                 
         GOTO1 GETWORD,DMCB,=C'ADDRESS2',60(R4)                                 
         MVC   WORK,SPACES                                                      
         GOTO1 GETWORD,DMCB,=C'CITY',WORK                                       
         GOTO1 GETWORD,DMCB,=C'STATE',WORK+20                                   
         GOTO1 GETWORD,DMCB,=C'ZIP',WORK+30                                     
         GOTO1 =V(SQUASHER),DMCB,WORK,64                                        
         MVC   90(30,R4),WORK                                                   
         CLC   60(30,R4),SPACES    SHUFFLE UP IF NO ADDRESS2                    
         BNE   *+16                                                             
         MVC   60(30,R4),90(R4)                                                 
         MVC   90(30,R4),SPACES                                                 
         SPACE 1                                                                
         LA    R4,L'STACK(R4)                                                   
         BCT   R5,READ                                                          
         BAS   RE,PAGE                                                          
         LA    R4,STACK                                                         
         LA    R5,2                                                             
         B     READ                                                             
         SPACE 1                                                                
MARKEOF  CLOSE (MARKIN)                                                         
         BAS   RE,PAGE                                                          
         GOTO1 =V(PRINT),PARA,=C'CLOSE'                                         
         XBASE                                                                  
         EJECT                                                                  
*              PRINT OUT PAGE (2 LABELS)                                        
         SPACE 3                                                                
PAGE     NTR1                                                                   
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
         MVC   P+2(30),STACK                                                    
         MVC   P+38(30),STACK2                                                  
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
         MVC   P+2(30),STACK+30                                                 
         MVC   P+38(30),STACK2+30                                               
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
         MVC   P+2(30),STACK+60                                                 
         MVC   P+38(30),STACK2+60                                               
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
         MVC   P+2(30),STACK+90                                                 
         MVC   P+38(30),STACK2+90                                               
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   STACK,SPACES                                                     
         MVC   STACK2,SPACES                                                    
         B     XIT                                                              
         EJECT                                                                  
*              SCANNING ROUTINES                                                
         SPACE 3                                                                
SCAN     NTR1                                                                   
         LR    R1,R2               CLEAR FIRST                                  
         LA    R0,20                                                            
         SPACE 1                                                                
SCAN2    XC    0(12,R1),0(R1)                                                   
         MVC   12(48,R1),SPACES                                                 
         LA    R1,60(R1)                                                        
         BCT   R0,SCAN2                                                         
         SPACE 1                                                                
         CLC   IO(132),SPACES                                                   
         BE    XIT                                                              
         LA    R1,IO+131                                                        
         LA    R0,132                                                           
         SPACE 1                                                                
SCAN4    CLI   0(R1),C' '          FIND LENGTH OF INPUT                         
         BNE   SCAN6                                                            
         BCTR  R1,0                                                             
         BCT   R0,SCAN4                                                         
         SPACE 1                                                                
SCAN6    STC   R0,HEAD+5                                                        
         GOTO1 =V(SCANNER),DMCB,(38,HEAD),(20,(R2)),0                           
         B     XIT                                                              
         EJECT                                                                  
*              PICK OUT SELECTED WORD                                           
         SPACE 3                                                                
GETWORD  NTR1                                                                   
         LR    R9,R1                                                            
         MVI   0(R9),0                                                          
         LM    R4,R5,0(R1)         A(SEARCH WORD)                               
*                                  A(OUTPUT)                                    
         L     R2,=A(ABLOCK)                                                    
         L     R3,=A(BBLOCK)                                                    
         LA    R0,20                                                            
         SPACE 1                                                                
GW2      ZIC   R1,0(R2)            L'STACKED KEY WORD                           
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),0(R4)                                                   
         BE    GW4                                                              
         LA    R2,60(R2)                                                        
         LA    R3,60(R3)                                                        
         BCT   R0,GW2                                                           
         B     XIT                                                              
         SPACE 1                                                                
GW4      ZIC   R1,0(R3)            L'DATA                                       
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         STC   R1,0(R9)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),12(R3)                                                   
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
SPACES   DS    CL256                                                            
HEAD     DS    XL8'00'                                                          
IO       DS    CL256                                                            
PARA     DS    6F                                                               
PFILL    DS    CL1                                                              
P        DC    CL165' '                                                         
MARKIN   DCB   DSORG=PS,EODAD=MARKEOF,RECFM=FB,BLKSIZE=2640,           X        
               DDNAME=MARKIN,MACRF=(GM),LRECL=132                               
         SPACE 1                                                                
         LTORG                                                                  
STACK    DC    CL200' '                                                         
STACK2   DC    CL200' '                                                         
ABLOCK   DS    20CL60                                                           
BBLOCK   DS    20CL60                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014DDSTICKY  05/01/02'                                      
         END                                                                    
