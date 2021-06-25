*          DATA SET DDODDLIST  AT LEVEL 006 AS OF 05/01/02                      
*PHASE ODDLIST,*                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE SQUASHER                                                               
         TITLE 'ODDLIST - SUBSIDIARY NAME AND TELEPHONE LIST'                   
ODDLIST  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**ODDL**,=V(REGSAVE),RA                                        
         SPACE 1                                                                
         OPEN  (MARKIN,(INPUT))                                                 
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(255),SPACES                                             
         SPACE 1                                                                
         GOTO1 =V(DATCON),PARA,(5,0),(0,WORK)                                   
         PACK  DUB,WORK+2(2)       TRANSLATE MONTH                              
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         MH    R1,=H'10'                                                        
         LA    R1,MONTABLE(R1)                                                  
         MVC   DATE(9),1(R1)       UP TO 9 CHARACTER LOWER CASE                 
         ZIC   R2,0(R1)            PICK UP LENGTH                               
         LA    R2,DATE+1(R2)                                                    
         MVC   0(2,R2),=C'19'                                                   
         MVC   2(2,R2),WORK        POP IN YEAR                                  
         MVC   HEADA+134(20),DATE                                               
         EJECT                                                                  
*              MAIN PROGRAM FLOW                                                
         SPACE 3                                                                
         GET   MARKIN,IO                                                        
         OC    IO,SPACES                                                        
         L     R2,=A(ABLOCK)                                                    
         BAS   RE,SCAN                                                          
         LA    R4,STACK                                                         
         LA    R5,100                                                           
         SPACE 1                                                                
READ     MVC   IO,SPACES                                                        
         GET   MARKIN,IO                                                        
         L     R2,=A(BBLOCK)                                                    
         BAS   RE,SCAN                                                          
         SPACE 1                                                                
         GOTO1 GETWORD,DMCB,=C'NAME',1(R4)                                      
         GOTO1 GETWORD,DMCB,=C'PROFS',46(R4)                                    
         OC    46(8,R4),SPACES                                                  
         GOTO1 GETWORD,DMCB,=C'EXT',55(R4)                                      
         GOTO1 GETWORD,DMCB,=C'TELEPHONE',63(R4)                                
         LA    R4,80(R4)                                                        
         BCT   R5,READ                                                          
         BAS   RE,PAGE                                                          
         LA    R4,STACK                                                         
         LA    R5,100                                                           
         B     READ                                                             
         SPACE 1                                                                
MARKEOF  CLOSE (MARKIN)                                                         
         BAS   RE,PAGE                                                          
         GOTO1 =V(PRINT),PARA,P-1,=C'BC01'                                      
         GOTO1 =V(PRINT),PARA,=C'CLOSE'                                         
         XBASE                                                                  
         EJECT                                                                  
*              PRINT OUT PAGE                                                   
         SPACE 3                                                                
PAGE     NTR1                                                                   
         GOTO1 =V(PRINT),PARA,P-1,=C'BC01'                                      
         BAS   RE,BOXSETA                                                       
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,HEADA                                                          
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         SPACE 1                                                                
         BAS   RE,BOXSET                                                        
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,PATTERNA                                                       
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         LA    R2,STACK                                                         
         LA    R3,50                                                            
         MH    R3,=H'80'                                                        
         LR    R4,R3                                                            
         AR    R3,R2                                                            
         LA    R0,50                                                            
         SPACE 1                                                                
PAGE2    MVC   P+2(80),0(R2)                                                    
         MVC   0(80,R2),SPACES                                                  
         MVC   P+79(80),0(R3)                                                   
         MVC   0(80,R3),SPACES                                                  
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
         LA    R2,80(R2)                                                        
         LA    R3,80(R3)                                                        
         BCT   R0,PAGE2                                                         
         B     XIT                                                              
         SPACE 1                                                                
BOXSET   NTR1                                                                   
         L     R8,=V(BOXAREA)                                                   
         USING BOXD,R8                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXWIDTH+3,165                                                   
         MVC   BOXCOLS(165),ACOLS                                               
         MVC   BOXROWS(60),MYROWS                                               
         B     XIT                                                              
         SPACE 1                                                                
BOXSETA  NTR1                                                                   
         L     R8,=V(BOXAREA)                                                   
         USING BOXD,R8                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXWIDTH+3,165                                                   
         MVC   BOXCOLS(165),HEADCOLS                                            
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS,C'T'                                                     
         MVI   BOXROWS+2,C'B'                                                   
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
WORK     DS    CL32                                                             
DMCB     DS    6F                                                               
SPACES   DS    CL256                                                            
CITY     DS    CL64                                                             
HEAD     DS    CL8                                                              
IO       DS    CL256                                                            
LASTLET  DC    C' '                                                             
PARA     DS    6F                                                               
PFILL    DS    CL1                                                              
P        DC    CL165' '                                                         
P2       DC    CL165' '                                                         
P3       DC    CL165' '                                                         
ACOLS    DS    0F                                                               
         DC    CL2' '                                                           
         DC    C'L'                                                             
         DC    CL44' '                                                          
         DC    C'C'                                                             
         DC    CL8' '                                                           
         DC    C'C'                                                             
         DC    CL7' '                                                           
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'R'                                                             
         DC    CL1' '                                                           
         DC    C'L'                                                             
         DC    CL44' '                                                          
         DC    C'C'                                                             
         DC    CL8' '                                                           
         DC    C'C'                                                             
         DC    CL7' '                                                           
         DC    C'C'                                                             
         DC    CL12' '                                                          
         DC    C'R'                                                             
         DC    100C' '                                                          
         SPACE 1                                                                
HEADCOLS DC    CL2' '                                                           
         DC    C'L'                                                             
         DC    CL51' '                                                          
         DC    C'R'                                                             
         DC    CL45' '                                                          
         DC    CL2' '                                                           
         DC    C'L'                                                             
         DC    CL51' '                                                          
         DC    C'R'                                                             
         DC    CL12' '                                                          
MYROWS   DC    C'     T M'                                                      
         DC    50C' '                                                           
         DC    CL100'B'                                                         
         SPACE 1                                                                
PATTERNA DS    0F                                                               
         DC    CL2' '              GAP                                          
         DC    CL1' '                                                           
         DC    CL44'                Name/Department'                            
         DC    CL1' '                                                           
         DC    CL8' Profs.'                                                     
         DC    CL1' '                                                           
         DC    CL7'  Ext  '                                                     
         DC    CL1' '                                                           
         DC    CL12' Telephone '                                                
         DC    CL1' '                                                           
         DC    CL1' '              GAP                                          
         DC    CL1' '                                                           
         DC    CL44'                Name/Department'                            
         DC    CL1' '                                                           
         DC    CL8' Profs.'                                                     
         DC    CL1' '                                                           
         DC    CL7'  Ext  '                                                     
         DC    CL1' '                                                           
         DC    CL12' Telephone '                                                
         DC    CL1' '                                                           
         SPACE 1                                                                
HEADA    DC    CL14' '                                                          
         DC    CL28'    DDS Telephone Listing   '                               
         DC    CL65' '                                                          
         DC    CL38'Subsidiary Listing                     '                    
         DC    CL28' '                                                          
         SPACE 1                                                                
MONTABLE DS    0F                                                               
         DC    AL1(7),CL9'January'                                              
         DC    AL1(8),CL9'February'                                             
         DC    AL1(5),CL9'March'                                                
         DC    AL1(5),CL9'April'                                                
         DC    AL1(3),CL9'May'                                                  
         DC    AL1(4),CL9'June'                                                 
         DC    AL1(4),CL9'July'                                                 
         DC    AL1(6),CL9'August'                                               
         DC    AL1(9),CL9'September'                                            
         DC    AL1(7),CL9'October'                                              
         DC    AL1(8),CL9'November'                                             
         DC    AL1(8),CL9'December'                                             
         SPACE 1                                                                
DATE     DC    CL20' '                                                          
MARKIN   DCB   DSORG=PS,EODAD=MARKEOF,RECFM=FB,BLKSIZE=2640,           X        
               DDNAME=MARKIN,MACRF=(GM),LRECL=132                               
         SPACE 1                                                                
         LTORG                                                                  
STACK    DC    200CL80' '                                                       
ABLOCK   DS    20CL60                                                           
BBLOCK   DS    20CL60                                                           
       ++INCLUDE DDBIGBOX                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DDODDLIST 05/01/02'                                      
         END                                                                    
