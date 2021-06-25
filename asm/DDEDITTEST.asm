*          DATA SET DDEDITTEST AT LEVEL 012 AS OF 05/01/02                      
*PHASE EDITTEST,*                                                               
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE EDITOR                                                                 
*INCLUDE HEXIN                                                                  
         TITLE 'EDITTEST - TEST MODULE FOR EDITOR'                              
EDITTEST CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**TEST**,=V(REGSAVE)                                           
         SPACE 1                                                                
READ     GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BNE   READ2                                                            
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
         XBASE                                                                  
         SPACE 1                                                                
READ2    MVC   P(80),C                                                          
         GOTO1 =V(PRINT),DMCB,P1F,=C'BL01'                                      
         MVC   P(80),SPACES                                                     
         CLC   C(4),=C'LEN='                                                    
         BNE   READ4                                                            
         PACK  DUB,C+4(2)                                                       
         CVB   R1,DUB                                                           
         STC   R1,EBLOUT                                                        
         B     READ                                                             
         SPACE 1                                                                
READ4    CLC   C(5),=C'DECS='                                                   
         BNE   READ6                                                            
         PACK  DUB,C+5(1)                                                       
         CVB   R1,DUB                                                           
         STC   R1,EBDECS                                                        
         B     READ                                                             
         SPACE 1                                                                
READ6    CLC   C(5),=C'FILL='                                                   
         BNE   READ8                                                            
         MVC   EBFILL,C+5                                                       
         B     READ                                                             
         SPACE 1                                                                
READ8    CLC   C(6),=C'FLOAT='                                                  
         BNE   READ10                                                           
         MVC   EBFLOAT,C+6                                                      
         B     READ                                                             
         SPACE 1                                                                
READ10   CLC   C(6),=C'ROUND='                                                  
         BNE   READ12                                                           
         MVC   EBROUND,C+6                                                      
         NI    EBROUND,X'0F'                                                    
         CLI   C+6,C'-'                                                         
         BNE   READ                                                             
         MVC   EBROUND,C+7                                                      
         NI    EBROUND,X'0F'                                                    
         OI    EBROUND,X'80'                                                    
         B     READ                                                             
         SPACE 1                                                                
READ12   CLC   C(6),=C'COMMAS'                                                  
         BNE   READ14                                                           
         OI    EBOPT,X'80'                                                      
         B     READ                                                             
         SPACE 1                                                                
READ14   CLC   C(6),=C'MINUS=YES'                                               
         BNE   READ16                                                           
         OI    EBOPT,X'40'                                                      
         B     READ                                                             
         SPACE 1                                                                
READ16   CLC   C(6),=C'ZERO=NOBLANK'                                            
         BNE   READ18                                                           
         OI    EBOPT,X'20'                                                      
         B     READ                                                             
         SPACE 1                                                                
READ18   CLC   C(9),=C'BRACKET=YES'                                             
         BNE   READ19                                                           
         OI    EBOPT,X'10'                                                      
         B     READ                                                             
         SPACE 1                                                                
READ19   CLC   C(9),=C'BRACKET=MINUS'                                           
         BNE   READ20                                                           
         OI    EBOPT,X'08'                                                      
         B     READ                                                             
         SPACE 1                                                                
READ20   CLC   C(9),=C'TRIM=DECS'                                               
         BNE   READ22                                                           
         OI    EBTRIM,X'80'                                                     
         B     READ                                                             
         SPACE 1                                                                
READ22   CLC   C(9),=C'TRIM=BELOW'                                              
         BNE   READ24                                                           
         OI    EBTRIM,X'40'                                                     
         B     READ                                                             
         SPACE 1                                                                
READ24   CLC   C(6),=C'ALIGN='                                                  
         BNE   READ26                                                           
         MVC   EBALIGN,C+6                                                      
         B     READ                                                             
         SPACE 1                                                                
READ26   CLC   C(4),=C'TEST'                                                    
         BNE   READ28                                                           
         BAS   RE,TEST                                                          
         B     READ                                                             
         SPACE 1                                                                
READ28   CLC   C(5),=C'CLEAR'                                                   
         BNE   READ30                                                           
         XC    EBLOCK,EBLOCK                                                    
         B     READ                                                             
         SPACE 1                                                                
READ30   CLC   C(5),=C'SCALE'                                                   
         BNE   READ                                                             
         GOTO1 =V(HEXIN),DMCB,C+6,EBSCIN,2                                      
         GOTO1 =V(HEXIN),DMCB,C+8,EBSCOUT,2                                     
         GOTO1 =V(HEXIN),DMCB,C+10,EBDECS,2                                     
         B     READ                                                             
         EJECT                                                                  
*              DO SOME TESTING                                                  
         SPACE 3                                                                
TEST     NTR1                                                                   
         SR    R3,R3                                                            
         LA    R0,15                                                            
         SPACE 1                                                                
TEST2    EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FIFTEEN(0)                                                   
         MP    FACTOR,=PL1'-1'                                                  
         CP    FACTOR,=PL1'-1'                                                  
         BNE   TEST4                                                            
         NI    DUB+7,X'FD'                                                      
         SPACE 1                                                                
TEST4    LA    R2,DUB                                                           
         ST    R2,EBAIN                                                         
         MVI   EBTIN,C'P'                                                       
         MVI   EBLIN,8                                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),FIFTEEN                                                     
         MVI   P+20,C'*'                                                        
         LA    R2,P+22                                                          
         ST    R2,EBAOUT                                                        
         GOTO1 =V(EDITOR),DMCB,EBLOCK                                           
         GOTO1 =V(PRINT),DMCB,P1F,=C'BL01'                                      
         GOTO1 =V(PRINT),DMCB,P2F,=C'BL01'                                      
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         LA    R3,1(R3)                                                         
         BCT   R0,TEST2                                                         
         GOTO1 =V(PRINT),DMCB,P1F,=C'BC01'                                      
         B     READ                                                             
         XIT1                                                                   
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
DMCB     DS    6F                                                               
DUB      DS    D                                                                
C        DS    CL80                                                             
P1F      DC    X'00'                                                            
P        DC    CL132' '                                                         
P2F      DC    X'00'                                                            
P2       DC    CL132' '                                                         
SPACES   DC    CL132' '                                                         
FIFTEEN  DC    C'123456789012345'                                               
FACTOR   DC    PL2'1'                                                           
         SPACE 3                                                                
       ++INCLUDE DDEBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012DDEDITTEST05/01/02'                                      
         END                                                                    
         TITLE 'EDITOR - OUTPUT EDITOR'                                         
EDITOR   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 20,**EDIT**                                                      
         USING EDITD,RC                                                         
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              DSECT FOR EDITOR                                                 
         SPACE 3                                                                
EDITD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL32                                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
         END                                                                    
