*          DATA SET DDANYPRINT AT LEVEL 004 AS OF 05/01/02                      
*PHASE ANYPRINT,*                                                               
         TITLE 'FORMATS CARD INPUT DATA FOR PRINTING'                           
ANYPRINT CSECT                                                                  
         NBASE 0,*ANYPRIN                                                       
         LA    3,PRINT+133                                                      
         LA    13,REGSAVE                                                       
         LA    8,1                                                              
         LA    1,CARD                                                           
         ST    1,LIST                                                           
         LA    1,PARAS                                                          
         ST    1,LIST+4                                                         
         LA    1,LIST                                                           
         EJECT                                                                  
*                        READING DATA CARDS                                     
         SPACE 3                                                                
         L     15,=V(CARDS)                                                     
         SPACE 3                                                                
ANY2     BASR  14,15                                                            
         CLC   CARD(2),=C'/*'                                                   
         BC    8,ANY10                                                          
         CLI   CARD+1,C'0'         VALIDATE NUMERIC                             
         BC    4,ANY2                                                           
         CLI   CARD+2,C'0'                                                      
         BC    4,ANY2                                                           
         CLI   CARD,C'0'                                                        
         BC    4,ANY4                                                           
         PACK  DUB,CARD(3)         TIMES CARD                                   
         CVB   8,DUB                                                            
         B     ANY2                                                             
         SPACE 3                                                                
ANY4     PACK  DUB,CARD+1(2)       LINE NUMBER                                  
         CVB   2,DUB                                                            
         CH    2,=H'60'                                                         
         BC    12,ANY6                                                          
         LA    2,60                                                             
         SPACE 3                                                                
ANY6     BCTR  2,0                 ADDRESS IN TABLE                             
         MH    2,=H'133'                                                        
         LA    2,1(2)                                                           
         CLI   CARD,C'R'           RIGHT SIDE OF PAGE                           
         BC    7,ANY8                                                           
         LA    2,66(2)                                                          
         SPACE 3                                                                
ANY8     AR    2,3                                                              
         MVC   0(66,2),CARD+3      STRIP DATA OFF CARD                          
         B     ANY2                                                             
         EJECT                                                                  
*                        PRINTING FROM TABLE N TIMES                            
         SPACE 3                                                                
ANY10    MVC   PARAS(2),=C'BC'                                                  
         LA    4,PRINT                                                          
         ST    4,LIST                                                           
         LA    1,LIST                                                           
         L     15,=V(PRINT)                                                     
         BASR  14,15                                                            
         LA    4,133(4)                                                         
         LA    5,60                                                             
         SPACE 3                                                                
ANY14    ST    4,LIST                                                           
         MVI   PARAS+1,C'L'                                                     
         BASR  14,15                                                            
         LA    4,133(4)                                                         
         BCT   5,ANY14             PRINT 60 LINES (3-62)                        
         BCT   8,ANY10             PRINT N PAGES                                
         XBASE                                                                  
         EJECT                                                                  
DUB      DS    D                                                                
REGSAVE  DS    CL160                                                            
LIST     DS    CL8                                                              
CARD     DS    CL80                                                             
PARAS    DC    C'RE01'                                                          
         LTORG                                                                  
PRINT    DC    63CL133' '                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDANYPRINT05/01/02'                                      
         END                                                                    
