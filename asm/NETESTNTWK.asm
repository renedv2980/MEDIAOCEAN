*          DATA SET NETESTNTWK AT LEVEL 006 AS OF 08/10/00                      
*PHASE TESTNTWA TESTNTWK                                                        
*INCLUDE ADDAY                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE NETWEEK                                                                
*INCLUDE REGSAVE                                                                
TESTNTWK CSECT                                                                  
         NBASE 0,TEST,=V(REGSAVE)                                               
         PRINT NOGEN                                                            
*                                                                               
* 1992 *                                                                        
*                                                                               
         LA    R2,53                                                            
LOOP     GOTO1 =V(NETWEEK),DMCB,DATE,V(GETDAY),V(ADDAY)                         
         MVC   P(6),DATE                                                        
         MVC   P+10(8),=C'HUT WEEK'                                             
         EDIT  (1,DMCB),(2,P+20)                                                
         MVC   P+30(8),=C'RTG WEEK'                                             
         EDIT  (1,DMCB+8),(2,P+40)                                              
         GOTO1 =V(PRINT),DMCB,P-1,=C'BL01'                                      
         GOTO1 =V(ADDAY),DMCB,DATE,DATE+6,7                                     
         MVC   DATE(6),DATE+6                                                   
         BCT   R2,LOOP                                                          
*                                                                               
* 1995 *                                                                        
*                                                                               
         LA    R2,53                                                            
         MVC   DATE,=CL12'941226'                                               
LOOP2    GOTO1 =V(NETWEEK),DMCB,DATE,V(GETDAY),V(ADDAY)                         
         MVC   P(6),DATE                                                        
         MVC   P+10(8),=C'HUT WEEK'                                             
         EDIT  (1,DMCB),(2,P+20)                                                
         MVC   P+30(8),=C'RTG WEEK'                                             
         EDIT  (1,DMCB+8),(2,P+40)                                              
         GOTO1 =V(PRINT),DMCB,P-1,=C'BL01'                                      
         GOTO1 =V(ADDAY),DMCB,DATE,DATE+6,7                                     
         MVC   DATE(6),DATE+6                                                   
         BCT   R2,LOOP2                                                         
                                                                                
         XBASE                                                                  
DATE     DC    CL12'911230'                                                     
DMCB     DS    6F                                                               
WORK     DS    CL32                                                             
DUB      DS    D                                                                
         DS    CL1                                                              
P        DC    CL132' '                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NETESTNTWK08/10/00'                                      
         END                                                                    
