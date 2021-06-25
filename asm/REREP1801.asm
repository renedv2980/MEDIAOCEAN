*          DATA SET REREP1801  AT LEVEL 002 AS OF 07/12/07                      
*PHASE RE1801A,*                                                                
RE1801   TITLE 'REREP1801 -- ACE/TWX CONTRACT SWITCHER'                         
*                                                                               
*- REREP1801 -- ACE/TWX CONTRACT SWITCHER -- SPECS                              
*                                                                               
*  NO 'FSPECS' CURRENTLY DEFINED                                                
*                                                                               
RE1801   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         FSPEC UPDATE,REPFIL                                                    
         FSPEC UPDATE,REPDIR                                                    
         SPROG 0,1,2,3,4,5,6,7,8,9                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,59,C'CONTRACT SWITCHER'                                       
         ASPEC H1,110,RENUM                                                     
         ASPEC H1,123,PAGE                                                      
*                                                                               
         ASPEC H2,59,C'-----------------'                                       
         ASPEC H2,110,REQUESTOR                                                 
*                                                                               
         ASPEC F1,2,REQDETS        FOOTERS                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REREP1801 07/12/07'                                      
         END                                                                    
