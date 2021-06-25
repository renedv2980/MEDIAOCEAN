*          DATA SET REREP7501  AT LEVEL 009 AS OF 07/23/96                      
*          DATA SET REREP7201  AT LEVEL 009 AS OF 11/08/95                      
*PHASE RE7501A,+0                                                               
         TITLE 'SPECS FOR STANDARD COMMENT LISTING'                             
RE7501   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,47,C'COMMENT RECORD LISTING'                                  
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H2,2,REP1                                                        
         ASPEC H2,47,22C'-'                                                     
         ASPEC H2,88,REQUESTOR                                                  
         ASPEC H3,1,SPACES                                                      
         ASPEC H4,05,C'KEY '                                                    
         ASPEC H4,12,C'LINE#'                                                   
         ASPEC H4,21,C'COMMENT DESCRIPTION'                                     
         ASPEC H5,05,C'--- '                                                    
         ASPEC H5,12,C'-----'                                                   
         ASPEC H5,21,C'-------------------'                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009REREP7501 07/23/96'                                      
         END                                                                    
