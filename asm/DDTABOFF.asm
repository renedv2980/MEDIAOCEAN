*          DATA SET DDTABOFF   AT LEVEL 006 AS OF 11/07/16                      
*PHASE T00A3BA                                                                  
*INCLUDE FATABOFF                                                               
         TITLE   'DDTABOFF - OFFLINE PHASE FOR FATABOFF'                        
         SPACE 1                                                                
DDTABOFF CSECT                                                                  
         SPACE 1                                                                
* ADDRESS LIST OF FATABOFF ENRTY POINTS                                         
         ORG   DDTABOFF+(TBACTRYT-TABOFFD)                                      
         DC    V(CTRYTAB)                                                       
         ORG   DDTABOFF+(TBALANGT-TABOFFD)                                      
         DC    V(LANGTAB)                                                       
         ORG   DDTABOFF+(TBASELIS-TABOFFD)                                      
         DC    V(SELIST)                                                        
         SPACE 1                                                                
       ++INCLUDE DDTABOFFD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DDTABOFF  11/07/16'                                      
         END                                                                    
