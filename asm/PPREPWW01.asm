*          DATA SET PPREPWW01  AT LEVEL 005 AS OF 07/22/94                      
*PHASE PPWW01A,+0                                                               
         TITLE 'PPWW01 - PUBFILE CREATION FOR WESTERN'                          
PPWW01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PUBFILE                                                   
         FSPEC UPDATE,PUBDIR                                                    
*                                                                               
         PSPEC H1,52,C'PRINTPAK WESTERN PUBFILE CREATION PROGRAM'               
         PSPEC H3,49,PERIOD                                                     
         PSPEC H2,52,C'-----------------------------------------'               
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         PSPEC H7,1,C'MEDIA'                                                    
         PSPEC H8,1,C'-----'                                                    
         PSPEC H7,8,C'PUB #'                                                    
         PSPEC H8,8,C'-----'                                                    
         PSPEC H7,35,C'DDS DATA'                                                
         PSPEC H8,35,C'--------'                                                
         PSPEC H7,82,C'WESTERN DATA'                                            
         PSPEC H8,82,C'------------'                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005PPREPWW01 07/22/94'                                      
         END                                                                    
