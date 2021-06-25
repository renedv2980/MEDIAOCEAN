*          DATA SET PPREPYR01  AT LEVEL 013 AS OF 01/22/01                      
*          DATA SET PPREPYR01  AT LEVEL 011 AS OF 08/20/97                      
*PHASE PPYR01A                                                                  
         TITLE 'PPYR01 - PUBFILE CREATION'                                      
PPYR01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PUBFILE                                                   
         FSPEC UPDATE,PUBDIR                                                    
*                                                                               
         SPROG 0,10,20                                                          
         PSPEC H1,52,C'PRINTPAK STAR COM'                                       
         PSPEC H3,49,PERIOD                                                     
         PSPEC H2,52,C'-----------------'                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         SPROG 10                                                               
         PSPEC H7,1,C'MEDIA'                                                    
         PSPEC H8,1,C'-----'                                                    
         PSPEC H7,9,C'PUB #'                                                    
         PSPEC H8,7,C'--------'                                                 
         PSPEC H7,30,C'PUB NAME'                                                
         PSPEC H8,22,C'--------------------'                                    
         PSPEC H7,50,C'ADDRESS1'                                                
         PSPEC H8,44,C'------------------------------'                          
         PSPEC H7,80,C'ADDRESS2'                                                
         PSPEC H8,76,C'------------------------------'                          
         PSPEC H7,112,C'CITY'                                                   
         PSPEC H8,108,C'----------------'                                       
*                                                                               
         SPROG 20                                                               
         PSPEC H7,1,C'REPS TO BE ADDED'                                         
         PSPEC H8,1,C'----------------'                                         
         PSPEC H7,20,C'CODE'                                                    
         PSPEC H8,20,C'----'                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREPYR01 01/22/01'                                      
         END                                                                    
