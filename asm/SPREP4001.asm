*          DATA SET SPREP4001  AT LEVEL 008 AS OF 08/16/83                      
*PHASE SP4001T,*,NOAUTO                                                         
         TITLE 'SP4001 - ACTIVE MARKET LISTING - PRINT SPECS'                   
SP4001   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP4003                                                       
         SPROG 1,2                                                              
         SSPEC H1,3,MEDIA                                                       
         SSPEC H1,55,C'ACTIVE MARKET LISTING'                                   
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,55,C'------ ------ -------'                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,3,CLIENT                                                      
         SSPEC H4,100,REPORT                                                    
         SSPEC H5,100,PAGE                                                      
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H4,55,C'LIST BY MARKET NUMBER'                                   
         SSPEC M1,56,C'B MEANS BUYS ONLY'                                       
         SSPEC M2,56,C'G MEANS GOALS ONLY'                                      
         SPACE 1                                                                
         SPROG 2                                                                
         SSPEC H4,55,C'LIST BY MARKET GROUP'                                    
         SSPEC M1,3,C'MARKET GROUP'                                             
         SSPEC M1,20,C'MARKETS'                                                 
         SSPEC M2,3,C'------ -----'                                             
         SSPEC M2,20,C'-------'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPREP4001 08/16/83'                                      
         END                                                                    
