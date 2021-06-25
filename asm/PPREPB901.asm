*          DATA SET PPREPB901  AT LEVEL 009 AS OF 08/09/00                      
*PHASE PPB901A                                                                  
         TITLE 'PPREPB901 - PRINT/ RETAIL BILLING REPORT - SPECS'               
         PRINT NOGEN                                                            
PPB901   CSECT                                                                  
         SPACE 2                                                                
*                                                                               
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUB                                                          
         FSPEC GET,REGIONS                                                      
         FSPEC GET,DISTRICTS                                                    
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
*                                                                               
         SPROG 0,10,20,30                                                       
         PSPEC H1,1,MEDIA                                                       
         PSPEC H2,1,CLIENT                                                      
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H3,122,PAGE                                                      
*                                                                               
*                                                                               
         PSPEC H1,48,C'MEDIA/RETAIL BILLING REFERENCE LIST'                     
         PSPEC H2,48,C'-----------------------------------'                     
*                                                                               
         SPROG 10                                                               
         PSPEC H4,51,C'** SCHEME PERCENTAGE TABLE **'                           
*                                                                               
         SPROG 20                                                               
         PSPEC H4,53,C'** MEDIA ESTIMATE LIST **'                               
*                                                                               
         SPROG 30                                                               
         PSPEC H4,53,C' ** SCHEME USAGE LIST ** '                               
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PPREPB901 08/09/00'                                      
         END                                                                    
