*          DATA SET SPREP4701  AT LEVEL 005 AS OF 08/29/00                      
*PHASE SP4701A                                                                  
         TITLE 'SP4701 - SPOTPAK PRDGRP/MKTGRP LIST - PRINT SPECS'              
SP4701   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP4703                                                       
*                                                                               
         SPROG 1,2,3                                                            
*                                                                               
         SSPEC H1,2,MEDIA                                                       
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
*                                                                               
         SSPEC H2,2,REQUESTOR                                                   
*                                                                               
         SSPEC H2,100,AGYADD                                                    
*                                                                               
         SSPEC H4,2,CLIENT                                                      
*                                                                               
         SSPEC H4,100,PAGE                                                      
*                                                                               
         SSPEC H4,111,REPORT                                                    
*                                                                               
         SPROG 1                                                                
*                                                                               
         SSPEC H1,56,C'PRODUCT GROUP LISTING'                                   
         SSPEC H2,56,C'------- ----- -------'                                   
*                                                                               
         SPROG 2                                                                
*                                                                               
         SSPEC H4,50,PRDGRP                                                     
*                                                                               
        SSPEC H1,57,C'MARKET GROUP LISTING'                                     
         SSPEC H2,57,C'------ ----- -------'                                    
         SPROG 3                                                                
         SSPEC H1,52,C'MARKET GROUP - MARKET ACTIVITY'                          
         SSPEC H2,52,C'------------------------------'                          
 END                                                                            
