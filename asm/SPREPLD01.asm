*          DATA SET SPREPLD01  AT LEVEL 020 AS OF 08/29/00                      
*PHASE SPLD01A                                                                  
         TITLE 'DEMO MENU LISTING REPORT HEADINGS'                              
         PRINT NOGEN                                                            
SPLD01   CSECT                                                                  
         FSPEC USE,SPRQ03                                                       
         SSPEC H1,3,MEDIA                                                       
         SSPEC H1,57,C'DEMO MENU LISTING'                                       
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,57,C'---- ---- -------'                                       
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,100,PAGE                                                      
         SSPEC H3,111,REPORT                                                    
         SSPEC H6,3,C'MENU'                                                     
         SSPEC H6,15,C'DEMO 1/11'                                               
         SSPEC H6,26,C'DEMO 2/12'                                               
         SSPEC H6,37,C'DEMO 3/13'                                               
         SSPEC H6,48,C'DEMO 4/14'                                               
         SSPEC H6,59,C'DEMO 5/15'                                               
         SSPEC H6,70,C'DEMO 6/16'                                               
         SSPEC H6,81,C'DEMO 7/17'                                               
         SSPEC H6,92,C'DEMO 8/18'                                               
         SSPEC H6,103,C'DEMO 9/19'                                              
         SSPEC H6,114,C'DEMO 10/20'                                             
         SSPEC H7,3,C'----'                                                     
         SSPEC H7,15,C'---------'                                               
         SSPEC H7,26,C'---------'                                               
         SSPEC H7,37,C'---------'                                               
         SSPEC H7,48,C'---------'                                               
         SSPEC H7,59,C'---------'                                               
         SSPEC H7,70,C'---------'                                               
         SSPEC H7,81,C'---------'                                               
         SSPEC H7,92,C'---------'                                               
         SSPEC H7,103,C'---------'                                              
         SSPEC H7,114,C'----------'                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPREPLD01 08/29/00'                                      
         END                                                                    
