*          DATA SET SPREP7401  AT LEVEL 021 AS OF 09/22/92                      
*PHASE SP7401A,+0                                                               
         TITLE 'SPECS FOR REPORT PROFILE LISTING'                               
SP7401   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP7403                                                       
*                                                                               
         SPROG 0,1                                                              
         SSPEC H1,2,REQUESTOR                                                   
         SSPEC H1,46,C'REPORT PROFILE LISTING'                                  
         SSPEC H1,83,AGYNAME                                                    
         SSPEC H2,2,REPORT                                                      
         SSPEC H2,46,22C'-'                                                     
         SSPEC H2,83,AGYADD                                                     
         SSPEC H4,2,C'REPORT'                                                   
         SSPEC H4,83,PAGE                                                       
*                                                                               
         SPROG 0                                                                
         SSPEC M1,2,C'FIELD   DESCRIPTION'                                      
         SSPEC M1,42,C'ACCEPTABLE VALUES     DEFAULT  DDS'                      
         SSPEC M2,2,C'NUMBER  -----------'                                      
         SSPEC M2,42,C'-----------------      VALUE   ---'                      
*                                                                               
         SPROG 1                                                                
         SSPEC M1,2,C'USERID     MEDIA CLIENT  ---------------'                 
         SSPEC M1,43,C'EFFECTIVE OVERRIDE FIELD VALUES'                         
         SSPEC M1,75,C'--------------'                                          
         SSPEC M1,91,C'ACTIVITY'                                                
         SSPEC M2,2,C'------     ----- ------  01  02  03'                      
         SSPEC M2,39,C'04  05  06  07  08  09  10  11'                          
         SSPEC M2,71,C'12  13  14  15  16'                                      
         SSPEC M2,91,C'  DATE  '                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPREP7401 09/22/92'                                      
         END                                                                    
