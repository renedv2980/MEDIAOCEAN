*          DATA SET PPREP7401  AT LEVEL 008 AS OF 12/10/92                      
*PHASE PP7401A,+0                                                               
         TITLE 'SPECS FOR REPORT PROFILE LISTING'                               
PP7401   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         SPROG 0,1                                                              
         PSPEC H1,2,REQUESTOR                                                   
         PSPEC H1,46,C'REPORT PROFILE LISTING'                                  
         PSPEC H1,83,AGYNAME                                                    
         PSPEC H2,2,REPORT                                                      
         PSPEC H2,46,22C'-'                                                     
         PSPEC H2,83,AGYADD                                                     
         PSPEC H4,2,C'REPORT'                                                   
         PSPEC H4,83,PAGE                                                       
*                                                                               
         SPROG 0                                                                
         PSPEC M1,2,C'FIELD   DESCRIPTION'                                      
         PSPEC M1,42,C'ACCEPTABLE VALUES     DEFAULT  DDS'                      
         PSPEC M2,2,C'NUMBER  -----------'                                      
         PSPEC M2,42,C'-----------------      VALUE   ---'                      
*                                                                               
         SPROG 1                                                                
         PSPEC M1,2,C'USERID     MEDIA CLIENT  ---------------'                 
         PSPEC M1,43,C'EFFECTIVE OVERRIDE FIELD VALUES'                         
         PSPEC M1,75,C'--------------'                                          
         PSPEC M1,91,C'ACTIVITY'                                                
         PSPEC M2,2,C'------     ----- ------  01  02  03'                      
         PSPEC M2,39,C'04  05  06  07  08  09  10  11'                          
         PSPEC M2,71,C'12  13  14  15  16'                                      
         PSPEC M2,91,C'  DATE  '                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008PPREP7401 12/10/92'                                      
         END                                                                    
