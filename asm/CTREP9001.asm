*          DATA SET CTREP9001  AT LEVEL 010 AS OF 09/15/14                      
*PHASE CT9001A                                                                  
         TITLE 'SPECS FOR DEMO FORMULA LISTING'                                 
CT9001   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,47,C'DEMO FORMULA LISTING'                                    
         ASPEC H1,89,REPORT                                                     
         ASPEC H1,103,PAGE                                                      
         SPACE 1                                                                
         ASPEC H2,47,20C'-'                                                     
         SPACE 1                                                                
         ASPEC H3,2,C'FILE'                                                     
         ASPEC H4,2,C'MEDIA'                                                    
         ASPEC H5,2,C'SOURCE'                                                   
         SPACE 1                                                                
         ASPEC H7,2,C'DEMO'                                                     
         ASPEC H7,25,C'FORMULA'                                                 
         SPACE 1                                                                
         ASPEC H8,2,4C'-'                                                       
         ASPEC H8,25,7C'-'                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H7,89,C'LAST ACTIVE'                                             
         ASPEC H8,89,11C'-'                                                     
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H7,89,C'BOOK'                                                    
         ASPEC H8,89,4C'-'                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010CTREP9001 09/15/14'                                      
         END                                                                    
