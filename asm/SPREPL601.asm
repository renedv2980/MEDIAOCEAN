*          DATA SET SPREPL601  AT LEVEL 004 AS OF 08/29/00                      
*PHASE SPL601A                                                                  
         TITLE 'SPL601 - SPOTPAK COMMENT LISTING - PRINT SPECS'                 
SPL601   CSECT                                                                  
         FSPEC USE,SPL603                                                       
*                                                                               
         SPROG 1,2,3,4                                                          
*                                                                               
         SSPEC H1,2,MEDIA                                                       
*                                                                               
         SSPEC H1,51,C'COMMENT LISTING'                                         
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
*                                                                               
         SSPEC H2,2,REQUESTOR                                                   
*                                                                               
         SSPEC H2,51,C'------- -------'                                         
*                                                                               
         SSPEC H2,100,AGYADD                                                    
*                                                                               
         SSPEC H4,100,PAGE                                                      
*                                                                               
         SSPEC H4,111,REPORT                                                    
*                                                                               
         SSPEC H8,4,C'LAST UPDATE'                                              
*                                                                               
         SSPEC H8,30,C'OPTIONS'                                                 
*                                                                               
         SSPEC H8,55,C'TEXT OF COMMENT'                                         
*                                                                               
         SSPEC H9,4,C'-----------'                                              
*                                                                               
         SSPEC H9,24,C'---------------------'                                   
*                                                                               
         SSPEC H9,51,C'-----------------------------------'                     
*                                                                               
         SSPEC H9,86,C'-----------------------------------'                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPL601 08/29/00'                                      
         END                                                                    
