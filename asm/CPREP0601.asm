*          DATA SET CPREP0601  AT LEVEL 003 AS OF 09/01/00                      
*PHASE CP0601A                                                                  
         TITLE 'SPECS FOR CPP COMPOSITION REPORT'                               
CP0601   CSECT                                                                  
         PRINT NOGEN                                                            
         PSPEC H1,1,MEDIA                                                       
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H1,41,C'SUPERTARGET COMPOSITON REPORT'                           
         PSPEC H2,41,C'-----------------------------'                           
         PSPEC H1,77,AGYNAME                                                    
         PSPEC H2,77,AGYADD                                                     
         PSPEC H4,1,RANGE                                                       
         PSPEC H4,45,PERIOD                                                     
         PSPEC H4,77,REPORT                                                     
         PSPEC H4,103,PAGE                                                      
         PSPEC H7,27,C'PCT'                                                     
         PSPEC H7,43,C'PCT'                                                     
         PSPEC H7,59,C'PCT'                                                     
         PSPEC H7,75,C'PCT'                                                     
         PSPEC H8,1,C'SUPER    ACTUAL     SPOTS SPT     DOLLARS DOL'            
         PSPEC H9,1,C'-----    ------     ----- ---     ------- ---'            
         PSPEC H8,51,C'RATINGS RTG IMPRESSIONS IMP'                             
         PSPEC H9,51,C'------- --- ----------- ---'                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CPREP0601 09/01/00'                                      
         END                                                                    
