*          DATA SET CTREP9101  AT LEVEL 023 AS OF 05/04/09                      
*PHASE CT9101A                                                                  
         TITLE 'SPECS FOR DEMO FORMULA LISTING'                                 
CT9101   CSECT                                                                  
         SPROG 0,1,2,3,4,5,6                                                    
         PRINT NOGEN                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,89,REPORT                                                     
         ASPEC H1,103,PAGE                                                      
         SPROG 2                                                                
         ASPEC H1,35,C'STATION CALL LETTER CHANGES'                             
         ASPEC H2,35,C'---------------------------'                             
         ASPEC H3,1,C'SOURCE'                                                   
         ASPEC H4,1,C'MEDIA'                                                    
         ASPEC H7,13,C'START               START'                               
         ASPEC H8,01,C'OLD   NEW   BOOK    OLD   NEW   BOOK'                    
         ASPEC H9,01,C'---   ---   ----    ---   ---   ----'                    
         ASPEC H7,53,C'START               START'                               
         ASPEC H8,41,C'OLD   NEW   BOOK    OLD   NEW   BOOK'                    
         ASPEC H9,41,C'---   ---   ----    ---   ---   ----'                    
         ASPEC H7,93,C'START               START'                               
         ASPEC H8,81,C'OLD   NEW   BOOK    OLD   NEW   BOOK'                    
         ASPEC H9,81,C'---   ---   ----    ---   ---   ----'                    
         SPROG 3                                                                
         ASPEC H1,35,C'DEMOGRAPHIC NAMES'                                       
         ASPEC H2,35,C'-----------------'                                       
         ASPEC H3,1,C'FILE'                                                     
         ASPEC H4,1,C'MEDIA'                                                    
         ASPEC H5,1,C'AGENCY'                                                   
         ASPEC H6,1,C'LOOKUP'                                                   
         ASPEC H8,1,C'  DEMO     4X4 CHR      5X5 CHR        5 CHR'             
         ASPEC H9,1,C'  ----     ---------    -----------    -----'             
         ASPEC H8,44,C'    6 CHR     7 CHR  '                                   
         ASPEC H9,44,C'    ------    -------'                                   
         SPROG 4                                                                
         ASPEC H1,35,C'DEMOGRAPHIC MODIFIER CODES'                              
         ASPEC H2,35,C'--------------------------'                              
         ASPEC H3,1,C'FILE'                                                     
         ASPEC H4,1,C'MEDIA'                                                    
         ASPEC H5,1,C'AGENCY'                                                   
         ASPEC H6,1,C'LOOKUP'                                                   
         ASPEC H8,1,C'  MODIFIER  NAME'                                         
         ASPEC H9,1,C'  --------  ----'                                         
         SPROG 5                                                                
         ASPEC H1,35,C'RATING SERVICE AUTHORIZATIONS/LOOKUP OPTIONS'            
         ASPEC H2,35,C'--------------------------------------------'            
         ASPEC H3,1,C'SOURCE'                                                   
         ASPEC H4,1,C'MEDIA'                                                    
         ASPEC H6,01,C'        BOOK            START'                           
         ASPEC H7,01,C'  AGY   TYPE    CLIENT   BOOK'                           
         ASPEC H8,01,C'  ---   ----    ------  -----'                           
         SPROG 6                                                                
         ASPEC H1,35,C'DEMOGRAPHICS BY SVI TYPE'                                
         ASPEC H2,35,C'------------------------'                                
         ASPEC H3,1,C'SOURCE'                                                   
         ASPEC H4,1,C'MEDIA'                                                    
         ASPEC H5,1,C'AGENCY'                                                   
         ASPEC H6,1,C'CLIENT'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023CTREP9101 05/04/09'                                      
         END                                                                    
