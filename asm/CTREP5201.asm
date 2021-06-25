*          DATA SET CTREP5201  AT LEVEL 007 AS OF 08/22/00                      
*PHASE CT5201A                                                                  
         TITLE 'SPECS FOR PROFILE REPORT'                                       
CT5201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,PROFILE                                                     
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,48,C'EOD PROFILE REPORT'                                      
         ASPEC H2,48,C'------------------'                                      
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,C'SYSTEM'                                                   
         ASPEC H5,2,C'PROGRAM'                                                  
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H10,2,C'FIELD'                                                   
         ASPEC H11,2,C'-----'                                                   
         ASPEC H10,26,C'ID OVERRIDE   PROFILE TYPE    PROFILE VALUE'            
         ASPEC H11,26,C'OR DEFAULT    ------------    -------------'            
         SPROG 2,3,4,5                                                          
         ASPEC H1,2,RUN                                                         
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         SPROG 2                                                                
         ASPEC H1,40,C'CROSS-REFERENCE OF ATTENTIONS'                           
         ASPEC H2,40,29C'-'                                                     
         ASPEC H6,2,C'ATTENTION'                                                
         ASPEC H7,2,C'TYPE '                                                    
         SPROG 3                                                                
         ASPEC H1,40,C'CROSS-REFERENCE OF OUTPUT TYPES'                         
         ASPEC H2,40,31C'-'                                                     
         ASPEC H6,2,C'OUTPUT'                                                   
         ASPEC H7,2,C' TYPE '                                                   
         SPROG 4                                                                
         ASPEC H1,40,C'CROSS-REFERENCE OF PRIORITIES'                           
         ASPEC H2,40,29C'-'                                                     
         ASPEC H6,2,C'PRIORITY'                                                 
         ASPEC H7,2,C'CODE    '                                                 
         SPROG 2,3,4,5                                                          
         ASPEC H6,12,C'LIST OF REFERENCING PROGRAMS(-IDS)'                      
         ASPEC H7,12,34C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTREP5201 08/22/00'                                      
         END                                                                    
