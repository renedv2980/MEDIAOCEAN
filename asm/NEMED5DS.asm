*          DATA SET NEMED5DS   AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E5DA                                                                  
         TITLE 'T31E5D - HEADLINES FOR PFM'                                     
T31E5D   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'PHYSICAL FILE LISTING'                                    
         SSPEC H2,1,21C'-'                                                      
         SSPEC H1,55,NETREP                                                     
         SSPEC H2,55,PAGE                                                       
         SSPEC H4,1,C'FILE'                                                     
         SSPEC H5,1,C'KEY'                                                      
         SSPEC H6,1,C'FILTER'                                                   
         SSPEC H4,55,C'DATA START'                                              
         SSPEC H5,55,C'DATA END'                                                
         SSPEC H6,55,C'HOW MANY'                                                
         SSPEC H9,1,C'POSN.   ---------HEXADECIMAL REPRESENTATION----'          
         SSPEC H9,48,C'-----'                                                   
         SSPEC H9,55,C'EBCDIC REPRESENTATION'                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED5DS  08/10/00'                                      
         END                                                                    
