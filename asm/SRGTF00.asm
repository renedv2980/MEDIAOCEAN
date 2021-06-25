*          DATA SET SRGTF00    AT LEVEL 046 AS OF 08/22/00                      
*PHASE T16600A                                                                  
         TITLE 'GETFILE IND$FILE INTERFACE'                                     
         PRINT NOGEN                                                            
GETFILE  CSECT                                                                  
         NMOD1 0,**GTF**,CLEAR=YES,RR=R2                                        
         XIT1                                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046SRGTF00   08/22/00'                                      
         END                                                                    
