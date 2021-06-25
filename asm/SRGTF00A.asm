*          DATA SET SRGTF00A   AT LEVEL 049 AS OF 02/09/98                      
*PHASE T16600A                                                                  
         TITLE 'GETFILE IND$FILE INTERFACE'                                     
         PRINT NOGEN                                                            
GETFILE  CSECT                                                                  
         NMOD1 0,**GTF**,CLEAR=YES,RR=R2                                        
         USING SRPARMD,R1                                                       
*                                                                               
         L     R2,SRQATIOB                                                      
         USING TIOBD,R2                                                         
         OI    TIOBINDS,TIOBSETC                                                
         MVC   TIOBCURS,=X'0051'                                                
         XC    TIOBCURD,TIOBCURD                                                
         XC    TIOBCURI,TIOBCURI                                                
*                                                                               
         L     RA,SRQATWA                                                       
         USING T166FFD,RA                                                       
**       XC    GTF1ST,GTF1ST                                                    
**       OI    GTF1STH+6,X'80'                                                  
         MVC   GTF2ND,=CL8'READY'                                               
         OI    GTF2NDH+6,X'80'                                                  
*                                                                               
         XIT1                                                                   
*********************************************************                       
T166FFD  DSECT                                                                  
         DS    XL64                                                             
**F1STH  DS    XL8                                                              
**F1ST   DS    CL78                                                             
GTF2NDH  DS    XL8                                                              
GTF2ND   DS    CL5                                                              
GTF3RDH  DS    CL8                                                              
GTF3RD   DS    CL247                                                            
GTF4THH  DS    CL8                                                              
GTF4TH   DS    CL78                                                             
       ++INCLUDE FASRPARM                                                       
       ++INCLUDE FATIOB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049SRGTF00A  02/09/98'                                      
         END                                                                    
