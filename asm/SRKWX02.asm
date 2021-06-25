*          DATA SET SRKWX02    AT LEVEL 002 AS OF 08/22/00                      
*PHASE T14702A                                                                  
         TITLE '$KWX - ERROR HANDLING OVERLAY'                                  
T14702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 5,**T147**                                                       
         USING WORKD,RC                                                         
         MVC   WORKD(8),0(R1)                                                   
         L     RA,0(R1)                                                         
         USING T147FFD,RA                                                       
         ZIC   R4,FERN                                                          
         BCTR  R4,0                                                             
         MH    R4,=H'30'                                                        
         LA    R4,ERRLST(R4)                                                    
         MVC   KWXHEAD+14(30),0(R4)                                             
         CLI   FNDX,0                                                           
         BE    EXIT                                                             
         LA    R4,KWXHEAD+59                                                    
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   2(6,R4),=C'- FLD#'                                               
         EDIT  FNDX,(2,8(R4)),ALIGN=LEFT                                        
EXIT     XMOD1 1                                                                
         SPACE 1                                                                
ERRLST   DS    0CL30                                                            
         DC    CL30'MISSING INPUT FIELD'                                        
         DC    CL30'INVALID INPUT FIELD'                                        
         DC    CL30'NO SUCH USER OR LIST EXISTS'                                
         DC    CL30'LIST NESTED TOO DEEP'                                       
         DC    CL30'TOO MANY INPUT FIELDS'                                      
         DC    CL30'NO ADDRESSEES'                                              
         DC    CL30'SUBSTITUTION INVALID'                                       
         DC    CL30'NO SUBSTITUTION HITS'                                       
         DC    CL30'NOT ENOUGH ROOM FOR TEXT'                                   
         DC    CL30'REPORT DOES NOT EXIST'                                      
         DC    CL30'TEXT DOES NOT EXIST'                                        
         DC    CL30'KEYWORD INVALID'                                            
         DC    CL30'MESSAGE SIZE EXCEEDS MAXIMUM'                               
         DC    CL30'NOT LOGGED ON'                                              
         DC    CL30'SEPARATE ADDRESSEES WITH +/-'                               
         DC    CL30'NOT THE SENDER''S COPY OF A KWX'                            
         DC    CL30'NO ''REPORT='' ON PRECEDING LINE'                           
         DC    CL30'SINGLE PAGE MUST BE SPECIFIED'                              
         DC    CL30'NOT NUMERIC (N OR N1-N2)'                                   
         DC    CL30'START GREATER THAN END'                                     
         DC    CL30'NUMBER EXCEEDS MAXIMUM'                                     
         DC    CL30'MORE THAN 100 ADDRESSEES'                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
FERN     DS    0CL1                                                             
ATWA     DS    A                                                                
FNDX     DS    0CL1                                                             
         DS    A                                                                
DUB      DS    D                                                                
WORK     DS    CL18                                                             
         SPACE 1                                                                
* SRKWXFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SRKWXFFD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRKWX02   08/22/00'                                      
         END                                                                    
