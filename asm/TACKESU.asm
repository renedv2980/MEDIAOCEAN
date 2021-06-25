*          DATA SET TACKESU    AT LEVEL 010 AS OF 12/09/13                      
*PHASE TACKESUA                                                                 
*                                                                               
         TITLE 'TACKESU - ESUTAB FOR TALENT CHECK PROGRAM'                      
ESUTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF EMP/SSN/UNIT CHECK AMOUNTS                                    
*                                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(ESULEN)         RECORD LENGTH                                
         DC    AL4(ESULKEY)        DISP. TO KEY / KEY LENGTH                    
         DC    AL4(MAXESUS)        MAX. NUMBER OF RECORDS                       
         DC    AL1((ESULEN-ESUDSP)/4)   NUMBER OF BUCKETS                       
         DC    AL1(ESUDSP)         DISP TO BUCKETS                              
         DC    X'80'               BINARY                                       
         DC    AL1(0)                                                           
         DC    (MAXESUS*ESULEN)X'00'  THE TABLE                                 
*                                                                               
*        TABLE OF EMP/SSN/UNIT CHECK AMOUNTS FOR AN INVOICE                     
*                                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(ESULEN)         RECORD LENGTH                                
         DC    AL4(ESULKEY)        DISP. TO KEY / KEY LENGTH                    
         DC    AL4(MAXCAST*5)      MAX. NUMBER OF RECORDS                       
         DC    AL1((ESULEN-ESUDSP)/4)   NUMBER OF BUCKETS                       
         DC    AL1(ESUDSP)         DISP TO BUCKETS                              
         DC    X'80'               BINARY                                       
         DC    AL1(0)                                                           
         DC    (MAXCAST*5*ESULEN)X'00'  THE TABLE                               
*                                                                               
*TACKREPD                                                                       
*DDDLCB                                                                         
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TACKESU   12/09/13'                                      
         END                                                                    
