*          DATA SET TACKLIN    AT LEVEL 005 AS OF 06/02/10                      
*PHASE TACKLINC                                                                 
*                                                                               
         TITLE 'TACKLIN - LINTAB FOR TALENT CHECK PROGRAM'                      
LINTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF LIEN ELEMENTS                                                 
*                                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(LINLEN)         RECORD LENGTH                                
         DC    AL4(LINLKEY)        DISP. TO KEY / KEY LENGTH                    
         DC    AL4(MAXLINS)        MAX. NUMBER OF RECORDS                       
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    X'80'               BINARY                                       
         DC    AL1(0)                                                           
         DC    (MAXLINS*LINLEN)X'00'  THE TABLE                                 
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005TACKLIN   06/02/10'                                      
         END                                                                    
