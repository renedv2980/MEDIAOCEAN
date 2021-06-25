*          DATA SET TACKPYT    AT LEVEL 002 AS OF 12/09/13                      
*PHASE TACKPYTA                                                                 
*                                                                               
         TITLE 'TACKPYT - PTYDTAB FOR TALENT CHECK PROGRAM'                     
PYTDTAB  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF PERFORMER YTD AMOUNTS                                         
*                                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(PYTDLEN)        RECORD LENGTH                                
         DC    AL4(PYTDLKEY)       DISP. TO KEY / KEY LENGTH                    
         DC    AL4(MAXPYTDS)       MAX. NUMBER OF RECORDS                       
         DC    AL1((PYTDLEN-PYTDDSP)/4) NUMBER OF BUCKETS                       
         DC    AL1(PYTDDSP)        DISP TO BUCKETS                              
         DC    X'80'               BINARY                                       
         DC    AL1(0)                                                           
         DC    (MAXPYTDS*PYTDLEN)X'00'  THE TABLE                               
*                                                                               
*        TABLE OF PERFORMER YTD AMOUNTS FOR AN INVOICE                          
*                                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(PYTDLEN)        RECORD LENGTH                                
         DC    AL4(PYTDLKEY)       DISP. TO KEY / KEY LENGTH                    
         DC    AL4(MAXCAST)        MAX. NUMBER OF RECORDS                       
         DC    AL1((PYTDLEN-PYTDDSP)/4) NUMBER OF BUCKETS                       
         DC    AL1(PYTDDSP)        DISP TO BUCKETS                              
         DC    X'80'               BINARY                                       
         DC    AL1(0)                                                           
         DC    (MAXCAST*PYTDLEN)X'00'  THE TABLE                                
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
*DDDLCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TACKPYT   12/09/13'                                      
         END                                                                    
