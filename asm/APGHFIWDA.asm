*          DATA SET APGHFIWDA  AT LEVEL 016 AS OF 06/16/95                      
*PHASE ACHFIWDA,+0                                                              
         TITLE 'BATES CLIENT WARNER LAMBERT'                                    
ACHFIWDA CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R5                                                        
                                                                                
*---------------------------------------------------------------------*         
*                                                                     *         
*---------------------------------------------------------------------*         
HOOKSORT DS    0H                                                               
         CLI   HOOKNUM,1                                                        
         BNE   XIT                                                              
                                                                                
         CLI   QOPT5,C'D'                                                       
         BNE   HK010                                                            
HK000B   L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK010    L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         SP    R1COL7,R1COL6                                                    
                                                                                
HK099    DS    0H                                                               
         CLI   QOPT5,C'D'                                                       
         BNE   XIT                                                              
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        CONSTANTS AND LITERAL POOL                                             
*----------------------------------------------------------------*              
DUBB1    DC    D'0'                                                             
DUBB2    DC    D'0'                                                             
DUBB3    DC    D'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
*        WORKING STORAGE                                                        
*----------------------------------------------------------------*              
R1RECD   DSECT                                                                  
R1ROW1   DS    XL2                                                              
R1CDE1   DS    XL14                                                             
R1ROW2   DS    XL2                                                              
R1CDE2   DS    XL14                                                             
R1ROW3   DS    XL2                                                              
R1CDE3   DS    XL14                                                             
R1ROW4   DS    XL2                                                              
R1CDE4   DS    XL14                                                             
R1REPNO  DS    XL1                                                              
R1REPCP  DS    XL1                                                              
R1TYPE   DS    XL2                                                              
*                                                                               
R1NME1   DS    CL36                                                             
R1NME2   DS    CL36                                                             
R1NME3   DS    CL36                                                             
R1NME4   DS    CL36                                                             
*                                                                               
R1COL1   DS    PL8                                                              
R1COL2   DS    PL8                                                              
R1COL3   DS    PL8                                                              
R1COL4   DS    PL8                                                              
R1COL5   DS    PL8                                                              
R1COL6   DS    PL8                                                              
R1COL7   DS    PL8                                                              
R1COL8   DS    PL8                                                              
R1LEN    EQU   *-R1RECD                                                         
                                                                                
         EJECT                                                                  
                                                                                
*        ACAPGGEND                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT ON                                                               
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016APGHFIWDA 06/16/95'                                      
         END                                                                    
