*          DATA SET APGHFILM0  AT LEVEL 006 AS OF 11/16/95                      
*PHASE ACHFILM0,+0                                                              
         TITLE 'PROFIT AND LOSS MICRO CONTROL'                                  
ACHFILM0 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R5                                                        
*                                                                               
HK01     CLI   QOPT5,C'D'                                                       
         BNE   HK10                                                             
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECORD'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
*                                                                               
HK10     MVC   R1CDE5(6),=C'105000'     DEFAULT                                 
         CLC   ACTACC+1(2),=C'1C'                                               
         BE    XIT                                                              
         MVC   R1CDE5+4(2),ACTACC+4     FROM 2D                                 
         B     XIT                                                              
         EJECT                                                                  
XIT      SR    RC,RC                                                            
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
R1RECD   DSECT                                                                  
R1ROW1   DS    XL2                                                              
R1CDE1   DS    XL14                                                             
R1ROW2   DS    XL2                                                              
R1CDE2   DS    XL14                                                             
R1ROW3   DS    XL2                                                              
R1CDE3   DS    XL14                                                             
R1ROW4   DS    XL2                                                              
R1CDE4   DS    XL14                                                             
R1ROW5   DS    XL2                                                              
R1CDE5   DS    XL14                                                             
R1REPNO  DS    XL1                                                              
R1REPCP  DS    XL1                                                              
         DS    XL2                                                              
*                                                                               
R1NME1   DS    CL36                                                             
R1NME2   DS    CL36                                                             
R1NME3   DS    CL36                                                             
R1NME4   DS    CL36                                                             
R1NME5   DS    CL36                                                             
R1NME6   DS    CL36                                                             
*                                                                               
R1COL1   DS    PL8                                                              
R1COL2   DS    PL8                                                              
R1COL3   DS    PL8                                                              
R1COL4   DS    PL8                                                              
R1COL5   DS    PL8                                                              
R1COL6   DS    PL8                                                              
R1COL7   DS    PL8                                                              
R1COL8   DS    PL8                                                              
R1COL9   DS    PL8                                                              
R1COL10  DS    PL8                                                              
R1COL11  DS    PL8                                                              
R1COL12  DS    PL8                                                              
R1LEN    EQU   *-R1RECD                                                         
         EJECT                                                                  
*        ACREPWORKD                                                             
*        ACAPGMAND                                                              
*        ACAPGDSECT                                                             
*        ACGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACAPGMAND                                                      
       ++INCLUDE ACAPGDSECT                                                     
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006APGHFILM0 11/16/95'                                      
         END                                                                    
