*          DATA SET APGHFIDWB  AT LEVEL 063 AS OF 08/13/98                      
*PHASE ACHFIDWB,+0                                                              
         TITLE 'SAATCHI - CORRECT PROFIT PERCENTAGE'                            
ACHFIDWB CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
*---------------------------------------------------------------------*         
*        ACCOUNT F12H IS A PERCENT ACCOUNT AS DEFINED IN THE SUPER    *         
*        LEDGER - IN ORDER TO TAKE 90% OF ONE OF IT'S COMPONENTS      *         
*        (F12HB) IT MUST BE DONE HERE IN HOOK                         *         
*                                                                     *         
*        HOOK MODE SORTHOOK=2                                         *         
*---------------------------------------------------------------------*         
         USING HOOK2D,R5                                                        
HK200    CLI   HOOKNUM,2                                                        
         BNE   XIT                                                              
                                                                                
         CLI   QOPT6,C'2'                DUMP BEFORE RECORD                     
         BNE   HK205                                                            
         LA    R0,H2LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK205    DS    0H                                                               
         CLC   CURRACC+1(6),=C'F12H1B'                                          
         BNE   HK230                                                            
         LA    R0,6                                                             
         LA    R2,H2COL1                                                        
HK210    ZAP   WORK(10),0(L'H2COL1,R2)                                          
         MP    WORK(10),=P'90'                                                  
         SRP   WORK(10),62,5                                                    
         ZAP   0(L'H2COL1,R2),WORK+2(8)                                         
         LA    R2,L'H2COL1(R2)                                                  
         BCT   R0,HK210                                                         
                                                                                
HK230    CLI   QOPT7,C'2'                DUMP AFTER RECORD                      
         BNE   XIT                                                              
         LA    R0,H2LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
HOOKSW   DC    C'N'                                                             
                                                                                
*----------------------------------------------------------------*              
*        LITERAL POOL                                                           
*----------------------------------------------------------------*              
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
*        RECORD DSECT FOR SORTHOOK                                              
*----------------------------------------------------------------*              
HOOK2D   DSECT                                                                  
H2ROW1   DS    XL2                                                              
H2CODE1  DS    XL14                                                             
H2ROW2   DS    XL2                                                              
H2CODE2  DS    XL14                                                             
H2ROW3   DS    XL2                                                              
H2CODE3  DS    XL14                                                             
H2ROW4   DS    XL2                                                              
H2CODE4  DS    XL14                                                             
H2ROW5   DS    XL2                                                              
H2CODE5  DS    XL14                                                             
H2ROW6   DS    XL2                                                              
H2CODE6  DS    XL14                                                             
H2REPNO  DS    XL1                                                              
H2REPCP  DS    XL1                                                              
H2TYPE   DS    XL2                                                              
*                                                                               
H2NAME1  DS    CL36                                                             
H2NAME2  DS    CL36                                                             
H2NAME3  DS    CL36                                                             
H2NAME4  DS    CL36                                                             
H2NAME5  DS    CL36                                                             
H2NAME6  DS    CL36                                                             
*                                                                               
H2COL1   DS    PL8                                                              
H2COL2   DS    PL8                                                              
H2COL3   DS    PL8                                                              
H2COL4   DS    PL8                                                              
H2COL5   DS    PL8                                                              
H2COL6   DS    PL8                                                              
H2LEN    EQU   *-HOOK2D                                                         
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
**PAN#1  DC    CL21'063APGHFIDWB 08/13/98'                                      
         END                                                                    
