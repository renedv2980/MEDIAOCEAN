*          DATA SET APGHFIMIA  AT LEVEL 037 AS OF 05/04/94                      
*PHASE ACHFIMIA,+0                                                              
         TITLE 'BBDO CANADA BALANCE SHEET'                                      
ACHFIMIA CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R5                                                        
                                                                                
*---------------------------------------------------------------------*         
*   THESE RULES ONLY APPLY TO CASH ACCOUNT - IF CASH ACCOUNT IS       *         
*   NEGATIVE (FA2BABB) SHOW AMOUNT AS A POSITIVE IN THE LIABILITIES   *         
*   SECTION UNDER BANK LOANS OVERDRAFTS (FA2HACC)                     *         
*                                                                     *         
*---------------------------------------------------------------------*         
         CLI   HOOKNUM,1                                                        
         BNE   XIT                                                              
                                                                                
         CLI   QOPT5,C'D'                                                       
         BNE   HK010                                                            
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECORD'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK010    CLC   R1CDE2(2),=C'BA'                                                 
         BNE   HK011                                                            
         CLC   R1CDE3(2),=C'BB'                                                 
         BNE   XIT                                                              
         B     HK025                                                            
HK011    CLC   R1CDE2(2),=C'FA'                                                 
         BNE   HK012                                                            
         CLC   R1CDE3(3),=C'BB0'                                                
         BNE   XIT                                                              
         B     HK025                                                            
HK012    DS    0H                                                               
         CLC   R1CDE2(2),=C'LA'                                                 
         BNE   HK013                                                            
         CLC   R1CDE3(3),=C'DH0'                                                
         BNE   XIT                                                              
         B     HK025                                                            
HK013    DS    0H                                                               
         CLC   FROMACC+1(8),=C'FA2LADH0'                                        
         BNE   HK050                                                            
                                                                                
HK025    ZAP   COLWORK,R1COL1                                                   
         AP    COLWORK,R1COL2                                                   
         AP    COLWORK,R1COL3                                                   
         CP    COLWORK,=P'0'                                                    
         BH    HK030                                                            
         ZAP   R1COL1,=P'0'                                                     
         ZAP   R1COL2,=P'0'                                                     
         ZAP   R1COL3,=P'0'                                                     
                                                                                
HK030    CP    R1COL4,=P'0'                                                     
         BH    HK990                                                            
         ZAP   R1COL4,=P'0'                                                     
                                                                                
HK050    CLC   R1CDE2(2),=C'HA'                                                 
         BNE   HK051                                                            
         CLC   R1CDE3(2),=C'CC'                                                 
         BNE   XIT                                                              
         B     HK100                                                            
HK051    CLC   R1CDE2(2),=C'JA'                                                 
         BNE   HK052                                                            
         CLC   R1CDE3(3),=C'BB0'                                                
         BNE   XIT                                                              
         B     HK100                                                            
HK052    DS    0H                                                               
         CLC   FROMACC+1(7),=C'FA2HACC'                                         
         BE    HK100                                                            
         B     XIT                                                              
                                                                                
HK100    ZAP   COLWORK,R1COL1                                                   
         AP    COLWORK,R1COL2                                                   
         AP    COLWORK,R1COL3                                                   
         CP    COLWORK,=P'0'                                                    
         BL    HK600                                                            
         ZAP   R1COL1,=P'0'                                                     
         ZAP   R1COL2,=P'0'                                                     
         ZAP   R1COL3,=P'0'                                                     
         B     HK700                                                            
HK600    MP    R1COL1,=P'-1'                                                    
         MP    R1COL2,=P'-1'                                                    
         MP    R1COL3,=P'-1'                                                    
                                                                                
HK700    CP    R1COL4,=P'0'                                                     
         BL    HK800                                                            
         ZAP   R1COL4,=P'0'                                                     
         B     HK990                                                            
HK800    MP    R1COL4,=P'-1'                                                    
                                                                                
HK990    CLI   QOPT5,C'D'                                                       
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECORD'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
COLWORK  DC    PL10'0'                                                          
                                                                                
         LTORG                                                                  
R1RECD   DSECT                                                                  
R1ROW1   DS    XL2                                                              
R1CDE1   DS    XL14                                                             
R1ROW2   DS    XL2                                                              
R1CDE2   DS    XL14                                                             
R1ROW3   DS    XL2                                                              
R1CDE3   DS    XL14                                                             
R1REPNO  DS    XL1                                                              
R1REPCP  DS    XL1                                                              
R1TYPE   DS    XL2                                                              
*                                                                               
R1NME1   DS    CL36                                                             
R1NME2   DS    CL36                                                             
R1NME3   DS    CL36                                                             
*                                                                               
R1COL1   DS    PL8                                                              
R1COL2   DS    PL8                                                              
R1COL3   DS    PL8                                                              
R1COL4   DS    PL8                                                              
R1LEN    EQU   *-R1RECD                                                         
         EJECT                                                                  
*        ACREPWORKD                                                             
*        ACAPGEQU                                                               
*        ACAPGWORKD                                                             
*        ACGENBOTH                                                              
*        DDDLCB                                                                 
*        DDBUFFALOD                                                             
*        ACBIGPRNTD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACAPGEQU                                                       
       ++INCLUDE ACAPGWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037APGHFIMIA 05/04/94'                                      
         END                                                                    
