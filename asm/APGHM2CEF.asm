*          DATA SET APGHM2CEF  AT LEVEL 010 AS OF 11/12/93                      
*PHASE ACHM2CEF,+0                                                              
         TITLE 'DEBUG  (HOOK)'                                                  
ACHM2CEF CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R5                                                        
*                                                                               
         LA    R2,=C'IN REP 11'                                                 
         LA    R3,R1CDE5                                                        
         CLI   HOOKNUM,1                                                        
         BE    HK06                DON'T NEED IT                                
         LA    R2,=C'OUT REP 11'                                                
         CLI   HOOKNUM,2                                                        
         BE    HK06                DON'T NEED IT                                
         LA    R2,=C'IN REP 12'                                                 
         LA    R3,R1CDE4                                                        
         CLI   HOOKNUM,3                                                        
         BE    HK06                DON'T NEED IT                                
         LA    R2,=C'OUT REP 12'                                                
         CLI   HOOKNUM,4                                                        
         BNE   XIT                 DON'T NEED IT                                
HK06     CLI   QOPT5,C'D'                                                       
         BNE   XIT                                                              
         CLC   0(3,R3),=C'018'                                                  
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(10,(R2)),(R5),C'DUMP',(R0),=C'2D',         X        
               (C'P',PRINT)                                                     
         CLI   QOPT4,C'X'                                                       
         BNE   XIT                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 3                                                                
MYEND    DC    XL6'00'                                                          
MYWORK   DS    XL(L'WORK)                                                       
CURRATE  DS    PL8                                                              
CURHRS   DS    PL8                                                              
PACK16   DS    PL16                                                             
         ORG   PACK16                                                           
ANSWER   DS    PL8                                                              
LEFTOVER DS    PL8                                                              
FRSTPASS DC    XL1'00'                                                          
RECAP#   DS    XL1                 OVER WRITE WITH A SECOND RECAP               
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
R1TYPE   DS    XL2                                                              
*                                                                               
R1NME1   DS    CL36                                                             
R1NME2   DS    CL36                                                             
R1NME3   DS    CL36                                                             
R1NME4   DS    CL36                                                             
R1NME5   DS    CL36                                                             
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
R1COL13  DS    PL8                                                              
R1COL14  DS    PL8                                                              
R1COL15  DS    PL8                                                              
R1COL16  DS    PL8                                                              
R1COL17  DS    PL8                                                              
R1COL18  DS    PL8                                                              
R1COL19  DS    PL8                                                              
R1COL20  DS    PL8                                                              
R1COL21  DS    PL8                                                              
R1COL22  DS    PL8                                                              
R1COL23  DS    PL8                                                              
R1COL24  DS    PL8                                                              
R1COL25  DS    PL8                                                              
R1COL26  DS    PL8                                                              
R1COL27  DS    PL8                                                              
R1COL28  DS    PL8                                                              
R1LEN    EQU   *-R1RECD                                                         
         EJECT                                                                  
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*        ACAPGEQU                                                               
         PRINT OFF                                                              
       ++INCLUDE ACAPGEQU                                                       
         PRINT ON                                                               
*        ACAPGWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACAPGWORKD                                                     
         PRINT ON                                                               
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010APGHM2CEF 11/12/93'                                      
         END                                                                    
