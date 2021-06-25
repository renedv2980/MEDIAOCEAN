*          DATA SET APGHM2NEB  AT LEVEL 064 AS OF 09/23/97                      
*PHASE ACHM2NEB,+0                                                              
         TITLE 'BBDO OFFICE/OFFICE LIST FILTERING'                              
ACHM2NEB CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R5                                                        
                                                                                
*---------------------------------------------------------------------*         
*                                                                               
*---------------------------------------------------------------------*         
HOOKSORT DS    0H                                                               
         L     RF,=A(HOOKIO)                                                    
         ST    RF,AHOOKIO                                                       
                                                                                
         CLI   HOOKNUM,1                                                        
         BNE   XIT                                                              
                                                                                
         AH    R5,=H'18'                                                        
         CLI   QOPT7,C'D'                                                       
         BNE   HK010                                                            
*        L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
*        AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         AH    R5,=H'18'                                                        
                                                                                
HK010    DS    0H                                                               
                                                                                
         USING R1RECD,R5                                                        
         LA    R2,R1CDE3                                                        
         LA    R3,R1NME3                                                        
         CLI   R1ROW1,1                                                         
         BE    HK030                                                            
                                                                                
         LA    R2,R2CDE4                                                        
         LA    R3,R2NME4                                                        
         CLI   R1ROW1,2                                                         
         BE    HK030                                                            
                                                                                
         LA    R2,R3CDE2                                                        
         LA    R3,R3NME2                                                        
         CLI   R1ROW1,3                                                         
         BE    HK030                                                            
                                                                                
         LA    R2,R4CDE2                                                        
         LA    R3,R4NME2                                                        
         CLI   R1ROW1,4                                                         
         BE    HK030                                                            
                                                                                
HK030    DS    0H                                                               
         LA    R4,DEPTTAB                                                       
HK035    CLI   0(R4),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(3,R2),0(R4)                                                    
         BE    HK040                                                            
         LA    R4,3(R4)                                                         
         B     HK035                                                            
                                                                                
HK040    MVC   0(3,R2),DEPTCODE                                                 
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
AHOOKIO  DS    A                                                                
                                                                                
DEPTTAB  DS    0C                                                               
         DC    CL3'130'                                                         
         DC    CL3'131'                                                         
         DC    CL3'132'                                                         
         DC    CL3'133'                                                         
         DC    CL3'134'                                                         
         DC    CL3'135'                                                         
         DC    CL3'136'                                                         
         DC    CL3'137'                                                         
         DC    CL3'138'                                                         
         DC    CL3'139'                                                         
         DC    CL3'140'                                                         
         DC    X'FF'                                                            
                                                                                
                                                                                
DEPTCODE DC    CL3'130'                                                         
DEPTNAME DC    CL36'MEDIA'                                                      
                                                                                
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
R1CDE4   DS    XL50                                                             
R1ROW5   DS    XL2                                                              
R1CDE5   DS    XL50                                                             
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
R1LEN    EQU   *-R1RECD                                                         
                                                                                
R2RECD   ORG   R1RECD                                                           
R2ROW1   DS    XL2                                                              
R2CDE1   DS    XL14                                                             
R2ROW2   DS    XL2                                                              
R2CDE2   DS    XL14                                                             
R2ROW3   DS    XL2                                                              
R2CDE3   DS    XL14                                                             
R2ROW4   DS    XL2                                                              
R2CDE4   DS    XL50                                                             
R2ROW5   DS    XL2                                                              
R2CDE5   DS    XL50                                                             
R2REPNO  DS    XL1                                                              
R2REPCP  DS    XL1                                                              
R2TYPE   DS    XL2                                                              
*                                                                               
R2NME1   DS    CL36                                                             
R2NME2   DS    CL36                                                             
R2NME3   DS    CL36                                                             
R2NME4   DS    CL36                                                             
R2NME5   DS    CL36                                                             
*                                                                               
R2COL1   DS    PL8                                                              
R2COL2   DS    PL8                                                              
R2COL3   DS    PL8                                                              
R2COL4   DS    PL8                                                              
R2COL5   DS    PL8                                                              
R2LEN    EQU   *-R2RECD                                                         
                                                                                
R3RECD   ORG   R1RECD                                                           
R3ROW1   DS    XL2                                                              
R3CDE1   DS    XL14                                                             
R3ROW2   DS    XL2                                                              
R3CDE2   DS    XL14                                                             
R3ROW3   DS    XL2                                                              
R3CDE3   DS    XL50                                                             
R3ROW4   DS    XL2                                                              
R3CDE4   DS    XL50                                                             
R3ROW5   DS    XL2                                                              
R3CDE5   DS    XL14                                                             
R3REPNO  DS    XL1                                                              
R3REPCP  DS    XL1                                                              
R3TYPE   DS    XL2                                                              
*                                                                               
R3NME1   DS    CL36                                                             
R3NME2   DS    CL36                                                             
R3NME3   DS    CL36                                                             
R3NME4   DS    CL36                                                             
R3NME5   DS    CL36                                                             
*                                                                               
R3COL1   DS    PL8                                                              
R3COL2   DS    PL8                                                              
R3COL3   DS    PL8                                                              
R3COL4   DS    PL8                                                              
R3COL5   DS    PL8                                                              
R3LEN    EQU   *-R3RECD                                                         
                                                                                
R4RECD   ORG   R1RECD                                                           
R4ROW1   DS    XL2                                                              
R4CDE1   DS    XL14                                                             
R4ROW2   DS    XL2                                                              
R4CDE2   DS    XL14                                                             
R4ROW3   DS    XL2                                                              
R4CDE3   DS    XL50                                                             
R4ROW4   DS    XL2                                                              
R4CDE4   DS    XL50                                                             
R4ROW5   DS    XL2                                                              
R4CDE5   DS    XL14                                                             
R4REPNO  DS    XL1                                                              
R4REPCP  DS    XL1                                                              
R4TYPE   DS    XL2                                                              
*                                                                               
R4NME1   DS    CL36                                                             
R4NME2   DS    CL36                                                             
R4NME3   DS    CL36                                                             
R4NME4   DS    CL36                                                             
R4NME5   DS    CL36                                                             
*                                                                               
R4COL1   DS    PL8                                                              
R4COL2   DS    PL8                                                              
R4COL3   DS    PL8                                                              
R4COL4   DS    PL8                                                              
R4COL5   DS    PL8                                                              
R4LEN    EQU   *-R4RECD                                                         
                                                                                
         CSECT                                                                  
         DS    0F                                                               
HOOKIO   DS    CL2000                                                           
         EJECT                                                                  
                                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064APGHM2NEB 09/23/97'                                      
         END                                                                    
