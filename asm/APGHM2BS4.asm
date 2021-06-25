*          DATA SET APGHM2BS4  AT LEVEL 107 AS OF 10/27/97                      
*PHASE ACHM2BS4,+0                                                              
         TITLE 'BATES CLIENT WARNER LAMBERT'                                    
ACHM2BS4 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R5                                                        
                                                                                
*---------------------------------------------------------------------*         
*        THIS HOOK IS USED FOR MULTIPLE FORMATS                       *         
*        (FORMATS M24, M25, M26, M27)                                 *         
*---------------------------------------------------------------------*         
HOOKSORT DS    0H                                                               
         CLI   QOPT4,C' '          IF BLANK THEN NO FILTERING REQUESTED         
         BE    XIT                                                              
                                                                                
         L     RF,=A(HOOKIO)                                                    
         ST    RF,AHOOKIO                                                       
         CLI   HOOKNUM,1                                                        
         BNE   XIT                                                              
         AH    R5,=H'18'                                                        
                                                                                
         CLI   QOPT7,C'D'                                                       
         BNE   HK010                                                            
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         AH    R5,=H'18'                                                        
                                                                                
HK010    DS    0H                                                               
         CLC   LSTCNTR,CURRCON                                                  
         BNE   HK015                                                            
         CLC   QOPT4,LSTF5                                                      
         BNE   XITNO                                                            
         B     HK099                                                            
                                                                                
HK015    DS    0H                                                               
         MVC   LSTCNTR,CURRCON                                                  
         MVI   LSTF5,C' '                                                       
         USING ACTRECD,R3                                                       
         L     R3,AHOOKIO                                                       
         MVC   0(42,R3),SPACES                                                  
         MVC   0(L'CURRCON,R3),CURRCON                                          
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),=C'ACCDIR',AHOOKIO,AHOOKIO           
         TM    DMCB,X'10'                                                       
         BO    XIT                                                              
         L     R3,AHOOKIO                                                       
         MVC   LSTF5,ACTKSAF5                                                   
         CLC   QOPT4,LSTF5                                                      
         BNE   XITNO                                                            
                                                                                
HK099    CLI   QOPT7,C'D'                                                       
         BNE   XIT                                                              
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
LSTCNTR  DC    CL15' '                                                          
LSTF5    DC    CL1' '                                                           
AHOOKIO  DS    A                                                                
                                                                                
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
**PAN#1  DC    CL21'107APGHM2BS4 10/27/97'                                      
         END                                                                    
