*          DATA SET APGHFIBDC  AT LEVEL 028 AS OF 08/21/96                      
*PHASE ACHFIBDC,+0                                                              
         TITLE 'BBDO OFFICE/OFFICE LIST FILTERING'                              
ACHFIBDC CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R5                                                        
                                                                                
*---------------------------------------------------------------------*         
*        THIS PROGRAM WILL LET CLIENT FILTER ON OFFICE LIST           *         
*        OR A SPECIFIC OFFICE ENTERED IN THE SELECT FIELD                       
*        WILL BUILD A TABLE AND FILTER THIS AGAINST ROW ONE                     
*---------------------------------------------------------------------*         
HOOKSORT DS    0H                                                               
         CLC   QSELECT(2),=C'  '   IF BLANK THEN NO FILTERING REQUESTED         
         BE    XIT                                                              
                                                                                
         L     RF,=A(HOOKIO)                                                    
         ST    RF,AHOOKIO                                                       
         CLI   HOOKNUM,1                                                        
         BNE   XIT                                                              
         CLI   HOOKSW,C'Y'                                                      
         BE    HK050                                                            
                                                                                
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
         MVI   HOOKSW,C'Y'                                                      
                                                                                
         USING OFFRECD,R3                                                       
         L     R3,AHOOKIO                                                       
         MVC   0(42,R3),SPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,QCOMPANY                                                 
         MVC   OFFKOFF,QSELECT                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',AHOOKIO,AHOOKIO                  
         TM    DMCB,X'10'                                                       
         BO    XIT                                                              
                                                                                
         LA    R2,OFFTAB                                                        
         MVC   0(2,R2),QSELECT                                                  
         MVI   2(R2),X'FF'                                                      
                                                                                
         L     R3,AHOOKIO                                                       
         AH    R3,DATADISP                                                      
HK015    CLI   0(R3),0                                                          
         BE    HK050                                                            
         CLI   0(R3),OFLELQ                                                     
         BE    HK020                                                            
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     HK015                                                            
                                                                                
         USING OFLELD,R3                                                        
HK020    LA    R2,OFFTAB                                                        
         LA    R4,OFLNTRY                                                       
         ZIC   R5,OFLLN                                                         
         SH    R5,=H'3'                                                         
HK025    MVC   0(2,R2),0(R4)                                                    
         LA    R2,2(R2)                                                         
         LA    R4,2(R4)                                                         
         BCTR  R5,0                                                             
         BCT   R5,HK025                                                         
         MVI   0(R2),X'FF'                                                      
                                                                                
HK050    DS    0H                                                               
         USING R1RECD,R5                                                        
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         AH    R5,=H'18'                                                        
         CLC   R1CDE2(2),=C'  '                                                 
         BE    XIT                                                              
         MVC   THISOFF,R1CDE2                                                   
         LA    R2,OFFTAB                                                        
HK052    CLI   0(R2),X'FF'                                                      
         BE    XITNO                                                            
         CLC   0(2,R2),THISOFF                                                  
         BE    XIT                                                              
         LA    R2,2(R2)                                                         
         B     HK052                                                            
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
AHOOKIO  DS    A                                                                
HOOKSW   DC    C'N'                                                             
THISOFF  DC    CL2' '                                                           
                                                                                
OFFTAB   DC    255CL2'  '                                                       
                                                                                
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
R1COL9   DS    PL8                                                              
R1COL10  DS    PL8                                                              
R1COL11  DS    PL8                                                              
R1COL12  DS    PL8                                                              
R1COL13  DS    PL8                                                              
R1COL14  DS    PL8                                                              
R1COL15  DS    PL8                                                              
R1COL16  DS    PL8                                                              
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
**PAN#1  DC    CL21'028APGHFIBDC 08/21/96'                                      
         END                                                                    
