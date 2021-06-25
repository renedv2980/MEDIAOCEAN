*          DATA SET APGHM2BS1  AT LEVEL 110 AS OF 08/19/97                      
*PHASE ACHM2BS1,+0                                                              
         TITLE 'BATES CLIENT WARNER LAMBERT'                                    
ACHM2BS1 CSECT                                                                  
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
         L     RF,=A(HOOKIO)                                                    
         ST    RF,AHOOKIO                                                       
         CLI   HOOKNUM,1                                                        
         BNE   HK200                                                            
         AH    R5,=H'18'                                                        
                                                                                
         CLI   QOPT4,C'1'                                                       
         BNE   HK010                                                            
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         AH    R5,=H'18'                                                        
                                                                                
HK010    CLC   LSTCNTR,CURRCON                                                  
         BNE   HK015                                                            
         MVC   R1CDE1(1),LSTF4                                                  
         B     HK099                                                            
                                                                                
HK015    DS    0H                                                               
         MVC   LSTCNTR,CURRCON                                                  
         MVI   LSTF4,C' '                                                       
         USING ACTRECD,R3                                                       
         L     R3,AHOOKIO                                                       
         MVC   0(42,R3),SPACES                                                  
         MVC   0(L'CURRCON,R3),CURRCON                                          
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),=C'ACCDIR',AHOOKIO,AHOOKIO           
         TM    DMCB,X'10'                                                       
         BO    XIT                                                              
         L     R3,AHOOKIO                                                       
         MVC   LSTF4,ACTKSAF4                                                   
         MVC   R1CDE1(1),LSTF4                                                  
                                                                                
HK099    CLI   QOPT6,C'1'                                                       
         BNE   XIT                                                              
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
                                                                                
HK200    CLI   HOOKNUM,2                                                        
         BNE   HK300                                                            
         CLI   QOPT5,C'2'                                                       
         BNE   HK201                                                            
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
HK201    DS    0H                                                               
                                                                                
         CLC   THISPER,SPACES                                                   
         BE    HK203                                                            
         CLC   THISPER,R1CDE4                                                   
         BNE   HK203                                                            
         ZAP   R1COL2,MYCLTHRS                                                  
         NI    R1COL2+7,X'FF'-X'08'                                             
         B     HK205                                                            
HK203    MVC   THISPER,R1CDE4                                                   
         ZAP   MYCLTHRS,R1COL2                                                  
                                                                                
HK205    LA    R4,USEF4                                                         
HK210    CLI   0(R4),X'FF'                                                      
         BE    XITNO                                                            
         CLC   0(1,R4),R1CDE1                                                   
         BE    HK220                                                            
         LA    R4,37(R4)                                                        
         B     HK210                                                            
HK220    MVC   R1NME1,1(R4)                                                     
         CLI   R1REPNO,2                                                        
         BNE   HK230                                                            
         MVI   R1CDE1,C' '                                                      
                                                                                
HK230    CLI   QOPT6,C'2'                                                       
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
HK300    DS    0H                                                               
         CLI   HOOKNUM,3                                                        
         BNE   XIT                                                              
         CLI   QOPT5,C'3'                                                       
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD3B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK305    LA    R4,USEF4                                                         
HK310    CLI   0(R4),X'FF'                                                      
         BE    XITNO                                                            
         CLC   0(1,R4),R1CDE1                                                   
         BE    HK320                                                            
         LA    R4,37(R4)                                                        
         B     HK310                                                            
HK320    MVC   R1NME1,1(R4)                                                     
                                                                                
         CLI   QOPT6,C'3'                                                       
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD3A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
HOOK1ST  DC    C'Y'                                                             
LSTCNTR  DC    CL15' '                                                          
LSTF4    DC    CL1' '                                                           
LSTNAME  DC    CL36' '                                                          
                                                                                
USEF4    DS    0C                                                               
         DC    C'U',CL36'UPPER RESPIRATORY'                                     
         DC    C'A',CL36'AMERICAN CHICHLE'                                      
         DC    C'F',CL36'FIRST AID'                                             
         DC    X'FF'                                                            
                                                                                
THISPER  DC    CL7' '                                                           
MYCLTHRS DC    PL8'0'                                                           
                                                                                
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
R1LEN    EQU   *-R1RECD                                                         
         ORG   R1RECD                                                           
R2ROW1   DS    XL2                                                              
R2CDE1   DS    XL14                                                             
R2ROW2   DS    XL2                                                              
R2CDE2   DS    XL14                                                             
R2ROW3   DS    XL2                                                              
R2CDE3   DS    XL14                                                             
R2ROW4   DS    XL2                                                              
R2CDE4   DS    XL14                                                             
R2ROW5   DS    XL2                                                              
R2CDE5   DS    XL14                                                             
*                                                                               
R2ZEROS  DS    XL80                                                             
*                                                                               
R2COL1   DS    PL8                                                              
R2COL2   DS    PL8                                                              
R2COL3   DS    PL8                                                              
R2COL4   DS    PL8                                                              
*                                                                               
R2NME1   DS    CL36                                                             
R2NME2   DS    CL36                                                             
R2NME3   DS    CL36                                                             
R2NME4   DS    CL36                                                             
R2NME5   DS    CL36                                                             
R2LEN    EQU   *-R1RECD                                                         
                                                                                
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
**PAN#1  DC    CL21'110APGHM2BS1 08/19/97'                                      
         END                                                                    
