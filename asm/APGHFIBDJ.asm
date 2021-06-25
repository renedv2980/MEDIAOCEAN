*          DATA SET APGHFIBDJ  AT LEVEL 039 AS OF 06/17/05                      
*PHASE ACHFBDJA                                                                 
         TITLE 'BBDO OFFICE/OFFICE LIST FILTERING'                              
ACHFIBDJ CSECT                                                                  
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
         L     RF,=A(HOOKIO)                                                    
         ST    RF,AHOOKIO                                                       
         CLI   HOOKNUM,1                                                        
         BNE   XIT                                                              
                                                                                
         CLI   QOPT7,C'D'                                                       
         BNE   HK005                                                            
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
                                                                                
HK005    DS    0H                                                               
*        MVI   HOOKSW,C'Y'                                                      
*                                                                               
*        LA    R7,GRPS                                                          
*        LA    R6,GRPTAB                                                        
*                                                                               
*        USING OFFRECD,R2                                                       
*K010    L     R2,AHOOKIO                                                       
*        MVC   0(42,R2),SPACES                                                  
*        MVI   OFFKTYP,OFFKTYPQ                                                 
*        MVC   OFFKCPY,QCOMPANY                                                 
*        MVC   OFFKOFF,0(R6)                                                    
*        GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',AHOOKIO,AHOOKIO                  
*        TM    DMCB,X'10'                                                       
*        BZ    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
*        L     R2,AHOOKIO                                                       
*        AH    R2,DATADISP                                                      
*K035    CLI   0(R2),0                                                          
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*        CLI   0(R2),OFLELQ                                                     
*        BE    HK040                                                            
*        ZIC   R0,1(R2)                                                         
*        AR    R2,R0                                                            
*        B     HK035                                                            
*                                                                               
*        USING OFLELD,R2                                                        
*K040    DS    0H                                                               
*        LA    R4,2(R6)                                                         
*        LA    R3,OFLNTRY                                                       
*        ZIC   R5,OFLLN                                                         
*        SH    R5,=H'3'                                                         
*K045    MVC   0(2,R4),0(R3)                                                    
*        LA    R3,2(R3)                                                         
*        LA    R4,2(R4)                                                         
*        BCTR  R5,0                                                             
*        BCT   R5,HK045                                                         
*        MVI   0(R4),X'FF'                                                      
*                                                                               
*        LA    R6,ENTLEN(R6)                                                    
*        BCT   R7,HK010                                                         
         EJECT                                                                  
*------------------------------------------------------------------*            
*        IF OFFICE LISTS HAVE ALREADY BEEN BUILD SKIPDOWN TO HERE               
*------------------------------------------------------------------*            
HK050    DS    0H                                                               
         USING R1RECD,R5                                                        
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         CLC   R1CDE6(5),SPACES                                                 
         BE    XIT                                                              
                                                                                
         MVC   ACCOFF,R1CDE2                                                    
         MVC   MYCON,R1CDE6                                                     
                                                                                
         LA    R6,GRPTAB                                                        
         LA    R7,GRPS                                                          
HK052    MVC   ACCGRP,0(R6)                                                     
         LA    R4,2(R6)                                                         
HK055    CLI   0(R4),X'FF'                                                      
         BE    HK058                                                            
         CLC   0(2,R4),ACCOFF                                                   
         BE    HK060                                                            
         LA    R4,2(R4)                                                         
         B     HK055                                                            
HK058    LA    R6,ENTLEN(R6)                                                    
         BCT   R7,HK052                                                         
         MVC   ACCGRP,=C'99'                                                    
*                                                                               
HK060    DS    0H                                                               
         MVC   CONOFF,MYCON+3                                                   
         CLI   MYCON+1,C'6'                                                     
         BNE   *+10                                                             
         MVC   CONOFF,MYCON+2                                                   
                                                                                
         LA    R6,GRPTAB                                                        
         LA    R7,GRPS                                                          
HK062    MVC   CONGRP,0(R6)                                                     
         LA    R4,2(R6)                                                         
HK065    CLI   0(R4),X'FF'                                                      
         BE    HK068                                                            
         CLC   0(2,R4),CONOFF                                                   
         BE    HK070                                                            
         LA    R4,2(R4)                                                         
         B     HK065                                                            
HK068    LA    R6,ENTLEN(R6)                                                    
         BCT   R7,HK062                                                         
         MVC   CONGRP,=C'99'                                                    
*                                                                               
HK070    DS    0H                                                               
         MVC   R1CDE6,SPACES                                                    
         MVC   R1CDE6(L'CONGRP),CONGRP                                          
                                                                                
         CLC   ACCGRP,CONGRP                                                    
         BE    HK080                                                            
         ZAP   R1COL1,=P'0'                                                     
         ZAP   R1COL2,=P'0'                                                     
         ZAP   R1COL3,=P'0'                                                     
         ZAP   R1COL4,=P'0'                                                     
         B     XIT                                                              
                                                                                
HK080    ZAP   R1COL5,=P'0'                                                     
         ZAP   R1COL6,=P'0'                                                     
         ZAP   R1COL7,=P'0'                                                     
         ZAP   R1COL8,=P'0'                                                     
         B     XIT                                                              
*                                                                               
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        DEFINE CONSTANTS                                                       
*----------------------------------------------------------------*              
AHOOKIO  DS    A                                                                
ACCOFF   DC    CL2' '                                                           
ACCGRP   DC    CL2' '                                                           
CONOFF   DC    CL2' '                                                           
CONGRP   DC    CL2' '                                                           
MYCON    DC    CL5' '                                                           
                                                                                
         DS    0D                                                               
         DC    CL8'*GRPTAB*'                                                    
GRPTAB   DS    0H                                                               
GRPA1    DC    CL2'A1'                                                          
A1TAB    DC    CL100'AAABACADAEAFAGAHAIAJAK'                                    
         DC    X'FF'                                                            
ENTLEN   EQU   *-GRPTAB                                                         
GRPC1    DC    CL2'C1'                                                          
C1TAB    DC    CL100'CACBCDCE'                                                  
         DC    X'FF'                                                            
GRPW2    DC    CL2'W2'                                                          
W2TAB    DC    CL100'WAWDWF'                                                    
         DC    X'FF'                                                            
GRPD1    DC    CL2'D1'                                                          
D1TAB    DC    CL100'DADBDFDGDHDIDJDKDLDM'                                      
         DC    X'FF'                                                            
GRPN1    DC    CL2'N1'                                                          
N1TAB    DC    CL100'NANDNH'                                                    
         DC    X'FF'                                                            
GRPM1    DC    CL2'M1'                                                          
M1TAB    DC    CL100'MA'                                                        
         DC    X'FF'                                                            
GRPX1    DC    CL2'X1'                                                          
X1TAB    DC    CL100'XAXB'                                                      
         DC    X'FF'                                                            
GRPQ1    DC    CL2'Q1'                                                          
Q1TAB    DC    CL100'QAQBQC'                                                    
         DC    X'FF'                                                            
GRPI1    DC    CL2'I1'                                                          
I1TAB    DC    CL100'IA'                                                        
         DC    X'FF'                                                            
TABLEN   EQU   *-GRPTAB                                                         
GRPS     EQU   (*-GRPTAB)/ENTLEN                                                
                                                                                
                                                                                
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
R1ROW6   DS    XL2                                                              
R1CDE6   DS    XL14                                                             
R1REPNO  DS    XL1                                                              
R1REPCP  DS    XL1                                                              
R1TYPE   DS    XL2                                                              
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
**PAN#1  DC    CL21'039APGHFIBDJ 06/17/05'                                      
         END                                                                    
