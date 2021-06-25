*          DATA SET APGHFINEB  AT LEVEL 002 AS OF 05/17/96                      
*PHASE ACHFINEB,+0                                                              
         TITLE 'BATES CLIENT WARNER LAMBERT'                                    
ACHFINEB CSECT                                                                  
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
                                                                                
         CLI   QOPT5,C'D'                                                       
         BNE   HK010                                                            
HK000B   L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK010    CLC   CURRCON+1(2),=C'11'                                              
         BE    HK099                                                            
         CLC   CURRCON+1(2),=C'12'                                              
         BE    HK099                                                            
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         AH    R5,=H'18'                                                        
         CLC   CURRCON+1(2),=C'16'                                              
         BNE   HK015                                                            
         MVC   R1CDE1(2),CURRCON+3                                              
         B     HK099                                                            
HK015    MVC   R1CDE1(2),CURRCON+4                                              
                                                                                
HK099    DS    0H                                                               
         CLI   QOPT5,C'D'                                                       
         BNE   XIT                                                              
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
HK200    CLI   HOOKNUM,2                                                        
         BNE   XIT                                                              
         CLI   QOPT5,C'E'                                                       
         BNE   HK205                                                            
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK205    CLC   R1CDE1(2),SPACES                                                 
         BE    XITNO                                                            
         CLI   R1CDE1,0                                                         
         BE    XITNO                                                            
                                                                                
         MVC   R1NME1,SPACES                                                    
         LA    R4,USEOFF                                                        
HK210    CLI   0(R4),X'FF'                                                      
         BE    HK230                                                            
         CLC   0(2,R4),R1CDE1                                                   
         BE    HK220                                                            
         LA    R4,OFFTABL(R4)                                                   
         B     HK210                                                            
HK220    MVC   R1NME1,2(R4)                                                     
                                                                                
HK230    CLI   QOPT5,C'E'                                                       
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
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
HOOK1ST  DC    C'Y'                                                             
TEMPOFF  DC    CL2' '                                                           
                                                                                
USEOFF   DS    0C                                                               
         DC    C'CA',CL36'CHICAGO'                                              
OFFTABL  EQU   *-USEOFF                                                         
         DC    C'CC',CL36'DUMMY'                                                
         DC    C'HA',CL36'NY CORPORATE'                                         
         DC    C'LA',CL36'LOS ANGELES'                                          
         DC    C'LC',CL36'DDBN-LA A/V STUDIOS'                                  
         DC    C'LM',CL36'DDBN/LA DIRECT MARKETING'                             
         DC    C'LN',CL36'DDBN/LA PUBLIC RELATIONS'                             
         DC    C'LP',CL36'DDBN/LA SALES AND PROMOTION'                          
         DC    C'LR',CL36'ART STUDIO LA'                                        
         DC    C'NA',CL36'NEW YORK'                                             
         DC    C'NC',CL36'STUDIO BROADCAST'                                     
         DC    C'NN',CL36'DUMMYE'                                               
         DC    C'NP',CL36'E G SERVICES'                                         
         DC    C'NR',CL36'E G CENTRAL'                                          
         DC    C'TA',CL36'DETROIT'                                              
         DC    C'UA',CL36'USA MEDIA'                                            
         DC    C'WA',CL36'INTL PARTNERS'                                        
         DC    C'YA',CL36'LEFT BANK GENERAL'                                    
         DC    C'ZA',CL36'INTL'                                                 
         DC    C'99',CL36'DUMMY'                                                
         DC    C'  ',CL36'SPACES'                                               
         DC    X'FF'                                                            
                                                                                
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
R1LEN    EQU   *-R1RECD                                                         
                                                                                
         CSECT                                                                  
         DS    0F                                                               
HOOKIO   DS    CL2000                                                           
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
**PAN#1  DC    CL21'002APGHFINEB 05/17/96'                                      
         END                                                                    
