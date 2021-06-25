*          DATA SET APGHFIBDG  AT LEVEL 047 AS OF 06/17/05                      
*PHASE ACHFBDGA                                                                 
         TITLE 'BATES CLIENT FILTER GROUPINGS'                                  
ACHFBDG  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(,R1)                                                        
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R7                                                        
                                                                                
*---------------------------------------------------------------------*         
*                                                                     *         
*---------------------------------------------------------------------*         
HOOKSORT DS    0H                                                               
         CLI   HOOKNUM,1                                                        
         BNE   HK200                                                            
         AHI   R7,18                                                            
                                                                                
         CLI   QOPT7,C'D'                                                       
         BNE   HK010                                                            
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AHI   R0,18                                                            
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         AHI   R7,18                                                            
                                                                                
HK010    DS    0H                                                               
         CLC   R1CDE1,SPACES                                                    
         BE    XITNO                                                            
                                                                                
         LA    R4,FLTRTAB                                                       
HK015    CLI   0(R4),X'FF'                                                      
         BE    HK098                                                            
         CLC   2(2,R4),R1CDE1                                                   
         BE    HK030                                                            
         LA    R4,FLTRLEN(R4)                                                   
         B     HK015                                                            
                                                                                
HK030    MVC   R1CDE1(2),0(R4)                                                  
         B     HK099                                                            
                                                                                
HK098    MVC   R1CDE1(2),=C'00'                                                 
HK099    CLI   QOPT7,C'D'                                                       
         BNE   XIT                                                              
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AHI   R0,18                                                            
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1A'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
HK200    CLI   HOOKNUM,2                                                        
         BNE   XIT                                                              
         CLI   QOPT7,C'D'                                                       
         BNE   HK205                                                            
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK205    DS    0H                                                               
         LA    R4,GRPTAB                                                        
HK210    CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   0(2,R4),R1CDE1                                                   
         BE    HK220                                                            
         LA    R4,GRPLEN(R4)                                                    
         B     HK210                                                            
HK220    DS    0H                                                               
         MVC   R1NME1,2(R4)                                                     
                                                                                
HK230    CLI   QOPT7,C'D'                                                       
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2A'),(R7),C'DUMP',(R0),=C'2D',    X        
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
LSTNAME  DC    CL36' '                                                          
                                                                                
*        FIELD 1 - GROUP NUMBER FOR FILTER                                      
*        FIELD 2 - FILTER                                                       
FLTRTAB  DS    0C                                                               
         DC    CL2'M1',CL2'MA'                                                  
FLTRLEN  EQU   *-FLTRTAB                                                        
                                                                                
         DC    CL2'N1',CL2'NA'                                                  
         DC    CL2'N1',CL2'ND'                                                  
         DC    CL2'N1',CL2'NH'                                                  
         DC    CL2'N1',CL2'NM'                                                  
         DC    CL2'N1',CL2'NP'                                                  
         DC    CL2'N1',CL2'NW'                                                  
                                                                                
         DC    CL2'W1',CL2'WA'                                                  
         DC    CL2'W1',CL2'WB'                                                  
         DC    CL2'W1',CL2'WC'                                                  
         DC    CL2'W1',CL2'WD'                                                  
         DC    CL2'W1',CL2'WE'                                                  
         DC    CL2'W1',CL2'WF'                                                  
         DC    CL2'W1',CL2'WG'                                                  
                                                                                
         DC    CL2'X1',CL2'XA'                                                  
         DC    CL2'X1',CL2'XB'                                                  
         DC    CL2'X1',CL2'XM'                                                  
                                                                                
         DC    CL2'D1',CL2'DA'                                                  
         DC    CL2'D1',CL2'DB'                                                  
         DC    CL2'D1',CL2'DC'                                                  
         DC    CL2'D1',CL2'DF'                                                  
         DC    CL2'D1',CL2'DG'                                                  
         DC    CL2'D1',CL2'DH'                                                  
         DC    CL2'D1',CL2'DI'                                                  
         DC    CL2'D1',CL2'DJ'                                                  
         DC    CL2'D1',CL2'DK'                                                  
         DC    CL2'D1',CL2'DL'                                                  
         DC    CL2'D1',CL2'DM'                                                  
         DC    CL2'D1',CL2'DR'                                                  
         DC    CL2'D1',CL2'EA'                                                  
         DC    CL2'D1',CL2'EB'                                                  
         DC    CL2'D1',CL2'EC'                                                  
         DC    CL2'D1',CL2'ED'                                                  
         DC    CL2'D1',CL2'EE'                                                  
         DC    CL2'D1',CL2'EF'                                                  
         DC    CL2'D1',CL2'EG'                                                  
         DC    CL2'D1',CL2'EH'                                                  
         DC    CL2'D1',CL2'EJ'                                                  
         DC    CL2'D1',CL2'EK'                                                  
         DC    CL2'D1',CL2'EL'                                                  
         DC    CL2'D1',CL2'EM'                                                  
         DC    CL2'D1',CL2'EN'                                                  
         DC    CL2'D1',CL2'EO'                                                  
         DC    CL2'D1',CL2'EP'                                                  
         DC    CL2'D1',CL2'EQ'                                                  
                                                                                
         DC    CL2'A1',CL2'AA'                                                  
         DC    CL2'A1',CL2'AB'                                                  
         DC    CL2'A1',CL2'AC'                                                  
         DC    CL2'A1',CL2'AD'                                                  
         DC    CL2'A1',CL2'AE'                                                  
         DC    CL2'A1',CL2'AF'                                                  
         DC    CL2'A1',CL2'AG'                                                  
         DC    CL2'A1',CL2'AH'                                                  
         DC    CL2'A1',CL2'AI'                                                  
         DC    CL2'A1',CL2'AJ'                                                  
         DC    CL2'A1',CL2'AK'                                                  
*                                                                               
         DC    CL2'C1',CL2'CA'                                                  
         DC    CL2'C1',CL2'CB'                                                  
         DC    CL2'C1',CL2'CD'                                                  
         DC    CL2'C1',CL2'CE'                                                  
*                                                                               
         DC    CL2'C3',CL2'CM'                                                  
*                                                                               
         DC    CL2'F1',CL2'FA'                                                  
         DC    CL2'F1',CL2'FR'                                                  
*                                                                               
         DC    CL2'Q1',CL2'QA'                                                  
         DC    CL2'Q1',CL2'QB'                                                  
         DC    CL2'Q1',CL2'QC'                                                  
*                                                                               
         DC    CL2'I1',CL2'IA'                                                  
*                                                                               
         DC    CL2'N9',CL2'NP'                                                  
         DC    CL2'N9',CL2'NW'                                                  
*                                                                               
         DC    CL2'Z1',CL2'ZA'                                                  
*                                                                               
         DC    X'FF'                                                            
                                                                                
GRPTAB   DS    0C                                                               
         DC    CL2'D1',CL36'DETROIT'                                            
GRPLEN   EQU   *-GRPTAB                                                         
         DC    CL2'M1',CL36'MINNEAPOLIS'                                        
         DC    CL2'N1',CL36'NEW YORK'                                           
         DC    CL2'N9',CL36'BBDO DIVISION'                                      
         DC    CL2'W1',CL36'WEST'                                               
         DC    CL2'X1',CL36'CORPORATE'                                          
         DC    CL2'Z1',CL36'WORLDWIDE'                                          
         DC    CL2'A1',CL36'BBDO SOUTH'                                         
         DC    CL2'C1',CL36'BBDO CHICAGO'                                       
         DC    CL2'C3',CL36'BBDO MICHIGAN'                                      
         DC    CL2'F1',CL36'INTERONE MARKETING'                                 
         DC    CL2'Q1',CL36'@TMOSPHERE'                                         
         DC    CL2'I1',CL36'INTL N.Y.'                                          
         DC    CL2'00',CL36'UNDEFINED'                                          
         DC    X'FF'                                                            
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
R1LEN    EQU   *-R1RECD                                                         
                                                                                
                                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047APGHFIBDG 06/17/05'                                      
         END                                                                    
