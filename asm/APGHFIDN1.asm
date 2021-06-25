*          DATA SET APGHFIDN1  AT LEVEL 032 AS OF 01/14/00                      
*PHASE ACHFIDN1,+0                                                              
         TITLE 'PALMER JARVIS DDB VANCOUVER'                                    
ACHFIDN1 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R9                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R8,HOOKAREC         ADDR OF SORT RECORD                          
         USING MRRECD,R8                                                        
                                                                                
         CLI   QOPT7,C'D'                                                       
         BNE   HK001                                                            
         LA    R0,MRLEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD3B'),(R8),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK001    CLI   HOOKNUM,3                                                        
         BNE   HK005                                                            
         BAS   RE,TOTCOL                                                        
         CLC   MRCODE6(2),=C'90'                                                
         BNE   HK100                                                            
         BAS   RE,YTDACUM                                                       
         B     HK100                                                            
                                                                                
HK005    CLI   HOOKNUM,4                                                        
         BNE   XIT                                                              
         BAS   RE,TOTCOL                                                        
         CLC   MRCODE5(2),=C'90'                                                
         BNE   HK100                                                            
         BAS   RE,YTDACUM                                                       
         B     HK100                                                            
                                                                                
HK100    LA    R0,MRCOLLNQ         NUMBER OF COLUMNS                            
         LA    R1,MRCOL                                                         
         CP    0(L'MRCOL,R1),=P'0' ANY AMOUNT?                                  
         BNE   *+16                                                             
         LA    R1,L'MRCOL(R1)                                                   
         BCT   R0,*-14                                                          
         B     XITNO                                                            
         CLI   QOPT7,C'D'                                                       
         BNE   XIT                                                              
         LA    R0,MRLEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD3A'),(R8),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        ACCUMULATE CALENDAR FORECAST COLUMN                                    
*----------------------------------------------------------------*              
TOTCOL   NTR1                                                                   
         ZAP   MRCOL13,=P'0'                                                    
         AP    MRCOL13,MRCOL1                                                   
         AP    MRCOL13,MRCOL2                                                   
         AP    MRCOL13,MRCOL3                                                   
         AP    MRCOL13,MRCOL4                                                   
         AP    MRCOL13,MRCOL5                                                   
         AP    MRCOL13,MRCOL6                                                   
         AP    MRCOL13,MRCOL7                                                   
         AP    MRCOL13,MRCOL8                                                   
         AP    MRCOL13,MRCOL9                                                   
         AP    MRCOL13,MRCOL10                                                  
         AP    MRCOL13,MRCOL11                                                  
         AP    MRCOL13,MRCOL12                                                  
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------*              
*        ACCUMULATE THE YTD PROFIT LOSS ROW                                     
*----------------------------------------------------------------*              
YTDACUM  NTR1                                                                   
         AP    MRCOL2,MRCOL1                                                    
         AP    MRCOL3,MRCOL2                                                    
         AP    MRCOL4,MRCOL3                                                    
         AP    MRCOL5,MRCOL4                                                    
         AP    MRCOL6,MRCOL5                                                    
         AP    MRCOL7,MRCOL6                                                    
         AP    MRCOL8,MRCOL7                                                    
         AP    MRCOL9,MRCOL8                                                    
         AP    MRCOL10,MRCOL9                                                   
         AP    MRCOL11,MRCOL10                                                  
         AP    MRCOL12,MRCOL11                                                  
         ZAP   MRCOL14,=P'0'                                                    
         ZAP   MRCOL15,=P'0'                                                    
         ZAP   MRCOL16,=P'0'                                                    
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------*              
*        LITERAL POOL                                                           
*----------------------------------------------------------------*              
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
*        WORKING STORAGE                                                        
*----------------------------------------------------------------*              
MRRECD   DSECT                                                                  
MRROW1   DS    XL2                                                              
MRCODE1  DS    XL14                                                             
MRROW2   DS    XL2                                                              
MRCODE2  DS    XL14                                                             
MRROW3   DS    XL2                                                              
MRCODE3  DS    XL14                                                             
MRROW4   DS    XL2                                                              
MRCODE4  DS    XL14                                                             
MRROW5   DS    XL2                                                              
MRCODE5  DS    XL14                                                             
MRROW6   DS    XL2                                                              
MRCODE6  DS    XL14                                                             
MRROW7   DS    XL2                                                              
MRCODE7  DS    XL14                                                             
MRREPNO  DS    XL1                                                              
MRREPCP  DS    XL1                                                              
MRTYPE   DS    XL2                                                              
*                                                                               
MRNME1   DS    CL36                                                             
MRNME2   DS    CL36                                                             
MRNME3   DS    CL36                                                             
MRNME4   DS    CL36                                                             
MRNME5   DS    CL36                                                             
MRNME6   DS    CL36                                                             
MRNME7   DS    CL36                                                             
*                                                                               
MRCOL    DS    0PL8                                                             
MRCOL1   DS    PL8                                                              
MRCOL2   DS    PL8                                                              
MRCOL3   DS    PL8                                                              
MRCOL4   DS    PL8                                                              
MRCOL5   DS    PL8                                                              
MRCOL6   DS    PL8                                                              
MRCOL7   DS    PL8                                                              
MRCOL8   DS    PL8                                                              
MRCOL9   DS    PL8                                                              
MRCOL10  DS    PL8                                                              
MRCOL11  DS    PL8                                                              
MRCOL12  DS    PL8                                                              
MRCOL13  DS    PL8                                                              
MRCOL14  DS    PL8                                                              
MRCOL15  DS    PL8                                                              
MRCOL16  DS    PL8                                                              
MRCOLLNQ EQU   (*-MRCOL1)/L'MRCOL                                               
MRLEN    EQU   *-MRRECD                                                         
         EJECT                                                                  
                                                                                
*        ACAPGGEND                                                              
*        ACGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032APGHFIDN1 01/14/00'                                      
         END                                                                    
