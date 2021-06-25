*          DATA SET APGHIVBSG  AT LEVEL 009 AS OF 05/13/96                      
*PHASE ACHIVBSG,+0                                                              
         TITLE 'ADDING COLUMN TOGETHER'                                         
ACHIVBSG CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**                                                       
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R7                                                        
***********************************************************************         
*  ADD THE BUDGET UP                                                  *         
***********************************************************************         
         SPACE 1                                                                
HOOKSORT DS    0H                                                               
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         CLI   QOPT7,C'B'                                                       
         BNE   HK010                                                            
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECIN '),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
*                                                                               
HK010    SR    R1,R1               FIND COLUMN LIST BY REPORT                   
         LA    R2,COLTAB                                                        
HK012    CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   R1REPNO,0(R2)       MATCH ON REPORT NUMBER                       
         BE    HK020                                                            
         IC    R1,1(R2)            GET NEXT                                     
         AR    R2,R1                                                            
         B     HK012                                                            
*                                                                               
HK020    SR    RE,RE                                                            
         IC    RE,2(R2)            GET COLUMN TO ADD TO                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'R1COL1)                                                  
         LA    RE,R1COL1(RE)                                                    
         ZAP   HKDUB,=P'0'                                                      
*                                                                               
         IC    R1,1(R2)            LENGTH OF ELEMENT                            
         SH    R1,=H'05'           R1 = NUMBER OF COLUMNS TO ADD                
         LA    R3,5(R2)            POINT TO START OF COLUMNS                    
HK030    SR    RF,RF                                                            
         IC    RF,0(R3)            GET COLUMN TO ADD TO                         
         BCTR  RF,0                                                             
         MH    RF,=Y(L'R1COL1)                                                  
         LA    RF,R1COL1(RF)           POINT TO ACCUMN TO ADD IN                
         AP    HKDUB,0(L'R1COL1,RF)    ACCUMULATE TOTAL                         
         LA    R3,1(R3)                NEXT COLUMN NUMBER                       
         BCT   R1,HK030                                                         
         ZAP   0(L'R1COL1,RE),HKDUB    SAVE THE ANSWER                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,3(R2)              GET COLUMN TO SUBTRACT                   
         BZ    HK099                                                            
         BCTR  RF,0                                                             
         MH    RF,=Y(L'R1COL1)                                                  
         LA    RF,R1COL1(RF)                                                    
         SP    HKDUB,0(L'R1COL1,RF)                                             
         SR    RF,RF                                                            
         ICM   RF,1,4(R2)              GET COLUMN TO SUBTRACT                   
         BZ    HK099                                                            
         BCTR  RF,0                                                             
         MH    RF,=Y(L'R1COL1)                                                  
         LA    RF,R1COL1(RF)                                                    
         ZAP   0(L'R1COL1,RF),HKDUB    SAVE THE ANSWER                          
*                                                                               
HK099    CLI   QOPT7,C'A'                                                       
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECOUT'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
*                                                                               
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  COLUMN ADDITION TABLE  AL1(REPORT#),AL1(ENTRY LENGTH)              *         
*                         AL1(TOTAL COLUMN),AL1(COLUMNS TO SUB)       *         
*                         AL1(DIFFENCE COL),AL1(COLUMNS TO ADD)       *         
***********************************************************************         
         SPACE 1                                                                
COLTAB   DC    AL1(3,20,30,29,31)                                               
         DC    AL1(2,5,8,11,14,17,20,22,23,24,25,26,27,28,30)                   
         DC    AL1(5,20,30,29,31)                                               
         DC    AL1(2,5,8,11,14,17,20,22,23,24,25,26,27,28,30)                   
         DC    AL1(7,20,30,29,31)                                               
         DC    AL1(2,5,8,11,14,17,20,22,23,24,25,26,27,28,30)                   
         DC    AL1(9,20,30,29,31)                                               
         DC    AL1(2,5,8,11,14,17,20,22,23,24,25,26,27,28,30)                   
         DC    AL1(11,20,32,31,31)                                              
         DC    AL1(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)                    
         DC    AL1(0)                                                           
HKDUB    DS    PL8                                                              
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  WORKING STORAGE                                                    *         
***********************************************************************         
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
R1ROW7   DS    XL2                                                              
R1CDE7   DS    XL14                                                             
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
R1NME7   DS    CL36                                                             
*                                                                               
R1COL1   DS    32PL8                                                            
R1LEN    EQU   *-R1RECD                                                         
                                                                                
                                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009APGHIVBSG 05/13/96'                                      
         END                                                                    
