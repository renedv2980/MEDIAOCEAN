*          DATA SET APGHFIBSC  AT LEVEL 047 AS OF 04/08/98                      
*PHASE ACHFIBSC,+0                                                              
         TITLE 'BATES CLIENT FILTER GROUPINGS'                                  
ACHFIBSC CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
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
         AH    R7,=H'18'                                                        
                                                                                
         CLI   QOPT7,C'D'                                                       
         BNE   HK010                                                            
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R7),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         AH    R7,=H'18'                                                        
                                                                                
HK010    DS    0H                                                               
         LA    R4,FLTRTAB                                                       
HK015    CLI   0(R4),X'FF'                                                      
         BE    HK020                                                            
         CLC   1(1,R4),R1CDE1                                                   
         BE    HK030                                                            
         LA    R4,FLTRLEN(R4)                                                   
         B     HK015                                                            
HK020    MVI   R1CDE1,C'0'                                                      
         B     HK099                                                            
HK030    MVC   R1CDE1(1),0(R4)                                                  
                                                                                
HK099    CLI   QOPT7,C'D'                                                       
         BNE   XIT                                                              
         L     R7,HOOKAREC         ADDR OF SORT RECORD                          
         LA    R0,R1LEN                                                         
         AH    R0,=H'18'                                                        
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
         CLI   R1REPNO,4                                                        
         BE    HK230                                                            
         CLI   R1REPNO,8                                                        
         BE    HK230                                                            
                                                                                
         LA    R4,GRPTAB                                                        
HK210    CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),R1CDE1                                                   
         BE    HK220                                                            
         LA    R4,GRPLEN(R4)                                                    
         B     HK210                                                            
HK220    DS    0H                                                               
         CLI   R1REPNO,3                                                        
         BE    HK225                                                            
         CLI   R1REPNO,7                                                        
         BE    HK225                                                            
         MVC   R1NME2,R1NME1                                                    
HK225    MVC   R1NME1,1(R4)                                                     
                                                                                
HK230    DS    0H                                                               
         B     HK340                                                            
                                                                                
         LA    RE,REPTAB                                                        
HK311    CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    HK340                                                            
         CLC   R1REPNO,0(RE)       MATCH ON REPORT NUMBER                       
         BE    HK315                                                            
         LA    RE,2(RE)                                                         
         B     HK311                                                            
                                                                                
HK315    SR    RF,RF                                                            
         IC    RF,1(RE)            GET DISPLACEMENT INTO SORT RECORD            
         AR    RF,R7                                                            
                                                                                
         CLI   2(RF),C'M'          ROW CODE                                     
         BNE   HK340                                                            
HK330    CP    R1COL2,=P'0'                                                     
         BH    HK335                                                            
         ZAP   R1COL2,=P'0'                                                     
HK335    CP    R1COL4,=P'0'                                                     
         BH    HK340                                                            
         ZAP   R1COL4,=P'0'                                                     
                                                                                
HK340    CLI   QOPT7,C'D'                                                       
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
                                                                                
*        POSITION 1 - GROUP NUMBER FOR FILTER                                   
*        POSITION 2 - FILTER                                                    
FLTRTAB  DS    0C                                                               
         DC    CL1'0',C' '                                                      
FLTRLEN  EQU   *-FLTRTAB                                                        
         DC    CL1'1',C'A'                                                      
         DC    CL1'1',C'C'                                                      
         DC    CL1'1',C'D'                                                      
         DC    CL1'1',C'E'                                                      
         DC    CL1'1',C'G'                                                      
         DC    CL1'1',C'K'                                                      
         DC    CL1'1',C'L'                                                      
         DC    CL1'1',C'U'                                                      
                                                                                
         DC    CL1'2',C'B'                                                      
         DC    CL1'2',C'F'                                                      
         DC    CL1'2',C'J'                                                      
         DC    CL1'2',C'T'                                                      
         DC    CL1'2',C'W'                                                      
         DC    CL1'2',C'X'                                                      
                                                                                
         DC    CL1'3',C'M'                                                      
         DC    CL1'3',C'N'                                                      
         DC    CL1'3',C'O'                                                      
         DC    CL1'3',C'P'                                                      
         DC    CL1'3',C'Q'                                                      
         DC    CL1'3',C'R'                                                      
                                                                                
         DC    CL1'4',C'H'                                                      
         DC    CL1'4',C'I'                                                      
         DC    CL1'4',C'S'                                                      
         DC    CL1'4',C'V'                                                      
         DC    X'FF'                                                            
                                                                                
GRPTAB   DS    0C                                                               
         DC    CL1'0',CL36'NON-FILTERED'                                        
GRPLEN   EQU   *-GRPTAB                                                         
         DC    CL1'1',CL36'MARK MORRIS'                                         
         DC    CL1'2',CL36'SPECIALTY UNITS'                                     
         DC    CL1'3',CL36'ART D''ANGELO'                                       
         DC    CL1'4',CL36'OTHER OFFICES'                                       
         DC    X'FF'                                                            
                                                                                
REPTAB   DS    0F                                                               
         DC    AL1(1,R1ROW4-R1RECD)                                             
         DC    AL1(2,R1ROW3-R1RECD)                                             
         DC    AL1(3,R1ROW2-R1RECD)                                             
         DC    AL1(4,R1ROW2-R1RECD)                                             
*                                                                               
         DC    AL1(5,R1ROW4-R1RECD)                                             
         DC    AL1(6,R1ROW3-R1RECD)                                             
         DC    AL1(7,R1ROW2-R1RECD)                                             
         DC    AL1(8,R1ROW2-R1RECD)                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
                                                                                
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
R1LEN    EQU   *-R1RECD                                                         
                                                                                
                                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047APGHFIBSC 04/08/98'                                      
         END                                                                    
