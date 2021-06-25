*          DATA SET APGHFIBSA  AT LEVEL 007 AS OF 03/29/95                      
*PHASE ACHFIBSA,+0                                                              
         TITLE 'BACKER, ELEIMINATE NEGATIVE REVENUE PCTS'                       
ACHFIBSA CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R5                                                        
*                                                                               
HK01     CLI   QOPT5,C'D'                                                       
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
*                                                                               
HK10     LA    RE,REPTAB                                                        
HK11     CLI   0(RE),EOT           END OF TABLE?                                
         BE    XIT                                                              
         CLC   R1REPNO,0(RE)       MATCH ON REPORT NUMBER                       
         BE    HK15                                                             
         LA    RE,2(RE)                                                         
         B     HK11                                                             
*                                                                               
HK15     SR    RF,RF                                                            
         IC    RF,1(RE)            GET DISPLACEMENT INTO SORT RECORD            
         AR    RF,R5                                                            
         CLI   2(RF),C'M'          ROW CODE                                     
         BL    XIT                                                              
         LA    RF,16(RF)           BUMP TO NEXT ROW                             
         CLI   HOOKNUM,3                                                        
         BE    HK60                                                             
         CLI   HOOKNUM,4                                                        
         BE    HK60                                                             
*                                                                               
         CLC   FRSTPASS,R1REPNO                                                 
         BE    HK20                                                             
         MVC   FRSTPASS,R1REPNO    SAVE PREVIOUS REPORT NUMBER                  
         LA    R2,COLSAVE                                                       
         LA    R0,R1#BKT                                                        
         ZAP   0(8,R2),=P'0'       CLEAR                                        
         LA    R2,8(R2)            BUMP UP                                      
         BCT   R0,*-10                                                          
*                                                                               
HK20     CLC   2(3,RF),=C'620'                                                  
         BNE   HK30                                                             
         MVC   COLSAVE(R1#BKT*8),R1COL1  SAVE 12% OF REVENUE LINE               
         LA    RE,COLEQ93          EQUAL TO 1993                                
         CLI   HOOKNUM,1           WHICH COLUMNS TO CHECK                       
         BE    *+8                 NO                                           
         LA    RE,COLGT93          AFTER 1993                                   
         LA    RF,COLSAVE                                                       
         LA    R3,R1COL1                                                        
         LA    R0,R1#BKT           NUMBER OF COLUMNS                            
*                                                                               
HK22     CLI   0(RE),0             PROCESS OR NOT?                              
         BNE   *+14                                                             
         ZAP   0(8,RF),=P'0'       ZERO OUT                                     
         B     HK28                                                             
         CP    0(8,RF),=P'0'                                                    
         BNL   *-16                NOT NEGATIVE SO ZERO OUT                     
         ZAP   0(8,R3),=P'0'       ZERO OUT IN RECORD                           
*                                                                               
HK28     LA    R3,8(R3)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,HK22                                                          
         B     XIT                                                              
*                                                                               
HK30     MVI   REVFLAG,0                                                        
         CLC   2(3,RF),=C'650'                                                  
         BE    HK40                                                             
         MVI   REVFLAG,1           REVERSE OPERATION                            
         CLC   2(3,RF),=C'700'                                                  
         BE    HK40                                                             
         CLC   2(3,RF),=C'750'                                                  
         BNE   XIT                                                              
         CLI   15(RF),C'B'                                                      
         BE    XIT                                                              
*        BAS   RE,DUMP                                                          
*                                                                               
HK40     LA    R0,R1#BKT           NUMBER OF COLUMNS                            
         LA    R3,R1COL1                                                        
         LA    RF,COLSAVE                                                       
HK45     CLI   REVFLAG,0                                                        
         BNE   HK46                                                             
         SP    0(8,R3),0(8,RF)                                                  
         B     *+10                                                             
HK46     AP    0(8,R3),0(8,RF)                                                  
         LA    R3,8(R3)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,HK45                                                          
         B     XIT                                                              
*                                                                               
HK60     CLC   2(3,RF),=C'550'                                                  
         BE    HK65                                                             
*        CLC   CURRCON+1(2),=C'15'                                              
         CLC   CURRCON+1(3),=C'150'                                             
*        BE    *+10                                                             
*        CLC   CURRCON+1(3),=C'159'                                             
         BNE   XIT                                                              
HK65     ZAP   R1COL1,=P'0'                                                     
         ZAP   R1COL3,=P'0'                                                     
         ZAP   R1COL4,=P'0'                                                     
         ZAP   R1COL5,=P'0'                                                     
         ZAP   R1COL6,=P'0'                                                     
         ZAP   R1COL11,=P'0'                                                    
         CLI   HOOKNUM,4                                                        
         BNE   XIT                                                              
         ZAP   R1COL2,=P'0'                                                     
         ZAP   R1COL7,=P'0'                                                     
         ZAP   R1COL8,=P'0'                                                     
         ZAP   R1COL9,=P'0'                                                     
         ZAP   R1COL10,=P'0'                                                    
         ZAP   R1COL12,=P'0'                                                    
         B     XIT                                                              
         SPACE 3                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 3                                                                
DUMP     NTR1                                                                   
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECORD'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
         EJECT                                                                  
*        AL1(REPORT NUMBER)                                                     
*        AL1(DISPLACMENT TO SUPERLEDGER CODE)                                   
*                                                                               
REPTAB   DS    0F                                                               
         DC    AL1(5,R1ROW4-R1RECD)                                             
         DC    AL1(6,R1ROW3-R1RECD)                                             
         DC    AL1(7,R1ROW2-R1RECD)                                             
         DC    AL1(8,R1ROW1-R1RECD)                                             
*                                                                               
         DC    AL1(9,R1ROW4-R1RECD)                                             
         DC    AL1(10,R1ROW3-R1RECD)                                            
         DC    AL1(11,R1ROW2-R1RECD)                                            
         DC    AL1(12,R1ROW1-R1RECD)                                            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
*        AL1(NO=0,YES=1, WHICH COLUMNS TO PROCESS)                              
*OLEQ93  DC    AL1(1,0,1,1,1,1,0,0,0,0,1,0)                                     
COLEQ93  DC    AL1(1,1,1,1,1,1,1,1,1,1,1,1)                                     
COLGT93  DC    AL1(1,1,1,1,1,1,1,1,1,1,1,1)                                     
         SPACE 3                                                                
MYEND    DC    XL6'00'                                                          
MYWORK   DS    XL(L'WORK)                                                       
CURRATE  DS    PL8                                                              
CURHRS   DS    PL8                                                              
PACK16   DS    PL16                                                             
         ORG   PACK16                                                           
ANSWER   DS    PL8                                                              
LEFTOVER DS    PL8                                                              
FRSTPASS DC    XL1'00'                                                          
REVFLAG  DS    AL1                                                              
COLSAVE  DS    (R1#BKT)PL8                                                      
         EJECT                                                                  
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
         DS    XL2                                                              
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
R1#BKT   EQU   (*-R1COL1)/8                                                     
R1LEN    EQU   *-R1RECD                                                         
         EJECT                                                                  
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*        ACAPGDSECT                                                             
         PRINT OFF                                                              
       ++INCLUDE ACAPGDSECT                                                     
         PRINT ON                                                               
*        ACAPGEQU                                                               
*        PRINT OFF                                                              
*        INCLUDE ACAPGEQU                                                       
*        PRINT ON                                                               
*        ACAPGMAND                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGMAND                                                      
         PRINT ON                                                               
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007APGHFIBSA 03/29/95'                                      
         END                                                                    
