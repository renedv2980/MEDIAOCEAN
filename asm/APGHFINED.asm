*          DATA SET APGHFINED  AT LEVEL 002 AS OF 12/29/99                      
*PHASE ACHFINED,+0                                                              
         TITLE 'BATES CLIENT WARNER LAMBERT'                                    
ACHFINED CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
*---------------------------------------------------------------------*         
*        MODE PUTHOOK=1 - BEFORE SORT                                 *         
*---------------------------------------------------------------------*         
         USING HOOK1D,R5                                                        
         CLI   QOPT6,C'1'          DUMP RECORD BEFORE CHANGES                   
         BNE   HK010                                                            
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK010    DS    0H                                                               
         CLC   CURRCON+1(2),=C'12'                                              
         BE    HK020                                                            
         MVC   R1CODE1(2),CURRCON+3      IF CUL=16 GET OFFICE FROM              
         CLC   CURRCON+1(2),=C'16'       CA POSITION 1                          
         BE    *+10                      ELSE CUL=13,14,15 AND OFFICE           
         MVC   R1CODE1(2),CURRCON+4      IS FROM CA POSITION 2                  
                                                                                
HK020    DS    0H                                                               
         CLI   R1REPNO,1                                                        
         BNE   HK030                                                            
         CLC   R1CODE1(2),=C'UA'                                                
         BNE   XITNO                                                            
         B     HK099                                                            
                                                                                
HK030    DS    0H                                                               
         CLI   R1REPNO,2                                                        
         BNE   HK040                                                            
         CLC   R1CODE1(2),=C'UB'                                                
         BNE   XITNO                                                            
         B     HK099                                                            
                                                                                
HK040    DS    0H                                                               
         CLI   R1REPNO,3                                                        
         BNE   HK040                                                            
         CLC   R1CODE1(2),=C'UA'                                                
         BE    HK099                                                            
         CLC   R1CODE1(2),=C'UB'                                                
         BE    HK099                                                            
         B     XITNO                                                            
                                                                                
HK099    DS    0H                                                               
         CLI   QOPT7,C'1'                DUMP AFTER RECORD                      
         BNE   XIT                                                              
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD1A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HOOK MODE SORTHOOK=2                                         *         
*        DO NOT NEED TO CHECK REPORT TABLE - SPEC ONLY COMES HERE     *         
*        FROM REPORTS ACTUALLY DOING REPORTING OFFICE FEATURE         *         
*---------------------------------------------------------------------*         
         USING HOOK2D,R5                                                        
HK200    CLI   HOOKNUM,2                                                        
         BNE   XIT                                                              
                                                                                
         CLI   QOPT6,C'2'                DUMP BEFORE RECORD                     
         BNE   HK205                                                            
         LA    R0,R2LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2B'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
                                                                                
HK205    DS    0H                                                               
                                                                                
HK230    CLI   QOPT7,C'2'                DUMP AFTER RECORD                      
         BNE   XIT                                                              
         LA    R0,R2LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'RECD2A'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
XIT      SR    RC,RC                                                            
XITNO    LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*        LITERAL POOL                                                           
*----------------------------------------------------------------*              
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
*        RECORD DSECT FOR HOOK MODE 1                                           
*----------------------------------------------------------------*              
HOOK1D   DSECT                                                                  
R1REPNO  DS    XL1                                                              
         DS    XL17                                                             
R1ROW1   DS    XL2                                                              
R1CODE1  DS    XL14                                                             
R1ROW2   DS    XL2                                                              
R1CODE2  DS    XL14                                                             
R1ROW3   DS    XL2                                                              
R1CODE3  DS    XL14                                                             
R1ROW4   DS    XL2                                                              
R1CODE4  DS    XL14                                                             
R1ROW5   DS    XL2                                                              
R1CODE5  DS    XL14                                                             
R1ROW6   DS    XL2                                                              
R1CODE6  DS    XL14                                                             
R1ROW7   DS    XL2                                                              
R1CODE7  DS    XL14                                                             
R1ROW8   DS    XL2                                                              
R1CODE8  DS    XL14                                                             
         DS    XL32                                                             
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
*                                                                               
R1NAME1  DS    CL36                                                             
R1NAME2  DS    CL36                                                             
R1NAME3  DS    CL36                                                             
R1NAME4  DS    CL36                                                             
R1NAME5  DS    CL36                                                             
R1NAME6  DS    CL36                                                             
R1NAME7  DS    CL36                                                             
R1NAME8  DS    CL36                                                             
*                                                                               
R1LEN    EQU   *-HOOK1D                                                         
         EJECT                                                                  
*----------------------------------------------------------------*              
*        RECORD DSECT FOR HOOK MODE 2                                           
*----------------------------------------------------------------*              
HOOK2D   DSECT                                                                  
R2ROW1   DS    XL2                                                              
R2CODE1  DS    XL14                                                             
R2ROW2   DS    XL2                                                              
R2CODE2  DS    XL14                                                             
R2ROW3   DS    XL2                                                              
R2CODE3  DS    XL14                                                             
R2ROW4   DS    XL2                                                              
R2CODE4  DS    XL14                                                             
R2ROW5   DS    XL2                                                              
R2CODE5  DS    XL14                                                             
R2ROW6   DS    XL2                                                              
R2CODE6  DS    XL14                                                             
R2ROW7   DS    XL2                                                              
R2CODE7  DS    XL14                                                             
R2REPNO  DS    XL1                                                              
R2REPCP  DS    XL1                                                              
R2TYPE   DS    XL2                                                              
*                                                                               
R2NAME1  DS    CL36                                                             
R2NAME2  DS    CL36                                                             
R2NAME3  DS    CL36                                                             
R2NAME4  DS    CL36                                                             
R2NAME5  DS    CL36                                                             
R2NAME6  DS    CL36                                                             
R2NAME7  DS    CL36                                                             
*                                                                               
R2COL1   DS    PL8                                                              
R2COL2   DS    PL8                                                              
R2COL3   DS    PL8                                                              
R2COL4   DS    PL8                                                              
R2COL5   DS    PL8                                                              
R2COL6   DS    PL8                                                              
R2COL7   DS    PL8                                                              
R2COL8   DS    PL8                                                              
R2COL9   DS    PL8                                                              
R2LEN    EQU   *-HOOK2D                                                         
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
**PAN#1  DC    CL21'002APGHFINED 12/29/99'                                      
         END                                                                    
