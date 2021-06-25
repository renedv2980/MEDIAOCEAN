*          DATA SET APGHFIHK1  AT LEVEL 013 AS OF 07/16/98                      
*PHASE ACHFIHK1,+0                                                              
         TITLE 'BELL ATLANTIC RECONCILLIATION'                                  
ACHFIHK1 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8,RR=R5                                              
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         ST    R5,HKRELO                                                        
                                                                                
*---------------------------------------------------------------------*         
*        REPORT NEEDS TO EITHER EXCLUDE OR INLCUDE ONLY               *         
*        NEW BUSINESS, PROBONO, AND HOUSE                             *         
*---------------------------------------------------------------------*         
HOOKSORT DS    0H                                                               
         CLI   QOPT4,C' '                                                       
         BE    XIT                                                              
         CLI   QOPT4,C'A'                                                       
         BE    XIT                                                              
                                                                                
         CLI   HOOKNUM,1              MODE=PUTHOOK                              
         BNE   HK200                                                            
                                                                                
         USING HOOK1D,R7                                                        
         L     R7,HOOKAREC            ADDR OF SORT RECORD                       
                                                                                
         CLI   QOPT6,C'1'             DUMP RECORD BEFORE (MODE 1)               
         BNE   HK110                                                            
         LA    R0,H1LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC1B'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
                                                                                
HK110    DS    0H                                                               
                                                                                
HK190    CLI   QOPT7,C'1'             DUMP RECORD AFTER (MODE 1)                
         BNE   XIT                                                              
         LA    R0,H1LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC1A'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PROCESS SORT RECORD                                          *         
*---------------------------------------------------------------------*         
HK200    DS    0H                                                               
         CLI   HOOKNUM,2              MODE=SORTHOOK                             
         BNE   XIT                                                              
                                                                                
         USING HOOK2D,R7                                                        
         L     R7,HOOKAREC            ADDR OF SORT RECORD                       
                                                                                
         CLI   QOPT6,C'2'             DUMP RECORD BEFORE (MODE 2)               
         BNE   HK210                                                            
         LA    R0,H2LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC2B'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
                                                                                
HK210    DS    0H                                                               
         USING RSTELD,R2                                                        
         L     R2,ADACCSTA                                                      
         CLI   QOPT4,C'O'                                                       
         BE    HK240                                                            
         CLI   QOPT4,C'E'                                                       
         BE    HK245                                                            
         B     XIT                                                              
HK240    TM    RSTSTAT5,X'C8'                                                   
         BZ    HK250                                                            
         B     HK290                                                            
HK245    TM    RSTSTAT5,X'C8'                                                   
         BNZ   XITNO                                                            
                                                                                
HK250    L     R2,ADLVCSTA                                                      
         CLI   QOPT4,C'O'                                                       
         BE    HK252                                                            
         CLI   QOPT4,C'E'                                                       
         BE    HK255                                                            
         B     XIT                                                              
HK252    TM    RSTSTAT5,X'C8'                                                   
         BZ    XITNO                                                            
         B     HK290                                                            
HK255    TM    RSTSTAT5,X'C8'                                                   
         BNZ   XITNO                                                            
                                                                                
HK290    CLI   QOPT7,C'2'             DUMP RECORD AFTER (MODE 2)                
         BNE   XIT                                                              
         LA    R0,H2LEN                                                         
         GOTO1 PRNTBL,DMCB,(5,=C'REC2A'),(R7),C'DUMP',(R0),=C'2D',     X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
                                                                                
*        PROGRAM EXITS                                                          
                                                                                
XIT      SR    RC,RC                    KEEP RECORD                             
XITNO    LTR   RC,RC                    DROP RECORD                             
         XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        DEFINE CONSTANTS                                                       
*--------------------------------------------------------------------*          
AHOOKIO  DC    A(HOOKIO)                                                        
HKRELO   DS    F                                                                
HOOKSW   DC    C'N'                     BEEN HERE BEFORE?                       
                                                                                
         LTORG                                                                  
                                                                                
         DS    0F                                                               
HOOKIO   DS    CL2000                                                           
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        MODE PUTHOOK RECORD (1)                                                
*-------------------------------------------------------------------*           
HOOK1D   DSECT                                                                  
H1HEADER DS    0XL18                                                            
H1REPNO  DS    XL1                                                              
H1REPCPY DS    XL1                                                              
         DS    XL16                                                             
H1ROW1   DS    XL2                                                              
H1CODE1  DS    XL14                                                             
H1ROW2   DS    XL2                                                              
H1CODE2  DS    XL14                                                             
H1ROW3   DS    XL2                                                              
H1CODE3  DS    XL14                                                             
H1ROW4   DS    XL2                                                              
H1CODE4  DS    XL14                                                             
*                                                                               
HZEROS   DS    CL96                                                             
*                                                                               
H1COL1   DS    PL8                                                              
H1COL2   DS    PL8                                                              
H1COL3   DS    PL8                                                              
H1COL4   DS    PL8                                                              
H1COL5   DS    PL8                                                              
H1COL6   DS    PL8                                                              
H1COL7   DS    PL8                                                              
H1COL8   DS    PL8                                                              
H1COL9   DS    PL8                                                              
H1COL10  DS    PL8                                                              
H1COL11  DS    PL8                                                              
H1COL12  DS    PL8                                                              
H1COL13  DS    PL8                                                              
H1COL14  DS    PL8                                                              
*                                                                               
HZEROS2  DS    XL144                                                            
*                                                                               
H1NAME1  DS    CL36                                                             
H1NAME2  DS    CL36                                                             
H1NAME3  DS    CL36                                                             
H1NAME4  DS    CL36                                                             
*                                                                               
H1LEN    EQU   *-HOOK1D                                                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        MODE SORTHOOK RECORD (2)                                               
*-------------------------------------------------------------------*           
HOOK2D   DSECT                                                                  
H2ROW1   DS    XL2                                                              
H2NSEQ1  DS    XL36                                                             
H2CODE1  DS    XL14                                                             
H2ROW2   DS    XL2                                                              
H2NSEQ2  DS    XL36                                                             
H2CODE2  DS    XL14                                                             
H2ROW3   DS    XL2                                                              
H2NSEQ3  DS    XL36                                                             
H2CODE3  DS    XL14                                                             
H2ROW4   DS    XL2                                                              
H2NSEQ4  DS    XL36                                                             
H2CODE4  DS    XL14                                                             
H2REPNO  DS    XL1                                                              
H2REPCP  DS    XL1                                                              
H2TYPE   DS    XL2                                                              
*                                                                               
H2NAME1  DS    CL36                                                             
H2NAME2  DS    CL36                                                             
H2NAME3  DS    CL36                                                             
H2NAME4  DS    CL36                                                             
*                                                                               
H2COL1   DS    PL8                                                              
H2COL2   DS    PL8                                                              
H2COL3   DS    PL8                                                              
H2COL4   DS    PL8                                                              
H2COL5   DS    PL8                                                              
H2COL6   DS    PL8                                                              
H2COL7   DS    PL8                                                              
H2COL8   DS    PL8                                                              
H2COL9   DS    PL8                                                              
H2COL10  DS    PL8                                                              
H2COL11  DS    PL8                                                              
H2COL12  DS    PL8                                                              
H2COL13  DS    PL8                                                              
H2COL14  DS    PL8                                                              
H2LEN    EQU   *-HOOK2D                                                         
         EJECT                                                                  
                                                                                
*        ACAPGGEND                                                              
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACAPGGEND                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013APGHFIHK1 07/16/98'                                      
         END                                                                    
