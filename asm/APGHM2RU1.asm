*          DATA SET APGHM2RU1  AT LEVEL 013 AS OF 12/02/94                      
*PHASE ACHM2RU1,+0                                                              
         TITLE 'RUMN BUDGET PCT'                                                
ACHM2RU1 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**                                                       
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT RECORD                          
         USING R1RECD,R5                                                        
         CLI   HOOKNUM,1           BUILD TABLE                                  
         BE    BLDTAB                                                           
         CLI   HOOKNUM,2           SEARCH TABLE                                 
         BE    SRHTAB                                                           
         DC    H'00'                                                            
*                                                                               
         USING PRSND,R3                                                         
BLDTAB   CLI   FRSTPASS,0                                                       
         BNE   BLDTAB10                                                         
         LA    R3,PERSNTAB                                                      
         ST    R3,PTRNEXT                                                       
         MVI   FRSTPASS,1                                                       
*                                                                               
BLDTAB10 L     R2,COUNT                                                         
         L     R3,PTRNEXT                                                       
         MVC   PRSON,R1CDE1        SAVE OFF PERSON CODE                         
         MVC   PRSBUD,R1COL2       SAVE OFF PERSON TOTAL BUDGET                 
         AH    R2,=H'01'                                                        
         ST    R2,COUNT            COUNT NUMBER OF PEOPLE                       
         LA    R3,PRSNQ(R3)                                                     
         ST    R3,PTRNEXT                                                       
         CLI   QOPT5,C'D'                                                       
         BNE   XITNO                                                            
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'REC 01'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
         B     XITNO               ELIMINATE RECORD                             
*                                                                               
SRHTAB   CLI   FRSTPASS,1                                                       
         BH    SRHTAB10                                                         
         MVI   FRSTPASS,2                                                       
         GOTO1 XSORT,DMCB,PERSNTAB,COUNT,PRSNQ,6,0                              
*                                                                               
SRHTAB10 CLI   QOPT5,C'D'                                                       
         BNE   SRHTAB20                                                         
         LA    R0,R1LEN                                                         
         GOTO1 PRNTBL,DMCB,(6,=C'REC 02'),(R5),C'DUMP',(R0),=C'2D',    X        
               (C'P',PRINT)                                                     
*                                                                               
SRHTAB20 GOTO1 BINSRCH,DMCB,(2,R1CDE6),PERSNTAB,COUNT,PRSNQ,6,200               
         ICM   R3,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'00'                                                            
         ZAP   R1COL12,=P'0'                                                    
         MVC   TEMPPRSN,R1CDE7                                                  
         CLC   R1CDE7(6),0(R3)                                                  
         BNE   XIT                                                              
         ZAP   R1COL12,PRSBUD                                                   
         B     XIT                                                              
*                                                                               
XIT      SR    R1,R1                                                            
XITNO    LTR   R1,R1                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
FRSTPASS DC    AL1(0)                                                           
TEMPPRSN DS    CL6                                                              
PTRNEXT  DS    A                                                                
COUNT    DS    A                                                                
CURBUD   DS    PL8                                                              
PACK16   DS    PL16                                                             
         ORG   PACK16                                                           
ANSWER   DS    PL8                                                              
LEFTOVER DS    PL8                                                              
PERSNTAB DS    CL(200*14)                                                       
         EJECT                                                                  
PRSND    DSECT                                                                  
PRSON    DS    CL6                                                              
PRSBUD   DS    PL8                                                              
PRSNQ    EQU   *-PRSND                                                          
*                                                                               
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
         EJECT                                                                  
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*        ACAPGEQU                                                               
         PRINT OFF                                                              
       ++INCLUDE ACAPGEQU                                                       
         PRINT ON                                                               
*        ACAPGWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACAPGWORKD                                                     
         PRINT ON                                                               
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013APGHM2RU1 12/02/94'                                      
         END                                                                    
