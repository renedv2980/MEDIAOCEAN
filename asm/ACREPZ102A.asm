*          DATA SET ACREPZ102A AT LEVEL 076 AS OF 09/06/00                      
*PHASE ACZ102A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'FIX TRANSACTION MISSING OFFICE CODE'                            
ACZ102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZ1**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZ1D,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     XIT                                                              
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
         ZAP   TOTCPYDR,=P'0'                                                   
         ZAP   TOTCPYCR,=P'0'                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PROCACC                                                *         
***********************************************************************         
*                                                                               
PACC     DS    0H                                                               
         ZAP   TOTDR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PROCTRNS                                               *         
***********************************************************************         
*                                                                               
         USING PLINED,R4                                                        
         USING TRNELD,R5                                                        
PTRN     DS    0H                                                               
         LA    R4,P                                                             
         L     R5,ADTRANS                                                       
         LR    R3,R5                                                            
         SH    R3,DATADISP                                                      
*                                                                               
         USING TRNRECD,R3                                                       
         CLC   TRNKULA(2),=C'SC'                                                
         BNE   PTRNX                                                            
         CLC   TRNOFFC,SPACES                                                   
         BH    PTRNX                                                            
         BAS   RE,DUMP                                                          
         MVC   CURACC,TRNKULA                                                   
*                                                                               
PTR20    LA    RE,TOTDR                                                         
         TM    TRNSTAT,X'80'                                                    
         BO    *+8                                                              
         LA    RE,TOTCR                                                         
         AP    0(L'TOTDR,RE),TRNAMNT                                            
         AP    8(L'TOTDR,RE),TRNAMNT                                            
*                                                                               
         MVC   PACCT,TRNKACT                                                    
         MVC   PTOFF,TRNOFFC                                                    
         CLC   PTOFF,SPACES                                                     
         BH    *+10                                                             
         MVC   PTOFF(3),=C'NIL'                                                 
         MVC   PSIGN,=C'==>'                                                    
*        CLI   QOPT2,C'F'                                                       
*        BNE   *+10                                                             
         MVC   TRNOFFC,=C'1 '                                                   
         BAS   RE,DUMP                                                          
         MVC   PNTOFF,TRNOFFC                                                   
         MVC   PCULA,TRNKULC                                                    
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(0,PDATE)                               
         MVC   PREF,TRNKREF                                                     
         EDIT  TRNTYPE,PTYPE                                                    
         LA    R1,PDEB                                                          
         TM    TRNSTAT,X'80'                                                    
         BO    *+8                                                              
         LA    R1,PCRD                                                          
         EDIT  (P6,TRNAMNT),(14,(R1)),2,MINUS=YES                               
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
*        CLI   TRNOFFC,X'40'                                                    
*        BH    PTRNX                                                            
         CLI   RCWRITE,C'N'                                                     
         BE    PTRNX                                                            
         MVI   MODE,WRITRANS                                                    
PTRNX    XIT1                                                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*              ACCOUNT LAST                                           *         
***********************************************************************         
         USING PLINED,R4                                                        
ACCL     DS    0H                                                               
         CP    TOTDR,=P'0'                                                      
         BNE   ACCL10                                                           
         CP    TOTCR,=P'0'                                                      
         BE    XIT                                                              
*                                                                               
ACCL10   LA    R4,P                                                             
         MVC   PACCT(9),=C'TOTAL FOR'                                           
         MVC   PACCT+11(14),CURACC                                              
         EDIT  (P8,TOTDR),(14,PDEB),2,MINUS=YES                                 
         EDIT  (P8,TOTCR),(14,PCRD),2,MINUS=YES                                 
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*             RUN LAST                                                *         
***********************************************************************         
*                                                                               
         USING PLINED,R4                                                        
RUNL     DS    0H                                                               
         GOTO1 ACREPORT                                                         
         LA    R4,P                                                             
         MVC   PACCT(17),=C'TOTAL FOR COMPANY'                                  
         EDIT  (P8,TOTCPYDR),(14,PDEB),2,MINUS=YES                              
         EDIT  (P8,TOTCPYCR),(14,PCRD),2,MINUS=YES                              
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              DUMP OUT RECORDS                                       *         
***********************************************************************         
*                                                                               
         USING TRNRECD,R3                                                       
DUMP     NTR1                                                                   
         CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         LA    R6,=C'GET'                                                       
         SR    R8,R8                                                            
         ICM   R8,3,TRNRLEN                                                     
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
XIT      XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         DC    X'FF'                                                            
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT '                                                    
*                                                                               
FLAG     DC    X'0'                                                             
FIRSTAC  EQU   X'80'                                                            
FIRSTTR  EQU   X'40'                                                            
*                                                                               
CHAREC   DC    PL6'0'                                                           
*                                                                               
ACDR     DC    PL6'0'                                                           
ACCR     DC    PL6'0'                                                           
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'5000'                                                        
*                                                                               
LONG     DS    CL1                                                              
TOTINT   DS    PL6                                                              
TOTADJ   DS    PL6                 TOTAL ADJ AMOUNT                             
ESTINT   DS    PL6                                                              
ESTADJ   DS    PL6                                                              
*                                                                               
TOTB     DC    PL4'0'                                                           
TYPTAB   DS    0XL16                                                            
         DC    AL1(1),CL15'ALLOCATED'                                           
         DC    AL1(2),CL15'CLIENT   '                                           
         DC    AL1(3),CL15'MANUAL   '                                           
         DC    AL1(4),CL15'ONE-LINE '                                           
         DC    AL1(5),CL15'% EST    '                                           
         DC    AL1(6),CL15'PROGRESS '                                           
         DC    AL1(7),CL15'SPECIAL  '                                           
         DC    AL1(8),CL15'TOTAL    '                                           
         DC    X'FF',CL15'UNKNOWN'                                              
         EJECT                                                                  
**********************************************************************          
* LITERAL                                                            *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* WORK DSECT                                                         *          
**********************************************************************          
         SPACE 1                                                                
ACZ1D    DSECT                                                                  
PARM     DS    6F                                                               
CURACC   DS    CL14                                                             
*                                                                               
TOTDR    DS    PL8                                                              
TOTCPYDR DS    PL8                                                              
*                                                                               
TOTCR    DS    PL8                                                              
TOTCPYCR DS    PL8                                                              
*                                                                               
TODAY2   DS    CL2                                                              
DMPSW    DS    CL1                                                              
ELIST    DS    3A                  HELLO PARM LIST                              
ELERR    DS    0XL1                HELLO ERROR RETURN BYTE                      
ELADDR   DS    A                   HELLO ELEMENT ADDRESS (GET)                  
ELADDR2  DS    A                   HELLO ELEMENT ADDRESS (ADD)                  
ELCODE   DS    CL1                                                              
ELEMENT  DS    XL255                                                            
IO       DS    CL2000                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT DSECT                                                        *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
PACCT    DS    CL12                ACCOUNT                                      
         DS    CL2                                                              
PCULA    DS    CL14                CONTRA ACCOUNT                               
         DS    CL4                                                              
PREF     DS    CL6                 REFERENCE NUMBER                             
         DS    CL3                                                              
PDATE    DS    CL6                 DATE                                         
         DS    CL1                                                              
PNTOFF   DS    CL2                 NEW OFFICE IN THE TRANSACTION                
PSIGN    DS    CL3                                                              
PTOFF    DS    CL2                 OFFICE IN THE TRANSACTION                    
         DS    CL3                                                              
PTYPE    DS    CL2                 TRANSACTION TYPE                             
         DS    CL2                                                              
PDEB     DS    CL14                                                             
         DS    CL2                                                              
PCRD     DS    CL14                                                             
         EJECT                                                                  
**********************************************************************          
* ++INCLUDES                                                         *          
**********************************************************************          
         SPACE 1                                                                
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076ACREPZ102A09/06/00'                                      
         END                                                                    
