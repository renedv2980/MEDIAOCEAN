*          DATA SET ACREPZ102B AT LEVEL 065 AS OF 11/04/99                      
*PHASE ACZ102B,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'FIX TRANSACTION EL FOR MDCO'                                    
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
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PROCACC                                                *         
***********************************************************************         
*                                                                               
PACC     DS    0H                                                               
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
         MVC   BATCHMOS,TRNBTCH                                                 
         NI    BATCHMOS,X'9F'                                                   
         NI    BATCHMOS+1,X'0F'                                                 
         CLI   TRNBTCH+1,C'1'                                                   
         BNL   PTRN10                                                           
         MVI   BATCHMOS+1,X'0A'                                                 
         CLI   TRNBTCH+1,C'A'                                                   
         BE    PTRN10                                                           
         MVI   BATCHMOS+1,X'0B'                                                 
         CLI   TRNBTCH+1,C'B'                                                   
         BE    PTRN10                                                           
         MVI   BATCHMOS+1,X'0C'                                                 
*                                                                               
         USING TRSELD,R5                                                        
PTRN10   EQU   *                                                                
         CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R5),TRSELQ        X'60'                                        
         BE    PTRN20                                                           
         ZIC   RF,TRSLN                                                         
         B     PTRN10                                                           
*                                                                               
PTRN20   CLC   BATCHMOS,TRSPMOS                                                 
         BE    PTRNX                                                            
         L     R5,ADTRANS                                                       
*                                                                               
         BAS   RE,DUMP                                                          
*                                                                               
*        MVC   PID,ALPHAID                                                      
*        MVC   PACCT,TRNKACT                                                    
*        MVC   PKOFF,TRNKOFF                                                    
*        MVC   PTOFF,TRNOFFC                                                    
*                                                                               
*        MVC   TRNOFFC,TRNKOFF     IF DIFFERENT UPDATE ELEMENT                  
*                                                                               
*        MVC   PNTOFF,TRNOFFC                                                   
*        MVC   PCULA,TRNKULC                                                    
*        GOTO1 DATCON,DMCB,(1,TRNKDATE),(0,PDATE)                               
*        MVC   PREF,TRNKREF                                                     
*        EDIT  TRNTYPE,PTYPE                                                    
*                                                                               
*        GOTO1 ACREPORT                                                         
*                                                                               
*        CLI   RCWRITE,C'N'                                                     
*        BE    PTRNX                                                            
*        MVI   MODE,WRITRANS                                                    
PTRNX    XIT1                                                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*              ACCOUNT LAST                                           *         
***********************************************************************         
*                                                                               
ACCL     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*             RUN LAST                                                *         
***********************************************************************         
*                                                                               
RUNL     DS    0H                                                               
         B     XIT                                                              
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
TODAY2   DS    CL2                                                              
DMPSW    DS    CL1                                                              
BATCHMOS DS    CL2                                                              
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
PID      DS    CL2                 ALPHA ID                                     
         DS    CL3                                                              
PACCT    DS    CL12                ACCOUNT                                      
         DS    CL3                                                              
PKOFF    DS    CL2                 OFFICE IN THE KEY                            
         DS    CL8                                                              
PTOFF    DS    CL2                 OFFICE IN THE TRANSACTION                    
         DS    CL10                                                             
PNTOFF   DS    CL2                 NEW OFFICE IN THE TRANSACTION                
         DS    CL10                                                             
PCULA    DS    CL14                CONTRA ACCOUNT                               
         DS    CL6                                                              
PDATE    DS    CL6                 DATE                                         
         DS    CL4                                                              
PREF     DS    CL6                 REFERENCE NUMBER                             
         DS    CL4                                                              
PTYPE    DS    CL2                 TRANSACTION TYPE                             
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
**PAN#1  DC    CL21'065ACREPZ102B11/04/99'                                      
         END                                                                    
