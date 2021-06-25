*          DATA SET PRODMORT   AT LEVEL 011 AS OF 05/01/02                      
*PHASE MORTGAGE,*                                                               
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
         PRINT NOGEN                                                            
         TITLE 'MORTGAGE PAYMENT TRANSACTION REPORT'                            
MORTGAGE CSECT                                                                  
         NBASE 0,**MORTGAGE,WORK                                                
         BAS   R2,HEADER                                                        
         SPACE                                                                  
READ     GOTO1 =V(CARDS),PARAMS,CARD,=C'RE00'                                   
         CLC   CARD(2),=C'/*'                                                   
         BNE   TESTNUM                                                          
         B     SUMMARY                                                          
         EJECT                                                                  
TESTNUM  EQU   *              TEST FOR NON-NUMERIC AND ZERO INPUTS              
         MVC   PACCT(6),CACCT                                                   
         SPACE                                                                  
ACCTTEST CLC   CACCT,ZERO     TEST ACCOUNT NUMBER                               
         BH    PRINTEST       OK GO TO NEXT TEST                                
         BE    *+14           BRANCH AROUND NON-NUMERIC ROUTINE                 
         MVC   MSG,ACCTNUM                                                      
         B     PRT                                                              
         MVC   MSG,ACCTZER    ZERO ROUTINE                                      
         B     PRT                                                              
         SPACE                                                                  
PRINTEST CLC   COPRIN,ZERO    TEST PRINCIPAL                                    
         BH    INTTEST                                                          
         BE    *+14                                                             
         MVC   MSG,PRINNUM                                                      
         B     PRT                                                              
         MVC   MSG,PRINZER                                                      
         B     PRT                                                              
         SPACE                                                                  
INTTEST  CLC   CINT,ZERO      TEST INTEREST RATE                                
         BH    MPAYTEST                                                         
         BE    *+14                                                             
         MVC   MSG,INTNUM                                                       
         B     PRT                                                              
         MVC   MSG,INTZER                                                       
         B     PRT                                                              
         SPACE                                                                  
MPAYTEST CLC   CMPAY,ZERO     TEST MONTHLY PAYMENT                              
         BH    CONBIN                                                           
         BE    *+14                                                             
         MVC   MSG,MPAYNUM                                                      
         B     PRT                                                              
         MVC   MSG,MPAYZER                                                      
         B     PRT                                                              
         EJECT                                                                  
CONBIN   EQU   *              CONVERT CARD FIELDS TO BINARY                     
         MVC   PRIN,COPRIN    MOVE PRIN,INT,AND MPAY TO WORK AREAS              
         MVC   INT,CINT                                                         
         MVC   MPAY,CMPAY                                                       
         SPACE                                                                  
         MVZ   PRIN+6(1),PLUS                                                   
         PACK  INPRIN,PRIN                                                      
         CVB   R5,INPRIN      OLD PRIN PLACED IN R5                             
         SPACE                                                                  
         MVZ   INT+3(1),PLUS                                                    
         PACK  ININT,INT                                                        
         CVB   R6,ININT       INT PLACED IN R6                                  
         SPACE                                                                  
         MVZ   MPAY+4(1),PLUS                                                   
         PACK  INMPAY,MPAY                                                      
         CVB   R7,INMPAY      MPAY PLACED IN R7                                 
         SPACE                                                                  
         LR    R8,R5          PLACE OLD PRIN IN 2 REGS FOR CALC                 
         EJECT                                                                  
CALC     EQU   *              CALCULATE MONTHLY INTEREST                        
         MR    R4,R6          COMPUTE ANNUAL INTEREST                           
         L     R9,DIV        LOAD DIVISOR OF 120,000 INTO R9                    
         A     R5,ROUNDER    ADD ROUNDER OF 5,000 TO R5                         
         DR    R4,R9          MONTHLY INTEREST NOW IN R5                        
         CR    R5,R7          COMPARE MONTHLY INT W ACTUAL PAYMENT              
         BNH   FINCALC       IF LESS THAN OR = GO TO FINISH CALC                
         MVC   MSG,LOWPAY                                                       
         B     PRT                                                              
         SPACE                                                                  
FINCALC  EQU   *    FIND NEW PRIN AND AMT APPL TO PRIN                          
         SR    R7,R5          R7 NOW CONTAINS AMT APPL TO PRIN                  
         SR    R8,R7          SUBTRACT TO FIND NEW PRIN                         
         BAS   R3,SUBTOTAL                                                      
         SPACE                                                                  
CONZONE  EQU   *             PREPARE PRINT LINE                                 
         SPACE                                                                  
         CVD   R5,OUTDUE                                                        
         UNPK  PINT(5),OUTDUE                                                   
         MVZ   PINT+4(1),PINT+3                                                 
         SPACE                                                                  
         CVD   R7,OUTAPPL                                                       
         UNPK  PAPPL(5),OUTAPPL                                                 
         MVZ   PAPPL+4(1),PAPPL+3                                               
         SPACE                                                                  
         CVD   R8,OUTPRIN                                                       
         UNPK  PNPRIN(7),OUTPRIN                                                
         MVZ   PNPRIN+6(1),PNPRIN+5                                             
         SPACE                                                                  
         MVC   POPRIN(7),COPRIN                                                 
         MVC   PMPAY(5),CMPAY                                                   
         SPACE                                                                  
PRT      GOTO1 =V(PRINT),PARAMS,PLINE,=C'BL01'                                  
         MVI   PLINE,C' '                                                       
         MVC   PLINE+1(132),PLINE                                               
         B     READ                                                             
         EJECT                                                                  
SUMMARY  LM    R3,R5,ACCUM                                                      
         LR    R6,R3         MOVE MONTHLY INTEREST TO R6                        
         AR    R6,R4         ADD TO FIND MONTHLY PAYMENTS                       
         LR    R7,R5         MOVE NEW PRIN TO R7                                
         AR    R7,R4         ADD TO FIND SUM OF OLD PRIN                        
         BC    14,*+14       BRANCH AROUND OVERFLOW DEFENSE                     
         MVC   MSG,OVERSUM                                                      
         B     OVERFLOW      ABORT JOB                                          
         SPACE                                                                  
         BAS   R2,SHEADER                                                       
         LR    R8,R7                                                            
         BAS   R9,CONEDIT                                                       
         MVC   LABEL,NP                                                         
         MVC   FIGURE,AREA                                                      
         BAS   R2,SPRT                                                          
         LR    R8,R5                                                            
         BAS   R9,CONEDIT                                                       
         MVC   LABEL,OP                                                         
         MVC   FIGURE,AREA                                                      
         BAS   R2,SPRT                                                          
         LR    R8,R6                                                            
         BAS   R9,CONEDIT                                                       
         MVC   LABEL,MPY                                                        
         MVC   FIGURE,AREA                                                      
         BAS   R2,SPRT                                                          
         LR    R8,R3                                                            
         BAS   R9,CONEDIT                                                       
         MVC   LABEL,MI                                                         
         MVC   FIGURE,AREA                                                      
         BAS   R2,SPRT                                                          
         LR    R8,R4                                                            
         BAS   R9,CONEDIT                                                       
         MVC   LABEL,AAP                                                        
         MVC   FIGURE,AREA                                                      
         BAS   R2,SPRT                                                          
ENDOFJOB EOJ                                                                    
         EJECT                                                                  
HEADER   GOTO1 =V(PRINT),PARAMS,PLINE,=C'BC01'                                  
         SPACE                                                                  
         MVC   PLINE+48(35),=C'MORTGAGE PAYMENT TRANSACTION REPORT'             
         GOTO1 =V(PRINT),PARAMS,PLINE,=C'BL01'                                  
         MVI   PLINE+48,X'60'                                                   
         MVC   PLINE+49(34),PLINE+48                                            
         GOTO1 =V(PRINT),PARAMS,PLINE,=C'BL02'                                  
         SPACE                                                                  
         MVI   PLINE,C' '                                                       
         MVC   PLINE+1(132),PLINE                                               
         MVC   PACCT,TOPCOL1                                                    
         MVC   POPRIN,TOPCOL2                                                   
         MVC   PNPRIN,TOPCOL3                                                   
         MVC   PMPAY,TOPCOL4                                                    
         MVC   PINT,TOPCOL5                                                     
         MVC   PAPPL,TOPCOL6                                                    
         GOTO1 =V(PRINT),PARAMS,PLINE,=C'BL01'                                  
         MVC   PACCT,BOTCOL1                                                    
         MVC   POPRIN,BOTCOL2                                                   
         MVC   PNPRIN,BOTCOL3                                                   
         MVC   PMPAY,BOTCOL4                                                    
         MVC   PINT,BOTCOL5                                                     
         MVC   PAPPL,BOTCOL6                                                    
         GOTO1 =V(PRINT),PARAMS,PLINE,=C'BL02'                                  
         SPACE                                                                  
         MVI   PLINE,C' '                                                       
         MVC   PLINE+1(132),PLINE                                               
         BR    R2                                                               
         SPACE                                                                  
SUBTOTAL LR    R6,R5         MOVE MONTHLY INTEREST TO R6                        
         STM   R6,R8,SAVEREG                                                    
         A     R6,ACCUM      UPDATE MONTHLY INTERST                             
         A     R7,ACCUM+4    UPDATE AMT APPLD TO PRIN                           
         A     R8,ACCUM+8    UPDATE NEW PRIN                                    
         BC    14,*+14       BRANCH AROUND OVERFLOW DEFENSE                     
         MVC   MSG,OVERSUB   SET OVERFLOW MESSAGE                               
         B     OVERFLOW      ABORT JOB                                          
         STM   R6,R8,ACCUM   STORE UPDATED SUB-TOTALS                           
         LM    R6,R8,SAVEREG                                                    
         BR    R3            RETURN TO CONZONE                                  
         SPACE                                                                  
OVERFLOW GOTO1 =V(PRINT),PARAMS,PLINE,=C'BL01'                                  
         B     ENDOFJOB                                                         
         EJECT                                                                  
CONEDIT  CVD   R8,DUB                                                           
         ZAP   TEMP,DUB                                                         
         MVC   AREA,PATTRN                                                      
         ED    AREA,TEMP                                                        
         BR    R9                                                               
SHEADER  GOTO1 =V(PRINT),PARAMS,SLINE,=C'BC01'                                  
         MVC   SLINE+60(14),=C'SUMMARY TOTALS'                                  
         GOTO1 =V(PRINT),PARAMS,SLINE,=C'BL01'                                  
         MVI   SLINE+60,X'60'                                                   
         MVC   SLINE+61(13),SLINE+60                                            
         GOTO1 =V(PRINT),PARAMS,SLINE,=C'BL02'                                  
         SPACE                                                                  
         MVI   SLINE,C' '                                                       
         MVC   SLINE+1(132),SLINE                                               
         MVC   SHEAD1,CAT                                                       
         MVC   SHEAD2,TOT                                                       
         GOTO1 =V(PRINT),PARAMS,SLINE,=C'BL02'                                  
         BR    R2                                                               
SPRT     GOTO1 =V(PRINT),PARAMS,SLINE,=C'BL01'                                  
         MVI   SLINE,C' '                                                       
         MVC   SLINE+1(132),SLINE                                               
         BR    R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
CARD     DS    0CL80                                                            
CACCT    DS    CL6                                                              
COPRIN   DS    CL7                                                              
CINT     DS    CL4                                                              
CMPAY    DS    CL5                                                              
         DS    CL58                                                             
*                                                                               
PLINE    DC    CL133' '                                                         
         ORG   PLINE                                                            
         DS    CL15                                                             
PACCT    DS    CL8                                                              
         DS    CL5                                                              
POPRIN   DS    CL9                                                              
         DS    CL5                                                              
PNPRIN   DS    CL9                                                              
         DS    CL5                                                              
PMPAY    DS    CL7                                                              
         DS    CL5                                                              
PINT     DS    CL8                                                              
         DS    CL5                                                              
PAPPL    DS    CL15                                                             
         ORG                                                                    
         ORG   POPRIN                                                           
MSG      DS    CL48                                                             
         ORG                                                                    
*                                                                               
*                                                                               
SLINE    DC    CL133' '                                                         
         ORG   SLINE                                                            
         DS    CL20                                                             
LABEL    DS    CL27                                                             
         DS    CL23                                                             
FIGURE   DS    CL13                                                             
         ORG                                                                    
         ORG   LABEL                                                            
SHEAD1   DS    CL8                                                              
         ORG                                                                    
         ORG   FIGURE+4                                                         
SHEAD2   DS    CL6                                                              
         ORG                                                                    
*                                                                               
ZERO     DC    7X'F0'                                                           
PLUS     DC    X'C0'                                                            
*                                                                               
PRIN     DS    CL7                                                              
INT      DS    CL4                                                              
MPAY     DS    CL5                                                              
*                                                                               
TEMP     DS    PL6                                                              
AREA     DS    CL13                                                             
PATTRN   DC    X'402020202020202020214B2020'                                    
*                                                                               
         DS    0D                                                               
INPRIN   DS    D                                                                
ININT    DS    D                                                                
INMPAY   DS    D                                                                
OUTDUE   DS    D                                                                
OUTAPPL  DS    D                                                                
OUTPRIN  DS    D                                                                
DUB      DS    D                                                                
*                                                                               
WORK     DS    100D                                                             
*                                                                               
PARAMS   DS    6F                                                               
DIV      DC    F'120000'     DIVIDES BY 12 AND ADJUSTS FOR INT PERCENT          
ROUNDER  DC    F'5000'       ROUNDS ANNUAL INTEREST BEFORE DIVISION             
ACCUM    DS    3F                                                               
SAVEREG  DS    3F                                                               
*                                                                               
ACCTNUM  DC    CL48'ACCOUNT NUMBER NOT NUMERIC'                                 
ACCTZER  DC    CL48'ACCOUNT NUMBER ALL ZEROS'                                   
PRINNUM  DC    CL48'PRINCIPAL NOT NUMERIC'                                      
PRINZER  DC    CL48'PRINCIPAL IS ZERO - PLEASE RECHECK'                         
INTNUM   DC    CL48'INTEREST RATE NOT NUMERIC'                                  
INTZER   DC    CL48'INTEREST RATE IS ZERO'                                      
MPAYNUM  DC    CL48'MONTHLY PAYMENT NOT NUMERIC'                                
MPAYZER  DC    CL48'**MONTHLY PAYMENT IS ZERO - CONTACT CUSTOMER**'             
LOWPAY   DC    CL48'**MONTHLY PAYMENT TOO LOW - CONTACT CUSTOMER**'             
OVERSUB  DC    CL48'OVERFLOW OCCURRED IN SUBTOTAL--JOB ABORTED'                 
OVERSUM  DC    CL48'OVERFLOW OCCURRED IN SUMMARY--JOB ABORTED'                  
NP       DC    CL27'NEW PRINCIPAL'                                              
OP       DC    CL27'OLD PRINCIPAL'                                              
MPY      DC    CL27'MONTHLY PAYMENTS'                                           
MI       DC    CL27'MONTHLY INTEREST'                                           
AAP      DC    CL27'AMOUNT APPLIED TO PRINCIPAL'                                
*                                                                               
TOPCOL1  DC    CL8'ACCOUNT'                                                     
TOPCOL2  DC    CL9'OLD'                                                         
TOPCOL3  DC    CL9'NEW'                                                         
TOPCOL4  DC    CL7'MONTHLY'                                                     
TOPCOL5  DC    CL8'MONTHLY'                                                     
TOPCOL6  DC    CL15'AMOUNT'                                                     
*                                                                               
BOTCOL1  DC    CL8'NUMBER'                                                      
BOTCOL2  DC    CL9'PRINCIPAL'                                                   
BOTCOL3  DC    CL9'PRINCIPAL'                                                   
BOTCOL4  DC    CL7'PAYMENT'                                                     
BOTCOL5  DC    CL8'INTEREST'                                                    
BOTCOL6  DC    CL15'APPLIED TO PRIN'                                            
TOT      DC    CL6'TOTALS'                                                      
CAT      DC    CL8'CATEGORY'                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PRODMORT  05/01/02'                                      
         END                                                                    
