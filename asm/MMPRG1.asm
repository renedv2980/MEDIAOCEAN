*          DATA SET MMPRG1     AT LEVEL 046 AS OF 01/18/89                      
*PHASE MMPRG1,*                                                                 
         TITLE 'PAYROLL PROGRAM'                                                
MMOS     START 0                                                                
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,**MMOS**,WRKAREA,RR=R5                                         
*                                                                               
         PRINT GEN                                                              
**************                                                                  
* OPEN FILES *                                                                  
**************                                                                  
         OPEN  (IN,(INPUT))                                                     
         OPEN  (OUT,(OUTPUT))                                                   
*******************                                                             
* PRINT HEADLINES *                                                             
*******************                                                             
         MVC   LINEOUT,HEAD1                                                    
         PUT   OUT,LINEOUT                                                      
*                                                                               
         MVC   LINEOUT,HEAD2                                                    
         PUT   OUT,LINEOUT                                                      
*                                                                               
         MVC   LINEOUT,HEAD3                                                    
         PUT   OUT,LINEOUT                                                      
*                                                                               
         MVC   LINEOUT,BLANKS            CLEAR PRINT AREA                       
         PUT   OUT,LINEOUT                                                      
********                                                                        
READCARD GET   IN,INREC                  READ CARD                              
********                                                                        
         MVC   PRTDEPT,DEPT               DEPARTMENT                            
         MVC   PRTNAME,NAME               NAME                                  
         MVC   PRTBD,BD                   BIRTHDAY                              
         EDIT  (C5,HOURS),(6,PRTHRS),2 HOURS WORKED                             
         EDIT  (C5,WAGE),(6,PRTWAGE),2 SALARY RATE                              
         PACK  PHOURS,HOURS                                                     
         PACK  PWAGE,WAGE                                                       
************************                                                        
* COMPUTE PAY (REG/OT) *                                                        
************************                                                        
         CP    PHOURS,PFORTY             IS THERE OT?                           
         BH    OVERTIME                  YES                                    
*                                                                               
         ZAP   POT,PZERO                 CLEAR REG AMOUNT                       
         ZAP   PREG,PHOURS               ADD HOURS TO REG AMOUNT                
         MP    PREG,PWAGE                MULTIPLE RATE                          
         B     OUTAMT                    SKIP TO GROSS                          
*                                                                               
OVERTIME SP    PHOURS,PFORTY             CAL OT HOURS                           
         ZAP   POT,PWAGE                                                        
         MP    POT,P2                    DOUBLETIME                             
         MP    POT,PHOURS                CAL OT AMOUNT                          
         ZAP   PREG,PFORTY                                                      
         MP    PREG,PWAGE                CAL REG AMOUNT                         
*********************                                                           
* OUTPUT REG AMOUNT *                                                           
*********************                                                           
OUTAMT   AP    TREG,PREG               ADD TO REG AMT SUBTOTALS                 
         ZAP   PGROSS,PREG               ADD REG TO GROSS                       
         AP    PGROSS,POT                ADD OT TO GROSS                        
         EDIT  (P6,PREG),(14,PRTREG),2,COMMAS=YES,FLOAT=$                       
*********************                                                           
* OUTPUT OT  AMOUNT *                                                           
*********************                                                           
         AP    TOT,POT                  ADD TO OT AMT TOTALS                    
         EDIT  (P6,POT),(14,PRTOT),2,COMMAS=YES,FLOAT=$                         
*****************                                                               
* CALCULATE TAX *                                                               
*****************                                                               
         ZAP   PTAX,PGROSS                                                      
         DP    PTAX,TAXRATE                                                     
         SRP   PTAX(7),64-1,5                                                   
         EDIT  (P7,PTAX),(14,PRTFED),2,COMMAS=YES,FLOAT=$                       
         AP    TTAX,PTAX(7)             ADD TO TAX TOTALS                       
*****************                                                               
* CALCULATE NET *            NET = GROSS - TAX                                  
*****************                                                               
         ZAP   PNET,PGROSS                                                      
         SP    PNET,PTAX(7)                                                     
         EDIT  (P6,PGROSS),(14,PRTGROSS),2,COMMAS=YES,FLOAT=$                   
         EDIT  (P6,PNET),(14,PRTNET),2,COMMAS=YES,FLOAT=$                       
         AP    TNET,PNET                ADD TO NET TOTALS                       
         AP    TGROSS,PGROSS            ADD TO GROSS TOTALS                     
**************                                                                  
* PRINT LINE *                                                                  
**************                                                                  
         PUT   OUT,LINEOUT                                                      
         B     READCARD                                                         
**************                                                                  
* EOF TOTALS *                                                                  
**************                                                                  
FINISH   MVC   LINEOUT,BLANKS                 CLEAR LINE                        
         PUT   OUT,LINEOUT                                                      
*                                                                               
         MVC   PRTLIT,TOTALS                  LITERAL 'TOTALS'                  
*                                                                               
         EDIT  (P6,TREG),(14,PRTREG),2,COMMAS=YES,FLOAT=$                       
*                                                                               
         EDIT  (P6,TOT),(14,PRTOT),2,COMMAS=YES,FLOAT=$                         
*                                                                               
         EDIT  (P6,TGROSS),(14,PRTGROSS),2,COMMAS=YES,FLOAT=$                   
*                                                                               
         EDIT  (P6,TTAX),(14,PRTTAX),2,COMMAS=YES,FLOAT=$                       
*                                                                               
         EDIT  (P6,TNET),(14,PRTNET),2,COMMAS=YES,FLOAT=$                       
*                                                                               
         PUT   OUT,LINEOUT                    PRINT TOTAL LINE                  
***************                                                                 
* CLOSE FILES *                                                                 
***************                                                                 
         CLOSE (IN)                                                             
         CLOSE (OUT)                                                            
         XBASE                                                                  
         EJECT                                                                  
*************                                                                   
* WORK AREA *                                                                   
*************                                                                   
WRKAREA  DS    15D                                                              
****************                                                                
* INPUT RECORD *                                                                
****************                                                                
INREC    DS    0CL80                     INPUT RECORD LAYOUT                    
DEPT     DS    CL10                      DEPARTMENT                             
NAME     DS    CL20                      LAST FIRST NAME                        
HOURS    DS    CL5                       HOURS WORKED                           
WAGE     DS    CL5                       HOURLY RATE                            
BD       DS    CL6                       BIRTHDAY(MMDDYY)                       
         DS    CL34                      FILLER                                 
*                                                                               
*****************                                                               
* OUTPUT RECORD *                                                               
*****************                                                               
LINEOUT  DS    0CL133                                                           
         DS    CL2                  0                                           
PRTNAME  DS    CL20                 2    NAME                                   
         DS    CL2                  22                                          
PRTDEPT  DS    CL10                 24   DEPARTMENT                             
         DS    CL1                  34                                          
PRTBD    DS    CL6                  35   BIRTHDATE                              
         DS    CL1                  41                                          
PRTHRS   DS    CL6                  42   HOURS                                  
         DS    CL1                  48                                          
PRTWAGE  DS    CL6                  49   WAGES                                  
         DS    CL1                  55                                          
PRTREG   DS    CL14                 56   REG PAY                                
         DS    CL1                  68                                          
PRTOT    DS    CL14                 69   OT PAY                                 
         DS    CL1                  81                                          
PRTGROSS DS    CL14                 82   GROSS PAY                              
         DS    CL1                  94                                          
PRTFED   DS    CL14                 95   FEDERAL INCOME TAX                     
         DS    CL1                 107                                          
PRTNET   DS    CL14                108   NET PAY                                
         DS    CL2                 120                                          
******************                                                              
* TOTAL HEADINGS *                                                              
******************                                                              
         ORG   LINEOUT                                                          
         DS    CL40                                                             
PRTLIT   DS    CL6                                                              
         DS    CL10                                                             
PRTTREG  DS    CL14                                                             
         DS    CL1                                                              
PRTTOT   DS    CL14                                                             
         DS    CL1                                                              
PRTTGROS DS    CL14                                                             
         DS    CL1                                                              
PRTTAX   DS    CL14                                                             
         DS    CL1                                                              
PRTTNET  DS    CL14                                                             
         DS    CL2                                                              
*                                                                               
         ORG                                                                    
**********                                                                      
* HEAD 1                                                                        
**********                                                                      
HEAD1    DC    CL133'                                     NEW YORK KNICX        
               K PAYROLL LISTING FOR 1989    '                                  
HEAD2    DC    CL133'     EMPLOYEE NAME      DEPARTMENT BIRTH  HOURS  WX        
               AGES  REGULAR PAY     OVERTIME PAY   GROSS PAY      FEDEX        
               RAL TAX       NET PAY    '                                       
HEAD3    DC    CL133'  --------------------  ---------- ------ ------ -X        
               ----- -------------- -------------- -------------- -----X        
               --------- -------------- '                                       
*                                                                               
***********************                                                         
* TEMPORARY VARIABLES *                                                         
***********************                                                         
PGROSS   DC    PL6'0'                    GROSS                                  
POT      DC    PL6'0'                    OVERTIME AMOUNT                        
PREG     DC    PL6'0'                    REGULAR AMOUNT                         
PNET     DC    PL6'0'                    NET                                    
PTAX     DC    PL8'0'                    TAX                                    
PWAGE    DC    PL3'0'                    WAGE                                   
PHOURS   DC    PL3'0'                    HOURS                                  
TTAX     DC    PL6'0'                    TAX TOTAL                              
TOT      DC    PL6'0'                    OT AMOUNT TOTAL                        
TREG     DC    PL6'0'                    REGULAR AMOUNT TOTAL                   
TNET     DC    PL6'0'                    NET AMOUNT TOTAL                       
TGROSS   DC    PL6'0'                    GROSS AMOUNT TOTAL                     
TOTAL    DS    CL6                       USE FOR LITERAL                        
DUB      DS    D                                                                
WORK     DS    CL64                                                             
*                                                                               
*************                                                                   
* CONSTANTS *                                                                   
*************                                                                   
PFORTY   DC    PL3'4000'                 REGULAR HOURS LIMIT                    
TAXRATE  DC    PL1'4'                    FEDERAL RATE                           
P2       DC    P'2'                      DOUBLETIME                             
BLANKS   DC    CL133' '                  BLANK LINE                             
PZERO    DC    P'0'                                                             
P4       DC    P'4'                                                             
TOTALS   DC    CL6'TOTALS'                                                      
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
IN       DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIPT,BLKSIZE=80,             X        
               LRECL=80,EODAD=FINISH,RECFM=FB                                   
*                                                                               
OUT      DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,LRECL=133,            X        
               RECFM=A,DEVD=PR                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046MMPRG1    01/18/89'                                      
         END                                                                    
