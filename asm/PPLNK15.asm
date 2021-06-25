*          DATA SET PPLNK15    AT LEVEL 035 AS OF 10/14/20                      
*PHASE T41415A                                                                  
PPLNK15  TITLE '- PPLNK15 - CLEAR FOR PAYMENT REQUEST'                          
***********************************************************************         
* PROGRAM INTERFACE BLOCK                                             *         
***********************************************************************         
         SPACE 2                                                                
SVRDEF   CSECT                                                                  
         DC    (RSVRDEFL)X'00'                                                  
                                                                                
         ORG   SVRDEF                                                           
         DC    C'*SERVER**SERVER**SERVER**SERVER*'                              
         DC    AL2(0)              NO CODE                                      
         DC    AL2(0)              NO FILE LIST                                 
         DC    AL2(0)              NO FACILITIES LIST                           
         DC    AL2(REQUEST-SVRDEF) REQUEST MAP                                  
         DC    AL2(IBLOCK-SVRDEF)  PROGRAM INTERFACE BLOCK                      
         ORG   SVRDEF+(RSVRSYSC-RSVRDEFD)                                       
         DC    C'PP'               SYSTEM                                       
         DC    C'PY'               PROGRAM                                      
         ORG   SVRDEF+(RSVRIND1-RSVRDEFD)                                       
         DC    AL1(RSVRILNK)                                                    
         ORG                                                                    
                                                                                
***********************************************************************         
* PROGRAM INTERFACE BLOCK                                             *         
***********************************************************************         
                                                                                
IBLOCK   DS    0X                                                               
         DS    (LI_LNQ)X'00'                                                    
         ORG   IBLOCK                                                           
         DC    AL2(100)            L'RETURNED AREA                              
         DC    C'PRI',C'LIN'       FROM SYSTEM/PROGRAM                          
         DC    C'PRI',C'PAY'       TO SYSTEM/PROGRAM                            
         ORG                                                                    
                                                                                
         TITLE '- PPLNK15 - CLEAR FOR PAYMENT REQUEST'                          
***********************************************************************         
* PROGRAM INTERFACE BLOCK                                             *         
***********************************************************************         
         SPACE 2                                                                
REQUEST  DS    0X                                                               
                                                                                
***********************************************************************         
* REQUEST MAP FOR CLEAR FOR PAYMENT REQUEST                           *         
***********************************************************************         
                                                                                
CLRPAY   DS    0XL(LH_LNQ)                                                      
         DC    AL2(CLRPAYX+1-*)                                                 
         DC    AL2(M#ULCLRP)                                                    
         DC    AL1(LH_IANUR)                                                    
         DC    AL2(0)                                                           
         DC    XL4'00'                                                          
                                                                                
CP#MEDC  EQU   1                   MEDIA CODE                                   
         DC    AL2(D#MEDCOD)       DATA CODE                                    
         DC    CL5'MEDCD'          DATA ID                                      
         DC    AL1(CP#MEDC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(L'PCLTKMED)     OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#MED) SYSTEM, DICTIONARY EQU                  
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#REQID EQU   2                   REQUEST ID                                   
         DC    AL2(D#REQID)        DATA CODE                                    
         DC    CL5'REQID'          DATA ID                                      
         DC    AL1(CP#REQID)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#RQSTR) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#CLTC  EQU   3                   CLIENT CODE                                  
         DC    AL2(D#CLTCOD)       DATA CODE                                    
         DC    CL5'CLTCD'          DATA ID                                      
         DC    AL1(CP#CLTC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(L'PCLTKCLT)     OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#CLTC)  SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PRDC  EQU   4                   PRODUCT CODE                                 
         DC    AL2(D#PRDCOD)       DATA CODE                                    
         DC    CL5'PRDCD'          DATA ID                                      
         DC    AL1(CP#PRDC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PRDC)  SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#STEND EQU   6                   START - END DATES                            
         DC    AL2(D#STEND)        DATA CODE                                    
         DC    CL5'STEND'          DATA ID                                      
         DC    AL1(CP#STEND)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#STEND) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PUBC  EQU   7                   PUBLICATION CODE                             
         DC    AL2(D#PUBCOD)       DATA CODE                                    
         DC    CL5'PUBCD'          DATA ID                                      
         DC    AL1(CP#PUBC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PUBCD) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PAYC  EQU   8                   PAYEE CODE                                   
         DC    AL2(D#PAYREP)       DATA CODE                                    
         DC    CL5'PAYCD'          DATA ID                                      
         DC    AL1(CP#PAYC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PAYEE) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#CTDTE EQU   9                   CONTROL DATE                                 
         DC    AL2(D#CTDTE)        DATA CODE                                    
         DC    CL5'CTDTE'          DATA ID                                      
         DC    AL1(CP#CTDTE)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#CTDTE) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#INVDT EQU   10                  INVOICE DATE                                 
         DC    AL2(D#INVDAT)       DATA CODE                                    
         DC    CL5'INVDT'          DATA ID                                      
         DC    AL1(CP#INVDT)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#INVDT) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PRTPY EQU   11                  PARTIAL PAY - Y/N                            
         DC    AL2(D#PARTPY)       DATA CODE                                    
         DC    CL5'PRTPY'          DATA ID                                      
         DC    AL1(CP#PRTPY)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(1)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PRTPY) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PYIND EQU   12                  PAY INDIVIDUALLY - Y/N                       
         DC    AL2(D#PAYIND)       DATA CODE                                    
         DC    CL5'PYIND'          DATA ID                                      
         DC    AL1(CP#PYIND)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(1)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PYIND) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
*                                                                               
CP#CRCK  EQU   13                  CR/CK OPTION                                 
         DC    AL2(D#CRCKOP)       DATA CODE                                    
         DC    CL5'CRCOP'          DATA ID                                      
         DC    AL1(CP#CRCK)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ+LD_ILSTQ)   DATA INDICATOR                          
*                                    ADD TO WORK POOL                           
*                                    LIST                                       
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(20)             OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#CRCK)  SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#GPST  EQU   14                  GST/PST OVERRIDE                             
         DC    AL2(D#GPSTOV)       DATA CODE                                    
         DC    CL5'GPST '          DATA ID                                      
         DC    AL1(CP#GPST)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ+LD_ILSTQ)   DATA INDICATOR                          
*                                    ADD TO WORK POOL                           
*                                    LIST                                       
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(27)             OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#GPST)  SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#OVPRV EQU   15                  OVERRIDE PROFILE VALUES - Y/N                
         DC    AL2(D#OVPROV)       DATA CODE                                    
         DC    CL5'OVPRV'          DATA ID                                      
         DC    AL1(CP#OVPRV)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(1)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#OVPRV) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
                                                                                
CP#BYSER EQU   16                  BUY SERIAL NUMBER                            
         DC    AL2(D#BYSER)        DATA CODE                                    
         DC    CL5'BYSER'          DATA ID                                      
         DC    AL1(CP#BYSER)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ+LD_ILSTQ)   DATA INDICATOR                          
*                                    ADD TO WORK POOL                           
*                                    LIST                                       
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(9)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#BYSQN) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#VINV1 EQU   17                  VENDOR INVOICE 1 (MATCH)                     
         DC    AL2(D#VINVNO)       DATA CODE                                    
         DC    CL5'VINV1'          DATA ID                                      
         DC    AL1(CP#VINV1)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#VENIN) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#CAMT1 EQU   18                  AMOUNT 1 TO CLEAR                            
         DC    AL2(D#AMTCLR)       DATA CODE                                    
         DC    CL5'CAMT1'          DATA ID                                      
         DC    AL1(CP#CAMT1)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#CLAMT) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PCOM1 EQU   19                  REMITTANCE (PAY) COMMENT 1                   
         DC    AL2(D#REMCOM)       DATA CODE                                    
         DC    CL5'PCOM1'          DATA ID                                      
         DC    AL1(CP#PCOM1)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PYCOM) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#VINV2 EQU   20                  VENDOR INVOICE 2 (MATCH)                     
         DC    AL2(D#VINVN2)       DATA CODE                                    
         DC    CL5'VINV2'          DATA ID                                      
         DC    AL1(CP#VINV2)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#VINV2) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#CAMT2 EQU   21                  AMOUNT 2 TO CLEAR                            
         DC    AL2(D#AMTCL2)       DATA CODE                                    
         DC    CL5'CAMT2'          DATA ID                                      
         DC    AL1(CP#CAMT2)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#CAMT2) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PCOM2 EQU   22                  REMITTANCE (PAY) COMMENT 2                   
         DC    AL2(D#REMCO2)       DATA CODE                                    
         DC    CL5'PCOM2'          DATA ID                                      
         DC    AL1(CP#PCOM2)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PYCM2) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#VINV3 EQU   23                  VENDOR INVOICE 3 (MATCH)                     
         DC    AL2(D#VINVN3)       DATA CODE                                    
         DC    CL5'VINV3'          DATA ID                                      
         DC    AL1(CP#VINV3)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#VINV3) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#CAMT3 EQU   24                  AMOUNT 3 TO CLEAR                            
         DC    AL2(D#AMTCL3)       DATA CODE                                    
         DC    CL5'CAMT3'          DATA ID                                      
         DC    AL1(CP#CAMT3)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#CAMT3) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PCOM3 EQU   25                  REMITTANCE (PAY) COMMENT 3                   
         DC    AL2(D#REMCO3)       DATA CODE                                    
         DC    CL5'PCOM3'          DATA ID                                      
         DC    AL1(CP#PCOM3)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PYCM3) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#VINV4 EQU   26                  VENDOR INVOICE 4 (MATCH)                     
         DC    AL2(D#VINVN4)       DATA CODE                                    
         DC    CL5'VINV4'          DATA ID                                      
         DC    AL1(CP#VINV4)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#VINV4) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#CAMT4 EQU   27                  AMOUNT 4 TO CLEAR                            
         DC    AL2(D#AMTCL4)       DATA CODE                                    
         DC    CL5'CAMT4'          DATA ID                                      
         DC    AL1(CP#CAMT4)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#CAMT4) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PCOM4 EQU   28                  REMITTANCE (PAY) COMMENT 4                   
         DC    AL2(D#REMCO4)       DATA CODE                                    
         DC    CL5'PCOM4'          DATA ID                                      
         DC    AL1(CP#PCOM4)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PYCM4) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#VINV5 EQU   29                  VENDOR INVOICE 5 (MATCH)                     
         DC    AL2(D#VINVN5)       DATA CODE                                    
         DC    CL5'VINV5'          DATA ID                                      
         DC    AL1(CP#VINV5)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#VINV5) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#CAMT5 EQU   30                  AMOUNT 5 TO CLEAR                            
         DC    AL2(D#AMTCL5)       DATA CODE                                    
         DC    CL5'CAMT5'          DATA ID                                      
         DC    AL1(CP#CAMT5)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#CAMT5) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PCOM5 EQU   31                  REMITTANCE (PAY) COMMENT 5                   
         DC    AL2(D#REMCO5)       DATA CODE                                    
         DC    CL5'PCOM5'          DATA ID                                      
         DC    AL1(CP#PCOM5)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PYCM5) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#PAYOK EQU   32                  PAYMENT SUCCESSFUL                           
         DC    AL2(D#PAYSUC)       DATA CODE                                    
         DC    CL5'PAYOK'          DATA ID                                      
         DC    AL1(CP#PAYOK)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(1)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PAYOK) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#CLRMS EQU   33                  CLEARANCE MESSAGE                            
         DC    AL2(D#CLRMSG)       DATA CODE                                    
         DC    CL5'CLRMS'          DATA ID                                      
         DC    AL1(CP#CLRMS)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#CLRMS) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#NETPD EQU   34                  NET AMOUNT PAID                              
         DC    AL2(D#NETPD)        DATA CODE                                    
         DC    CL5'NETPD'          DATA ID                                      
         DC    AL1(CP#NETPD)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#NETPD) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#ADRL1 EQU   35                  ADDRESS LINE 1                               
         DC    AL2(D#ADDRL1)       DATA CODE                                    
         DC    CL5'ADRL1'          DATA ID                                      
         DC    AL1(CP#ADRL1)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#ADRL1) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#ADRL2 EQU   36                  ADDRESS LINE 1                               
         DC    AL2(D#ADDRL2)       DATA CODE                                    
         DC    CL5'ADRL2'          DATA ID                                      
         DC    AL1(CP#ADRL2)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#ADRL2) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
CP#ADRL3 EQU   37                  ADDRESS LINE 1                               
         DC    AL2(D#ADDRL3)       DATA CODE                                    
         DC    CL5'ADRL3'          DATA ID                                      
         DC    AL1(CP#ADRL3)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#ADRL3) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
CP#XINV# EQU   38                  EXTENDED INVOICES (MORE THAN FIVE)           
         DC    AL2(D#INVNUM)       DATA CODE                                    
         DC    CL5'XINV#'          DATA ID                                      
         DC    AL1(CP#BYSER)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ+LD_ILSTQ)   DATA INDICATOR                          
*                                    ADD TO WORK POOL                           
*                                    LIST                                       
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(63)             OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#INVS_) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
*                                                                               
CP#PYSRC EQU   38                  SOURCE OF PAYMENT                            
         DC    AL2(D#INVSRC)       DATA CODE                                    
         DC    CL5'PYSRC'          DATA ID                                      
         DC    AL1(CP#PYSRC)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(1)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#FROM)  SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
*                                                                               
CLRPAYX  DC    AL1(LD_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR REFRESH PAY INFORMATION REQUEST                     *         
***********************************************************************         
                                                                                
REFPYEE  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REFPYEEX+1-*)                                                
         DC    AL2(M#DLRPA)                                                     
         DC    AL1(LH_IANUR)                                                    
         DC    AL2(0)                                                           
         DC    XL4'00'                                                          
                                                                                
RP#MEDC  EQU   1                   MEDIA CODE                                   
         DC    AL2(D#MEDCOD)       DATA CODE                                    
         DC    CL5'MEDCD'          DATA ID                                      
         DC    AL1(RP#MEDC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(L'PCLTKMED)     OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#MED) SYSTEM, DICTIONARY EQU                  
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
RP#REQID EQU   2                   REQUEST ID                                   
         DC    AL2(D#REQID)        DATA CODE                                    
         DC    CL5'REQID'          DATA ID                                      
         DC    AL1(RP#REQID)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#RQSTR) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
RP#CLTC  EQU   3                   CLIENT CODE                                  
         DC    AL2(D#CLTCOD)       DATA CODE                                    
         DC    CL5'CLTCD'          DATA ID                                      
         DC    AL1(RP#CLTC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(L'PCLTKCLT)     OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#CLTC)  SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
RP#PRDC  EQU   4                   PRODUCT CODE                                 
         DC    AL2(D#PRDCOD)       DATA CODE                                    
         DC    CL5'PRDCD'          DATA ID                                      
         DC    AL1(RP#PRDC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PRDC)  SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
RP#STEND EQU   5                   START - END DATES                            
         DC    AL2(D#STEND)        DATA CODE                                    
         DC    CL5'STEND'          DATA ID                                      
         DC    AL1(RP#STEND)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#STEND) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
RP#PUBC  EQU   6                   PUBLICATION CODE                             
         DC    AL2(D#PUBCOD)       DATA CODE                                    
         DC    CL5'PUBCD'          DATA ID                                      
         DC    AL1(RP#PUBC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PUBCD) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
RP#INVDT EQU   7                   PAYEE CODE                                   
         DC    AL2(D#INVDAT)       DATA CODE                                    
         DC    CL5'INVDT'          DATA ID                                      
         DC    AL1(RP#INVDT)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#INVDT) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
RP#PAYC  EQU   8                   PAYEE CODE                                   
         DC    AL2(D#PAYREP)       DATA CODE                                    
         DC    CL5'PAYCD'          DATA ID                                      
         DC    AL1(RP#PAYC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(0)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_VSTRQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PAYEE) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
REFPYEEX DC    AL1(LD_EOTQ)                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PPLNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035PPLNK15   10/14/20'                                      
         END                                                                    
