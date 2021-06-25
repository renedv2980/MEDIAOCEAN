*          DATA SET PPLNK14    AT LEVEL 040 AS OF 02/02/04                      
*PHASE T41414A                                                                  
PPLNK14  TITLE '- PPLNK14 - INSERTION ORDER REQUEST'                            
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
         DC    C'IO'               PROGRAM                                      
         ORG   SVRDEF+(RSVRIND1-RSVRDEFD)                                       
         DC    AL1(RSVRILNK)                                                    
         ORG                                                                    
                                                                                
***********************************************************************         
* PROGRAM INTERFACE BLOCK                                             *         
***********************************************************************         
                                                                                
IBLOCK   DS    0X                                                               
         DS    (LI_LNQ)X'00'                                                    
         ORG   IBLOCK                                                           
         DC    AL2(1500)           L'RETURNED AREA                              
         DC    C'PRI',C'LIN'       FROM SYSTEM/PROGRAM                          
         DC    C'PRI',C'INS'       TO SYSTEM/PROGRAM                            
         ORG                                                                    
                                                                                
         TITLE '- PPLNK14 - INSERTION ORDER REQUEST - REQUEST'                  
***********************************************************************         
* PROGRAM INTERFACE BLOCK                                             *         
***********************************************************************         
         SPACE 2                                                                
REQUEST  DS    0X                                                               
                                                                                
***********************************************************************         
* REQUEST MAP FOR INSERTION ORDER REQUEST                             *         
***********************************************************************         
                                                                                
REQIOR   DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQIORX+1-*)                                                 
         DC    AL2(M#ULIOR)                                                     
         DC    AL1(LH_IANUR)                                                    
         DC    AL2(0)                                                           
         DC    XL4'00'                                                          
                                                                                
DI#REQID EQU   1                   REQUEST ID                                   
         DC    AL2(D#REQID)        DATA CODE                                    
         DC    CL5'REQID'          DATA ID                                      
         DC    AL1(DI#REQID)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(3)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#RQSTR) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#MEDC  EQU   2                   MEDIA CODE                                   
         DC    AL2(D#MEDCOD)       DATA CODE                                    
         DC    CL5'MEDCD'          DATA ID                                      
         DC    AL1(DI#MEDC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(L'PAGYKMED)     OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#MED) SYSTEM, DICTIONARY EQU                  
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#CLTC  EQU   3                   CLIENT CODE                                  
         DC    AL2(D#CLTCOD)       DATA CODE                                    
         DC    CL5'CLTCD'          DATA ID                                      
         DC    AL1(DI#CLTC)        COLUMN NUMBER                                
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
                                                                                
DI#PRDC  EQU   4                   PRODUCT CODE                                 
         DC    AL2(D#PRDCOD)       DATA CODE                                    
         DC    CL5'PRDCD'          DATA ID                                      
         DC    AL1(DI#PRDC)        COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ)       DATA INDICATOR                               
*                                    ADD TO WORK POOL                           
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(L'PPRDKPRD)     OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - CHARACTER                        
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#PRDC)  SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#ESTN  EQU   5                   ESTIMATE NUMBER                              
         DC    AL2(D#ESTNUM)       DATA CODE                                    
         DC    CL5'ESTCD'          DATA ID                                      
         DC    AL1(DI#ESTN)        COLUMN NUMBER                                
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
         DC    AL1(PRTSYSQ),AL2(PP#ESTNO) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#ADCOD EQU   6                   ADD CODE                                     
         DC    AL2(D#ADCODE)       DATA CODE                                    
         DC    CL5'ADCOD'          DATA ID                                      
         DC    AL1(DI#ADCOD)       COLUMN NUMBER                                
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
         DC    AL1(PRTSYSQ),AL2(PP#ADCOD) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#PUBC  EQU   7                   PUBLICATION CODE                             
         DC    AL2(D#PUBCOD)       DATA CODE                                    
         DC    CL5'PUBCD'          DATA ID                                      
         DC    AL1(DI#PUBC)        COLUMN NUMBER                                
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
                                                                                
DI#STEND EQU   8                   START - END DATES                            
         DC    AL2(D#STEND)        DATA CODE                                    
         DC    CL5'STEND'          DATA ID                                      
         DC    AL1(DI#STEND)       COLUMN NUMBER                                
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
                                                                                
DI#CTDTE EQU   9                   START - END DATES                            
         DC    AL2(D#CTDTE)        DATA CODE                                    
         DC    CL5'CTDTE'          DATA ID                                      
         DC    AL1(DI#CTDTE)       COLUMN NUMBER                                
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
                                                                                
DI#TSTYN EQU   10                  TEST RUN Y/N                                 
         DC    AL2(D#TSTYN)        DATA CODE                                    
         DC    CL5'TSTYN'          DATA ID                                      
         DC    AL1(DI#TSTYN)       COLUMN NUMBER                                
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
         DC    AL1(PRTSYSQ),AL2(PP#TSTYN) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#NDOYN EQU   11                  NEEDED ONLY - Y/N                            
         DC    AL2(D#NDOYN)        DATA CODE                                    
         DC    CL5'NDOYN'          DATA ID                                      
         DC    AL1(DI#NDOYN)       COLUMN NUMBER                                
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
         DC    AL1(PRTSYSQ),AL2(PP#NDOYN) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#FAX#2 EQU   12                  ADDITIONAL FAX NUMBER                        
         DC    AL2(D#FAX#2)        DATA CODE                                    
         DC    CL5'FAX#2'          DATA ID                                      
         DC    AL1(DI#FAX#2)       COLUMN NUMBER                                
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
         DC    AL1(PRTSYSQ),AL2(PP#FAX#2) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#SPCST EQU   13                  SUPPRESS COST ON SECOND COPY                 
         DC    AL2(D#SPCST)        DATA CODE                                    
         DC    CL5'SPCST'          DATA ID                                      
         DC    AL1(DI#SPCST)       COLUMN NUMBER                                
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
         DC    AL1(PRTSYSQ),AL2(PP#SPCST) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#ICOM1 EQU   14                  INSERTION COMMENT 1                          
         DC    AL2(D#INSCO1)       DATA CODE                                    
         DC    CL5'ICOM1'          DATA ID                                      
         DC    AL1(DI#ICOM1)       COLUMN NUMBER                                
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
         DC    AL1(PRTSYSQ),AL2(PP#ICOM1) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#ICOM2 EQU   15                  INSERTION COMMENT 2                          
         DC    AL2(D#INSCO2)       DATA CODE                                    
         DC    CL5'ICOM2'          DATA ID                                      
         DC    AL1(DI#ICOM2)       COLUMN NUMBER                                
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
         DC    AL1(PRTSYSQ),AL2(PP#ICOM2) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#ICOM3 EQU   16                  INSERTION COMMENT 3                          
         DC    AL2(D#INSCO3)       DATA CODE                                    
         DC    CL5'ICOM3'          DATA ID                                      
         DC    AL1(DI#ICOM3)       COLUMN NUMBER                                
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
         DC    AL1(PRTSYSQ),AL2(PP#ICOM3) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
DI#BYSER EQU   18                  BUY SERIAL NUMBER                            
         DC    AL2(D#BYSER)        DATA CODE                                    
         DC    CL5'BYSER'          DATA ID                                      
         DC    AL1(DI#BYSER)       COLUMN NUMBER                                
         DC    AL1(LD_IAWPQ+LD_ILSTQ)   DATA INDICATOR                          
*                                    ADD TO WORK POOL                           
*                                    LIST                                       
         DC    AL1(0)              DATA INDICATOR 2                             
         DC    AL1(0)              DATA INDICATOR 3                             
         DC    AL1(0,0)            VALIDATION ROUTINE INDEX, SPARE              
         DC    AL2(0)              DISP OF DATA IN CONTROL BLOCK                
         DC    AL2(0)              DISP TO DATA INDEX                           
         DC    AL1(9)              OUTPUT DATA LENGTH                           
         DC    AL1(LD_CHARQ)       DATA TYPE - STRING                           
         DC    AL1(0,0)            MAX IN LIST, MAX INPUT LENGTH                
         DC    XL2'00'             DATA IND 4, PARAMETER VALUE                  
         DC    AL1(PRTSYSQ),AL2(PP#BYSQN) SYSTEM, DICTIONARY EQU                
         DC    XL4'00'             VERSION NUMBER                               
                                                                                
REQIORX  DC    AL1(LD_EOTQ)                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PPLNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040PPLNK14   02/02/04'                                      
         END                                                                    
