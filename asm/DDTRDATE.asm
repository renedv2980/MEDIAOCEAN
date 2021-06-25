*          DATA SET DDTRDATE   AT LEVEL 080 AS OF 08/31/98                      
*PHASE GCHEDATE                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
***********************************************************************         
*                                                                               
*                                                                               
* 1.  BE SURE TO CHANGE THE *PHASE CARD ABOVE SO YOU DON'T WIPE                 
*     OUT EACH OTHER'S LOAD MODULES.  REPLACE THE 'XXXX' WITH YOUR              
*     USERID.                                                                   
*                                                                               
* 2.  USE THE CVD AND UNPK INSTRUCTIONS TO PUT EACH BINARY NUMBER               
*     INTO EACH PRINT FIELD.                                                    
*                                                                               
* 3.  THERE IS JCL TO RUN THE PROGRAM IN 'DEIS.DDS.JCL(XXXXFIB)' --             
*     COPY THIS TO YOUR OWN JCL LIBRARY.                                        
*                                                                               
***********************************************************************         
         EJECT                                                                  
* LEAVE THIS CODE ALONE                                                         
*                                                                               
TESTHEX  CSECT                                                                  
         PRINT GEN                                                              
         NBASE 0,*TESTHEX*,=V(REGSAVE)                                          
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TESTHEX),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* PUT YOUR CODE HERE *                                                          
**********************                                                          
*                                                                               
*                                                                               
         USING JULTAB,R3                                                        
         LA    R3,TABLE                                                         
*                                                                               
LOOP     LA    R4,CARD                                                          
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    OUT                                                              
         MVC   P(18),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*        L     R8,=F'1901'                                                      
*        L     R9,SRTYEAR                                                       
*        CVD   R8,DUB                                                           
*        UNPK  P(8),DUB                                                         
*        OI    P+7,X'F0'                                                        
*        CVD   R9,DUB                                                           
*        UNPK  P(8),DUB                                                         
*        OI    P+7,X'F0'                                                        
*        CR    R8,R9                                                            
*        BH    OUT                                                              
*        BL    DIV                                                              
*NDY     ST    9(4,R4),SRTYEAR                                                  
*        L     R9,ENDYEAR                                                       
*        CVD   R8,DUB                                                           
*        UNPK  P(8),DUB                                                         
*        OI    P+7,X'F0'                                                        
*        CVD   R9,DUB                                                           
*        UNPK  P(8),DUB                                                         
*        OI    P+7,X'F0'                                                        
*        CR    R9,R8                                                            
*        BH    OUT                                                              
*        BL    DIV1          /CHECK IF ENDYEAR DIVISABLE BY 4                   
*                                                                               
DIV      DS    0H                                                               
         PACK  DUB,SRTYEAR                                                      
         CVB   R7,DUB                                                           
*        CVB   R5,DUB                                                           
*        CVB   R7,DUB                                                           
*                                                                               
         M     R6,=F'1'                                                         
*        M     R4,=F'1'                                                         
*        M     R6,=F'1'                                                         
         D     R6,=F'4'                                                         
*        D     R4,=F'100'                                                       
*        D     R6,=F'400'                                                       
*                                                                               
         C     R6,=F'0'                                                         
         BE    LEAPRT                                                           
*        C     R2,=F'0'                                                         
         BNE   NLEAPRT                                                          
*        C     R4,=F'0'                                                         
*        BE    NLEAPRT                                                          
*                                                                               
*        MVI   LEAPYEAR,C'1'                                                    
*        CLI   LEAPYEAR,C'1'                                                    
*        BE    LEAPRT                                                           
NLEAPRT  L     R2,SRTDAY                                                        
         L     R5,NLEAP                                                         
         CLI   0(R2),X'FF'                                                      
         BE    INVALID                                                          
*                                                                               
         LA    R3,TABLELQ(R3)                                                   
         L     R5,NLEAP                                                         
         CR    R5,R2                                                            
         BH    STEP                                                             
         BL    NLEAPRT                                                          
*                                                                               
LEAPRT   DS    0H                                                               
         L     R2,SRTDAY                                                        
         L     R5,LEAP                                                          
         CLI   0(R2),X'FF'                                                      
         BE    INVALID                                                          
*                                                                               
         LA    R3,TABLELQ(R3)   /BUMP TO NEXT ENTRY                             
         L     R5,LEAP                                                          
         CR    R5,R2                                                            
         BH    STEP                                                             
         BL    LEAPRT                                                           
*                                                                               
STEP     DS    0H                                                               
         S     R3,=F'7'                                                         
         SR    R2,R3                                                            
*                                                                               
         MVC   PRTMNT,MONTH                                                     
         CVD   R2,DUB                                                           
         UNPK  P+4(2),DUB                                                       
         OI    P+5,X'F0'                                                        
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
*                                                                               
         MVC   PRTYEAR,SRTYEAR                                                  
         B     ENDY                                                             
*                                                                               
DIV1     DS    0H                                                               
         PACK  DUB,ENDYEAR                                                      
         CVB   R7,DUB                                                           
         M     R6,=F'1'                                                         
         D     R6,=F'4'                                                         
         C     R6,=F'0'                                                         
         BE    LRT1                                                             
         BNE   NLRT1                                                            
NLRT1    DS    0H                                                               
         L     R2,ENDDAY                                                        
         L     R5,NLEAP                                                         
         CLI   0(R2),X'FF'                                                      
         BE    INVALID                                                          
         LA    R3,TABLELQ(R3)                                                   
         L     R5,NLEAP                                                         
         CR    R5,R2                                                            
         BH    STEP1                                                            
         BL    NLRT1                                                            
LRT1     DS    0H                                                               
         L     R2,ENDDAY                                                        
         L     R5,LEAP                                                          
         CR    R5,R2                                                            
         BH    STEP1                                                            
         BL    LRT1                                                             
STEP1    DS    0H                                                               
         S     R3,=F'7'                                                         
         SR    R2,R3                                                            
         MVC   PRTMNT,MONTH                                                     
         CVD   R2,DUB                                                           
         UNPK  P+4(2),DUB                                                       
         OI    P+5,X'F0'                                                        
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         MVC   PRTYEAR,ENDYEAR                                                  
         B     LOOP                                                             
INVALID  MVC   P(L'ERRMSG),ERRMSG                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
OUT      XBASE                                                                  
         EJECT                                                                  
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
DMCB     DS    6F                                                               
CARD     DS    0CL80                                                            
SRTYEAR  DS    CL4                                                              
SLASH    DS    CL1                                                              
SRTDAY   DS    CL3                                                              
SLASH1   DS    CL1                                                              
ENDYEAR  DS    CL4                                                              
SLASH    DS    CL1                                                              
ENDDAY   DS    CL3                                                              
         DS    CL64                                                             
DUB      DS    D                                                                
ERRMSG   DC    C'INVALID DATA'                                                  
TABLE    DC    C'   ',F'000',F'000'                                             
         DC    C'JAN',F'031',F'031'                                             
         DC    C'FEB',F'059',F'060'                                             
         DC    C'MAR',F'090',F'091'                                             
         DC    C'APR',F'120',F'121'                                             
         DC    C'MAY',F'151',F'152'                                             
         DC    C'JUN',F'181',F'182'                                             
         DC    C'JUL',F'212',F'213'                                             
         DC    C'AUG',F'243',F'244'                                             
         DC    C'SEP',F'273',F'274'                                             
         DC    C'OCT',F'304',F'305'                                             
         DC    C'NOV',F'334',F'335'                                             
         DC    C'DEC',F'365',F'366'                                             
         DC    X'FF'                                                            
LEAPYEAR DS    C                                                                
         LTORG                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         ORG   P                                                                
PRTMNT   DS    CL3                                                              
         DS    CL1                                                              
PRTDAY   DS    CL2                                                              
         DS    CL2                                                              
PRTYEAR  DS    CL4                                                              
         ORG                                                                    
JULTAB   DSECT                                                                  
MONTH    DS    CL3                                                              
NLEAP    DS    F                                                                
LEAP     DS    F                                                                
TABLELQ  EQU   *-JULTAB                                                         
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080DDTRDATE  08/31/98'                                      
         END                                                                    
