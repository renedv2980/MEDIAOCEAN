*          DATA SET MPBT550    AT LEVEL 029 AS OF 08/10/00                      
*PHASE MPBT550A                                                                 
         TITLE 'MBBT550 - LOAD SCOOP CPP''S TO MEDIA PLANNING FILE'             
***********************************************************************         
*                                                                     *         
*        START UP                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MPBT550  CSECT                                                                  
         NBASE WORKL,MPBT550,WORK=WRKSTOR                                       
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        INITIALIZATION - OPEN FILES - READ FIRST RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         OPEN  (SMP550,(INPUT))                                                 
         OPEN  (SMP560,(OUTPUT))                                                
         LA    RA,SMP550           ESTABLISH DCB                                
         USING IHADCB,RA                                                        
         GET   SMP550,SCPIN        READ MASTER YR/QTR                           
         MVC   YRQTR,SCPVAL        VERIFY YR AND QTR ARE NUMERIC                
         NC    YRQTR,ZEROS                                                      
         CLC   YRQTR,ZEROS                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   YRQTR,SCPVAL        SAVE YEAR AND QUARTER                        
         EJECT                                                                  
         PACK  DUB,YY              PACK YEAR                                    
         CVB   RE,DUB              CVB YEAR                                     
         PACK  DUB,QTR             CVB QTR                                      
         CVB   RF,DUB                                                           
         LA    R1,YRQTR1           POINT TO NEXT YEAR/QUARTER                   
         LA    R2,4                                                             
YYQLOOP  DS    0H                                                               
         LA    RF,1(RF)            BUMP QUARTER                                 
         CH    RF,=H'4'            TEST FOR NEXT YEAR                           
         BNH   *+12                                                             
         LA    RF,1                RESET TO FIRST QUARTER                       
         LA    RE,1(RE)            BUMP YEAR                                    
         CVD   RE,DUB              SET YEAR                                     
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(L'YY,R1),DUB                                                   
         STC   RF,2(R1)            SET QUARTER                                  
         OI    2(R1),X'F0'         FORCE SIGN                                   
         LA    R1,L'YRQTR(R1)      BUMP TO NEXT YEAR/QTR FIELD                  
         BCT   R2,YYQLOOP                                                       
*                                                                               
RECLOOP  DS    0H                                                               
         XC    CPPREC,CPPREC       INIT OUTPUT AREA                             
         LA    R3,CPPREC           ESTABLISH OUTPUT RECORD                      
         USING CPPRECD,R3                                                       
         MVC   CPPYYQ,YRQTR        SET FIRST YEAR AND QUARTER                   
         LA    R1,CPPDPTN          GET MAX NUMBER OF DAYPARTS                   
INITLOOP DS    0H                                                               
         MVC   CPPDPT,SPACES       INIT DAYPART TO SPACES                       
         MVC   CPPCPP,=C'000000'   INIT COST PER POINT                          
         LA    R3,CPPDPTSL(R3)     BUMP TO NEXT DAYPART AREA                    
         BCT   R1,INITLOOP                                                      
*                                                                               
         LA    R3,CPPREC           RE-POINT TO OUTPUT AREA                      
*                                                                               
         MVC   SCPIN,SPACES        INIT INPUT AREA                              
         GET   SMP550,SCPIN        GET DEMO                                     
         MVC   CPPDEMO,SCPVAL      PUT IN OUTPUT REC                            
*                                                                               
         MVC   SCPIN,SPACES        INIT INPUT AREA                              
         GET   SMP550,SCPIN        GET DMA CODE                                 
         MVC   CPPMKT,SCPVAL+2     PUT IN OUTPUT REC                            
*                                                                               
         MVC   SCPIN,SPACES        INIT INPUT AREA                              
         GET   SMP550,SCPIN        GET MARKET NAME                              
         MVC   CPPMKTNM,SCPVAL     PUT IN OUTPUT REC                            
*                                                                               
         LA    R1,4                COPY MARKET OUTPUT RECORD FIVE TIMES         
         LA    R4,YRQTR1                                                        
CPYLOOP  DS    0H                                                               
         MVC   L'CPPREC(L'CPPREC,R3),0(R3)                                      
         LA    R3,L'CPPREC(R3)     BUMP RECORD POINTER                          
         MVC   CPPYYQ,0(R4)        SET NEXT YEAR QUARTER                        
         LA    R4,L'YRQTR(R4)      BUMP TO NEXT YEAR AND QUARTER                
         BCT   R1,CPYLOOP                                                       
*                                                                               
         LA    R4,CPPDPTN          INIT MAX NUMBER OF DAYPARTS                  
         SR    R5,R5               INIT DAYPART POINTER                         
DPTLOOP  DS    0H                                                               
         MVC   SCPIN,SPACES        INIT INPUT AREA                              
         GET   SMP550,SCPIN        GET NEXT DAYPART                             
         CLC   SCPVAL+2(L'CPPDPT),=C'0000' NUMERIC MEANS NEXT MKT               
         BNL   CPPLPDN                                                          
         MVC   DPT,SCPVAL          SAVE DAYPART                                 
*                                                                               
         LA    R3,CPPREC(R5)       INIT OUTPUT POINTER                          
         LA    R6,5                NUMBER OF QUARTERS IN INPUT                  
CPPLOOP  DS    0H                                                               
         MVC   SCPIN,SPACES        INIT INPUT AREA                              
         GET   SMP550,SCPIN        GET NEXT CPP                                 
         LA    RE,SCPVAL                                                        
         OC    0(7,RE),=C'0000.00'  ADD LEADING ZEROS                           
         MVC   CPPDPT,DPT          SET DAYPART                                  
         MVC   CPP,0(RE)           PASS CPP WITHOUT DECIMAL POINT               
         MVC   CPPCPP$S,CPPINT                                                  
         MVC   CPPCPPCT,CPPDEC                                                  
         LA    R3,L'CPPREC(R3)     BUMP TO NEXT OUTPUT RECORD                   
         BCT   R6,CPPLOOP          DO NEXT QUARTER'S CPP                        
*                                                                               
         LA    R5,CPPDPTSL(R5)     BUMP TO NEXT DAYPART AREA                    
         BCT   R4,DPTLOOP          GET NEXT DAYPART                             
         DC    H'0'                                                             
CPPLPDN  DS    0H                                                               
         LA    R3,CPPREC           POINT TO OUTPUT RECORDS                      
         LA    R4,5                SET FOR FIVE RECORDS                         
OUTLOOP  DS    0H                                                               
         PUT   SMP560,(R3)         PUT OUT RECORD                               
         LA    R3,L'CPPREC(R3)     BUMP TO NEXT OUT RECORD                      
         BCT   R4,OUTLOOP                                                       
         B     RECLOOP                                                          
*                                                                               
SMP550X  DS    0H                                                               
         LA    R3,CPPREC           POINT TO OUTPUT RECORDS                      
         LA    R4,5                SET FOR FIVE RECORDS                         
ENDLOOP  DS    0H                                                               
         PUT   SMP560,(R3)         PUT OUT RECORD                               
         LA    R3,L'CPPREC(R3)     BUMP TO NEXT OUT RECORD                      
         BCT   R4,ENDLOOP                                                       
*                                                                               
         CLOSE SMP550                                                           
         CLOSE SMP560                                                           
         XBASE 1                                                                
         LTORG                                                                  
ZEROS    DC    16C'0'                                                           
SPACES   DC    CL256' '                                                         
SMP550   DCB   DDNAME=SMP550,                                          X        
               DSORG=PS,                                               X        
               MACRF=GM,                                               X        
               EODAD=SMP550X                                                    
SMP560   DCB   DDNAME=SMP560,                                          X        
               DSORG=PS,                                               X        
               MACRF=PT                                                         
WRKSTOR  DS    1000D               WORKING STORAGE                              
         DCBD  DSORG=PS                                                         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                     WORKING STORAGE                              
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
DPT      DS    CL3                 DAYPART                                      
CPP      DS    0XL7                CPP                                          
CPPINT   DS    XL4                 INTEGER PART                                 
CPPDECPT DS    X                   DECIMAL POINT                                
CPPDEC   DS    XL2                 DECIMAL PORTION                              
YRQTR    DS    0XL3                BASE YEAR AND QUARTER                        
YY       DS    XL2                 YEAR                                         
QTR      DS    XL1                 QUARTER                                      
YRQTR1   DS    0XL3                BASE YEAR AND QUARTER                        
YY1      DS    XL2                 YEAR                                         
QTR1     DS    XL1                 QUARTER                                      
YRQTR2   DS    0XL3                BASE YEAR AND QUARTER                        
YY2      DS    XL2                 YEAR                                         
QTR2     DS    XL1                 QUARTER                                      
YRQTR3   DS    0XL3                BASE YEAR AND QUARTER                        
YY3      DS    XL2                 YEAR                                         
QTR3     DS    XL1                 QUARTER                                      
YRQTR4   DS    0XL3                BASE YEAR AND QUARTER                        
YY4      DS    XL2                 YEAR                                         
QTR4     DS    XL1                 QUARTER                                      
SCPIN    DS    XL255                                                            
         ORG   SCPIN                                                            
SCPLRECL DS    XL2                 LRECL                                        
         DS    XL2                                                              
SCPREC   DS    0X                  RECORD                                       
*SCPSQN   DS    XL8                 SEQUENCE NUMBER                             
SCPVAL   DS    0X                  ACTUAL SCCOP DATA                            
         ORG                                                                    
CPPREC   DS    XL255               OUTPUT RECORDS AREA                          
CPPREC1  DS    XL255               OUTPUT RECORDS AREA                          
CPPREC2  DS    XL255               OUTPUT RECORDS AREA                          
CPPREC3  DS    XL255               OUTPUT RECORDS AREA                          
CPPREC4  DS    XL255               OUTPUT RECORDS AREA                          
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        OUTPUT RECORD DSECT                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CPPRECD  DSECT                     WORKING STORAGE                              
CPPYYQ   DS    0XL3                YEAR/QUARTER                                 
CPPYY    DS    XL2                 YEAR                                         
CPPQTR   DS    XL1                 QUARTER                                      
CPPDEMO  DS    CL8                 DEMO                                         
CPPMKT   DS    CL3                 DMA CODE                                     
CPPMKTNM DS    CL18                MARKET NAME                                  
CPPDPTS  DS    0C                  DAYPART SECTION - OCCURS 8 TIMES             
CPPDPT   DS    CL3                 DAYPART                                      
CPPCPP   DS    0CL6                CPP                                          
CPPCPP$S DS    CL4                 CPP DOLLARS FOR DAYPART                      
CPPCPPCT DS    CL2                 CPP CENTS   FOR DAYPART                      
CPPDPTSL EQU   *-CPPDPTS           LENGTH OF DAYAPRT SECTION                    
*                                                                               
CPPDPTN  EQU   12                  MAX NUMBER OF DAYPARTS                       
         EJECT                                                                  
MPBT550  CSECT                                                                  
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029MPBT550   08/10/00'                                      
         END                                                                    
