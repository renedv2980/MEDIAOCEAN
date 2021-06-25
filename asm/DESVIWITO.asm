*          DATA SET DESVIWITO  AT LEVEL 043 AS OF 08/23/00                      
*PHASE SVIWITOA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DEMTIME                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
         PRINT NOGEN                                                            
*                                                                               
SVIWITO  TITLE 'CREATE INDICES FOR WITO'                                        
SVIWITO  CSECT                                                                  
         NBASE 600,SVIWITO,=V(REGSAVE)                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         OPEN  (OUT,(OUTPUT))                                                   
*                                                                               
         LA    R4,NSIMKTS          R4 - NSIMKTS TABLE                           
         LA    R4,NSIMKTS1         R4 - NSIMKTS TABLE                           
         MVI   PASS,1                                                           
*                                                                               
MAIN     LH    RE,0(R4)                                                         
         C     RE,=F'0'            END OF NSIMKTS TABLE ?                       
         BZ    NEXTTAB                                                          
         LA    R5,DYTMS            R5 - DYTMS TABLE                             
         MVI   CARDD,C' '                                                       
         MVC   CARDD+1(79),CARDD                                                
*                                                                               
         MVC   CMED(3),=C'CNA'     MEDIA/SERVICE/SOURCE                         
*                                                                               
M300     LH    RE,0(R4)            MKT# FROM NSIMKTS TABLE                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CMKT,DUB                                                         
*                                                                               
         MVC   CDAY,0(R5)          DAY...FROM DYTMS                             
*                                                                               
         LA    R6,4(R5)                                                         
         GOTO1 =V(HRTOQH),DUB,(0,(R6)),CSTQH                                    
         SR    RE,RE                                                            
         IC    RE,CSTQH                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CSTQH,DUB           START QH                                     
*                                                                               
         LA    R6,6(R5)                                                         
         GOTO1 =V(HRTOQH),DUB,(0,(R6)),CENDQH                                   
         SR    RE,RE                                                            
         IC    RE,CENDQH                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CENDQH,DUB          END QH                                       
*                                                                               
         MVC   CAUD,=C'01'         AUDIENCE TYPE                                
         MVC   CACT,=C'A'          ACTION CODE                                  
*                                                                               
         MVC   CMONS(4),=C' 100'   MONTHS (JAN)                                 
         MVC   CMONS+4(44),CMONS                                                
         CLI   PASS,0                                                           
         BNE   IDX1                                                             
         MVC   CMONS(08),=C' 110 110'  JAN/FEB                                  
         MVC   CMONS+16(08),=C' 090 090'  MAY/JUL                               
         MVC   CMONS+24(08),=C' 080 080'  JUL/AUG                               
         B     IDXDONE                                                          
*                                                                               
IDX1     CLI   PASS,1                                                           
         BNE   IDX2                                                             
         MVC   CMONS(08),=C' 100 110'  JAN/FEB                                  
         MVC   CMONS+16(08),=C' 090 090'  MAY/JUL                               
         MVC   CMONS+24(08),=C' 100 080'  JUL/AUG                               
         B     IDXDONE                                                          
*                                                                               
IDX2     CLI   PASS,2                                                           
         BNE   IDX3                                                             
         MVC   CMONS(08),=C' 100 100'  JAN/FEB                                  
         MVC   CMONS+16(08),=C' 090 090'  MAY/JUL                               
         MVC   CMONS+24(08),=C' 100 080'  JUL/AUG                               
         B     IDXDONE                                                          
*                                                                               
IDX3     CLI   PASS,3                                                           
         BNE   IDX4                                                             
         MVC   CMONS(08),=C' 100 110'  JAN/FEB                                  
         MVC   CMONS+16(08),=C' 090 090'  MAY/JUL                               
         MVC   CMONS+24(08),=C' 100 080'  JUL/AUG                               
         B     IDXDONE                                                          
IDX4     DC    H'0'                                                             
*                                                                               
IDXDONE  L     R3,=A(OUT)          OUTPUT CARDD RECORD                          
         PUT   (R3),CARDD                                                       
*                                                                               
*        LA    RE,NSIMKTS          R4 - NSIMKTS TABLE                           
*        CR    RE,R4               1ST MKT ?                                    
*        BNE   NOPRINT               IF NOT - BYPASS PRINT                      
         XC    P,P                                                              
         MVC   P(80),CARDD         PRINT OUTPUT OF 1ST MARKET ONLY              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NOPRINT  LA    R5,12(R5)           DYTMS TABLE - POINT TO NEXT ENTRY            
         LH    RE,0(R5)                                                         
         C     RE,=F'0'            END OF DYTMS TABLE ?                         
         BNZ   M300                  IF NOT - LOOP BACK                         
         LA    R4,32(R4)           NSIMKTS TABLE - POINT TO NEXT ENTRY          
         B     MAIN                PROCESS NEXT MARKET                          
*                                                                               
NEXTTAB  ZIC   RF,PASS                                                          
         LA    RF,1(RF)                                                         
         STC   RF,PASS                                                          
         CLI   PASS,1                                                           
         BNE   *+12                                                             
         LA    R4,NSIMKTS1                                                      
         B     MAIN                                                             
         CLI   PASS,2                                                           
         BNE   *+12                                                             
         LA    R4,NSIMKTS2                                                      
         B     MAIN                                                             
         CLI   PASS,3                                                           
         BNE   *+12                                                             
         LA    R4,NSIMKTS3                                                      
         B     MAIN                                                             
ENDJOB   CLOSE (OUT)                                                            
         XBASE                                                                  
*                                                                               
         EJECT                                                                  
DUB      DS    D                   DOUBLE WORD                                  
WORD     DS    F                   FULL WORD                                    
HALF     DS    H                   HALF WORD                                    
CHAR     DS    C                   CHARACTER                                    
PASS     DS    C                                                                
*                                                                               
CARDD    DS    0H                                                               
CMED     DS    CL1                 MEDIA = T                                    
CRAT     DS    CL1                 SERVICE = N                                  
CSRC     DS    CL1                 SOURCE = E/F                                 
         DS    CL1                                                              
CMKT     DS    CL4                 MARKET                                       
         DS    CL1                                                              
CDAY     DS    CL3                 DAY M-F,SAT,SUN                              
         DS    CL1                                                              
CSTQH    DS    CL2                 START QH                                     
         DS    CL1                                                              
CENDQH   DS    CL2                 END QH                                       
         DS    CL1                                                              
CAUD     DS    CL2                 AUDIENCE TYPE = 01                           
         DS    CL1                                                              
CACT     DS    CL1                 ACTION CODE = A                              
         DS    CL1                                                              
CMONS    DS    CL48                MONTHS                                       
         DS    CL8                 SPARE                                        
*                                                                               
         LTORG                                                                  
*                                                                               
DYTMS    DS    0H                            SEPT                               
*                DAY       STRT      END      SVI                               
         DC    C'M-F ',AL2(0600),AL2(0200),C' 100' M-F/6A-2A                    
         DC    C'SAT ',AL2(0600),AL2(0200),C' 100' SAT/6A-2A                    
         DC    C'SUN ',AL2(0600),AL2(0200),C' 100' SUN/6A-2A                    
DYTMSX   DC    F'0'                                                             
*                                                                               
NSIMKTS  DS    0CL32                                                            
         DC    AL2(0457),CL30'BARRIE'                                           
         DC    AL2(0453),CL30'CARLETON'                                         
         DC    AL2(0434),CL30'CHARLOTTETOWN'                                    
         DC    AL2(0449),CL30'CHICOUTIMI/JONQUIERE'                             
         DC    AL2(0483),CL30'DAWSON CREEK'                                     
         DC    AL2(0430),CL30'HALIFAX'                                          
         DC    AL2(0492),CL30'KELOWNA/KAMLOOPS'                                 
         DC    AL2(0435),CL30'KENORA'                                           
         DC    AL2(0454),CL30'KINGSTON'                                         
         DC    AL2(0447),CL30'KITCHENER'                                        
         DC    AL2(0491),CL30'KITCHENER/LONDON'                                 
         DC    AL2(0486),CL30'LLOYDMINSTER'                                     
         DC    AL2(0445),CL30'LONDON'                                           
         DC    AL2(0477),CL30'MARITINES'                                        
         DC    AL2(0488),CL30'MEDICINE HAT'                                     
         DC    AL2(0431),CL30'MONCTON-ST. JOHN'                                 
         DC    AL2(0414),CL30'MONTREAL/ANGLO'                                   
         DC    AL2(0415),CL30'MONTREAL/FRENCH'                                  
         DC    AL2(0433),CL30'NEWFOUNDLAND/ST. JOHNS'                           
         DC    AL2(0464),CL30'PEMBROKE'                                         
         DC    AL2(0456),CL30'PETERBOROUGH'                                     
         DC    AL2(0475),CL30'PRINCE ALBERT'                                    
         DC    AL2(0494),CL30'PRINCE GEORGE'                                    
         DC    AL2(0443),CL30'QUEBEC'                                           
         DC    AL2(0427),CL30'RED DEER'                                         
         DC    AL2(0471),CL30'REGINA/MOOSE JAW'                                 
         DC    AL2(0448),CL30'RIMOUSKI/MATANE'                                  
         DC    AL2(0463),CL30'RIVIERE DU LOOP'                                  
         DC    AL2(0452),CL30'ROUYN-NORANDA'                                    
         DC    AL2(0474),CL30'SASKATOON'                                        
         DC    AL2(0451),CL30'SHERBROOKE'                                       
         DC    AL2(0495),CL30'SUDBURY/TIMM/NB/SS MARIE'                         
         DC    AL2(0476),CL30'SWIFT CURRENT'                                    
         DC    AL2(0432),CL30'SYDNEY/GLACE BAY'                                 
         DC    AL2(0496),CL30'TERRACE-KITIMAT'                                  
         DC    AL2(0461),CL30'THUNDER BAY'                                      
         DC    AL2(0450),CL30'TRIOS RIVIERIES'                                  
         DC    AL2(0459),CL30'WINDSOR'                                          
         DC    AL2(0470),CL30'WINIPEG/BRANDON'                                  
         DC    AL2(0472),CL30'BRANDON'                                          
NSIMKTSX DC    AL2(0)                                                           
*                                                                               
NSIMKTS1 DS    0CL32                                                            
         DC    AL2(0440),CL30'TORONTO'                                          
         DC    AL2(0441),CL30'MONTREAL'                                         
         DC    AL2(0490),CL30'VANCOUVER'                                        
         DC    AL2(0484),CL30'CALGARY'                                          
         DC    AL2(0482),CL30'EDMONTON'                                         
         DC    AL2(0)                                                           
*                                                                               
NSIMKTS2 DS    0CL32                                                            
         DC    AL2(0444),CL30'OTTAWA'                                           
         DC    AL2(0)                                                           
*                                                                               
NSIMKTS3 DS    0CL32                                                            
         DC    AL2(0416),CL30'OTTAWA/A'                                         
         DC    AL2(0478),CL30'OTTAWA/F'                                         
         DC    AL2(0)                                                           
*                                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=08000,                                          X        
               MACRF=PM                                                         
*                                                                               
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043DESVIWITO 08/23/00'                                      
         END                                                                    
