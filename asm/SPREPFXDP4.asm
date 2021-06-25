*          DATA SET SPREPFXDP4 AT LEVEL 021 AS OF 05/01/02                      
*PHASE SPFX02U                                                                  
         TITLE 'SPFX02 - FIX COKE DAYPARTS'                                     
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,MKTFRST                                                     
         BE    MKTF                                                             
*                                                                               
         CLI   MODE,STAFRST                                                     
         BE    STAF                                                             
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    FX10                                                             
*                                                                               
         CLI   MODE,REQLAST                                                     
         BE    FX400                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* MARKET FIRST                                                                  
MKTF     DS    0H                  GET TIME ZONE                                
         USING MKTRECD,R6                                                       
         L     R6,ADMARKET                                                      
         MVC   ZONE,MKTZONE                                                     
                                                                                
         DROP  R6                                                               
         B     EXIT                                                             
                                                                                
STAF     DS    0H                                                               
         USING STARECD,R6                                                       
         L     R6,ADSTAT                                                        
         MVC   NETWRK,SNETWRK                                                   
         CLI   NETWRK,C'I'           IF NOT INDEPENDENT                         
         BE    STAFEX                                                           
         MVI   NETWRK,C'N'           SET TO NETWORK                             
         CLI   PROGPROF+3,C'Y'       TEST FOX OPTION                            
         BNE   STAFEX                                                           
         CLC   SNETWRK,=C'FOX'                                                  
         BNE   STAFEX                                                           
         MVI   NETWRK,C'F'         SPECIAL DAYPARTS FOR FOX                     
                                                                                
STAFEX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
                                                                                
* PROCBUY                                                                       
FX10     DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         CLC   =C'SI5',BDPROGRM                                                 
         BE    FX15                                                             
         CLC   =C'AUTO - I5',BDPROGRM                                           
         BNE   EXIT                                                             
                                                                                
FX15     LA    R2,TIMTDIR                                                       
FX17     CLI   0(R2),X'FF'                                                      
         BE    FX200                                                            
         CLC   QAGY,0(R2)          AGENCY                                       
         BNE   FX19                                                             
         CLC   QMED,2(R2)          MEDIA                                        
         BNE   FX19                                                             
         CLC   ZONE,3(R2)                                                       
         BE    FX17B                                                            
         CLI   3(R2),C'Z'          ALL ZONES                                    
         BNE   FX19                                                             
*                                                                               
FX17B    DS    0H                                                               
         CLC   NETWRK,4(R2)                                                     
         BE    FX17C                                                            
         CLI   4(R2),C'Z'          ALL AFFILS                                   
         BNE   FX19                                                             
*                                                                               
FX17C    DS    0H                                                               
         B     FX20                                                             
*                                                                               
FX19     DS    0H                                                               
         LA    R2,12(R2)           NEXT TABLE DIRECTORY ENTRY                   
         B     FX17                                                             
                                                                                
FX20     DS    0H                                                               
         L     R2,8(R2)            A(DPT TABLE ENTRY)                           
*                                                                               
FX40     DS    0H                                                               
         CLI   0(R2),X'FF'         EOT                                          
         BE    FX47                                                             
*                                                                               
FX42     DS    0H                                                               
         CLC   BDDAY,0(R2)         DAY(S)                                       
         BNE   FX50                                                             
         CLC   BDDAYPT,3(R2)       DAYPART                                      
         BNE   FX50                                                             
*                                                                               
         CLC   BDTIMST(4),14(R2)      TIMES                                     
         BE    FX60                IF =, DONE WITH BUY                          
         MVC   SAVTIM,BDTIMST                                                   
         MVC   BDTIMST(4),14(R2)   ELSE SET NEW TIMES                           
*                                                                               
FX43     DS    0H                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 PUT                                                              
*                                                                               
FX46     DS    0H                                                               
         BAS   RE,PRTBUY                                                        
         B     FX60                                                             
*                                                                               
FX47     DS    0H                  SPECIAL FIXUP                                
         LA    R2,FIXLST                                                        
*                                                                               
FX47D    DS    0H                                                               
         CLI   0(R2),X'FF'         EOL                                          
         BE    FX48                                                             
*                                                                               
         CLC   ZONE(2),0(R2)       RIGHT ZONE/AFFIL                             
         BNE   FX47M                                                            
         CLC   BDDAYPT,2(R2)       DAYPART                                      
         BNE   FX47M                                                            
         CLC   BDDAY,3(R2)         DAY(S)                                       
         BNE   FX47M                                                            
*                                                                               
         MVC   BDDAY,4(R2)         SET NEW DAY                                  
         MVC   BDTIMST(4),5(R2)    AND START-END TIME                           
         B     FX43                                                             
*                                                                               
FX47M    DS    0H                                                               
         LA    R2,9(R2)                                                         
         B     FX47D                                                            
*                                  NO DAYPART FOUND FOR DAY/TIME                
FX48     DS    0H                                                               
         MVC   P+4(17),=C'**BAD DPT BELOW**'                                    
         GOTO1 REPORT                                                           
         B     FX46                                                             
*                                                                               
FX50     DS    0H                                                               
         LA    R2,TIMTABL(R2)                                                   
         B     FX40                                                             
*                                                                               
FX60     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
FX200    DS    0H                                                               
         MVC   P+4(2),=C'**'                                                    
         MVC   P+7(2),ZONE                                                      
         MVC   P+10(18),=C'NO DAYPART TABLE**'                                  
         GOTO1 REPORT                                                           
         B     FX46                                                             
*                                                                               
FX300    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
FX400    DS    0H                                                               
         B     EXIT                                                             
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          PRINT BUY                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRTBUY   NTR1                                                                   
         USING BUYRECD,R6                                                       
         LA    R4,P                                                             
         USING PLINED,R4                                                        
*                                                                               
         MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(5),WORK                                                     
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         MVC   PPRD,PRD                                                         
*                                                                               
         MVC   PDPT,BDDAYPT                                                     
         MVC   PZNAF,ZONE          ZONE AND AFFIL                               
                                                                                
         GOTO1 HEXOUT,DMCB,BDDAY,PDAY,1                                         
         GOTO1 HEXOUT,DMCB,SAVTIM,PTIMO,4                                       
         GOTO1 HEXOUT,DMCB,BDTIMST,PTIMN,4                                      
         GOTO1 HEXOUT,DMCB,KEY,PKEY,18                                          
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FIXLST   DS    0C                                                               
         DC    C'2N',C'A',X'7F',X'7E',AL2(1700,1900)                            
         DC    C'2N',C'P',X'7F',X'7E',AL2(1900,2200)                            
         DC    C'2N',C'L',X'7F',X'7F',AL2(2200,2400)                            
         DC    C'2N',C'L',X'01',X'7F',AL2(2200,2400)                            
         DC    C'1N',C'A',X'7C',X'7E',AL2(1800,2000)                            
         DC    C'1I',C'A',X'01',X'7F',AL2(1800,2000)                            
         DC    C'1I',C'A',X'7E',X'7F',AL2(1800,2000)                            
         DC    C'1I',C'P',X'01',X'7F',AL2(2000,2300)                            
         DC    C'1I',C'P',X'7E',X'7F',AL2(2000,2300)                            
         DC    X'FF'                                                            
         DS    0D                                                               
COUNT1   DS    F                                                                
COUNT2   DS    F                                                                
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
SAVEKEY  DS    XL18                                                             
ITIME    DS    XL2                                                              
IDAYB    DS    X                                                                
IDPT     DS    X                                                                
ZONE     DS    C                                                                
NETWRK   DS    C                                                                
SAVTIM   DS    XL4                                                              
         EJECT                                                                  
         LTORG                                                                  
DAYLIST  DC    X'4020100804020100'                                              
*                                                                               
         SPACE 3                                                                
*                                  DIRECTORY FOR TIM/DPT TABLES                 
*                                                                               
*           ***NOTE- THESE TABLES COPIED FROM SPREPI502**                       
*                                                                               
*                                  AGYMED(3),TIME ZONE(1),                      
*                                  STATION TYPE(1),SPARE(3)                     
*                                  A(TABLE)                                     
*                                                                               
TIMTDIR  DS    0F                                                               
         DC    C'MCT1I   ',AL4(TTMCT1I)    MCCANN                               
         DC    C'MCT1N   ',AL4(TTMCT1N)                                         
         DC    C'MCT2I   ',AL4(TTMCT2I)                                         
         DC    C'MCT2N   ',AL4(TTMCT2N)                                         
*                                                                               
         DC    C'PET2I   ',AL4(TTPET2I)    PEPSI                                
         DC    C'PET2N   ',AL4(TTPET2N)                                         
         DC    C'PET2F   ',AL4(TTPET2F)     (FOX)                               
         DC    C'PET4I   ',AL4(TTPET4I)                                         
         DC    C'PET4N   ',AL4(TTPET4N)                                         
         DC    C'PET4F   ',AL4(TTPET4F)      (FOX)                              
*                                                                               
         DC    C'CCT1I   ',AL4(TTCCT1I)    COKE                                 
         DC    C'CCT1N   ',AL4(TTCCT1N)                                         
         DC    C'CCT2I   ',AL4(TTCCT2I)                                         
         DC    C'CCT2N   ',AL4(TTCCT2N)                                         
*                                                                               
         DC    C'CKT1I   ',AL4(TTCCT1I)    MORE COKE (CK = CC)                  
         DC    C'CKT1N   ',AL4(TTCCT1N)                                         
         DC    C'CKT2I   ',AL4(TTCCT2I)                                         
         DC    C'CKT2N   ',AL4(TTCCT2N)                                         
*                                                                               
         DC    C'CKRZZ   ',AL4(TTCCRZZ)     COKE RADIO                          
*                                      *** NOTE- OLD WUNDERMAN NOOPED           
*                                          WUNDERMAN                            
*        DC    C'WWT1I   ',AL4(TTWWT1I)                                         
*        DC    C'WWT1N   ',AL4(TTWWT1N)                                         
*        DC    C'WWT2I   ',AL4(TTWWT2I)                                         
*        DC    C'WWT2N   ',AL4(TTWWT2N)                                         
*        DC    C'WWT3I   ',AL4(TTWWT2I)                                         
*        DC    C'WWT3N   ',AL4(TTWWT2N)                                         
*        DC    C'WWT4I   ',AL4(TTWWT1I)                                         
*        DC    C'WWT4N   ',AL4(TTWWT1N)                                         
*                                           NEW WUNDERMAN                       
         DC    C'WWT1Z   ',AL4(TTWWT1Z)                                         
         DC    C'WWT2Z   ',AL4(TTWWT2Z)                                         
         DC    C'WWT3Z   ',AL4(TTWWT2Z)                                         
         DC    C'WWT4Z   ',AL4(TTWWT1Z)                                         
*                                                                               
         DC    C'JWT1I   ',AL4(TTJWT1I)    JWT                                  
         DC    C'JWT1N   ',AL4(TTJWT1N)    JWT                                  
         DC    C'JWT2I   ',AL4(TTJWT1I)                                         
         DC    C'JWT2N   ',AL4(TTJWT1N)                                         
         DC    C'JWT3I   ',AL4(TTJWT2I)                                         
         DC    C'JWT3N   ',AL4(TTJWT2N)                                         
         DC    C'JWT4I   ',AL4(TTJWT2I)                                         
         DC    C'JWT4N   ',AL4(TTJWT2N)                                         
*                                                                               
         DC    C'PBRZZ   ',AL4(TTPBRZZ)    PREMIUM                              
         DC    C'PBTZZ   ',AL4(TTPBTZZ)                                         
*                                                                               
         DC    C'SSRZZ   ',AL4(TTSSZZZ)    SSCB                                 
         DC    C'SSTZZ   ',AL4(TTSSZZZ)                                         
*                                                                               
         DC    C'WITZZ   ',AL4(TTWITZZ)    WILA - TV                            
         DC    C'WIRZZ   ',AL4(TTWIRZZ)    WILA - RADIO                         
*                                                                               
         DC    C'WRTZZ   ',AL4(TTWITZZ)    WILA - TV                            
         DC    C'WRRZZ   ',AL4(TTWIRZZ)    WILA - RADIO                         
*                                                                               
         DC    C'BJT1Z   ',AL4(TTBJT1Z)    BJ                                   
         DC    C'BJT2Z   ',AL4(TTBJT2Z)                                         
         DC    C'BJT3Z   ',AL4(TTBJT2Z)                                         
         DC    C'BJT4Z   ',AL4(TTBJT1Z)                                         
*                                                                               
         DC    C'CZTZZ   ',AL4(TTCZZZZ)    CMI - CZ                             
         DC    C'CZRZZ   ',AL4(TTCZZZZ)    CMI - CZ                             
*                                                                               
         DC    C'AAT1Z   ',AL4(TTAAT1Z)     4 A'S (DRUG CAMPAIGN)               
         DC    C'AAT2Z   ',AL4(TTAAT2Z)     NOTE - 4 A'S IS DEFAULT             
         DC    C'AARZZ   ',AL4(TTAARZZ)     MUST BE AT END OF LIST              
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         SPACE 3                                                                
*                                  TIME/DPT TABLE                               
*              BYTE   0    DAY MASK                                             
*              BYTES  1-2  END TIME                                             
*              BYTE   3    CODE                                                 
*              BYTE   4(10)  DESCRIPTION                                        
*              BYTE   14(4)  MILITARY TIME                                      
*                                                                               
*              NOTE- TIMES CAN START OR END AT 600A,                            
*                    BUT CANNOT SPAN 600A.                                      
*                                                                               
TIMTABL  EQU   18                                                               
*                                                                               
TTMCT1N  DS    0C                   MC/T/EAST-WEST/NETWORK                      
*                                                                               
         DC    X'03',AL2(1859),C'E',CL10'6A-7P ',X'0258076C'                    
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P ',X'02580640'                    
         DC    X'7C',AL2(1859),C'E',CL10'4-7P  ',X'0640076C'                    
         DC    X'7E',AL2(1959),C'A',CL10'7-8P  ',X'076C07D0'                    
         DC    X'7E',AL2(2259),C'P',CL10'8-11P ',X'07D008FC'                    
         DC    X'7F',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'7F',AL2(2559),C'L',CL10'11P-2A',X'08FC00C8'                    
         DC    X'7F',AL2(2959),C'Z',CL10'2A-6A ',X'00C80258'                    
         DC    X'7F',AL2(2959),C'B',CL10'6A-2A ',X'025800C8'  *BONUS*           
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTMCT2N  DS    0C              MC/T/CENTRAL-MOUNTAIN/NETWORK                    
*                                                                               
         DC    X'03',AL2(1759),C'E',CL10'6A-6P ',X'02580708'                    
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P ',X'025805DC'                    
         DC    X'7C',AL2(1759),C'E',CL10'3-6P  ',X'05DC0708'                    
         DC    X'7E',AL2(1859),C'A',CL10'6-7P  ',X'0708076C'                    
         DC    X'7E',AL2(2159),C'P',CL10'7-10P ',X'076C0898'                    
         DC    X'7F',AL2(2159),C'P',CL10'6-10P ',X'07080898'                    
         DC    X'7F',AL2(2459),C'L',CL10'10P-1A',X'08980064'                    
         DC    X'7F',AL2(2959),C'Z',CL10'1A-6A ',X'00640258'                    
         DC    X'7F',AL2(2959),C'B',CL10'6A-2A ',X'025800C8'  *BONUS*           
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTMCT1I  DS    0C                   MC/T/EAST-WEST/INDEPENDENT                  
*                                  (SAME AS NETWORK AFFILIATES)                 
         DC    X'03',AL2(1859),C'E',CL10'6A-7P ',X'0258076C'                    
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P ',X'02580640'                    
         DC    X'7C',AL2(1859),C'E',CL10'4-7P  ',X'0640076C'                    
         DC    X'7F',AL2(1959),C'A',CL10'7-8P  ',X'076C07D0'                    
         DC    X'7F',AL2(2259),C'P',CL10'8-11P ',X'07D008FC'                    
         DC    X'7F',AL2(2559),C'L',CL10'11P-2A',X'08FC00C8'                    
         DC    X'7F',AL2(2959),C'Z',CL10'2A-6A ',X'00C80258'                    
         DC    X'7F',AL2(2959),C'B',CL10'6A-2A ',X'025800C8'  *BONUS*           
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTMCT2I  DS    0C              MC/T/CENTRAL-MOUNTAIN/INDEPENDENT                
*                                                                               
         DC    X'03',AL2(1759),C'E',CL10'6A-6P ',X'02580708'                    
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P ',X'025805DC'                    
         DC    X'7C',AL2(1759),C'E',CL10'3-6P  ',X'05DC0708'                    
         DC    X'7F',AL2(1859),C'A',CL10'6-7P  ',X'0708076C'                    
         DC    X'7F',AL2(2159),C'P',CL10'7-10P ',X'076C0898'                    
         DC    X'7F',AL2(2459),C'L',CL10'10P-1A',X'08980064'                    
         DC    X'7F',AL2(2959),C'Z',CL10'1A-6A ',X'00640258'                    
         DC    X'7F',AL2(2959),C'B',CL10'6A-2A ',X'025800C8'  *BONUS*           
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                                                               
TTPET2N  DS    0C                PEPSI/T/EAST-WEST/NETWORK                      
*                                                                               
         DC    X'03',AL2(1857),C'E',CL10'6A-657P   ',AL2(0600,1857)             
         DC    X'7C',AL2(1557),C'D',CL10'6A-357P   ',AL2(0600,1557)             
         DC    X'7C',AL2(1857),C'E',CL10'358-657P  ',AL2(1558,1857)             
         DC    X'7E',AL2(1957),C'A',CL10'658-757P  ',AL2(1858,1957)             
         DC    X'7E',AL2(2300),C'P',CL10'758-11P   ',AL2(1958,2300)             
         DC    X'01',AL2(2300),C'P',CL10'658-11P   ',AL2(1858,2300)             
         DC    X'7F',AL2(2959),C'L',CL10'1101P-559A',AL2(2301,0559)             
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTPET4N  DS    0C           PEPSI/T/CENTRAL-MOUNTAIN/NETWORK                    
*                                                                               
         DC    X'03',AL2(1757),C'E',CL10'6A-557P   ',AL2(0600,1757)             
         DC    X'7C',AL2(1457),C'D',CL10'6A-257P   ',AL2(0600,1457)             
         DC    X'7C',AL2(1757),C'E',CL10'258-557P  ',AL2(1458,1757)             
         DC    X'7E',AL2(1857),C'A',CL10'558-657P  ',AL2(1758,1857)             
         DC    X'7E',AL2(2200),C'P',CL10'658-10P   ',AL2(1858,2200)             
         DC    X'01',AL2(2200),C'P',CL10'558-10P   ',AL2(1758,2200)             
         DC    X'7F',AL2(2959),C'L',CL10'1001P-559A',AL2(2201,0559)             
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTPET2I  DS    0C                PEPSI/T/EAST-WEST/INDEPENDENT                  
         DC    X'03',AL2(1857),C'I',CL10'6A-657P   ',AL2(0600,1857)             
         DC    X'7C',AL2(1557),C'D',CL10'6A-357P   ',AL2(0600,1557)             
         DC    X'7C',AL2(1857),C'I',CL10'358-657P  ',AL2(1558,1857)             
         DC    X'7E',AL2(1957),C'A',CL10'658-757P  ',AL2(1858,1957)             
         DC    X'7E',AL2(2300),C'N',CL10'758-11P   ',AL2(1958,2300)             
         DC    X'01',AL2(2300),C'N',CL10'658-11P   ',AL2(1858,2300)             
         DC    X'7F',AL2(2959),C'M',CL10'1101P-559A',AL2(2301,0559)             
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTPET4I  DS    0C           PEPSI/T/CENTRAL-MOUNTAIN/INDEPENDENT                
*                                                                               
*                                                                               
         DC    X'03',AL2(1757),C'I',CL10'6A-557P   ',AL2(0600,1757)             
         DC    X'7C',AL2(1457),C'D',CL10'6A-257P   ',AL2(0600,1457)             
         DC    X'7C',AL2(1757),C'I',CL10'258-557P  ',AL2(1458,1757)             
         DC    X'7E',AL2(1857),C'A',CL10'558-657P  ',AL2(1758,1857)             
         DC    X'7E',AL2(2200),C'N',CL10'658-10P   ',AL2(1858,2200)             
         DC    X'01',AL2(2200),C'N',CL10'558-10P   ',AL2(1758,2200)             
         DC    X'7F',AL2(2959),C'M',CL10'1001P-559A',AL2(2201,0559)             
         DC    X'FFFFFFFF'                                                      
*                                                                               
         SPACE 3                                                                
TTPET2F  DS    0C                PEPSI/T/EAST-WEST/FOX                          
*                                (LIKE IND, BUT PRIME IS U, NOT N)              
*                                                                               
         DC    X'03',AL2(1857),C'I',CL10'6A-657P   ',AL2(0600,1857)             
         DC    X'7C',AL2(1557),C'D',CL10'6A-357P   ',AL2(0600,1557)             
         DC    X'7C',AL2(1857),C'I',CL10'358-657P  ',AL2(1558,1857)             
         DC    X'7E',AL2(1957),C'A',CL10'658-757P  ',AL2(1858,1957)             
         DC    X'7E',AL2(2200),C'U',CL10'758-10P   ',AL2(1958,2200)             
         DC    X'7E',AL2(2300),C'N',CL10'1001-11P  ',AL2(2201,2300)             
         DC    X'01',AL2(2300),C'U',CL10'658-11P   ',AL2(1858,2300)             
         DC    X'7F',AL2(2959),C'M',CL10'1101P-559A',AL2(2301,0559)             
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTPET4F  DS    0C           PEPSI/T/CENTRAL-MOUNTAIN/FOX                        
*                           (LIKE IND, BUT PRIME IS U, NOT N)                   
*                                                                               
         DC    X'03',AL2(1757),C'I',CL10'6A-557P   ',AL2(0600,1757)             
         DC    X'7C',AL2(1457),C'D',CL10'6A-257P   ',AL2(0600,1457)             
         DC    X'7C',AL2(1757),C'I',CL10'258-557P  ',AL2(1458,1757)             
         DC    X'7E',AL2(1857),C'A',CL10'558-657P  ',AL2(1758,1857)             
         DC    X'7E',AL2(2100),C'U',CL10'658-9P    ',AL2(1858,2100)             
         DC    X'7E',AL2(2200),C'N',CL10'901-10P   ',AL2(2101,2200)             
         DC    X'01',AL2(2200),C'U',CL10'558-10P   ',AL2(1758,2200)             
         DC    X'7F',AL2(2959),C'M',CL10'1001P-559A',AL2(2201,0559)             
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                                                               
TTCCT1N  DS    0C                 COKE/T/EAST-WEST/NETWORK                      
*                                                                               
         DC    X'03',AL2(1259),C'D',CL10'6A-1P ',AL2(0600,1300)                 
         DC    X'03',AL2(1759),C'E',CL10'1P-6P ',AL2(1300,1800)                 
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P ',AL2(0600,1600)                 
         DC    X'7C',AL2(1759),C'E',CL10'4-6P  ',AL2(1600,1800)                 
         DC    X'7E',AL2(1959),C'A',CL10'6-8P  ',AL2(1800,2000)                 
         DC    X'7E',AL2(2259),C'P',CL10'8-11P ',AL2(2000,2300)                 
         DC    X'01',AL2(1859),C'A',CL10'6-7P  ',AL2(1800,1900)                 
         DC    X'01',AL2(2259),C'P',CL10'7-11P ',AL2(1900,2300)                 
         DC    X'7F',AL2(2459),C'L',CL10'11P-1A',AL2(2300,0100)                 
         DC    X'7F',AL2(2959),C'Z',CL10'1A-6A ',AL2(0100,0600)                 
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTCCT2N  DS    0C            COKE/T/CENTRAL-MOUNTAIN/NETWORK                    
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N ',AL2(0600,1200)                
         DC    X'03',AL2(1659),C'E',CL10'12N-5P ',AL2(1200,1700)                
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P  ',AL2(0600,1500)                
         DC    X'7C',AL2(1659),C'E',CL10'3-5P   ',AL2(1500,1700)                
         DC    X'7E',AL2(1859),C'A',CL10'5-7P   ',AL2(1700,1900)                
         DC    X'7E',AL2(2159),C'P',CL10'7-10P  ',AL2(1900,2200)                
         DC    X'01',AL2(1759),C'A',CL10'5-6P   ',AL2(1700,1800)                
         DC    X'01',AL2(2159),C'P',CL10'6-10P  ',AL2(1800,2200)                
         DC    X'7F',AL2(2359),C'L',CL10'10P-12M',AL2(2200,2400)                
         DC    X'7F',AL2(2959),C'Z',CL10'12M-6A ',AL2(2400,0600)                
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTCCT1I  DS    0C                 COKE/T/EAST-WEST/INDEPENDENT                  
*                                  (SAME AS NETWORK AFFILIATES)                 
         DC    X'03',AL2(1259),C'D',CL10'6A-1P ',AL2(0600,1300)                 
         DC    X'03',AL2(1759),C'E',CL10'1P-6P ',AL2(1300,1800)                 
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P ',AL2(0600,1600)                 
         DC    X'7C',AL2(1759),C'E',CL10'4-6P  ',AL2(1600,1800)                 
         DC    X'7F',AL2(1959),C'A',CL10'6-8P  ',AL2(1800,2000)                 
         DC    X'7F',AL2(2259),C'P',CL10'8-11P ',AL2(2000,2300)                 
         DC    X'7F',AL2(2459),C'L',CL10'11P-1A',AL2(2300,0100)                 
         DC    X'7F',AL2(2959),C'Z',CL10'1A-6A ',AL2(0100,0600)                 
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTCCT2I  DS    0C            COKE/T/CENTRAL-MOUNTAIN/INDEPENDENT                
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N ',AL2(0600,1200)                
         DC    X'03',AL2(1659),C'E',CL10'12N-5P ',AL2(1200,1700)                
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P  ',AL2(0600,1500)                
         DC    X'7C',AL2(1659),C'E',CL10'3-5P   ',AL2(1500,1700)                
         DC    X'7F',AL2(1859),C'A',CL10'5-7P   ',AL2(1700,1900)                
         DC    X'7F',AL2(2159),C'P',CL10'7-10P  ',AL2(1900,2200)                
         DC    X'7F',AL2(2359),C'L',CL10'10P-12M',AL2(2200,2400)                
         DC    X'7F',AL2(2959),C'Z',CL10'12M-6A ',AL2(2400,0600)                
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTCCRZZ  DS    0C            COKE RADIO -ALL TIME ZONES AND STATIONS            
         DC    X'7C',AL2(0959),C'A',CL10'6A-10A',AL2(0600,1000)                 
         DC    X'7C',AL2(1459),C'M',CL10'10A-3P',AL2(1000,1500)                 
         DC    X'7C',AL2(1859),C'P',CL10'3P-7P ',AL2(1500,1900)                 
         DC    X'7C',AL2(2359),C'E',CL10'7P-12M',AL2(1900,2400)                 
         DC    X'03',AL2(2359),C'W',CL10'6A-12M',AL2(0600,2400)                 
         DC    X'7F',AL2(2959),C'X',CL10'12M-6A',AL2(2400,0600)                 
         DC    X'FFFFFFFF'                                                      
*                                                                               
TTWWT1Z  DS    0C            WUNDERMAN/T/EAST-WEST/ALL STATIONS                 
*                                                                               
         DC    X'03',AL2(1559),C'W',CL10'6A-4P ',X'02580640'                    
         DC    X'03',AL2(1759),C'F',CL10'4-6P  ',X'06400708'                    
         DC    X'01',AL2(1859),C'A',CL10'6-7P  ',X'0708076C'                    
         DC    X'01',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'7C',AL2(0859),C'E',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1559),C'D',CL10'9A-4P ',X'03840640'                    
         DC    X'7E',AL2(1759),C'F',CL10'4-6P  ',X'06400708'                    
         DC    X'7E',AL2(1959),C'A',CL10'6-8P  ',X'070807D0'                    
         DC    X'7E',AL2(2259),C'P',CL10'8-11P ',X'07D008FC'                    
         DC    X'7F',AL2(1759),C'F',CL10'4-6P  ',X'06400708'                    
         DC    X'7F',AL2(2559),C'L',CL10'11P-2A',X'08FC00C8'                    
         DC    X'7F',AL2(2959),C'O',CL10'2-6A  ',X'00C80258'                    
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTWWT2Z  DS    0C       WUNDERMAN/T/CENTRAL-MOUNTAIN/ALL STATIONS               
*                                                                               
         DC    X'03',AL2(1459),C'W',CL10'6A-3P ',X'025805DC'                    
         DC    X'03',AL2(1659),C'F',CL10'3-5P  ',X'05DC06A4'                    
         DC    X'01',AL2(1759),C'A',CL10'5-6P  ',X'06A40708'                    
         DC    X'01',AL2(2159),C'P',CL10'6-10P ',X'07080898'                    
         DC    X'7C',AL2(0859),C'E',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P ',X'038405DC'                    
         DC    X'7E',AL2(1659),C'F',CL10'3-5P  ',X'05DC06A4'                    
         DC    X'7E',AL2(1859),C'A',CL10'5-7P  ',X'06A4076C'                    
         DC    X'7E',AL2(2159),C'P',CL10'7-10P ',X'076C0898'                    
         DC    X'7F',AL2(1659),C'F',CL10'3-5P  ',X'05DC06A4'                    
         DC    X'7F',AL2(2559),C'L',CL10'10P-2A',X'089800C8'                    
         DC    X'7F',AL2(2959),C'O',CL10'2-6A  ',X'00C80258'                    
*                                                                               
         DC    X'FFFFFFFF'                                                      
*                                                                               
TTWWT1N  DS    0C            WUNDERMAN/T/EAST-WEST/NETWORK                      
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N',X'025804B0'                    
         DC    X'03',AL2(1859),C'E',CL10'12N-7P',X'04B0076C'                    
         DC    X'03',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'7C',AL2(0859),C'M',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1559),C'D',CL10'9A-4P ',X'03840640'                    
         DC    X'7C',AL2(1929),C'E',CL10'4-730P',X'0640078A'                    
         DC    X'7C',AL2(1959),C'A',CL10'730-8P',X'078A07D0'                    
         DC    X'7C',AL2(2259),C'P',CL10'8-11P ',X'07D008FC'                    
         DC    X'7F',AL2(2959),C'L',CL10'11P-6A',X'08FC0258'                    
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTWWT2N  DS    0C       WUNDERMAN/T/CENTRAL-MOUNTAIN/NETWORK                    
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N',X'025804B0'                    
         DC    X'03',AL2(1859),C'E',CL10'12N-7P',X'04B0076C'                    
         DC    X'03',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'03',AL2(2959),C'L',CL10'11P-6A',X'08FC0258'                    
         DC    X'7C',AL2(0859),C'M',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P ',X'038405DC'                    
         DC    X'7C',AL2(1829),C'E',CL10'3-630P',X'05DC0726'                    
         DC    X'7C',AL2(1859),C'A',CL10'630-7P',X'0726076C'                    
         DC    X'7C',AL2(2159),C'P',CL10'7-10P ',X'076C0898'                    
         DC    X'7C',AL2(2959),C'L',CL10'10P-6A',X'08980258'                    
*                                                                               
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTWWT1I  DS    0C            WUNDERMAN/T/EAST-WEST/INDEPENDENT                  
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N',X'025804B0'                    
         DC    X'03',AL2(1859),C'E',CL10'12N-7P',X'04B0076C'                    
         DC    X'03',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'03',AL2(2959),C'L',CL10'11P-6A',X'08FC0258'                    
         DC    X'7C',AL2(0859),C'M',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1559),C'D',CL10'9A-4P ',X'03840640'                    
         DC    X'7C',AL2(1929),C'E',CL10'4-730P',X'064007A8'                    
         DC    X'7C',AL2(1959),C'A',CL10'730-8P',X'07A807D0'                    
         DC    X'7C',AL2(2959),C'P',CL10'8P-6A ',X'07D00258'                    
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTWWT2I  DS    0C       WUNDERMAN/T/CENTRAL-MOUNTAIN/INDEPENDENT                
*                                                                               
         DC    X'03',AL2(1159),C'D',CL10'6A-12N',X'025804B0'                    
         DC    X'03',AL2(1859),C'E',CL10'12N-7P',X'04B0076C'                    
         DC    X'03',AL2(2259),C'P',CL10'7-11P ',X'076C08FC'                    
         DC    X'03',AL2(2959),C'L',CL10'11P-6A',X'08FC0258'                    
         DC    X'7C',AL2(0859),C'M',CL10'6A-9A ',X'02580384'                    
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P ',X'038405DC'                    
         DC    X'7C',AL2(1829),C'E',CL10'3-630P',X'05DC0726'                    
         DC    X'7C',AL2(1859),C'A',CL10'630-7P',X'0726076C'                    
         DC    X'7C',AL2(2959),C'P',CL10'7P-6A ',X'076C0258'                    
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                  JW/T/TZ 1+2 (E+W) /IND STATIONS              
TTJWT1I  DS    0C                                                               
         DC    X'01',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'01',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'01',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'01',AL2(2259),C'I',CL10'7P-11P  ',AL2(1900,2300)               
         DC    X'03',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'03',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'03',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'7C',AL2(0859),C'B',CL10'6A-9A   ',AL2(0600,0900)               
         DC    X'7C',AL2(1559),C'D',CL10'9A-4P   ',AL2(0900,1600)               
         DC    X'7C',AL2(1759),C'E',CL10'4P-6P   ',AL2(1600,1800)               
         DC    X'7F',AL2(1859),C'M',CL10'6P-7P   ',AL2(1800,1900)               
         DC    X'7E',AL2(1959),C'A',CL10'7P-8P   ',AL2(1900,2000)               
         DC    X'7E',AL2(2259),C'I',CL10'8P-11P  ',AL2(2000,2300)               
         DC    X'7F',AL2(2329),C'N',CL10'11-1130P',AL2(2300,2330)               
         DC    X'7F',AL2(2559),C'L',CL10'1130P-2A',AL2(2330,0200)               
         DC    X'7F',AL2(2959),C'X',CL10'2A-6A   ',AL2(0200,0600)               
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                  JW/T/TZ 1+2 (E+W) /NET STATIONS              
TTJWT1N  DS    0C                                                               
         DC    X'01',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'01',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'01',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'01',AL2(2259),C'P',CL10'7P-11P  ',AL2(1900,2300)               
         DC    X'03',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'03',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'03',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'7C',AL2(0859),C'B',CL10'6A-9A   ',AL2(0600,0900)               
         DC    X'7C',AL2(1559),C'D',CL10'9A-4P   ',AL2(0900,1600)               
         DC    X'7C',AL2(1759),C'E',CL10'4P-6P   ',AL2(1600,1800)               
         DC    X'7F',AL2(1859),C'M',CL10'6P-7P   ',AL2(1800,1900)               
         DC    X'7E',AL2(1959),C'A',CL10'7P-8P   ',AL2(1900,2000)               
         DC    X'7E',AL2(2259),C'P',CL10'8P-11P  ',AL2(2000,2300)               
         DC    X'7F',AL2(2329),C'N',CL10'11-1130P',AL2(2300,2330)               
         DC    X'7F',AL2(2559),C'L',CL10'1130P-2A',AL2(2330,0200)               
         DC    X'7F',AL2(2959),C'X',CL10'2A-6A   ',AL2(0200,0600)               
         DC    X'FFFFFFFF'                                                      
*                                  JW/T/TZ 3+4 (C+M) /IND STATIONS              
TTJWT2I  DS    0C                                                               
         DC    X'01',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'01',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'01',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'01',AL2(2159),C'I',CL10'6P-10P  ',AL2(1800,2200)               
         DC    X'03',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'03',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'03',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'7C',AL2(0859),C'B',CL10'6A-9A   ',AL2(0600,0900)               
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P   ',AL2(0900,1500)               
         DC    X'7C',AL2(1659),C'E',CL10'3P-5P   ',AL2(1500,1700)               
         DC    X'7C',AL2(1759),C'M',CL10'5P-6P   ',AL2(1700,1800)               
         DC    X'7E',AL2(1859),C'A',CL10'6P-7P   ',AL2(1800,1900)               
         DC    X'7E',AL2(2159),C'I',CL10'7P-10P  ',AL2(1900,2200)               
         DC    X'7F',AL2(1759),C'M',CL10'5P-6P   ',AL2(1700,1800)               
         DC    X'7F',AL2(2229),C'N',CL10'10-1030P',AL2(2200,2230)               
         DC    X'7F',AL2(2459),C'L',CL10'1030P-1A',AL2(2230,0100)               
         DC    X'7F',AL2(2959),C'X',CL10'1A-6A   ',AL2(0100,0600)               
         DC    X'FFFFFFFF'                                                      
*                                  JW/T/TZ 3+4 (C+M) /NET STATIONS              
TTJWT2N  DS    0C                                                               
         DC    X'01',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'01',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'01',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'01',AL2(2159),C'P',CL10'6P-10P  ',AL2(1800,2200)               
         DC    X'03',AL2(0659),C'X',CL10'6A-7A   ',AL2(0600,0700)               
         DC    X'03',AL2(0859),C'K',CL10'7A-9A   ',AL2(0700,0900)               
         DC    X'03',AL2(1759),C'W',CL10'9A-6P   ',AL2(0900,1800)               
         DC    X'7C',AL2(0859),C'B',CL10'6A-9A   ',AL2(0600,0900)               
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P   ',AL2(0900,1500)               
         DC    X'7C',AL2(1659),C'E',CL10'3P-5P   ',AL2(1500,1700)               
         DC    X'7C',AL2(1759),C'M',CL10'5P-6P   ',AL2(1700,1800)               
         DC    X'7E',AL2(1859),C'A',CL10'6P-7P   ',AL2(1800,1900)               
         DC    X'7E',AL2(2159),C'P',CL10'7P-10P  ',AL2(1900,2200)               
         DC    X'7F',AL2(1759),C'M',CL10'5P-6P   ',AL2(1700,1800)               
         DC    X'7F',AL2(2229),C'N',CL10'10-1030P',AL2(2200,2230)               
         DC    X'7F',AL2(2459),C'L',CL10'1030P-1A',AL2(2230,0100)               
         DC    X'7F',AL2(2959),C'X',CL10'1A-6A   ',AL2(0100,0600)               
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               PREMIUM INC (PB) TV                             
TTPBTZZ  DS    0C                                                               
         DC    X'7F',AL2(1559),C'D',CL10'6A-4P   ',X'02580640'                  
         DC    X'7F',AL2(1859),C'E',CL10'4P-7P   ',X'0640076C'                  
         DC    X'7F',AL2(1959),C'A',CL10'7P-8P   ',X'076C07D0'                  
         DC    X'7F',AL2(2259),C'P',CL10'8P-11P  ',X'07D008FC'                  
         DC    X'7F',AL2(2959),C'L',CL10'11P-6A  ',X'08FC0258'                  
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               PREMIUM INC (PB) RADIO                          
TTPBRZZ  DS    0C                                                               
         DC    X'7F',AL2(2959),C'R',CL10'6A-559A ',X'02580257'                  
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               SSCB - TV AND RADIO                             
TTSSZZZ  DS    0C                                                               
         DC    X'03',AL2(2959),C'W',CL10'6A-559A ',X'02580257'                  
         DC    X'7C',AL2(0959),C'A',CL10'530A-10A',X'021203E8'                  
         DC    X'7C',AL2(1459),C'H',CL10'10A-3P  ',X'03E805DC'                  
         DC    X'7C',AL2(1959),C'P',CL10'3P-8P   ',X'05DC07D0'                  
         DC    X'7C',AL2(2929),C'N',CL10'8P-530A ',X'07D00212'                  
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               WILA - TV                                       
TTWITZZ  DS    0C                                                               
         DC    X'7F',AL2(0859),C'M',CL10'6A-9A   ',X'02580384'                  
         DC    X'7F',AL2(1559),C'D',CL10'9A-4P   ',X'03840640'                  
         DC    X'7F',AL2(1859),C'F',CL10'4P-7P   ',X'0640076C'                  
         DC    X'7F',AL2(1959),C'A',CL10'7P-8P   ',X'076C07D0'                  
         DC    X'7F',AL2(2259),C'P',CL10'8P-11P  ',X'07D008FC'                  
         DC    X'7F',AL2(2329),C'N',CL10'11-1130P',X'08FC091A'                  
         DC    X'7F',AL2(2959),C'L',CL10'1130P-6A',X'091A0258'                  
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                               WILA - RADIO                                    
*                                                                               
*            NOTE- DAYPART A IS SPLIT BECAUSE 5A-10A HAS TO                     
*                  TREATED AS 5-6A AND 6-10A. NOTE THAT IN THE                  
*                  2ND ENTRY FOR A THE END TIME (AT +1) IS 6A, NOT 10A          
*                                                                               
TTWIRZZ  DS    0C                                                               
         DC    X'7F',AL2(0959),C'A',CL10'5A-10A  ',AL2(0500,1000)               
         DC    X'7F',AL2(1459),C'D',CL10'10A-3P  ',AL2(1000,1500)               
         DC    X'7F',AL2(1959),C'P',CL10'3P-8P   ',AL2(1500,2000)               
         DC    X'7F',AL2(2359),C'E',CL10'8P-12M  ',AL2(2000,2400)               
         DC    X'7F',AL2(2859),C'L',CL10'12M-5A  ',AL2(2400,0500)               
         DC    X'7F',AL2(2959),C'A',CL10'5A-10A  ',AL2(0500,1000)               
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                  BJ/T/TZ 1+4/ALL STATIONS                     
TTBJT1Z  DS    0C                                                               
         DC    X'03',AL2(1159),C'W',CL10'6A-12N  ',AL2(0600),AL2(1200)          
         DC    X'03',AL2(1559),C'X',CL10'12N-4P  ',AL2(1200),AL2(1600)          
         DC    X'03',AL2(1859),C'J',CL10'4-7P    ',AL2(1600),AL2(1900)          
         DC    X'03',AL2(2259),C'P',CL10'7-11P   ',AL2(1900),AL2(2300)          
         DC    X'7C',AL2(0859),C'M',CL10'6-9A    ',AL2(0600),AL2(0900)          
         DC    X'7C',AL2(1159),C'E',CL10'9A-12N  ',AL2(0900),AL2(1200)          
         DC    X'7C',AL2(1559),C'D',CL10'12N-4P  ',AL2(1200),AL2(1600)          
         DC    X'7C',AL2(1959),C'A',CL10'4-8P    ',AL2(1600),AL2(2000)          
         DC    X'7C',AL2(2259),C'P',CL10'8-11P   ',AL2(2000),AL2(2300)          
         DC    X'7F',AL2(2559),C'L',CL10'11P-2A  ',AL2(2300),AL2(0200)          
         DC    X'7F',AL2(2959),C'V',CL10'2-6A    ',AL2(0200),AL2(0600)          
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                  BJ/T/TZ 2+3/ALL STATIONS                     
TTBJT2Z  DS    0C                                                               
         DC    X'03',AL2(1159),C'W',CL10'6A-12N  ',AL2(0600),AL2(1200)          
         DC    X'03',AL2(1559),C'X',CL10'12N-4P  ',AL2(1200),AL2(1600)          
         DC    X'03',AL2(1859),C'J',CL10'4-7P    ',AL2(1600),AL2(1900)          
         DC    X'03',AL2(2159),C'P',CL10'7-10P   ',AL2(1900),AL2(2200)          
         DC    X'7C',AL2(0859),C'M',CL10'6-9A    ',AL2(0600),AL2(0900)          
         DC    X'7C',AL2(1159),C'E',CL10'9A-12N  ',AL2(0900),AL2(1200)          
         DC    X'7C',AL2(1559),C'D',CL10'12N-4P  ',AL2(1200),AL2(1600)          
         DC    X'7C',AL2(1859),C'A',CL10'4-7P    ',AL2(1600),AL2(1900)          
         DC    X'7C',AL2(2159),C'P',CL10'7-10P   ',AL2(1900),AL2(2200)          
         DC    X'7F',AL2(2559),C'L',CL10'10P-2A  ',AL2(2200),AL2(0200)          
         DC    X'7F',AL2(2959),C'V',CL10'2-6A    ',AL2(0200),AL2(0600)          
         DC    X'FFFFFFFF'                                                      
*                                                                               
*                                  CZ/T+R/ALL TZ/ALL STATIONS                   
TTCZZZZ  DS    0C                                                               
         DC    X'03',AL2(2559),C'W',CL10'5A-2A   ',AL2(0500),AL2(0200)          
         DC    X'7C',AL2(0859),C'M',CL10'6-9A    ',AL2(0500),AL2(0900)          
         DC    X'7C',AL2(1459),C'D',CL10'9A-3P   ',AL2(0900),AL2(1500)          
         DC    X'7C',AL2(1659),C'E',CL10'3-5P    ',AL2(1500),AL2(1700)          
         DC    X'7C',AL2(1859),C'Y',CL10'5-7P    ',AL2(1700),AL2(1900)          
         DC    X'7C',AL2(1959),C'A',CL10'7-8P    ',AL2(1900),AL2(2000)          
         DC    X'7C',AL2(2259),C'P',CL10'8-11P   ',AL2(2000),AL2(2300)          
         DC    X'7C',AL2(2329),C'V',CL10'11-1130P',AL2(2300),AL2(2330)          
         DC    X'7C',AL2(2559),C'L',CL10'1130P-2A',AL2(2330),AL2(0200)          
         DC    X'7C',AL2(2859),C'I',CL10'2-5A    ',AL2(0200),AL2(0500)          
         DC    X'7C',AL2(2959),C'M',CL10'5-6A    ',AL2(0500),AL2(0600)          
         DC    X'7F',AL2(2859),C'I',CL10'2-5A    ',AL2(0200),AL2(0500)          
         DC    X'FFFFFFFF'                                                      
*                                                                               
TTAAT1Z  DS    0C                   AA/T/EAST-WEST  (4 A'S)                     
*                                                                               
         DC    X'01',AL2(1859),C'E',CL10'6A-7P  ',X'0258076C'                   
         DC    X'01',AL2(2259),C'P',CL10'7-11P  ',X'076C08FC'                   
         DC    X'02',AL2(1929),C'E',CL10'6A-730P',X'0258078A'                   
         DC    X'7C',AL2(1559),C'D',CL10'6A-4P  ',X'02580640'                   
         DC    X'7C',AL2(1929),C'E',CL10'4-730P ',X'0640078A'                   
         DC    X'7E',AL2(1959),C'A',CL10'730-8P ',X'078A07D0'                   
         DC    X'7E',AL2(2259),C'P',CL10'8-11P  ',X'07D008FC'                   
         DC    X'7F',AL2(2959),C'L',CL10'11P-6A ',X'08FC0258'                   
         DC    X'FFFFFFFF'                                                      
         SPACE 3                                                                
TTAAT2Z  DS    0C              AA/T/CENTRAL-MOUNTAIN                            
*                                                                               
         DC    X'01',AL2(1759),C'E',CL10'6A-6P  ',X'02580708'                   
         DC    X'01',AL2(2159),C'P',CL10'6-10P  ',X'07080898'                   
         DC    X'02',AL2(1829),C'E',CL10'6A-630P',X'02580726'                   
         DC    X'7C',AL2(1459),C'D',CL10'6A-3P  ',X'025805DC'                   
         DC    X'7C',AL2(1829),C'E',CL10'3-630P ',X'05DC0726'                   
         DC    X'7E',AL2(1859),C'A',CL10'630-7P ',X'0726076C'                   
         DC    X'7E',AL2(2159),C'P',CL10'7-10P  ',X'076C0898'                   
         DC    X'7F',AL2(2959),C'L',CL10'10P-6A ',X'08980258'                   
         DC    X'FFFFFFFF'                                                      
*                               AA RADIO                                        
TTAARZZ  DS    0C                                                               
         DC    X'7F',AL2(1959),C'R',CL10'6A-8P   ',X'025807D0'                  
         DC    X'7F',AL2(2959),C'R',CL10'8P-6A   ',X'07D00258'                  
         DC    X'FFFFFFFF'                                                      
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PDPT     DS    CL1                                                              
         DS    CL1                                                              
PDAY     DS    CL2                                                              
         DS    CL1                                                              
PZNAF    DS    CL2                                                              
         DS    CL1                                                              
PTIMO    DS    CL8                                                              
         DS    CL1                                                              
PTIMN    DS    CL8                                                              
         DS    CL1                                                              
PKEY     DS    CL36                BUY LINE KEY                                 
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPREPFXDP405/01/02'                                      
         END                                                                    
