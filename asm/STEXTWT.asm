*          DATA SET STEXTWT    AT LEVEL 002 AS OF 04/05/90                      
*          DATA SET SPEXTWT    AT LEVEL 024 AS OF 04/02/90                      
*PHASE STEXTWT,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* SPOTG MARKET FIX FOR WITO RADIO STATIONS                                      
* MAR30/90                                                                      
         SPACE                                                                  
         SPACE                                                                  
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         LA    R2,MSTABLEN                                                      
         LA    R3,MSTABLE                                                       
         SPACE                                                                  
INIT10   LA    R4,MSTABLEN                                                      
         LA    R5,MSTABLE                                                       
         SPACE                                                                  
INIT20   CR    R3,R5               SAME ENTRY                                   
         BE    INIT24                                                           
         CLC   3(2,R3),5(R5)       DUPE MKT                                     
         BNE   INIT22                                                           
         MVC   P(7),=C'STATION'                                                 
         MVC   P+8(5),0(R3)                                                     
         SR    R0,R0                                                            
         ICM   R0,3,3(R3)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   P+20(10),=C'OLD MARKET'                                          
         UNPK  P+31(4),DUB                                                      
         MVC   P+40(9),=C'OTHER STA'                                            
         MVC   P+50(5),0(R5)                                                    
         SR    R0,R0                                                            
         ICM   R0,3,5(R5)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   P+20(10),=C'NEW MARKET'                                          
         UNPK  P+31(4),DUB                                                      
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
INIT22   CLC   0(5,R3),0(R5)  SAME STATION?                                     
         BNE   INIT24                                                           
         MVC   P+10(9),=C'EQUAL STA'                                            
         MVC   P+20(5),0(R3)                                                    
         MVC   P+30(5),0(R5)                                                    
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
INIT24   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         SPACE                                                                  
         AP    TOTRD,=P'1'                                                      
         SPACE                                                                  
         USING SPGENSTAD,R3                                                     
         CLI   STAKTYPE,C'S'       STATION REC?                                 
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         CLI   STAKMED,C'R'        MEDIA RADIO                                  
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         CLC   STAKAGY,=C'WT'      WITO                                         
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         SPACE                                                                  
         AP    TSTACT,=P'1'                                                     
         SPACE                                                                  
         LA    R4,MSTABLEN                                                      
         LA    R5,MSTABLE                                                       
         USING MSTABLED,R5                                                      
STSTA10  CLC   STAKCALL,MSTA                                                    
         BNE   STSTA14                                                          
         SR    R0,R0                                                            
         ICM   R0,3,MOLDMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         CLC   SMKT(2),WORK                                                     
         BE    STSTA16                                                          
         MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'STATION'                                              
         MVC   P+18(5),MSTA                                                     
         MVC   P+10(6),=C'MARKET'                                               
         MVC   P+37(4),SMKT                                                     
         MVC   P+50(13),=C'TABLE OLD MKT'                                       
         ICM   R0,3,MOLDMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+65(4),DUB                                                      
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'STA RECCORD'                                            
         B     PRT                                                              
         SPACE                                                                  
STSTA14  LA    R5,MSTABNXT                                                      
         BCT   R4,STSTA10                                                       
         SPACE                                                                  
         MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'MISSING'                                              
         MVC   P+20(5),STAKCALL                                                 
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'STA RECCORD'                                            
         B     DMXKEEP                                                          
STSTA16  ICM   R0,3,MNEWMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SMKT,DUB                                                         
         AP    TSTANEW,=P'1'                                                    
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA NEW MKT'                                            
         B     PRT                                                              
         DROP  R3                                                               
         SPACE                                                                  
PRT      DS    0H                                                               
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
         SPACE                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(100),RTITLE                                                    
         GOTO1 VPRINTER                                                         
         LA    R2,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF10 MVC   P+5(28),5(R3)                                                    
         EDIT  (P5,0(R3)),(8,P+33)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,33(,R3)                                                       
         BCT   R2,DMXEOF10                                                      
         SPACE                                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
TOTRD    DC    PL5'0',CL28'TOT RECS READ'                                       
TSTACT   DC    PL5'0',CL28'TOT STA RECS'                                        
TSTANEW  DC    PL5'0',CL28'TOT STAS NEW'                                        
TOTCTRS  EQU   (*-TOTRD)/33                                                     
          SPACE                                                                 
WORK     DS    CL64                                                             
         SPACE                                                                  
RTITLE   DC    CL100'RADIO STATION REC MARKET FIX FOR WITO '                    
         LTORG                                                                  
          SPACE                                                                 
MSTABLE   DC   CL5'CFVRA',AL2(2100),AL2(0001),XL3'00'                           
          DC   CL5'CHOOA',AL2(0990),AL2(0005),XL3'00'                           
          DC   CL5'CFGTA',AL2(0430),AL2(0010),XL3'00'                           
          DC   CL5'CBG A',AL2(0040),AL2(0025),XL3'00'                           
          DC   CL5'CFAMA',AL2(0040),AL2(0025),XL3'00'                           
          DC   CL5'CFYQA',AL2(0040),AL2(0025),XL3'00'                           
          DC   CL5'CKGAA',AL2(0040),AL2(0025),XL3'00'                           
          DC   CL5'CKDHA',AL2(0170),AL2(0045),XL3'00'                           
          DC   CL5'CKPCF',AL2(0170),AL2(0045),XL3'00'                           
          DC   CL5'CHADA',AL2(0440),AL2(0055),XL3'00'                           
          DC   CL5'CAVRA',AL2(0225),AL2(0065),XL3'00'                           
          DC   CL5'CFJXA',AL2(0180),AL2(0075),XL3'00'                           
          DC   CL5'CFJXF',AL2(0180),AL2(0075),XL3'00'                           
          DC   CL5'CJANA',AL2(0450),AL2(0085),XL3'00'                           
          DC   CL5'CKPBA',AL2(0460),AL2(0090),XL3'00'                           
          DC   CL5'CKIMA',AL2(0010),AL2(0095),XL3'00'                           
          DC   CL5'CHAYF',AL2(1000),AL2(0126),XL3'00'                           
          DC   CL5'CIDCF',AL2(1000),AL2(0126),XL3'00'                           
          DC   CL5'CKBBA',AL2(1000),AL2(0126),XL3'00'                           
          DC   CL5'CKBCA',AL2(0310),AL2(0135),XL3'00'                           
          DC   CL5'CIGLF',AL2(1010),AL2(0145),XL3'00'                           
          DC   CL5'CJBQA',AL2(1010),AL2(0145),XL3'00'                           
          DC   CL5'CJNHA',AL2(1010),AL2(0145),XL3'00'                           
          DC   CL5'CJTNA',AL2(1010),AL2(0145),XL3'00'                           
          DC   CL5'CQUIA',AL2(1010),AL2(0145),XL3'00'                           
          DC   CL5'CFTLF',AL2(1020),AL2(0155),XL3'00'                           
          DC   CL5'CJPRA',AL2(1890),AL2(0165),XL3'00'                           
          DC   CL5'CJNRA',AL2(1030),AL2(0175),XL3'00'                           
          DC   CL5'CJRBA',AL2(1640),AL2(0185),XL3'00'                           
          DC   CL5'CHICA',AL2(1040),AL2(0195),XL3'00'                           
          DC   CL5'CJCMF',AL2(1650),AL2(0205),XL3'00'                           
          DC   CL5'CKLQA',AL2(1650),AL2(0205),XL3'00'                           
          DC   CL5'CKX A',AL2(1650),AL2(0205),XL3'00'                           
          DC   CL5'CKX F',AL2(1650),AL2(0205),XL3'00'                           
          DC   CL5'CKPCA',AL2(1050),AL2(0215),XL3'00'                           
          DC   CL5'CKBWA',AL2(0190),AL2(0226),XL3'00'                           
          DC   CL5'CFJRA',AL2(1060),AL2(0235),XL3'00'                           
          DC   CL5'CIBQA',AL2(1900),AL2(0245),XL3'00'                           
          DC   CL5'CKBRA',AL2(1900),AL2(0245),XL3'00'                           
          DC   CL5'CINGF',AL2(1070),AL2(0255),XL3'00'                           
          DC   CL5'CFLDA',AL2(2110),AL2(0265),XL3'00'                           
          DC   CL5'CBR A',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CBR F',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CCKOF',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CFACA',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CFCNA',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CFFRA',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CHFMF',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CHQRA',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CJAYF',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CKIKF',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CKRYF',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CKXLF',AL2(1910),AL2(0275),XL3'00'                           
          DC   CL5'CFTJA',AL2(1080),AL2(0285),XL3'00'                           
          DC   CL5'CFWBA',AL2(2120),AL2(0295),XL3'00'                           
          DC   CL5'CKNBA',AL2(0320),AL2(0305),XL3'00'                           
          DC   CL5'CFCWA',AL2(1920),AL2(0315),XL3'00'                           
          DC   CL5'CJVAA',AL2(0330),AL2(0325),XL3'00'                           
          DC   CL5'CKQRA',AL2(2130),AL2(0335),XL3'00'                           
          DC   CL5'CBC F',AL2(0150),AL2(0345),XL3'00'                           
          DC   CL5'CFCYA',AL2(0150),AL2(0345),XL3'00'                           
          DC   CL5'CFOYA',AL2(0150),AL2(0345),XL3'00'                           
          DC   CL5'CHLQF',AL2(0150),AL2(0345),XL3'00'                           
          DC   CL5'CHTNA',AL2(0150),AL2(0345),XL3'00'                           
          DC   CL5'CFCOA',AL2(1090),AL2(0355),XL3'00'                           
          DC   CL5'CKSYF',AL2(1090),AL2(0355),XL3'00'                           
          DC   CL5'CJMDA',AL2(0470),AL2(0365),XL3'00'                           
          DC   CL5'CBJ A',AL2(0480),AL2(0375),XL3'00'                           
          DC   CL5'CBJ F',AL2(0480),AL2(0375),XL3'00'                           
          DC   CL5'CFIXF',AL2(0480),AL2(0375),XL3'00'                           
          DC   CL5'CJMTA',AL2(0480),AL2(0375),XL3'00'                           
          DC   CL5'CHWKA',AL2(2140),AL2(0385),XL3'00'                           
          DC   CL5'CKVOA',AL2(0020),AL2(0395),XL3'00'                           
          DC   CL5'CFMXF',AL2(1100),AL2(0405),XL3'00'                           
          DC   CL5'CHUCA',AL2(1100),AL2(0405),XL3'00'                           
          DC   CL5'CKCBA',AL2(1110),AL2(0415),XL3'00'                           
          DC   CL5'CBY A',AL2(0030),AL2(0425),XL3'00'                           
          DC   CL5'CFCBA',AL2(0030),AL2(0425),XL3'00'                           
          DC   CL5'CKWKA',AL2(0030),AL2(0425),XL3'00'                           
          DC   CL5'CFIXA',AL2(1120),AL2(0435),XL3'00'                           
          DC   CL5'CFLGF',AL2(1120),AL2(0435),XL3'00'                           
          DC   CL5'CJJSA',AL2(1120),AL2(0435),XL3'00'                           
          DC   CL5'CFCPA',AL2(2150),AL2(0445),XL3'00'                           
          DC   CL5'CKEKA',AL2(2160),AL2(0455),XL3'00'                           
          DC   CL5'CFKCA',AL2(2170),AL2(0465),XL3'00'                           
          DC   CL5'CFNCA',AL2(1660),AL2(0465),XL3'00'                           
          DC   CL5'CFDRA',AL2(0200),AL2(0485),XL3'00'                           
          DC   CL5'CKDMA',AL2(1670),AL2(0495),XL3'00'                           
          DC   CL5'CJDCA',AL2(2180),AL2(0506),XL3'00'                           
          DC   CL5'CKDYA',AL2(0210),AL2(0525),XL3'00'                           
          DC   CL5'CJLPA',AL2(0490),AL2(0535),XL3'00'                           
          DC   CL5'CHVDA',AL2(0500),AL2(0545),XL3'00'                           
          DC   CL5'CHRDA',AL2(0505),AL2(0565),XL3'00'                           
          DC   CL5'CKRVA',AL2(0505),AL2(0565),XL3'00'                           
          DC   CL5'CKDRA',AL2(1130),AL2(0575),XL3'00'                           
          DC   CL5'CKAYA',AL2(2190),AL2(0585),XL3'00'                           
          DC   CL5'CBX A',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CBX F',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CFRNA',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CHEDA',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CHFAA',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CHQTA',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CIRKF',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CISNF',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CJCAA',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CJCAF',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CJOVA',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CKERA',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CKNGF',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CKRAF',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CKSTA',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CKVAA',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CKVAF',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CKXMF',AL2(1940),AL2(0595),XL3'00'                           
          DC   CL5'CJEMA',AL2(0340),AL2(0605),XL3'00'                           
          DC   CL5'CJYRA',AL2(1950),AL2(0615),XL3'00'                           
          DC   CL5'CKNRA',AL2(1140),AL2(0626),XL3'00'                           
          DC   CL5'CKNSA',AL2(1150),AL2(0635),XL3'00'                           
          DC   CL5'CJSLA',AL2(1760),AL2(0645),XL3'00'                           
          DC   CL5'CFEKA',AL2(2200),AL2(0655),XL3'00'                           
          DC   CL5'CFARA',AL2(1680),AL2(0665),XL3'00'                           
          DC   CL5'CFOBA',AL2(1160),AL2(0675),XL3'00'                           
          DC   CL5'CJOKA',AL2(1960),AL2(0685),XL3'00'                           
          DC   CL5'CFNLA',AL2(2210),AL2(0695),XL3'00'                           
          DC   CL5'CKNLA',AL2(2220),AL2(0706),XL3'00'                           
          DC   CL5'CBZ A',AL2(0350),AL2(0715),XL3'00'                           
          DC   CL5'CBZ F',AL2(0350),AL2(0715),XL3'00'                           
          DC   CL5'CFNBA',AL2(0350),AL2(0715),XL3'00'                           
          DC   CL5'CIHIA',AL2(0350),AL2(0715),XL3'00'                           
          DC   CL5'CKHJA',AL2(0350),AL2(0715),XL3'00'                           
          DC   CL5'CKGRA',AL2(2225),AL2(0725),XL3'00'                           
          DC   CL5'CFGQA',AL2(0050),AL2(0735),XL3'00'                           
          DC   CL5'CFLNA',AL2(0050),AL2(0735),XL3'00'                           
          DC   CL5'CHEFA',AL2(0510),AL2(0745),XL3'00'                           
          DC   CL5'CKYQA',AL2(0060),AL2(0755),XL3'00'                           
          DC   CL5'CFGPA',AL2(1970),AL2(0765),XL3'00'                           
          DC   CL5'CJXXA',AL2(1970),AL2(0765),XL3'00'                           
          DC   CL5'CBT A',AL2(0070),AL2(0775),XL3'00'                           
          DC   CL5'CIYQA',AL2(0070),AL2(0775),XL3'00'                           
          DC   CL5'CKCMA',AL2(0070),AL2(0775),XL3'00'                           
          DC   CL5'CGIAA',AL2(0125),AL2(0795),XL3'00'                           
          DC   CL5'CJOYA',AL2(1170),AL2(0805),XL3'00'                           
          DC   CL5'CKLAF',AL2(1170),AL2(0805),XL3'00'                           
          DC   CL5'CBH A',AL2(0220),AL2(0815),XL3'00'                           
          DC   CL5'CBH F',AL2(0220),AL2(0815),XL3'00'                           
          DC   CL5'CFRQF',AL2(0220),AL2(0815),XL3'00'                           
          DC   CL5'CHFXA',AL2(0220),AL2(0815),XL3'00'                           
          DC   CL5'CHFXF',AL2(0220),AL2(0815),XL3'00'                           
          DC   CL5'CHNSA',AL2(0220),AL2(0815),XL3'00'                           
          DC   CL5'CIOOF',AL2(0220),AL2(0815),XL3'00'                           
          DC   CL5'CJCHA',AL2(0220),AL2(0815),XL3'00'                           
          DC   CL5'CHKOA',AL2(0220),AL2(0815),XL3'00'                           
          DC   CL5'CHKOF',AL2(0220),AL2(0815),XL3'00'                           
          DC   CL5'CHAMA',AL2(1180),AL2(0825),XL3'00'                           
          DC   CL5'CHMLA',AL2(1180),AL2(0825),XL3'00'                           
          DC   CL5'CJJDA',AL2(1180),AL2(0825),XL3'00'                           
          DC   CL5'CKDSF',AL2(1180),AL2(0825),XL3'00'                           
          DC   CL5'CKLHF',AL2(1180),AL2(0825),XL3'00'                           
          DC   CL5'CKOCA',AL2(1180),AL2(0825),XL3'00'                           
          DC   CL5'CHLCA',AL2(0520),AL2(0835),XL3'00'                           
          DC   CL5'CHPRA',AL2(1190),AL2(0845),XL3'00'                           
          DC   CL5'CFHGA',AL2(2240),AL2(0855),XL3'00'                           
          DC   CL5'CFCTA',AL2(2250),AL2(0865),XL3'00'                           
          DC   CL5'CKGOA',AL2(2250),AL2(0865),XL3'00'                           
          DC   CL5'CIMFF',AL2(0530),AL2(0875),XL3'00'                           
          DC   CL5'CKCHA',AL2(0530),AL2(0875),XL3'00'                           
          DC   CL5'CFBKA',AL2(1200),AL2(0885),XL3'00'                           
          DC   CL5'CFWLF',AL2(2260),AL2(0895),XL3'00'                           
          DC   CL5'CJLMA',AL2(0540),AL2(0905),XL3'00'                           
          DC   CL5'CHOCF',AL2(0550),AL2(0915),XL3'00'                           
          DC   CL5'CKRSA',AL2(0550),AL2(0915),XL3'00'                           
          DC   CL5'CFFMF',AL2(2270),AL2(0925),XL3'00'                           
          DC   CL5'CFJCA',AL2(2270),AL2(0925),XL3'00'                           
          DC   CL5'CHNLA',AL2(2270),AL2(0925),XL3'00'                           
          DC   CL5'CHRKF',AL2(2270),AL2(0925),XL3'00'                           
          DC   CL5'CIFMF',AL2(2270),AL2(0925),XL3'00'                           
          DC   CL5'CKAPA',AL2(1210),AL2(0935),XL3'00'                           
          DC   CL5'CHIMF',AL2(2280),AL2(0945),XL3'00'                           
          DC   CL5'CILKF',AL2(2280),AL2(0945),XL3'00'                           
          DC   CL5'CKIQA',AL2(2280),AL2(0945),XL3'00'                           
          DC   CL5'CKOVA',AL2(2280),AL2(0945),XL3'00'                           
          DC   CL5'CJRLA',AL2(1220),AL2(0955),XL3'00'                           
          DC   CL5'CKENA',AL2(0230),AL2(0965),XL3'00'                           
          DC   CL5'CKWMF',AL2(0230),AL2(0965),XL3'00'                           
          DC   CL5'CFFXA',AL2(1230),AL2(0975),XL3'00'                           
          DC   CL5'CFLYA',AL2(1230),AL2(0975),XL3'00'                           
          DC   CL5'CFMKF',AL2(1230),AL2(0975),XL3'00'                           
          DC   CL5'CFRCA',AL2(1230),AL2(0975),XL3'00'                           
          DC   CL5'CFRCF',AL2(1230),AL2(0975),XL3'00'                           
          DC   CL5'CKLCA',AL2(1230),AL2(0975),XL3'00'                           
          DC   CL5'CKWSA',AL2(1230),AL2(0975),XL3'00'                           
          DC   CL5'CJKLA',AL2(1240),AL2(0985),XL3'00'                           
          DC   CL5'CFCAF',AL2(1250),AL2(0990),XL3'00'                           
          DC   CL5'CHYMA',AL2(1250),AL2(0990),XL3'00'                           
          DC   CL5'CIAMA',AL2(1250),AL2(0990),XL3'00'                           
          DC   CL5'CKGLF',AL2(1250),AL2(0990),XL3'00'                           
          DC   CL5'CKKWA',AL2(1250),AL2(0990),XL3'00'                           
          DC   CL5'CKWRF',AL2(1250),AL2(0990),XL3'00'                           
          DC   CL5'CKTKA',AL2(2290),AL2(1005),XL3'00'                           
          DC   CL5'CJLAA',AL2(0600),AL2(1015),XL3'00'                           
          DC   CL5'CKFLA',AL2(0590),AL2(1025),XL3'00'                           
          DC   CL5'CJJCA',AL2(2300),AL2(1035),XL3'00'                           
          DC   CL5'CHGBA',AL2(0560),AL2(1045),XL3'00'                           
          DC   CL5'CHGBF',AL2(0560),AL2(1045),XL3'00'                           
          DC   CL5'CKLSA',AL2(0570),AL2(1055),XL3'00'                           
          DC   CL5'CFLMA',AL2(0580),AL2(1065),XL3'00'                           
          DC   CL5'CFGLF',AL2(0610),AL2(1075),XL3'00'                           
          DC   CL5'CKLMA',AL2(0610),AL2(1075),XL3'00'                           
          DC   CL5'CHYRA',AL2(1260),AL2(1085),XL3'00'                           
          DC   CL5'CHECA',AL2(1980),AL2(1095),XL3'00'                           
          DC   CL5'CILAF',AL2(1980),AL2(1095),XL3'00'                           
          DC   CL5'CJOCA',AL2(1980),AL2(1095),XL3'00'                           
          DC   CL5'CFLSA',AL2(0620),AL2(1105),XL3'00'                           
          DC   CL5'CKLYA',AL2(1270),AL2(1115),XL3'00'                           
          DC   CL5'CKSAA',AL2(1990),AL2(1125),XL3'00'                           
          DC   CL5'CBBLF',AL2(1280),AL2(1135),XL3'00'                           
          DC   CL5'CBCLF',AL2(1280),AL2(1135),XL3'00'                           
          DC   CL5'CFPLA',AL2(1280),AL2(1135),XL3'00'                           
          DC   CL5'CFPLF',AL2(1280),AL2(1135),XL3'00'                           
          DC   CL5'CIQMF',AL2(1280),AL2(1135),XL3'00'                           
          DC   CL5'CJBKA',AL2(1280),AL2(1135),XL3'00'                           
          DC   CL5'CJBXA',AL2(1280),AL2(1135),XL3'00'                           
          DC   CL5'CKOLF',AL2(1280),AL2(1135),XL3'00'                           
          DC   CL5'CKSLA',AL2(1280),AL2(1135),XL3'00'                           
          DC   CL5'LCKOF',AL2(1280),AL2(1135),XL3'00'                           
          DC   CL5'CHAPA',AL2(1290),AL2(1145),XL3'00'                           
          DC   CL5'CKMKA',AL2(2310),AL2(1175),XL3'00'                           
          DC   CL5'CHCMA',AL2(0080),AL2(1185),XL3'00'                           
          DC   CL5'CBGAA',AL2(0640),AL2(1195),XL3'00'                           
          DC   CL5'CHRMA',AL2(0640),AL2(1195),XL3'00'                           
          DC   CL5'CJNSA',AL2(1770),AL2(1205),XL3'00'                           
          DC   CL5'CHATA',AL2(2000),AL2(1215),XL3'00'                           
          DC   CL5'CJCYA',AL2(2000),AL2(1215),XL3'00'                           
          DC   CL5'CHCLA',AL2(2010),AL2(1225),XL3'00'                           
          DC   CL5'CJVRA',AL2(1780),AL2(1235),XL3'00'                           
          DC   CL5'CJNLA',AL2(2320),AL2(1245),XL3'00'                           
          DC   CL5'CKADA',AL2(0240),AL2(1255),XL3'00'                           
          DC   CL5'CKMPA',AL2(1300),AL2(1265),XL3'00'                           
          DC   CL5'CJMRA',AL2(1310),AL2(1275),XL3'00'                           
          DC   CL5'CFRMA',AL2(0650),AL2(1285),XL3'00'                           
          DC   CL5'CBAFA',AL2(0360),AL2(1295),XL3'00'                           
          DC   CL5'CBA A',AL2(0360),AL2(1295),XL3'00'                           
          DC   CL5'CFQMF',AL2(0360),AL2(1295),XL3'00'                           
          DC   CL5'CJMOF',AL2(0360),AL2(1295),XL3'00'                           
          DC   CL5'CKCWA',AL2(0360),AL2(1295),XL3'00'                           
          DC   CL5'CKMAA',AL2(0660),AL2(1305),XL3'00'                           
          DC   CL5'CKMLA',AL2(0670),AL2(1315),XL3'00'                           
          DC   CL5'CKBMA',AL2(0680),AL2(1325),XL3'00'                           
          DC   CL5'CBF A',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CBF F',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CBM A',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CBM F',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CFCFA',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CFMBA',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CFQRF',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CHOMF',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CHTXA',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CIELF',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CINQF',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CITEF',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CJADA',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CJFMF',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CJMSA',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CKACA',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CKGMA',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'CKO A',AL2(0690),AL2(1335),XL3'00'                           
          DC   CL5'NSSWA',AL2(0370),AL2(1345),XL3'00'                           
          DC   CL5'CKMFF',AL2(0700),AL2(1355),XL3'00'                           
          DC   CL5'CKOIA',AL2(0700),AL2(1355),XL3'00'                           
          DC   CL5'CKVLA',AL2(0700),AL2(1355),XL3'00'                           
          DC   CL5'CHABA',AL2(1790),AL2(1365),XL3'00'                           
          DC   CL5'CHMOA',AL2(0690),AL2(1375),XL3'00'                           
          DC   CL5'CHYQA',AL2(0090),AL2(1385),XL3'00'                           
          DC   CL5'CHUBA',AL2(2330),AL2(1395),XL3'00'                           
          DC   CL5'CKEGA',AL2(2330),AL2(1395),XL3'00'                           
          DC   CL5'CKKCA',AL2(2340),AL2(1405),XL3'00'                           
          DC   CL5'CHNCA',AL2(0705),AL2(1415),XL3'00'                           
          DC   CL5'CFANA',AL2(0380),AL2(1425),XL3'00'                           
          DC   CL5'CKECA',AL2(0250),AL2(1435),XL3'00'                           
          DC   CL5'CJTTA',AL2(1330),AL2(1445),XL3'00'                           
          DC   CL5'CJRNA',AL2(1340),AL2(1465),XL3'00'                           
          DC   CL5'CJNBA',AL2(1800),AL2(1475),XL3'00'                           
          DC   CL5'CFCHA',AL2(1350),AL2(1485),XL3'00'                           
          DC   CL5'CHURA',AL2(1350),AL2(1485),XL3'00'                           
          DC   CL5'CKATF',AL2(1350),AL2(1485),XL3'00'                           
          DC   CL5'CJNCA',AL2(1690),AL2(1495),XL3'00'                           
          DC   CL5'CHWOA',AL2(1360),AL2(1505),XL3'00'                           
          DC   CL5'CFORA',AL2(1370),AL2(1515),XL3'00'                           
          DC   CL5'CKARA',AL2(1380),AL2(1525),XL3'00'                           
          DC   CL5'CKQTF',AL2(1380),AL2(1525),XL3'00'                           
          DC   CL5'CKOOA',AL2(2360),AL2(1535),XL3'00'                           
          DC   CL5'CBOFA',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CBOFF',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CBO A',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CBO F',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CFGOA',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CFMOF',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CFRAA',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CHEZF',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CNWWA',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CJRCA',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CJJBA',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CKBYF',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CKCUF',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CKOYA',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'Q101A',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'Q101F',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CIWWA',AL2(1390),AL2(1545),XL3'00'                           
          DC   CL5'CFOSA',AL2(1400),AL2(1555),XL3'00'                           
          DC   CL5'CHPQA',AL2(2370),AL2(1565),XL3'00'                           
          DC   CL5'CFBQA',AL2(1410),AL2(1575),XL3'00'                           
          DC   CL5'CKYLA',AL2(2020),AL2(1585),XL3'00'                           
          DC   CL5'CHOVA',AL2(1420),AL2(1595),XL3'00'                           
          DC   CL5'CHROA',AL2(1420),AL2(1595),XL3'00'                           
          DC   CL5'CHROF',AL2(1420),AL2(1595),XL3'00'                           
          DC   CL5'CJMGF',AL2(2380),AL2(1605),XL3'00'                           
          DC   CL5'CKOKA',AL2(2380),AL2(1605),XL3'00'                           
          DC   CL5'CKOKF',AL2(2380),AL2(1605),XL3'00'                           
          DC   CL5'CFMPF',AL2(1430),AL2(1615),XL3'00'                           
          DC   CL5'CHEXA',AL2(1430),AL2(1615),XL3'00'                           
          DC   CL5'CKPTA',AL2(1430),AL2(1615),XL3'00'                           
          DC   CL5'CKQMF',AL2(1430),AL2(1615),XL3'00'                           
          DC   CL5'CKQWF',AL2(1430),AL2(1615),XL3'00'                           
          DC   CL5'CKTLA',AL2(0710),AL2(1625),XL3'00'                           
          DC   CL5'MCKOF',AL2(0720),AL2(1635),XL3'00'                           
          DC   CL5'CFGNA',AL2(0100),AL2(1645),XL3'00'                           
          DC   CL5'CFNWA',AL2(0110),AL2(1655),XL3'00'                           
          DC   CL5'CFRYA',AL2(1700),AL2(1665),XL3'00'                           
          DC   CL5'CJAVA',AL2(2390),AL2(1675),XL3'00'                           
          DC   CL5'CIPCA',AL2(0730),AL2(1685),XL3'00'                           
          DC   CL5'CIGOF',AL2(0260),AL2(1695),XL3'00'                           
          DC   CL5'CKPVF',AL2(0740),AL2(1705),XL3'00'                           
          DC   CL5'CHQBA',AL2(2400),AL2(1715),XL3'00'                           
          DC   CL5'CFMMF',AL2(1810),AL2(1725),XL3'00'                           
          DC   CL5'CKBIA',AL2(1810),AL2(1725),XL3'00'                           
          DC   CL5'CJCIA',AL2(2410),AL2(1735),XL3'00'                           
          DC   CL5'CKPGA',AL2(2410),AL2(1735),XL3'00'                           
          DC   CL5'CFPRA',AL2(2420),AL2(1745),XL3'00'                           
          DC   CL5'CHTKA',AL2(2420),AL2(1745),XL3'00'                           
          DC   CL5'CINLA',AL2(2425),AL2(1755),XL3'00'                           
          DC   CL5'CBV A',AL2(0750),AL2(1765),XL3'00'                           
          DC   CL5'CBV F',AL2(0750),AL2(1765),XL3'00'                           
          DC   CL5'CHOIF',AL2(0750),AL2(1765),XL3'00'                           
          DC   CL5'CHRVA',AL2(0750),AL2(1765),XL3'00'                           
          DC   CL5'CITFF',AL2(0750),AL2(1765),XL3'00'                           
          DC   CL5'CJMFF',AL2(0750),AL2(1765),XL3'00'                           
          DC   CL5'CJRPA',AL2(0750),AL2(1765),XL3'00'                           
          DC   CL5'CKCVA',AL2(0750),AL2(1765),XL3'00'                           
          DC   CL5'CKRLF',AL2(0750),AL2(1765),XL3'00'                           
          DC   CL5'CKCQA',AL2(2430),AL2(1775),XL3'00'                           
          DC   CL5'CFCRF',AL2(2030),AL2(1785),XL3'00'                           
          DC   CL5'CIZZF',AL2(2030),AL2(1785),XL3'00'                           
          DC   CL5'CKGYA',AL2(2030),AL2(1785),XL3'00'                           
          DC   CL5'CKRDA',AL2(2030),AL2(1785),XL3'00'                           
          DC   CL5'CKRDF',AL2(2030),AL2(1785),XL3'00'                           
          DC   CL5'CKOBA',AL2(1440),AL2(1805),XL3'00'                           
          DC   CL5'CKCRA',AL2(2440),AL2(1815),XL3'00'                           
          DC   CL5'CKNXF',AL2(2440),AL2(1815),XL3'00'                           
          DC   CL5'CBAFF',AL2(0390),AL2(1825),XL3'00'                           
          DC   CL5'CFGMA',AL2(1450),AL2(1835),XL3'00'                           
          DC   CL5'CFLPA',AL2(0760),AL2(1845),XL3'00'                           
          DC   CL5'CJBRA',AL2(0760),AL2(1845),XL3'00'                           
          DC   CL5'CJBRF',AL2(0760),AL2(1845),XL3'00'                           
          DC   CL5'CJFPA',AL2(0770),AL2(1855),XL3'00'                           
          DC   CL5'CHRLA',AL2(0780),AL2(1865),XL3'00'                           
          DC   CL5'CKKRA',AL2(1830),AL2(1875),XL3'00'                           
          DC   CL5'CHLMF',AL2(0790),AL2(1885),XL3'00'                           
          DC   CL5'CIRCF',AL2(0790),AL2(1885),XL3'00'                           
          DC   CL5'CKRNA',AL2(0790),AL2(1885),XL3'00'                           
          DC   CL5'CBD A',AL2(0400),AL2(1895),XL3'00'                           
          DC   CL5'CFBCA',AL2(0400),AL2(1895),XL3'00'                           
          DC   CL5'CFBCF',AL2(0400),AL2(1895),XL3'00'                           
          DC   CL5'CHSJA',AL2(0400),AL2(1895),XL3'00'                           
          DC   CL5'CIDKF',AL2(0400),AL2(1895),XL3'00'                           
          DC   CL5'CJYCF',AL2(0400),AL2(1895),XL3'00'                           
          DC   CL5'SCKOA',AL2(0400),AL2(1895),XL3'00'                           
          DC   CL5'CKXRA',AL2(2450),AL2(1905),XL3'00'                           
          DC   CL5'CFGXF',AL2(1460),AL2(1915),XL3'00'                           
          DC   CL5'CHOKA',AL2(1460),AL2(1915),XL3'00'                           
          DC   CL5'CKJDA',AL2(1460),AL2(1915),XL3'00'                           
          DC   CL5'CKTYA',AL2(1460),AL2(1915),XL3'00'                           
          DC   CL5'CBKSF',AL2(1840),AL2(1925),XL3'00'                           
          DC   CL5'CFMCF',AL2(1840),AL2(1925),XL3'00'                           
          DC   CL5'CFQCA',AL2(1840),AL2(1925),XL3'00'                           
          DC   CL5'CJUSA',AL2(1840),AL2(1925),XL3'00'                           
          DC   CL5'CJWWA',AL2(1840),AL2(1925),XL3'00'                           
          DC   CL5'CKOMA',AL2(1840),AL2(1925),XL3'00'                           
          DC   CL5'CFYNA',AL2(1470),AL2(1935),XL3'00'                           
          DC   CL5'CHASF',AL2(1470),AL2(1935),XL3'00'                           
          DC   CL5'CJRMF',AL2(1470),AL2(1935),XL3'00'                           
          DC   CL5'CKCYA',AL2(1470),AL2(1935),XL3'00'                           
          DC   CL5'WYSSF',AL2(1470),AL2(1935),XL3'00'                           
          DC   CL5'CKCNA',AL2(0800),AL2(1945),XL3'00'                           
          DC   CL5'CJSMA',AL2(1850),AL2(1955),XL3'00'                           
          DC   CL5'CKSMA',AL2(0810),AL2(1965),XL3'00'                           
          DC   CL5'CHLTA',AL2(0820),AL2(1975),XL3'00'                           
          DC   CL5'CIIMF',AL2(0820),AL2(1975),XL3'00'                           
          DC   CL5'CIMOF',AL2(0820),AL2(1975),XL3'00'                           
          DC   CL5'CJRSA',AL2(0820),AL2(1975),XL3'00'                           
          DC   CL5'CKTSA',AL2(0820),AL2(1975),XL3'00'                           
          DC   CL5'CHRNA',AL2(1480),AL2(1985),XL3'00'                           
          DC   CL5'CFBVA',AL2(2460),AL2(1995),XL3'00'                           
          DC   CL5'CHEQF',AL2(1490),AL2(2005),XL3'00'                           
          DC   CL5'CJETA',AL2(1490),AL2(2005),XL3'00'                           
          DC   CL5'CJETF',AL2(1490),AL2(2005),XL3'00'                           
          DC   CL5'CJJOA',AL2(0830),AL2(2015),XL3'00'                           
          DC   CL5'CHREF',AL2(1500),AL2(2025),XL3'00'                           
          DC   CL5'CHSCA',AL2(1500),AL2(2025),XL3'00'                           
          DC   CL5'CHSCF',AL2(1500),AL2(2025),XL3'00'                           
          DC   CL5'CHTZF',AL2(1500),AL2(2025),XL3'00'                           
          DC   CL5'CJQRF',AL2(1500),AL2(2025),XL3'00'                           
          DC   CL5'CKTBA',AL2(1500),AL2(2025),XL3'00'                           
          DC   CL5'CKTBF',AL2(1500),AL2(2025),XL3'00'                           
          DC   CL5'CHRSA',AL2(0850),AL2(2035),XL3'00'                           
          DC   CL5'CJENA',AL2(0860),AL2(2045),XL3'00'                           
          DC   CL5'CBN A',AL2(0120),AL2(2055),XL3'00'                           
          DC   CL5'CBN F',AL2(0120),AL2(2055),XL3'00'                           
          DC   CL5'CHOZF',AL2(0120),AL2(2055),XL3'00'                           
          DC   CL5'CHVOA',AL2(0120),AL2(2055),XL3'00'                           
          DC   CL5'CJYQA',AL2(0120),AL2(2055),XL3'00'                           
          DC   CL5'CKIXF',AL2(0120),AL2(2055),XL3'00'                           
          DC   CL5'VOARA',AL2(0120),AL2(2055),XL3'00'                           
          DC   CL5'VOCMA',AL2(0120),AL2(2055),XL3'00'                           
          DC   CL5'VOCMF',AL2(0120),AL2(2055),XL3'00'                           
          DC   CL5'VOWRA',AL2(0120),AL2(2055),XL3'00'                           
          DC   CL5'CHLOA',AL2(1510),AL2(2065),XL3'00'                           
          DC   CL5'CKBSA',AL2(0840),AL2(2075),XL3'00'                           
          DC   CL5'CKSBA',AL2(1710),AL2(2085),XL3'00'                           
          DC   CL5'CIMEF',AL2(0870),AL2(2095),XL3'00'                           
          DC   CL5'CJSAA',AL2(0880),AL2(2105),XL3'00'                           
          DC   CL5'CJMCA',AL2(0890),AL2(2115),XL3'00'                           
          DC   CL5'CJVLA',AL2(0900),AL2(2125),XL3'00'                           
          DC   CL5'CHSMA',AL2(1720),AL2(2135),XL3'00'                           
          DC   CL5'CFSXA',AL2(0130),AL2(2145),XL3'00'                           
          DC   CL5'CHOAA',AL2(2050),AL2(2155),XL3'00'                           
          DC   CL5'CILWA',AL2(2040),AL2(2165),XL3'00'                           
          DC   CL5'CIOKA',AL2(2040),AL2(2165),XL3'00'                           
          DC   CL5'CJCSA',AL2(1520),AL2(2175),XL3'00'                           
          DC   CL5'CBCSF',AL2(1530),AL2(2185),XL3'00'                           
          DC   CL5'CBONF',AL2(1530),AL2(2185),XL3'00'                           
          DC   CL5'CFBRA',AL2(1530),AL2(2185),XL3'00'                           
          DC   CL5'CHNDA',AL2(1530),AL2(2185),XL3'00'                           
          DC   CL5'CHNOA',AL2(1530),AL2(2185),XL3'00'                           
          DC   CL5'CIGMF',AL2(1530),AL2(2185),XL3'00'                           
          DC   CL5'CJMXF',AL2(1530),AL2(2185),XL3'00'                           
          DC   CL5'CKSOA',AL2(1530),AL2(2185),XL3'00'                           
          DC   CL5'CKSPA',AL2(2430),AL2(2195),XL3'00'                           
          DC   CL5'CJRWA',AL2(0160),AL2(2205),XL3'00'                           
          DC   CL5'CJCWA',AL2(0410),AL2(2215),XL3'00'                           
          DC   CL5'CJGLF',AL2(1860),AL2(2226),XL3'00'                           
          DC   CL5'CKSWA',AL2(1860),AL2(2226),XL3'00'                           
          DC   CL5'CBI A',AL2(0270),AL2(2235),XL3'00'                           
          DC   CL5'CBI F',AL2(0270),AL2(2235),XL3'00'                           
          DC   CL5'CHERA',AL2(0270),AL2(2235),XL3'00'                           
          DC   CL5'CJCBA',AL2(0270),AL2(2235),XL3'00'                           
          DC   CL5'CJCBF',AL2(0270),AL2(2235),XL3'00'                           
          DC   CL5'CKPEA',AL2(0270),AL2(2235),XL3'00'                           
          DC   CL5'CKTAA',AL2(2060),AL2(2245),XL3'00'                           
          DC   CL5'CFTKA',AL2(2480),AL2(2255),XL3'00'                           
          DC   CL5'CJARA',AL2(1730),AL2(2265),XL3'00'                           
          DC   CL5'CKLDA',AL2(0910),AL2(2275),XL3'00'                           
          DC   CL5'CHTMA',AL2(1740),AL2(2285),XL3'00'                           
          DC   CL5'CBQ A',AL2(1540),AL2(2295),XL3'00'                           
          DC   CL5'CFPAF',AL2(1540),AL2(2295),XL3'00'                           
          DC   CL5'CHSDF',AL2(1540),AL2(2295),XL3'00'                           
          DC   CL5'CJLBA',AL2(1540),AL2(2295),XL3'00'                           
          DC   CL5'SJSDF',AL2(1540),AL2(2295),XL3'00'                           
          DC   CL5'CKPRA',AL2(1540),AL2(2295),XL3'00'                           
          DC   CL5'CKOTA',AL2(1550),AL2(2305),XL3'00'                           
          DC   CL5'CKOTF',AL2(1550),AL2(2305),XL3'00'                           
          DC   CL5'CFCLA',AL2(1560),AL2(2315),XL3'00'                           
          DC   CL5'CFTIF',AL2(1560),AL2(2315),XL3'00'                           
          DC   CL5'CKGBA',AL2(1560),AL2(2315),XL3'00'                           
          DC   CL5'CBL A',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CBL F',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CFNYF',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CFRBA',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CFTRA',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CHFIF',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CHINA',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CHINF',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CHUMA',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CHUMF',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CILQF',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CJBCA',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CJCLA',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CJEZF',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CJRTF',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CKEYA',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CKFHA',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CKFMF',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CKO F',AL2(1570),AL2(2325),XL3'00'                           
          DC   CL5'CJATA',AL2(2490),AL2(2335),XL3'00'                           
          DC   CL5'CFCQF',AL2(0920),AL2(2345),XL3'00'                           
          DC   CL5'CHLNA',AL2(0920),AL2(2345),XL3'00'                           
          DC   CL5'CIBGF',AL2(0920),AL2(2345),XL3'00'                           
          DC   CL5'CJTRA',AL2(0920),AL2(2345),XL3'00'                           
          DC   CL5'CKCLA',AL2(0280),AL2(2355),XL3'00'                           
          DC   CL5'CKTOF',AL2(0280),AL2(2355),XL3'00'                           
          DC   CL5'CKVDA',AL2(0930),AL2(2365),XL3'00'                           
          DC   CL5'CFLVA',AL2(0940),AL2(2375),XL3'00'                           
          DC   CL5'CBUFF',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CFMIF',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CFOXF',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CFROF',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CFUNA',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CHQMA',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CHQMF',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CISLA',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CJJRF',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CJORA',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CJVBA',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CKKSF',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CKLGA',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CKLGF',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CKNWA',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CKWXA',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CKXYA',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'VCKOF',AL2(2500),AL2(2385),XL3'00'                           
          DC   CL5'CIVHA',AL2(2510),AL2(2395),XL3'00'                           
          DC   CL5'CKRWA',AL2(2510),AL2(2395),XL3'00'                           
          DC   CL5'CJIBA',AL2(2520),AL2(2405),XL3'00'                           
          DC   CL5'CKALA',AL2(2520),AL2(2405),XL3'00'                           
          DC   CL5'CFAXA',AL2(2530),AL2(2415),XL3'00'                           
          DC   CL5'CFMSF',AL2(2530),AL2(2415),XL3'00'                           
          DC   CL5'CJVIA',AL2(2530),AL2(2415),XL3'00'                           
          DC   CL5'CKDAA',AL2(2530),AL2(2415),XL3'00'                           
          DC   CL5'CKKQF',AL2(2530),AL2(2415),XL3'00'                           
          DC   CL5'CFDAA',AL2(0950),AL2(2426),XL3'00'                           
          DC   CL5'CFVDA',AL2(0960),AL2(2435),XL3'00'                           
          DC   CL5'CKVMA',AL2(0970),AL2(2445),XL3'00'                           
          DC   CL5'CKRBA',AL2(0980),AL2(2455),XL3'00'                           
          DC   CL5'CFLWA',AL2(0140),AL2(2465),XL3'00'                           
          DC   CL5'CJWAA',AL2(1580),AL2(2475),XL3'00'                           
          DC   CL5'CHOWA',AL2(1590),AL2(2485),XL3'00'                           
          DC   CL5'CJOIA',AL2(2080),AL2(2495),XL3'00'                           
          DC   CL5'CFOKA',AL2(2070),AL2(2505),XL3'00'                           
          DC   CL5'CFSLA',AL2(1870),AL2(2515),XL3'00'                           
          DC   CL5'CKWLA',AL2(2540),AL2(2525),XL3'00'                           
          DC   CL5'CFABA',AL2(0290),AL2(2535),XL3'00'                           
          DC   CL5'CBEFA',AL2(1600),AL2(2545),XL3'00'                           
          DC   CL5'CBE A',AL2(1600),AL2(2545),XL3'00'                           
          DC   CL5'CBE F',AL2(1600),AL2(2545),XL3'00'                           
          DC   CL5'CJOMF',AL2(1600),AL2(2545),XL3'00'                           
          DC   CL5'CKEZA',AL2(1600),AL2(2545),XL3'00'                           
          DC   CL5'CKEZF',AL2(1600),AL2(2545),XL3'00'                           
          DC   CL5'CKLWA',AL2(1600),AL2(2545),XL3'00'                           
          DC   CL5'CKLWF',AL2(1600),AL2(2545),XL3'00'                           
          DC   CL5'CKMRF',AL2(1600),AL2(2545),XL3'00'                           
          DC   CL5'CKWWA',AL2(1600),AL2(2545),XL3'00'                           
          DC   CL5'CKNXA',AL2(1610),AL2(2555),XL3'00'                           
          DC   CL5'CBW A',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CBW F',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CFRWA',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CHIQF',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CHMMF',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CIFXA',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CITIF',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CJOBA',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CJUMF',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CKISF',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CKJSA',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CKRCA',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CKWGF',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CKY A',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CKY F',AL2(1750),AL2(2565),XL3'00'                           
          DC   CL5'CJCJA',AL2(0420),AL2(2575),XL3'00'                           
          DC   CL5'CKDKA',AL2(0420),AL2(2575),XL3'00'                           
          DC   CL5'CJLSA',AL2(0300),AL2(2585),XL3'00'                           
          DC   CL5'CJCDA',AL2(2550),AL2(2595),XL3'00'                           
          DC   CL5'CJGXA',AL2(1880),AL2(2605),XL3'00'                           
          DC   CL5'CKBXA',AL2(2090),AL2(2615),XL3'00'                           
MSTABLEN  EQU (*-MSTABLE)/L'MSTABENT                                            
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
MSTABLED DSECT                                                                  
MSTABENT  DS   0XL12                                                            
MSTA     DS    CL5                                                              
MOLDMKT  DS    XL2                                                              
MNEWMKT  DS    XL2                                                              
MSTAP    DS    XL3                                                              
MSTABNXT EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
SPGENSTAD DSECT                                                                 
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002STEXTWT   04/05/90'                                      
         END                                                                    
