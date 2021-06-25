*          DATA SET STEXTBS    AT LEVEL 009 AS OF 12/05/92                      
*          DATA SET STEXTCC    AT LEVEL 007 AS OF 04/23/92                      
*          DATA SET SPEXTWT    AT LEVEL 024 AS OF 04/02/90                      
*PHASE STEXTBS,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* SPOT7 MARKET FIX FOR BSNY RADIO STATIONS                                      
* DEC03/92                                                                      
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
         NMOD1 20,DMLDEXT,R6,R7                                                 
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
         LA    R2,MTABLE1N                                                      
         LA    R3,MTABLE1                                                       
         SPACE                                                                  
INIT10   LA    R4,MTABLE1N                                                      
         LA    R5,MTABLE1                                                       
         SPACE                                                                  
INIT20   CR    R3,R5               SAME ENTRY                                   
         BE    INIT24                                                           
         CLC   0(5,R3),0(R5)       DUPE STATION                                 
         BNE   INIT24                                                           
         MVC   P(21),=C'STATION IN LIST TWICE'                                  
         MVC   P+24(5),0(R3)                                                    
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
INIT24   LA    R5,L'MTABENT(,R5)                                                
         BCT   R4,INIT20                                                        
         LA    R3,L'MTABENT(,R3)                                                
         BCT   R2,INIT10                                                        
         SPACE                                                                  
         LA    R2,MTABLE1N                                                      
         LA    R3,MTABLE1                                                       
INIT30   CLC   5(4,R3),9(R3)       DUPE MKT                                     
         BNE   INIT34                                                           
         MVC   P(7),=C'DUP MKT'                                                 
         MVC   P+8(4),5(R3)                                                     
         MVC   P+20(11),=C'FOR STATION'                                         
         MVC   P+33(5),0(R3)                                                    
         GOTO1 VPRINTER                                                         
INIT34   LA    R3,L'MTABENT(,R3)                                                
         BCT   R2,INIT30                                                        
         SPACE                                                                  
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
         USING STARECD,R3                                                       
         CLI   STAKTYPE,C'S'       STATION REC?                                 
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         CLI   STAKMED,C'R'        MEDIA RADIO                                  
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         CLC   STAKAGY,=C'BS'      BACKER                                       
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         AP    TSTACT,=P'1'                                                     
         SPACE                                                                  
         CLC   STAKCLT,=C'000'     NON CLIENT SPECIFIC STATION                  
         BE    STSTA10                                                          
         SPACE                                                                  
         LA    R4,CTABLEN                                                       
         LA    R5,CTABLE                                                        
STSTA00  CLC   STAKCLT,0(R5)       NON CLIENT SPECIFIC STATION                  
         BE    STSTA04                                                          
         LA    R5,3(,R5)                                                        
         BCT   R4,STSTA00                                                       
         B     DMXKEEP                                                          
         SPACE                                                                  
STSTA04  LA    R4,MTABLE1N                                                      
         LA    R5,MTABLE1                                                       
         USING MTABLED,R5                                                       
STSTA06  CLC   STAKCALL,MSTA                                                    
         BE    STSTA08                                                          
         LA    R5,MTABNXT                                                       
         BCT   R4,STSTA06                                                       
         B     DMXKEEP                                                          
         SPACE                                                                  
STSTA08  MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'STATION'                                              
         MVC   P+18(5),STAKCALL                                                 
         SPACE                                                                  
         MVC   P+30(7),=C'OLD MKT'                                              
         MVC   P+40(4),SMKT                                                     
         SPACE                                                                  
         MVC   P+50(7),=C'NEW MKT'                                              
         MVC   P+60(4),MNEWMKT                                                  
         MVC   P+80(3),=C'CLT'                                                  
         MVC   P+84(3),STAKCLT     NON CLIENT SPECIFIC STATION                  
         MVC   SMKT,MNEWMKT                                                     
         AP    TCSTANEW,=P'1'                                                   
         GOTO1 VPRINTER                                                         
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA NEW MKT'                                            
         B     PRT                                                              
         SPACE                                                                  
STSTA10  LA    R4,MTABLEN                                                       
         LA    R5,MTABLE                                                        
         USING MTABLED,R5                                                       
STSTA12  CLC   STAKCALL,MSTA                                                    
         BNE   STSTA14                                                          
         SPACE                                                                  
         CLC   SMKT,MOLDMKT                                                     
         BE    STSTA16                                                          
         B     DMXKEEP                                                          
         SPACE                                                                  
STSTA14  LA    R5,MTABNXT                                                       
         BCT   R4,STSTA12                                                       
         B     DMXKEEP                                                          
         SPACE                                                                  
STSTA16  MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'STATION'                                              
         MVC   P+18(5),STAKCALL                                                 
         SPACE                                                                  
         MVC   P+30(7),=C'OLD MKT'                                              
         MVC   P+40(4),MOLDMKT                                                  
         SPACE                                                                  
         MVC   P+50(7),=C'NEW MKT'                                              
         MVC   P+60(4),MNEWMKT                                                  
         AP    TSTANEW,=P'1'                                                    
         MVC   SMKT,MNEWMKT                                                     
         GOTO1 VPRINTER                                                         
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA NEW MKT'                                            
         B     PRT                                                              
         SPACE                                                                  
         MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'MISSING'                                              
         MVC   P+20(5),STAKCALL                                                 
         MVC   P+30(4),SMKT                                                     
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'STA RECCORD'                                            
         B     DMXKEEP                                                          
         DROP  R3                                                               
         SPACE                                                                  
PRT      DS    0H                                                               
         LA    R5,116                                                           
         SPACE                                                                  
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
TCSTANEW DC    PL5'0',CL28'TOT STAS NEW MKT-CLT SPEC'                           
TSTANEW  DC    PL5'0',CL28'TOT STAS NEW MKT'                                    
TOTCTRS  EQU   (*-TOTRD)/33                                                     
          SPACE                                                                 
WORK     DS    CL64                                                             
         SPACE                                                                  
RTITLE   DC    CL100'TV STATION REC MARKET FIX FOR CCUSA'                       
         LTORG                                                                  
          SPACE                                                                 
         LTORG                                                                  
         SPACE                                                                  
         DS    0D                                                               
CTABLE   DC    CL3'BAW'                                                         
         DC    CL3'BW '                                                         
         DC    CL3'CBS'                                                         
         DC    CL3'CS '                                                         
         DC    CL3'DLN'                                                         
         DC    CL3'HDC'                                                         
         DC    CL3'HDE'                                                         
         DC    CL3'HDS'                                                         
         DC    CL3'HDW'                                                         
         DC    CL3'HMA'                                                         
         DC    CL3'IGL'                                                         
         DC    CL3'ILI'                                                         
         DC    CL3'KFA'                                                         
         DC    CL3'MMA'                                                         
         DC    CL3'MRI'                                                         
         DC    CL3'MZ '                                                         
         DC    CL3'NOT'                                                         
         DC    CL3'NRA'                                                         
         DC    CL3'OSF'                                                         
         DC    CL3'PI '                                                         
         DC    CL3'UB '                                                         
         DC    CL3'XE '                                                         
CTABLEN  EQU   (*-CTABLE)/3                                                     
         SPACE                                                                  
MTABLE1  DS    0C                                                               
*TABLE   DC    C'KOGOA',C'3218',C'3727',XL7'00'                                 
         DC    C'KOKZF',C'3832',C'0628',XL7'00'                                 
         DC    C'KOLAF',C'3104',C'3218',XL7'00'                                 
         DC    C'KOOSF',C'2637',C'0837',XL7'00'                                 
         DC    C'KOOVF',C'3766',C'3587',XL7'00'                                 
         DC    C'KORAF',C'3767',C'0493',XL7'00'                                 
         DC    C'KORDF',C'4028',C'3652',XL7'00'                                 
         DC    C'KOSOF',C'2808',C'2390',XL7'00'                                 
         DC    C'KOVCF',C'3714',C'1761',XL7'00'                                 
         DC    C'KOWFF',C'3218',C'2641',XL7'00'                                 
         DC    C'KOWLA',C'3384',C'3389',XL7'00'                                 
         DC    C'KOXRA',C'2106',C'3727',XL7'00'                                 
         DC    C'KOYNF',C'0922',C'0986',XL7'00'                                 
         DC    C'KOZTF',C'1269',C'2306',XL7'00'                                 
         DC    C'KPBQF',C'2061',C'2861',XL7'00'                                 
         DC    C'KPKYF',C'1700',C'2907',XL7'00'                                 
         DC    C'KPLEF',C'3766',C'3587',XL7'00'                                 
         DC    C'KPNDF',C'3235',C'0780',XL7'00'                                 
         DC    C'KPRMF',C'2795',C'0309',XL7'00'                                 
*        DC    C'KQDVF',C'2374',C'0359',XL7'00'                                 
         DC    C'KQEGF',C'1883',C'1885',XL7'00'                                 
*        DC    C'KMXKF',C'2377',C'3449',XL7'00'                                 
         DC    C'KMYZF',C'2999',C'3670',XL7'00'                                 
         DC    C'KMZZA',C'1063',C'2377',XL7'00'                                 
*        DC    C'KNACF',C'2097',C'2106',XL7'00'                                 
         DC    C'KNEKF',C'1908',C'2710',XL7'00'                                 
         DC    C'KNFOF',C'3766',C'3767',XL7'00'                                 
         DC    C'KNIAA',C'1878',C'2746',XL7'00'                                 
         DC    C'KNLTF',C'2800',C'3652',XL7'00'                                 
         DC    C'KNNNF',C'2851',C'3060',XL7'00'                                 
*        DC    C'KNTIF',C'3221',C'2340',XL7'00'                                 
         DC    C'KNUIA',C'1810',C'2262',XL7'00'                                 
         DC    C'KNUIF',C'1653',C'2262',XL7'00'                                 
         DC    C'KNXRF',C'2253',C'3117',XL7'00'                                 
         DC    C'KOAIF',C'0924',C'0922',XL7'00'                                 
*        DC    C'KOAQF',C'0647',C'3279',XL7'00'                                 
         DC    C'KOASF',C'1823',C'1875',XL7'00'                                 
         DC    C'KOCDF',C'1800',C'1803',XL7'00'                                 
*        DC    C'KOCHF',C'2609',C'0093',XL7'00'                                 
*        DC    C'KODHF',C'2679',C'2353',XL7'00'                                 
         DC    C'KOELF',C'2680',C'0628',XL7'00'                                 
         DC    C'KOGAF',C'2682',C'2640',XL7'00'                                 
         DC    C'KOGAA',C'2682',C'2640',XL7'00'                                 
*        DC    C'KLVAF',C'1653',C'1875',XL7'00'                                 
         DC    C'KLWDF',C'3040',C'3344',XL7'00'                                 
         DC    C'KLWTF',C'1976',C'3146',XL7'00'                                 
         DC    C'KLZRF',C'1963',C'3636',XL7'00'                                 
         DC    C'KMAGF',C'3670',C'1284',XL7'00'                                 
*        DC    C'KMASA',C'3341',C'2697',XL7'00'                                 
         DC    C'KMBLA',C'3215',C'0969',XL7'00'                                 
         DC    C'KMBYF',C'3204',C'3206',XL7'00'                                 
         DC    C'KMCKF',C'1284',C'1235',XL7'00'                                 
         DC    C'KMDLF',C'1826',C'1903',XL7'00'                                 
         DC    C'KMEZF',C'0924',C'2573',XL7'00'                                 
         DC    C'KMGGF',C'2106',C'3251',XL7'00'                                 
         DC    C'KMGNF',C'0218',C'1248',XL7'00'                                 
         DC    C'KMGOF',C'0639',C'2746',XL7'00'                                 
         DC    C'KMGZF',C'3937',C'1970',XL7'00'                                 
         DC    C'KMIAF',C'1673',C'3087',XL7'00'                                 
         DC    C'KMIXF',C'3151',C'2390',XL7'00'                                 
         DC    C'KMMSF',C'2373',C'0422',XL7'00'                                 
         DC    C'KMNTF',C'3293',C'0645',XL7'00'                                 
         DC    C'KMSRF',C'3449',C'0050',XL7'00'                                 
         DC    C'KMTXF',C'2373',C'1584',XL7'00'                                 
         DC    C'KKMGF',C'3001',C'0793',XL7'00'                                 
*        DC    C'KKORF',C'0045',C'1364',XL7'00'                                 
         DC    C'KKOSF',C'3218',C'2641',XL7'00'                                 
         DC    C'KKOWF',C'2878',C'1803',XL7'00'                                 
         DC    C'KKRKF',C'3663',C'3361',XL7'00'                                 
*        DC    C'KKSIF',C'2734',C'2746',XL7'00'                                 
         DC    C'KKSNA',C'3707',C'2945',XL7'00'                                 
         DC    C'KKSSF',C'3249',C'0045',XL7'00'                                 
         DC    C'KKTXF',C'1851',C'3684',XL7'00'                                 
         DC    C'KKURF',C'2686',C'3727',XL7'00'                                 
*        DC    C'KKYRF',C'3354',C'3592',XL7'00'                                 
         DC    C'KKZXF',C'0938',C'3428',XL7'00'                                 
         DC    C'KLANF',C'3957',C'1400',XL7'00'                                 
         DC    C'KLAUA',C'3206',C'3206',XL7'00'                                 
         DC    C'KLAWF',C'3937',C'1970',XL7'00'                                 
         DC    C'KLAZF',C'2061',C'1664',XL7'00'                                 
         DC    C'KLCYF',C'3211',C'3729',XL7'00'                                 
         DC    C'KLFXF',C'3766',C'3587',XL7'00'                                 
         DC    C'KLGTF',C'0437',C'3344',XL7'00'                                 
         DC    C'KLKIA',C'3293',C'2497',XL7'00'                                 
         DC    C'KLQPF',C'2157',C'2229',XL7'00'                                 
         DC    C'KISMF',C'3293',C'2497',XL7'00'                                 
         DC    C'KISZF',C'0862',C'1224',XL7'00'                                 
         DC    C'KITEF',C'2947',C'0969',XL7'00'                                 
*        DC    C'KIZZF',C'2374',C'2378',XL7'00'                                 
         DC    C'KJCKF',C'1805',C'2186',XL7'00'                                 
         DC    C'KJJKF',C'1238',C'0050',XL7'00'                                 
         DC    C'KJJYF',C'0110',C'0990',XL7'00'                                 
         DC    C'KJJZF',C'0027',C'1880',XL7'00'                                 
         DC    C'KJMOF',C'0800',C'1774',XL7'00'                                 
         DC    C'KJNEF',C'1622',C'3767',XL7'00'                                 
         DC    C'KJOIF',C'2106',C'1330',XL7'00'                                 
         DC    C'KJOYF',C'3181',C'3509',XL7'00'                                 
         DC    C'KJPWF',C'3893',C'3146',XL7'00'                                 
         DC    C'KJUGF',C'1330',C'3755',XL7'00'                                 
*        DC    C'KKSTF',C'2683',C'3211',XL7'00'                                 
         DC    C'KKBLF',C'1803',C'1803',XL7'00'                                 
         DC    C'KKDLF',C'0995',C'1219',XL7'00'                                 
         DC    C'KKFRF',C'1405',C'2851',XL7'00'                                 
         DC    C'KKHRF',C'2106',C'0010',XL7'00'                                 
         DC    C'KKIXF',C'1284',C'1235',XL7'00'                                 
*        DC    C'KKJRF',C'1695',C'1463',XL7'00'                                 
*        DC    C'KHHTF',C'2374',C'2378',XL7'00'                                 
*        DC    C'KHBMF',C'2420',C'0984',XL7'00'                                 
*        DC    C'KHOKF',C'1640',C'1454',XL7'00'                                 
         DC    C'KHOPF',C'3181',C'2390',XL7'00'                                 
         DC    C'KHQTF',C'3221',C'3222',XL7'00'                                 
         DC    C'KHTNF',C'2110',C'2390',XL7'00'                                 
*        DC    C'KHTTF',C'3222',C'3251',XL7'00'                                 
         DC    C'KICXF',C'2278',C'2640',XL7'00'                                 
         DC    C'KICXA',C'2278',C'2640',XL7'00'                                 
         DC    C'KIIKF',C'0990',C'2746',XL7'00'                                 
         DC    C'KIIZA',C'1852',C'3587',XL7'00'                                 
         DC    C'KIKCF',C'1270',C'2355',XL7'00'                                 
         DC    C'KIKFF',C'2714',C'0093',XL7'00'                                 
         DC    C'KIKMF',C'3347',C'0986',XL7'00'                                 
         DC    C'KIKSF',C'0988',C'1822',XL7'00'                                 
*        DC    C'KIKTF',C'1484',C'0986',XL7'00'                                 
*        DC    C'KIMPA',C'3354',C'2487',XL7'00'                                 
         DC    C'KIOCF',C'2712',C'0273',XL7'00'                                 
         DC    C'KIOKF',C'3086',C'3652',XL7'00'                                 
         DC    C'KIOTF',C'0235',C'3249',XL7'00'                                 
         DC    C'KIOZF',C'1944',C'2641',XL7'00'                                 
         DC    C'KIQYF',C'1977',C'2945',XL7'00'                                 
*        DC    C'KESMF',C'1106',C'1939',XL7'00'                                 
         DC    C'KEWBF',C'0096',C'3060',XL7'00'                                 
         DC    C'KEYRF',C'3766',C'3767',XL7'00'                                 
         DC    C'KEZAF',C'1284',C'1235',XL7'00'                                 
*        DC    C'KFBDF',C'0800',C'3146',XL7'00'                                 
         DC    C'KFJZA',C'1343',C'0922',XL7'00'                                 
         DC    C'KFLSA',C'2293',C'1874',XL7'00'                                 
         DC    C'KFMGF',C'0045',C'0990',XL7'00'                                 
         DC    C'KFMHF',C'2514',C'3017',XL7'00'                                 
*        DC    C'KFMNF',C'1693',C'2041',XL7'00'                                 
*        DC    C'KFMWF',C'3832',C'0628',XL7'00'                                 
         DC    C'KFROA',C'2098',C'3684',XL7'00'                                 
         DC    C'KFSOF',C'3755',C'1330',XL7'00'                                 
         DC    C'KFTMF',C'0989',C'3497',XL7'00'                                 
         DC    C'KFYRF',C'2374',C'0359',XL7'00'                                 
*        DC    C'KGDDA',C'2790',C'0986',XL7'00'                                 
*        DC    C'KGEEF',C'2679',C'2353',XL7'00'                                 
*        DC    C'KGGIF',C'0990',C'3216',XL7'00'                                 
         DC    C'KGMGF',C'3218',C'2641',XL7'00'                                 
*        DC    C'KGVLA',C'1484',C'1152',XL7'00'                                 
         DC    C'KGWYF',C'1393',C'3344',XL7'00'                                 
         DC    C'KCREF',C'0886',C'1178',XL7'00'                                 
         DC    C'KCRKF',C'0814',C'2459',XL7'00'                                 
         DC    C'KCRSA',C'2349',C'2353',XL7'00'                                 
         DC    C'KCVLA',C'0814',C'2459',XL7'00'                                 
         DC    C'KCVRA',C'3181',C'3509',XL7'00'                                 
         DC    C'KCWWA',C'3590',C'2851',XL7'00'                                 
         DC    C'KCXYF',C'0555',C'1095',XL7'00'                                 
         DC    C'KDFCA',C'3221',C'3222',XL7'00'                                 
         DC    C'KDFCF',C'3221',C'3222',XL7'00'                                 
         DC    C'KDIFA',C'3104',C'3216',XL7'00'                                 
*        DC    C'KDJSA',C'2377',C'1463',XL7'00'                                 
         DC    C'KDKOA',C'2063',C'0989',XL7'00'                                 
         DC    C'KDONF',C'3204',C'3206',XL7'00'                                 
         DC    C'KDSIA',C'0053',C'1866',XL7'00'                                 
         DC    C'KDSNF',C'0987',C'0597',XL7'00'                                 
         DC    C'KDSQF',C'3347',C'0986',XL7'00'                                 
*        DC    C'KDUZA',C'1695',C'1463',XL7'00'                                 
         DC    C'KDZNF',C'0346',C'2355',XL7'00'                                 
         DC    C'KEDGF',C'2818',C'1951',XL7'00'                                 
         DC    C'KELFF',C'2106',C'3727',XL7'00'                                 
         DC    C'KERVA',C'1840',C'0969',XL7'00'                                 
*        DC    C'KBOPF',C'2895',C'2960',XL7'00'                                 
*        DC    C'KBQQF',C'2374',C'2378',XL7'00'                                 
         DC    C'KBRGF',C'3221',C'3222',XL7'00'                                 
         DC    C'KBRLA',C'2640',C'2640',XL7'00'                                 
*        DC    C'KBRZA',C'1323',C'0103',XL7'00'                                 
         DC    C'KBUSF',C'2790',C'0986',XL7'00'                                 
         DC    C'KBYZF',C'2184',C'0359',XL7'00'                                 
         DC    C'KCALA',C'2106',C'3216',XL7'00'                                 
         DC    C'KCAQF',C'2757',C'3727',XL7'00'                                 
         DC    C'KCBWF',C'0800',C'3295',XL7'00'                                 
         DC    C'KCDQF',C'3214',C'2353',XL7'00'                                 
         DC    C'KCFXF',C'1546',C'1822',XL7'00'                                 
         DC    C'KCHHF',C'0701',C'0702',XL7'00'                                 
         DC    C'KCILF',C'1670',C'2573',XL7'00'                                 
         DC    C'KCIXF',C'1362',C'0395',XL7'00'                                 
         DC    C'KCIZF',C'3432',C'1235',XL7'00'                                 
         DC    C'KCLBA',C'0775',C'2769',XL7'00'                                 
         DC    C'KCLBF',C'2106',C'2769',XL7'00'                                 
         DC    C'KCMTF',C'3075',C'3543',XL7'00'                                 
         DC    C'KCOWA',C'0060',C'0652',XL7'00'                                 
         DC    C'KCREA',C'0886',C'1178',XL7'00'                                 
         DC    C'KABXF',C'1330',C'2390',XL7'00'                                 
         DC    C'KAFXF',C'2631',C'2132',XL7'00'                                 
         DC    C'KALEA',C'3084',C'3652',XL7'00'                                 
         DC    C'KALFF',C'3050',C'0702',XL7'00'                                 
         DC    C'KALKF',C'0986',C'2487',XL7'00'                                 
         DC    C'KAMOF',C'1284',C'1235',XL7'00'                                 
         DC    C'KAMSF',C'2175',C'3902',XL7'00'                                 
         DC    C'KARAF',C'3221',C'3222',XL7'00'                                 
         DC    C'KARZF',C'0519',C'3060',XL7'00'                                 
         DC    C'KASTF',C'0161',C'3292',XL7'00'                                 
         DC    C'KATPF',C'0575',C'0077',XL7'00'                                 
         DC    C'KAYIF',C'2271',C'3670',XL7'00'                                 
         DC    C'KAZAA',C'3221',C'3222',XL7'00'                                 
         DC    C'KBAYF',C'3222',C'3221',XL7'00'                                 
*        DC    C'KBBZF',C'3926',C'1816',XL7'00'                                 
         DC    C'KBEEF',C'3181',C'2390',XL7'00'                                 
*        DC    C'KBFCF',C'1267',C'3900',XL7'00'                                 
         DC    C'KBICF',C'0053',C'1866',XL7'00'                                 
         DC    C'KBLQF',C'3211',C'2085',XL7'00'                                 
         DC    C'KBMAF',C'1219',C'0493',XL7'00'                                 
         DC    C'KBMWA',C'1219',C'2664',XL7'00'                                 
*        DC    C'KQENA',C'3153',C'1175',XL7'00'                                 
         DC    C'KQHTF',C'1219',C'1436',XL7'00'                                 
*        DC    C'KQICF',C'3958',C'1463',XL7'00'                                 
*        DC    C'KQIPF',C'2679',C'2353',XL7'00'                                 
         DC    C'KQLTF',C'0615',C'0619',XL7'00'                                 
         DC    C'KQLXF',C'1219',C'2664',XL7'00'                                 
         DC    C'KQLXA',C'1219',C'2664',XL7'00'                                 
         DC    C'KQMXF',C'0800',C'3146',XL7'00'                                 
         DC    C'KQQKF',C'1315',C'1673',XL7'00'                                 
         DC    C'KQNNF',C'0857',C'1866',XL7'00'                                 
         DC    C'KQXLF',C'2580',C'0253',XL7'00'                                 
         DC    C'KRCHF',C'2253',C'3117',XL7'00'                                 
         DC    C'KRFDF',C'2250',C'3181',XL7'00'                                 
*        DC    C'KRIMF',C'2851',C'1411',XL7'00'                                 
         DC    C'KRITF',C'0725',C'1275',XL7'00'                                 
         DC    C'KRLDF',C'0924',C'0922',XL7'00'                                 
         DC    C'KROCA',C'2253',C'3117',XL7'00'                                 
         DC    C'KROCF',C'2253',C'3117',XL7'00'                                 
*        DC    C'KRPQF',C'3143',C'3257',XL7'00'                                 
         DC    C'KRVLF',C'1840',C'0969',XL7'00'                                 
         DC    C'KRVRF',C'0938',C'3017',XL7'00'                                 
         DC    C'KRWQF',C'1450',C'2293',XL7'00'                                 
         DC    C'KRZZF',C'0985',C'3936',XL7'00'                                 
         DC    C'KSEIF',C'1700',C'2907',XL7'00'                                 
         DC    C'KSJQF',C'1822',C'3432',XL7'00'                                 
         DC    C'KSKEF',C'1434',C'1410',XL7'00'                                 
         DC    C'KSLQF',C'3466',C'3821',XL7'00'                                 
         DC    C'KSNRF',C'3603',C'1436',XL7'00'                                 
         DC    C'KSPAF',C'1165',C'2641',XL7'00'                                 
*        DC    C'KSPEA',C'3247',C'3244',XL7'00'                                 
         DC    C'KSPLA',C'3224',C'2559',XL7'00'                                 
         DC    C'KSRFF',C'3221',C'0093',XL7'00'                                 
         DC    C'KSWVA',C'0045',C'3249',XL7'00'                                 
         DC    C'KTBBA',C'3354',C'3684',XL7'00'                                 
*        DC    C'KTKVF',C'0027',C'1808',XL7'00'                                 
         DC    C'KTOMF',C'2746',C'3206',XL7'00'                                 
         DC    C'KTRSF',C'0045',C'0619',XL7'00'                                 
         DC    C'KTSRF',C'3766',C'0493',XL7'00'                                 
         DC    C'KTUXF',C'3354',C'3587',XL7'00'                                 
*        DC    C'KTWNF',C'3354',C'3592',XL7'00'                                 
         DC    C'KTXYF',C'1774',C'0800',XL7'00'                                 
         DC    C'KTYZF',C'4002',C'1400',XL7'00'                                 
*        DC    C'KUDBF',C'2219',C'2390',XL7'00'                                 
         DC    C'KUEZF',C'2132',C'2523',XL7'00'                                 
         DC    C'KUICF',C'3706',C'2526',XL7'00'                                 
         DC    C'KUKIA',C'3691',C'2306',XL7'00'                                 
         DC    C'KUNAA',C'1713',C'2769',XL7'00'                                 
         DC    C'KUOOF',C'3423',C'3426',XL7'00'                                 
         DC    C'KUULF',C'0938',C'3017',XL7'00'                                 
         DC    C'KVILF',C'0924',C'0922',XL7'00'                                 
         DC    C'KVLYF',C'1087',C'2272',XL7'00'                                 
         DC    C'KVOUA',C'3705',C'1657',XL7'00'                                 
*        DC    C'KVWGA',C'2812',C'2960',XL7'00'                                 
*        DC    C'KVWGF',C'2812',C'2960',XL7'00'                                 
         DC    C'KWADA',C'3768',C'0050',XL7'00'                                 
         DC    C'KWALA',C'3778',C'0780',XL7'00'                                 
         DC    C'KWAVF',C'3204',C'3206',XL7'00'                                 
         DC    C'KWBGF',C'0990',C'0080',XL7'00'                                 
         DC    C'KWEDA',C'3299',C'3215',XL7'00'                                 
         DC    C'KWFSF',C'3936',C'3937',XL7'00'                                 
         DC    C'KWINF',C'2082',C'3519',XL7'00'                                 
         DC    C'KWLFF',C'0027',C'1197',XL7'00'                                 
         DC    C'KWLOA',C'3832',C'0628',XL7'00'                                 
         DC    C'KWMGF',C'0808',C'1329',XL7'00'                                 
         DC    C'KWOZF',C'2061',C'1329',XL7'00'                                 
         DC    C'KWPNA',C'3906',C'1329',XL7'00'                                 
         DC    C'KWPNF',C'2698',C'1329',XL7'00'                                 
         DC    C'KWREA',C'3799',C'3466',XL7'00'                                 
*        DC    C'KWSFF',C'3206',C'1094',XL7'00'                                 
         DC    C'KWWRF',C'2333',C'0800',XL7'00'                                 
         DC    C'KWXEF',C'2061',C'1664',XL7'00'                                 
         DC    C'KWWWF',C'3015',C'3810',XL7'00'                                 
*        DC    C'KWYNF',C'3863',C'3900',XL7'00'                                 
         DC    C'KXAAF',C'3428',C'3890',XL7'00'                                 
         DC    C'KXALF',C'2877',C'2487',XL7'00'                                 
*        DC    C'KXARF',C'1657',C'3592',XL7'00'                                 
         DC    C'KXLMF',C'2106',C'3727',XL7'00'                                 
*        DC    C'KXMGF',C'2201',C'3663',XL7'00'                                 
         DC    C'KXTZF',C'1592',C'1951',XL7'00'                                 
         DC    C'KXXOF',C'3293',C'2697',XL7'00'                                 
         DC    C'KYCKF',C'0893',C'1436',XL7'00'                                 
         DC    C'KYKRF',C'2927',C'0273',XL7'00'                                 
         DC    C'KYKXF',C'2098',C'3684',XL7'00'                                 
         DC    C'KYMGF',C'0027',C'0094',XL7'00'                                 
*        DC    C'KYOSA',C'2314',C'2390',XL7'00'                                 
         DC    C'KYREF',C'4042',C'2490',XL7'00'                                 
         DC    C'KYSLF',C'0989',C'1410',XL7'00'                                 
         DC    C'KYTXF',C'0077',C'0282',XL7'00'                                 
         DC    C'KYYYF',C'2374',C'0359',XL7'00'                                 
         DC    C'KZIOF',C'3541',C'1040',XL7'00'                                 
         DC    C'KZMKF',C'0353',C'3361',XL7'00'                                 
         DC    C'KZOQF',C'2373',C'2379',XL7'00'                                 
         DC    C'KZORF',C'1635',C'3155',XL7'00'                                 
         DC    C'KZZPF',C'2851',C'0702',XL7'00'                                 
         DC    C'KZZYF',C'3215',C'0996',XL7'00'                                 
         SPACE 2                                                                
*          DATA SET SPEXTBSM1  AT LEVEL 001 AS OF 11/19/92                      
         SPACE                                                                  
MTABLE   DS   0C                                                                
         DC    C'WAAXA',C'1350',C'0108',XL7'00'                                 
         DC    C'WABKF',C'1367',C'0190',XL7'00'                                 
         DC    C'WACKA',C'2588',C'0185',XL7'00'                                 
         DC    C'WALFF',C'2356',C'3207',XL7'00'                                 
         DC    C'WAFYF',C'3819',C'1311',XL7'00'                                 
         DC    C'WAGOA',C'2845',C'3049',XL7'00'                                 
         DC    C'WAILF',C'3380',C'1847',XL7'00'                                 
         DC    C'WAJOA',C'2211',C'3302',XL7'00'                                 
         DC    C'WAJIF',C'1288',C'1341',XL7'00'                                 
         DC    C'WAITF',C'0698',C'2265',XL7'00'                                 
         DC    C'WAJYF',C'2573',C'0192',XL7'00'                                 
         DC    C'WAKGF',C'3106',C'3097',XL7'00'                                 
         DC    C'WAKQF',C'3173',C'4007',XL7'00'                                 
         DC    C'WALKF',C'2805',C'2533',XL7'00'                                 
         DC    C'WAMRA',C'3726',C'3257',XL7'00'                                 
         DC    C'WAMXF',C'0156',C'0102',XL7'00'                                 
         DC    C'WANBA',C'3869',C'3825',XL7'00'                                 
         DC    C'WAOAF',C'2724',C'2300',XL7'00'                                 
         DC    C'WAPLF',C'1459',C'0121',XL7'00'                                 
         DC    C'WAQEF',C'3081',C'0236',XL7'00'                                 
         DC    C'WAUDA',C'0183',C'2709',XL7'00'                                 
         DC    C'WAUKA',C'3845',C'2370',XL7'00'                                 
         DC    C'WAVTF',C'2963',C'1579',XL7'00'                                 
         DC    C'WAVWF',C'3734',C'1282',XL7'00'                                 
         DC    C'WAYIF',C'0036',C'1402',XL7'00'                                 
         DC    C'WAYZF',C'3867',C'1508',XL7'00'                                 
         DC    C'WAZLA',C'1579',C'3947',XL7'00'                                 
         DC    C'WAZLF',C'1579',C'3947',XL7'00'                                 
         DC    C'WAZYF',C'1907',C'1708',XL7'00'                                 
         DC    C'WBABA',C'0211',C'2533',XL7'00'                                 
         DC    C'WBABF',C'0211',C'2533',XL7'00'                                 
         DC    C'WBBIA',C'0012',C'2210',XL7'00'                                 
         DC    C'WBBNF',C'1954',C'1561',XL7'00'                                 
         DC    C'WBBOF',C'1263',C'1475',XL7'00'                                 
         DC    C'WBCPA',C'3436',C'0654',XL7'00'                                 
         DC    C'WBELA',C'0304',C'3126',XL7'00'                                 
         DC    C'WBEVA',C'0274',C'1661',XL7'00'                                 
         DC    C'WBGAF',C'3565',C'0491',XL7'00'                                 
         DC    C'WBHVF',C'3489',C'1791',XL7'00'                                 
         DC    C'WBILA',C'3678',C'2709',XL7'00'                                 
         DC    C'WBLIF',C'2805',C'2533',XL7'00'                                 
         DC    C'WBLYA',C'3435',C'0940',XL7'00'                                 
         DC    C'WBNZF',C'2187',C'3645',XL7'00'                                 
         DC    C'WBRMA',C'2218',C'0828',XL7'00'                                 
         DC    C'WBSKA',C'2631',C'2631',XL7'00'                                 
         DC    C'WBSSF',C'3748',C'0173',XL7'00'                                 
         DC    C'WBTMA',C'0933',C'3097',XL7'00'                                 
         DC    C'WBULF',C'0497',C'0728',XL7'00'                                 
         DC    C'WBUSF',C'1818',C'0698',XL7'00'                                 
         DC    C'WBWBF',C'0375',C'1708',XL7'00'                                 
         DC    C'WBXQF',C'0071',C'1792',XL7'00'                                 
         DC    C'WBYRF',C'0499',C'1341',XL7'00'                                 
         DC    C'WBZKA',C'4035',C'3123',XL7'00'                                 
         DC    C'WCBHF',C'0618',C'3588',XL7'00'                                 
         DC    C'WCCQF',C'0698',C'1797',XL7'00'                                 
         DC    C'WCDOF',C'3359',C'2655',XL7'00'                                 
         DC    C'WCEIF',C'1075',C'0640',XL7'00'                                 
         DC    C'WCEMF',C'0543',C'0640',XL7'00'                                 
         DC    C'WCENF',C'1252',C'3187',XL7'00'                                 
         DC    C'WCFLF',C'2449',C'1797',XL7'00'                                 
         DC    C'WCGYF',C'1962',C'0413',XL7'00'                                 
         DC    C'WCHOF',C'3826',C'0705',XL7'00'                                 
         DC    C'WCJLF',C'2308',C'1459',XL7'00'                                 
         DC    C'WCJUA',C'0801',C'2277',XL7'00'                                 
         DC    C'WCLOA',C'1763',C'2165',XL7'00'                                 
         DC    C'WCLRF',C'0698',C'0940',XL7'00'                                 
         DC    C'WCLTF',C'2587',C'0807',XL7'00'                                 
         DC    C'WCMIA',C'0156',C'1688',XL7'00'                                 
         DC    C'WCMPF',C'2865',C'2437',XL7'00'                                 
         DC    C'WCMSF',C'2630',C'2631',XL7'00'                                 
         DC    C'WCODF',C'1698',C'0579',XL7'00'                                 
         DC    C'WCSMF',C'0632',C'3451',XL7'00'                                 
         DC    C'WCSWF',C'2377',C'0236',XL7'00'                                 
         DC    C'WCTKF',C'0413',C'2556',XL7'00'                                 
         DC    C'WCTQF',C'3726',C'3257',XL7'00'                                 
         DC    C'WCTYF',C'2654',C'2571',XL7'00'                                 
         DC    C'WCVIA',C'0827',C'1472',XL7'00'                                 
         DC    C'WCVQF',C'2535',C'0731',XL7'00'                                 
         DC    C'WCZIF',C'1480',C'3818',XL7'00'                                 
         DC    C'WDAEA',C'3571',C'3570',XL7'00'                                 
         DC    C'WDARF',C'0935',C'1255',XL7'00'                                 
         DC    C'WDBFA',C'0976',C'3901',XL7'00'                                 
         DC    C'WDBRF',C'0654',C'3434',XL7'00'                                 
         DC    C'WDCGF',C'3034',C'3037',XL7'00'                                 
         DC    C'WDDCF',C'2941',C'0226',XL7'00'                                 
         DC    C'WDEKF',C'0946',C'3450',XL7'00'                                 
         DC    C'WDHAF',C'1025',C'2585',XL7'00'                                 
         DC    C'WDHRF',C'1879',C'1574',XL7'00'                                 
         DC    C'WDLTF',C'2385',C'2387',XL7'00'                                 
         DC    C'WDNCA',C'1058',C'3037',XL7'00'                                 
         DC    C'WDRCA',C'1551',C'1552',XL7'00'                                 
         DC    C'WDRCF',C'1551',C'1552',XL7'00'                                 
         DC    C'WDRKF',C'1477',C'2780',XL7'00'                                 
         DC    C'WDRMF',C'0960',C'1692',XL7'00'                                 
         DC    C'WDSTF',C'0036',C'3261',XL7'00'                                 
         DC    C'WDUVF',C'0423',C'3257',XL7'00'                                 
         DC    C'WDUXF',C'3847',C'3503',XL7'00'                                 
         DC    C'WDVAA',C'0933',C'3097',XL7'00'                                 
         DC    C'WDXEA',C'1964',C'3004',XL7'00'                                 
         DC    C'WDZKF',C'0695',C'3123',XL7'00'                                 
         DC    C'WECOF',C'3810',C'0835',XL7'00'                                 
         DC    C'WEFXF',C'0451',C'2565',XL7'00'                                 
         DC    C'WEGZF',C'1040',C'0155',XL7'00'                                 
         DC    C'WEIRA',C'3879',C'3502',XL7'00'                                 
         DC    C'WELAF',C'1060',C'2880',XL7'00'                                 
         DC    C'WELDA',C'1245',C'3095',XL7'00'                                 
         DC    C'WEOLA',C'1140',C'0750',XL7'00'                                 
         DC    C'WEOWF',C'2335',C'1847',XL7'00'                                 
         DC    C'WEQXF',C'2185',C'3179',XL7'00'                                 
         DC    C'WERZF',C'1192',C'2950',XL7'00'                                 
         DC    C'WESAF',C'0660',C'2880',XL7'00'                                 
         DC    C'WESRF',C'3575',C'0295',XL7'00'                                 
         DC    C'WEVEF',C'1185',C'3750',XL7'00'                                 
         DC    C'WEVRF',C'3102',C'2312',XL7'00'                                 
         DC    C'WEZXF',C'3286',C'3947',XL7'00'                                 
         DC    C'WEZYF',C'0778',C'1920',XL7'00'                                 
         DC    C'WFAWA',C'1333',C'3834',XL7'00'                                 
         DC    C'WFBCF',C'1482',C'1475',XL7'00'                                 
         DC    C'WFCLA',C'0768',C'3326',XL7'00'                                 
         DC    C'WFGMF',C'1207',C'0728',XL7'00'                                 
         DC    C'WFGYF',C'1791',C'0071',XL7'00'                                 
         DC    C'WFHNF',C'1698',C'2556',XL7'00'                                 
         DC    C'WFLOF',C'1228',C'1846',XL7'00'                                 
         DC    C'WFOGF',C'2630',C'2631',XL7'00'                                 
         DC    C'WFONF',C'1459',C'0121',XL7'00'                                 
         DC    C'WFPGF',C'0172',C'0173',XL7'00'                                 
         DC    C'WFQXF',C'1331',C'3972',XL7'00'                                 
         DC    C'WFRAF',C'1305',C'1606',XL7'00'                                 
         DC    C'WFRDF',C'1530',C'2532',XL7'00'                                 
         DC    C'WFTNF',C'0413',C'2182',XL7'00'                                 
         DC    C'WGBIF',C'3286',C'3947',XL7'00'                                 
         DC    C'WGCOF',C'0504',C'3265',XL7'00'                                 
         DC    C'WGESF',C'2748',C'3557',XL7'00'                                 
         DC    C'WGGDF',C'2300',C'0778',XL7'00'                                 
         DC    C'WGGOF',C'1352',C'2670',XL7'00'                                 
         DC    C'WGIRF',C'0413',C'2182',XL7'00'                                 
         DC    C'WGLRA',C'2969',C'1039',XL7'00'                                 
         DC    C'WGLRF',C'1932',C'1039',XL7'00'                                 
         DC    C'WGMGF',C'0171',C'3148',XL7'00'                                 
         DC    C'WGNEF',C'2780',C'0942',XL7'00'                                 
         DC    C'WGNYF',C'2593',C'2418',XL7'00'                                 
         DC    C'WGOLF',C'2139',C'3106',XL7'00'                                 
         DC    C'WGRMF',C'1487',C'1488',XL7'00'                                 
         DC    C'WGSMA',C'1686',C'2533',XL7'00'                                 
         DC    C'WGTCF',C'0375',C'3404',XL7'00'                                 
         DC    C'WGTRF',C'1361',C'1413',XL7'00'                                 
         DC    C'WGTYF',C'1392',C'4036',XL7'00'                                 
         DC    C'WGULA',C'2578',C'3570',XL7'00'                                 
         DC    C'WGULF',C'2578',C'3570',XL7'00'                                 
         DC    C'WGUSA',C'2638',C'0192',XL7'00'                                 
         DC    C'WHEPA',C'1259',C'2387',XL7'00'                                 
         DC    C'WHFMF',C'3410',C'2533',XL7'00'                                 
         DC    C'WHFXF',C'1756',C'0491',XL7'00'                                 
         DC    C'WHKQF',C'3026',C'2370',XL7'00'                                 
         DC    C'WHKRF',C'2724',C'0778',XL7'00'                                 
         DC    C'WHLMF',C'0379',C'3947',XL7'00'                                 
         DC    C'WHMPA',C'2642',C'3433',XL7'00'                                 
         DC    C'WHMQF',C'1242',C'3628',XL7'00'                                 
         DC    C'WHNNF',C'1252',C'3187',XL7'00'                                 
         DC    C'WHNRA',C'3999',C'1920',XL7'00'                                 
         DC    C'WHOKF',C'1930',C'0807',XL7'00'                                 
         DC    C'WHOUF',C'1667',C'2976',XL7'00'                                 
         DC    C'WHPAF',C'1791',C'0071',XL7'00'                                 
         DC    C'WHPTF',C'3570',C'3257',XL7'00'                                 
         DC    C'WHRZF',C'2996',C'1659',XL7'00'                                 
         DC    C'WHSMF',C'2377',C'0236',XL7'00'                                 
         DC    C'WHTEF',C'3818',C'1481',XL7'00'                                 
         DC    C'WHTFF',C'3488',C'4036',XL7'00'                                 
         DC    C'WHTGF',C'1080',C'2909',XL7'00'                                 
         DC    C'WHTHA',C'2587',C'0807',XL7'00'                                 
         DC    C'WHTXF',C'2880',C'4041',XL7'00'                                 
         DC    C'WHVEF',C'3257',C'2724',XL7'00'                                 
         DC    C'WHVYF',C'3433',C'0221',XL7'00'                                 
         DC    C'WHZTF',C'3436',C'0654',XL7'00'                                 
         DC    C'WICKA',C'3286',C'3947',XL7'00'                                 
         DC    C'WIGMF',C'2312',C'2294',XL7'00'                                 
         DC    C'WIHNF',C'2624',C'0378',XL7'00'                                 
*        DC    C'WIKBF',C'1724',C'2618',XL7'00'                                 
         DC    C'WIKXF',C'0037',C'0351',XL7'00'                                 
         DC    C'WILAA',C'0933',C'3097',XL7'00'                                 
         DC    C'WILIF',C'3956',C'1854',XL7'00'                                 
         DC    C'WIMTF',C'2042',C'3451',XL7'00'                                 
         DC    C'WINLF',C'2319',C'1179',XL7'00'                                 
         DC    C'WIOGF',C'1252',C'3187',XL7'00'                                 
*        DC    C'WIOIA',C'2951',C'1300',XL7'00'                                 
         DC    C'WIOVF',C'1539',C'1929',XL7'00'                                 
         DC    C'WIQQF',C'1483',C'1488',XL7'00'                                 
         DC    C'WIROA',C'1726',C'1688',XL7'00'                                 
         DC    C'WIRXF',C'3463',C'3404',XL7'00'                                 
         DC    C'WISMF',C'2165',C'1081',XL7'00'                                 
         DC    C'WIXKF',C'2579',C'2312',XL7'00'                                 
         DC    C'WIZDF',C'1282',C'3503',XL7'00'                                 
         DC    C'WJACA',C'1791',C'1792',XL7'00'                                 
         DC    C'WJATF',C'3545',C'3742',XL7'00'                                 
         DC    C'WJBDF',C'3196',C'0641',XL7'00'                                 
         DC    C'WJCWA',C'1789',C'0461',XL7'00'                                 
         DC    C'WJDXA',C'1740',C'2118',XL7'00'                                 
         DC    C'WJEQF',C'2148',C'1358',XL7'00'                                 
         DC    C'WJERF',C'1029',C'0574',XL7'00'                                 
         DC    C'WJJBF',C'1697',C'3095',XL7'00'                                 
         DC    C'WJLKA',C'0143',C'2909',XL7'00'                                 
         DC    C'WJLKF',C'2585',C'2909',XL7'00'                                 
         DC    C'WJLQF',C'2387',C'2825',XL7'00'                                 
         DC    C'WJLTF',C'1288',C'1341',XL7'00'                                 
         DC    C'WJLWF',C'0950',C'1459',XL7'00'                                 
         DC    C'WJMCF',C'3081',C'0236',XL7'00'                                 
         DC    C'WJMQF',C'0768',C'3326',XL7'00'                                 
*        DC    C'WJNRF',C'1721',C'2618',XL7'00'                                 
         DC    C'WJOBA',C'3464',C'0698',XL7'00'                                 
         DC    C'WJPDA',C'1732',C'2228',XL7'00'                                 
         DC    C'WJRIA',C'2000',C'0828',XL7'00'                                 
         DC    C'WJRZF',C'2585',C'2909',XL7'00'                                 
         DC    C'WJYYF',C'0822',C'2182',XL7'00'                                 
         DC    C'WJZQF',C'1833',C'0698',XL7'00'                                 
         DC    C'WKAJA',C'3258',C'0036',XL7'00'                                 
*        DC    C'WKBCF',C'2522',C'3940',XL7'00'                                 
         DC    C'WKBEF',C'0036',C'1402',XL7'00'                                 
         DC    C'WKBHF',C'1885',C'1883',XL7'00'                                 
         DC    C'WKBIA',C'3469',C'3096',XL7'00'                                 
         DC    C'WKCIF',C'1551',C'2565',XL7'00'                                 
         DC    C'WKCQF',C'1252',C'3187',XL7'00'                                 
         DC    C'WKDWA',C'1544',C'1545',XL7'00'                                 
         DC    C'WKFRF',C'0254',C'1812',XL7'00'                                 
         DC    C'WKFXF',C'1459',C'0121',XL7'00'                                 
         DC    C'WKGRF',C'1282',C'3901',XL7'00'                                 
         DC    C'WKHYF',C'1907',C'1708',XL7'00'                                 
         DC    C'WKIOF',C'3702',C'0654',XL7'00'                                 
         DC    C'WKIXA',C'3034',C'3037',XL7'00'                                 
         DC    C'WKJNF',C'1519',C'0253',XL7'00'                                 
         DC    C'WKJXF',C'1110',C'2631',XL7'00'                                 
         DC    C'WKKIF',C'0632',C'3451',XL7'00'                                 
         DC    C'WKLAF',C'2131',C'3645',XL7'00'                                 
         DC    C'WKMIA',C'1444',C'1812',XL7'00'                                 
         DC    C'WKMZF',C'3819',C'2244',XL7'00'                                 
         DC    C'WKNEF',C'1825',C'2532',XL7'00'                                 
         DC    C'WKOJF',C'2585',C'2418',XL7'00'                                 
         DC    C'WKPEF',C'1698',C'0579',XL7'00'                                 
         DC    C'WKPLF',C'2165',C'2969',XL7'00'                                 
         DC    C'WKPTA',C'1861',C'0461',XL7'00'                                 
         DC    C'WKQSF',C'1791',C'1792',XL7'00'                                 
         DC    C'WKQZF',C'1252',C'3187',XL7'00'                                 
         DC    C'WKREF',C'1193',C'0295',XL7'00'                                 
         DC    C'WKRGA',C'2385',C'2387',XL7'00'                                 
         DC    C'WKRGF',C'2385',C'2387',XL7'00'                                 
         DC    C'WKRZF',C'3944',C'3947',XL7'00'                                 
         DC    C'WKSJF',C'2385',C'2387',XL7'00'                                 
         DC    C'WKSQF',C'1131',C'0225',XL7'00'                                 
         DC    C'WKWMA',C'1834',C'1444',XL7'00'                                 
         DC    C'WKXCF',C'0681',C'0192',XL7'00'                                 
         DC    C'WKXDF',C'2535',C'0835',XL7'00'                                 
         DC    C'WKXNF',C'1476',C'2432',XL7'00'                                 
         DC    C'WLCQF',C'0734',C'1846',XL7'00'                                 
         DC    C'WLDYF',C'1901',C'0236',XL7'00'                                 
         DC    C'WLENF',C'0017',C'0994',XL7'00'                                 
         DC    C'WLERF',C'1074',C'0526',XL7'00'                                 
         DC    C'WLETA',C'3624',C'0164',XL7'00'                                 
         DC    C'WLEVF',C'1074',C'0058',XL7'00'                                 
         DC    C'WLKAF',C'0562',C'3119',XL7'00'                                 
         DC    C'WLLRF',C'2392',C'0938',XL7'00'                                 
         DC    C'WLMXF',C'3156',C'0681',XL7'00'                                 
         DC    C'WLRQF',C'2724',C'2300',XL7'00'                                 
         DC    C'WLRVA',C'1985',C'2647',XL7'00'                                 
         DC    C'WLRZF',C'2838',C'3441',XL7'00'                                 
         DC    C'WLSTF',C'2209',C'1459',XL7'00'                                 
         DC    C'WLTYF',C'2630',C'2631',XL7'00'                                 
         DC    C'WLXRF',C'1885',C'1883',XL7'00'                                 
         DC    C'WMAGF',C'1615',C'1471',XL7'00'                                 
         DC    C'WMBSA',C'3699',C'2880',XL7'00'                                 
         DC    C'WMBTA',C'3343',C'1579',XL7'00'                                 
         DC    C'WMCDF',C'3490',C'3742',XL7'00'                                 
         DC    C'WMCIF',C'3436',C'0668',XL7'00'                                 
         DC    C'WMDKF',C'2840',C'2532',XL7'00'                                 
         DC    C'WMDMF',C'2033',C'3701',XL7'00'                                 
         DC    C'WMEEF',C'1288',C'1341',XL7'00'                                 
         DC    C'WMEVF',C'2210',C'0461',XL7'00'                                 
         DC    C'WMFRA',C'1615',C'1471',XL7'00'                                 
         DC    C'WMGFF',C'2417',C'2724',XL7'00'                                 
         DC    C'WMGQF',C'2560',C'2585',XL7'00'                                 
         DC    C'WMGVF',C'1459',C'0121',XL7'00'                                 
         DC    C'WMIDF',C'2890',C'0173',XL7'00'                                 
         DC    C'WMIMA',C'2466',C'1579',XL7'00'                                 
         DC    C'WMINA',C'3473',C'2377',XL7'00'                                 
         DC    C'WMIXF',C'2494',C'0641',XL7'00'                                 
         DC    C'WMJDF',C'1497',C'2647',XL7'00'                                 
         DC    C'WMJQF',C'3119',C'0499',XL7'00'                                 
         DC    C'WMKLF',C'2592',C'3645',XL7'00'                                 
         DC    C'WMKMA',C'1666',C'0994',XL7'00'                                 
         DC    C'WMMBA',C'2300',C'2300',XL7'00'                                 
         DC    C'WMPIF',C'3284',C'2118',XL7'00'                                 
         DC    C'WMQQF',C'2118',C'0228',XL7'00'                                 
         DC    C'WMQTF',C'1732',C'2228',XL7'00'                                 
         DC    C'WMTYF',C'1475',C'0099',XL7'00'                                 
         DC    C'WMTZF',C'2241',C'0192',XL7'00'                                 
         DC    C'WMVRF',C'3357',C'0579',XL7'00'                                 
         DC    C'WMXNF',C'2630',C'2631',XL7'00'                                 
         DC    C'WMYIF',C'1482',C'1475',XL7'00'                                 
         DC    C'WMYTF',C'1475',C'3964',XL7'00'                                 
         DC    C'WMYUF',C'3312',C'1879',XL7'00'                                 
         DC    C'WMZKF',C'0994',C'2794',XL7'00'                                 
         DC    C'WNAWA',C'0036',C'2881',XL7'00'                                 
         DC    C'WNCEF',C'1539',C'1929',XL7'00'                                 
         DC    C'WNCOF',C'0150',C'2194',XL7'00'                                 
         DC    C'WNDHF',C'2530',C'0968',XL7'00'                                 
         DC    C'WNFIF',C'2724',C'0942',XL7'00'                                 
         DC    C'WNHCA',C'1557',C'2565',XL7'00'                                 
         DC    C'WNHQF',C'0413',C'2532',XL7'00'                                 
         DC    C'WNIRF',C'3044',C'0026',XL7'00'                                 
         DC    C'WNKOF',C'2587',C'0807',XL7'00'                                 
         DC    C'WNMBF',C'2521',C'1255',XL7'00'                                 
         DC    C'WNNCA',C'2615',C'0828',XL7'00'                                 
         DC    C'WNNHF',C'0822',C'2182',XL7'00'                                 
         DC    C'WNNJF',C'2614',C'3398',XL7'00'                                 
         DC    C'WNNKF',C'1539',C'1540',XL7'00'                                 
         DC    C'WNNOF',C'4001',C'0226',XL7'00'                                 
         DC    C'WNPQF',C'2574',C'0574',XL7'00'                                 
         DC    C'WNPTA',C'3673',C'2319',XL7'00'                                 
         DC    C'WNRGA',C'1497',C'2647',XL7'00'                                 
         DC    C'WNSLF',C'1954',C'1561',XL7'00'                                 
         DC    C'WNUSA',C'3433',C'2797',XL7'00'                                 
         DC    C'WNVZF',C'2630',C'2631',XL7'00'                                 
         DC    C'WNWNF',C'0785',C'1812',XL7'00'                                 
*        DC    C'WNXTF',C'2951',C'1300',XL7'00'                                 
         DC    C'WOBMF',C'2585',C'2909',XL7'00'                                 
         DC    C'WOBRF',C'3789',C'2631',XL7'00'                                 
         DC    C'WOCQF',C'0327',C'3207',XL7'00'                                 
         DC    C'WODEF',C'2845',C'0058',XL7'00'                                 
         DC    C'WOKAF',C'1019',C'0491',XL7'00'                                 
         DC    C'WOKIF',C'2662',C'1879',XL7'00'                                 
         DC    C'WOKQF',C'1026',C'2950',XL7'00'                                 
         DC    C'WOMPA',C'0289',C'3923',XL7'00'                                 
         DC    C'WOSHA',C'2733',C'0121',XL7'00'                                 
         DC    C'WOSXF',C'3850',C'2239',XL7'00'                                 
         DC    C'WDVVF',C'1282',C'3901',XL7'00'                                 
         DC    C'WOWIF',C'2631',C'0693',XL7'00'                                 
         DC    C'WOWQF',C'1036',C'1791',XL7'00'                                 
         DC    C'WOZZF',C'2570',C'0121',XL7'00'                                 
*        DC    C'WPAYF',C'2951',C'1300',XL7'00'                                 
         DC    C'WPCNA',C'2464',C'0058',XL7'00'                                 
         DC    C'WPCVF',C'3999',C'1920',XL7'00'                                 
         DC    C'WPGUF',C'3436',C'0654',XL7'00'                                 
         DC    C'WPKRF',C'3851',C'0121',XL7'00'                                 
         DC    C'WPLRF',C'1552',C'2565',XL7'00'                                 
         DC    C'WPNHF',C'2902',C'2182',XL7'00'                                 
         DC    C'WPPAA',C'2963',C'1579',XL7'00'                                 
         DC    C'WPQRF',C'3699',C'1472',XL7'00'                                 
         DC    C'WPRSA',C'2788',C'3588',XL7'00'                                 
         DC    C'WPSIF',C'3947',C'1579',XL7'00'                                 
         DC    C'WPSTF',C'2984',C'3648',XL7'00'                                 
         DC    C'WPTFA',C'3034',C'3037',XL7'00'                                 
         DC    C'WPUPF',C'0171',C'0164',XL7'00'                                 
         DC    C'WPXCF',C'1698',C'0579',XL7'00'                                 
         DC    C'WPXRF',C'3124',C'0938',XL7'00'                                 
         DC    C'WPYRF',C'0351',C'2302',XL7'00'                                 
         DC    C'WQBZF',C'1287',C'2151',XL7'00'                                 
         DC    C'WQCMF',C'3819',C'1508',XL7'00'                                 
         DC    C'WQDRF',C'3034',C'3037',XL7'00'                                 
         DC    C'WQEQF',C'1319',C'3947',XL7'00'                                 
         DC    C'WQHKA',C'1288',C'1341',XL7'00'                                 
         DC    C'WQHYF',C'2979',C'1574',XL7'00'                                 
         DC    C'WQLRF',C'1444',C'1812',XL7'00'                                 
         DC    C'WQLTF',C'1254',C'1692',XL7'00'                                 
         DC    C'WQMTF',C'0684',C'3148',XL7'00'                                 
         DC    C'WQMXF',C'0750',C'0026',XL7'00'                                 
         DC    C'WQNSF',C'3872',C'0149',XL7'00'                                 
         DC    C'WQNYF',C'3557',C'0863',XL7'00'                                 
         DC    C'WQOLF',C'3734',C'1282',XL7'00'                                 
         DC    C'WQSBF',C'0043',C'1502',XL7'00'                                 
         DC    C'WQSIA',C'3696',C'3819',XL7'00'                                 
         DC    C'WQSNA',C'1444',C'1812',XL7'00'                                 
         DC    C'WQTCF',C'3683',C'1845',XL7'00'                                 
         DC    C'WQTEF',C'0017',C'0994',XL7'00'                                 
         DC    C'WQTLF',C'2741',C'1242',XL7'00'                                 
         DC    C'WQWMA',C'1820',C'0121',XL7'00'                                 
         DC    C'WQXCF',C'1444',C'1812',XL7'00'                                 
         DC    C'WQXXF',C'2443',C'0828',XL7'00'                                 
         DC    C'WQZKF',C'1850',C'3095',XL7'00'                                 
         DC    C'WRALF',C'3034',C'3037',XL7'00'                                 
         DC    C'WRARA',C'3573',C'3804',XL7'00'                                 
         DC    C'WRCNF',C'3103',C'2533',XL7'00'                                 
         DC    C'WRCOF',C'3083',C'0226',XL7'00'                                 
         DC    C'WRDBA',C'3064',C'0226',XL7'00'                                 
         DC    C'WRDXF',C'3208',C'0674',XL7'00'                                 
         DC    C'WRELF',C'1544',C'1545',XL7'00'                                 
         DC    C'WRFXF',C'1819',C'0674',XL7'00'                                 
         DC    C'WRHIA',C'3123',C'0674',XL7'00'                                 
         DC    C'WRHMF',C'3123',C'0674',XL7'00'                                 
         DC    C'WRHTF',C'3818',C'1481',XL7'00'                                 
         DC    C'WRIXF',C'0149',C'1475',XL7'00'                                 
         DC    C'WRJCF',C'2259',C'3421',XL7'00'                                 
         DC    C'WRJNA',C'3026',C'2370',XL7'00'                                 
         DC    C'WRJOF',C'3850',C'1064',XL7'00'                                 
         DC    C'WRKIF',C'0475',C'2565',XL7'00'                                 
         DC    C'WRKRF',C'1444',C'1812',XL7'00'                                 
         DC    C'WRKXF',C'2742',C'3441',XL7'00'                                 
         DC    C'WRLSF',C'1573',C'0236',XL7'00'                                 
         DC    C'WROQF',C'0674',C'1475',XL7'00'                                 
         DC    C'WRQRF',C'1227',C'1481',XL7'00'                                 
         DC    C'WRRRF',C'3923',C'3126',XL7'00'                                 
         DC    C'WRSFF',C'1110',C'2631',XL7'00'                                 
         DC    C'WRVCA',C'0665',C'1688',XL7'00'                                 
         DC    C'WRWDF',C'2585',C'3261',XL7'00'                                 
         DC    C'WRXRF',C'0023',C'0192',XL7'00'                                 
         DC    C'WSAQF',C'2930',C'0994',XL7'00'                                 
         DC    C'WSBRA',C'0393',C'3901',XL7'00'                                 
         DC    C'WSCPA',C'3238',C'3557',XL7'00'                                 
         DC    C'WSEIF',C'2696',C'2480',XL7'00'                                 
         DC    C'WSFLF',C'0456',C'1481',XL7'00'                                 
         DC    C'WSHKF',C'1692',C'2515',XL7'00'                                 
         DC    C'WSHVF',C'3407',C'1846',XL7'00'                                 
         DC    C'WSJYF',C'1271',C'2165',XL7'00'                                 
         DC    C'WSMDF',C'1895',C'3701',XL7'00'                                 
         DC    C'WSMTF',C'3420',C'0835',XL7'00'                                 
         DC    C'WSNEF',C'3576',C'2556',XL7'00'                                 
         DC    C'WSTHF',C'0047',C'0809',XL7'00'                                 
         DC    C'WSTJA',C'3461',C'0234',XL7'00'                                 
         DC    C'WSTOF',C'2749',C'1184',XL7'00'                                 
         DC    C'WSTUA',C'3516',C'1282',XL7'00'                                 
         DC    C'WSULF',C'2426',C'2418',XL7'00'                                 
         DC    C'WSUNA',C'3571',C'3570',XL7'00'                                 
         DC    C'WSUSF',C'1304',C'3398',XL7'00'                                 
         DC    C'WSWNF',C'0291',C'3901',XL7'00'                                 
         DC    C'WSYNF',C'0805',C'1255',XL7'00'                                 
         DC    C'WSNIF',C'2845',C'3565',XL7'00'                                 
         DC    C'WTARA',C'2630',C'2631',XL7'00'                                 
         DC    C'WTAZF',C'2457',C'2830',XL7'00'                                 
         DC    C'WTBXF',C'1611',C'3750',XL7'00'                                 
         DC    C'WTCRF',C'0665',C'1688',XL7'00'                                 
         DC    C'WTGCA',C'2011',C'1579',XL7'00'                                 
         DC    C'WTICA',C'1551',C'1552',XL7'00'                                 
         DC    C'WTKXF',C'2387',C'2825',XL7'00'                                 
         DC    C'WTLZF',C'1252',C'3187',XL7'00'                                 
         DC    C'WTMPA',C'3571',C'3570',XL7'00'                                 
         DC    C'WTNSF',C'0868',C'4049',XL7'00'                                 
         DC    C'WTPAF',C'1539',C'1929',XL7'00'                                 
         DC    C'WTRGF',C'3034',C'3037',XL7'00'                                 
         DC    C'WTRPA',C'1889',C'1494',XL7'00'                                 
         DC    C'WTRSF',C'1052',C'2670',XL7'00'                                 
         DC    C'WTTSF',C'0375',C'1708',XL7'00'                                 
         DC    C'WTWRF',C'2404',C'3628',XL7'00'                                 
         DC    C'WTZQA',C'1599',C'0149',XL7'00'                                 
         DC    C'WUPSF',C'0528',C'3645',XL7'00'                                 
         DC    C'WUSWF',C'2546',C'0121',XL7'00'                                 
         DC    C'WVAYF',C'0516',C'3179',XL7'00'                                 
         DC    C'WVBSA',C'0505',C'3964',XL7'00'                                 
         DC    C'WVKMF',C'2257',C'3951',XL7'00'                                 
         DC    C'WVKZF',C'3271',C'0036',XL7'00'                                 
         DC    C'WVNAF',C'3677',C'2515',XL7'00'                                 
         DC    C'WVOWF',C'2084',C'3951',XL7'00'                                 
         DC    C'WVRQF',C'3754',C'3421',XL7'00'                                 
         DC    C'WVRYF',C'3855',C'0731',XL7'00'                                 
         DC    C'WVSCF',C'3397',C'1792',XL7'00'                                 
         DC    C'WVVVF',C'0365',C'3192',XL7'00'                                 
         DC    C'WVVXF',C'1617',C'0698',XL7'00'                                 
         DC    C'WVVYF',C'2557',C'1481',XL7'00'                                 
         DC    C'WWBEF',C'3947',C'1579',XL7'00'                                 
         DC    C'WWHTF',C'0667',C'0807',XL7'00'                                 
         DC    C'WWISF',C'0361',C'3421',XL7'00'                                 
         DC    C'WWJMF',C'0807',C'4049',XL7'00'                                 
         DC    C'WWKFF',C'3698',C'4007',XL7'00'                                 
         DC    C'WWKSF',C'0275',C'2880',XL7'00'                                 
         DC    C'WWKXF',C'1360',C'2997',XL7'00'                                 
         DC    C'WWKZF',C'2553',C'0810',XL7'00'                                 
         DC    C'WWMGF',C'3329',C'0674',XL7'00'                                 
         DC    C'WWMMF',C'0698',C'1475',XL7'00'                                 
         DC    C'WWTRF',C'0332',C'3207',XL7'00'                                 
         DC    C'WWWCA',C'2522',C'3948',XL7'00'                                 
         DC    C'WWWZF',C'3531',C'0667',XL7'00'                                 
         DC    C'WWYZF',C'1552',C'1569',XL7'00'                                 
         DC    C'WXBMF',C'2365',C'2825',XL7'00'                                 
         DC    C'WXCEF',C'0082',C'0236',XL7'00'                                 
         DC    C'WXCVF',C'0905',C'2670',XL7'00'                                 
         DC    C'WXCYF',C'1567',C'0221',XL7'00'                                 
         DC    C'WXFXF',C'2971',C'2417',XL7'00'                                 
         DC    C'WXGLF',C'2015',C'2948',XL7'00'                                 
         DC    C'WXKEF',C'1288',C'1341',XL7'00'                                 
         DC    C'WXKSA',C'2295',C'0413',XL7'00'                                 
         DC    C'WXLCF',C'3844',C'1503',XL7'00'                                 
         DC    C'WXLOF',C'1246',C'4019',XL7'00'                                 
         DC    C'WXLPF',C'2392',C'0938',XL7'00'                                 
         DC    C'WXLTF',C'2277',C'0253',XL7'00'                                 
         DC    C'WXQRF',C'1753',C'1481',XL7'00'                                 
         DC    C'WXRCF',C'1614',C'0674',XL7'00'                                 
         DC    C'WXROF',C'0274',C'1661',XL7'00'                                 
         DC    C'WXVAF',C'3819',C'2244',XL7'00'                                 
         DC    C'WXVWA',C'1775',C'2118',XL7'00'                                 
         DC    C'WXXKF',C'2600',C'2532',XL7'00'                                 
         DC    C'WXXQF',C'1322',C'3126',XL7'00'                                 
         DC    C'WYCOF',C'3852',C'3272',XL7'00'                                 
         DC    C'WYCQF',C'3334',C'2535',XL7'00'                                 
         DC    C'WYHTF',C'0750',C'2194',XL7'00'                                 
         DC    C'WYKXF',C'1164',C'1459',XL7'00'                                 
         DC    C'WYNSA',C'1993',C'0058',XL7'00'                                 
         DC    C'WYREA',C'0107',C'0640',XL7'00'                                 
         DC    C'WYSSF',C'2592',C'3645',XL7'00'                                 
         DC    C'WYSYF',C'0193',C'0698',XL7'00'                                 
         DC    C'WYTMF',C'1233',C'3004',XL7'00'                                 
         DC    C'WYURF',C'3099',C'2733',XL7'00'                                 
         DC    C'WYVEA',C'4023',C'2210',XL7'00'                                 
         DC    C'WYXLF',C'1733',C'0863',XL7'00'                                 
         DC    C'WZBOF',C'1086',C'2631',XL7'00'                                 
         DC    C'WZEWF',C'1204',C'2387',XL7'00'                                 
         DC    C'WZKXF',C'0347',C'0348',XL7'00'                                 
         DC    C'WZMXF',C'1557',C'1552',XL7'00'                                 
         DC    C'WZNFF',C'3436',C'3434',XL7'00'                                 
         DC    C'WZNLF',C'1721',C'1845',XL7'00'                                 
         DC    C'WZNSF',C'1002',C'1234',XL7'00'                                 
         DC    C'WZOMF',C'3628',C'0968',XL7'00'                                 
         DC    C'WZOOF',C'0750',C'2762',XL7'00'                                 
         DC    C'WZOQF',C'3790',C'1242',XL7'00'                                 
*        DC    C'WZOZF',C'2705',C'2651',XL7'00'                                 
         DC    C'WZPKF',C'0329',C'2948',XL7'00'                                 
         DC    C'WZPRF',C'2291',C'1162',XL7'00'                                 
         DC    C'WZRTF',C'0516',C'3179',XL7'00'                                 
         DC    C'WZVUF',C'2585',C'2909',XL7'00'                                 
         DC    C'WZXLF',C'3943',C'0173',XL7'00'                                 
         DC    C'WZXRF',C'2302',C'3952',XL7'00'                                 
         DC    C'WZYCF',C'0163',C'3818',XL7'00'                                 
         DC    C'WZYQF',C'1311',C'3819',XL7'00'                                 
         DC    C'WZZOF',C'0336',C'0058',XL7'00'                                 
         DC    C'WZZRF',C'2932',C'1282',XL7'00'                                 
         SPACE                                                                  
*          DATA SET SPEXTBSM2  AT LEVEL 001 AS OF 12/01/92                      
         SPACE                                                                  
         DC    C'KBBZF',C'3926',C'1816',XL7'00'                                 
         DC    C'KBFCF',C'1267',C'3900',XL7'00'                                 
         DC    C'KBOPF',C'2895',C'2960',XL7'00'                                 
         DC    C'KBQQF',C'2374',C'2378',XL7'00'                                 
         DC    C'KBRZA',C'1323',C'0103',XL7'00'                                 
         DC    C'KDJSA',C'2377',C'1463',XL7'00'                                 
         DC    C'KDUZA',C'1695',C'1463',XL7'00'                                 
         DC    C'KESMF',C'1106',C'1939',XL7'00'                                 
         DC    C'KFBDF',C'0800',C'3146',XL7'00'                                 
         DC    C'KFMNF',C'1693',C'2041',XL7'00'                                 
         DC    C'KFMWF',C'3832',C'0628',XL7'00'                                 
         DC    C'KGDDA',C'2790',C'0986',XL7'00'                                 
         DC    C'KGEEF',C'2679',C'2353',XL7'00'                                 
         DC    C'KGVLA',C'1484',C'1152',XL7'00'                                 
         DC    C'KHHTF',C'2374',C'2378',XL7'00'                                 
         DC    C'KHBMF',C'2420',C'0984',XL7'00'                                 
         DC    C'KHOKF',C'1640',C'1454',XL7'00'                                 
         DC    C'KHTTF',C'3222',C'3251',XL7'00'                                 
         DC    C'KIZZF',C'2374',C'2378',XL7'00'                                 
         DC    C'KKJRF',C'1695',C'1463',XL7'00'                                 
         DC    C'KKSIF',C'2734',C'2746',XL7'00'                                 
         DC    C'WJMCA',C'3081',C'2377',XL7'00'                                 
         DC    C'KLUAF',C'1653',C'1875',XL7'00'                                 
         DC    C'KMASA',C'3341',C'2697',XL7'00'                                 
         DC    C'KMXKF',C'2377',C'3449',XL7'00'                                 
         DC    C'KNTIF',C'3221',C'2340',XL7'00'                                 
         DC    C'KOAQA',C'0697',C'3279',XL7'00'                                 
         DC    C'KOGOA',C'3218',C'3727',XL7'00'                                 
         DC    C'KOLAF',C'3218',C'3216',XL7'00'                                 
         DC    C'KOWLA',C'3389',C'3181',XL7'00'                                 
         DC    C'KQENA',C'3153',C'1175',XL7'00'                                 
         DC    C'KQICF',C'3958',C'1463',XL7'00'                                 
         DC    C'KQIPF',C'2679',C'2353',XL7'00'                                 
         DC    C'KRIMF',C'2851',C'1411',XL7'00'                                 
         DC    C'KRPQF',C'3257',C'3251',XL7'00'                                 
         DC    C'KSJQF',C'3432',C'3462',XL7'00'                                 
         DC    C'KSLQF',C'3466',C'3821',XL7'00'                                 
         DC    C'KSPEA',C'3247',C'3244',XL7'00'                                 
         DC    C'KTRAF',C'0045',C'1224',XL7'00'                                 
         DC    C'KTUXF',C'3587',C'3684',XL7'00'                                 
         DC    C'KTWNF',C'3354',C'3592',XL7'00'                                 
         DC    C'KXJKA',C'1267',C'2302',XL7'00'                                 
         DC    C'WBOPF',C'1545',C'1544',XL7'00'                                 
         DC    C'KVOUA',C'1657',C'1651',XL7'00'                                 
         DC    C'KVWGA',C'2812',C'2960',XL7'00'                                 
         DC    C'KVWGF',C'2812',C'2960',XL7'00'                                 
         DC    C'KWINF',C'3519',C'3509',XL7'00'                                 
         DC    C'KWOZF',C'1329',C'2599',XL7'00'                                 
         DC    C'KWWWF',C'3810',C'3890',XL7'00'                                 
         DC    C'KWYNF',C'3863',C'3900',XL7'00'                                 
         DC    C'KXARF',C'1657',C'3592',XL7'00'                                 
         DC    C'KXMGF',C'2201',C'3663',XL7'00'                                 
         DC    C'KYOSA',C'2314',C'2390',XL7'00'                                 
         DC    C'KKORF',C'0045',C'1364',XL7'00'                                 
         DC    C'KKYRF',C'3354',C'3592',XL7'00'                                 
         DC    C'KIKTF',C'1484',C'0986',XL7'00'                                 
         DC    C'KGGIF',C'3104',C'3216',XL7'00'                                 
         DC    C'KIMPA',C'3354',C'2487',XL7'00'                                 
         DC    C'WHFBF',C'0322',C'3404',XL7'00'                                 
         DC    C'WKCYF',C'1545',C'1544',XL7'00'                                 
         DC    C'WKDWA',C'1545',C'1544',XL7'00'                                 
         DC    C'WRELF',C'1545',C'1544',XL7'00'                                 
         DC    C'KORDF',C'3652',C'4028',XL7'00'                                 
         DC    C'KBATF',C'2353',C'2349',XL7'00'                                 
         DC    C'KBBYF',C'2106',C'3727',XL7'00'                                 
         DC    C'KBFMF',C'1537',C'2272',XL7'00'                                 
         DC    C'KBTSF',C'3767',C'0203',XL7'00'                                 
         DC    C'KCALF',C'2106',C'3216',XL7'00'                                 
         DC    C'KCMXF',C'2293',C'0151',XL7'00'                                 
         DC    C'KDKSF',C'3354',C'0320',XL7'00'                                 
         DC    C'KDKSF',C'3354',C'0320',XL7'00'                                 
         DC    C'KEANF',C'0010',C'0486',XL7'00'                                 
         DC    C'KLLZA',C'3767',C'3587',XL7'00'                                 
         DC    C'KKBGF',C'1653',C'1625',XL7'00'                                 
         DC    C'KKPLF',C'3428',C'2708',XL7'00'                                 
         DC    C'KMGCF',C'2405',C'2404',XL7'00'                                 
         DC    C'KMVIF',C'1653',C'3771',XL7'00'                                 
         DC    C'KQZYF',C'0922',C'0924',XL7'00'                                 
         DC    C'KTEXF',C'1537',C'2272',XL7'00'                                 
         DC    C'KWSSF',C'3221',C'3222',XL7'00'                                 
MTABLEN  EQU   (*-MTABLE)/L'MTABENT                                             
         SPACE 2                                                                
MTABLE1N  EQU   (*-MTABLE1)/L'MTABENT                                           
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
MTABLED  DSECT                                                                  
MTABENT  DS   0XL20                                                             
MSTA     DS    CL5                                                              
MOLDMKT  DS    CL4                                                              
MNEWMKT  DS    CL4                                                              
         DS    XL7                                                              
MTABNXT  EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
STARECD   DSECT                                                                 
       ++INCLUDE SPGENSTA                                                       
MKTRECD   DSECT                                                                 
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009STEXTBS   12/05/92'                                      
         END                                                                    
