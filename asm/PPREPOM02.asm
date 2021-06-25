*          DATA SET PPREPOM02  AT LEVEL 007 AS OF 05/01/02                      
*PHASE PPOM02A                                                                  
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE QSORT                                                                  
*INCLUDE OMPUBTAB                                                               
         TITLE 'PPOM02- OGILVY PRINT CONVERSION PROGRAM'                        
         SPACE 1                                                                
PPOM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPOM02                                                         
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING PPOM02+4096,RA                                                   
*                                                                               
         L     R9,0(R1)                                                         
         USING PPWORKD,R9                                                       
*                                                                               
         L     RC,PPFILEC                                                       
         LR    R8,RC                                                            
         AH    R8,=H'4096'                                                      
         USING PPFILED,RC,R8                                                    
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    OM10                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 1                                                                
OM10     DS    0H                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         OPEN  (FILEOUT,(OUTPUT))                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         GOTO1 =A(FIXEDT),DMCB,(RC)   FIX CONV TABLE EDITION CODES              
*                                                                               
         L     R0,=V(PUBCTABX)        AND SORT THE CONV TABLE                   
         S     R0,=V(PUBCTAB)                                                   
         SRDL  R0,32                                                            
         D     R0,=F'13'           DIVIDE BY ENTRY LEN                          
         LTR   R0,R0               SHOULD HAVE NO REMAINDER                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,PUBCCNT          SET ENTRY COUNT IN  PARMS                    
*                                                                               
         LR    R0,R1               GET COUNT IN REG                             
         GOTO1 =V(QSORT),DMCB,V(PUBCTAB),(R0),13,7,0                            
*                                                                               
         GOTO1 =A(BLDACP),DMCB,(RC) BUILD CLT/PRD LIST                          
*                                                                               
         CLI   QOPT2,C'X'          TEST SUPPRESS PUB TABLE                      
         BE    OM20                                                             
         GOTO1 =A(BLDPUBS),DMCB,(RC)  BUILD PUBFILE LIST                        
         B     OM20                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2000'                                  
         EJECT                                                                  
OM20     DS    0H                                                               
         BAS   RE,GETFILE                                                       
*                                                                               
OM22     CLI   EOFSW,C'Y'          TEST EOF                                     
         BE    OM400                                                            
*                                                                               
         LA    R7,OMREC                                                         
         USING OMRECD,R7                                                        
*                                                                               
         CLI   0(R7),X'10'         TEST VALID AGY/MED                           
         BL    OM20                                                             
         CLI   0(R7),C'*'          TEST CONTROL RECORD                          
         BE    OM20                                                             
         CLI   MAGRECID,C'2'       TEST ESTIMATE HEADER                         
         BE    OM24                YES - CONVERT AGENCY                         
         CLI   MAGRECID,C'4'       TEST DETAIL RECORD                           
         BNE   OM20                                                             
         CLC   MAGINSDT,=X'870100' TEST 1987 DATA                               
         BL    OM20                                                             
         CLC   MAGINSDT,=X'871231' TEST 1987 DATA                               
         BH    OM20                                                             
*                                                                               
         CLC   MAGPR,=C'ZZZ'                                                    
         BNE   *+14                                                             
         AP    ZZZCOUNT,=P'1'                                                   
         B     OM20                                                             
*                                                                               
         TM    MAGDELTE,X'80'      TEST DELETED                                 
         BZ    OM24                NO                                           
         CLC   MAGPDDT,=C'CAN'     TEST CANCELLED                               
         BE    OM20                YES - SKIP                                   
         OC    MAGPDDT,MAGPDDT     TEST PAID                                    
         BNZ   OM24                                                             
         OC    MAGBLLDT,MAGBLLDT   OR BILLED                                    
         BNZ   OM24                                                             
         B     OM20                UNPAID/UNBILLED/DELETED ==> SKIP             
*                                                                               
OM24     ZIC   R4,MAGKEY           GET AGENCY/MEDIA                             
         SRL   R4,4                ISOLATE AGENCY                               
         BCTR  R4,0                                                             
         MH    R4,=H'3'            X 3                                          
         LA    R4,AGYTAB(R4)                                                    
         CLC   =C'XX',0(R4)        TEST TO CONVERT THIS AGY                     
         BE    OM20                NO                                           
         B     OM30                                                             
         EJECT                                                                  
* ENTRIES ARE AGENCY CODE/THIRD CHARACTER OF CLIENT CODE                        
*                                                                               
AGYTAB   DC    C'XX',C' '          1 = NETWORK                                  
         DC    C'OG',C'O'          2 = OUT OF HOME                              
         DC    C'XX',C' '          3 = OMSF                                     
         DC    C'XX',C' '          4 = SPECIAL MARKETS                          
         DC    C'OG',C' '          5 = OMNY                                     
         DC    C'XX',C' '          6 = HAL RINEY                                
         DC    C'OG',C'S'          7 = OMDA                                     
         DC    C'XX',C' '          8 = OM PARTNERS                              
         DC    C'OG',C'P'          9 = PROMOTIONS                               
         DC    C'XX',C' '          A = OMCH                                     
         DC    C'XX',C' '          B = OMHO                                     
         DC    C'XX',C' '          C = OMLA                                     
         DC    C'OG',C'D'          D = DIRECT RESPONSE                          
         DC    C'XX',C' '          E = HAL RINEY                                
         DC    C'XX',C' '          F = LOST DATA                                
*                                                                               
MEDTAB   DC    C'MN'                                                            
         EJECT                                                                  
* THIS RECORD TO BE CONVERTED *                                                 
         SPACE 1                                                                
OM30     DS    0H                                                               
         CLI   MAGRECID,C'2'       TEST ESTIMATE HEADER                         
         BE    OM300                                                            
         CLI   MAGSRD,C'D'         TEST SHORT RATED TO ANOTHER EST              
         BNE   OM32                                                             
         BAS   RE,SRERR                                                         
         B     OM20                                                             
*                                                                               
OM32     LA    R0,4                                                             
         LA    R1,PBUYREC                                                       
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVC   PBUYKAGY(2),0(R4)   MOVE AGENCY                                  
         ZIC   RE,OMREC                                                         
         N     RE,=X'0000000F'     DROP ALL BUT MEDIA                           
         CH    RE,=H'2'                                                         
         BNH   OM34                                                             
         BAS   RE,MEDERR                                                        
         B     OM20                                                             
*                                                                               
OM34     BCTR  RE,0                                                             
         LA    RE,MEDTAB(RE)                                                    
         MVC   PBUYKMED,0(RE)                                                   
*                                                                               
         MVC   PBUYKCLT(2),MAGCLI                                               
         MVC   PBUYKCLT+2(1),2(R4)   MOVE THIRD CHAR OF CLT CODE                
         CLI   2(R4),C'O'            IF = C'O', FORCE MEDIA                     
         BNE   OM36                                                             
         MVI   PBUYKMED,C'O'         TO OUTDOOR                                 
         TM    MAGAGMED,X'01'      TEST MAGAZINES                               
         BZ    OM36                YES                                          
         MVI   PBUYKCLT+2,C'M'                                                  
*                                                                               
OM36     MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKPRD,MAGPR                                                   
         OI    PBUYKPRD+2,X'40'                                                 
         MVC   PBUYKPUB,MAGPUBNO                                                
*                                                                               
         MVC   PBUYKDAT,MAGINSDT                                                
         LA    R4,PBUYKDAT                                                      
         BAS   RE,CONVDATE                                                      
*                                                                               
         MVC   PBUYKEST,MAGEST                                                  
         MVC   PBUYKLIN,MAGLINNO                                                
*                                                                               
         MVI   PBUYLEN+1,33+105+17   KEY+CNTL+PBDELEM+OMCONVEL                  
*                                                                               
         MVI   PBDELEM,X'20'                                                    
         MVI   PBDELEM+1,105                                                    
*                                                                               
         MVC   PBDBUYDT,=X'010101' SET RECOGNIZABLE CREATION DATE               
* MAKE SURE ALL PACKDED FIELDS ARE PACKED                                       
         ZAP   PBDUNITS,=P'0'                                                   
         ZAP   PBDCOS,=P'0'                                                     
         ZAP   PBDCD,=P'0'                                                      
         ZAP   PBDPRCOS,=P'0'                                                   
         ZAP   PBDCLMS,=P'0'                                                    
*                                                                               
         LA    R6,PBDELEM                                                       
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         MVI   0(R6),X'99'         CREATE OGILVY CONVERSION ELEMENT             
         MVI   1(R6),17                                                         
         MVC   2(3,R6),PBUYKCLT                                                 
         MVC   5(3,R6),PBUYKPRD                                                 
         MVC   8(2,R6),PBUYKEST                                                 
         MVC   10(6,R6),PBUYKPUB                                                
         MVC   16(1,R6),MAGLINNO                                                
*                                                                               
         TM    MAGAGMED,X'01'      TEST MAGAZINES                               
         BO    OM100               YES                                          
         B     OM200                                                            
         EJECT                                                                  
* MAGAZINES *                                                                   
         SPACE 1                                                                
OM100    DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),PBUYKMED                                                  
         MVC   DUB+1(6),PBUYKPUB                                                
         GOTO1 BINSRCH,PUBCPARS,DUB  CONVERT PUB NUMBER                         
         CLI   0(R1),X'01'         TEST NOT FOUND                               
         BE    OM102               IF NOT FOUND, KEEP OLD PUB CODE              
         L     RE,0(R1)            GET ENTRY ADDRESS                            
         MVC   DUB+1(6),7(RE)                                                   
         MVC   PBUYKPUB(6),7(RE)   GOOD IDEA TO SET IT IN RECORD TOO            
         SPACE 1                                                                
* NOW MAKE SURE ITS ON THE FILE                                                 
         SPACE 1                                                                
OM102    CLI   QOPT2,C'X'          TEST TABLE ACTIVE                            
         BE    OM104                                                            
         GOTO1 BINSRCH,PUBTPARS,DUB                                             
         CLI   0(R1),1             TEST NOT FOUND                               
         BNE   OM104                                                            
         BAS   RE,PUBERR                                                        
*                                                                               
OM104    BAS   RE,OMVAL            GET GROSS/AGYCOM/CSHDSC/TAX                  
*                                                                               
         CLI   PBUYKDAT+2,0        TEST INSERTION DAY = 00                      
         BNE   *+12                                                             
         MVI   PBUYKDAT+2,1                                                     
         MVI   PBDFREQ,C'M'                                                     
* NEED TO WORK OUT BILLABLE/PAYABLE DATES                                       
         LA    R0,2             INS MONTH -2 IF PUB STARTS W/ 3 OR 8            
         MVC   BYTE,PBUYKPUB                                                    
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'30'                                                       
         BE    OM106                                                            
         CLI   BYTE,X'80'                                                       
         BE    OM106                                                            
         LA    R0,1                ELSE INS MONTH -1                            
OM106    MVC   PBDBDATE,PBUYKDAT                                                
         ZIC   R1,PBDBDATE+1       GET INSERTION MONTH                          
         SR    R1,R0                                                            
         LTR   R1,R1                                                            
         BP    OM107                                                            
         BZ    OM106A                                                           
         LA    R1,11               -1 BECOMES NOVEMBER                          
         B     *+8                                                              
OM106A   LA    R1,12               0 BECOMES DECEMBER                           
         IC    RE,PBDBDATE         FORCE TO PRIOR YEAR                          
         BCTR  RE,0                                                             
         STC   RE,PBDBDATE                                                      
*                                                                               
OM107    STC   R1,PBDBDATE+1       SET MONTH                                    
         MVI   PBDBDATE+2,1        AND FORCE DAY TO 1                           
*                                                                               
         MVC   PBDPDATE,PBDBDATE   PAYABLE DATE = BILLABLE + 1 MONTH            
         ZIC   R1,PBDPDATE+1       GET INSERTION MONTH                          
         LA    R1,1(R1)                                                         
         STC   R1,PBDPDATE+1                                                    
         CLI   PBDPDATE+1,13                                                    
         BL    OM108                                                            
         MVI   PBDPDATE+1,1                                                     
         IC    RE,PBDPDATE         FORCE TO NEXT YEAR                           
         LA    RE,1(RE)                                                         
         STC   RE,PBDPDATE                                                      
*                                                                               
OM108    OC    MAGEST90,MAGEST90                                                
         BZ    *+12                                                             
         BAS   RE,EST90ERR                                                      
         B     OM20                                                             
*                                                                               
         ZAP   PBDCOS,MAGGRSRT                                                  
         MVI   PBDCOSIN,C' '                                                    
*                                                                               
OM110    DS    0H                                                               
         CLC   MAGDISC,=X'FFFF'    TEST FOR BAD DISCOUNT VALUE                  
         BNE   *+10                                                             
         XC    MAGDISC,MAGDISC     TRY NOT TO BLOW UP                           
         SPACE 1                                                                
         OC    MAGPDDT,MAGPDDT     TEST PAID                                    
         BZ    *+8                                                              
         BAS   RE,OMPAYEL                                                       
*                                                                               
         OC    MAGBLLDT,MAGBLLDT   TEST BILLED                                  
         BZ    *+8                                                              
         BAS   RE,OMBILEL                                                       
*                                                                               
         GOTO1 DATCON,DMCB,MAGACTDT,(3,PBDDATE)  ACTIVITY DATE                  
*                                                                               
         TM    MAGDELTE,X'80'      TEST DELETED                                 
         BZ    *+8                                                              
         OI    PBUYCNTL,X'80'      SET DELETED IND                              
*                                                                               
         ZAP   DUB,=P'0'           GET CASH DISCOUNT AMOUNT                     
         MVO   DUB,MAGDISC                                                      
         DP    DUB,=P'10'          SCALE FROM 2 DECIMALS TO 1                   
         ZAP   PBDCD,DUB(6)                                                     
*                                                                               
         OC    MAGTAX,MAGTAX                                                    
         BZ    OM115                                                            
         CP    MAGTAX,=P'0'                                                     
         BZ    OM115                                                            
         ZAP   DUB,MAGTAX                                                       
         MP    DUB,=P'100'         SCALE FROM 2 DECIMAL TO 4                    
         ZAP   PBDTAX,DUB                                                       
*                                                                               
OM111    CLI   PBUYKMED,C'O'       TEST MEDIA OUTDOOR                           
         BNE   OM115                                                            
         MVI   PBDSPACE,X'FF'                                                   
         ZAP   PBDSHOW,=P'0'                                                    
         ZAP   PBDREG,=P'0'                                                     
         ZAP   PBDILLUM,=P'0'                                                   
* CREATE A COMMENT ELEMENT FOR ORIGINAL SPACE DESCRIPTION                       
         LA    R6,PBDELEM                                                       
OM113    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   OM113                                                            
*                                                                               
         XC    0(51,R6),0(R6)      CLEAR ELEM LEN + 2                           
         MVI   0(R6),X'66'                                                      
         MVI   1(R6),49                                                         
         MVC   2(47,R6),NWSSPDSC                                                
*                                                                               
         SR    RE,RE               SET NEW RECORD LENGTH                        
         ICM   RE,3,PBUYLEN                                                     
         LA    RE,49(RE)                                                        
         STCM  RE,3,PBUYLEN                                                     
         B     OM117                                                            
*                                                                               
OM115    MVC   PBDSPACE,MAGSPDSC                                                
         CLC   MAGSPDSC+17(8),SPACES                                            
         BNH   *+8                                                              
         BAS   RE,SPDSCERR                                                      
*                                                                               
OM117    MVC   PBDSDATE,MAGONSDT                                                
         LA    R4,PBDSDATE                                                      
         BAS   RE,CONVDATE                                                      
*                                                                               
         MVC   PBDCDATE,MAGCLODT                                                
         LA    R4,PBDCDATE                                                      
         BAS   RE,CONVDATE                                                      
*                                                                               
OM120    CLI   QOPT1,C'Y'          TEST MAGAZINE TRACE ON                       
         BNE   OM125                                                            
         BAS   RE,OMTRACE         PRINT OM REC TRACE                            
         BAS   RE,DDSTRACE         PRINT DDS RECORD TRACE                       
*                                                                               
OM125    MVC   SVOMKEY,OMREC                                                    
*                                                                               
         BAS   RE,GETFILE                                                       
*                                                                               
         CLC   SVOMKEY(18),OMREC   TEST SAME KEY                                
         BE    OM130               YES - ADD MORE ELEMENTS                      
*                                                                               
         LA    R1,PBUYREC                                                       
         BAS   RE,PUTSORT          NO - PUT DDS RECORD                          
         AP    MAGCOUNT,=P'1'                                                   
         B     OM22                AND CREATE NEW                               
*                                                                               
OM130    CLI   MAGRECID,C'4'       TEST DETAIL RECORD                           
         BNE   OM140                                                            
*                                                                               
         BAS   RE,OMVAL            GET GROSS/NET/ETC                            
         AP    PBDCOS,MAGGRSRT     UPDATE GROSS RATE                            
         B     OM110               AND ADD BILL/PAY DATA AS NEEDED              
         SPACE 1                                                                
*                                                                               
OM140    CLI   MAGRECID,C'9'       TEST COMMENT                                 
         BE    *+12                                                             
         BAS   RE,RECIDERR                                                      
         B     OM20                                                             
*                                                                               
         BAS   RE,OMCOMEL                                                       
         B     OM22                RECORD WAS PUT BY OMCOMEL                    
*                                                                               
         EJECT                                                                  
**************                                                                  
* NEWSPAPERS *                                                                  
**************                                                                  
         SPACE 1                                                                
OM200    DS    0H                                                               
         PACK  DUB,NWSZONE(3)      GET ZONE IN PWOS                             
         CLC   NWSZONE,=C'60'      CONDENSE SPECIAL CODES TO MAIN PUB           
         BL    *+10                                                             
         ZAP   DUB,=P'0'                                                        
         MVC   PBUYKZON,DUB+6      MOVE ONE BYTE PWOS ZONE                      
         CLI   NWSEDITN,C'O'       TEST EDITION = 'OUTDOOR'                     
         BE    *+10                YES - IGNORE IT                              
         MVC   PBUYKEDT,NWSEDITN   MOVE EDITION CODE                            
* NEED TO CHANGE PUB IN OGILVY CONVERSION ELEMENT                               
         LA    R6,PBDELEM                                                       
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'99'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   10(6,R6),PBUYKPUB    MOVE PUB WITH ZONE/EDITION                  
* NOW CHECK PUB TABLE                                                           
         XC    DUB,DUB                                                          
         MVC   DUB(1),PBUYKMED                                                  
         MVC   DUB+1(6),PBUYKPUB                                                
         GOTO1 BINSRCH,PUBCPARS,DUB  CONVERT PUB NUMBER                         
         CLI   0(R1),X'01'         TEST NOT FOUND                               
         BE    OM202               IF NOT FOUND, KEEP OLD PUB CODE              
         L     RE,0(R1)            GET ENTRY ADDRESS                            
         MVC   PBUYKPUB(6),7(RE)   MOVE NEW PUB TO RECORD                       
         MVC   DUB+1(6),PBUYKPUB   AND MOVE TO SEARCH ARG                       
         SPACE 1                                                                
* NOW MAKE SURE ITS ON THE FILE                                                 
         SPACE 1                                                                
OM202    CLI   QOPT2,C'X'          TEST TABLE ACTIVE                            
         BE    OM204                                                            
         GOTO1 BINSRCH,PUBTPARS,DUB                                             
         CLI   0(R1),1             TEST NOT FOUND                               
         BNE   OM204                                                            
         BAS   RE,PUBERR                                                        
*                                                                               
OM204    BAS   RE,OMVAL            GET GROSS/AGYCOM/CSHDSC/TAX                  
*                                                                               
         ZAP   PBDCOS,NWSGRSRT                                                  
         MVI   PBDCOSIN,C' '       INDICATE GROSS RATE                          
         MVI   PBDCOSTY,C'T'       INDICATE TOTAL RATE GIVEN                    
*                                                                               
         CLI   PBUYKDAT+2,0        TEST INSERTION DAY = 00                      
         BNE   *+12                                                             
         MVI   PBUYKDAT+2,1                                                     
         MVI   PBDFREQ,C'M'                                                     
*                                                                               
         MVC   PBDBDATE,PBUYKDAT   BILLABLE DATE = INS DATE                     
*                                                                               
         MVC   PBDPDATE,PBDBDATE   PAYABLE DATE = BILLABLE + 1 MONTH            
         ZIC   R1,PBDPDATE+1       GET INSERTION MONTH                          
         LA    R1,1(R1)                                                         
         STC   R1,PBDPDATE+1                                                    
         CLI   PBDPDATE+1,13                                                    
         BL    OM206                                                            
         MVI   PBDPDATE+1,1                                                     
         IC    RE,PBDPDATE         FORCE TO NEXT YEAR                           
         LA    RE,1(RE)                                                         
         STC   RE,PBDPDATE                                                      
*                                                                               
OM206    OC    NWSEST90,NWSEST90                                                
         BZ    *+12                                                             
         BAS   RE,EST90ERR                                                      
         B     OM20                                                             
*                                                                               
         CLI   NWSGPBUY,0          TEST GROUP MEMBER IND                        
         BE    OM210                                                            
         CLI   NWSSBLI,11          BUT ONLY FOR BIG SUBLINES                    
         BL    OM210                                                            
         BAS   RE,GRPPUB                                                        
         B     OM20                                                             
*                                                                               
OM210    DS    0H                                                               
         CLC   NWSDISC,=X'FFFF'    TEST FOR BAD DISCOUNT VALUE                  
         BNE   *+10                                                             
         XC    NWSDISC,NWSDISC     AND FIX IF NEEDED                            
         SPACE 1                                                                
         OC    NWSPDDT,NWSPDDT     TEST PAID                                    
         BZ    *+8                                                              
         BAS   RE,OMPAYEL                                                       
*                                                                               
         OC    NWSBLLDT,NWSBLLDT   TEST BILLED                                  
         BZ    *+8                                                              
         BAS   RE,OMBILEL                                                       
*                                                                               
         GOTO1 DATCON,DMCB,NWSACTDT,(3,PBDDATE)  ACTIVITY DATE                  
*                                                                               
         TM    NWSDELTE,X'80'      TEST DELETED                                 
         BZ    *+8                                                              
         OI    PBUYCNTL,X'80'      SET DELETED IND                              
*                                                                               
         ZAP   DUB,=P'0'           GET CASH DISCOUNT AMOUNT                     
         MVO   DUB,NWSDISC                                                      
         DP    DUB,=P'10'          SCALE FROM 2 DECIMALS TO 1                   
         ZAP   PBDCD,DUB(6)                                                     
*                                                                               
         OC    NWSTAX,NWSTAX                                                    
         BZ    OM211                                                            
         ZAP   DUB,NWSTAX                                                       
         MP    DUB,=P'100'        SCALE FROM 1 DECIMAL TO 4                     
         CVB   R0,DUB                                                           
         STCM  R0,7,PBDTAX                                                      
*                                                                               
OM211    CLI   PBUYKMED,C'O'       TEST MEDIA OUTDOOR                           
         BNE   OM215                                                            
         MVI   PBDSPACE,X'FF'                                                   
         ZAP   PBDSHOW,=P'0'                                                    
         ZAP   PBDREG,=P'0'                                                     
         ZAP   PBDILLUM,=P'0'                                                   
* CREATE A COMMENT ELEMENT FOR ORIGINAL SPACE DESCRIPTION                       
         LA    R6,PBDELEM                                                       
OM213    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   OM213                                                            
*                                                                               
         XC    0(51,R6),0(R6)      CLEAR ELEM LEN + 2                           
         MVI   0(R6),X'66'                                                      
         MVI   1(R6),49                                                         
         MVC   2(47,R6),NWSSPDSC                                                
*                                                                               
         SR    RE,RE               SET NEW RECORD LENGTH                        
         ICM   RE,3,PBUYLEN                                                     
         LA    RE,49(RE)                                                        
         STCM  RE,3,PBUYLEN                                                     
         B     OM223                                                            
*                                                                               
OM215    CLC   =X'FFFFFF',NWSCOLNO  TEST FREE FORM SPACE DESC                   
         BNE   OM217                                                            
         MVC   PBDSPACE,NWSSPDSC    MOVE TO DDS REC                             
         B     OM223                                                            
*                                                                               
OM217    TM    NWSSPTYP,X'08'      TEST NEW INCHES                              
         BZ    OM219                                                            
         MVI   PBDUIND,X'89'       SET SPACE IND = INCHES (2 DEC)               
         ZAP   DUB,=P'0'                                                        
         MVO   DUB,NWSCOLNO                                                     
         ZAP   PBDCLMS,DUB                                                      
*                                                                               
         ZAP   DUB,=P'0'                                                        
         MVO   DUB,NWSCOLIN        INCHES X 1000                                
         CVB   R0,DUB                                                           
         SRDL  R0,32                                                            
         D     R0,=F'10'                                                        
         CVD   R1,DUB                                                           
         ZAP   PBDUNITS,DUB                                                     
         STH   R0,HALF             SAVE REMAINDER                               
* NEED TO SET SPACE DESCRIPTION                                                 
         LA    R4,PBDSPACE                                                      
         EDIT  (P2,PBDCLMS),(2,(R4)),0,ALIGN=LEFT                               
         AR    R4,R0                                                            
         MVI   0(R4),C'X'                                                       
         LA    R4,1(R4)                                                         
         EDIT  (P3,PBDUNITS),(6,(R4)),2,ALIGN=LEFT                              
*                                                                               
         ZAP   DUB,PBDUNITS                                                     
         MP    DUB,PBDCLMS                                                      
         ZAP   PBDUNITS,DUB        *** SET TOTAL SPACE                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,HALF           TEST FOR REMAINDER                           
         BZ    *+8                                                              
         BAS   RE,INCHERR                                                       
         B     OM223                                                            
*                                                                               
OM219    CLI   NWSSPTYP,0          TEST LINES                                   
         BNE   OM221                                                            
         ZAP   DUB,=P'0'                                                        
         MVO   DUB,NWSCOLNO                                                     
         ZAP   PBDCLMS,DUB                                                      
         MVI   PBDUIND,C'L'        SET 'LINES' IND                              
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NWSLINES       GET NUMBER OF LINES                          
         CVD   R0,DUB                                                           
         MP    DUB,PBDCLMS         GET LINES X COLUMNS                          
         ZAP   PBDUNITS,DUB        SET TOTAL UNITS                              
* NEED TO SET SPACE DESCRIPTION                                                 
         LA    R4,PBDSPACE                                                      
         EDIT  (P2,PBDCLMS),(2,(R4)),0,ALIGN=LEFT                               
         AR    R4,R0                                                            
         MVI   0(R4),C'X'                                                       
         LA    R4,1(R4)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,NWSLINES                                                    
         EDIT  (R0),(6,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
         MVI   0(R4),C'L'                                                       
         B     OM223                                                            
*                                                                               
OM221    BAS   RE,INTYPERR         MISSING UNIT TYPE CODE                       
         B     OM20                                                             
*                                                                               
OM223    CLI   QOPT1,C'Y'          TEST TRACE ON                                
         BNE   OM225                                                            
         BAS   RE,OMTRACE          PRINT OM REC TRACE                           
         BAS   RE,DDSTRACE         PRINT DDS RECORD TRACE                       
*                                                                               
OM225    MVC   SVOMKEY,OMREC                                                    
*                                                                               
         BAS   RE,GETFILE                                                       
*                                                                               
         CLC   SVOMKEY(18),OMREC   TEST SAME KEY                                
         BE    OM230               YES - ADD MORE ELEMENTS                      
*                                                                               
         LA    R1,PBUYREC                                                       
         BAS   RE,PUTSORT          NO - PUT DDS RECORD                          
         AP    NWSCOUNT,=P'1'                                                   
         B     OM22                AND CREATE NEW                               
*                                                                               
OM230    CLI   MAGRECID,C'4'       TEST DETAIL RECORD                           
         BNE   OM240                                                            
*                                                                               
         BAS   RE,OMVAL            GET GROSS/NET/ETC                            
         AP    PBDCOS,MAGGRSRT     UPDATE GROSS RATE                            
         B     OM210               AND ADD BILL/PAY DATA AS NEEDED              
         SPACE 1                                                                
*                                                                               
OM240    CLI   MAGRECID,C'9'       TEST COMMENT                                 
         BE    *+12                                                             
         BAS   RE,RECIDERR                                                      
         B     OM20                                                             
*                                                                               
         BAS   RE,OMCOMEL                                                       
         B     OM22                RECORD WAS PUT BY OMCOMEL                    
         EJECT                                                                  
* PROCESS OM ESTIMATE HEADER RECORD *                                           
         SPACE 1                                                                
OM300    DS    0H                                                               
         XC    PESTREC(249),PESTREC                                             
         MVC   PESTKAGY(2),0(R4)   MOVE AGENCY                                  
         ZIC   RE,OMREC                                                         
         N     RE,=X'0000000F'     DROP ALL BUT MEDIA                           
         CH    RE,=H'2'                                                         
         BNH   OM302                                                            
         BAS   RE,MEDERR                                                        
         B     OM20                                                             
*                                                                               
OM302    BCTR  RE,0                                                             
         LA    RE,MEDTAB(RE)                                                    
         MVC   PESTKMED,0(RE)                                                   
*                                                                               
         MVC   PESTKCLT(2),ESTCLI                                               
         MVC   PESTKCLT+2(1),2(R4)   MOVE THIRD CHAR OF CLT CODE                
         CLI   2(R4),C'O'            IF = C'O', FORCE MEDIA                     
         BNE   OM304                                                            
         MVI   PESTKMED,C'O'       TO OUTDOOR                                   
         TM    MAGAGMED,X'01'      TEST MAGAZINES                               
         BZ    OM304                                                            
         MVI   PESTKCLT+2,C'M'     YES - CHANGE CLIENT CODE                     
*                                                                               
OM304    MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKPRD,ESTPR                                                   
         CLC   PESTKPRD,=C'ZZZ'    IGNORE PRODUCT ZZZ                           
         BE    OM320                                                            
         OI    PESTKPRD+2,X'40'                                                 
         MVI   PESTLEN+1,249                                                    
*                                                                               
         MVI   PESTELEM,7                                                       
         MVI   PESTELEM+1,216                                                   
         MVI   PESTPROF,C'0'                                                    
         MVC   PESTPROF+1(31),PESTPROF                                          
*                                                                               
         OC    ESTNODD,ESTNODD                                                  
         BZ    OM310                                                            
         CLC   ESTENDO,=C'870100'                                               
         BL    OM310                                                            
* MAKE SURE PRODUCT IS ON FILE                                                  
         XC    DUB,DUB                                                          
         MVC   DUB(1),PESTKMED     MOVE MED                                     
         MVC   DUB+1(6),PESTKCLT   MOVE CLT/PRD                                 
         GOTO1 BINSRCH,PRDTPARS,DUB                                             
         CLI   0(R1),X'01'         TEST NOT FOUND                               
         BNE   *+8                                                              
         BAS   RE,PRDERR                                                        
*                                                                               
         MVC   PESTKEST,ESTNODD                                                 
         MVC   PESTST(12),ESTSTRTO                                              
         CLC   PESTST+4(2),=C'00'                                               
         BNE   *+8                                                              
         MVI   PESTST+5,C'1'                                                    
         CLC   PESTEND+4(2),=C'00'                                              
         BNE   *+8                                                              
         MVI   PESTEND+5,C'1'                                                   
         LA    R4,PCONREC                                                       
         GOTO1 =V(CHOPPER),DMCB,(40,ESTDESCO),(20,PCONREC+1000),5               
         MVC   PESTNAME(20),PCONREC+1000                                        
         MVC   PESTNAM2(20),PCONREC+1020                                        
*                                                                               
         LA    R1,PESTREC                                                       
         BAS   RE,PUTSORT                                                       
         AP    ESTCOUNT,=P'1'                                                   
         BAS   RE,ESTTRACE                                                      
*                                                                               
OM310    OC    ESTNOEVN,ESTNOEVN                                                
         BZ    OM320                                                            
         CLC   ESTENDE,=C'870100'                                               
         BL    OM320                                                            
* MAKE SURE PRODUCT IS ON FILE                                                  
         XC    DUB,DUB                                                          
         MVC   DUB(1),PESTKMED     MOVE MED                                     
         MVC   DUB+1(6),PESTKCLT   MOVE CLT/PRD                                 
         GOTO1 BINSRCH,PRDTPARS,DUB                                             
         CLI   0(R1),X'01'         TEST NOT FOUND                               
         BNE   *+8                                                              
         BAS   RE,PRDERR                                                        
*                                                                               
         MVC   PESTKEST,ESTNOEVN                                                
         MVC   PESTST(12),ESTSTRTE                                              
         CLC   PESTST+4(2),=C'00'                                               
         BNE   *+8                                                              
         MVI   PESTST+5,C'1'                                                    
         CLC   PESTEND+4(2),=C'00'                                              
         BNE   *+8                                                              
         MVI   PESTEND+5,C'1'                                                   
         LA    R4,PCONREC                                                       
         GOTO1 =V(CHOPPER),DMCB,(40,ESTDESCE),(20,PCONREC+1000),5               
         MVC   PESTNAME(20),PCONREC+1000                                        
         MVC   PESTNAM2(20),PCONREC+1020                                        
*                                                                               
         LA    R1,PESTREC                                                       
         BAS   RE,PUTSORT                                                       
         AP    ESTCOUNT,=P'1'                                                   
         BAS   RE,ESTTRACE                                                      
*                                                                               
OM320    B     OM20                                                             
         EJECT                                                                  
ESTTRACE NTR1                                                                   
         CLI   QOPT1,C'E'                                                       
         BNE   EXIT                                                             
         MVC   P(3),PESTKEY                                                     
         MVC   P+4(6),PESTKCLT                                                  
         SR    R0,R0                                                            
         ICM   R0,3,PESTKEST                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+11(4),DUB                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,PESTKEY,P+20,28,=C'TOG'                              
         MVC   P+76(32),PESTNAME                                                
         MVC   P+109(20),PESTNAM2                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* RETRIEVE SORT OUTPUT AND WRITE OUTPUT FILE *                                  
         SPACE 1                                                                
OM400    DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
OM402    DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R4,15,4(R1)         R4 POINTS TO 4 BYTE RECLEN                   
         BZ    OM410                                                            
         CLI   7(R4),X'20'         TEST BUYREC                                  
         BNE   OM404                                                            
         CLC   KEY(24),4(R4)       TEST SAME UP TO LINE                         
         BE    *+6                                                              
         SR    R5,R5                                                            
         LA    R5,1(R5)            BUMP LINE NUMBER                             
         STC   R5,28(R4)           SET LINE NUMBER IN RECORD                    
         MVC   KEY(25),4(R4)       SAVE THIS KEY                                
OM404    DS    0H                                                               
         LR    R0,R4                                                            
         PUT   FILEOUT,(R0)                                                     
         B     OM402                                                            
*                                                                               
OM410    CLOSE FILEOUT                                                          
*                                                                               
         LA    R4,COUNTS                                                        
         LA    R5,NCOUNTS                                                       
*                                                                               
OM420    OI    3(R4),X'0F'                                                      
         UNPK  P(7),0(4,R4)                                                     
         MVC   P+9(20),4(R4)                                                    
         GOTO1 REPORT                                                           
         LA    R4,L'COUNTS(R4)                                                  
         BCT   R5,OM420                                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************                                 
* CREATE PAID PAY ELEMENT FROM OM DATA RECORD *                                 
***********************************************                                 
         SPACE 1                                                                
OMPAYEL  NTR1                                                                   
*                                                                               
         CLC   =C'CAN',MAGPDDT                                                  
         BE    EXIT                                                             
*                                                                               
         LA    R6,PBDELEM                                                       
OMPAY2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST EOF                                     
         BNE   OMPAY2                                                           
*                                                                               
         USING PPAYELEM,R6                                                      
*                                                                               
         XC    0(24,R6),0(R6)      CLEAR ELEM LEN + 2                           
         MVI   0(R6),X'25'                                                      
         MVI   1(R6),22                                                         
         MVC   PPDDATE,MAGPDDT     SET CLEARANCE DATE                           
         LA    R4,PPDDATE                                                       
         BAS   RE,CONVDATE         GET BINARY DATE                              
         MVC   PPGROSS,GROSS                                                    
         MVC   PPAGYCOM,AGYCOM                                                  
         MVC   PPCSHDSC,CSHDSC                                                  
*                                                                               
OMPAYX   SR    RE,RE               SET NEW RECORD LENGTH                        
         ICM   RE,3,PBUYLEN                                                     
         LA    RE,22(RE)                                                        
         STCM  RE,3,PBUYLEN                                                     
         B     EXIT                                                             
         EJECT                                                                  
*******************************************                                     
* CREATE BILL ELEMENT FROM OM DATA RECORD *                                     
*******************************************                                     
         SPACE 1                                                                
OMBILEL  NTR1                                                                   
*                                                                               
         LA    R6,PBDELEM                                                       
BILEL2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST EOR                                     
         BNE   BILEL2                                                           
*                                                                               
         USING PBILELEM,R6                                                      
*                                                                               
         XC    0(25,R6),0(R6)      CLEAR ELEM LEN + 2                           
         MVI   0(R6),X'26'                                                      
         MVI   1(R6),23                                                         
         MVC   PBPRD,PBUYKPRD      SET PRODUCT CODE                             
         MVC   PBLDATE,MAGBLLDT    SET BILLED DATE                              
         LA    R4,PBLDATE                                                       
         BAS   RE,CONVDATE         GET BINARY DATE                              
*                                                                               
* NOTE - LEAVE PBINVO AT X'0000'                                                
*                                                                               
         MVC   PBGROSS,GROSS       SET GROSS BILLED                             
         MVC   PBAGYCOM,AGYCOM                                                  
         MVC   PBCSHDSC,CSHDSC                                                  
*                                                                               
OMBILX   SR    RE,RE               SET NEW RECORD LENGTH                        
         ICM   RE,3,PBUYLEN                                                     
         LA    RE,23(RE)                                                        
         STCM  RE,3,PBUYLEN                                                     
         B     EXIT                                                             
         EJECT                                                                  
*******************************************                                     
* CREATE COMMENT ELEMENT FROM DATA RECORD *                                     
* ROUTINE WILL PUT RECORD AT END          *                                     
*******************************************                                     
         SPACE 1                                                                
OMCOMEL  NTR1                                                                   
         LA    R4,PCONREC          USE PCONREC AS SAVE AREA                     
         LA    R0,4                                                             
         MVC   0(80,R4),SPACES                                                  
         LA    R4,80(R4)                                                        
         BCT   R0,*-10                                                          
         MVI   0(R4),C' '          ADD ONE MORE                                 
*                                                                               
         LA    R4,PCONREC                                                       
*                                                                               
COMEL2   CLI   MPCDELTE,X'FF'                                                   
         BE    COMEL4                                                           
         CLI   MPCNO,3             SKIP PERMANENT PRINTING COMMENTS             
         BH    COMEL4                                                           
         MVC   0(77,R4),MPCBODY1                                                
         CLI   MPCLINNO,0                                                       
         BE    *+10                                                             
         MVC   0(80,R4),MPCBODYX                                                
         OC    0(80,R4),SPACES                                                  
         LA    R4,80(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,2(R4)            SET FOR NEXT MOVE                            
*                                                                               
COMEL4   MVC   SVOMKEY,OMREC                                                    
*                                                                               
         BAS   RE,GETFILE                                                       
*                                                                               
         CLC   SVOMKEY(19),OMREC   TEST SAME THRU REC ID                        
         BE    COMEL2                                                           
*                                                                               
         LA    R5,PCONREC                                                       
         SR    R5,R4               GET TOTAL LENGTH OF COMMENTS                 
         BZ    COMELX              PUT RECORD NOW IF NO DATA SAVED              
         LPR   R5,R5                                                            
*                                                                               
         GOTO1 =V(SQUASHER),DMCB,PCONREC,(R5)                                   
*                                                                               
         L     R5,4(R1)            GET NEW LENGTH                               
         CH    R5,=H'235'          IF MORE THAN 235 WILL LOSE DATA              
         BNH   *+12                                                             
         BAS   RE,COMERR                                                        
         LA    R5,235              LIMIT TO FIRST 235 CHARS                     
*                                                                               
         GOTO1 =V(CHOPPER),DMCB,((R5),PCONREC),(47,PCONREC+1000),5              
*                                                                               
         LA    R4,PCONREC+1000     POINT TO FIRST CHUNK                         
         ICM   R5,15,DMCB+8        GET NUMBER OF LINES                          
         BZ    COMELX                                                           
*                                                                               
         LA    R6,PBDELEM                                                       
COMEL10  ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST EOR                                     
         BNE   COMEL10                                                          
*                                                                               
         XC    0(51,R6),0(R6)      CLEAR ELEM LEN + 2                           
         MVI   0(R6),X'66'                                                      
         MVI   1(R6),49                                                         
         MVC   2(47,R6),0(R4)                                                   
*                                                                               
         SR    RE,RE               SET NEW RECORD LENGTH                        
         ICM   RE,3,PBUYLEN                                                     
         LA    RE,49(RE)                                                        
         STCM  RE,3,PBUYLEN                                                     
*                                                                               
         LA    R4,47(R4)           NEXT CHUNK                                   
         BCT   R5,COMEL10                                                       
*                                                                               
COMELX   CLI   QOPT1,C'Y'                                                       
         BNE   *+12                                                             
         BAS   RE,OMTRACE                                                       
         BAS   RE,DDSTRACE                                                      
*                                                                               
         LA    R1,PBUYREC                                                       
         BAS   RE,PUTSORT          PUT RECORD TO SORT                           
         LA    RE,MAGCOUNT                                                      
         CLI   PBUYKMED,C'M'                                                    
         BE    *+8                                                              
         LA    RE,NWSCOUNT                                                      
         AP    0(4,RE),=P'1'                                                    
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************                      
* GET GROSS, NET, AGYCOMM, AND TAX FROM OM DETAIL RECORD *                      
**********************************************************                      
         SPACE 1                                                                
OMVAL    NTR1                                                                   
         XC    GROSS(48),GROSS                                                  
         SPACE 1                                                                
*===============================================================*               
         CLC    MAGDISC,=X'FFFF'    TEST FOR BAD DISCOUNT VALUE                 
         BNE    *+10                                                            
         XC     MAGDISC,MAGDISC     TRY NOT TO BLOW UP                          
*===============================================================*               
         SPACE 1                                                                
         ZAP   DUB,MAGGRSRT                                                     
         CVB   R0,DUB                                                           
         STCM  R0,15,GROSS                                                      
         SPACE 1                                                                
* FIND COMMISSION RATE IN TABLE                                                 
         SPACE 1                                                                
         LA    R1,COMMTAB                                                       
         LA    R0,(COMMTABX-COMMTAB)/L'COMMTAB                                  
OMVAL2   CLC   MAGCOMM,0(R1)                                                    
         BE    OMVAL4                                                           
         LA    R1,L'COMMTAB(R1)                                                 
         BCT   R0,OMVAL2                                                        
*                                                                               
OMVAL4   ICM   RE,15,1(R1)                                                      
         CVD   RE,DUB                                                           
         ZAP   PBDACP,DUB          SET COMMISSION RATE IN DDS REC               
*                                                                               
         L     R0,=F'100000'                                                    
         SR    R0,RE               GET COMPLEMENT OF COMM (=NET)                
         L     R1,GROSS                                                         
         AR    R1,R1               X 2                                          
         MR    R0,R0                                                            
         D     R0,=F'100000'                                                    
         LTR   R1,R1                                                            
         BM    *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                ROUND TO GET NET                             
         ST    R1,NET                                                           
*                                                                               
         L     R1,GROSS                                                         
         S     R1,NET              GROSS - NET GIVES AGYCOMM                    
         ST    R1,AGYCOM                                                        
*                                                                               
         OC    MAGDISC,MAGDISC                                                  
         BZ    OMVAL8                                                           
         CLI   MAGPWNOD,X'FF'      TEST PAID W/O CD                             
         BE    OMVAL8                                                           
         L     R1,NET                                                           
         ZAP   DUB,=P'0'                                                        
         MVO   DUB,MAGDISC                                                      
         CVB   R0,DUB              GIVES CD TO 2 (2 PCT = 0200)                 
         AR    R0,R0               X 2                                          
         MR    R0,R0               CD X NET                                     
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BM    *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         ST    R1,CSHDSC                                                        
*                                                                               
OMVAL8   OC    MAGTAX,MAGTAX                                                    
         BZ    OMVALX                                                           
         CP    MAGTAX,=P'0'                                                     
         BZ    OMVALX                                                           
         L     R1,GROSS                                                         
         S     R1,AGYCOM           GIVES NET                                    
         ZAP   DUB,MAGTAX                                                       
         CVB   R0,DUB                                                           
         AR    R0,R0               X 2                                          
         MR    R0,R0                                                            
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BM    *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                GIVES TAX AMOUNT IN R1                       
         ST    R1,TAX                                                           
*                                                                               
         L     R0,GROSS                                                         
         AR    R0,R1                                                            
         ST    R0,GROSS                                                         
OMVALX   B     EXIT                                                             
         SPACE 2                                                                
COMMTAB  DS    0CL5                                                             
         DC    C'Y',AL4(15000)                                                  
         DC    C'A',AL4(14167)                                                  
         DC    C'B',AL4(14250)                                                  
         DC    C'C',AL4(16670)                                                  
         DC    C'D',AL4(19250)                                                  
         DC    C'E',AL4(07500)                                                  
         DC    C'F',AL4(07200)                                                  
         DC    C'G',AL4(13000)                                                  
         DC    C'H',AL4(20000)                                                  
         DC    C'I',AL4(13667)                                                  
         DC    C'J',AL4(16667)                                                  
         DC    C'K',AL4(25000)                                                  
         DC    C'L',AL4(17000)                                                  
         DC    C'M',AL4(12667)                                                  
         DC    C'N',AL4(00000)                                                  
         DC    C'O',AL4(10000)                                                  
         DC    C'P',AL4(30000)                                                  
         DC    C'Q',AL4(08700)                                                  
         DC    C'R',AL4(12000)                                                  
         DC    C'S',AL4(12500)                                                  
         DC    C'T',AL4(01000)                                                  
         DC    C'U',AL4(15869)                                                  
         DC    C'V',AL4(16000)                                                  
COMMTABX EQU   *                                                                
         EJECT                                                                  
OMTRACE  NTR1                                                                   
         GOTO1 HEXOUT,DMCB,OMREC,P,21,=C'TOG'                                   
         GOTO1 (RF),(R1),OMREC+21,P+44,34                                       
         GOTO1 (RF),(R1),OMREC+60,P+114,5                                       
         GOTO1 REPORT                                                           
         TM    OMREC,X'01'         TEST MAGAZINE                                
         BZ    OMTR2                                                            
         MVC   P+44(25),MAGSPDSC                                                
         MVC   P+70(6),MAGACTDT                                                 
         GOTO1 HEXOUT,DMCB,OMREC+131,P+77,15                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
* NEWSPAPERS                                                                    
OMTR2    GOTO1 HEXOUT,DMCB,OMREC+65,P+44,9                                      
         MVC   P+63(15),NWSSPDSC                                                
         MVC   P+79(6),NWSACTDT                                                 
         GOTO1 HEXOUT,DMCB,OMREC+131,P+86,15                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
DDSTRACE NTR1                                                                   
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         GOTO1 HEXOUT,DMCB,PBUYKEY,P,25,=C'TOG'                                 
         GOTO1 (RF),(R1),PBUYLEN,P+51,3                                         
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,PBDELEM,P+5,55                                       
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBDELEM+55,P+7,50                                    
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,PBDELEM                                                       
*                                                                               
DDSTR2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXIT                                                             
         ZIC   R0,1(R6)            GET CURRENT ELEMENT LENGTH                   
         CLI   0(R6),X'66'         TEST COMMENT ELEM                            
         BNE   *+8                                                              
         LA    R0,2                                                             
         GOTO1 HEXOUT,DMCB,(R6),P+5,(R0),=C'TOG'                                
*                                                                               
         CLI   0(R6),X'66'         TEST COMMENT ELEM                            
         BNE   DDSTR4                                                           
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,DDSTRMVC                                                      
DDSTR4   DS    0H                                                               
         GOTO1 REPORT                                                           
         B     DDSTR2                                                           
*                                                                               
DDSTRMVC MVC   P+10(0),2(R6) *EXECUTED*                                         
         SPACE 2                                                                
* ON ENTRY R1 POINTS TO RECORD *                                                
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R0,3,(PBUYLEN-PBUYREC)(R1)                                       
         AH    R0,=H'4'                                                         
         SLL   R0,16                                                            
         SH    R1,=H'4'            BACK UP TO RECLEN                            
         ST    R0,0(R1)            STORE IT IN FRONT OF RECORD                  
         LR    R0,R1               POINT R0 TO RECORD                           
         GOTO1 SORTER,DMCB,=C'PUT',(R0)                                         
         AP    OUTCOUNT,=P'1'                                                   
         B     EXIT                                                             
         SPACE 1                                                                
GETFILE  NTR1                                                                   
*                                                                               
GETFILE2 GET   FILEIN,OMREC                                                     
*                                                                               
         CLI   MAGRECID,C'4'       TEST BUY DETAIL                              
         BE    GETFILE4                                                         
         CLI   MAGRECID,C'9'       TEST BUY COMMENT                             
         BE    GETFILE4                                                         
         B     GETFILEX                                                         
*                                                                               
GETFILE4 CLI   MAGSBLI,10          TEST GROUP SUBLINE CODE                      
         BH    GETFILE2            YES - SKIP RECORD                            
*                                                                               
GETFILEX B     EXIT                                                             
*                                                                               
ENDIN    CLOSE FILEIN                                                           
         MVI   OMREC,X'FF'                                                      
         MVC   OMREC+1(149),OMREC                                               
         MVI   EOFSW,C'Y'                                                       
         B     EXIT                                                             
         EJECT                                                                  
**************************************                                          
* CONVERT PWOS DATE TO 3 BYTE BINARY *                                          
* R4 POINTS TO 3 BYTE FIELD -        *                                          
**************************************                                          
         SPACE 1                                                                
CONVDATE NTR1                                                                   
         OC    0(3,R4),0(R4)                                                    
         BZ    EXIT                                                             
         GOTO1 DATCON,DMCB,(1,(R4)),(3,DUB)                                     
         MVC   0(3,R4),DUB                                                      
         B     EXIT                                                             
         EJECT                                                                  
SRERR    L     R1,=A(MSG1)                                                      
         AP    SRERRS,=P'1'                                                     
         B     BUYERR                                                           
MEDERR   L     R1,=A(MSG2)                                                      
         B     BUYERR                                                           
EST90ERR L     R1,=A(MSG3)                                                      
         AP    E90ERRS,=P'1'                                                    
         B     BUYERR                                                           
RECIDERR L     R1,=A(MSG4)                                                      
         B     BUYERR                                                           
SPDSCERR L     R1,=A(MSG5)                                                      
         AP    SPCERRS,=P'1'                                                    
         BR    RE                  ******** NOP SPACERR PRINT                   
         B     BUYERR                                                           
PRDERR   CLC   DUB(7),SRCHPRD      TEST SAME AS LAST TIME                       
         BER   RE                                                               
         AP    PRDERRS,=P'1'                                                    
         MVC   SRCHPRD,DUB                                                      
         L     R1,=A(MSG11)                                                     
         B     BUYERR                                                           
PUBERR   DS    0H                                                               
         AP    PUBERRS,=P'1'                                                    
         CLC   SRCHPUB(6),DUB+1    TEST SAME AS LAST TIME                       
         BER   RE                                                               
         L     R1,=A(MSG7)                                                      
         MVI   ERRFLAG,C'P'        SET PUB ERROR FLAG                           
         MVC   SRCHPUB(6),DUB+1                                                 
         B     BUYERR                                                           
INTYPERR L     R1,=A(MSG8)                                                      
         AP    INTERRS,=P'1'                                                    
         B     BUYERR                                                           
INCHERR  L     R1,=A(MSG9)                                                      
         AP    INCERRS,=P'1'                                                    
         B     BUYERR                                                           
GRPPUB   L     R1,=A(MSG10)                                                     
         AP    GRPERRS,=P'1'                                                    
         B     BUYERR                                                           
*                                                                               
BUYERR   NTR1                                                                   
         MVC   P(11),=C'** ERROR **'                                            
         MVC   P+12(40),0(R1)                                                   
         MVC   P+53(4),=C'KEY='                                                 
         LA    R5,OMREC                                                         
         BAS   RE,FMTKEY                                                        
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,OMREC+21,P+44,34                                     
         GOTO1 (RF),(R1),OMREC+60,P+114,5                                       
         GOTO1 REPORT                                                           
         MVC   P+44(25),MAGSPDSC                                                
         TM    OMREC,X'01'         TEST MAGAZINES                               
         BO    BUYERR2                                                          
         MVC   P+44(25),SPACES                                                  
         GOTO1 HEXOUT,DMCB,OMREC+65,P+44,9                                      
BUYERR2  MVC   P+70(6),MAGACTDT                                                 
         GOTO1 HEXOUT,DMCB,OMREC+131,P+77,15                                    
         GOTO1 REPORT                                                           
         MVI   ERRFLAG,0           CLEAR FLAG                                   
         B     EXIT                                                             
*                                                                               
COMERR   NTR1                                                                   
         L     R1,=A(MSG6)                                                      
         MVC   P(11),=C'** ERROR **'                                            
         MVC   P+12(40),0(R1)                                                   
         MVC   P+53(4),=C'KEY='                                                 
         LA    R5,SVOMKEY                                                       
         BAS   RE,FMTKEY                                                        
         GOTO1 REPORT                                                           
*                                                                               
         LA    R5,PCONREC                                                       
         LA    R0,3                                                             
COMERR2  MVC   P+25(107),0(R5)                                                  
         CLC   P,SPACES                                                         
         BE    EXIT                                                             
         GOTO1 REPORT                                                           
         LA    R5,110(R5)                                                       
         BCT   R0,COMERR2                                                       
         B     EXIT                                                             
         SPACE 1                                                                
FMTKEY   NTR1                                                                   
         LA    R4,P+58                                                          
         GOTO1 HEXOUT,DMCB,(R5),(R4),1      AGY/MEDIA                           
         MVC   3(2,R4),1(R5)                CLIENT                              
         MVC   6(3,R4),7(R5)                PRODUCT                             
         SR    R0,R0                                                            
         ICM   R0,3,10(R5)                  ESTIMATE                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  10(4,R4),DUB                                                     
         MVI   14(R4),C'-'                                                      
         ZIC   R0,13(R5)                    LINE                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  15(2,R4),DUB                                                     
         ICM   R0,1,19(R5)         TEST SUBLINE PRESENT                         
         BZ    FMTKEY4                                                          
         MVI   17(R4),C'/'                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  18(2,R4),DUB                                                     
*                                                                               
FMTKEY4  GOTO1 HEXOUT,DMCB,14(R5),21(R4),6  OM PUB                              
*                                                                               
         CLI   ERRFLAG,C'P'                                                     
         BNE   EXIT                                                             
         GOTO1 HEXOUT,DMCB,SRCHPUB,34(R4),6   SEARCH PUB                        
         B     EXIT                                                             
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=ENDIN    X        
               BLKSIZE=2250,LRECL=150                                           
         SPACE 1                                                                
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=32760,LRECL=4004                                         
*                                                                               
NET      DS    F                                                                
         DS    0D                                                               
COUNTS   DS    0CL24                                                            
ZZZCOUNT DC    PL4'0',CL20'PURGED ZZZ RECORDS'                                  
AAACOUNT DC    PL4'0',CL20'PRODUCT=AAA RECORDS'                                 
CPYCOUNT DC    PL4'0',CL20'A/C/P COPIES'                                        
MAGCOUNT DC    PL4'0',CL20'MAGAZINE RECS OUT'                                   
NWSCOUNT DC    PL4'0',CL20'NWSPAPER RECS OUT'                                   
ESTCOUNT DC    PL4'0',CL20'ESTIMATE HDRS OUT'                                   
OUTCOUNT DC    PL4'0',CL20'TOTAL RECORDS OUT'                                   
PUBERRS  DC    PL4'0',CL20'MISSING PUBS'                                        
SPCERRS  DC    PL4'0',CL20'SPACE DESC ERRORS'                                   
GRPERRS  DC    PL4'0',CL20'GROUP ID ERRORS'                                     
SRERRS   DC    PL4'0',CL20'SHORT RATE ERRORS'                                   
E90ERRS  DC    PL4'0',CL20'EST 90 ERRORS'                                       
INCERRS  DC    PL4'0',CL20'INCH SPEC ERRORS'                                    
INTERRS  DC    PL4'0',CL20'INPUT TYPE ERRORS'                                   
PRDERRS  DC    PL4'0',CL20'MISSING PRODUCTS'                                    
NCOUNTS  EQU   ((*-COUNTS)/24)                                                  
SVOMKEY  DS    CL21                                                             
ERRFLAG  DC    X'00'                                                            
EOFSW    DC    X'00'                                                            
SRCHPUB  DS    XL6                                                              
SRCHPRD  DS    XL7                                                              
         DS    0D                                                               
         DC    CL8'*OMREC*'                                                     
OMREC    DS    CL150                                                            
*                                                                               
PRDTPARS DC    A(0)                PARAMS TO SEARCH EST TABLE                   
         DC    A(PRDTAB)                                                        
PRDTCNT  DC    F'0'                                                             
         DC    A(7)                                                             
         DC    AL1(0),AL3(7)                                                    
         DC    A((PRDTABX-PRDTAB)/7)                                            
*                                                                               
PUBTPARS DC    A(0)                PARAMS TO SEARCH PUB TABLE                   
         DC    A(PUBTAB)                                                        
PUBTCNT  DC    F'0'                                                             
         DC    A(7)                                                             
         DC    AL1(0),AL3(7)                                                    
         DC    A((PUBTABX-PUBTAB)/7)                                            
*                                                                               
PUBCPARS DC    A(0)                PARAMS TO SEARCH PUB CONV TABLE              
         DC    V(PUBCTAB)                                                       
PUBCCNT  DC    F'0'                                                             
         DC    A(13)                                                            
         DC    AL1(0),AL3(7)                                                    
         DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
MSG1     DC    CL40'SHORT RATED TO ANOTHER EST'                                 
MSG2     DC    CL40'MEDIA CODE NOT 1 OR 2'                                      
MSG3     DC    CL40'SHORT RATE ESTIMATE DATA'                                   
MSG4     DC    CL40'BUY RECORD ID NOT 4 OR 9'                                   
MSG5     DC    CL40'SPACE DESCRIPTION TOO LONG'                                 
MSG6     DC    CL40'COMMENTS TOO LONG'                                          
MSG7     DC    CL40'PUB RECORD NOT FOUND'                                       
MSG8     DC    CL40'NO CODE FOR INPUT TYPE'                                     
MSG9     DC    CL40'INCHES MORE THAN 2 DEC'                                     
MSG10    DC    CL40'GROUP PUB ID PRESENT'                                       
MSG11    DC    CL40'MISSING PRODUCT HEADER'                                     
         EJECT                                                                  
***************************************************                             
* TRANSLATION TABLE FOR ONE BYTE EDITION CODES IN *                             
* OM CONVERSION TABLE TO INTERNAL EDITION CODES   *                             
*                                                 *                             
* TABLE ENTRIES ARE  MEDIA     CL1                *                             
*                    OLDPUB    XL5,CL1            *                             
*                    NEWPUB    XL5,CL1            *                             
***************************************************                             
         SPACE 1                                                                
FIXEDT   NMOD1 0,FIXEDT                                                         
         L     RC,0(R1)            RESTORE REG POINTER                          
*                                                                               
         L     R1,=V(PUBCTAB)                                                   
*                                                                               
FIXEDT2  LA    R1,6(R1)            POINT TO EDITION CODE                        
         BAS   RE,FIXIT                                                         
*                                                                               
         LA    R1,6(R1)            POINT TO NEW EDITION CODE                    
         BAS   RE,FIXIT                                                         
*                                                                               
         LA    R1,1(R1)            POINT TO NEXT MEDIA                          
         CLI   0(R1),X'FF'                                                      
         BNE   FIXEDT2                                                          
         XIT1                                                                   
*                                                                               
FIXIT    CLI   0(R1),0             TEST NO EDITION CODE                         
         BER   RE                                                               
*                                                                               
         LA    R4,EDTABLE          POINT TO TRANSLATE LIST                      
         LA    R5,(EDTABLX-EDTABLE)/L'EDTABLE                                   
*                                                                               
FIXIT2   CLC   0(1,R1),0(R4)       MATCH EXTERNAL CODE                          
         BE    FIXEDT4                                                          
         LA    R4,L'EDTABLE(R4)                                                 
         BCT   R5,FIXIT2                                                        
         DC    H'0'                                                             
*                                                                               
FIXEDT4  MVC   0(1,R1),3(R4)       MOVE INTERNAL CODE TO ENTRY                  
         BR    RE                                                               
         SPACE 1                                                                
EDTABLE  DS    0CL4                                                             
         DC    C'M  A'                                                          
         DC    C'E  B'                                                          
         DC    C'D  C'                                                          
         DC    C'V  D'             INPUT FORMAT IS ME                           
         DC    C'U  I'             INPUT FORMAT IS SU                           
         DC    C'X  L'             INPUT FORMAT IS OD                           
         DC    C'P  P'                                                          
         DC    C'R  R'                                                          
         DC    C'S  S'                                                          
         DC    C'T  T'                                                          
         DC    C'W  W'                                                          
EDTABLX  DS    0C                                                               
***      DC    C'SAME'                                                          
***      DC    C'SAEF'                                                          
***      DC    C'SD G'                                                          
***      DC    C'SMEH'                                                          
***      DC    C'OM J'                                                          
***      DC    C'OE K'                                                          
***      DC    C'OMEM'                                                          
***      DC    C'MONU'                                                          
         EJECT                                                                  
***************************************                                         
* BUILD TABLE BY MEDIA OF ALL OM PUBS *                                         
***************************************                                         
         SPACE 1                                                                
BLDPUBS  NMOD1 0,BLDPUBS                                                        
         L     RC,0(R1)            RESTORE REG                                  
*                                                                               
         L     R4,=A(PUBTAB)                                                    
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),QOPT2        SET MEDIA CODE                               
         GOTO1 HIGHPUB                                                          
         B     BLDPUB4                                                          
*                                                                               
BLDPUB2  DS    0H                                                               
         GOTO1 SEQPUB                                                           
*                                                                               
BLDPUB4  CLI   KEY,X'FF'                                                        
         BE    BLDPUBX                                                          
         CLI   QOPT2,C' '          TEST MEDIA LIMIT                             
         BE    *+14                                                             
         CLC   KEY(1),QOPT2        YES - MATCH MEDIA                            
         BNE   BLDPUBX                                                          
         CLI   KEY+PUBKCOD-PUBREC,X'81'      TEST MASTER RECORD                 
         BNE   BLDPUB2                                                          
         CLC   KEY+PUBKAGY-PUBREC(2),=C'OG'                                     
         BNE   BLDPUB2                                                          
*                                                                               
         C     R4,=A(PUBTABX)                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   0(7,R4),KEY         MOVE MEDIA/PUB/ZON/ED                        
         LA    R4,7(R4)                                                         
         BCT   R5,BLDPUB2                                                       
*                                                                               
BLDPUBX  LPR   R5,R5                                                            
         L     RE,=A(PUBTCNT)                                                   
         ST    R5,0(RE)            SET NUMBER OF ENTRIES IN PARMS               
         XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'*PUBTAB*'                                                    
PUBTAB   DS    20000XL7                                                         
PUBTABX  EQU   *                                                                
         EJECT                                                                  
****************************************************                            
* PUT ALL CLIENT AND AGENCY HEADERS TO OUTPUT TAPE *                            
****************************************************                            
         SPACE 1                                                                
BLDACP   NMOD1 0,BLDACP                                                         
         L     RC,0(R1)            RESTORE REG                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'OG'       SET AGENCY CODE                              
         MVI   KEY+2,C'M'          SET FOR MAGAZINES                            
         MVI   KEY+3,1             SET RECORD CODE                              
*                                                                               
BLDAGY2  GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(4),KEYSAVE      SAME AG/M/CODE                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT3,C'N'          TEST SUPPRESS OUTPUT TAPE                    
         BE    BLDAGY4                                                          
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETBUY                                                           
         SR    RE,RE                                                            
         ICM   RE,3,PBUYREC+25                                                  
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,PBUYREC-4                                                  
         LA    R0,PBUYREC-4                                                     
         PUT   FILEOUT,(R0)                                                     
         L     RE,=A(OUTCOUNT)                                                  
         AP    0(4,RE),=P'1'                                                    
         L     RE,=A(CPYCOUNT)                                                  
         AP    0(4,RE),=P'1'                                                    
*                                                                               
BLDAGY4  XC    KEY,KEY                                                          
         MVC   KEY(4),KEYSAVE      RESTORE PREV AG/M/CODE                       
         CLI   KEY+2,C'M'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'N'                                                       
         B     BLDAGY2                                                          
         CLI   KEY+2,C'N'                                                       
         BNE   BLDCLT                                                           
         MVI   KEY+2,C'O'                                                       
         B     BLDAGY2                                                          
         EJECT                                                                  
*****************************************                                       
* PUT ALL CLIENT HEADERS TO OUTPUT TAPE *                                       
*****************************************                                       
         SPACE 1                                                                
BLDCLT   XC    KEY,KEY                                                          
         MVC   KEY(2),=C'OG'       SET AGENCY CODE                              
         MVI   KEY+2,C'M'          SET FOR MAGAZINES                            
         MVI   KEY+3,2             SET RECORD CODE                              
*                                                                               
BLDCLT1  GOTO1 HIGH                                                             
         B     BLDCLT4                                                          
*                                                                               
BLDCLT2  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
BLDCLT4  CLC   KEY(4),KEYSAVE      SAME AG/M/CODE                               
         BNE   BLDCLT10                                                         
*                                                                               
         CLI   QOPT3,C'N'          TEST SUPPRESS TAPE                           
         BE    BLDCLT10                                                         
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETBUY                                                           
         SR    RE,RE                                                            
         ICM   RE,3,PBUYREC+25                                                  
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,PBUYREC-4                                                  
         LA    R0,PBUYREC-4                                                     
         PUT   FILEOUT,(R0)                                                     
         L     RE,=A(OUTCOUNT)                                                  
         AP    0(4,RE),=P'1'                                                    
         L     RE,=A(CPYCOUNT)                                                  
         AP    0(4,RE),=P'1'                                                    
         B     BLDCLT2                                                          
*                                                                               
BLDCLT10 XC    KEY,KEY                                                          
         MVC   KEY(4),KEYSAVE      RESTORE PREV AG/M/CODE                       
         CLI   KEY+2,C'M'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'N'                                                       
         B     BLDCLT1                                                          
         CLI   KEY+2,C'N'                                                       
         BNE   BLDPRD                                                           
         MVI   KEY+2,C'O'                                                       
         B     BLDCLT1                                                          
         EJECT                                                                  
*************************************                                           
* BUILD LIST OF ALL PRODUCT HEADERS *                                           
*************************************                                           
         SPACE 1                                                                
BLDPRD   L     R4,=A(PRDTAB)                                                    
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'OG'       SET AGENCY CODE                              
         MVI   KEY+2,C'M'          SET FOR MAGAZINES                            
         MVI   KEY+3,6             SET RECORD CODE                              
*                                                                               
BLDPRD1  GOTO1 HIGH                                                             
         B     BLDPRD4                                                          
*                                                                               
BLDPRD2  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
BLDPRD4  CLC   KEY(4),KEYSAVE      SAME AG/M/CODE                               
         BNE   BLDPRD10                                                         
*                                                                               
         CLI   QOPT3,C'N'          TEST SUPPRESS TAPE                           
         BE    BLDPRD8                                                          
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETBUY                                                           
         SR    RE,RE                                                            
         ICM   RE,3,PBUYREC+25                                                  
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,PBUYREC-4                                                  
         LA    R0,PBUYREC-4                                                     
         PUT   FILEOUT,(R0)                                                     
         L     RE,=A(OUTCOUNT)                                                  
         AP    0(4,RE),=P'1'                                                    
         L     RE,=A(CPYCOUNT)                                                  
         AP    0(4,RE),=P'1'                                                    
*                                                                               
BLDPRD8  C     R4,=A(PRDTABX)                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R4),KEY+2       MOVE MEDIA                                   
         MVC   1(6,R4),KEY+4       MOVE CLT/PRD                                 
         LA    R4,7(R4)                                                         
         BCT   R5,BLDPRD2                                                       
*                                                                               
BLDPRD10 XC    KEY,KEY                                                          
         MVC   KEY(4),KEYSAVE      RESTORE PREV AG/M/CODE                       
         CLI   KEY+2,C'M'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'N'                                                       
         B     BLDPRD1                                                          
         CLI   KEY+2,C'N'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'O'                                                       
         B     BLDPRD1                                                          
*                                                                               
BLDPRDX  LPR   R5,R5                                                            
         L     RE,=A(PRDTCNT)                                                   
         ST    R5,0(RE)            SET NUMBER OF ENTRIES IN PARMS               
         XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'*PRDTAB*'                                                    
PRDTAB   DS    5000XL7                                                          
PRDTABX  EQU   *                                                                
         EJECT                                                                  
OMRECD   DSECT                                                                  
*                                                                               
MAGREC   DS    0C                  MAGAZINE DETAIL RECORD                       
MAGDETL  DS    0CL150                                                           
MAGKEY   DS    0CL21                                                            
MAGAGMED DS    XL1                                                              
MAGCLI   DS    CL2                                                              
MAGPUBCD DS    CL4                                                              
MAGPR    DS    CL3                                                              
MAGEST   DS    CL2                                                              
         DS    XL1                                                              
MAGLINNO DS    XL1                                                              
MAGPUBNO DS    PL4                                                              
MAGRECID DS    CL1'4'                                                           
MAGSBLI  DS    XL1                                                              
MAGSSBLI DS    XL1                                                              
* * * * * * *  END OF KEY * * * * * * * *                                       
MAGLINK  DS    XL1                                                              
         DS    X'00'                                                            
MAGORBIL DS    PL2                                                              
MAGDELTE DS    XL1                                                              
MAGLSSL  DS    XL1                                                              
         DS    XL1'0'                                                           
MAGLSL   DS    XL1                                                              
MAGSRD   DS    CL1                                                              
MAGINSDT DS    XL3                                                              
MAGONSDT DS    XL3                                                              
MAGCLODT DS    XL3                                                              
MAGCANDT DS    XL3                                                              
MAGBLLDT DS    XL3                                                              
MAGPDDT  DS    XL3                                                              
MAGGRSRT DS    PL7                                                              
         DS    XL5'0'                                                           
MAGPWNOD DS    XL1                                                              
         DS    3X'0'                                                            
MAGADCHG DS    CL1                                                              
MAGSPDSC DS    CL25                                                             
MAGBUYER DS    CL10                                                             
MAGACTDT DS    CL6                                                              
MAGMEMNM DS    CL25                                                             
MAGDISC  DS    PL2                                                              
MAGCOMM  DS    CL1                                                              
MAGTAX   DS    PL2                                                              
         DS    XL2                                                              
MAGEST90 DS    XL3                                                              
         DS    XL4                                                              
MAGBLIND DS    CL1                                                              
MAGJOBNO DS    CL4                                                              
MAGLNGTH EQU   *-MAGDETL                                                        
         EJECT                                                                  
         ORG   OMRECD                                                           
MPCRCRD  DS    0CL150              MAGAZINE PERMANENT COMMENT RECORD            
MPCKEY   DS    0CL21                                                            
MPCAMCL  DS    0CL3                                                             
MPCAM    DS    XL1                                                              
MPCCL    DS    CL2                                                              
MPCPUBCD DS    CL4                                                              
MPCPR    DS    CL3                                                              
MPCEST   DS    XL2                                                              
         DS    XL1                                                              
MPCLINNO DS    XL1                                                              
MPCPUBNO DS    PL4                                                              
MPCRECID DS    0XL2                                                             
         DS    CL1'9'                                                           
MPCNO    DS    XL1                                                              
         DS    XL5                                                              
MPCDELTE DS    XL1                                                              
         DS    XL7                                                              
MPCBODYX DS    0CL80                                                            
         DS    CL3                                                              
MPCBODY1 DS    CL77                                                             
         DS    CL31                                                             
MPCACTDT DS    CL6                                                              
         EJECT                                                                  
         ORG   OMRECD                                                           
NWSRCRD  DS    0CL150              NEWSPAPER DETAIL RECORD                      
NWSKEY   DS    0CL21                                                            
NWSAMCL  DS    0CL3                                                             
NWSAM    DS    XL1                                                              
NWSCL    DS    CL2                                                              
NWSPUBCD DS    0XL4                                                             
NWSPUBTP DS    CL1                                                              
NWSPUBNM DS    XL3                                                              
NWSPR    DS    CL3                                                              
NWSEST   DS    XL2                                                              
         DS    X'00'                                                            
NWSLINNO DS    XL1                                                              
NWSPUBNO DS    PL4                                                              
NWSRECID DS    X'4'                                                             
NWSSBLI  DS    XL1                                                              
NWSSSBLI DS    XL1                                                              
***************    END OF KEY  *******************                              
NWSLINK  DS    XL1                                                              
NWSFROM  DS    XL1                                                              
NWSCANDT DS    0PL2                                                             
NWSCANYR DS    PL1                                                              
NWSCANMO DS    PL1                                                              
NWSDELTE DS    XL1                                                              
NWSLSSL  DS    XL1                                                              
NWSLSSSL DS    XL1                                                              
NWSLSL   DS    XL1                                                              
NWSSRD   DS    CL1                                                              
NWSINSDT DS    0PL3                                                             
NWSINSYR DS    PL1                                                              
NWSINSMO DS    PL1                                                              
NWSINSDY DS    PL1                                                              
NWSTOTCR DS    PL2                                                              
NWSCZCR  DS    PL2                                                              
NWSTZCR  DS    PL2                                                              
NWSOTHCR DS    PL2                                                              
NWSGPBUY DS    CL1                                                              
NWSBLLDT DS    0PL3                                                             
NWSBLLYR DS    PL1                                                              
NWSBLLMO DS    PL1                                                              
NWSBLLDY DS    PL1                                                              
NWSPDDT  DS    0PL3                                                             
NWSPDYR  DS    PL1                                                              
NWSPDMO  DS    PL1                                                              
NWSPDDY  DS    PL1                                                              
NWSGRSRT DS    PL7                                                              
NWSGRIND DS    CL1                                                              
NWSGPDT  DS    XL3                                                              
NWSCLRNO DS    CL1                                                              
NWSPWNOD DS    XL1                                                              
NWSZONE  DS    CL2                                                              
NWSEDITN DS    CL1                                                              
NWSADCHG DS    CL1                                                              
NWSLIN   DS    PL3                                                              
NWSPAGES DS    CL1                                                              
NWSSPACE DS    0CL5                                                             
NWSSPTYP DS    CL1                                                              
NWSCOLNO DS    PL1                                                              
NWSCOLIN DS    0XL3                                                             
NWSINCHS DS    PL1                                                              
NWSNUM   DS    PL1                                                              
NWSDEN   DS    PL1                                                              
         ORG   NWSCOLNO                                                         
NWSSNUM  DS    XL1                                                              
NWSSALF  DS    CL1                                                              
NWSSCNT  DS    XL1                                                              
         ORG   NWSINCHS                                                         
NWSLINES DS    XL2                                                              
         DS    X'00'                                                            
NWSSPDSC DS    CL15                                                             
         DS    C                                                                
         ORG   NWSSPTYP                                                         
NWSMEMNM DS    CL21                                                             
NWSBUYER DS    0CL10                                                            
NWSTOTIN DS    PL4                                                              
         DS    CL5                                                              
NWSORLNO DS    X                                                                
NWSACTDT DS    0CL6                                                             
NWSACTYR DS    CL2                                                              
NWSACTMO DS    CL2                                                              
NWSACTDY DS    CL2                                                              
         ORG   NWSORLNO                                                         
NWSGMIND DS    XL1                                                              
NWSGM    DS    0CL6                                                             
NWSGMPUB DS    PL4                                                              
NWSGMZNE DS    PL1                                                              
NWSGMEDT DS    CL1                                                              
NWSADNO  DS    CL15                                                             
         DS    XL10                                                             
NWSDISC  DS    PL2                                                              
NWSCOMM  DS    CL1                                                              
NWSTAX   DS    PL2                                                              
NWSFLAG1 DS    XL1                                                              
NWSFLAG  DS    0XL1                                                             
NWSRATE  DS    XL1                                                              
NWSEST90 DS    0XL3                                                             
NWSORGES DS    XL1                                                              
NWSORGLN DS    XL1                                                              
NWSORGSL DS    XL1                                                              
         ORG   NWSEST90                                                         
NWSDREC  DS    0XL3                                                             
NWSOVR90 DS    XL1                                                              
NWS90SL  DS    XL1                                                              
         DS    XL1                                                              
NWSLNRTE DS    PL4                                                              
NWSBLIND DS    CL1                                                              
NWSDSTOV DS    PL4                                                              
NWSEND   DS    0C                                                               
NWSLNGTH EQU   *-NWSRCRD                                                        
         EJECT                                                                  
         ORG   OMRECD                                                           
R4RECORD DS    0CL150                                                           
R4RKEY   DS    0CL21                                                            
R4RAGMED DS    XL1                                                              
R4RCLI   DS    CL2                                                              
R4RFF    DS    X'FF'                                                            
R4RPRO   DS    CL3                                                              
R4REST   DS    XL2                                                              
R4RPUBN  DS    XL4                                                              
R4RLINE  DS    XL1                                                              
R4RSUBL  DS    XL1                                                              
R4RNWSID DS    0XL3                                                             
R4RNWYY  DS    XL1                                                              
R4RNNWMM DS    XL1                                                              
R4RNWDD  DS    XL1                                                              
R4RRECID DS    C'9'                                                             
         DS    XL2'0'                                                           
R4RCHDTE DS    XL3                                                              
R4RPUBAL DS    CL1                                                              
R4RDELET DS    XL1                                                              
R4RSUPER DS    XL3                                                              
R4RTRAN  DS    CL1                                                              
R4RGROS  DS    PL5                                                              
R4RGRLCD DS    PL5                                                              
R4RORIG  DS    PL5                                                              
R4ROGLCD DS    PL5                                                              
R4RTAX   DS    XL2                                                              
R4RCOM   DS    XL1                                                              
R4RCD    DS    XL2                                                              
R4RPRALO DS    5CL6                                                             
         ORG   R4RPRALO                                                         
R4RPRAL1 DS    CL3                                                              
R4RPRPC1 DS    PL3                                                              
         DS    4PL6                                                             
R4RCURTX DS    XL3                                                              
R4RORGTX DS    XL3                                                              
R4RADNO  DS    CL10                                                             
R4RINSDT DS    XL3                                                              
R4RCHCOD DS    CL6                                                              
R4RSPDES DS    CL25                                                             
R4RMAGZ  DS    0C                                                               
R4RCLOSE DS    XL3                                                              
R4RONSLE DS    CL3                                                              
         ORG   R4RMAGZ                                                          
R4REDTN  DS    CL1                                                              
R4RSPACE DS    0CL6                                                             
R4RLINES DS    CL4                                                              
R4RTYPE  DS    CL1                                                              
R4RCOLOR DS    C' '                                                             
R4RGRPIN DS    C'N'                                                             
R4RLNERT DS    PL4                                                              
R4RERORS DS    XL3                                                              
         ORG   R4RKEY+150                                                       
R4RLNGTH EQU   *-R4RECORD                                                       
         EJECT                                                                  
         ORG   OMRECD                                                           
ESTKEY   DS    0CL21                                                            
ESTAGMED DS    XL1                                                              
ESTCLI   DS    CL2                                                              
         DS    XL4'0'                                                           
ESTPR    DS    CL3                                                              
ESTEST   DS    XL2                                                              
         DS    XL6'0'                                                           
ESTRECID DS    CL1'2'                                                           
         DS    XL2'0'                                                           
************ END OF KEY ***********                                             
         DS    XL4                 +21                                          
ESTDELTE DS    XL1                 +25                                          
*                                                                               
         DS    XL4                 +26                                          
*                                                                               
ESTNODD  DS    XL2                 +30                                          
ESTSTRTO DS    CL6                 +32                                          
ESTENDO  DS    CL6                 +38                                          
ESTDESCO DS    CL45                +44                                          
         DS    XL1                 +89                                          
*                                                                               
         DS    XL1                 +90                                          
ESTNOEVN DS    XL2                 +91                                          
ESTSTRTE DS    CL6                 +93                                          
ESTENDE  DS    CL6                 +99                                          
ESTDESCE DS    CL45                +105                                         
*                                                                               
ESTHEND  DS    0X                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PPREPOM02 05/01/02'                                      
         END                                                                    
