*          DATA SET SRCAL00    AT LEVEL 002 AS OF 11/09/18                      
*PHASE T13900A                                                                  
*INCLUDE SIXPACK                                                                
*INCLUDE SIXUNPK                                                                
*&&US                                                                           
*INCLUDE TINVCON                                                                
*&&                                                                             
*&&UK                                                                           
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*&&                                                                             
         TITLE 'T13900 - CONVERT / COMPUTE FACILITY'                            
CALCMOD  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,*$CALC**,R8,RR=R2,CLEAR=YES                            
         USING WRKD,RC                                                          
         MVC   SRPARMS(24),0(R1)                                                
         ST    R2,RELO                                                          
         MVI   MSG,C' '                                                         
         MVC   MSG+1(59),MSG                                                    
         MVC   SPACES,MSG                                                       
         MVI   EXTDATE,0           CLEAR EXTENDED DATE FLAG                     
         MVI   DATE,C' '                                                        
         MVC   DATE+1(L'DATE-1),DATE                                            
         L     RA,SRPARMS+20                                                    
         USING T139FFD,RA                                                       
         L     R9,SRPARMS+12                                                    
         USING COMFACSD,R9                                                      
         L     R4,SRPARMS+8        R4=A(UTL)                                    
         ST    R4,AUTL                                                          
*                                                                               
         MVC   ADATCON,CDATCON     SET A(DATCON)                                
         MVC   ADATVAL,CDATVAL     SET A(DATVAL)                                
*&&DO                                                                           
         LT    RF,=V(DATCON)                                                    
         BZ    *+10                                                             
         AR    RF,R2                                                            
         ST    RF,ADATCON                                                       
         LT    RF,=V(DATVAL)                                                    
         BZ    *+10                                                             
         AR    RF,R2                                                            
         ST    RF,ADATVAL                                                       
*&&                                                                             
         EJECT                                                                  
* LOOK FOR COMMANDS                                                             
*                                                                               
         LA    R2,CALBH                                                         
         JIF   CALA(3),=,=C'MSPACK',EX2                                         
         JIF   CALA(3),=,=C'MSUNPK',EX4                                         
         JIF   CALA(3),=,=C'DATVAL',EX5                                         
         JIF   CALA(3),=,=C'DTPACK',EX6                                         
         JIF   CALA(3),=,=C'DNPACK',EX7                                         
         JIF   CALA(3),=,=C'DTUNPK',EX8                                         
         JIF   CALA(3),=,=C'DNUNPK',EX9                                         
         JIF   CALA(3),=,=C'CLPACK',EX10                                        
         JIF   CALA(3),=,=C'CLUNPK',EX12                                        
         JIF   CALA(3),=,=C'TINVCON',EX13                                       
         JIF   CALA(4),=,=C'GETJUL',EX14                                        
         JIF   CALA(3),=,=C'GETDAY',EX15                                        
         JIF   CALA(4),=,=C'ADDAY ',EX16                                        
         JIF   CALA(3),=,=C'CVB   ',EX18                                        
         JIF   CALA(3),=,=C'CVD   ',EX20                                        
         JIF   CALA(3),=,=C'ADD',EX26                                           
         JIF   CALA(3),=,=C'SUB',EX28                                           
         JIF   CALA(3),=,=C'MUL',EX30                                           
         JIF   CALA(3),=,=C'DIV',EX32                                           
         JIF   CALA(3),=,=C'INVERT',EX40                                        
         JIF   CALA(3),=,=C'NETWEEK',EX42                                       
         JIF   CALA(3),=,=C'SXPACK',EX44                                        
         JIF   CALA(3),=,=C'SXUNPK',EX46                                        
         JIF   CALA,=,C'+',EX26                                                 
         JIF   CALA,=,C'-',EX28                                                 
         JIF   CALA,=,C'X',OR,C'*',EX30                                         
         JIF   CALA,=,C'/',EX32                                                 
         JIF   CALA(3),=,=C'BRD',EX34                                           
         JIF   CALA(2),=,=C'TU',EX50         CONVERT TIMER UNITS TO MS          
         JIF   CALA(3),=,=C'UNJ',EX52        UNJULIAN YYDDD                     
         JIF   CALA(3),=,=C'ORPACK',EX54     ORDER # PACK                       
         JIF   CALA(3),=,=C'ORUNPK',EX56     ORDER # UNPACK                     
         JIF   CALA(3),=,=C'TRPACK',EX60                                        
         JIF   CALA(3),=,=C'TRUNPK',EX62                                        
         JIF   CALA(3),=,=C'RCPACK',EX64                                        
         JIF   CALA(3),=,=C'RCUNPK',EX66                                        
         LA    R2,CALAH                                                         
         B     ERR5                                                             
         EJECT                                                                  
* EXECUTE SPECIFIC COMMANDS                                                     
*                                                                               
EX2      DS    0H                                                               
*&&US                                                                           
         BAS   RE,GETAGY           SWITCH TO SPOT                               
         MVC   PARA+4(3),=X'D9000A'   GET V(STAVAL)                             
         MVI   PARA+7,QSTAVAL                                                   
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         LA    R4,STAVBLK          STAVAL WORK DSECT                            
         USING STABLKD,R4                                                       
         XC    STBADDR(STBLNQ),STBADDR                                          
         LA    R2,CALCH                                                         
         ST    R2,STBADDR                                                       
         LA    RE,AGYIO                                                         
         MVC   STBCTRY,AGYPROF+7-AGYHDR(RE)                                     
         MVI   STBMED,C'T'         MEDIA TV                                     
         CLI   CALC+4,C'/'                                                      
         BNE   MSP6                                                             
         CLI   STBCTRY,C'C'        CANADIAN NETWORK?                            
         BNE   MSP10                                                            
         MVI   STBMED,C'N'         SET CANADIAN NETWORK                         
         B     MSP10                                                            
*                                                                               
MSP6     CLI   CALC+4,C' '                                                      
         BNH   MSP10                                                            
         MVI   STBMED,C'R'         MEDIA RADIO                                  
         CLI   CALC+4,C'A'                                                      
         BE    MSP10                                                            
         CLI   CALC+4,C'F'                                                      
         BE    MSP10                                                            
         MVC   STBMED,CALC+4       ALL OTHER                                    
*                                                                               
MSP10    STCM  R9,15,STBACOM       R9 STILL HAS A(COMFACS)                      
         GOTO1 (RF),PARA,(R4)                                                   
         CLI   STBERR,0                                                         
         BNE   ERR8                                                             
         MVC   WORK+4(5),STBSTA                                                 
         CLC   STBNET,SPACES                                                    
         BNH   MSP30                                                            
         MVI   WORK+8,C'/'                                                      
         MVC   WORK+9(3),STBNET                                                 
*                                                                               
MSP30    CLI   WORK+4,C' '                                                      
         BH    *+8                                                              
         MVI   WORK+4,C'T'                                                      
         LA    R2,CALBH                                                         
         BAS   RE,GETNUM                                                        
         UNPK  WORK(4),DUB                                                      
         OI    WORK+3,X'F0'                                                     
         MVC   PARA+4(3),=X'D9000A'   GET V(STAPACK)                            
         MVI   PARA+7,QSTAPACK                                                  
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         L     RE,AUTL                                                          
         MVC   STAPAGY,TAGY-UTLD(RE)                                            
         LA    RE,STAVBLK                                                       
         MVC   STAPMED,STBMED-STABLKD(RE)  MEDIA ALREADY FIGURED ABOVE          
         MVC   STAPCTRY,STBCTRY-STABLKD(RE)                                     
         STCM  R9,15,STAPACOM                                                   
         MVC   STAPQMKT,WORK       MARKET                                       
         MVC   STAPQSTA(8),WORK+4  STATION                                      
*                                                                               
         GOTO1 (RF),(R1)                                                        
         CLI   STAPERR,0                                                        
         BNE   ERR16                                                            
         MVC   DUB(5),STAPMKST    RETURN RESULT                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 CHEXOUT,PARA,DUB,CALD,5,=C'TOG'                                  
*&&                                                                             
         B     OK                                                               
*                                                                               
EX4      DS    0H                                                               
*&&US                                                                           
         BAS   RE,GETAGY           MSUNPK                                       
         BAS   RE,GETHEX                                                        
         GOTO1 CHEXIN,PARA,WORK,DUB,10                                          
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
*                                                                               
         MVC   PARA+4(3),=X'D9000A'   GET V(STAPACK)                            
         MVI   PARA+7,QSTAPACK                                                  
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         L     R3,AUTL                                                          
         MVC   STAPAGY,TAGY-UTLD(R3)                                            
         LA    RE,AGYIO                                                         
         MVC   STAPCTRY,AGYPROF+7-AGYHDR(RE)                                    
         MVI   STAPMED,C'T'                                                     
*                                                                               
         CLI   STAPCTRY,C'C'       TEST CANADA                                  
         BE    *+12                                                             
         CLI   DUB+2,X'E8'         CABLE?                                       
         BNL   EX4A                                                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,DUB+4                                                       
         N     R0,=X'0000000F'                                                  
         CHI   R0,1                0=AM, 1=FM, 2=TV                             
         BH    *+12                                                             
         MVI   STAPMED,C'R'                                                     
         B     EX4A                                                             
         CLI   STAPCTRY,C'C'       TEST CANADA                                  
         BNE   EX4A                MEDIA SET                                    
         CHI   R0,3                TEST 3=N, 4=X, >4=N                          
         BE    *+12                                                             
         CHI   R0,4                                                             
         BNH   EX4A                                                             
         MVI   STAPMED,C'N'                                                     
*                                                                               
EX4A     CLC   CALD,SPACES                                                      
         BNH   *+10                                                             
         MVC   STAPMED,CALD                                                     
*                                                                               
         STCM  R9,15,STAPACOM                                                   
         MVC   STAPMKST,DUB        MKTSTA                                       
*                                                                               
         GOTO1 (RF),(R1)                                                        
         CLI   STAPERR,0                                                        
         BNE   ERR16                                                            
*                                                                               
         MVC   CALC(4),STAPQMKT    RETURN RESULT                                
         MVC   CALD(8),STAPQSTA                                                 
         DROP  R1                                                               
*                                                                               
         CLC   CALD+5(3),SPACES                                                 
         BNH   *+8                                                              
         MVI   CALD+4,C'/'                                                      
*&&                                                                             
         B     OK                                                               
*                                                                               
EX5      CLI   CALA+6,C'X'         DATVAL/DATVALX                               
         BNE   *+8                                                              
         MVI   EXTDATE,X'20'       SET EXTENDED DATE ALLOWED DATVALX            
         XC    ISODATE,ISODATE                                                  
         BAS   RE,GETDATE                                                       
         TM    EXTDATE,X'20'       TEST IF ALLOWED 1900-1963 EXTENDED           
         BZ    EX5B                                                             
         TM    PARA+4,X'10'        TEST IF 19XX YEAR INPUT                      
         BZ    EX5B                                                             
EX5A     MVC   DUB(6),DATE         CREATE 19XX ISO DATE                         
         MVC   ISODATE+0(2),=C'19'                                              
         MVC   ISODATE+2(2),DUB+0                                               
         MVI   ISODATE+4,C'-'                                                   
         MVC   ISODATE+5(2),DUB+2                                               
         MVI   ISODATE+7,C'-'                                                   
         MVC   ISODATE+8(2),DUB+4                                               
         B     EX5C                                                             
EX5B     GOTO1 ADATCON,PARA,(0,DATE),(23,ISODATE)  YYYY-MM-DD                   
EX5C     GOTO1 ADATCON,PARA,(10,ISODATE),(21,CALC) MMMDD/YYYY DDMMMYYYY         
         MVC   CALD,ISODATE                                                     
         B     OK                                                               
*                                                                               
EX6      BAS   RE,GETDATE          DTPACK                                       
         XC    ISODATE,ISODATE                                                  
         GOTO1 ADATCON,PARA,DATE,(23,ISODATE)                                   
         CLC   ISODATE(4),=C'2027'                                              
         BNH   *+14                                                             
         MVC   CALC(8),=C'ERR>2027'                                             
         B     EX6A                                                             
         GOTO1 ADATCON,PARA,DATE,(2,DUB)                                        
         GOTO1 CHEXOUT,PARA,DUB,CALC,2,=C'TOG'                                  
EX6A     MVC   CALD,ISODATE                                                     
         B     OK                                                               
*                                                                               
EX7      BAS   RE,GETDATE          DTPACK WITH NEW 1964 BASE YEAR               
         XC    ISODATE,ISODATE                                                  
         GOTO1 ADATCON,PARA,DATE,(23,ISODATE)                                   
         CLC   ISODATE(4),=C'1964'                                              
         BNL   *+14                                                             
         MVC   CALC(8),=C'ERR<1964'                                             
         B     EX7A                                                             
         GOTO1 ADATCON,PARA,DATE,(30,DUB)                                       
         GOTO1 CHEXOUT,PARA,DUB,CALC,2,=C'TOG'                                  
EX7A     MVC   CALD,ISODATE                                                     
         B     OK                                                               
*                                                                               
EX8      BAS   RE,GETHEX           DTUNPK                                       
         GOTO1 CHEXIN,PARA,WORK,DUB,4                                           
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
         GOTO1 ADATCON,PARA,(2,DUB),(21,CALC)                                   
         B     OK                                                               
*                                                                               
EX9      BAS   RE,GETHEX           DTUNPK WITH NEW 1964 BASE YEAR               
         GOTO1 CHEXIN,PARA,WORK,DUB,4                                           
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
         GOTO1 ADATCON,PARA,(14,DUB),(21,CALC)                                  
         B     OK                                                               
*                                                                               
EX10     DS    0H                  CLPACK                                       
*&&US                                                                           
         BAS   RE,GETDATA                                                       
*                                                                               
         MVC   PARA+4(3),=X'D9000A'                                             
         MVI   PARA+7,QCLPACK                                                   
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
*                                                                               
         GOTO1 (RF),PARA,WORK,DUB                                               
         CLI   0(R1),X'FF'                                                      
         BE    ERR7                                                             
         GOTO1 CHEXOUT,PARA,DUB,CALC,2,=C'TOG'                                  
*&&                                                                             
*&&UK                                                                           
         BAS   RE,GETDATA          CLPACK                                       
         GOTO1 =V(CLPACK),PARA,WORK,DUB,RR=RELO                                 
         CLI   0(R1),X'FF'                                                      
         BE    ERR7                                                             
         GOTO1 CHEXOUT,PARA,DUB,CALC,2,=C'TOG'                                  
*&&                                                                             
         B     OK                                                               
*                                                                               
EX12     DS    0H                  CLUNPK                                       
*&&US                                                                           
         BAS   RE,GETHEX                                                        
         GOTO1 CHEXIN,PARA,WORK,DUB,4                                           
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
*                                                                               
         MVC   PARA+4(3),=X'D9000A'                                             
         MVI   PARA+7,QCLUNPK                                                   
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
*                                                                               
         GOTO1 (RF),PARA,DUB,CALC                                               
*&&                                                                             
*&&UK                                                                           
         BAS   RE,GETHEX           CLUNPK                                       
         GOTO1 CHEXIN,PARA,WORK,DUB,4                                           
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
         GOTO1 =V(CLUNPK),PARA,DUB,CALC,RR=RELO                                 
*&&                                                                             
         B     OK                                                               
*                                                                               
EX13     DS    0H                  TINVCON                                      
*&&US                                                                           
         BAS   RE,GETDATA                                                       
         CLI   WORK,C'1'                                                        
         BL    EX13A                                                            
         CLI   5(R2),12                                                         
         BNE   ERR10                                                            
         PACK  DUB,WORK(12)                                                     
         LM    R0,R1,DUB                                                        
         SRDL  R0,4                                                             
         STCM  R0,3,WORK                                                        
         STCM  R1,15,WORK+2                                                     
         GOTO1 =V(TINVCON),PARA,WORK,DUB,ADATCON,RR=RELO                        
         CLI   0(R1),X'FF'                                                      
         BE    ERR10                                                            
         MVC   CALC(6),DUB                                                      
         B     OK                                                               
EX13A    CLI   5(R2),6                                                          
         BNE   ERR10                                                            
         XC    DUB,DUB                                                          
         GOTO1 =V(TINVCON),PARA,WORK,DUB,ADATCON,RR=RELO                        
         CLI   0(R1),X'FF'                                                      
         BE    ERR10                                                            
         LM    R0,R1,DUB                                                        
         SRDL  R0,12                                                            
         O     R1,=X'0000000C'                                                  
         STM   R0,R1,DUB                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  CALC(12),DUB                                                     
*&&                                                                             
         B     OK                                                               
*                                                                               
EX14     BAS   RE,GETDATE          GET JULIAN DATE                              
         GOTO1 ADATCON,PARA,DATE,(15,DUB)                                       
         GOTO1 CHEXOUT,PARA,DUB,CALC,4,=C'TOG'                                  
         B     OK                                                               
*                                                                               
EX15     BAS   RE,GETDATE          GETDAY                                       
         GOTO1 CGETDAY,PARA,DATE,CALC                                           
         B     OK                                                               
*                                                                               
EX16     BAS   RE,GETDATE          ADDAY                                        
         LA    R2,CALCH                                                         
         BAS   RE,GETCASH                                                       
         L     R4,CASH                                                          
         SRDA  R4,32                                                            
         D     R4,=F'100'                                                       
         GOTO1 CADDAY,PARA,DATE,DUB,(R5)                                        
         GOTO1 ADATCON,PARA,DUB,(8,CALD)                                        
         B     OK                                                               
*                                                                               
EX18     BAS   RE,GETNUM           CVB                                          
         GOTO1 CHEXOUT,PARA,NUM,CALC,4,=C'TOG'                                  
         B     OK                                                               
*                                                                               
EX20     BAS   RE,GETHEX           CVD                                          
         GOTO1 CHEXIN,PARA,WORK,DUB,(R4)                                        
         L     R4,PARA+12                                                       
         LTR   R4,R4                                                            
         BZ    ERR3                                                             
         CH    R4,=H'2'                                                         
         BL    EX22                                                             
         CH    R4,=H'3'                                                         
         BE    EX24                                                             
         CH    R4,=H'4'                                                         
         BE    EX25                                                             
         EDIT  (2,DUB),(10,CALC),ALIGN=LEFT,DUB=PARA                            
         B     OK                                                               
*                                                                               
EX22     EDIT  (1,DUB),(3,CALC),ALIGN=LEFT,DUB=PARA                             
         B     OK                                                               
*                                                                               
EX24     EDIT  (3,DUB),(12,CALC),ALIGN=LEFT,DUB=PARA                            
         B     OK                                                               
*                                                                               
EX25     EDIT  (4,DUB),(12,CALC),ALIGN=LEFT,DUB=PARA                            
         B     OK                                                               
*                                                                               
EX26     BAS   RE,GETNUM           ADD                                          
         L     R5,NUM                                                           
         LA    R2,CALCH                                                         
         BAS   RE,GETNUM                                                        
         A     R5,NUM                                                           
         B     EXOUT                                                            
*                                                                               
EX28     BAS   RE,GETNUM           SUBTRACT                                     
         L     R5,NUM                                                           
         LA    R2,CALCH                                                         
         BAS   RE,GETNUM                                                        
         S     R5,NUM                                                           
         B     EXOUT                                                            
*                                                                               
EX30     BAS   RE,GETNUM           MULTIPLY                                     
         L     R5,NUM                                                           
         LA    R2,CALCH                                                         
         BAS   RE,GETNUM                                                        
         M     R4,NUM                                                           
         B     EXOUT                                                            
*                                                                               
EX32     BAS   RE,GETNUM           DIVIDE                                       
         L     R5,NUM                                                           
         LA    R2,CALCH                                                         
         BAS   RE,GETNUM                                                        
         SR    R4,R4                                                            
         OC    NUM,NUM                                                          
         BZ    ERR7                                                             
         D     R4,NUM                                                           
         B     EXOUT                                                            
*                                                                               
EX34     CLI   5(R2),0             BRD                                          
         BE    ERR1                                                             
         GOTO1 ADATVAL,PARA,CALB,DATE   VALIDATE FOR M/D/Y                      
         OC    0(4,R1),0(R1)       IF IT'S VALID                                
         BNZ   EX35                IT'S NOT MONTH/YEAR                          
*                                                                               
         GOTO1 ADATVAL,PARA,(X'02',CALB),DATE    VALIDATE M/Y                   
         OC    0(4,R1),0(R1)       IF LENGTH=0 ==> ERROR                        
         BZ    ERR4                                                             
         MVC   DATE+4(2),=C'01'    MAKE DAY 01                                  
*                                                                               
EX35     MVC   PARA+4(3),=X'D9000A'   GET V(GETBROAD)                           
*&&US*&& MVI   PARA+7,QGETBRD                                                   
*&&UK*&& MVI   PARA+7,X'1D'                                                     
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),PARA,(1,DATE),WORK,CGETDAY,CADDAY   GET B'CAST MON          
*                                                                               
         MVC   DATE(2),WORK+6         GET B'CAST YEAR                           
         MVC   DATE+2(4),=C'0101'                                               
         GOTO1 (RF),(R1),,WORK+12     WORK+12 = B'CAST YR START DATE            
         MVC   DATE+2(4),=C'1201'                                               
         GOTO1 (RF),(R1),,WORK+18     WORK+24 = B'CAST YR END DATE              
         MVC   WORK+18(6),WORK+24                                               
*                                                                               
         GOTO1 ADATCON,PARA,(X'10',WORK),(8,CALCOM)                             
         GOTO1 (RF),(R1),(X'10',WORK+12),(8,CALCM2)                             
         MVC   CALCOM+20(17),=C'(BROADCAST MONTH)'                              
         MVC   CALCM2+20(16),=C'(BROADCAST YEAR)'                               
*                                                                               
         FOUT  CALCOMH             TRANSMIT                                     
         FOUT  CALCM2H                                                          
         B     OK                                                               
*                                                                               
EX40     BAS   RE,GETHEX           BINARY INVERT - R4 CONTAINS INPUT LN         
         STC   R4,BYTE             SAVE INPUT LENGTH                            
         GOTO1 CHEXIN,PARA,WORK,DUB,(R4)                                        
         ICM   R3,15,PARA+12       GET NUMBER OF OUTPUT BYTES                   
         BZ    ERR3                BAD HEX STRING                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    DUB(0),=8X'FF'                                                   
         LA    R3,1(R3)            RESTORE LENGTH                               
         GOTO1 CHEXOUT,PARA,DUB,CALC,(R3),0                                     
         TM    BYTE,X'01'          TEST FOR ODD INPUT LENGTH                    
         BZ    EX41                NO                                           
         ZIC   RE,BYTE                                                          
         LA    RE,CALC(RE)         BLANK OUT EXTRANEOUS CHARACTER               
         MVI   0(RE),C' '                                                       
*                                                                               
EX41     B     OK                  EXIT FROM INVERT                             
*                                                                               
EX42     BAS   RE,GETDATE          NETWEEK                                      
         MVC   PARA+4(4),=X'D9000A17' GET V(NETWEEK)                            
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),PARA,DATE,CGETDAY,CADDAY                                    
         MVC   YEAR,4(R1)          SAVE RETURNED VALUEDS                        
         MVC   HUTW,0(R1)                                                       
         MVC   NETW,8(R1)                                                       
*                                                                               
         MVC   CALC(3),=C'NET'     NETWORK(NTI) WEEK                            
         MVI   CALC+3,C'='                                                      
         ZIC   R1,NETW             WW/YY                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CALC+4(2),DUB                                                    
         MVI   CALC+6,C'/'                                                      
         ZIC   R1,YEAR                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CALC+7(2),DUB                                                    
*                                                                               
         MVC   CALD(3),=C'HUT'     HUT WEEK                                     
         MVI   CALD+3,C'='                                                      
         ZIC   R1,HUTW                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CALD+4(2),DUB                                                    
         MVI   CALD+6,C'/'                                                      
         MVC   CALD+7(2),CALC+7    COPY YEAR FROM NETWORK WK FIELD              
         B     OK                                                               
*                                                                               
EXOUT    EDIT  (R5),(12,CALD),ALIGN=LEFT                                        
         B     OK                                                               
         EJECT                                                                  
EX44     DS    0H                                                               
         LA    R2,CALBH                                                         
         CLI   5(R2),0                                                          
         BE    ERR1                                                             
         CLI   5(R2),4                                                          
         BH    ERR7                                                             
         ZIC   R0,5(R2)            GET INP LENGTH                               
         LA    R1,CALB                                                          
EX45     LA    RE,36                                                            
         LA    RF,ALPHA                                                         
         CLC   0(1,R1),0(RF)                                                    
         BE    *+16                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,*-14                                                          
         B     ERR7                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,EX45                                                          
*                                                                               
         GOTO1 =V(SIXPACK),PARA,CALB,WORK,4,RR=RELO                             
         GOTO1 CHEXOUT,PARA,WORK,CALC,3                                         
         B     OK                                                               
*                                                                               
EX46     DS    0H                                                               
         LA    R2,CALBH                                                         
         CLI   5(R2),0                                                          
         BE    ERR1                                                             
         CLI   5(R2),6                                                          
         BH    ERR7                                                             
         GOTO1 CHEXIN,PARA,CALB,WORK,6                                          
         GOTO1 =V(SIXUNPK),PARA,WORK,CALC,3,RR=RELO                             
         B     OK                                                               
         EJECT                                                                  
* TIMER UNITS TO MILLISECONDS                                                   
*                                                                               
EX50     BAS   RE,GETHEX                                                        
         GOTO1 CHEXIN,PARA,WORK,DUB,8                                           
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
*                                                                               
         SR    R0,R0                                                            
         L     R1,DUB                                                           
         D     R0,=F'38400'        GIVES TIME IN SECONDS                        
         SR    R0,R0                                                            
         D     R0,=F'60'           GIVES SECONDS IN R0                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CALD+6(2),DUB                                                    
         MVI   CALD+5,C'.'                                                      
*                                                                               
         SR    R0,R0               MINUTES ARE IN R1                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CALD+3(2),DUB                                                    
         MVI   CALD+2,C'.'                                                      
*                                                                               
         CVD   R1,DUB              HOURS                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  CALD(2),DUB                                                      
         B     OK                                                               
*                                                                               
EX52     BAS   RE,GETNUM           INPUT SHOULD BE YYDDD                        
         CLC   8(2,R2),=C'27'                                                   
         BH    *+8                                                              
         MVI   DUB+4,X'01'         SET CENTURY TO 1                             
         GOTO1 ADATCON,PARA,(6,DUB+4),(5,CALC)                                  
         B     OK                                                               
         EJECT                                                                  
EX54     BAS   RE,GETHEX           INPUT SHOULD BE 99991111                     
         CLI   WORK+1,C'3'                                                      
         BH    EX54N                                                            
*                                                                               
         GOTO1 CHEXIN,PARA,WORK,FULL,8                                          
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
*                                                                               
         MVC   CASH,FULL           MAKE A COPY OF THIS                          
         MVC   PACKOF4B,FULL                                                    
         OI    PACKOF4B+3,X'0F'                                                 
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
*                                                                               
         GOTO1 ADATCON,PARA,(5,0),(15,FULL)  CENTURY/YEAR FROM TODAY            
         SRP   FULL,61,0                                                        
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),CASH  STICK IN DAYS IN YEAR                        
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,NUM                                                         
         XC    NUM(2),=4X'FF'      ONLY XC'ING 2 BYTES OF FF'S                  
*                                                                               
         PACK  DUB,8+4(4,R2)       SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,NUM+2                                                       
         XC    NUM+2(2),=4X'FF'    ONLY XC'ING 2 BYTES OF FF'S                  
EX54G    GOTO1 CHEXOUT,PARA,NUM,CALC,4                                          
         B     OK                                                               
*                                                                               
EX54N    PACK  DUB,WORK(8)                                                      
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,NUM                                                        
         OI    NUM,X'80'           HOW WE KNOW IT IS NEW STYLE                  
         XC    NUM(4),=4X'FF'                                                   
         B     EX54G                                                            
*                                                                               
EX56     BAS   RE,GETHEX           PUT VALIDATED HEX INPUT INTO WORK            
         GOTO1 CHEXIN,PARA,WORK,FULL,8                                          
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
*                                                                               
         XC    FULL,=4X'FF'                                                     
         TM    FULL,X'80'          NEW STYLE?                                   
         BNZ   EX56N               YES                                          
         SR    R1,R1                                                            
         ICM   R1,3,FULL           DATE PORTION                                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CALC(4),DUB                                                      
         ICM   R1,3,FULL+2                SEQUENCE PORTION                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CALC+4(4),DUB                                                    
         B     OK                                                               
*                                                                               
EX56N    NI    FULL,X'FF'-X'80'                                                 
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         AP    DUB,=P'04000000'                                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  CALC(8),DUB                                                      
         B     OK                                                               
*                                                                               
EX60     DS    0H                  TRPACK                                       
         BAS   RE,GETDATA                                                       
         MVC   PARA+4(3),=X'D9000A'                                             
*&&US*&& MVI   PARA+7,QTRPACK                                                   
*&&UK*&& MVI   PARA+7,X'FE'                                                     
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
                                                                                
         GOTO1 (RF),PARA,(C'P',WORK),DUB                                        
         BNE   ERR20                                                            
         GOTO1 CHEXOUT,PARA,DUB,CALC,8,=C'TOG'                                  
         B     OK                                                               
                                                                                
EX62     DS    0H                  TRUNPK                                       
         BAS   RE,GETHEX                                                        
         GOTO1 CHEXIN,PARA,WORK,DUB,16                                          
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
                                                                                
         MVC   PARA+4(3),=X'D9000A'                                             
*&&US*&& MVI   PARA+7,QTRPACK                                                   
*&&UK*&& MVI   PARA+7,X'FE'                                                     
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
                                                                                
         GOTO1 (RF),PARA,(C'U',DUB),CALC                                        
         BNE   ERR20                                                            
         B     OK                                                               
                                                                                
EX64     DS    0H                  RCPACK                                       
         BAS   RE,GETDATA                                                       
         MVC   PARA+4(3),=X'D9000A'                                             
*&&US*&& MVI   PARA+7,QRCPACK                                                   
*&&UK*&& MVI   PARA+7,X'BC'                                                     
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         L     RF,0(R1)                                                         
         GOTO1 (RF),PARA,(C'P',WORK),DUB                                        
         BNE   ERR7                                                             
         GOTO1 CHEXOUT,PARA,DUB,CALC,2,=C'TOG'                                  
         B     OK                                                               
                                                                                
EX66     DS    0H                  RCUNPK                                       
         BAS   RE,GETHEX                                                        
         GOTO1 CHEXIN,PARA,WORK,DUB,4                                           
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
                                                                                
         MVC   PARA+4(3),=X'D9000A'                                             
*&&US*&& MVI   PARA+7,QRCPACK                                                   
*&&UK*&& MVI   PARA+7,X'BC'                                                     
         GOTO1 CCALLOV,PARA,0                                                   
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         L     RF,0(R1)                                                         
         GOTO1 (RF),PARA,(C'U',DUB),CALC                                        
         BNE   ERR7                                                             
         B     OK                                                               
         EJECT                                                                  
* VALIDATION ROUTINES                                                           
*                                                                               
GETNUM   ST    RE,MYRE                                                          
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         LTR   R4,R4                                                            
         BZ    ERR1                                                             
         TM    4(R2),X'08'                                                      
         BNO   ERR2                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         ST    R1,NUM                                                           
         B     BACK                                                             
*                                                                               
GETCASH  ST    RE,MYRE                                                          
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         LTR   R4,R4                                                            
         BZ    ERR1                                                             
         GOTO1 CCASHVAL,PARA,8(R2),(R4)                                         
         CLI   PARA,X'FF'                                                       
         BE    ERR2                                                             
         MVC   CASH,PARA+4                                                      
         B     BACK                                                             
*                                                                               
GETHEX   ST    RE,MYRE                                                          
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         LTR   R4,R4                                                            
         BZ    ERR1                                                             
         TM    4(R2),X'02'                                                      
         BNO   ERR3                                                             
         MVC   WORK,8(R2)                                                       
         B     BACK                                                             
*                                                                               
GETDATE  ST    RE,MYRE                                                          
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         LTR   R4,R4                                                            
         BZ    ERR1                                                             
         GOTO1 ADATVAL,PARA,8(R2),(EXTDATE,DATE)                                
         OC    PARA(4),PARA                                                     
         BZ    ERR4                                                             
         B     BACK                                                             
*                                                                               
GETDATA  ST    RE,MYRE                                                          
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         LTR   R4,R4                                                            
         BZ    ERR1                                                             
         MVC   WORK,SPACES                                                      
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     BACK                                                             
         MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
GETAGY   ST    RE,MYRE                                                          
         L     R3,AUTL                                                          
         USING UTLD,R3                                                          
         OC    TUSER,TUSER         MUST BE CONNECTED...                         
         BZ    ERR12                                                            
         CLI   TOVSYS,X'02'        ...TO SPOT SYSTEM                            
         BNE   ERR12                                                            
*                                                                               
* READ AGY REC FM SPTFIL TO GET CTRY CODE                                       
*                                                                               
         MVC   PARA(1),TSYS                                                     
         MVC   PARA+1(3),=X'FFFFFF'                                             
         GOTO1 CSWITCH,PARA,,0                                                  
         CLI   4(R1),0                                                          
         BNE   ERR18                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),TAGY                                                    
         MVC   AGYIO(13),KEY                                                    
         GOTO1 CDATAMGR,PARA,=C'DMREAD',=C'SPTDIR',AGYIO,KEY                    
         CLI   8(R1),0                                                          
         BNE   ERR14                                                            
         GOTO1 CDATAMGR,PARA,=C'GETREC',=C'SPTFIL',KEY+14,AGYIO,AGYWORK         
         CLI   8(R1),0                                                          
         BNE   ERR14                                                            
         B     BACK                                                             
         DROP  R3                                                               
*                                                                               
BACK     L     RE,MYRE                                                          
         BR    RE                                                               
         EJECT                                                                  
* EXITS                                                                         
*                                                                               
ERR1     MVC   MSG(19),=C'MISSING INPUT FIELD'                                  
         B     ALLX                                                             
*                                                                               
ERR2     MVC   MSG(21),=C'INVALID NUMERIC FIELD'                                
         B     ALLX                                                             
*                                                                               
ERR3     MVC   MSG(25),=C'INVALID HEXADECIMAL FIELD'                            
         B     ALLX                                                             
*                                                                               
ERR4     MVC   MSG(12),=C'INVALID DATE'                                         
         B     ALLX                                                             
*                                                                               
ERR5     MVC   MSG(27),=C'NO VALID COMMAND RECOGNISED'                          
         FOUT  CALCOMH,=C'COMMANDS - MSPACK MSUNPK DTPACK DTUNPK DNPACK+        
                DNUNPK'                                                         
         FOUT  CALCM2H,=C'           CLPACK CLUNPK GETDAY ADDAY DATVAL +        
               BRD'                                                             
         FOUT  CALCM3H,=C'           INVERT NETWEEK TINVCON'                    
         FOUT  CALCM4H,=C'           SXPACK SXUNPK TRPACK TRUNPK RCPACK+        
                RCUNPK'                                                         
         FOUT  CALCM5H,=C'           CVB CVD ADD + SUB - DIV / MUL X'           
         FOUT  CALCM6H,=C'           GETJUL UNJ ORPACK ORUNPK'                  
         B     ALLX                                                             
*                                                                               
ERR7     MVC   MSG(13),=C'INVALID FIELD'                                        
         B     ALLX                                                             
*                                                                               
ERR8     MVC   MSG(15),=C'INVALID STATION'                                      
         LA    R2,CALCH                                                         
         B     ALLX                                                             
*                                                                               
ERR10    MVC   MSG(15),=C'INVALID INVOICE'                                      
         B     ALLX                                                             
*                                                                               
ERR12    MVC   MSG(32),=C'MUST BE CONNECTED TO SPOT SYSTEM'                     
         B     ALLX                                                             
*                                                                               
ERR14    MVC   MSG(27),=C'ERROR READING AGENCY RECORD'                          
         B     ALLX                                                             
*                                                                               
ERR16    MVC   MSG(13),=C'STAPACK ERROR'                                        
         B     ALLX                                                             
*                                                                               
ERR18    MVC   MSG(24),=C'UNABLE TO SWITCH TO SPOT'                             
         B     ALLX                                                             
ERR20    MVC   MSG(12),=C'TRPACK ERROR'                                         
         B     ALLX                                                             
*                                                                               
OK       OI    CALCH+6,X'80'                                                    
         OI    CALDH+6,X'80'                                                    
         MVC   MSG(29),=C'RESULT DISPLAYED - ENTER NEXT'                        
         LA    R2,CALSRVH                                                       
*                                                                               
ALLX     OI    6(R2),X'40'                                                      
         FOUT  CALMSGH,MSG,60                                                   
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALID SIXPACK CHARS                                                           
*                                                                               
ALPHA    DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                          
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DSECTS ETC                                                                    
*                                                                               
WRKD     DSECT                                                                  
DUB      DS    D                                                                
PARA     DS    6F                                                               
WORK     DS    CL32                                                             
RELO     DS    A                                                                
ADATCON  DS    A                                                                
ADATVAL  DS    A                                                                
ISODATE  DS    CL16                                                             
EXTDATE  DS    X                                                                
         DS    XL3                                                              
MSG      DS    CL60                                                             
SPACES   DS    CL60                                                             
BYTE     DS    C                                                                
YEAR     DS    X                                                                
HUTW     DS    X                                                                
NETW     DS    X                                                                
NUM      DS    F                                                                
MYRE     DS    F                                                                
CASH     DS    F                                                                
FULL     DS    F                                                                
DATE     DS    CL12                                                             
SRPARMS  DS    6F                                                               
PACKOF4B DS    PL4                                                              
AUTL     DS    A                   A(UTL)                                       
STAVBLK  DS    CL(STBLNQ)                                                       
KEY      DS    XL32                                                             
AGYIO    DS    XL1000                                                           
AGYWORK  DS    XL12                                                             
STAWORK  DS    XL32                STAPACK INTERFACE AREA                       
WRKX     EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
SRCALFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRCALFFD                                                       
         EJECT                                                                  
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRCAL00   11/09/18'                                      
         END                                                                    
