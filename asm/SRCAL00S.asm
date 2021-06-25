*          DATA SET SRCAL00S   AT LEVEL 005 AS OF 05/01/02                      
*PHASE T13900A,+0                                                               
*INCLUDE TINVCON                                                                
*INCLUDE SIXPACK                                                                
*INCLUDE SIXUNPK                                                                
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
         L     RA,SRPARMS+20                                                    
         USING T139FFD,RA                                                       
         L     R9,SRPARMS+12                                                    
         USING COMFACSD,R9                                                      
         L     R4,SRPARMS+8        R4=A(UTL)                                    
         ST    R4,AUTL                                                          
         EJECT                                                                  
*              LOOK FOR COMMANDS                                                
         SPACE 3                                                                
         LA    R2,CALBH                                                         
         IF    CALA(3),=,=C'MSPACK',EX2                                         
         IF    CALA(3),=,=C'MSUNPK',EX4                                         
         IF    CALA(3),=,=C'DTPACK',EX6                                         
         IF    CALA(3),=,=C'DTUNPK',EX8                                         
         IF    CALA(3),=,=C'CLPACK',EX10                                        
         IF    CALA(3),=,=C'CLUNPK',EX12                                        
         IF    CALA(3),=,=C'TINVCON',EX13                                       
         IF    CALA(3),=,=C'GETDAY',EX14                                        
         IF    CALA(4),=,=C'ADDAY ',EX16                                        
         IF    CALA(3),=,=C'CVB   ',EX18                                        
         IF    CALA(3),=,=C'CVD   ',EX20                                        
         IF    CALA(3),=,=C'ADD',EX26                                           
         IF    CALA(3),=,=C'SUB',EX28                                           
         IF    CALA(3),=,=C'MUL',EX30                                           
         IF    CALA(3),=,=C'DIV',EX32                                           
         IF    CALA(3),=,=C'INVERT',EX40                                        
         IF    CALA(3),=,=C'NETWEEK',EX42                                       
         IF    CALA(3),=,=C'SXPACK',EX44                                        
         IF    CALA(3),=,=C'SXUNPK',EX46                                        
         IF    CALA,=,C'+',EX26                                                 
         IF    CALA,=,C'-',EX28                                                 
         IF    CALA,=,C'X',OR,C'*',EX30                                         
         IF    CALA,=,C'/',EX32                                                 
         IF    CALA(3),=,=C'BRD',EX34                                           
         LA    R2,CALAH                                                         
         B     ERR5                                                             
         EJECT                                                                  
*              EXECUTE SPECIFIC COMMANDS                                        
         SPACE 3                                                                
*        MSPACK                                                                 
*                                                                               
EX2      BAS   RE,GETAGY                                                        
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
         MVI   STBMED,C'T'         MEDIA TV                                     
         CLI   CALC+4,C'/'                                                      
         BE    MSP10                                                            
         CLI   CALC+4,C' '                                                      
         BNH   MSP10                                                            
         CLI   CALC+4,C'L'         TEST LOW POWER TV                            
         BE    MSP10                                                            
         MVI   STBMED,C'R'         MEDIA RADIO                                  
         CLI   CALC+4,C'A'                                                      
         BE    MSP10                                                            
         CLI   CALC+4,C'F'                                                      
         BE    MSP10                                                            
         MVC   STBMED,CALC+4       ALL OTHER                                    
*                                                                               
MSP10    GOTO1 (RF),PARA,(R4)                                                   
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
         LA    RE,AGYIO                                                         
         MVC   STAPCTRY,AGYPROF+7-AGYHDR(RE)                                    
         LA    RE,STAVBLK                                                       
         MVC   STAPMED,STBMED-STABLKD(RE)  MEDIA ALREADY FIGURED ABOVE          
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
         B     OK                                                               
         SPACE 2                                                                
EX4      DS    0H                                                               
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
         MVI   STAPMED,C'T'                                                     
         CLC   CALD,SPACES                                                      
         BNE   *+10                                                             
         MVC   STAPMED,CALD                                                     
         LA    RE,AGYIO                                                         
         MVC   STAPCTRY,AGYPROF+7-AGYHDR(RE)                                    
         STCM  R9,15,STAPACOM                                                   
         MVC   STAPMKST,DUB        MKTSTA                                       
*                                                                               
         GOTO1 (RF),(R1)                                                        
         CLI   STAPERR,0                                                        
         BNE   ERR16                                                            
*                                                                               
         MVC   CALC(4),STAPQMKT     RETURN RESULT                               
         MVC   CALD(8),STAPQSTA                                                 
         DROP  R1                                                               
*                                                                               
         CLC   CALD+5(3),SPACES                                                 
         BNH   *+8                                                              
         MVI   CALD+4,C'/'                                                      
         B     OK                                                               
         SPACE 2                                                                
EX6      BAS   RE,GETDATE          DTPACK                                       
         GOTO1 CDATCON,PARA,DATE,(2,DUB)                                        
         GOTO1 CHEXOUT,PARA,DUB,CALC,2,=C'TOG'                                  
         B     OK                                                               
         SPACE 2                                                                
EX8      BAS   RE,GETHEX           DTUNPK                                       
         GOTO1 CHEXIN,PARA,WORK,DUB,4                                           
         OC    PARA+12(4),PARA+12                                               
         BZ    ERR3                                                             
         GOTO1 CDATCON,PARA,(2,DUB),(8,CALC)                                    
         B     OK                                                               
         SPACE 2                                                                
EX10     BAS   RE,GETDATA          CLPACK                                       
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
         B     OK                                                               
         SPACE 2                                                                
EX12     BAS   RE,GETHEX           CLUNPK                                       
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
         B     OK                                                               
         SPACE 2                                                                
EX13     BAS   RE,GETDATA          TINVCON                                      
         CLI   WORK,C'1'                                                        
         BL    EX13A                                                            
         CLI   5(R2),12                                                         
         BNE   ERR10                                                            
         PACK  DUB,WORK(12)                                                     
         LM    R0,R1,DUB                                                        
         SRDL  R0,4                                                             
         STCM  R0,3,WORK                                                        
         STCM  R1,15,WORK+2                                                     
         GOTO1 =V(TINVCON),PARA,WORK,DUB,CDATCON,RR=RELO                        
         CLI   0(R1),X'FF'                                                      
         BE    ERR10                                                            
         MVC   CALC(6),DUB                                                      
         B     OK                                                               
EX13A    CLI   5(R2),6                                                          
         BNE   ERR10                                                            
         XC    DUB,DUB                                                          
         GOTO1 =V(TINVCON),PARA,WORK,DUB,CDATCON,RR=RELO                        
         CLI   0(R1),X'FF'                                                      
         BE    ERR10                                                            
         LM    R0,R1,DUB                                                        
         SRDL  R0,12                                                            
         O     R1,=X'0000000C'                                                  
         STM   R0,R1,DUB                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  CALC(12),DUB                                                     
         B     OK                                                               
         SPACE 2                                                                
EX14     BAS   RE,GETDATE          GETDAY                                       
         GOTO1 CGETDAY,PARA,DATE,CALC                                           
         B     OK                                                               
         SPACE 2                                                                
EX16     BAS   RE,GETDATE          ADDAY                                        
         LA    R2,CALCH                                                         
         BAS   RE,GETCASH                                                       
         L     R4,CASH                                                          
         SRDA  R4,32                                                            
         D     R4,=F'100'                                                       
         GOTO1 CADDAY,PARA,DATE,DUB,(R5)                                        
         GOTO1 CDATCON,PARA,DUB,(8,CALD)                                        
         B     OK                                                               
         SPACE 2                                                                
EX18     BAS   RE,GETNUM           CVB                                          
         GOTO1 CHEXOUT,PARA,NUM,CALC,4,=C'TOG'                                  
         B     OK                                                               
         SPACE 2                                                                
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
         SPACE 2                                                                
EX22     EDIT  (1,DUB),(3,CALC),ALIGN=LEFT,DUB=PARA                             
         B     OK                                                               
         SPACE 2                                                                
EX24     EDIT  (3,DUB),(12,CALC),ALIGN=LEFT,DUB=PARA                            
         B     OK                                                               
         SPACE 2                                                                
EX25     EDIT  (4,DUB),(12,CALC),ALIGN=LEFT,DUB=PARA                            
         B     OK                                                               
         SPACE 2                                                                
EX26     BAS   RE,GETNUM           ADD                                          
         L     R5,NUM                                                           
         LA    R2,CALCH                                                         
         BAS   RE,GETNUM                                                        
         A     R5,NUM                                                           
         B     EXOUT                                                            
         SPACE 2                                                                
EX28     BAS   RE,GETNUM           SUBTRACT                                     
         L     R5,NUM                                                           
         LA    R2,CALCH                                                         
         BAS   RE,GETNUM                                                        
         S     R5,NUM                                                           
         B     EXOUT                                                            
         SPACE 2                                                                
EX30     BAS   RE,GETNUM           MULTIPLY                                     
         L     R5,NUM                                                           
         LA    R2,CALCH                                                         
         BAS   RE,GETNUM                                                        
         M     R4,NUM                                                           
         B     EXOUT                                                            
         SPACE 2                                                                
EX32     BAS   RE,GETNUM           DIVIDE                                       
         L     R5,NUM                                                           
         LA    R2,CALCH                                                         
         BAS   RE,GETNUM                                                        
         SR    R4,R4                                                            
         OC    NUM,NUM                                                          
         BZ    ERR7                                                             
         D     R4,NUM                                                           
         SPACE 2                                                                
EX34     CLI   5(R2),0             BRD                                          
         BE    ERR1                                                             
         GOTO1 CDATVAL,PARA,CALB,DATE   VALIDATE FOR M/D/Y                      
         OC    0(4,R1),0(R1)       IF IT'S VALID                                
         BNZ   EX35                IT'S NOT MONTH/YEAR                          
*                                                                               
         GOTO1 CDATVAL,PARA,(X'02',CALB),DATE    VALIDATE M/Y                   
         OC    0(4,R1),0(R1)       IF LENGTH=0 ==> ERROR                        
         BZ    ERR4                                                             
*                                                                               
EX35     MVC   DATE+4(2),=C'01'    MAKE DAY 01                                  
         GOTO1 CGETDAY,PARA,DATE,NUM                                            
         MVC   WORK(6),DATE        IN CASE NO CHANGES NEEDED                    
         XR    R6,R6                                                            
         IC    R6,0(R1)                                                         
         BCTR  R6,0                                                             
         LNR   R6,R6                                                            
         BZ    HERE                IF ZERO NO CHANGE NECESSARY                  
         GOTO1 CADDAY,PARA,DATE,WORK,(R6)    NEW FRST OF MONTH                  
HERE     GOTO1 CDATCON,PARA,WORK,(8,CALCOM)  POSITION IT FOR SCREEN             
         GOTO1 CDATCON,PARA,DATE,(3,DUB) CONV DATE TO YMD (BINARY)              
         XR    RE,RE                                                            
         IC    RE,DUB+1            INSERT MONTH IN REG                          
         LA    RE,1(RE)                                                         
         STC   RE,DUB+1                                                         
         CLI   DUB+1,13            IF MONTH LESS THAN 13 IT'S OKAY              
         BL    GOODYR                                                           
         MVI   DUB+1,1             ELSE CHANGE IT TO 1                          
         IC    RE,DUB              INSERT YEAR IN REG                           
         LA    RE,1(RE)            ADD 1 TO THE YEAR                            
         STC   RE,DUB                                                           
GOODYR   GOTO1 CDATCON,PARA,(3,DUB),WORK CONV TO YYMMDD FORM                    
         GOTO1 CGETDAY,PARA,WORK,NUM                                            
         XR    R6,R6                                                            
         IC    R6,0(R1)                                                         
         LNR   R6,R6                                                            
         GOTO1 CADDAY,PARA,WORK,WORK+6,(R6)                                     
         MVI   CALCOM+9,C'-'                                                    
         MVC   CALCOM+20(17),=C'(BROADCAST MONTH)'                              
         GOTO1 CDATCON,PARA,WORK+6,(8,CALCOM+11)                                
         FOUT  CALCOMH             TRANSMIT                                     
         SPACE 2                                                                
**************** CALCULATE BROADCAST YEAR *****************************         
         MVC   DATE+2(2),=C'01'              CHANGE MONTH TO JAN                
         GOTO1 CGETDAY,PARA,DATE,NUM         GET DATE FOR YY0101                
         XR    R6,R6                                                            
         IC    R6,0(R1)                                                         
         MVC   WORK(6),DATE        SAVE DATE IF YY0101 IS A MONDAY              
         BCTR  R6,0                                                             
         LNR   R6,R6                                                            
         BZ    NOCHG               IF 0 YY0101 IS A MONDAY                      
         GOTO1 CADDAY,PARA,DATE,WORK,(R6)                                       
NOCHG    MVC   CALCM2,SPACES       CLEAR MESSAGE AREA                           
         GOTO1 CDATCON,PARA,WORK,(8,CALCM2) CONV TO MMMDD/YY                    
         GOTO1 (RF),(R1),DATE,(3,DUB)                                           
         IC    RE,DUB                                                           
         LA    RE,1(RE)            ADD 1 TO YEAR                                
         STC   RE,DUB                                                           
         GOTO1 (RF),(R1),(3,DUB),DATE                                           
         GOTO1 CGETDAY,PARA,DATE,NUM              GET DAY NUMBER                
         XR    R6,R6                                                            
         IC    R6,0(R1)                                                         
         LNR   R6,R6               MAKE IT NEGATIVE FOR ADDAY                   
         GOTO1 CADDAY,PARA,DATE,WORK,(R6)         SUBT IT FOR SUN.              
         MVI   CALCM2+9,C'-'                                                    
         MVC   CALCM2+20(16),=C'(BROADCAST YEAR)'                               
         GOTO1 CDATCON,PARA,WORK,(8,CALCM2+11)    CONV TO MMMDD/YY              
         FOUT  CALCM2H             TRANSMIT                                     
         B     OK                                                               
         SPACE 2                                                                
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
         SPACE 1                                                                
EX41     B     OK                  EXIT FROM INVERT                             
         SPACE 2                                                                
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
         SPACE 2                                                                
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
         SPACE 2                                                                
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
*              VALIDATION ROUTINES                                              
         SPACE 3                                                                
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
         SPACE 2                                                                
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
         SPACE 2                                                                
GETHEX   ST    RE,MYRE                                                          
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         LTR   R4,R4                                                            
         BZ    ERR1                                                             
         TM    4(R2),X'02'                                                      
         BNO   ERR3                                                             
         MVC   WORK,8(R2)                                                       
         B     BACK                                                             
         SPACE 2                                                                
GETDATE  ST    RE,MYRE                                                          
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         LTR   R4,R4                                                            
         BZ    ERR1                                                             
         GOTO1 CDATVAL,PARA,8(R2),DATE                                          
         OC    PARA(4),PARA                                                     
         BZ    ERR4                                                             
         B     BACK                                                             
         SPACE 2                                                                
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
         SPACE 2                                                                
BACK     L     RE,MYRE                                                          
         BR    RE                                                               
         EJECT                                                                  
*              EXITS                                                            
         SPACE 2                                                                
ERR1     MVC   MSG(19),=C'MISSING INPUT FIELD'                                  
         B     ALLX                                                             
         SPACE 2                                                                
ERR2     MVC   MSG(21),=C'INVALID NUMERIC FIELD'                                
         B     ALLX                                                             
         SPACE 2                                                                
ERR3     MVC   MSG(25),=C'INVALID HEXADECIMAL FIELD'                            
         B     ALLX                                                             
         SPACE 2                                                                
ERR4     MVC   MSG(12),=C'INVALID DATE'                                         
         B     ALLX                                                             
         SPACE 2                                                                
ERR5     MVC   MSG(27),=C'NO VALID COMMAND RECOGNISED'                          
         FOUT  CALCOMH,=C'COMMANDS - MSPACK MSUNPK DTPACK DTUNPK BRD'           
         FOUT  CALCM2H,=C'           CLPACK CLUNPK GETDAY ADDAY'                
         FOUT  CALCM3H,=C'           INVERT NETWEEK TINVCON'                    
         FOUT  CALCM4H,=C'           SXPACK SXUNPK'                             
         FOUT  CALCM5H,=C'           CVB CVD ADD + SUB - DIV / MUL X'           
         B     ALLX                                                             
         SPACE 2                                                                
ERR7     MVC   MSG(13),=C'INVALID FIELD'                                        
         B     ALLX                                                             
         SPACE 2                                                                
ERR8     MVC   MSG(15),=C'INVALID STATION'                                      
         LA    R2,CALCH                                                         
         B     ALLX                                                             
         SPACE 2                                                                
ERR10    MVC   MSG(15),=C'INVALID INVOICE'                                      
         B     ALLX                                                             
         SPACE 2                                                                
ERR12    MVC   MSG(32),=C'MUST BE CONNECTED TO SPOT SYSTEM'                     
         B     ALLX                                                             
         SPACE 2                                                                
ERR14    MVC   MSG(27),=C'ERROR READING AGENCY RECORD'                          
         B     ALLX                                                             
         SPACE 2                                                                
ERR16    MVC   MSG(13),=C'STAPACK ERROR'                                        
         B     ALLX                                                             
         SPACE 2                                                                
ERR18    MVC   MSG(24),=C'UNABLE TO SWITCH TO SPOT'                             
         B     ALLX                                                             
         SPACE 2                                                                
OK       OI    CALCH+6,X'80'                                                    
         OI    CALDH+6,X'80'                                                    
         MVC   MSG(29),=C'RESULT DISPLAYED - ENTER NEXT'                        
         LA    R2,CALSRVH                                                       
         SPACE 2                                                                
ALLX     OI    6(R2),X'40'                                                      
         FOUT  CALMSGH,MSG,60                                                   
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* VALID SIXPACK CHARS                                                           
         SPACE                                                                  
ALPHA    DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                          
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ETC                                                       
         SPACE 3                                                                
WRKD     DSECT                                                                  
DUB      DS    D                                                                
PARA     DS    6F                                                               
WORK     DS    CL32                                                             
RELO     DS    A                                                                
MSG      DS    CL60                                                             
SPACES   DS    CL60                                                             
BYTE     DS    C                                                                
YEAR     DS    X                                                                
HUTW     DS    X                                                                
NETW     DS    X                                                                
NUM      DS    F                                                                
MYRE     DS    F                                                                
CASH     DS    F                                                                
DATE     DS    CL6                                                              
SRPARMS  DS    6F                                                               
AUTL     DS    A                   A(UTL)                                       
STAVBLK  DS    CL(STBLNQ)                                                       
KEY      DS    XL32                                                             
AGYIO    DS    XL1000                                                           
AGYWORK  DS    XL12                                                             
STAWORK  DS    XL32                STAPACK INTERFACE AREA                       
WRKX     EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
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
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SRCAL00S  05/01/02'                                      
         END                                                                    
