*          DATA SET PPREP8102  AT LEVEL 148 AS OF 05/01/02                      
*PHASE PP8102A,+0                                                               
*                                                                               
************  CHANGE LOG  ************                                          
*                                                                               
*  SMYE  12/13/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
         TITLE 'PP8102 - PRINTPAK USER REPORT'                                  
*                                                                               
*        USER REPORT                                                            
*                                                                               
*        QOPT1 D= SHOW DUPLICATES ONLY                                          
*                                                                               
PP8102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP8102,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP81WRKD,R8                                                      
         SPACE 2                                                                
         CLI   MODE,UNSRTREC                                                    
         BNE   EXIT                                                             
         MVC   SAVPARS,DMCB+4                                                   
         LM    R2,R3,SAVPARS                                                    
         B     TLBRTAB(R2)                                                      
*                                                                               
TLBRTAB  B     TLFIRST                                                          
         B     TLINPUT                                                          
         B     TLOUTPUT                                                         
         B     TLLAST                                                           
         SPACE 3                                                                
TLFIRST  DS    0H                                                               
*                                                                               
*                                                                               
         MVI   RC2DSECT,C'Y'                                                    
         L     R0,=A(TLIN)                                                      
         A     R0,RELO                                                          
         ST    R0,ATLIN                                                         
         L     R0,=A(TLOUT)                                                     
         A     R0,RELO                                                          
         ST    R0,ATLOUT                                                        
         MVC   P,SPACES                                                         
         MVI   PBNAMX,C' '                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    OLDKEY,OLDKEY                                                    
         MVI   DUPSW,0                                                          
         XC    DIVSD,DIVSD                                                      
         XC    OLDJOB,OLDJOB                                                    
         XC    OLDDIV,OLDDIV                                                    
*                                                                               
TLF6     DS    0H                                                               
         MVI   RUNSW,0                                                          
         XC    KEY,KEY                                                          
         XC    PBUYREC(256),PBUYREC                                             
         XC    SRTLIN,SRTLIN                                                    
*                                                                               
*        BUILD RPTTAB - REPORT TABLE                                            
*                                                                               
*        3 BYTE ENTRIES   CODE (1), SORT LENGTH,DISPLAY LENGHT                  
*        SORT LEN =X'FF' IF NOT SORTING ON THIS FIELD                           
*        DISP LEN =X'FF' IF NOT DISPLAYING THIS FIELD                           
*        IF DISP LEN HAS HIGH ORDER BIT ON (X'80')                              
*        FIELD IS DISPLAYED ONLY IN HEADS                                       
*                                                                               
*                                                                               
         XC    RPTTAB,RPTTAB                                                    
         LA    R1,RPTTAB                                                        
         CLC   QDIV,SPACES         SEE IF DOING DIVISIONS                       
         BE    TLF7                NO                                           
         MVC   RPTTAB(3),DROCKW                                                 
         LA    R1,RPTTAB+3                                                      
TLF7     MVC   0(32,R1),ROCKW      10 X 3 = 30 + 2 = 32                         
         XC    HALF,HALF                                                        
         LA    R1,RPTTAB                                                        
TLF8     CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    TLF10                                                            
         CLI   0(R1),X'03'         CHK FOR PRD                                  
         BNE   TLF8C                                                            
         CLI   QOPT1,C'D'          SEE  IF DOING DUPLICATES ONLY                
         BNE   TLF8C               NO                                           
         MVI   1(R1),X'FF'         YES - DON'T SORT ON PRODUCT                  
TLF8C    CLI   1(R1),X'FF'         SEE IF SORTING ON THIS FIELD                 
         BE    TLF9                NO SKIP                                      
         ZIC   R0,1(R1)                                                         
         LH    R2,HALF                                                          
         AR    R2,R0                                                            
         STH   R2,HALF                                                          
*                                                                               
TLF9     LA    R1,3(R1)                                                         
         B     TLF8                                                             
*                                                                               
TLF10    LH    R0,HALF                                                          
         AH    R0,=H'2'            INCREASE SORT LEN BY 2                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAVPARS+6(2),DUB    KEY LENGHT                                   
         AH    R0,=H'4'            FOR DISK ADDR                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAVPARS+4(2),DUB    SORT RECORD LENGHT                           
*                                                                               
*              BUILD HEADLINES                                                  
*              AND SAVE IN SAVHD7,8,9                                           
TLH      DS    0H                                                               
         MVC   SAVHD7,SPACES                                                    
         MVC   SAVHD8,SPACES                                                    
         MVC   SAVHD9,SPACES                                                    
         XC    DDISP,DDISP         DISPLACEMENT INTO P                          
*                                  CHK RPTTAB TO SEE IF DISPLAY FIELD           
         LA    R4,RPTTAB                                                        
TLH5     CLI   0(R4),X'FF'       END OF TABLE                                   
         BE    TLHX                                                             
         CLI   2(R4),X'FF'                                                      
         BE    TLH40               NOT DISPLAYING THIS FIELD                    
         LA    R5,HEADTAB                                                       
TLH10    CLI   0(R5),X'FF'         END OF DISPLAY TABLE                         
         BNE   *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         CLC   0(1,R4),0(R5)                                                    
         BE    TLH15                                                            
         LA    R5,5(R5)                                                         
         B     TLH10               NEXT ENTRY                                   
*                                                                               
TLH15    DS    0H                                                               
         MVC   FULL,1(R5)                                                       
         L     RF,FULL                                                          
         BAS   RE,0(RF)                                                         
*                                                                               
TLH40    LA    R4,3(R4)            NEXT ENTRY IN RPTTAB                         
         B     TLH5                                                             
*                                                                               
TLHX     B     EXIT                                                             
*                                                                               
HEADTAB  DC    AL1(01),AL4(HD01)     PUB NUMBER      15                         
         DC    AL1(02),AL4(HD02)     PUB NAME        20                         
         DC    AL1(03),AL4(HD03)     PRODUCT CODE    3                          
         DC    AL1(04),AL4(HD04)     CLOSING DATE    8                          
         DC    AL1(05),AL4(HD05)     INSERTION DATE 12 P1,P2                    
         DC    AL1(06),AL4(HD06)     ON-SALE DATE   8                           
         DC    AL1(07),AL4(HD07)     SPACE DESCRIPTION 17 P1,P2                 
         DC    AL1(08),AL4(HD08)     PRODUCT NAME   20                          
         DC    AL1(09),AL4(HD09)     COPY NUMBER    17                          
         DC    AL1(10),AL4(HD10)     DIVISION       3                           
         DC    AL1(11),AL4(HD11)     ESTIMATE       3                           
         DC    AL1(12),AL4(HD12)     PAYABLE DATE  8                            
         DC    AL1(13),AL4(HD13)     BILLABLE DATE  8                           
         DC    AL1(14),AL4(HD14)     AD CODE 6                                  
         DC    AL1(20),AL4(HD20)     ORDERED GROSS 14                           
         DC    AL1(21),AL4(HD21)     ORDERED NET   14                           
         DC    AL1(22),AL4(HD22)     ORDERED GR-CD 14                           
         DC    AL1(23),AL4(HD23)     ORDERED NET-CD 14                          
         DC    AL1(24),AL4(HD24)     ORDERED AGY COM                            
         DC    AL1(25),AL4(HD25)     ORDERED CD                                 
         DC    AL1(26),AL4(HD26)     PAID GROSS 14                              
         DC    AL1(27),AL4(HD27)     PAID NET 14                                
         DC    AL1(28),AL4(HD28)     PAID GR-CD 14                              
         DC    AL1(29),AL4(HD29)     PAID NET-CD 14                             
         DC    AL1(30),AL4(HD30)     PAID AGY COM  14                           
         DC    AL1(31),AL4(HD31)     PAID CD 14                                 
         DC    AL1(32),AL4(HD32)     BILLED GROSS 14                            
         DC    AL1(33),AL4(HD33)     BILLED NET 14                              
         DC    AL1(34),AL4(HD34)     BILLED GR-CD 14                            
         DC    AL1(35),AL4(HD35)     BILLED NET-CD 14                           
         DC    AL1(36),AL4(HD36)     BILLED AGYCOM 14                           
         DC    AL1(37),AL4(HD37)     BILLED CD 14                               
         DC    X'FFFF'             END OF TABLE                                 
*                                                                               
HD01     DS    0H                                                               
         LA    R5,SAVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(08,R5),=C'PUB CODE'                                            
         MVC   132(08,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD02     DS    0H                                                               
         LA    R5,SAVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(08,R5),=C'PUB NAME'                                            
         MVC   132(08,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD03     DS    0H                                                               
         LA    R5,SAVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(03,R5),=C'PRD'                                                 
         MVC   132(03,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD04     DS    0H                                                               
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(07,R5),=C'CLOSING'                                             
         MVC   132(07,R5),=C' DATE  '                                           
         MVC   264(07,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD05     DS    0H                                                               
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(09,R5),=C'INSERTION'                                           
         MVC   132(09,R5),=C'  DATE   '                                         
         MVC   264(09,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD06     DS    0H                                                               
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(07,R5),=C'ON-SALE'                                             
         MVC   132(07,R5),=C' DATE  '                                           
         MVC   264(07,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD07     DS    0H                                                               
         LA    R5,SAVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(17,R5),=C'SPACE DESCRIPTION'                                   
         MVC   132(17,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD08     DS    0H                                                               
         LA    R5,SAVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(08,R5),=C'PRD NAME'                                            
         MVC   132(08,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
*                                                                               
HD09     DS    0H                                                               
         LA    R5,SAVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(11,R5),=C'COPY NUMBER'                                         
         MVC   132(11,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD10     DS    0H                                                               
         TM    2(R4),X'80'         SEE IF IN HEADLINES                          
         BNZ   0(RE)               RETURN                                       
         LA    R5,SAVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(03,R5),=C'DIV'                                                 
         MVC   132(03,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD11     DS    0H                                                               
         LA    R5,SAVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(03,R5),=C'EST'                                                 
         MVC   132(03,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD12     DS    0H                                                               
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(07,R5),=C'PAYABLE'                                             
         MVC   132(07,R5),=C' DATE  '                                           
         MVC   264(07,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD13     DS    0H                                                               
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   0(08,R5),=C'BILLABLE'                                            
         MVC   132(08,R5),=C'  DATE  '                                          
         MVC   264(08,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
*                                                                               
HD14     DS    0H                                                               
         LA    R5,SAVHD8                                                        
         AH    R5,DDISP                                                         
         MVC   0(07,R5),=C'AD CODE'                                             
         MVC   132(07,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD20     DS    0H                  GROSS ORDERED                                
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   8(5,R5),=C'GROSS'                                                
         B     HD25X                                                            
*                                                                               
HD21     DS    0H                  NET ORDERED                                  
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   9(3,R5),=C'NET'                                                  
         B     HD25X                                                            
*                                                                               
HD22     DS    0H                  GROSS-CD ORDERED                             
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   6(8,R5),=C'GROSS-CD'                                             
         B     HD25X                                                            
*                                                                               
HD23     DS    0H                  NET-CD ORDERED                               
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   8(6,R5),=C'NET-CD'                                               
         B     HD25X                                                            
*                                                                               
HD24     DS    0H                  AGYCOM ORDERED                               
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   7(7,R5),=C'AGY COM'                                              
         B     HD25X                                                            
*                                                                               
HD25     DS    0H                  CD ORDERED                                   
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   9(2,R5),=C'CD'                                                   
HD25X    MVC   139(07,R5),=C'ORDERED'                                           
         MVC   271(07,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
*                                                                               
HD26     DS    0H                  GROSS PAID                                   
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   9(5,R5),=C'GROSS'                                                
         B     HD31X                                                            
*                                                                               
HD27     DS    0H                  NET PAID                                     
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   11(3,R5),=C'NET'                                                 
         B     HD31X                                                            
*                                                                               
HD28     DS    0H                  GROSS-CD PAID                                
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   6(8,R5),=C'GROSS-CD'                                             
         B     HD31X                                                            
*                                                                               
HD29     DS    0H                  NET-CD PAID                                  
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   8(6,R5),=C'NET-CD'                                               
         B     HD31X                                                            
*                                                                               
HD30     DS    0H                  AGYCOM PAID                                  
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   7(7,R5),=C'AGY COM'                                              
         B     HD31X                                                            
*                                                                               
HD31     DS    0H                  CD PAID                                      
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   11(2,R5),=C'CD'                                                  
HD31X    MVC   142(04,R5),=C'PAID'                                              
         MVC   274(04,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
HD32     DS    0H                  GROSS BILLED                                 
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   9(5,R5),=C'GROSS'                                                
         B     HD37X                                                            
*                                                                               
HD33     DS    0H                  NET BILLED                                   
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   10(3,R5),=C'NET'                                                 
         B     HD37X                                                            
*                                                                               
HD34     DS    0H                  GROSS-CD BILLED                              
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   6(8,R5),=C'GROSS-CD'                                             
         B     HD37X                                                            
*                                                                               
HD35     DS    0H                  NET-CD BILLED                                
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   8(6,R5),=C'NET-CD'                                               
         B     HD37X                                                            
*                                                                               
HD36     DS    0H                  AGYCOM BILLED                                
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   7(7,R5),=C'AGY COM'                                              
         B     HD31X                                                            
*                                                                               
HD37     DS    0H                  CD BILLED                                    
         LA    R5,SAVHD7                                                        
         AH    R5,DDISP                                                         
         MVC   10(2,R5),=C'CD'                                                  
HD37X    MVC   140(06,R5),=C'BILLED'                                            
         MVC   272(06,R5),DASHES                                                
         B     HD0XX                                                            
*                                                                               
*                                                                               
HD0XX    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         BR    RE                                                               
*                                                                               
         SPACE 3                                                                
TLINPUT  DS    0H                                                               
         GOTO1 ATLIN,DMCB,(RA)                                                  
         B     EXIT                                                             
         SPACE 3                                                                
TLOUTPUT DS    0H                                                               
         GOTO1 ATLOUT,DMCB,(RA)                                                 
         B     EXIT                                                             
         SPACE 3                                                                
TLLAST   DS    0H                                                               
         MVI   PBUYREC,X'FF'                                                    
         MVC   PBUYREC+1(24),PBUYREC                                            
         GOTO1 ATLOUT,DMCB,(RA)                                                 
         B     EXIT                                                             
EXIT     DS    0H                                                               
         MVC   DMCB+4(8),SAVPARS                                                
XIT      DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
**                                                                              
*              DEFAULT VALUES FOR ROCKWELL                                      
*                                                                               
DROCKW   DC    X'0A0383'           DIV                                          
ROCKW    DC    X'01FF10'           PUB                                          
         DC    X'021415'           PUB NAME                                     
         DC    X'030304'           PRD                                          
         DC    X'08FF15'           PRD NAME                                     
         DC    X'07FF12'           SPACE DESC                                   
         DC    X'060309'           ON-SALE DATE                                 
         DC    X'05050D'           INSERTION DATE                               
         DC    X'04FF09'           CLOSING DATE                                 
         DC    X'0BFF04'           ESTIMATE                                     
         DC    X'09FF12'           COPY NUMBER                                  
         DC    X'FFFF'                                                          
*                                                                               
         EJECT                                                                  
*                                  INPUT                                        
TLIN     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TLIN                                                           
*                                                                               
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP81WRKD,R8                                                      
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   TLIN10                                                           
*                                       FIRST TIME                              
         XC    BSTART,BSTART                                                    
         MVC   BEND,=3X'FF'                                                     
         CLC   QSTART(2),=C'ES'                                                 
         BNE   TLIN1                                                            
         MVC   QSTART(2),SPACES    RESET TO SPACES                              
         CLC   QEST,=C'ALL'                                                     
         BE    TLIN1                                                            
         CLC   QEST,SPACES                                                      
         BE    TLIN1                                                            
*                                  MUST TRY TO READ EST FOR DATES               
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(3),QCLIENT                                                 
         MVC   KEY+7(3),QPRODUCT                                                
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   KEY+10(2),HALF                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   TLIN1               EST NOT READABLE OR NOT FOUND                
*                                  LEAVE QSTART BLANK                           
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   QSTART(12),PESTST   GET DATES FROM ESTIMATE                      
TLIN1    CLI   QSTART,C' '                                                      
         BE    TLIN2                                                            
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*                                                                               
TLIN2    DS    0H                                                               
         CLI   QEND,C' '                                                        
         BE    TLIN4                                                            
*        GOTO1 DTCNV,DMCB,QEND,(1,BEND)                                         
         GOTO1 DATCON,DMCB,(0,QEND),(3,BEND)                                    
*                                                                               
TLIN4    DS    0H                                                               
*                                                                               
         XC    KEY,KEY             RECLEAR KEY - MAY HAVE EST                   
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'20'                                                      
         OC    KEY+3(1),RUNSW                                                   
         CLC   QCLIENT,SPACES                                                   
         BE    TLIN6                                                            
         CLC   QCLIENT,=C'ALL'                                                  
         BE    TLIN6                                                            
         CLI   QCLIENT,C'*'                                                     
         BE    TLIN6                                                            
         MVC   KEY+4(3),QCLIENT                                                 
*                                                                               
TLIN5    DS    0H                                                               
         CLC   QPRODUCT,SPACES                                                  
         BE    TLIN6                                                            
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    TLIN6                                                            
         LA    R1,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+13                                                        
         MVC   0(3,R1),QPRODUCT                                                 
*                                                                               
TLIN6    DS    0H                                                               
         XC    BPUB,BPUB                                                        
         CLI   QPUB,C'0'                                                        
         BL    TLIN8                                                            
         GOTO1 PUBVAL,DMCB,(0,QPUB),BPUB                                        
*                                                                               
         LA    R1,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+7                                                         
         MVC   0(6,R1),BPUB                                                     
*                                                                               
TLIN8    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     TLIN10A                                                          
*                                                                               
TLIN10   DS    0H                                                               
         GOTO1 SEQ                                                              
TLIN10A  DS    0H                                                               
         CLI   KEY+25,X'FF'                                                     
         BE    TLIN10                                                           
         CLC   KEY(4),KEYSAVE      A/M/R                                        
         BNE   TLIN90                                                           
         CLC   QCLIENT,SPACES                                                   
         BE    TLIN12                                                           
         CLC   QCLIENT,=C'ALL'                                                  
         BE    TLIN12                                                           
         CLI   QCLIENT,C'*'        TEST OFFICE REQ                              
         BNE   TLIN10C             NO                                           
         CLC   KEY(3),PCLTKEY                                                   
         BNE   *+14                                                             
         CLC   KEY+4(3),PCLTKCLT                                                
         BE    TLIN12                                                           
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETCLT                                                       
         CLC   QCLIENT+1(2),PCLTOFF                                             
         BE    TLIN12                                                           
         IC    RF,KEY+6                                                         
         LA    RF,1(RF)                                                         
         STC   RF,KEY+6                                                         
         XC    KEY+7(18),KEY+7                                                  
         B     TLIN5                                                            
TLIN10C  DS    0H                                                               
         CLC   QCLIENT,KEY+4                                                    
         BNE   TLIN90                                                           
*                                                                               
         CLC   QPRODUCT,SPACES                                                  
         BE    TLIN12                                                           
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    TLIN12                                                           
         LA    R1,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+13                                                        
         CLC   QPRODUCT,0(R1)                                                   
         BNE   TLIN90                                                           
*                                                                               
TLIN12   DS    0H                                                               
         OC    KEY+21(3),KEY+21    PASSIVE POINTER                              
         BNZ   TLIN10                                                           
         CLC   QDIV,SPACES                                                      
         BE    TLIN13                                                           
         CLC   QDIV,=C'ALL'                                                     
         BE    TLIN13                                                           
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETPRD          MUST READ PRDOUCT TO CHK DIV                 
         CLC   QDIV,PPRDDIV                                                     
         BNE   TLIN10              NO MATCH - SKIP                              
*                                                                               
TLIN13   LA    R1,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+13                                                        
*                                                                               
         CLI   0(R1),C'*'           SKIP OTHER AGY DATA                         
         BE    TLIN10                                                           
*                                                                               
         OC    BPUB,BPUB                                                        
         BZ    TLIN14                                                           
         LA    R1,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+7                                                         
         CLC   BPUB,0(R1)                                                       
         BNE   TLIN10                                                           
*                                                                               
TLIN14   DS    0H                                                               
         CLC   QEST,=C'ALL'                                                     
         BE    TLIN15                                                           
         CLC   QEST,SPACES                                                      
         BE    TLIN15                                                           
         MVC   HALF,KEY+19                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         CLC   WORK(3),QEST                                                     
         BE    TLIN15                                                           
         BL    TLIN10                                                           
         CLC   QESTEND,SPACES                                                   
         BE    TLIN10                                                           
         CLC   WORK(3),QESTEND                                                  
         BH    TLIN10                                                           
*                                                                               
TLIN15   DS    0H                                                               
         CLI   QBPDATE,C' '                                                     
         BNE   TLIN16                                                           
         CLC   BSTART,KEY+16                                                    
         BH    TLIN10                                                           
         CLC   BEND,KEY+16                                                      
         BL    TLIN10                                                           
         CLI   RUNSW,0                                                          
         BNE   TLIN16                                                           
         CLC   QJOB,SPACES                                                      
         BNE   TLIN16                                                           
         CLI   QOPT1,C' '                                                       
         BNE   TLIN16                                                           
         TM    PBUYCNTL,X'80'                                                   
         BNZ   TLIN16                                                           
*                                  DONT NEED REC NOW                            
         B     TLIN20                                                           
*                                                                               
TLIN16   DS    0H                                                               
         MVI   CHGSW,0                                                          
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         CLC   QJOB,SPACES                                                      
         BE    TLIN18                                                           
         CLC   QJOB,=C'ALL   '                                                  
         BE    TLIN18                                                           
         CLC   QJOB,PBDJOB                                                      
         BE    TLIN18                                                           
         CLC   =C'NONE',QJOB                                                    
         BNE   TLIN10                                                           
         OC    PBDJOB,PBDJOB                                                    
         BNZ   TLIN10                                                           
*                                                                               
TLIN18   DS    0H                                                               
         CLI   QBPDATE,C' '                                                     
         BE    TLIN18B                                                          
         LA    R2,PBDCDATE                                                      
         CLI   QBPDATE,C'C'                                                     
         BE    TLIN18A                                                          
         LA    R2,PBDBDATE                                                      
         CLI   QBPDATE,C'B'                                                     
         BE    TLIN18A                                                          
         LA    R2,PBDPDATE                                                      
         CLI   QBPDATE,C'P'                                                     
         BE    TLIN18A                                                          
         LA    R2,PBDSDATE                                                      
         CLI   QBPDATE,C'S'                                                     
         BE    TLIN18A                                                          
         B     TLIN18A1                                                         
TLIN18A  DS    0H                                                               
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
TLIN18A1 DS    0H                                                               
         LA    R2,PBUYKDAT         OR USE DATE                                  
         CLC   BSTART,0(R2)                                                     
         BH    TLIN10                                                           
         CLC   BEND,0(R2)                                                       
         BL    TLIN10                                                           
         B     TLIN18B                                                          
*                                                                               
*                                                                               
TLIN18B  DS    0H                                                               
TLIN18E  DS    0H                                                               
*                                                                               
TLIN19   DS    0H                                                               
*                                                                               
TLIN20   DS    0H                                                               
         CLI   RUNSW,0                                                          
         BNE   TLINX               NO SORT                                      
         XC    SORTREC,SORTREC                                                  
         XC    SDISP,SDISP                                                      
*                               CHK RPTTAB TO SEE IF SORTING ON FIELD           
         LA    R4,RPTTAB                                                        
TLIN25   CLI   0(R4),X'FF'       END OF TABLE                                   
         BE    TLIN60                                                           
         CLI   1(R4),X'FF'                                                      
         BE    TLIN40              NOT SORTING ON THIS FLD SKIP                 
         LA    R5,SORTTAB                                                       
TLIN30   CLI   0(R5),X'FF'        END OF SORTTAB                                
         BNE   *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         CLC   0(1,R4),0(R5)                                                    
         BE    TLIN35                                                           
         LA    R5,5(R5)                                                         
         B     TLIN30              NEXT ENTRY                                   
*                                                                               
TLIN35   MVC   FULL,1(R5)                                                       
         L     RF,FULL                                                          
         BAS   RE,0(RF)                                                         
*                                                                               
TLIN40   LA    R4,3(R4)            NEXT ENTRY IN RPTTAB                         
         B     TLIN25                                                           
**                                                                              
ST01     DS    0H                  PUB SORT                                     
         LA    R1,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+7                                                         
         LA    R2,SORTREC                                                       
         AH    R2,SDISP            ADD PREVIOUS DISPLACEMENT                    
         ZIC   R3,1(R4)            SORT LENGHT                                  
         LH    R0,SDISP                                                         
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R2),0(R1)                                                    
*                                                                               
         DS    F                                                                
ST02     DS    0H                  PUB SORT                                     
         ST    RE,ST02-4           SAVE RETURN REGISTER                         
         LA    R1,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+7                                                         
         CLC   OLDPUB,0(R1)                                                     
         BE    ST02H                                                            
         XC    SRTLIN,SRTLIN       ON CHG OF PUB CLEAR SORT LINE NUMBER         
         MVC   OLDPUB,0(R1)                                                     
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),OLDPUB                                                  
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         CLC   PUBKEY(7),KEY                                                    
         BE    ST02E                                                            
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ST02D                                                            
         CLI   PAGYPROF+16,C'0'    TEST DEFAULT                                 
         BNE   *+8                                                              
         BAS   RE,INOPUB                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+8                                                              
         BAS   RE,INOPUB                                                        
         MVC   KEY+7(2),=C'ZZ'                                                  
         MVI   KEY+9,X'81'                                                      
         CLC   KEY(9),KEYSAVE                                                   
         BE    ST02D               HAVE SRDS                                    
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ST02D                                                            
INOPUB   DC    H'0'                PUB NOT FOUND                                
*                                                                               
ST02D    DS    0H                                                               
         BAS   RE,IGETP                                                         
ST02E    DS    0H                                                               
         MVC   KEY(64),SAVKEYS                                                  
ST02H    DS    0H                                                               
         MVC   SKPUB(13),PUBNAME                                                
         CLC   SKPUB(4),=C'THE '                                                
         BNE   *+10                                                             
         MVC   SKPUB(13),PUBNAME+4                                              
         MVC   SKPUB+13(6),PUBKPUB                                              
*                                                                               
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     ST02X                                                            
         MVC   0(0,R1),SKPUB                                                    
ST02X    L     RE,ST02-4                                                        
         BR    RE                                                               
*                                                                               
ST03     DS    0H                  PRODUCT CODE                                 
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LA    R2,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R2,KEY+13                                                        
         ZIC   R3,1(R4)            SORT LENGHT                                  
         LH    R0,SDISP                                                         
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),0(R2)       EXECUTED                                     
*                                                                               
ST04     LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDCDATE    EXECUTED                                     
*                                                                               
ST05     LA    R1,SORTREC          INSERTION DATE                               
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         MVC   0(3,R1),KEY+16                                                   
         CLI   QOPT1,C'D'          SEE IF DOING ONLY DUPS                       
         BE    ST05X               DON'T PRESERVE ORIG SEQ                      
         LH    R0,SRTLIN                                                        
         AH    R0,=H'1'                                                         
         STH   R0,SRTLIN                                                        
         MVC   3(2,R1),SRTLIN      TO PRESERVE ORIG SEQ                         
ST05X    BR    RE                                                               
*                                                                               
ST06     LA    R1,SORTREC          ON-SALE DATE                                 
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDSDATE    EXECUTED                                     
*                                                                               
ST07     DS    0H                  SPACE DESCRIPTION                            
         BR    RE                                                               
         DS    F                                                                
ST08     DS    0H                  PRODUCT NAME                                 
         ST    RE,ST08-4                                                        
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETPRD                                                       
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     ST08X                                                            
         MVC   0(0,R1),PPRDNAME    EXECUTED FROM PRODUCT REC                    
*                                                                               
ST08X    L     RE,ST08-4                                                        
         BR    RE                                                               
*                                                                               
ST09     BR    RE                  COPY NUMBER                                  
*                                                                               
         DS    F                                                                
ST10     DS    0H                  DIVISION                                     
         ST    RE,ST10-4                                                        
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETPRD                                                       
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         STH   R0,DIVSD            SAVE DIV SORT DISPLACEMENT                   
         ZIC   R3,1(R4)            SORT LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     ST10X                                                            
         MVC   0(0,R1),PPRDDIV     EXECUTED FROM PRODUCT REC                    
*                                                                               
ST10X    L     RE,ST10-4                                                        
         BR    RE                                                               
*                                                                               
ST11     DS    0H                  ESTIMATE                                     
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         ZIC   R3,1(R4)            SORT LENGHT                                  
         LH    R0,SDISP                                                         
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),KEY+19      EXECUTED                                     
*                                                                               
ST12     LA    R1,SORTREC          PAYABLE DATE                                 
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDPDATE    EXECUTED                                     
*                                                                               
ST13     LA    R1,SORTREC          BILLABLE DATE                                
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDBDATE    EXECUTED                                     
*                                                                               
ST14     LA    R1,SORTREC          AD CODE                                      
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         LH    R0,SDISP                                                         
         ZIC   R3,1(R4)            SORT LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,SDISP            UPDATE CURRENT SORT DISP                     
         BCTR  R3,0                                                             
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),PBDJOB      EXECUTED                                     
*                                                                               
*                                                                               
*                                                                               
SORTTAB  DC    AL1(01),AL4(ST01)     PUB NUMBER      6                          
         DC    AL1(02),AL4(ST02)     PUB NAME      20                           
         DC    AL1(03),AL4(ST03)     PRODUCT CODE 3                             
         DC    AL1(04),AL4(ST04)     CLOSING DATE 3                             
         DC    AL1(05),AL4(ST05)     INSERTION DATE 5                           
         DC    AL1(06),AL4(ST06)     ON-SALE DATE 3                             
         DC    AL1(07),AL4(ST07)     SPACE DESCRIPTION 17 P1,P2                 
         DC    AL1(08),AL4(ST08)     PRODUCT NAME   20                          
         DC    AL1(09),AL4(ST09)     COPY NUMBER    17                          
         DC    AL1(10),AL4(ST10)     DIVISION     3                             
         DC    AL1(11),AL4(ST11)     ESTIMATE     3                             
         DC    AL1(12),AL4(ST12)     PAYABLE DATE 3                             
         DC    AL1(13),AL4(ST13)     BILLABLE DATE 3                            
         DC    AL1(14),AL4(ST14)     AD CODE 6                                  
*                                                                               
*              20 THRU 37 ARE $ AMOUNTS -  NO SORTING                           
*                                                                               
         DC    X'FFFF'             END OF TABLE                                 
*                                                                               
TLIN60   DS    0H                                                               
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            ADD PREVIOUS DISPLACEMENT                    
         AH    R1,=H'2'                                                         
         MVC   0(4,R1),KEY+27   DISK ADDR                                       
         L     R3,SAVPARS+4                                                     
         LH    R1,SDISP                                                         
         LA    R1,5(R1)            ADJUST FOR DISK ADDR AND EXTRA KEY           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SORTREC                                                  
         B     TLINX                                                            
*                                                                               
TLIN90   DS    0H                                                               
         XC    OLDPUB,OLDPUB                                                    
         MVI   SAVPARS+3,8                                                      
         B     TLINX                                                            
*                                                                               
TLINX    DS    0H                                                               
TLIXIT   XIT1                                                                   
         EJECT                                                                  
IGETCLT  NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYKEY                                                   
         MVI   KEY+3,2                                                          
         GOTO1 READ                                                             
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
         B     TLIXIT        XIT1                                               
         SPACE 3                                                                
IGETPRD  NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,6                                                          
         GOTO1 READ                                                             
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
         B     TLIXIT        XIT1                                               
         SPACE 2                                                                
IGETEST  NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVC   KEY+10(2),PBUYKEST                                               
         MVI   KEY+3,7                                                          
         GOTO1 READ                                                             
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
         B     TLIXIT      XIT1                                                 
*                                                                               
IGETP    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,PUBREC,(0,DMWORK)             
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     TLIXIT              XIT1                                         
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
*                                  OUTPUT                                       
TLOUT    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TLOUT                                                          
*                                                                               
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP81WRKD,R8                                                      
         SPACE 2                                                                
         CLI   PBUYREC,X'FF'                                                    
         BE    TLOUT2                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         MVC   SORTREC,0(R3)                                                    
         LA    R1,SORTREC                                                       
         AH    R1,SDISP            SHOULD STILL HAVE SORT LEN                   
         AH    R1,=H'2'            EXTRA KEY BYTES                              
         MVC   KEY+27(4),0(R1)    DISK ADDR                                     
         GOTO1 GETPRT                                                           
         CLC   OLDCLT,PBUYKCLT                                                  
         BE    TLOUT1                                                           
         BAS   RE,GETCLT                                                        
TLOUT1   GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKEY+7                              
         TM    PBUYCNTL,X'80'                                                   
         BZ    *+10                                                             
         XC    GROSS,GROSS                                                      
         LA    R6,PPBYOWRK                                                      
         USING PPBYOUTD,R6                                                      
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         LA    RF,GROSS                                                         
         ST    RF,PBYOVALS                                                      
         MVC   PBYODTCN,DATCON                                                  
         MVI   PBYOCTL,X'24'       FOR ZZZ ALLOCATNS AND IO COMMENTS            
*                                                                               
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*                                                                               
TLOUT2   DS    0H                                                               
         CLI   PBUYREC,X'FF'                                                    
         BE    TLOUTXX                                                          
         XC    DDISP,DDISP         DISPLACEMENT INTO P                          
*                                  CHK RPTTAB TO SEE IF DISPLAY FIELD           
         LA    R4,RPTTAB                                                        
TLOUT5   CLI   0(R4),X'FF'       END OF TABLE                                   
         BE    TLOUTX                                                           
         CLI   2(R4),X'FF'                                                      
         BE    TLOUT40             NOT DISPLAYING THIS FIELD                    
         LA    R5,DISPTAB                                                       
TLOUT10  CLI   0(R5),X'FF'         END OF DISPLAY TABLE                         
         BNE   *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         CLC   0(1,R4),0(R5)                                                    
         BE    TLOUT15                                                          
         LA    R5,5(R5)                                                         
         B     TLOUT10             NEXT ENTRY                                   
*                                                                               
TLOUT15  DS    0H                                                               
         MVC   FULL,1(R5)                                                       
         L     RF,FULL                                                          
         BAS   RE,0(RF)                                                         
*                                                                               
TLOUT40  LA    R4,3(R4)            NEXT ENTRY IN RPTTAB                         
         B     TLOUT5                                                           
*                                                                               
TLOUTX   MVC   OLDPUB,PBUYKPUB                                                  
         MVC   OLDCLT,PBUYKCLT                                                  
         MVC   OLDPRD,PBUYKPRD                                                  
         MVC   OLDJOB,PBDJOB                                                    
         CLI   QOPT1,C'D'          SEE IF SHOWING DUPS ONLY                     
         BNE   TLOUTX8                                                          
         XC    OLDPUB,OLDPUB       MUST ALWAYS SHOW PUB + PRD                   
         XC    OLDPRD,OLDPRD                                                    
         LH    R1,SDISP                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   OLDKEY(0),SORTREC                                                
         BNE   TLOUTX3                                                          
         MVI   DUPSW,1                                                          
TLOUTX2  MVC   NSAVP,P1            SWAP PRINT LINES                             
         MVC   NSAVP2,P2                                                        
         MVC   NSAVP3,P3                                                        
         MVC   P1,SAVP                                                          
         MVC   P2,SAVP2                                                         
         MVC   P3,SAVP3                                                         
         MVC   LINENEED,SAVLND                                                  
         MVC   SAVP,NSAVP                                                       
         MVC   SAVP2,NSAVP2                                                     
         MVC   SAVP3,NSAVP3                                                     
         B     TLOUTX8                                                          
*                                                                               
TLOUTX3  LH    R1,SDISP                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OLDKEY(0),SORTREC                                                
         CLI   DUPSW,1             SEE IF I NEED TO PRINT SAVP'S                
         BE    TLOUTX5             YES                                          
         MVC   SAVP,P1             SAVE P LINES                                 
         MVC   SAVP2,P2                                                         
         MVC   SAVP3,P3                                                         
         MVC   SAVLND,LINENEED                                                  
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         B     TLOUTXX                                                          
*                                                                               
TLOUTX5  MVI   DUPSW,0                                                          
         B     TLOUTX2                                                          
*                                                                               
TLOUTX8  MVI   SPACING,2                                                        
         BAS   RE,TLPRT                                                         
         B     TLOXIT                                                           
TLOUTXX  DS    0H                                                               
         CLI   PBUYREC,X'FF'       LAST RECORD                                  
         BNE   TLOXIT                                                           
         CLI   QOPT1,C'D'          SEE IF DOING DUPS                            
         BNE   TLOXIT                                                           
         CLI   DUPSW,1                                                          
         BNE   TLOXIT                                                           
         MVC   P1,SAVP             MUST PRINT LAST RECORD                       
         MVC   P2,SAVP2                                                         
         MVC   P3,SAVP3                                                         
         MVC   LINENEED,SAVLND                                                  
         MVI   SPACING,2                                                        
         BAS   RE,TLPRT                                                         
TLOXIT   XIT1                                                                   
**                                                                              
*                                                                               
DISPTAB  DC    AL1(01),AL4(DP01)     PUB NUMBER      15                         
         DC    AL1(02),AL4(DP02)     PUB NAME        20                         
         DC    AL1(03),AL4(DP03)     PRODUCT CODE    3                          
         DC    AL1(04),AL4(DP04)     CLOSING DATE    8                          
         DC    AL1(05),AL4(DP05)     INSERTION DATE 12 P1,P2                    
         DC    AL1(06),AL4(DP06)     ON-SALE DATE   8                           
         DC    AL1(07),AL4(DP07)     SPACE DESCRIPTION 17 P1,P2                 
         DC    AL1(08),AL4(DP08)     PRODUCT NAME   20                          
         DC    AL1(09),AL4(DP09)     COPY NUMBER    17                          
         DC    AL1(10),AL4(DP10)     DIVISION      3                            
         DC    AL1(11),AL4(DP11)     ESTIMATE      3                            
         DC    AL1(12),AL4(DP12)     PAYABLE DATE 8                             
         DC    AL1(13),AL4(DP13)     BILLABLE DATE 8                            
         DC    AL1(14),AL4(DP14)     AD CODE 6                                  
         DC    AL1(20),AL4(DP20)     ORDERED GROSS                              
         DC    AL1(21),AL4(DP21)     ORDERED NET                                
         DC    AL1(22),AL4(DP22)     ORDERED GR-CD                              
         DC    AL1(23),AL4(DP23)     ORDERED NET-CD                             
         DC    AL1(24),AL4(DP24)     ORDERED AGY COM                            
         DC    AL1(25),AL4(DP25)     ORDERED CD                                 
         DC    AL1(26),AL4(DP26)     PAID GROSS                                 
         DC    AL1(27),AL4(DP27)     PAID NET                                   
         DC    AL1(28),AL4(DP28)     PAID GR-CD                                 
         DC    AL1(29),AL4(DP29)     PAID NET-CD                                
         DC    AL1(30),AL4(DP30)     PAID AGY COM                               
         DC    AL1(31),AL4(DP31)     PAID CD                                    
         DC    AL1(32),AL4(DP32)     BILLED GROSS                               
         DC    AL1(33),AL4(DP33)     BILLED NET                                 
         DC    AL1(34),AL4(DP34)     BILLED GR-CD                               
         DC    AL1(35),AL4(DP35)     BILLED NET-CD                              
         DC    AL1(36),AL4(DP36)     BILLED AGY COM                             
         DC    AL1(37),AL4(DP37)     BILLED CD                                  
         DC    X'FFFF'             END OF TABLE                                 
*                                                                               
         DS    F                                                                
DP01     DS    0H                                                               
         ST    RE,DP01-4                                                        
         IC    R3,PAGYPROF+12                                                   
         LA    R5,P1                                                            
         LH    R0,DDISP                                                         
         AR    R5,R0                                                            
         GOTO1 PUBEDIT,DMCB,((R3),PBUYKPUB),(C'S',0(R5))                        
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         L     RE,DP01-4                                                        
         BR    RE                                                               
*                                                                               
         DS    F                                                                
DP02     DS    0H                                                               
         ST    RE,DP02-4                                                        
         CLC   OLDPUB,PBUYKPUB                                                  
         BNE   DP02B                                                            
         BAS   RE,TLCKHD                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    DP02D5                                                           
         MVC   PBNAM1(61),SPACES                                                
         MVC   PBNAM1+4(2),=C''''''     DITTOS                                  
         MVI   PBNAM1,0                                                         
         B     DP02H                                                            
*                                                                               
DP02B    DS    0H                  GET PUBNAME                                  
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),PBUYKPUB                                                
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         CLC   PUBKEY(07),KEY                                                   
         BE    DP02D3              ALREADY HAVE RECORD                          
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    DP02D                                                            
         CLI   PAGYPROF+16,C'0'    TEST SRDS DEFAULT                            
         BNE   *+8                                                              
         BAS   RE,NOPUB                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+8                                                              
         BAS   RE,NOPUB                                                         
         MVC   KEY+7(2),=C'ZZ'                                                  
         MVI   KEY+9,X'81'                                                      
         CLC   KEY(25),KEYSAVE                                                  
         BE    DP02D               HAVE SRDS                                    
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
NOPUB    DC    H'0'                PUB NOT FOUND                                
*                                                                               
DP02D    DS    0H                                                               
         BAS   RE,GETP                                                          
DP02D3   DS    0H                                                               
         MVC   KEY(64),SAVKEYS                                                  
DP02D5   DS    0H                                                               
         MVC   PBNAM1(61),SPACES                                                
         MVC   PBNAM1(40),PUBNAME       NAME & ZONE NAME                        
         CLI   PAGYKMED,C'N'                                                    
         BNE   DP02F                                                            
*                                                                               
         LA    RF,PBNAM2                                                        
         CLI   PBNAM2,C' '                                                      
         BNH   *+8                                                              
         LA    RF,PBNAM3                                                        
         MVC   0(16,RF),PUBCITY                                                 
         LA    RF,16(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C','                                                       
         MVC   3(2,RF),PUBSTATE                                                 
*                                                                               
DP02F    DS    0H                                                               
         OC    PBNAM1(60),SPACES                                                
DP02H    DS    0H                                                               
         LA    RF,1                                                             
         CLI   PBNAM2,C' '                                                      
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         CLI   PBNAM3,C' '                                                      
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         CLC   BYTE,LINENEED                                                    
         BNH   *+10                                                             
         MVC   LINENEED,BYTE                                                    
         LA    R1,P1                                                            
         AH    R1,DDISP                                                         
         MVC   0(20,R1),PBNAM1         P1                                       
         MVC   132(20,R1),PBNAM2       P2                                       
         MVC   264(20,R1),PBNAM3       P3                                       
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
DP02X    L     RE,DP02-4                                                        
         BR    RE                                                               
*                                                                               
DP03     DS    0H                  PRODUCT                                      
         LA    R1,P1                                                            
         AH    R1,DDISP                                                         
         MVC   0(3,R1),PBUYKPRD                                                 
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         BR    RE                                                               
*                                                                               
DP04     DS    0H                                                               
         LA    R2,PBDCDATE         CLOSING DATE                                 
         B     DDATE                                                            
*                                                                               
DP05     DS    0H                                                               
         LA    R3,P1                                                            
         AH    R3,DDISP                                                         
         MVC   0(11,R3),PBYOMDY                                                 
         MVC   BYTE,PBDBFD                                                      
         CLI   BYTE,C'B'                                                        
         BE    DP05C                                                            
         CLI   BYTE,C'W'                                                        
         BE    DP05C                                                            
         MVI   BYTE,C' '                                                        
DP05C    TM    PBUYCNTL,X'80'                                                   
         BZ    *+8                                                              
         MVI   BYTE,C'D'                                                        
*                                                                               
         LA    RF,8(R3)                                                         
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    RF,11(R3)                                                        
         MVC   0(1,RF),BYTE                                                     
         MVC   133(8,R3),PBYOMDY2    PSECOND                                    
         CLI   PBYOMDY2,C' '       SEE IF I HAVE SECOND DATE                    
         BNH   DP05E                                                            
         MVI   132(R3),C'+'                                                     
         CLI   LINENEED,2                                                       
         BH    DP05E                                                            
         MVI   LINENEED,2                                                       
DP05E    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         BR    RE                                                               
*                                                                               
DP06     DS    0H                                                               
         LA    R2,PBDSDATE         ON SALE DATE                                 
         B     DDATE                                                            
*                                                                               
         DS    F                                                                
DDATE    ST    RE,DDATE-4                                                       
         LA    R3,P1                                                            
         AH    R3,DDISP                                                         
         OC    0(3,R2),0(R2)       SEE IF I HAVE A DATE                         
         BZ    DDATE5                                                           
*        GOTO1 DTCNV,DMCB,(1,0(R2)),(3,0(R3))                                   
         GOTO1 DATCON,DMCB,(3,0(R2)),(5,0(R3))                                  
DDATE5   LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         L     RE,DDATE-4                                                       
         BR    RE                                                               
*                                                                               
DP07     DS    0H                                                               
         LA    R3,P1                                                            
         AH    R3,DDISP                                                         
         MVC   0(17,R3),PBYOSPC                                                 
         MVC   132(17,R3),PBYOSPC2                                              
*                                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   DP07D                                                            
*                                                                               
         CLI   PBYOSPC,C' '                                                     
         BH    *+10                                                             
         MVC   0(7,R3),PBYOUNTS   UNITS                                         
         LA    RF,15(R3)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(2,RF),PBYOPRM                                                  
*                                                                               
         CLI   PBYOLBC,C' '        LINES X COLS                                 
         BNH   DP07F                                                            
         MVC   132(17,R3),PBYOLBC                                               
         B     DP07E                                                            
*                                                                               
DP07D    CLI   PBYOSPC2,C' '                                                    
         BNH   DP07F                                                            
DP07E    CLI   LINENEED,2                                                       
         BH    DP07F                                                            
         MVI   LINENEED,2                                                       
DP07F    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         BR    RE                                                               
*                                                                               
         DS    F                                                                
DP08     DS    0H                  PRODUCT NAME                                 
         ST    RE,DP08-4                                                        
         CLC   OLDPRD,PBUYKPRD                                                  
         BNE   DP08B                                                            
         BAS   RE,TLCKHD                                                        
         CLI   FORCEHED,C'Y'                                                    
         BE    DP08S                                                            
         LA    R3,P1                                                            
         AH    R3,DDISP                                                         
         MVC   4(2,R3),=C''''''    DITTOS                                       
         B     DP08W                                                            
*                                                                               
DP08B    MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),PAGYKAGY                                                  
         MVC   KEY+2(1),PAGYKMED                                                
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         CLC   PPRDREC(17),KEY                                                  
         BE    DP08C               HAVE PRODUCT                                 
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PRD NOT ON FILE                              
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
DP08C    MVC   KEY(64),SAVKEYS        RESTORE KEYS                              
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
DP08S    LA    R3,P1                                                            
         AH    R3,DDISP                                                         
         MVC   0(20,R3),PPRDNAME                                                
DP08W    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         L     RE,DP08-4                                                        
         BR    RE                                                               
*                                                                               
*                                                                               
         DS    F                                                                
DP09     DS    0H                                                               
         ST    RE,DP09-4                                                        
         OC    PBDJOB,PBDJOB                                                    
         BZ    DP09W               NO JOB NUMBER                                
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),PAGYKAGY                                                  
         MVC   KEY+2(1),PAGYKMED                                                
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         MVC   KEY+10(6),PBDJOB                                                 
         CLC   PJOBREC(17),KEY                                                  
         BE    DP09C               HAVE JOB REC                                 
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JOB NOT ON FILE                              
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
DP09C    MVC   KEY(64),SAVKEYS        RESTORE KEYS                              
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
         LA    R3,P1                                                            
         AH    R3,DDISP                                                         
         MVC   0(17,R3),PJOBCPY                                                 
DP09W    LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         L     RE,DP09-4                                                        
         BR    RE                                                               
*                                                                               
         DS    F                                                                
DP10     DS    0H                  DIVISION CODE IN SORT REC                    
         ST    RE,DP10-4                                                        
         LA    R5,SORTREC                                                       
         AH    R5,DIVSD            DISPLACEMENT TO DIV                          
         TM    2(R4),X'80'         SEE IF IN HEADLINES                          
         BNZ   DP10D                                                            
         LA    R1,P1                                                            
         AH    R1,DDISP                                                         
         MVC   0(3,R1),0(R5)                                                    
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         B     DP10XX                                                           
*                                                                               
DP10D    DS    0H                                                               
         CLC   OLDDIV,0(R5)        SEE IF NEW DIVISION                          
         BE    DP10XX                                                           
DP10E    MVC   SAVKEYS,KEY                                                      
         MVC   OLDDIV,0(R5)                                                     
         MVI   FORCEHED,C'Y'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),PAGYKAGY                                                  
         MVC   KEY+2(1),PAGYKMED                                                
         MVI   KEY+3,X'03'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),0(R5)         DIVISION                                  
         CLC   PDIVREC(17),KEY                                                  
         BE    DP10F               HAVE DIVISION                                
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PRD NOT ON FILE                              
         LA    R0,PDIVREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
DP10F    MVC   KEY(64),SAVKEYS        RESTORE KEYS                              
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
*                                                                               
DP10XX   L     RE,DP10-4                                                        
         BR    RE                                                               
*                                                                               
DP11     DS    0H                  ESTIMATE                                     
         LA    R1,P1                                                            
         AH    R1,DDISP                                                         
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R1),DUB                                                      
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         BR    RE                                                               
*                                                                               
DP12     DS    0H                                                               
         LA    R2,PBDPDATE         PAYABLE DATE                                 
         B     DDATE                                                            
*                                                                               
DP13     DS    0H                                                               
         LA    R2,PBDBDATE         BILLABLE DATE                                
         B     DDATE                                                            
*                                                                               
DP14     DS    0H                  AD CODE                                      
         LA    R1,P1                                                            
         AH    R1,DDISP                                                         
         MVC   0(6,R1),PBDJOB                                                   
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         BR    RE                                                               
*                                                                               
DP20     L     R2,GROSS            GROSS                                        
         B     DOLDISP                                                          
*                                                                               
DP21     L     R2,GROSS            NET                                          
         S     R2,AGYCOM                                                        
         B     DOLDISP                                                          
*                                                                               
DP22     L     R2,GROSS            GR-CD                                        
         S     R2,CSHDSC                                                        
         B     DOLDISP                                                          
*                                                                               
DP23     L     R2,GROSS            NET-CD                                       
         S     R2,AGYCOM                                                        
         S     R2,CSHDSC                                                        
         B     DOLDISP                                                          
*                                                                               
DP24     L     R2,AGYCOM           AGY COM                                      
         B     DOLDISP                                                          
*                                                                               
DP25     L     R2,CSHDSC           CD                                           
         B     DOLDISP                                                          
*                                                                               
DP26     L     R2,PGROSS         PAID  GROSS                                    
         B     DOLDISP                                                          
*                                                                               
DP27     L     R2,PGROSS         PAID  NET                                      
         S     R2,PAGYCOM                                                       
         B     DOLDISP                                                          
*                                                                               
DP28     L     R2,PGROSS         PAID  GR-CD                                    
         S     R2,PCSHDSC                                                       
         B     DOLDISP                                                          
*                                                                               
DP29     L     R2,PGROSS        PAID   NET-CD                                   
         S     R2,PAGYCOM                                                       
         S     R2,PCSHDSC                                                       
         B     DOLDISP                                                          
*                                                                               
DP30     L     R2,PAGYCOM          PAID AGY COM                                 
         B     DOLDISP                                                          
*                                                                               
DP31     L     R2,PCSHDSC          PAID CD                                      
         B     DOLDISP                                                          
*                                                                               
DP32     L     R2,BGROSS         BILLED GROSS                                   
         B     DOLDISP                                                          
*                                                                               
DP33     L     R2,BGROSS         BILLED NET                                     
         S     R2,BAGYCOM                                                       
         B     DOLDISP                                                          
*                                                                               
DP34     L     R2,BGROSS         BILLED GR-CD                                   
         S     R2,BCSHDSC                                                       
         B     DOLDISP                                                          
*                                                                               
DP35     L     R2,BGROSS        BILLED NET-CD                                   
         S     R2,BAGYCOM                                                       
         S     R2,BCSHDSC                                                       
         B     DOLDISP                                                          
*                                                                               
DP36     L     R2,BAGYCOM          BILLED AGY COM                               
         B     DOLDISP                                                          
*                                                                               
DP37     L     R2,BCSHDSC          BILLED CD                                    
         B     DOLDISP                                                          
*                                                                               
         DS    F                                                                
DOLDISP  DS    0H                                                               
         ST    RE,DOLDISP-4                                                     
         LA    R1,P1                                                            
         AH    R1,DDISP                                                         
         MVC   0(6,R1),PBDJOB                                                   
         LH    R0,DDISP                                                         
         ZIC   R3,2(R4)            DISP LENGHT                                  
         AR    R0,R3                                                            
         STH   R0,DDISP            UPDATE CURRENT DISP DISP                     
         LR    R3,R1                                                            
*                                                                               
         EDIT  (R2),(14,0(R3)),2,COMMAS=YES,FLOAT=-                             
         L     RE,DOLDISP-4                                                     
         BR    RE                                                               
*                                                                               
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
TLEDL    DS    0H                                                               
         EDIT  (P8,DUB),(6,(R5)),ALIGN=LEFT                                     
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
ENDEST   DS    0H                                                               
         CLC   QEST,SPACES                                                      
         BER   RE                                                               
         CLC   QESTEND,SPACES                                                   
         BNER  RE                                                               
         LA    R2,ESTTOTS                                                       
         LA    R3,=CL15'ESTIMATE TOTALS'                                        
         B     TLTOT                                                            
         SPACE 2                                                                
ENDPRD   DS    0H                                                               
         CLC   QPRODUCT,SPACES                                                  
         BER   RE                                                               
         LA    R2,PRDTOTS                                                       
         LA    R3,=CL15'PRODUCT TOTALS'                                         
         B     TLTOT                                                            
         SPACE 2                                                                
ENDCLT   DS    0H                                                               
         LA    R2,CLTTOTS                                                       
         LA    R3,=CL15'CLIENT TOTALS'                                          
         B     TLTOT                                                            
         SPACE 2                                                                
TLTOT    NTR1                                                                   
         SPACE 2                                                                
         OC    0(L'ESTTOTS,R2),0(R2)                                            
         BZ    TLOXIT              XIT1                                         
         MVC   TOTWRK,0(R2)                                                     
         XC    0(L'ESTTOTS,R2),0(R2)                                            
         LM    R0,R1,TOTWRK                                                     
         A     R0,TOTWRK+8                                                      
         A     R1,TOTWRK+12                                                     
         STM   R0,R1,TOTTOT                                                     
*                                                                               
         MVC   P1+1(15),0(R3)                                                   
         LA    RF,3                                                             
         LA    R5,ORDWRDS                                                       
         LA    R6,P1+20                                                         
         LA    R2,TOTWRK                                                        
TLTOT2   DS    0H                                                               
         MVC   0(9,R6),0(R5)                                                    
         EDIT  (B4,0(R2)),(14,9(R6)),2,COMMAS=YES,MINUS=YES,FLOAT=$             
         CLI   25(R6),C' '                                                      
         BNE   *+6                                                              
         BCTR  R6,R0                                                            
         MVI   23(R6),C'/'                                                      
         EDIT  (B4,4(R2)),(5,24(R6)),ALIGN=LEFT                                 
         OI    24(R6),C'0'                                                      
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R6,33(R6)                                                        
         LA    R5,9(R5)                                                         
         BCT   RF,TLTOT2                                                        
         BAS   RE,TLPRT                                                         
         B     TLOXIT              XIT1                                         
         EJECT                                                                  
         SPACE 2                                                                
*                                  PRINTING                                     
TLPRT    NTR1                                                                   
         SPACE 2                                                                
         BAS   RE,TLCKHD                                                        
*                                                                               
TLPRT2   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,TLHEAD                                                        
         GOTO1 REPORT                                                           
*                                                                               
         MVI   LINENEED,2                                                       
         B     TLOXIT              XIT1                                         
         SPACE 3                                                                
TLHEAD   DS    0H                                                               
         MVC   HEAD1+45(L'ROCKT),ROCKT                                          
         MVC   HEAD2+45(L'ROCKT),DASHES                                         
         LA    R1,RPTTAB           CHK FOR DIV IN HEADS                         
TLHEAD2  CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    TLHEAD3D                                                         
         CLI   0(R1),X'0A'         DIV                                          
         BE    TLHEAD3                                                          
         LA    R1,3(R1)                                                         
         B     TLHEAD2                                                          
TLHEAD3  TM    2(R1),X'80'                                                      
         BZ    TLHEAD3D                                                         
         MVI   RCSUBPRG,1          DIV IN HEADS                                 
*                                                                               
TLHEAD3D MVC   HEAD7,SAVHD7                                                     
         MVC   HEAD8,SAVHD8                                                     
         MVC   HEAD9,SAVHD9                                                     
TLHEAD4  DS    0H                                                               
         LA    RF,SPACES                                                        
         CLI   QBPDATE,C' '                                                     
         BE    TLHEAD5                                                          
         LA    RF,BILHD                                                         
         CLI   QBPDATE,C'B'                                                     
         BE    TLHEAD5                                                          
         LA    RF,PAYHD                                                         
         CLI   QBPDATE,C'P'                                                     
         BE    TLHEAD5                                                          
         LA    RF,CLOSHD                                                        
         CLI   QBPDATE,C'C'                                                     
         BE    TLHEAD5                                                          
         LA    RF,OSDHD                                                         
         CLI   QBPDATE,C'S'                                                     
         BE    TLHEAD5                                                          
         LA    RF,IODHD                                                         
         CLI   QBPDATE,C'I'                                                     
TLHEAD5  DS    0H                                                               
         MVC   HEAD5+43(22),0(RF)                                               
         CLI   QCLIENT,C'*'                                                     
         BNE   TLHEAD6                                                          
         MVC   HEAD6(6),=C'OFFICE'                                              
         MVC   HEAD6+9(2),QCLIENT+1                                             
TLHEAD6  DS    0H                                                               
         CLI   QOPT1,C'D'          DUPLICATES ONLY                              
         BNE   TLHEADX                                                          
         MVC   HEAD6+47(15),=C'DUPLICATES ONLY'                                 
TLHEADX  DS    0H                                                               
         BR    RE                                                               
         SPACE 2                                                                
BILHD    DC    C' ** BILLING PERIOD ** '                                        
PAYHD    DC    C' ** PAYABLE DATES **  '                                        
CLOSHD   DC    C' ** CLOSING DATES **  '                                        
IODHD    DC    C'** INS. ORDER DATES **'                                        
OSDHD    DC    C' ** ON-SALE DATES **  '                                        
*                                                                               
ROCKT    DC    C'MANAGEMENT CONTROL'                                            
         SPACE 3                                                                
TLCKHD   DS    0H                                                               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    R0,LINE                                                          
         IC    RF,LINENEED                                                      
         AR    R0,RF                                                            
         STC   R0,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BLR   RE                                                               
         MVI   FORCEHED,C'Y'                                                    
         BR    RE                                                               
         SPACE 3                                                                
GETCLT   NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYKEY                                                   
         MVI   KEY+3,2                                                          
         GOTO1 READ                                                             
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
         B     TLOXIT              XIT1                                         
         SPACE 3                                                                
GETPRD   NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,6                                                          
         GOTO1 READ                                                             
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
         B     TLOXIT              XIT1                                         
         SPACE 2                                                                
GETEST   NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVC   KEY+10(2),PBUYKEST                                               
         MVI   KEY+3,7                                                          
         GOTO1 READ                                                             
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
         B     TLOXIT              XIT1                                         
*                                                                               
GETP    NTR1                                                                    
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,PUBREC,(0,DMWORK)             
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     TLOXIT              XIT1                                         
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
ORDWRDS  DC    CL9'  ORDERED'                                                   
         DC    CL9'UNORDERED'                                                   
         DC    CL9'    TOTAL'                                                   
         SPACE 3                                                                
PP81WRKD DSECT                                                                  
ATLIN    DS    A                                                                
ATLOUT   DS    A                                                                
RUNSW    DS    X                                                                
SDISP    DS    H                                                                
DDISP    DS    H                                                                
DIVSD    DS    H                   DIV SORT DISPLACEMENT                        
CHGSW    DS    CL1                                                              
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
BPUB     DS    XL6                                                              
SKPUB    DS    CL20                                                             
*                                                                               
OLDKEY   DS    CL250                                                            
         DS    CL4                                                              
OLDCLT   DS    CL3                                                              
OLDPRD   DS    CL3                                                              
OLDPUB   DS    CL6                                                              
         DS    CL3                                                              
OLDEST   DS    CL2                                                              
         DS    CL4                                                              
OLDJOB   DS    CL6                                                              
OLDDIV   DS    CL3                                                              
*                                                                               
         DS    0F                                                               
SAVPARS  DS    0CL24                                                            
         DS    6F                                                               
SAVR2    DS    F                                                                
ELCODE   DS    X                                                                
DUPSW    DS    C                                                                
LINENEED DS    X                                                                
SAVLND   DS    X                   SAVED LINENEED FOR SAVP AND SAVP2            
SRTLIN   DS    H                                                                
SAVKEYS  DS    CL64                                                             
SAVDAT2  DS    CL8                                                              
PPBYOWRK DS    CL600                                                            
         DS    0D                                                               
ESTTOTS  DS    CL16                                                             
PRDTOTS  DS    CL16                                                             
CLTTOTS  DS    CL16                                                             
*                                                                               
TOTWRK   DS    CL16                                                             
TOTTOT   DS    CL8                                                              
*                                                                               
INSTAB   DS    20CL34                                                           
PBNAM1   DS    CL20                                                             
PBNAM2   DS    CL20                                                             
PBNAM3   DS    CL20                                                             
PBNAMX   DS    CL1                                                              
SPAC1    DS    CL20                                                             
SPAC2    DS    CL20                                                             
SPAC3    DS    CL20                                                             
SPACX    DS    CL1                                                              
COMTAB   DS    10CL35       COPY + 2 CAPTIONS + 2 ALLOS + 5 COMS                
COMTABX  DS    CL1                                                              
SAVHD7   DS    CL132                                                            
SAVHD8   DS    CL132                                                            
SAVHD9   DS    CL132                                                            
SAVP     DS    CL132                                                            
SAVP2    DS    CL132                                                            
SAVP3    DS    CL132                                                            
NSAVP    DS    CL132                                                            
NSAVP2   DS    CL132                                                            
NSAVP3   DS    CL132                                                            
*                                                                               
         SPACE 3                                                                
RPTTAB   DS    CL39                ROOM FOR 12 FLDS +3X'FF'                     
*              FORMAT IS FLD CODE,SORT LENGHT,DISPLAY LENGHT                    
*              IF SORT LEN IS X'FF' DON'T SORT ON THIS FLD                      
*              IF DISP LEN IS X'FF' DON'T DISPLAY THIS FLD                      
*              IF DISP LEN IS ORED WITH X'80' HEADLINE DISPLAY                  
*                                                                               
SORTREC  DS    CL250               MAX SORT REC                                 
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPMODEQU                                                       
*                                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
*                                                                               
         PRINT ON                                                               
PPWORKD  DSECT                                                                  
         ORG   QREGION                                                          
QJOB     DS    CL6                                                              
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'148PPREP8102 05/01/02'                                      
         END                                                                    
