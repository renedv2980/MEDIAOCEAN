*          DATA SET SPSFM18    AT LEVEL 025 AS OF 08/11/11                      
*PHASE T21718A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T21718 - NSID RANK'                                             
T21718   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21718,RA,RR=RE                                                
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
RANK2    CLI   MODE,VALKEY                                                      
         BE    RANK4                                                            
         CLI   MODE,VALREC                                                      
         BE    RANK6                                                            
         CLI   MODE,PRINTREP                                                    
         BE    RPT                 PRINT REPORT                                 
         B     RANKX                                                            
*                                                                               
RANK4    LA    RE,DYNAMIC                                                       
         LA    RF,DYNAMICL                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR DYNAMIC MODULE STORAGE                 
*                                                                               
         CLI   RNKPF,0             TEST FOR FIRST TIME FOR SCREEN               
         BNE   *+10                NO                                           
         XC    SVVALS,SVVALS       CLEAR LAST TIME KEY VALUES                   
         BAS   RE,VALHED           VALIDATE HEADLINE FIELDS                     
*                                                                               
         L     RF,SYSPARMS         EXTRACT ANY PF KEY                           
         L     RF,0(RF)            A(TIOB)                                      
         USING TIOBD,RF                                                         
         ZIC   R1,TIOBAID          PF KEY NUMBER                                
         CH    R1,=H'12'           TEST PF13-PF24                               
         BNH   *+8                 NO                                           
         SH    R1,=H'12'           YES - EQUATE TO PF1-PF12                     
         STC   R1,THISPF                                                        
         MVC   CURSDISP,TIOBCURD   SAVE DISP TO CURSOR FLDH                     
         DROP  RF                                                               
*                                                                               
         MVI   MYACT,REGULAR                                                    
         CLI   THISPF,PF5          TEST FOR RE-RANK                             
         BE    *+14                YES                                          
         CLC   CTLVALS(CTLVALN),SVVALS TEST FOR CHANGE IN KEY VALUES            
         BE    *+8                 NO                                           
         MVI   MYACT,FORCEDIS      YES-FORCE RE-RANK AND DISPLAY                
         CLI   MYACT,REGULAR                                                    
         BE    RANK5                                                            
         LA    RE,SAVES            CLEAR SAVED STORAGE ON FORCED DISPL          
         LA    RF,SAVEL                                                         
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
RANK5    MVC   SVVALS,CTLVALS      UPDATE SAVED KEY VALUES                      
         B     RANKX                                                            
*                                                                               
RANK6    MVC   ADETTAB,ATIA        BUILD DETAIL TABLE IN TIA                    
         L     RE,=A(FORM)         RELOCATE A(FORMAT ROUTINE)                   
         A     RE,RELO                                                          
         ST    RE,AFORM                                                         
         CLI   MYACT,FORCEDIS      TEST FOR FORCED DISPLAY                      
         BNE   RANK10              NO                                           
*                                                                               
* BUILD RANKED DETAIL TABLE - DISPLAY FIRST PAGE                                
*                                                                               
         MVC   SVSTA,STA           UPDATE SAVED STATION                         
         BAS   RE,RDSIR            READ THE DETAIL FILE                         
         BE    *+16                NO RECORDS FOUND                             
         BAS   RE,WRAP             WRAP UP DETAIL TABLE BUILDING                
         BAS   RE,VECTOR           GENERATE DETAIL VECTOR                       
         BE    RANK7               HAVE SOME ENTRIES                            
*                                                                               
         MVC   CONHEAD(L'NODATA),NODATA                                         
         LA    R2,RNKMEDH                                                       
         OI    6(R2),X'81'         XMIT BACK MODIFIED                           
         ST    R2,ACURFORC                                                      
         B     RANKX                                                            
*                                                                               
RANK7    GOTO1 CLEARF,DMCB,(1,RNKDEMH),RNKLAST                                  
         GOTO1 (RF),(R1),(0,RNKSEL1H),RNKLAST                                   
*                                                                               
         ZIC   R0,SVYEAR           TRANSMIT BACK THE YEAR                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RNKYR,DUB+6(2)                                                   
         OI    RNKYRH+6,X'80'                                                   
*                                                                               
         BAS   RE,DEMN             DISPLAY DEMO NAMES                           
         MVC   RNKPF(L'PFMSG),PFMSG  PF KEY LEGEND                              
*                                                                               
         LA    R2,RNKSEL1H                                                      
         LA    R3,1                                                             
         LA    R4,MAXDETS                                                       
         C     R4,SVNENTS          TEST IF PAST HIGHEST ENTRY                   
         BNH   *+8                                                              
         L     R4,SVNENTS          YES                                          
         STC   R3,SVFIRST          SAVE FIRST ENTRY ON SCREEN                   
         STC   R4,SVLAST           AND LAST ENTRY                               
         LA    R5,SVVECTOR         R5=A(VECTOR TABLE)                           
*                                                                               
RANK8    ZIC   R3,0(R5)            GET DETAIL ENTRY NUMBER                      
         GOTO1 AFORM,DMCB,(RC),(R2),(R3)                                        
         LA    R2,DISPSEL(R2)                                                   
         LA    R5,1(R5)                                                         
         BCT   R4,RANK8                                                         
         B     RANK20                                                           
*                                                                               
RANK10   BAS   RE,GETTAB           RETRIEVE DETAIL TABLE                        
         CLC   STA,SVSTA           TEST FOR CHANGE IN STATION                   
         BE    RANK10A             NO                                           
         MVC   SVSTA,STA           YES                                          
         MVI   SCROLL,0            REMOVE ANY SCROLL                            
         BAS   RE,VECTOR           RE-BUILD DISPLAY VECTOR                      
         BE    RANK11              START FROM PAGE 1                            
         MVC   CONHEAD(L'NODATA),NODATA                                         
         LA    R2,RNKMEDH                                                       
         OI    6(R2),X'81'         XMIT BACK MODIFIED                           
         ST    R2,ACURFORC                                                      
         B     RANKX                                                            
*                                                                               
RANK10A  CLI   THISPF,PF7          TEST TO SCROLL BACKWARDS                     
         BE    RANK15              YES                                          
         CLI   THISPF,PF8          TEST TO SCROLL FORWARDS                      
         BNE   RANK30              NO-EDIT THE SCREEN                           
*                                                                               
* SCROLL FORWARDS                                                               
*                                                                               
RANK11   LA    R2,RNKSEL1H                                                      
         GOTO1 CLEARF,DMCB,(1,RNKDET1H),RNKPFH                                  
         GOTO1 (RF),(R1),(0,(R2)),RNKLAST                                       
         ZIC   R3,SVLAST           PICK UP LAST ENTRY ON PREV SCRN              
         LA    R3,1(R3)            BUMP AHEAD TO NEXT ENTRY                     
         CLI   SCROLL,0            TEST FOR SCROLL AMOUNT                       
         BNE   RANK12              YES                                          
         C     R3,SVNENTS          TEST START ABOVE HIGHEST ENTRY               
         BNH   *+8                 NO                                           
         LA    R3,1                FORCE WRAP-AROUND TO FIRST PAGE              
         B     RANK13                                                           
*                                                                               
RANK12   ZIC   RE,SCROLL                                                        
         LA    RF,MAXDETS                                                       
         AR    R3,RE               ADD SCROLL AMOUNT                            
         SR    R3,RF               LESS 1 PAGE                                  
         BP    *+8                                                              
         LA    R3,1                                                             
         C     R3,SVNENTS          TEST IF PAST HIGHEST ENTRY                   
         BNH   *+8                 NO                                           
         L     R3,SVNENTS          YES-FORCE DISPLAY OF LAST ENTRY              
*                                                                               
RANK13   LA    R4,MAXDETS-1(R3)    R4=LAST ENTRY                                
         C     R4,SVNENTS          TEST BEYOND HIGHEST ENTRY                    
         BNH   *+8                                                              
         L     R4,SVNENTS                                                       
         STC   R3,SVFIRST                                                       
         STC   R4,SVLAST                                                        
         LA    R4,1(R4)                                                         
         SR    R4,R3               R4=N'ENTRIES ON SCREEN                       
         LA    R5,SVVECTOR-1(R3)   INDEX INTO DISPLAY VECTOR                    
*                                                                               
RANK14   ZIC   R3,0(R5)                                                         
         GOTO1 AFORM,DMCB,(RC),(R2),(R3)                                        
         LA    R2,DISPSEL(R2)                                                   
         LA    R5,1(R5)                                                         
         BCT   R4,RANK14                                                        
         B     RANK20                                                           
*                                                                               
* SCROLL BACKWARDS                                                              
*                                                                               
RANK15   LA    R2,RNKSEL1H                                                      
         GOTO1 CLEARF,DMCB,(1,RNKDET1H),RNKPFH                                  
         GOTO1 (RF),(R1),(0,(R2)),RNKLAST                                       
         ZIC   R3,SVFIRST          R3=FIRST ENTRY ON SCREEN                     
         SR    R4,R4                                                            
         ICM   R4,1,SCROLL                                                      
         BNZ   *+8                 BACK UP BY SCROLL AMOUNT                     
         LA    R4,MAXDETS          USE PAGE LIMIT IF NO SCROLL AMOUNT           
         SR    R3,R4               R3=FIRST ENTRY ON NEW PAGE                   
         BP    *+8                                                              
         LA    R3,1                CANNOT GO BACK PAST 1ST ENTRY                
         LA    R4,MAXDETS-1(R3)    SET LAST ENTRY                               
         C     R4,SVNENTS          TEST BEYOND HIGHEST ENTRY                    
         BNH   *+8                                                              
         L     R4,SVNENTS                                                       
         STC   R3,SVFIRST          SET FIRST AND LAST ENTRY ON PAGE             
         STC   R4,SVLAST                                                        
         LA    R4,1(R4)                                                         
         SR    R4,R3               R4=N'ENTRIES ON THIS PAGE                    
         LA    R5,SVVECTOR-1(R3)   INDEX INTO VECTOR                            
*                                                                               
RANK17   ZIC   R3,0(R5)                                                         
         GOTO1 AFORM,DMCB,(RC),(R2),(R3)                                        
         LA    R2,DISPSEL(R2)                                                   
         LA    R5,1(R5)                                                         
         BCT   R4,RANK17                                                        
         B     RANK20                                                           
*                                                                               
* EXIT FROM DISPLAY LOGIC - SET MESSAGE AND CURSOR                              
*                                                                               
RANK20   MVC   CONHEAD(7),=C'** LINE'                                           
         LA    R4,CONHEAD+7        R4=OUTPUT POINTER                            
         ZIC   R0,SVFIRST                                                       
         CLC   SVFIRST,SVLAST      TEST ONLY 1 LINE ON SCREEN                   
         BE    *+12                YES                                          
         MVI   0(R4),C'S'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R4,1(R4)            POSITION FOR LINE NUMBER                     
         EDIT  (R0),(3,0(R4)),ALIGN=LEFT                                        
         AR    R4,R0               UPDATE OUTPUT POINTER                        
         CLC   SVFIRST,SVLAST      TEST ONLY 1 LINE                             
         BE    RANK21              YES                                          
*                                                                               
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         ZIC   R0,SVLAST                                                        
         EDIT  (R0),(3,0(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
*                                                                               
RANK21   LA    R4,1(R4)                                                         
         MVC   0(2,R4),=C'OF'                                                   
         LA    R4,3(R4)                                                         
         L     R0,SVNENTS                                                       
         EDIT  (R0),(3,0(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         MVC   1(L'DISMSG,R4),DISMSG                                            
*                                                                               
RANK22   LA    R2,RNKSEL1H         SET CURSOR POSITION                          
         OI    6(R2),X'81'                                                      
         ST    R2,ACURFORC                                                      
         B     RANKX                                                            
*                                                                               
* EDIT SCREEEN IN TWO PASSES - NEXT PAGE IF NO CHANGES                          
*                                                                               
RANK30   CLI   GCMODE,C'S'         TEST CALL FROM T2171F                        
         BE    *+8                 YES-IGNORE PF KEYS                           
         BAS   RE,LOOKCUR          CHECK CURSOR/PF KEY                          
         MVI   EDTMODE,C'E'        SET TO EDIT ONLY-FIRST PASS                  
         L     RE,=A(EDT)                                                       
         A     RE,RELO                                                          
         ST    RE,AEDT                                                          
*                                                                               
RANK31   LA    R2,RNKSEL1H         INITIALIZE FOR EDIT                          
         ZIC   R3,SVFIRST                                                       
         LTR   R3,R3               TEST NOTHING ON SCREEN                       
         BZ    RANK11              YES-DISPLAY PAGE 1                           
         ZIC   R4,SVLAST                                                        
         LA    R4,1(R4)                                                         
         SR    R4,R3               N'DETAIL LINES ON SCREEN                     
         LA    R5,SVVECTOR-1(R3)                                                
*                                                                               
RANK32   ZIC   R3,0(R5)                                                         
         GOTO1 AEDT,DMCB,(RC),(R2),(R3)                                         
         LA    R2,DISPSEL(R2)                                                   
         LA    R5,1(R5)                                                         
         BCT   R4,RANK32                                                        
*                                                                               
         CLI   EDTMODE,C'U'        TEST FOR SECOND PASS                         
         BE    RANK35              YES-SET TO EXIT                              
         CLI   CHASW,C'Y'          TEST IF ANY CHANGES INPUT                    
         BNE   RANK11              NO-DISPLAY NEXT PAGE                         
         MVI   EDTMODE,C'U'        SET TO EDIT AND UPDATE                       
         B     RANK31                                                           
*                                                                               
RANK35   MVC   CONHEAD(L'CHAMSG),CHAMSG                                         
         LA    R2,RNKSEL1H                                                      
         OI    6(R2),X'81'                                                      
         ST    R2,ACURFORC                                                      
         BAS   RE,PUTTAB           SAVE UPDATED DETAIL TABLE                    
*                                                                               
RANKX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CREATE A VECTOR OF ENTRIES THAT DEFINE WHICH                   
* DETAIL TABLE ENTRIES WILL BE DISPLAYED                                        
*                                                                               
* ON EXIT, CC=EQ IF VECTOR HAS ENTRIES, CC=NEQ IF NONE                          
*                                                                               
VECTOR   NTR1                                                                   
         XC    SVVECTOR,SVVECTOR   CLEAR VECTOR                                 
         XC    SVFIRST(2),SVFIRST  AND FIRST/LAST ENTRY ON SCREEN               
         XC    SVNENTS,SVNENTS     AND N'ENTRIES IN VECTOR                      
         OC    SVSTA,SVSTA         TEST IF STATION OR MARKET                    
         BNZ   VECTOR2             STATION                                      
*                                                                               
         LA    RE,SVVECTOR                                                      
         ICM   R0,15,SVNDETS                                                    
         BZ    VECTORNO            NO ENTRIES FOR MARKET                        
         ST    R0,SVNENTS          N'ENTRIES=N'DETAIL TABLE ENTRIES             
         LA    R1,1                R1=DETAIL ENTRY NUMBER                       
*                                                                               
VECTOR1  STC   R1,0(RE)                                                         
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,VECTOR1                                                       
         B     VECTOROK                                                         
*                                                                               
VECTOR2  SR    R0,R0                                                            
         ICM   R0,1,SVNSTAS                                                     
         BZ    VECTORNO                                                         
         LA    RE,SVSTATAB                                                      
         LA    R1,1                R1=STATION NUMBER                            
*                                                                               
VECTOR3  CLC   SVSTA,0(RE)         LOCATE STATION IN STA TABLE                  
         BE    VECTOR4                                                          
         LA    RE,3(RE)            NEXT STATION                                 
         LA    R1,1(R1)                                                         
         BCT   R0,VECTOR3                                                       
         B     VECTORNO            NO RECORDS FOR STATION                       
*                                                                               
VECTOR4  STC   R1,BYTE                                                          
         LA    RE,SVVECTOR         RE=A(VECTOR)                                 
         L     R0,SVNDETS          R0=LOOP COUNTER                              
         LA    R1,1                R1=DETAIL ENTRY NUMBER                       
         L     R3,ADETTAB                                                       
         USING DETD,R3                                                          
*                                                                               
VECTOR6  CLC   DETSTA,BYTE         MATCH ON STATION                             
         BNE   *+12                NO                                           
         STC   R1,0(RE)            YES-ADD ENTRY TO VECTOR                      
         LA    RE,1(RE)            UPDATE VECTOR POINTER                        
         LA    R1,1(R1)            INCREMENT DETAIL ENTRY NUMBER                
         LA    R3,DETLN(R3)        NEXT DETAIL ENTRY                            
         BCT   R0,VECTOR6                                                       
*                                                                               
         LA    RF,SVVECTOR                                                      
         SR    RE,RF               COMPUTE NUMBER OF ENTRIES                    
         BZ    VECTORNO            NO ENTRIES FOR STATION                       
         ST    RE,SVNENTS                                                       
*                                                                               
VECTOROK CR    RB,RB               SET CC=EQ                                    
         B     VECTORX                                                          
*                                                                               
VECTORNO LTR   RB,RB               SET CC=NEQ                                   
*                                                                               
VECTORX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE HEADLINE FIELDS                                       
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,RNKMEDH                                                       
         GOTO1 VALIMED                                                          
         MVI   ERROR,INVMED                                                     
         CLI   QMED,C'T'           TEST TV                                      
         BNE   TRAPERR                                                          
         MVC   MEDIA,QMED                                                       
*                                                                               
         LA    R2,RNKCLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   CLT,QCLT                                                         
         MVC   RNKCLTN,CLTNM       CLIENT NAME                                  
         OI    RNKCLTNH+6,X'80'    XMIT BACK                                    
*                                                                               
         LA    R2,RNKPRDH                                                       
         GOTO1 VALIPRD                                                          
         MVC   PROD,WORK           EBCDIC CODE                                  
         MVC   RNKPRDN,PRDNM                                                    
         OI    RNKPRDNH+6,X'80'                                                 
*                                                                               
VALHED2  LA    R2,RNKESTH                                                       
         GOTO1 VALINUM                                                          
         MVC   EST,ACTUAL                                                       
         BAS   RE,GETEST                                                        
         MVC   RNKESTN,ESTNAME                                                  
         OI    RNKESTNH+6,X'80'                                                 
*                                                                               
VALHED4  LA    R2,RNKSTAH                                                       
         TM    4(R2),X'08'         TEST FOR NUMERIC FIELD                       
         BZ    VALHED5             NO-CHECK FOR STATION                         
         GOTO1 VALIMKT                                                          
         B     VALHED6                                                          
*                                                                               
VALHED5  GOTO1 VALISTA                                                          
         MVC   STA,BSTA            SAVE INPUT STATION                           
*                                                                               
VALHED6  MVC   MKT,BMKT            SAVE MARKET                                  
         LA    R2,RNKDPTH                                                       
         GOTO1 ANY                                                              
         BAS   RE,EDDPT            EDIT INPUT LIST                              
*                                                                               
         LA    R2,RNKLENH                                                       
         MVI   LEN,30                                                           
         CLI   5(R2),0                                                          
         BE    VALHED8                                                          
         GOTO1 VALINUM                                                          
         MVC   LEN,ACTUAL                                                       
*                                                                               
VALHED8  LA    R2,RNKPERSH         PERIOD/SCHEME                                
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    TRAPERR                                                          
         MVI   ERROR,INVALID                                                    
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),C',=,/'                              
         CLI   4(R1),1                                                          
         BNE   TRAPERR                                                          
         LA    R4,BLOCK                                                         
*                                                                               
         CLI   0(R4),2             BUYING PERIOD IS 2-4 BYTES                   
         BL    TRAPERR                                                          
         CLI   0(R4),4                                                          
         BH    TRAPERR                                                          
         MVC   BUYPER,12(R4)                                                    
*                                                                               
         MVC   SCHEME,=C'ALL'      DEFAULT IS 'ALL'                             
         CLI   1(R4),0             TEST IF SCHEME INPUT                         
         BE    VALHED10            NO                                           
         CLI   1(R4),2             SCHEME IS 2-3 BYTES                          
         BL    TRAPERR                                                          
         CLI   1(R4),3                                                          
         BH    TRAPERR                                                          
         MVC   SCHEME,22(R4)                                                    
*                                                                               
VALHED10 LA    R2,RNKYRH                                                        
         CLI   5(R2),0             YEAR FIELD IS OPTIONAL                       
         BE    VALHED11                                                         
         GOTO1 VALINUM                                                          
         MVC   YEAR,ACTUAL                                                      
*                                                                               
VALHED11 LA    R2,RNKOPTH                                                       
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,VALOPT                                                        
*                                                                               
         LA    R2,RNKSCRLH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED12                                                         
         GOTO1 VALINUM                                                          
         MVC   SCROLL,ACTUAL       MOVE IN N'LINES TO SCROLL                    
*                                                                               
VALHED12 MVI   SVUSECMP,C'N'       ASSUME USER DOESN'T USE COMPETITION          
         XC    KEY,KEY             READ SID PROFILE                             
         MVC   KEY(4),=C'S0SI'                                                  
         MVC   KEY+4(2),AGENCY                                                  
         MVC   KEY+6(1),QMED                                                    
         CLC   SCHEME,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   KEY+7(3),SCHEME                                                  
         GOTO1 GETPROF,DMCB,KEY,WORK,DATAMGR                                    
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    *+10                NO                                           
         MVC   SVUSECMP,WORK       SAVE PROFILE VALUE                           
*                                                                               
VALHEDX  B     XIT                                                              
         EJECT                                                                  
* SUBROUTINE TO READ ESTIMATE HEADER AND BUILD A DEMO LIST FROM IT              
*                                                                               
GETEST   NTR1                                                                   
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,PROD                                                     
         MVC   EKEYEST,EST                                                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   EKEY,KEYSAVE                                                     
         BNE   TRAPERR                                                          
         MVC   AIO,AIO2                                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   ESTNAME,EDESC                                                    
         MVC   DAYMENU,EDAYMENU                                                 
*                                                                               
GETEST1  LA    R0,MAXDEMS          MAX N'DEMOS                                  
         LA    R3,L'EDEMLST/3      MAX N'ESTIMATE HEADER DEMOS                  
         LA    RE,EDEMLST          POINT TO ESTIMATE HEADER DEMOS               
         LA    RF,DEMOS            POINT TO SCREEN DEMO LIST                    
         SR    R1,R1               R1=N'SCREEN DEMOS                            
*                                                                               
GETEST2  OC    0(3,RE),0(RE)       TEST FOR EOL                                 
         BZ    GETEST4             YES                                          
         CLI   1(RE),USERMOD       TEST FOR USER DEMO                           
         BE    GETEST3                                                          
         CLI   1(RE),WGTMOD        TEST FOR WEIGHTED DEMO                       
         BE    GETEST3                                                          
         MVC   0(3,RF),0(RE)       ADD DEMO TO LIST                             
         LA    RF,3(RF)            NEXT DEMO LIST ENTRY                         
         LA    R1,1(R1)            BUMP N'DEMOS                                 
         BCT   R0,*+8                                                           
         B     GETEST4                                                          
*                                                                               
GETEST3  LA    RE,3(RE)                                                         
         BCT   R3,GETEST2                                                       
*                                                                               
GETEST4  STC   R1,NDEMS            SET COUNTER                                  
         MVI   0(RF),X'FF'         SET EOL MARKER                               
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT DAYPART LIST                                              
*                                                                               
* AT ENTRY, WORK CONTAINS LIST AND 5(R2)=L'LIST                                 
*                                                                               
EDDPT    NTR1                                                                   
         LA    R1,DMCB                                                          
         MVC   0(2,R1),AGENCY                                                   
         MVC   2(1,R1),MEDIA                                                    
         MVC   3(1,R1),DAYMENU                                                  
         GOTO1 DPTRD,(R1),,AIO1,DATAMGR                                         
         CLI   8(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    8(R1),X'08'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EDDPT2   ZIC   R3,5(R2)            R3=INPUT LENGTH                              
         LA    R4,WORK             R4=A(INPUT)                                  
         XC    BLOCK(L'DAYPARTS),BLOCK                                          
         LA    R5,BLOCK            R5=A(OUTPUT)                                 
         SR    R6,R6               R6=N'DAYPARTS                                
         MVI   ERROR,INVDPT                                                     
*                                                                               
EDDPT3   LA    R0,36               MAX N'DAYPARTS                               
         L     RE,AIO1             RE=A(DAYPART MENU)                           
*                                                                               
EDDPT4   CLI   0(RE),0             TEST FOR EOT                                 
         BE    TRAPERR                                                          
         CLC   0(1,R4),0(RE)       MATCH ON INPUT DAYPART                       
         BE    EDDPT5                                                           
         LA    RE,5(RE)                                                         
         BCT   R0,EDDPT4                                                        
         B     TRAPERR                                                          
*                                                                               
EDDPT5   LA    R6,1(R6)            ADD INPUT DAYPART TO LIST                    
         CH    R6,=Y(L'DAYPARTS)                                                
         BH    TRAPERR                                                          
         MVC   0(1,R5),0(R4)                                                    
         LA    R5,1(R5)                                                         
*                                                                               
EDDPT6   ZIC   R1,1(RE)            ISOLATE FIRST NIBBLE                         
         SRL   R1,4                                                             
         LTR   R1,R1               TEST FOR ZERO                                
         BZ    EDDPT10             CANNOT HAVE CHILDREN                         
         LA    R0,36               FIND FIRST ENTRY WITH SAME VALUE             
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
*                                                                               
EDDPT7   IC    RF,1(RE)                                                         
         SRL   RF,4                                                             
         CR    RF,R1               TEST IF SAME SUB-DAYPART                     
         BE    *+12                YES                                          
         LA    RE,5(RE)                                                         
         BCT   R0,EDDPT7                                                        
*                                                                               
         CLC   0(1,R4),0(RE)       TEST FIRST VS. INPUT DAYPART                 
         BNE   EDDPT10             NOT THE SAME-MUST BE CHILD                   
         LA    RE,5(RE)            BUMP TO NEXT ENTRY                           
*                                                                               
EDDPT8   CLI   0(RE),0             TEST FOR EOL                                 
         BE    EDDPT10             YES                                          
         IC    RF,1(RE)                                                         
         SRL   RF,4                                                             
         CR    RF,R1               TEST FOR CHILD                               
         BNE   EDDPT10             NO                                           
         LA    R6,1(R6)            INCREMENT DAYPART COUNT                      
         CH    R6,=Y(L'DAYPARTS)                                                
         BH    TRAPERR             TOO MANY DAYPARTS                            
         MVC   0(1,R5),0(RE)       ADD DAYPART TO LIST                          
         LA    R5,1(R5)                                                         
*                                                                               
EDDPT9   LA    RE,5(RE)                                                         
         BCT   R0,EDDPT8                                                        
*                                                                               
EDDPT10  LA    R4,1(R4)            NEXT INPUT BYTE                              
         BCT   R3,EDDPT3                                                        
         MVC   DAYPARTS,BLOCK                                                   
*                                                                               
* BUILD DPTTAB - TABLE OF DAYPARTS FOR READ AND THEIR INTERNAL VALUES           
*                                                                               
         LA    R0,36               BUILD DPTTAB                                 
         L     RE,AIO1                                                          
         LA    R5,DPTTAB                                                        
*                                                                               
EDDPT14  CLI   0(RE),0             TEST FOR EOT                                 
         BE    EDDPTX                                                           
         LR    R1,R6               SET A COUNTER                                
         LA    R4,DAYPARTS                                                      
         CLC   0(1,R4),0(RE)                                                    
         BE    *+16                                                             
         LA    R4,1(R4)                                                         
         BCT   R1,*-14                                                          
         B     EDDPT15                                                          
*                                                                               
         MVC   0(2,R5),0(RE)       COPY ALPHA CODE/INTERNAL VALUE               
         LA    R5,2(R5)                                                         
*                                                                               
EDDPT15  LA    RE,5(RE)            NEXT DAYPART ENTRY                           
         BCT   R0,EDDPT14                                                       
*                                                                               
EDDPTX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE OPTIONS FIELD                                         
*                                                                               
VALOPT   NTR1                                                                   
         LA    R0,L'RNKOPT                                                      
         GOTO1 SCANNER,DMCB,((R0),(R2)),(2,AIO1),0                              
         CLI   4(R1),0                                                          
         BE    TRAPERR                                                          
         ZIC   R6,4(R1)            R6=N'FIELDS                                  
         L     R4,AIO1             R4=A(SCANNNER BLOCK)                         
*                                                                               
VALOPT1  CLI   0(R4),0                                                          
         BE    TRAPERR                                                          
         CLI   0(R4),5                                                          
         BH    TRAPERR                                                          
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              L(INES)=N                                    
         B     *+10                                                             
         CLC   12(0,R4),=C'LINES'                                               
         BNE   VALOPT2                                                          
*                                                                               
         CLI   1(R4),0                                                          
         BE    TRAPERR                                                          
         TM    3(R4),X'80'         TEST FOR NUMERIC PARAMETER                   
         BZ    TRAPERR                                                          
         ICM   R0,15,8(R4)                                                      
         BZ    TRAPERR                                                          
         CH    R0,=Y(MAXENTS)                                                   
         BH    TRAPERR                                                          
         STC   R0,LINFILT                                                       
         B     VALOPT6                                                          
*                                                                               
VALOPT2  EX    R1,*+8              ONLY D(EMOS)=DEM1/DEM2/...DEMN OK            
         B     *+10                                                             
         CLC   12(0,R4),=C'DEMOS'                                               
         BNE   TRAPERR                                                          
         CLI   1(R4),0                                                          
         BE    TRAPERR                                                          
         XC    BLOCK(256),BLOCK    BUILD DUMMY FIELD HEADER AND FIELD           
         LA    R0,L'RNKOPT+L'RNKOPTH                                            
         STC   R0,BLOCK                                                         
         MVC   BLOCK+5(1),1(R4)    LENGTH OF DEMO STRING                        
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+8(0),22(R4)                                                
         XC    DEMOS(MAXDEMS*3),DEMOS CLEAR OUTPUT LIST                         
*                                                                               
VALOPT4  L     R3,AIO3                                                          
         USING DBLOCKD,R3                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'                                                   
         BNE   *+16                                                             
         CLI   SVCXTRA,C'U'                                                     
         BE    *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         GOTO1 CDEMOVAL,DMCB,BLOCK,(4,DEMOS),(C'S',DBLOCK),(C'/',AIO2)          
         CLI   4(R1),0             TEST FOR ERROR                               
         BNE   *+12                                                             
         MVI   ERROR,INVDEMO                                                    
         B     TRAPERR                                                          
         MVC   NDEMS,4(R1)                                                      
         BAS   RE,VALDEM                                                        
*                                                                               
VALOPT6  LA    R4,22+L'RNKOPT(R4)                                               
         BCT   R6,VALOPT1                                                       
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE A DEMO LIST AGAINST ESTIMATE HEADER DEMOS             
* AT ENTRY, AIO2 POINTS TO ESTIMATE RECORD                                      
*                                                                               
VALDEM   NTR1                                                                   
         L     R4,AIO2                                                          
         USING ESTHDRD,R4                                                       
         ZIC   R0,NDEMS            COUNTER                                      
         LA    R1,DEMOS            R1=A(SCREEN DEMOS)                           
*                                                                               
VALDEM2  LA    RE,EDEMLST          RE=A(EST HD DEMOS)                           
         LA    RF,L'EDEMLST/3                                                   
*                                                                               
VALDEM3  OC    0(3,RE),0(RE)       TEST FOR EOL                                 
         BZ    VALDEMR                                                          
         CLC   0(3,R1),0(RE)       MATCH ON DEMO                                
         BE    VALDEM4                                                          
         LA    RE,3(RE)                                                         
         BCT   RF,VALDEM3                                                       
         B     VALDEMR                                                          
*                                                                               
VALDEM4  LA    R1,3(R1)            NEXT SCREEN DEMO                             
         BCT   R0,VALDEM2                                                       
         B     VALDEMX                                                          
*                                                                               
VALDEMR  MVI   ERROR,BADDEM                                                     
         B     TRAPERR                                                          
*                                                                               
VALDEMX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EXAMINE CURSOR POSITION AND PF KEYS TO TEST FOR                
* IMPLIED 'X' OR 'E' SELECT FIELD INPUT                                         
*                                                                               
LOOKCUR  ST    RE,SAVEREG                                                       
         LR    RE,R7               RE=A(TWA)                                    
         AH    RE,CURSDISP         RE=A(CURSOR FIELD)                           
         LA    R2,RNKSEL1H         R2=A(SELECT FIELD)                           
         LA    R0,MAXDETS          R0=LOOP COUNTER                              
*                                                                               
LOOKCUR1 CR    R2,RE               TEST IF CURSOR IN SELECT FIELD               
         BE    LOOKCUR2            YES                                          
         LA    R2,DISPSEL(R2)                                                   
         BCT   R0,LOOKCUR1                                                      
         B     LOOKCURX                                                         
*                                                                               
LOOKCUR2 MVI   BYTE,C'X'                                                        
         CLI   THISPF,PF1          TEST FOR COMPETITION                         
         BE    LOOKCUR4            YES                                          
         MVI   BYTE,C'E'                                                        
         CLI   THISPF,PF2          TEST FOR ESTIMATE                            
         BNE   LOOKCURX                                                         
*                                                                               
LOOKCUR4 MVC   8(L'RNKSEL1,R2),SPACES                                           
         MVC   8(1,R2),BYTE                                                     
         MVI   5(R2),1             SET INPUT LENGTH                             
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
LOOKCURX L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
********************************************************************            
*    I/O AREA CONVENTIONS IN RDSIR AND SUPPORTING SUB-ROUTINES     *            
*    IO1=DETAIL RECORD                                             *            
*    IO2=BINSRCH TABLE OF PRIMARY DEMO CPP/CPM                     *            
*    IO3=SR BLOCK                                                  *            
*        EQUIVALENCE RECORD                                        *            
********************************************************************            
         SPACE 2                                                                
* SUB-ROUTINE TO READ SIR DETAIL RECORDS                                        
*                                                                               
* ON EXIT, CC=NEQ IF RECORDS TO DISPLAY, CC=EQ IF NO RECORDS TO DISPLAY         
*                                                                               
RDSIR    NTR1                                                                   
*                                                                               
         MVC   BINATAB,AIO2        INITIALIZE BINSRCH PARMS                     
         L     RE,BINATAB          CLEAR BINSRCH TABLE                          
         LA    RF,MAXENTS*BINRECL                                               
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         XC    BINRECS,BINRECS                                                  
         LA    RE,BINRECL          SET L'BINSRCH RECORD                         
         ST    RE,BINLREC                                                       
         LA    RE,L'BINKEY         SET L'KEY FIELD                              
         ST    RE,BINLENK                                                       
         LA    RE,BINKEY-BINRECD   SET DISP TO KEY FIELD                        
         STC   RE,BINDISPK                                                      
         MVC   BINMAX,=AL4(MAXENTS) TABLE ENTRY LIMIT                           
*                                                                               
         L     R5,AIO3                                                          
         USING RANSIDD,R5                                                       
         LR    RE,R5                                                            
         LA    RF,SRBLKLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR SR BLOCK                               
         BAS   RE,GETEQU           GET EQUIV RECORD                             
*                                                                               
RDSIR2   MVC   SRASIR,AIO1         IO AREA FOR SIR RECORDS                      
         LA    RF,POST                                                          
         ST    RF,SRAHOOK                                                       
         MVC   SRACOM,ACOMFACS                                                  
         MVC   SRACLPAC,CLPACK                                                  
         MVC   SRAMSUNP,MSUNPK                                                  
         MVC   SRADYUNP,UNDAY                                                   
         MVC   SRAUNTIM,UNTIME                                                  
*                                                                               
         MVC   SRSELSCH,SCHEME                                                  
         MVC   SRSELAM,BAGYMD                                                   
         MVC   SRSELAGY,AGENCY                                                  
         MVC   SRSELMED,QMED                                                    
         MVC   SRSELPER,BUYPER                                                  
         MVC   SRSELSLN,LEN                                                     
         MVC   SRSELMKT,BMKT                                                    
         MVC   SRSELDPT,DAYPARTS                                                
         MVC   SRSELDP2,DAYPARTS+L'SRSELDPT                                     
         MVC   SRSELYR,YEAR                                                     
         MVC   SRSELCTY,SVAPROF+7                                               
*                                                                               
RDSIR4   GOTO1 RANSID,DMCB,(R5)                                                 
         CLI   SRERROR,SRNOERR     TEST IF RANSID OK                            
         BNE   *+16                                                             
         CLI   SRMODE,SRONEREC     TEST RECORD JUST RETURNED                    
         BE    RDSIR4              YES-READ NEXT ONE                            
         B     RDSIRX              NO                                           
*                                                                               
         CLI   SRERROR,SRNOMKT     TEST FOR MARKET/STATION ERROR                
         BH    RDSIR5              NO                                           
         LA    R2,RNKSTAH                                                       
         MVI   ERROR,INVMKT                                                     
         OC    BSTA,BSTA           TEST IF STATION INPUT                        
         BZ    *+8                 NO                                           
         MVI   ERROR,INVSTAT       YES                                          
         B     TRAPERR                                                          
*                                                                               
RDSIR5   LA    R2,RNKPERSH                                                      
         MVI   ERROR,INVBUYP                                                    
         CLI   SRERROR,SRNOPER     TEST FOR BUYING PERIOD ERROR                 
         BE    TRAPERR                                                          
         MVI   ERROR,INVSCH                                                     
         CLI   SRERROR,SRNOSCH                                                  
         BE    TRAPERR                                                          
         DC    H'0'                                                             
*                                                                               
RDSIRX   OC    SVNDETS,SVNDETS     SET CC ON EXIT                               
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO HANDLE POSTING TO DETAIL TABLE (HOOK ROUTINE)                  
*                                                                               
POST     NTR1  WORK=(R6,128)       GET SPACE FOR DEMAND I/O AREA                
*                                                                               
         CLI   SRMODE,SRONEREC                                                  
         BNE   POSTX                                                            
         CLC   SRACTPRO,SPACES     TEST FOR MISSING PROGRAM NAME                
         BNH   POSTX               YES-SKIP RECORD                              
*                                                                               
         CLI   SRCMPSRC,0          CAN WE CHECK RTG SVC AGAINST CLTHDR?         
         BE    POST0               NO -- WE MUST TAKE THE DETAIL RECORD         
*                                                                               
         LA    R3,ELEM             BUILD DBLOCK IN ELEM                         
         USING DBLOCKD,R3                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELSRC,C'A'       ASSUME ARBITRON                              
         CLI   SVCPROF+3,C'0'      CLIENT PROFILE IS NIELSON?                   
         BNE   *+8                                                              
         MVI   DBSELSRC,C'N'       YES                                          
         MVC   DBSELAGY,SRSELAGY                                                
         MVC   DBSELCLI,QCLT                                                    
         MVC   DBSELMED,SRSELMED                                                
         MVC   DBSELUMK,SRACTMKT                                                
         MVC   DBSELSTA,SRERSTAN                                                
         MVI   DBSELDAY,X'40'      DUMMY DAY AND TIME                           
         MVC   DBSELTIM(2),=AL2(600)                                            
         MVC   DBSELTIM+2(2),=AL2(615)                                          
         ST    R6,DBAREC                                                        
         L     RF,ACOMFACS                                                      
         ST    RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0  GOTO DEMAND                                  
         CLC   SRCMPSRC,DBACTSRC   DOES COMPETITION SOURCE MATCH?               
         BNE   POSTX               NO -- DON'T RANK THIS RECORD                 
         DROP  R3                                                               
*                                                                               
POST0    OC    SVACTSCH,SVACTSCH                                                
         BNZ   POST1                                                            
         MVC   SVYEAR,SRSELYR      SAVE THE YEAR                                
         MVC   SVACTSCH,SRACTSCH   EXTRACT VALUES FOR COMP REC READ             
         MVC   SVPERNUM,SRPERNUM                                                
         MVC   SVACTYR,SRACTYR                                                  
         XI    SVACTYR,X'FF'       REVERSE COMPLEMENT                           
*                                                                               
POST1    XC    DETREC,DETREC       CLEAR DETAIL ENTRY                           
         LA    R3,DETREC                                                        
         USING DETD,R3                                                          
         MVC   DETDA,SRACTDA                                                    
         BAS   RE,GETDEM           EXTRACT DEMO VALUES                          
*                                                                               
POST2    LA    R0,MAXDEMS          COUNTER                                      
         LA    RE,DEMVALS          RE=A(EXTRACTED DEMO VALUES)                  
         LA    RF,DETVAL1          RF=A(IST DEMO VALUE IN DETAIL REC)           
         MVC   0(3,RF),0(RE)                                                    
         LA    RE,3(RE)            NEXT VALUE                                   
         LA    RF,4(RF)            NEXT POSITION IN DETAIL RECORD               
         BCT   R0,*-14                                                          
*                                                                               
POST3    LA    R2,SRACTCST         R2=A(COST VALUES)                            
         LA    R4,4                R3=COST COUNTER                              
         XC    HALF,HALF           INITIALIZE EFFECTIVE DATE                    
*                                                                               
POST4    XC    ELEM(EBCLENEQ),ELEM                                              
         LA    R6,ELEM             SEARCH FOR BWS COST OVERRIDE                 
         USING EBCELEM,R6                                                       
         MVI   EBCCODE,EBCCODEQ                                                 
         MVC   EBCCLT,BCLT                                                      
         MVC   EBCSLN,SRSELSLN                                                  
         MVC   EBCEFF,DETEFF                                                    
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(EBCCODE,SRASIR),(6,EBCCLT),0           
         CLI   12(R1),0                                                         
         BNE   POST6                                                            
         L     R6,12(R1)                                                        
         OI    DETCTL,X'80'        SET BWS COST OVERRIDE PRESENT                
         MVC   DETCOST,EBCCOST+1                                                
*                                                                               
POST6    MVC   DETEFF,HALF         SET EFFECTIVE DATE                           
         TM    DETCTL,X'80'        TEST BWS OVERRIDE PRESENT                    
         BO    *+10                YES                                          
         MVC   DETCOST,4(R2)       EXTRACT COST                                 
         CLC   SRACTSLN,SRSELSLN   TEST IF SELECTED COST FOUND                  
         BE    *+16                YES                                          
         CLI   SRSELSLN,30         ONLY EQUIVALENCE IF 30 OR MORE SECS          
         BL    *+8                                                              
         BAS   RE,EQUIV            EQUIVALENCE TO SLN                           
         BAS   RE,PRIME            ADD DETAIL NTRY BASED ON PRIMARY DEM         
         LA    R2,7(R2)            NEXT COST                                    
         OC    0(3,R2),0(R2)       TEST FOR EFFECTIVE DATE                      
         BZ    POSTX               NO-ALL DONE                                  
         GOTO1 DATCON,DMCB,(3,0(R2)),(2,HALF)                                   
         BCT   R4,POST6                                                         
*                                                                               
POSTX    B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO EXTRACT DEMO VALUES FROM A SIR RECORD                          
*                                                                               
GETDEM   NTR1                                                                   
         USING DETD,R3                                                          
         XC    DEMVALS,DEMVALS                                                  
         ICM   R6,15,SROVELEM                                                   
         BZ    GETDEMX                                                          
         USING EDOELEM,R6                                                       
*                                                                               
GETDEM2  CLI   0(R6),0                                                          
         BE    GETDEMX                                                          
         CLI   0(R6),EDOCODEQ                                                   
         BE    GETDEM4                                                          
         CLI   0(R6),EDVCODEQ                                                   
         BE    GETDEM4                                                          
*                                                                               
GETDEM3  ZIC   R0,EDOLEN                                                        
         AR    R6,R0                                                            
         B     GETDEM2                                                          
*                                                                               
GETDEM4  ZIC   R1,NDEMS            R1=LOOP COUNTER                              
         LA    RE,DEMOS            RE=A(SCREEN DEMOS)                           
         SR    RF,RF               DEMO NUMBER INDEX                            
*                                                                               
GETDEM5  CLC   EDODEMO,1(RE)       MATCH ON DEMO                                
         BE    GETDEM6                                                          
         LA    RE,3(RE)            NEXT DEMO                                    
         LA    RF,1(RF)            INCREMENT INDEX NUMBER                       
         BCT   R1,GETDEM5                                                       
         B     GETDEM3             NEXT ELEMENT                                 
*                                                                               
GETDEM6  CLI   0(R6),EDOCODEQ      TEST FOR DEMO OVERRIDE                       
         BNE   GETDEM8             NO                                           
         LA    R1,X'40'            MASK FOR DEMO OVERRIDE                       
         SRL   R1,0(RF)            SHIFT TO POSITION FOR DEMO                   
         STC   R1,BYTE                                                          
         OC    DETCTL,BYTE         SET BIT ON IN CONTROL                        
*                                                                               
GETDEM8  MH    RF,=H'3'            DEVELOP INDEX INTO LIST                      
         LA    RE,DEMVALS(RF)                                                   
         SR    R0,R0                                                            
         ICM   R0,3,EDOVALUE       PICK UP OVERRIDE VALUE                       
         STCM  R0,7,0(RE)                                                       
         B     GETDEM3             NEXT ELEMENT                                 
*                                                                               
GETDEMX  B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO MAINTAIN BINSRCH TABLE ON PRIMARY DEMO CPP/CPM                 
* AND TO MANAGE THE ADDING OF DETAIL TABLE ENTRIES                              
*                                                                               
* AT ENTRY, DETREC CONTAINS NEW DETAIL ENTRY                                    
*                                                                               
PRIME    NTR1                                                                   
         LA    R3,DETREC           R3=A(DETAIL ENTRY TO BE ADDED)               
         USING DETD,R3                                                          
         LA    R4,BINREC           R4=A(NEW BINSRCH ENTRY)                      
         USING BINRECD,R4                                                       
         XC    BINREC,BINREC       CLEAR NEW BINSRCH ENTRY                      
*                                                                               
         SR    R0,R0               FIRST COMPUTE CPP/CPM                        
         ICM   R0,7,DETVAL1        GET PRIMARY DEMO VALUE                       
         BNZ   PRIME1              NON-ZERO DIVISOR                             
         L     RF,=F'-1'           FORCE ZERO DEMO/COST TO END                  
         OC    DETCOST,DETCOST     TEST FOR ZERO COST                           
         BZ    *+8                                                              
         L     RF,=F'-3'           NO-PUT COST NO DEMO ABOVE IT                 
         B     PRIME4                                                           
*                                                                               
PRIME1   SR    RF,RF                                                            
         ICM   RF,7,DETCOST        GET COST                                     
         BNZ   PRIME2                                                           
         L     RF,=F'-2'           FORCE ZERO COSTS/DEMOS TO                    
         B     PRIME4              BOTTOM ABOVE ZERO COST/ZERO DEMO             
*                                                                               
PRIME2   M     RE,=F'10'           SCALE THE COST FOR DIVISION                  
         SLDL  RE,1                                                             
         DR    RE,R0               ROUNDED DIVIDE                               
         AH    RF,=H'1'                                                         
         SRL   RF,1                                                             
*                                                                               
PRIME4   STCM  RF,7,BINCPP         SAVE CPP/CPM                                 
         LH    R1,SEQUENCE         ASSIGN UNIQUE SEQUENCE NUMBER                
         LA    R1,1(R1)            TO FORCE INSERTION OF ENTRY                  
         STH   R1,BINSEQ           EVEN IF IT HAS SAME CPP                      
         STH   R1,SEQUENCE                                                      
         ST    R4,BINAREC                                                       
         MVI   BINOP,BININS        INSERT ENTRY IN BINSRCH TABLE                
         GOTO1 BINSRCH,BINPARM                                                  
         OC    BINAREC,BINAREC     TEST IF TABLE FULL                           
         BZ    PRIME6              YES                                          
*                                                                               
         BAS   RE,GETSTA           GET STA NUMBER FOR DETAIL NTRY               
         GOTO1 ADDDET,DMCB,BINAREC,0                                            
         B     PRIMEX              ALL DONE                                     
*                                  TABLE IS FULL                                
PRIME6   MVC   BINMAX,=AL4(MAXENTS+1) MAKE TABLE 1 ENTRY LARGER                 
         LA    RE,BINRECD                                                       
         ST    RE,BINAREC                                                       
         MVI   BINOP,BININS                                                     
         GOTO1 BINSRCH,BINPARM                                                  
         ICM   R4,15,BINAREC       GET A(ENTRY ADD POINT)                       
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
         LA    R4,0(R4)            ZERO HOB (01 IS THERE)                       
         L     RE,BINATAB                                                       
         LA    RE,MAXENTS*BINRECL(RE) RE=A(LAST TABLE ENTRY)                    
         CR    R4,RE               TEST IF ADDED ENTRY IS LAST                  
         BNL   PRIME8              YES-FORGET THIS ITEM                         
*                                                                               
         ZIC   R6,BINENT-BINRECD(RE)  ENTRY NUMBER OF LAST ENTRY                
         BAS   RE,GETSTA                                                        
         GOTO1 ADDDET,DMCB,BINRECD,(R6)                                         
*                                                                               
PRIME8   MVC   BINMAX,=AL4(MAXENTS)   RESTORE TABLE SIZE LIMIT                  
         MVC   BINRECS,=AL4(MAXENTS)  AND NUMBER OF ENTRIES                     
*                                                                               
PRIMEX   B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET THE STATION NUMBER FOR A DETAIL ENTRY                      
*                                                                               
* AT ENTRY, SRACTSTA CONTAINS 3-BYTE STATION CODE, DETREC                       
* CONTAINS DETAIL ENTRY, SVSTATAB=A(STATION TABLE)                              
*                                                                               
GETSTA   NTR1                                                                   
         LA    RE,SVSTATAB         RE=A(STATION TABLE)                          
         LA    RF,1                RF=STATION NUMBER                            
         SR    R1,R1                                                            
         ICM   R1,1,SVNSTAS        R1=COUNTER                                   
         BZ    GETSTA4             ADD FIRST ENTRY                              
*                                                                               
GETSTA2  CLC   SRACTSTA,0(RE)                                                   
         BE    GETSTA6             FOUND STATION IN TABLE                       
         LA    RE,3(RE)                                                         
         LA    RF,1(RF)            INCREMENT STATION NUMBER                     
         BCT   R1,GETSTA2                                                       
*                                                                               
GETSTA4  CH    RF,=Y(MAXSTAS)      TEST FOR TABLE OVERFLOW                      
         BNH   *+6                                                              
         DC    H'0'                BLOW UP FOR NOW                              
         STC   RF,SVNSTAS          UPDATE N'STATIONS IN TABLE                   
         MVC   0(3,RE),SRACTSTA    ADD STATION TO TABLE                         
*                                                                               
GETSTA6  LA    R3,DETREC                                                        
         USING DETD,R3                                                          
         STC   RF,DETSTA           INSERT STATION NUMBER IN ENTRY               
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 1                                                                
* SUB-ROUTINE TO ADD A DETAIL ENTRY TO TABLE                                    
*                                                                               
* AT ENTRY, P1=A(BINSRCH TABLE ENTRY)                                           
*           P2=0 TO ADD TO END OF TABLE OR N TO REPLACE NTH ENTRY               
*           DETREC CONTAINS NEW DETAIL ENTRY                                    
*                                                                               
ADDDET   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         USING BINRECD,R2          R2=A(BINSRCH RECORD)                         
         LTR   R3,R3               TEST IF ADDING TO EOT                        
         BNZ   ADDDET2             NO                                           
         L     R3,SVNDETS                                                       
         LA    R3,1(R3)            INCREMENT DETAIL RECORD COUNT                
         ST    R3,SVNDETS                                                       
*                                                                               
ADDDET2  L     R4,ADETTAB                                                       
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         MH    RE,=Y(DETLN)        DISPLACEMENT TO ENTRY                        
         LA    R4,0(RE,R4)                                                      
         MVC   0(DETLN,R4),DETREC                                               
         STC   R3,BINENT           SAVE ENTRY NUM IN BINSRCH NTRY               
*                                                                               
ADDDETX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO COMPLETE THE DETAIL TABLE AT EOF ON SIR FILE                   
*                                                                               
* AT ENTRY, ADETTAB=A(DETAIL TAB), BINATAB=A(PRIMARY DEMO BINSRCH TAB)          
*                                                                               
WRAP     NTR1                                                                   
         L     R3,ADETTAB          R3=A(DETAIL LINE TABLE)                      
         L     R4,BINATAB                                                       
         USING BINRECD,R4                                                       
         MVC   SVCPP1+1(3),BINCPP  SAVE HIGHEST CPP/CPM                         
         L     R2,BINRECS          R2=COUNTER                                   
         LA    R1,1                RANK NUMBER                                  
         MVI   LASTRNK,1           INITIALIZE LAST RANK AND CPP                 
         XC    LASTCPP,LASTCPP                                                  
*                                                                               
*   SEED PRIMARY DEMO RANK INTO DETAIL TABLE                                    
*                                                                               
WRAP2    ZIC   R6,BINENT           GET ENTRY NUMBER                             
         BCTR  R6,0                                                             
         MH    R6,=Y(DETLN)        COMPUTE ITS INDEX INTO DET TAB               
         LA    R6,0(R3,R6)         R6=A(DETAIL ENTRY)                           
         USING DETD,R6                                                          
         MVC   DETRNK1,LASTRNK     INITIALIZE RANK                              
         CLC   LASTCPP,BINCPP      TEST SAME CPP/M AS PREVIOUS                  
         BE    *+12                YES                                          
         STC   R1,DETRNK1                                                       
         STC   R1,LASTRNK                                                       
         MVC   LASTCPP,BINCPP      UPDATE LAST CPP/M                            
         LA    R1,1(R1)            INCREMENT RNAK NUMBER                        
         LA    R4,BINRECL(R4)                                                   
         BCT   R2,WRAP2                                                         
*                                                                               
*   RANK SECONDARY DEMOS AND SEED RANKS INTO DETAIL TABLE                       
*                                                                               
WRAP4    ZIC   R2,SVNDEMS                                                       
         SH    R2,=H'1'            N'SECONDARY DEMOS                            
         BZ    WRAP6               NONE                                         
         LA    R1,2                R1=SECONDARY DEMO NUMBER                     
         BAS   RE,SORT             SORT AND RANK THAT DEMO                      
         LA    R1,1(R1)            INCREMENT DEMO NUMBER                        
         BCT   R2,*-8                                                           
*                                                                               
*   SORT THE DETAIL TABLE BASED ON PRIMARY DEMO RANK                            
*                                                                               
WRAP6    L     R2,SVNDETS                                                       
         LA    R3,DETRNK1-DETD                                                  
         GOTO1 QSORT,DMCB,ADETTAB,(R2),DETLN,L'DETRNK1,(R3)                     
         BAS   RE,PUTTAB           SAVE DETAIL TABLE ON TWA2                    
*                                                                               
WRAPX    B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO RANK SECONDARY DEMOS AND TO SEED THE RANKS                     
* BACK INTO DETAIL LINE TABLE                                                   
*                                                                               
* AT ENTRY, R1=SECONDARY DEMO NUMBER, ADETTAB=A(DETAIL TABLE)                   
*           SVNDETS=N'TABLE ENTRIES                                             
*                                                                               
SORT     NTR1                                                                   
         LR    R5,R1               SAVE DEMO NUMBER                             
         L     R2,SVNDETS          R2=LOOP COUNTER                              
         L     R3,ADETTAB          R3=A(TABLE)                                  
         USING DETD,R3                                                          
         L     R4,AIO1             R4=A(SORT TABLE)                             
         USING SORTRECD,R4                                                      
         LA    R1,1                R1=ENTRY NUMBER                              
*                                                                               
* BUILD A SORT TABLE - COMPUTE CPP/CPM FOR EACH DETAIL LINE                     
*                                                                               
SORT2    LR    R6,R5                                                            
         BCTR  R6,0                                                             
         SLL   R6,2                R6=INDEX INTO TABLE ENTRY DEMOS              
         LA    R6,DETDEMS(R6)      R6=A(DEMO VALUES)                            
         SR    R0,R0                                                            
         ICM   R0,7,0(R6)          GET DEMO VALUE                               
         BNZ   SORT3                                                            
         L     RF,=F'-1'           FORCE ZERO DEMO/ZERO COST TO END             
         OC    DETCOST,DETCOST                                                  
         BZ    *+8                                                              
         L     RF,=F'-3'           PUT COST/ZERO DEMO ABOVE IT                  
         B     SORT6                                                            
*                                                                               
SORT3    SR    RF,RF                                                            
         ICM   RF,7,DETCOST                                                     
         BNZ   SORT4                                                            
         L     RF,=F'-2'           ZERO COST/DEMO IN THE MIDDLE                 
         B     SORT6                                                            
*                                                                               
SORT4    M     RE,=F'10'           SCALE COST BEFORE DIVISION                   
         SLDL  RE,1                PREPARE FOR ROUNDED DIVIDE                   
         DR    RE,R0               COMPUTE CPP/CPM                              
         AH    RF,=H'1'                                                         
         SRL   RF,1                                                             
*                                                                               
SORT6    ST    RF,SORTRECD         SAVE VALUE                                   
         STC   R1,SORTENT          SAVE ENTRY NUMBER                            
         LA    R1,1(R1)            INCREMENT ENTRY NUMBER                       
         LA    R3,DETLN(R3)        NEXT TABLE ENTRY                             
         LA    R4,SORTRECL(R4)     NEXT SORT TABLE ENTRY                        
         BCT   R2,SORT2                                                         
*                                                                               
* SORT TABLE AND INITIALIZE TO RANK                                             
*                                                                               
SORT8    L     R0,SVNDETS                                                       
         LA    R3,SORTKEY-SORTRECD                                              
         GOTO1 QSORT,DMCB,AIO1,(R0),SORTRECL,L'SORTKEY,(R3)                     
         L     R2,SVNDETS          R2=LOOP COUNTER                              
         L     R3,ADETTAB          R3=A(TABLE)                                  
         L     R4,AIO1             R4=A(SORTED TABLE)                           
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         SLL   RE,2                INDEX INTO DETDEMS                           
         LA    RE,DETDEMS-DETD(RE) DISP TO SECONDARY DEMO                       
         LA    R1,1                R1=RANK NUMBER                               
         XC    LASTCPP,LASTCPP                                                  
         MVI   LASTRNK,1                                                        
*                                                                               
* WORK OUT RANKS FOR SORT TABLE ENTRIES AND SEED BACK INTO DETAIL TABLE         
*                                                                               
SORT10   ZIC   R6,SORTENT          GET ENTRY NUMBER                             
         BCTR  R6,0                                                             
         MH    R6,=Y(DETLN)                                                     
         AR    R6,R3                                                            
         LA    R6,0(RE,R6)         R6=A(DEMO SLOT)                              
         MVC   3(1,R6),LASTRNK     SET SAME RANK AS LAST ONE                    
         CLC   LASTCPP,SORTCPP     TEST CPP SAME AS BEFORE                      
         BE    *+12                YES                                          
         STC   R1,3(R6)            NO SET NEXT RANK NUMBER                      
         STC   R1,LASTRNK          UPDATE LAST RANK                             
         MVC   LASTCPP,SORTCPP                                                  
         LA    R1,1(R1)            INCREMENT RANK NUMBER                        
         LA    R4,SORTRECL(R4)     NEXT DEMO VALUE                              
         BCT   R2,SORT10                                                        
*                                                                               
* SAVE LOWEST CPP/CPM VALUE FOR DEMO                                            
*                                                                               
SORT12   L     R4,AIO1                                                          
         BCTR  R5,0                                                             
         SLL   R5,2                *4                                           
         LA    RE,SVCPP1(R5)       INDEX INTO FIRST DEMO                        
         MVC   1(3,RE),1(R4)       SAVE FIRST CPP/CPM VALUE                     
*                                                                               
SORTX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET EQUIVALENCE HEADER                                         
*                                                                               
GETEQU   NTR1                                                                   
         L     R4,AIO3             USE AREA RIGHT BEHIND SR BLOCK               
         LA    R4,SRBLKLN(R4)                                                   
         ST    R4,AEQUIV                                                        
         USING EQURECD,R4                                                       
         XC    DUB,DUB                                                          
         MVC   DUB(2),AGENCY                                                    
         MVC   DUB+2(1),QMED                                                    
         MVC   DUB+3(2),BCLT                                                    
         GOTO1 EQVRD,DMCB,DUB,EQUDPT,EQUSECT1,DATAMGR                           
         CLI   12(R1),0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO EQUIVALENCE RETURNED COST - CALLED FROM POST                   
* AFTER A SIR RECORD HAS BEEN RETURNED                                          
* AT ENTRY, R3=(DETAIL ENTRY), R5=A(SR BLOCK)                                   
*                                                                               
EQUIV    NTR1                                                                   
         USING DETD,R3                                                          
         L     R4,AEQUIV                                                        
         USING EQURECD,R4                                                       
         LA    R0,L'DAYPARTS                                                    
         LA    RE,DPTTAB                                                        
         CLC   SRACTDPT,0(RE)      MATCH ON DAYPART CODE                        
         BE    EQUIV2                                                           
         LA    RE,L'DPTTAB(RE)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
EQUIV2   ZIC   R6,1(RE)            DAYPART INTERNAL CODE                        
         N     R6,=F'15'           ZERO HIGH ORDER NIBBLE                       
         IC    R6,EQUDPT(R6)        TABLE IND                                   
         LTR   R6,R6                                                            
         BZ    *+6                                                              
         BCTR  R6,0                                                             
         MH    R6,=H'60'                                                        
*                                                                               
         LA    R6,EQUSECT1(R6)       EQUIV TABLE                                
         LA    R1,SLNTAB                                                        
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB+2(2),12(R6)     30 SECOND FACTOR                             
         MVC   DUB+6(2),12(R6)     IN CASE LENGTH NOT IN TABLE                  
*                                                                               
EQUIV4   CLI   0(R1),0             TEST FOR EOT                                 
         BE    EQUIV8              YES                                          
         CLC   SRSELSLN,0(R1)      MATCH ON LENGTH                              
         BE    EQUIV6                                                           
         LA    R6,4(R6)            EQUIV TABLE                                  
         LA    R1,1(R1)            LENGTH                                       
         B     EQUIV4                                                           
*                                                                               
EQUIV6   MVC   DUB+6(2),0(R6)      EQUIV FACTOR                                 
*                                                                               
EQUIV8   SR    RF,RF                                                            
         ICM   RF,7,DETCOST        GET 30 SECOND COST                           
         M     RE,DUB+4            30 SEC COST*SLN FACTOR                       
         SLDL  RE,1                                                             
         D     RE,DUB+0            /30 SEC FACTOR                               
         AH    RF,=H'1'                                                         
         SRL   RF,1                                                             
         STCM  RF,7,DETCOST        SAVE SLN COST                                
*                                                                               
EQUIVX   B     XIT                                                              
         DROP  R3,R4                                                            
         SPACE 2                                                                
SLNTAB   DS    0C                                                               
       ++INCLUDE SPSLNTAB                                                       
         DC    X'00'                                                            
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY THE DEMO NAMES ON SCREEN                               
*                                                                               
DEMN     NTR1                                                                   
         XC    RNKDEM,RNKDEM       CLEAR AND XMIT FIELD                         
         OI    RNKDEMH+6,X'80'                                                  
*                                                                               
DEMN2    LA    R5,BLOCK                                                         
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      TEST FOR CANADIAN AGENCY AND CLIENT          
         BNE   *+16                                                             
         CLI   SVCXTRA,C'U'                                                     
         BE    *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         ZIC   R2,SVNDEMS          R2=LOOP COUNTER                              
         LA    R3,SVDEMOS          R3=A(DEMO LIST)                              
         LA    R4,RNKDEM           R4=A(OUTPUT)                                 
*                                                                               
DEMN4    GOTO1 DEMOCON,DMCB,(1,0(R3)),(2,0(R4)),(C'S',DBLOCK),0                 
         LA    R3,3(R3)            NEXT DEMO VALUE                              
         LA    R4,8(R4)            NEXT OUTPUT AREA                             
         BCT   R2,DEMN4                                                         
*                                                                               
DEMNX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* PRINT REPORT FROM DETAIL TABLE                                                
*                                                                               
RPT      LA    R1,BUFF                                                          
         ST    R1,ADETTAB                                                       
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,RPTHOOK                                                       
         ST    R1,HEADHOOK                                                      
         L     RE,=A(FORM)                                                      
         A     RE,RELO                                                          
         ST    RE,AFORM                                                         
*                                                                               
RPT2     CLI   MYACT,REGULAR       TEST FOR SAME HEADLINE                       
         BE    RPT4                YES                                          
*                                                                               
         MVC   SVSTA,STA           NO                                           
         BAS   RE,RDSIR            READ THE FILE                                
         BE    RPTX                NOTHING FOUND                                
         BAS   RE,WRAP                                                          
         BAS   RE,VECTOR                                                        
         CLI   OFFLINE,C'Y'        TEST IF OFFLINE                              
         BE    *+8                 YES                                          
         BAS   RE,SCRN             NO-RE-BUILD FIRST PAGE                       
         B     RPT6                                                             
*                                                                               
RPT4     BAS   RE,GETTAB           FETCH THE TABLE                              
         CLC   STA,SVSTA           TEST FOR CHANGE IN STATION                   
         BE    RPT6                NO                                           
         MVC   SVSTA,STA                                                        
         BAS   RE,VECTOR                                                        
         BNE   RPTX                NOTHING TO PRINT                             
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    *+8                 YES                                          
         BAS   RE,SCRN             NO-DISPLAY FIRST PAGE                        
*                                                                               
RPT6     L     R2,SVNENTS          R2=ENTRIES TO PRINT                          
         SR    RE,RE                                                            
         ICM   RE,1,LINFILT                                                     
         BZ    *+12                                                             
         CR    RE,R2               TEST LIMIT IS LESS THAN N'ENTRIES            
         BH    *+6                 NO                                           
         LR    R2,RE               YES-USE IT AS LOOP COUNTER                   
         LA    R5,SVVECTOR         R5=A(DETAIL ENTRY VECTOR)                    
         LA    R3,1                R3=ENTRY NUMBER                              
*                                                                               
RPT8     ZIC   R0,0(R5)                                                         
         GOTO1 AFORM,DMCB,(RC),(X'FF',P+8),(R0)                                 
         EDIT  (R3),(3,P+1),ALIGN=LEFT                                          
         MVI   ALLOWLIN,4                                                       
         GOTO1 SPOOL,PARAS,ASPOOLD                                              
         GOTO1 (RF),(R1),ASPOOLD                                                
         LA    R5,1(R5)            BUMP VECTOR POINTER                          
         LA    R3,1(R3)            INCREMENT ENTRY NUMBER                       
         BCT   R2,RPT8                                                          
*                                                                               
RPTX     B     XIT                 RETURN TO GENCON                             
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY FIRST SCREEN                                           
*                                                                               
SCRN     ST    RE,SAVEREG                                                       
         GOTO1 CLEARF,DMCB,(1,RNKDEMH),RNKLAST                                  
         GOTO1 (RF),(R1),(0,RNKSEL1H),RNKLAST                                   
         BAS   RE,DEMN             DISPLAY DEMO NAMES                           
         MVC   RNKPF(L'PFMSG),PFMSG  PF KEY LEGEND                              
*                                                                               
         LA    R2,RNKSEL1H                                                      
         LA    R3,1                                                             
         LA    R4,MAXDETS                                                       
         C     R4,SVNENTS          TEST IF PAST HIGHEST ENTRY                   
         BNH   *+8                                                              
         L     R4,SVNENTS          YES                                          
         STC   R3,SVFIRST          SAVE FIRST ENTRY ON SCREEN                   
         STC   R4,SVLAST           AND LAST ENTRY                               
         LA    R5,SVVECTOR         R5=A(VECTOR TABLE)                           
*                                                                               
SCRN2    ZIC   R3,0(R5)            GET DETAIL ENTRY NUMBER                      
         GOTO1 AFORM,DMCB,(RC),(R2),(R3)                                        
         LA    R2,DISPSEL(R2)                                                   
         LA    R5,1(R5)                                                         
         BCT   R4,SCRN2                                                         
*                                                                               
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* REPORT HEADLINE HOOK                                                          
*                                                                               
RPTHOOK  NTR1                                                                   
         MVC   H1+10(1),QMED       MEDIA                                        
         MVC   H2+10(3),QCLT       CLIENT CODE                                  
         MVC   H2+17(20),CLTNM                                                  
         MVC   H3+10(3),PROD       PRODUCT CODE                                 
         MVC   H3+17(20),PRDNM                                                  
         ZIC   R0,EST                                                           
         EDIT  (R0),(3,H4+10),ALIGN=LEFT                                        
         MVC   H4+17(20),ESTNAME                                                
         OC    STA,STA             TEST IF MARKET INPUT                         
         BZ    *+14                YES                                          
         MVC   H5+10(L'STAPRNTN),STAPRNTN                                       
         B     RPTHOOK1                                                         
*                                                                               
         MVC   H5+10(L'QMKT),QMKT  MARKET NUMBER                                
         MVC   H5+17(L'MKTNM),MKTNM                                             
*                                                                               
RPTHOOK1 MVC   H6+10(L'RNKDPT),RNKDPT  DAYPARTS FROM SCREEN                     
         ZIC   R0,LEN                                                           
         EDIT  (R0),(3,H5+81),ALIGN=LEFT                                        
         MVC   H6+85(4),BUYPER                                                  
         MVC   H6+97(3),SCHEME                                                  
         ZIC   R0,SVYEAR                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H6+107(2),DUB+6(2)                                               
         OC    RNKDEM,SPACES                                                    
         MVC   H8+54(L'RNKDEM),RNKDEM                                           
*                                                                               
RPTHOOKX B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PUT DETAIL TABLE FROM TO TWA 2                                 
*                                                                               
PUTTAB   ST    RE,FULL                                                          
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,2            PAGE=TWA2                                    
         MVC   DMCB+10(2),2(R7)    TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,DMWRT,TEMPSTR,,ADETTAB                              
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET SAVED DETAIL TABLE FROM TWA2                               
*                                                                               
GETTAB   ST    RE,FULL                                                          
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,2                                                         
         MVC   DMCB+10(2),2(R7)                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,TEMPSTR,,ADETTAB                             
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
PATCH    DS    0H                  PATCH AREA                                   
         DC    XL32'00'                                                         
         SPACE 1                                                                
DMWRT    DC    C'DMWRT '                                                        
DMREAD   DC    C'DMREAD'                                                        
TEMPSTR  DC    C'TEMPSTR'                                                       
NODATA   DC    C'** NO DETAIL RECORDS FOUND **'                                 
DISMSG   DC    C'DISPLAYED - ENTER CHANGES **'                                  
CHAMSG   DC    C'** CHANGES COMPLETED **'                                       
PFMSG    DC    C'W=BWS XFR(DW=DEL),X=COMP,E=CES  PF1=COMP,PF2=CES,PF5=RX        
               E-RANK,PF7=BACK,PF8=FWD'                                         
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* PRINT SPEC POOL                                                               
*                                                                               
HEDSPECS DS    0D                                                               
         SSPEC H1,2,C'MEDIA'                                                    
         SSPEC H1,47,C'NSID RANK REPORT'                                        
         SSPEC H2,47,C'----------------'                                        
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H2,2,C'CLIENT'                                                   
         SSPEC H3,2,C'PRODUCT'                                                  
         SSPEC H4,2,C'ESTIMATE'                                                 
         SSPEC H5,2,C'STATION'                                                  
         SSPEC H6,2,C'DAYPART'                                                  
         SSPEC H3,75,REPORT                                                     
         SSPEC H3,87,RUN                                                        
         SSPEC H4,75,REQUESTOR                                                  
         SSPEC H4,100,PAGE                                                      
         SSPEC H5,75,C'LENGTH'                                                  
         SSPEC H6,75,C'BUY PERIOD'                                              
         SSPEC H6,91,C'SCHEME'                                                  
         SSPEC H6,102,C'YEAR'                                                   
         SSPEC H8,2,C'LINE'                                                     
         SSPEC H8,9,C'---------INVENTORY DETAILS----------'                     
         DC    X'00'                                                            
         EJECT                                                                  
* SUB-ROUTINE TO FORMAT A DETAIL LINE ON THE SCREEN                             
*                                                                               
* AT ENTRY, P1=A(WORKING STORAGE), P2=A(SELECT FLDH OR PRT LINE START)          
*           P3=DETAIL ENTRY NUMBER                                              
*                                                                               
FORM     NMOD1 0,**FORM                                                         
         L     RC,0(R1)            RESTORE WRK STORAGE ADDRESSABILITY           
         LM    R2,R3,4(R1)                                                      
         USING PRTD,R2                                                          
         MVC   FORMMODE,4(R1)      SAVE FORMAT MODE                             
         BCTR  R3,0                                                             
         MH    R3,=Y(DETLN)        COMPUTE INDEX FOR DETAIL LINE                
         A     R3,ADETTAB          R3=A(DETAIL ENTRY)                           
         USING DETD,R3                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),DETDA     SET DISK ADDRESS                             
         GOTO1 GETREC                                                           
*                                                                               
* INDICATE PREVIOUS WORKSHEET TRANSFER                                          
*                                                                               
FORM1    MVI   ELCODE,EBWCODEQ                                                  
         MVC   DUB(2),BCLT         FILTER DATA                                  
         MVC   DUB+2(1),BPRD                                                    
         MVC   DUB+3(1),SVEST                                                   
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,AIO),(4,DUB)                    
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   FORM2               NO                                           
         CLI   FORMMODE,X'FF'      TEST FOR PRINT                               
         BE    *+14                                                             
         MVC   8(2,R2),=C'*W'                                                   
         B     FORM2                                                            
         MVC   PRTSEL,=C'*W'                                                    
*                                                                               
* DISPLAY DETAIL DATA - STATION/DAYPART/DAY/TIME                                
*                                                                               
FORM2    MVC   WORK,SPACES         CLEAR OUTPUT AREA                            
         XC    DUB,DUB                                                          
         MVC   DUB(2),SVMKT        MARKET                                       
         ZIC   RE,DETSTA           GET STATION NUMBER                           
         BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         LA    RE,SVSTATAB(RE)     RE=STATION ENTRY                             
         MVC   DUB+2(3),0(RE)                                                   
         GOTO1 MSUNPK,DMCB,(X'80',DUB),FULL,WORK                                
         MVC   CSTA,WORK                                                        
         CLI   CSTA+4,C' '         TEST FOR TV                                  
         BNE   *+8                                                              
         MVI   CSTA+4,C'T'         YES-FORCE BAND TO 'T'                        
*                                                                               
         L     R4,AIO                                                           
         USING SIRREC,R4                                                        
         MVC   WORK+9(1),SIRKDPT   DAYPART                                      
         LR    R6,R4                                                            
         MVI   ELCODE,EDPCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EDPELEM,R6                                                       
         MVC   CDAY,EDPDAY         SAVE DAY/TIME FOR COMP REC READ              
         MVC   CTIME,EDPTIME                                                    
         MVC   BLOCK(7),SPACES                                                  
         GOTO1 UNDAY,DMCB,EDPDAY,BLOCK                                          
         MVC   WORK+12(7),BLOCK    DAY EXPRESSION                               
         MVC   BLOCK(11),SPACES                                                 
         GOTO1 UNTIME,DMCB,EDPTIME,BLOCK                                        
         MVC   WORK+21(11),BLOCK                                                
         LA    R5,DISPDET+8(R2)      ADD DISP TO DETAIL FLD                     
         CLI   FORMMODE,X'FF'      TEST FOR PRINTING                            
         BNE   *+8                                                              
         LA    R5,PRTDET                                                        
         MVC   0(L'RNKDET1,R5),WORK                                             
*                                                                               
* DISPLAY DEMO VALUES                                                           
*                                                                               
FORM4    MVC   BLOCK(L'RNKDEM1),SPACES                                          
         MVC   BLOCK(5),=C'DEMOS'                                               
         LA    R4,DETVAL1          R4=A(DEMO VALUES)                            
         LA    R6,BLOCK+7          R7=A(OUTPUT AREA)                            
         ZIC   R5,SVNDEMS          R5=LOOP COUNTER                              
         MVI   BYTE,X'40'          INITIALIZE DEMO OVERRIDE MASK                
*                                                                               
FORM5    SR    R0,R0                                                            
         ICM   R0,7,0(R4)          GET VALUE                                    
*                                                                               
         ZIC   R1,BYTE             GET OVERRIDE MASK                            
         LR    RF,R1                                                            
         SRL   RF,1                SHIFT TO NEXT POSITION                       
         STC   RF,BYTE             REPLACE WITH NEW MASK                        
         EX    R1,TSTOV            TEST FOR DEMO OVERRIDE                       
         BO    FORM5A              YES                                          
*                                                                               
         EDIT  (R0),(7,0(R6)),1                                                 
         B     FORM6                                                            
*                                                                               
FORM5A   EDIT  (R0),(7,0(R6)),1,FLOAT=*                                         
*                                                                               
FORM6    LA    R4,DETVAL2-DETVAL1(R4) NEXT DEMO VALUE                           
         LA    R6,8(R6)                                                         
         BCT   R5,FORM5                                                         
*                                                                               
FORM7    LA    R5,DISPDEM+8(R2)                                                 
         CLI   FORMMODE,X'FF'                                                   
         BNE   *+8                                                              
         LA    R5,PRTDEM                                                        
         MVC   0(L'RNKDEM1,R5),BLOCK                                            
*                                                                               
* DISPLAY PROGRAM AND COST EFFECTIVE DATE                                       
*                                                                               
FORM8    MVC   WORK(L'RNKPRG1),SPACES                                           
         BAS   RE,GETPRG           GET PROGRAM NAME IF ANY                      
         OC    DETEFF,DETEFF       TEST FOR FIRST COST                          
         BZ    FORM9               YES                                          
         GOTO1 DATCON,DMCB,(2,DETEFF),(6,WORK+18)                               
*                                                                               
FORM9    LA    R5,DISPPRG+8(R2)                                                 
         CLI   FORMMODE,X'FF'                                                   
         BNE   *+8                                                              
         LA    R5,PRTPRG                                                        
         MVC   0(L'RNKPRG1,R5),WORK                                             
*                                                                               
* DISPLAY COST                                                                  
*                                                                               
FORM10   SR    R0,R0                                                            
         ICM   R0,7,DETCOST                                                     
         MVC   BLOCK(L'RNKCOS1),SPACES                                          
         EDIT  (R0),(10,BLOCK),2,ALIGN=LEFT                                     
         TM    DETCTL,X'80'        TEST IF BWS COST OVERRIDE                    
         BZ    FORM11                                                           
         MVC   WORK(L'RNKCOS1),BLOCK                                            
         MVI   BLOCK,C'*'          INSERT A STAR BEFORE COST                    
         MVC   BLOCK+1(L'RNKCOS1-1),WORK                                        
*                                                                               
FORM11   LA    R5,DISPCOS+8(R2)                                                 
         CLI   FORMMODE,X'FF'                                                   
         BNE   *+8                                                              
         LA    R5,PRTCOS                                                        
         MVC   0(L'RNKCOS1,R5),BLOCK                                            
*                                                                               
* DISPLAY CPP/CPM DATA                                                          
*                                                                               
FORM12   BAS   RE,CPP              COMPUTE CPP                                  
         MVC   BLOCK(L'RNKCPP1),SPACES                                          
         MVC   BLOCK(5),=C'CPP/M'                                               
         ZIC   R6,SVNDEMS                                                       
         LA    R5,CPPVALS                                                       
         LA    R4,BLOCK+7                                                       
*                                                                               
FORM14   L     R0,0(R5)            GET CPP VALUE                                
         LTR   R0,R0               TEST FOR NEGATIVE CPP/CPM                    
         BM    FORM15              YES                                          
         EDIT  (R0),(7,0(R4)),2                                                 
         B     FORM16                                                           
*                                                                               
FORM15   MVC   3(2,R4),=C'NA'                                                   
*                                                                               
FORM16   LA    R5,4(R5)            NEXT CPP/M                                   
         LA    R4,8(R4)            NEXT OUTPUT POSITION                         
         BCT   R6,FORM14                                                        
*                                                                               
         LA    R5,DISPCPP+8(R2)                                                 
         CLI   FORMMODE,X'FF'                                                   
         BNE   *+8                                                              
         LA    R5,PRTCPP                                                        
         MVC   0(L'RNKCPP1,R5),BLOCK                                            
*                                                                               
* DISPLAY COMMENT IF ANY                                                        
*                                                                               
FORM18   L     R6,AIO                                                           
         MVI   ELCODE,ECOCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   FORM20                                                           
         USING ECOELEM,R6                                                       
         MVC   WORK(L'RNKCOM1),SPACES                                           
         ZIC   R1,ECOLEN                                                        
         SH    R1,=H'3'                                                         
         LA    RF,L'RNKCOM1-1                                                   
         CR    R1,RF               CANNOT MOVE OUT MORE THAN FIELD              
         BL    *+6                                                              
         LR    R1,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ECOMMENT                                                 
         LA    R5,DISPCOM+8(R2)                                                 
         CLI   FORMMODE,X'FF'                                                   
         BNE   *+8                                                              
         LA    R5,PRTCOM                                                        
         MVC   0(L'RNKCOM1,R5),WORK                                             
*                                                                               
* DISPLAY INDICES AND RANKS                                                     
*                                                                               
FORM20   MVC   BLOCK(L'RNKIDX1),SPACES                                          
         MVC   BLOCK(5),=C'IDX/R'                                               
         BAS   RE,INDEX                                                         
         LA    R5,DISPIDX+8(R2)                                                 
         CLI   FORMMODE,X'FF'                                                   
         BNE   *+8                                                              
         LA    R5,PRTIDX                                                        
         MVC   0(L'RNKIDX1,R5),BLOCK                                            
*                                                                               
FORMX    XIT1                                                                   
         DROP  R2,R6                                                            
         SPACE 2                                                                
TSTOV    TM    DETCTL,0                                                         
         EJECT                                                                  
* SUB-ROUTINE TO GET THE PROGRAM NAME FROM COMPETITION RECORD                   
* ON EXIT, WORK(17) CONTAINS PROGRAM NAME                                       
*                                                                               
GETPRG   NTR1                                                                   
*                                                                               
         CLI   CSTA,X'F0'          LOCAL CABLE HEADEND?                         
         BNL   GETPRGX             YES -- FORGET ABOUT COMPETITION              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CMPKEY,R6                                                        
         MVI   CMPKTYP,CMPKTYPQ                                                 
         MVI   CMPKFIL,CMPKFILQ                                                 
         MVC   CMPKAM,BAGYMD                                                    
         MVC   CMPKCODE,SVACTSCH                                                
         MVC   CMPKMKT,SVMKT                                                    
         MVC   CMPKDAY,CDAY                                                     
         MVC   CMPKSTIM(4),CTIME                                                
*                                                                               
         MVC   BYTE,SVPERNUM       GET PERIOD NUMBER                            
         NI    BYTE,X'0F'          ZERO HIGH ORDER NIBBLE                       
         ZIC   R1,SVACTYR                                                       
         SR    R0,R0                                                            
         D     R0,=F'10'           ISOLATE YEAR NUMBER AS REMAINDER             
         SLL   R0,4                SHIFT YEAR TO HIGH ORDER NIBBLE              
         STC   R0,CMPKYM                                                        
         OC    CMPKYM,BYTE         COMBINE YEAR AND PERIOD                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   CMPKEY,KEYSAVE                                                   
         BNE   GETPRGX             DID NOT FIND RECORD                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
GETPRG2  L     R6,AIO                                                           
         MVI   ELCODE,CMSCODEQ     GET COMPETING STATION ELEM                   
         BAS   RE,GETEL                                                         
         BNE   GETPRGX                                                          
*                                                                               
         USING CMSEL,R6                                                         
         SR    R0,R0                                                            
         ZIC   R1,CMSLEN           GET ELEM LEN                                 
         SH    R1,=H'2'            SUBTRACT CODE+LEN                            
         D     R0,=F'5'            R1=N'STATIONS IN ELEM                        
         LA    R5,1                R5=STATION NUMBER                            
         LA    RE,CMSSTA           RE=A(STATION ENTRY)                          
*                                                                               
GETPRG4  CLC   CSTA,0(RE)          MATCH ON STATION                             
         BE    GETPRG6                                                          
         LA    R5,1(R5)            INCREMENT STATION NUMBER                     
         LA    RE,L'CMSSTA(RE)                                                  
         BCT   R1,GETPRG4                                                       
         B     GETPRGX             DID NOT FIND STATION IN ELEM                 
*                                                                               
GETPRG6  MVI   ELCODE,CMPCODEQ     GET PROGRAM NAME FOR STATION                 
         STC   R5,BYTE             STATION NUMBER=SEQUENCE NUMBER               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,AIO),(1,BYTE),0                 
         CLI   12(R1),0            TEST IF ELEM FOUND                           
         BE    GETPRG8                                                          
*                                                                               
         OI    BYTE,X'80'          NOW SEARCH FOR OVERRIDE NAME                 
         GOTO1 (RF),(R1),(C'G',SYSFIL),(ELCODE,AIO),(1,BYTE),0                  
         CLI   12(R1),0            TEST IF ELEM FOUND                           
         BNE   GETPRGX             NO                                           
*                                                                               
GETPRG8  L     R6,12(R1)                                                        
         USING CMPEL,R6                                                         
         MVC   WORK(L'CMPPROG),CMPPROG EXTRACT PROGRAM NAME                     
*                                                                               
GETPRGX  MVC   AIO,AIO1            RESTORE AIO POINTER                          
         B     FORMX                                                            
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO COMPUTE CPP/CPM FOR DETAIL ENTRY'S DEMOS                       
* ON ENTRY, R3=A(DETAIL ENTRY)  ON EXIT, CPPVALS CONTAINS                       
* CALCULATED CPP/CPM                                                            
*                                                                               
CPP      NTR1                                                                   
         XC    CPPVALS(MAXDEMS*4),CPPVALS                                       
         ZIC   R2,SVNDEMS          R2=LOOP COUNTER                              
         LA    R5,DETVAL1          R5=A(DEMO VALUES)                            
         LA    R6,CPPVALS          R6=A(OUTPUT)                                 
*                                                                               
CPP2     SR    RF,RF                                                            
         ICM   RF,7,DETCOST        GET COST                                     
         SR    R0,R0                                                            
         ICM   R0,7,0(R5)          DEMO IS DIVISOR                              
         BNZ   CPP4                                                             
         MVC   0(4,R6),=F'-1'      INDICATE ZERO DIVISOR                        
         B     CPP6                                                             
*                                                                               
CPP4     M     RE,=F'10'                                                        
         SLDL  RE,1                                                             
         DR    RE,R0                                                            
         AH    RF,=H'1'                                                         
         SRL   RF,1                                                             
         ST    RF,0(R6)                                                         
*                                                                               
CPP6     LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         BCT   R2,CPP2                                                          
         B     FORMX                                                            
         EJECT                                                                  
* SUB-ROUTINE TO COMPUTE AND DISPLAY INDICES AND THEIR RANKS                    
*                                                                               
* AT ENTRY, R3=A(DETAIL ENTRY)                                                  
* ON EXIT, BLOCK+7=A(OUTPUT)                                                    
*                                                                               
INDEX    NTR1                                                                   
         ZIC   R2,SVNDEMS          R2=LOOP COUNTER                              
         LA    R3,DETRNK1-DETD(R3) R3=A(RANK)                                   
         LA    R4,CPPVALS          CPP/CPM FOR DEMOS                            
         LA    R5,SVCPP1           R5=A(INDEX BASES)                            
         LA    R6,BLOCK+7          R6=A(OUTPUT)                                 
*                                                                               
INDEX2   L     RF,0(R4)            GET CPP/CPM FOR DEMO                         
         LTR   RF,RF                                                            
         BM    INDEX3                                                           
         M     RE,=F'100'          *100 TO SCALE FOR BASE                       
         SLDL  RE,1                                                             
         OC    0(4,R5),0(R5)       TEST FOR ZERO DIVISOR                        
         BZ    INDEX3                                                           
         D     RE,0(R5)                                                         
         AH    RF,=H'1'                                                         
         SRL   RF,1                                                             
         LR    R0,RF                                                            
         CH    R0,=H'999'          NO INDEX CAN BE MORE THAN 999                
         BL    *+8                                                              
         LA    R0,999                                                           
         EDIT  (R0),(3,0(R6)),ZERO=NOBLANK                                      
         B     INDEX4                                                           
*                                                                               
INDEX3   MVC   1(2,R6),=C'NA'                                                   
*                                                                               
INDEX4   MVI   3(R6),C'/'                                                       
         ZIC   R0,0(R3)            GET RANK                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R6),DUB+5(3)                                                 
*                                                                               
INDEX6   LA    R3,DETRNK2-DETRNK1(R3) NEXT RANK FIELD                           
         LA    R4,4(R4)            NEXT CPP/M                                   
         LA    R5,4(R5)            NEXT BASE CPP/M                              
         LA    R6,8(R6)            NEXT OUTPUT AREA                             
         BCT   R2,INDEX2                                                        
         B     FORMX                                                            
         EJECT                                                                  
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
* LITERAL POOL FOR FORM                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SUB-ROUTINE TO EDIT FIELDS FOR A DETAIL LINE                                  
* AT ENTRY, P1 = A(WORKING STORAGE)                                             
*           P2 = A(SELECT FIELD)                                                
*           P3 = DETAIL ENTRY NUMBER                                            
*           EDTMODE = C'E'-EDIT ONLY,C'U'-EDIT AND UPDATE                       
*                                                                               
EDT      NMOD1 0,**EDT**                                                        
         L     RC,0(R1)                                                         
         LM    R4,R5,4(R1)                                                      
         LR    R3,R5                                                            
         BCTR  R3,0                                                             
         MH    R3,=Y(DETLN)                                                     
         A     R3,ADETTAB                                                       
         USING DETD,R3                                                          
*                                                                               
         MVI   PUTSW,C'N'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),DETDA                                                  
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AIO                                                           
         MVC   SAVEKEY,0(RE)       SAVE ORIGINAL KEY                            
*                                                                               
EDT2     XC    DUB,DUB                                                          
         MVI   DUB,C'N'            SET FOR NO COST OVERRIDE                     
         TM    DETCTL,X'80'        TEST FOR COST OVERRIDE                       
         BZ    *+8                                                              
         MVI   DUB,C'Y'            YES-NOTE COST OVERRIDE                       
         MVC   DUB+1(3),DETCOST                                                 
*                                                                               
         LA    R2,DISPCOS(R4)      EDIT COST FIELD                              
         ZIC   R0,5(R2)                                                         
         LTR   R0,R0                                                            
         BNZ   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERRX                                                          
*                                                                               
         LA    R6,8(R2)            R6=A(INPUT DATA)                             
         MVI   DUB+4,C'N'          SET NO COST OVERRIDE                         
         CLI   0(R6),C'*'          TEST FOR COST OVERRIDE SYMBOL                
         BNE   EDT3                                                             
         SH    R0,=H'1'            YES-DECREMENT INPUT LENGTH                   
         BZ    EDTERRX                                                          
         LA    R6,1(R6)            BUMP INPUT POINTER AHEAD                     
         MVI   DUB+4,C'Y'          NOTE COST OVERRIDE INPUT                     
*                                                                               
EDT3     GOTO1 CASHVAL,DMCB,(R6),(R0)                                           
         MVI   ERROR,INVALID                                                    
         CLI   0(R1),X'FF'                                                      
         BE    EDTERRX                                                          
         MVC   DUB+5(3),5(R1)                                                   
         MVC   NEWCOST,5(R1)                                                    
         CLC   DUB(4),DUB+4        TEST FOR COST CHANGES                        
         BE    EDT4                NO                                           
*                                                                               
         MVI   CHASW,C'Y'          SET ONE CHANGE FLAG                          
         MVC   NEWCOST,5(R1)                                                    
         MVI   PUTSW,C'Y'                                                       
         BAS   RE,UPCOST                                                        
*                                                                               
EDT4     LA    R2,DISPCOM(R4)                                                   
         MVI   ELCODE,ECOCODEQ                                                  
         GOTO1 REMELEM                                                          
         CLI   5(R2),0             TEST FOR ANY INPUT                           
         BNE   EDT5                YES                                          
*                                                                               
         CLI   ELEM,0              TEST IF COMMENT WAS THERE                    
         BE    EDT6                NO                                           
         MVI   CHASW,C'Y'                                                       
         MVI   PUTSW,C'Y'                                                       
         B     EDT6                                                             
*                                                                               
EDT5     LA    R6,ELEM             COMMENT WAS INPUT                            
         USING ECOELEM,R6                                                       
         GOTO1 ANY                                                              
         CLI   ELEM,0              TEST FOR COMMENT ON RECORD                   
         BE    EDT5A               NO-ITS A CHANGE                              
*                                                                               
         MVC   BLOCK(L'RNKCOM1),SPACES                                          
         ZIC   R1,ECOLEN                                                        
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),ECOMMENT                                                
         CLC   WORK(L'RNKCOM1),BLOCK    TEST SAME COMMENT                       
         BNE   EDT5A               NO                                           
         GOTO1 ADDELEM             YES-RESTORE ELEMENT                          
         B     EDT6                                                             
*                                                                               
EDT5A    MVI   CHASW,C'Y'                                                       
         CLI   EDTMODE,C'E'                                                     
         BE    EDT6                                                             
*                                                                               
         MVI   ECOCODE,ECOCODEQ                                                 
         ZIC   R1,5(R2)            GET COMMENT LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ECOMMENT(0),WORK                                                 
         LA    R1,ECOMMENT-ECOELEM+1(R1)                                        
         STC   R1,ECOLEN                                                        
         GOTO1 ADDELEM                                                          
         MVI   PUTSW,C'Y'                                                       
*                                                                               
EDT6     LR    R2,R4               EDIT SELECT FIELD                            
         CLI   5(R2),0             TEST FOR ANY INPUT                           
         BE    EDT8                                                             
         CLI   8(R2),C'*'          TEST NO-OP                                   
         BE    EDT8                YES                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'W'          TEST FOR BWS XFR                             
         BE    EDT7                                                             
         CLI   8(R2),C'X'          TEST FOR COMPETITION                         
         BE    EDTCOMP                                                          
         CLI   8(R2),C'E'          TEST FOR ESTIMATE (TREND)                    
         BE    EDTEST                                                           
         CLC   8(2,R2),=C'DW'      TEST FOR DELETE BWS XFR                      
         BNE   EDTERRX                                                          
*                                                                               
EDT7     MVI   CHASW,C'Y'                                                       
         CLI   EDTMODE,C'E'        TEST EDIT ONLY                               
         BE    EDT8                YES                                          
         XC    ELEM(EBWLENEQ),ELEM                                              
         LA    R6,ELEM                                                          
         USING EBWELEM,R6                                                       
         MVI   EBWCODE,EBWCODEQ                                                 
         MVI   EBWLEN,EBWLENEQ                                                  
         MVC   EBWCLT,BCLT                                                      
         MVC   EBWPRD,BPRD                                                      
         MVC   EBWEST,SVEST                                                     
*                                                                               
         MVI   PUTSW,C'Y'                                                       
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(EBWCODE,AIO),(4,EBWCLT),0              
         CLC   8(2,R2),=C'DW'      TEST FOR DELETE BWS XFT                      
         BE    EDT8                YES                                          
         GOTO1 ADDELEM                                                          
*                                                                               
EDT8     CLI   EDTMODE,C'E'                                                     
         BE    EDTX                                                             
         CLI   PUTSW,C'Y'          TEST FOR RECORD UPDATE                       
         BNE   EDTX                NO                                           
         L     RE,AIO                                                           
         CLC   SAVEKEY,0(RE)       TEST SAME KEY AS ORIGINAL                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 PUTREC                                                           
*                                  RE-FORMAT DETAIL LINE                        
         GOTO1 CLEARF,PARAS,(R2),DISPSEL(R2)                                    
         GOTO1 (RF),(R1),(1,(R2)),DISPSEL(R2)                                   
         GOTO1 AFORM,(R1),(RC),(R2),(R5)                                        
*                                                                               
EDTX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* COMPETITION SCREEN SELECT                                                     
*                                                                               
EDTCOMP  CLI   SVUSECMP,C'Y'       TEST COMPETITION USER                        
         BNE   EDTERRX             NO-NOT ALLOWED THIS SELECT                   
         MVI   CHASW,C'Y'                                                       
         CLI   EDTMODE,C'E'        TEST ON EDIT ONLY PASS                       
         BE    EDTX                YES-ALL DONE                                 
         CLI   PUTSW,C'Y'          TEST IF RECORD MUST BE PUT                   
         BNE   EDTCOMP2            NO                                           
         GOTO1 PUTREC                                                           
*                                  RE-FORMAT DETAIL LINE                        
         GOTO1 CLEARF,PARAS,(R2),DISPSEL(R2)                                    
         GOTO1 (RF),(R1),(1,(R2)),DISPSEL(R2)                                   
         GOTO1 AFORM,(R1),(RC),(R2),(R5)                                        
*                                                                               
EDTCOMP2 MVC   8(2,R2),=C'*X'      FLAG SELECT FIELD AS PROCESSED               
         MVC   DUB,RNKDEM          GET PRIMARY DEMO NAME                        
         BAS   RE,PREP             PREPARE FOR CALLING COMPETITION              
*                                                                               
         MVC   DMCB+4(4),=X'D90217B6'   LOAD COMPETITION SCREEN                 
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB,CONTAGH                                              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,=V(DUMMY)        LOAD COMPETITION PHASE AFTER MINE            
         A     RE,RELO                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB+4(4),=X'D9021716'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             A(COMPETITION PHASE)                         
*                                                                               
         MVC   CONREC(8),=C'COMPETE '                                           
         OI    CONRECH+6,X'80'                                                  
         MVC   CONACT(8),=C'SELECT  '                                           
         OI    CONACTH+6,X'80'                                                  
         LA    RE,DUB+6            GET DEMO NAME LENGTH                         
         LA    R0,7                                                             
         CLI   0(RE),C' '                                                       
         BH    *+10                FOUND A SIGNIFICANT CHARACTER                
         BCTR  RE,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,SRCDEMH+5                                                     
         OI    SRCDEMH+6,X'80'                                                  
         MVC   SRCDEM,DUB          SET IN DEMO NAME                             
*                                                                               
         MVI   MODE,VALKEY                                                      
         GOTO1 (RF),DMCB,(RC)      VALIDATE COMPETITION KEY                     
         MVI   MODE,DISPREC                                                     
         GOTO1 (RF),DMCB,(RC)      DISPLAY COMPETITION SCREEN                   
*                                                                               
         L     RD,SAVERD           EXIT BACK TO GENCON                          
         B     XIT                                                              
         EJECT                                                                  
* ESTIMATE SCREEN SELECT                                                        
*                                                                               
EDTEST   CLI   SVUSECMP,C'Y'       TEST COMPETITION USER                        
         BNE   EDTERRX             NO-NOT ALLOWED THIS SELECT                   
         MVI   CHASW,C'Y'                                                       
         CLI   EDTMODE,C'E'        TEST ON EDIT ONLY PASS                       
         BE    EDTX                YES-ALL DONE                                 
         CLI   PUTSW,C'Y'          TEST IF RECORD MUST BE PUT                   
         BNE   EDTEST2             NO                                           
         GOTO1 PUTREC                                                           
*                                  RE-FORMAT DETAIL LINE                        
         GOTO1 CLEARF,PARAS,(R2),DISPSEL(R2)                                    
         GOTO1 (RF),(R1),(1,(R2)),DISPSEL(R2)                                   
         GOTO1 AFORM,(R1),(RC),(R2),(R5)                                        
*                                                                               
EDTEST2  MVC   8(2,R2),=C'*E'      FLAG SELECT FIELD AS PROCESSED               
         BAS   RE,PREP             PREPARE FOR CALLING ESTIMATE                 
*                                                                               
         MVC   DMCB+4(4),=X'D90217B7'   LOAD ESTIMATE SCREEN                    
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB,CONTAGH                                              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,=V(DUMMY)        LOAD ESTIMATE PHASE AFTER MINE               
         A     RE,RELO                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB+4(4),=X'D9021717'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONREC(8),=C'ESTIMATE'                                           
         OI    CONRECH+6,X'80'                                                  
         MVC   CONACT(8),=C'SELECT  '                                           
         OI    CONACTH+6,X'80'                                                  
*                                                                               
         L     RF,DMCB             A(ESTIMATE PHASE)                            
         MVI   MODE,VALKEY                                                      
         GOTO1 (RF),DMCB,(RC)      VALIDATE ESTIMATE KEY                        
         MVI   MODE,DISPREC                                                     
         GOTO1 (RF),DMCB,(RC)      DISPLAY ESTIMATE SCREEN                      
*                                                                               
         L     RD,SAVERD           EXIT BACK TO GENCON                          
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PREPARE FOR CALLING COMPETITION OR ESTIMATE                    
*                                                                               
PREP     ST    RE,SAVEREG                                                       
         MVC   SVDETADR,DETDA      SAVE DA OF DETAIL RECORD                     
         L     R0,AIO2                                                          
         LAY   R1,LIOS                                                          
         L     RE,AIO                                                           
         SR    RF,RF                                                            
         ICM   RF,3,SIRRLEN-SIRKEY(RE)                                          
         MVCL  R0,RE               MOVE FROM IO1 TO IO2                         
*                                                                               
         XC    DMCB+8(4),DMCB+8    SAVE DETAIL TABLE                            
         MVI   DMCB+8,2            PAGE=TWA2                                    
         MVC   DMCB+10(2),2(R7)    TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',,ADETTAB                     
*                                                                               
         GOTO1 SAVEUWKA                                                         
*                                                                               
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,4            SAVE MY SCREEN ON TWA 4                      
         MVC   DMCB+10(2),2(R7)                                                 
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',,ATWA                        
*                                                                               
         LA    RE,SYSSPARE+1024                                                 
         LH    RF,=Y(SYSDEND-SYSSPARE-1024)                                     
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR SPARE STORAGE                          
*                                                                               
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO ADD OR CHANGE COST ELEMENT                                     
*                                                                               
* AT ENTRY, NEWCOST=NEW COST, AIO=A(DETAIL RECORD), R3=A(DETAIL ENTRY)          
*           DUB+0(1)=Y OR N FOR COST OVERRIDE PRESENT                           
*           DUB+1(3)=COST IN TABLE                                              
*           DUB+4(1)=Y OR N FOR COST OVERRIDE INPUT                             
*           DUB+5(3)=INPUT COST                                                 
*                                                                               
UPCOST   NTR1                                                                   
         CLC   DUB(1),DUB+4        TEST CHANGE IN COST OVERRIDES                
         BNE   *+12                YES                                          
         CLI   DUB+4,C'Y'          TEST FOR COST OVERRIDE INPUT                 
         BNE   UPCOST2             NO                                           
*                                                                               
         XC    ELEM(EBCLENEQ),ELEM DELETE COST OVERRIDE EL                      
         LA    R6,ELEM                                                          
         USING EBCELEM,R6                                                       
         MVI   EBCCODE,EBCCODEQ                                                 
         MVI   EBCLEN,EBCLENEQ                                                  
         MVC   EBCCLT,BCLT                                                      
         MVC   EBCSLN,SVLEN                                                     
         MVC   EBCEFF,DETEFF                                                    
         MVC   EBCCOST+1(3),NEWCOST                                             
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(EBCCODE,AIO),(6,EBCCLT),0              
         CLI   EDTMODE,C'E'        TEST FOR EDIT ONLY                           
         BE    *+8                                                              
         NI    DETCTL,X'FF'-X'80'  TURN OFF COST OVERRIDE BIT                   
*                                                                               
UPCOST1  CLI   DUB+4,C'Y'          TEST FOR COST OVERRIDE INPUT                 
         BNE   UPCOSTX             NO                                           
         GOTO1 (RF),(R1),(C'P',SYSFIL),(EBCCODE,AIO),EBCELEM                    
         CLI   12(R1),0                                                         
         BE    *+12                                                             
         MVI   ERROR,TOOLONG                                                    
         B     EDTERRX                                                          
         CLI   EDTMODE,C'E'        TEST EDIT ONLY                               
         BE    UPCOSTX             YES-EXIT                                     
         OI    DETCTL,X'80'                                                     
         MVC   DETCOST,NEWCOST                                                  
         B     UPCOSTX                                                          
*                                                                               
UPCOST2  MVI   BYTE,0              CLEAR HIGHEST INDEX SO FAR                   
         L     R6,AIO                                                           
         AH    R6,DATADISP                                                      
         USING EECELEM,R6                                                       
         SR    R0,R0               CLEAR WORK REGISTER                          
*                                                                               
UPCOST4  CLI   0(R6),0             TEST FOR EOR                                 
         BE    UPCOST8             YES                                          
         CLI   0(R6),EECCODEQ      TEST FOR COST ELEMENT                        
         BNE   UPCOST5             NO-GET NEXT ELEMENT                          
         CLC   BYTE,EECINDEX       TEST IF NEW INDEX IS GREATER                 
         BNL   *+10                NO                                           
         MVC   BYTE,EECINDEX       YES                                          
         CLC   SVLEN,EECSLN        TEST FOR SAME SLN AS SCREEN                  
         BE    UPCOST6             YES-FOUND IT                                 
*                                                                               
UPCOST5  IC    R0,EECLEN                                                        
         AR    R6,R0                                                            
         B     UPCOST4                                                          
*                                                                               
UPCOST6  OC    DETEFF,DETEFF       TEST FOR REPLACING FIRST COST                
         BNZ   UPCOST7             NO                                           
         ICM   R0,7,NEWCOST                                                     
         STCM  R0,15,EECCOST1                                                   
         CLI   EDTMODE,C'E'        TEST EDIT ONLY                               
         BE    *+10                                                             
         MVC   DETCOST,NEWCOST     UPDATE DETAIL ENTRY                          
         B     UPCOSTX                                                          
*                                                                               
UPCOST7  GOTO1 DATCON,DMCB,(2,DETEFF),(3,FULL)                                  
         LA    R1,3                R1=REMAINING COSTS                           
         LA    RE,EECDATE2         RE=A(DATE/COST)                              
         CLC   FULL(3),0(RE)       MATCH ON EFFECTIVE DATE                      
         BE    *+16                                                             
         LA    RE,7(RE)                                                         
         BCT   R1,*-14                                                          
         B     UPCOSTX                                                          
*                                                                               
         CLI   EDTMODE,C'E'                                                     
         BE    *+10                                                             
         MVC   DETCOST,NEWCOST     UPDATE DETAIL ENTRY                          
         ICM   R0,7,DETCOST                                                     
         STCM  R0,15,3(RE)                                                      
         B     UPCOSTX                                                          
*                                                                               
UPCOST8  XC    ELEM(EECLENEQ),ELEM                                              
         LA    R6,ELEM                                                          
         MVI   EECCODE,EECCODEQ                                                 
         MVI   EECLEN,EECLENEQ                                                  
         ZIC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         CH    R1,=Y(MAXSLNS)      TEST AGAINST MAX COST ELEMS                  
         BH    EDTERRX                                                          
         CLI   EDTMODE,C'E'        TEST EDIT ONLY                               
         BE    UPCOSTX             YES-CAN GET OUT NOW                          
         STC   R1,EECINDEX                                                      
         MVC   EECSLN,SVLEN                                                     
         MVC   EECCOST1+1(3),NEWCOST                                            
         MVC   DETCOST,NEWCOST                                                  
         GOTO1 ADDELEM                                                          
*                                                                               
UPCOSTX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
EDTERRX  GOTO1 ERREX                                                            
         SPACE 2                                                                
* LITERAL POOL FOR EDT                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* NSID RANK MODULE WORKING STORAGE                                              
*                                                                               
* STORAGE CONSISTS OF DYNAMIC SECTION WHICH IS CLEARED EACH TIME                
* AND A SAVE SECTION WHICH IS SAVED BETWEEN TRANSACTIONS                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
RELO     DS    A                                                                
SAVERD   DS    A                                                                
*                                                                               
DYNAMIC  DS    0C                                                               
ADETTAB  DS    A                   A(DETAIL TABLE)                              
AFORM    DS    A                                                                
AEDT     DS    A                                                                
SAVEREG  DS    A                                                                
AEQUIV   DS    A                   A(EQUIVALENCE HEADER)                        
*                                                                               
MYACT    DS    X                   INTERNAL ACTION                              
THISPF   DS    X                   THIS TIME PF KEY                             
CURSDISP DS    H                   DISPLACEMENT TO CURSOR FLDH                  
*                                                                               
FORMMODE DS    X                   X'FF'=PRINT                                  
EDTMODE  DS    C                   E=EDIT ONLY,U=EDIT AND UPDATE                
CHASW    DS    C                   Y=CHANGE HAS OCCURRED                        
PUTSW    DS    C                   Y=PUT THIS RECORD                            
NEWCOST  DS    XL3                                                              
*                                                                               
CTLVALS  DS    0X                                                               
MEDIA    DS    C                                                                
CLT      DS    CL3                                                              
PROD     DS    CL3                                                              
MKT      DS    XL2                                                              
EST      DS    X                                                                
NDEMS    DS    X                                                                
DEMOS    DS    (MAXDEMS)CL3                                                     
         DS    X                                                                
DAYPARTS DS    CL(L'SRSELDPT+L'SRSELDP2)                                        
LEN      DS    X                                                                
BUYPER   DS    CL4                                                              
SCHEME   DS    CL3                                                              
YEAR     DS    X                                                                
         DS    CL15                SPARE                                        
CTLVALN  EQU   *-CTLVALS                                                        
*                                                                               
STA      DS    XL3                                                              
ESTNAME  DS    CL(L'EDESC)                                                      
DAYMENU  DS    C                                                                
DPTTAB   DS    (L'DAYPARTS)CL2                                                  
SCROLL   DS    X                                                                
LINFILT  DS    X                   OPTIONAL N'LINES TO PRINT                    
*                                                                               
SEQUENCE DS    H                                                                
LASTCPP  DS    XL3                                                              
LASTRNK  DS    X                                                                
*                                                                               
DEMVALS  DS    CL(MAXDEMS*3)                                                    
         DS    0F                                                               
CPPVALS  DS    (MAXDEMS)F          CPP/M WORK AREA                              
*                                                                               
DETREC   DS    XL(DETLN)           NEW DETAIL ENTRY                             
BINREC   DS    XL(BINRECL)         BINSRCH RECORD                               
*                                                                               
BINPARM  DS    6F                                                               
         ORG   BINPARM                                                          
BINOP    DS    0X                  BINSRCH OPERATION                            
BININS   EQU   X'01'               INSERT RECORD IN TABLE                       
BINAREC  DS    A                   A(RECORD TO BE ADDED TO TABLE)               
BINATAB  DS    A                   A(TABLE)                                     
BINRECS  DS    F                   N'RECORDS SO FAR                             
BINLREC  DS    F                   L'RECORDS IN TABLE                           
BINDISPK DS    0X                  DISPLACEMENT TO KEY FIELD                    
BINLENK  DS    F                   L'KEY                                        
BINMAX   DS    F                   MAXIMUM RECORDS IN TABLE                     
*                                                                               
COMPVALS DS    0X                  VALUES EXTRACTED TO READ COMP REC            
CSTA     DS    CL8                                                              
CDAY     DS    X                                                                
CTIME    DS    XL4                                                              
*                                                                               
SAVEKEY  DS    CL13                ORIGINAL KEY BEFORE PUTREC                   
*                                                                               
         DS    CL60                SPARE                                        
DYNAMICL EQU   *-DYNAMIC           LENGTH OF DYNAMIC MODULE STORAGE             
*                                                                               
SAVES    DS    0H                  MODULE SAVE STORAGE                          
SVVALS   DS    0CL(CTLVALN)                                                     
SVMED    DS    C                                                                
SVCLT    DS    CL3                                                              
SVPROD   DS    CL3                                                              
SVMKT    DS    XL2                                                              
SVEST    DS    X                                                                
SVNDEMS  DS    X                                                                
SVDEMOS  DS    (MAXDEMS)CL3                                                     
         DS    X                                                                
SVDAYPTS DS    CL(L'SRSELDPT+L'SRSELDP2)                                        
SVLEN    DS    X                                                                
SVBUYPER DS    CL4                                                              
SVSCHEME DS    CL3                                                              
SVYEAR   DS    X                                                                
         DS    CL15                SPARE                                        
*                                                                               
SVSTA    DS    XL3                                                              
*                                                                               
SVACTSCH DS    XL2                 VALUES SAVED FROM SRBLK                      
SVPERNUM DS    X                                                                
SVACTYR  DS    X                                                                
*                                                                               
SVNENTS  DS    F                   N'ENTRIES IN VECTOR TABLE                    
SVNDETS  DS    F                   N'ENTRIES IN SIR TABLE                       
SVFIRST  DS    X                   FIRST ENTRY ON SCREEN                        
SVLAST   DS    X                   LAST ENTRY ON SCREEN                         
SVNSTAS  DS    X                                                                
SVSTATAB DS    XL(MAXSTAS*3)                                                    
SVCPP1   DS    F                   DEMO 1 CPP FOR INDEXING                      
SVCPP2   DS    F                                                                
SVCPP3   DS    F                                                                
SVCPP4   DS    F                                                                
*                                                                               
SVVECTOR DS    (MAXENTS)X          DETAIL ENTRIES IN THIS SEQUENCE              
*                                                                               
SAVEL    EQU   *-SAVES             LENGTH OF SAVED VALUES                       
         DS    CL(1024-(*-SYSSPARE))  SPARE                                     
         SPACE 2                                                                
* DSECT TO COVER DETAIL LINE TABLE ENTRIES                                      
*                                                                               
DETD     DSECT                                                                  
DETSTA   DS    X                   STATION NUMBER (REFERS TO SVSTATAB)          
DETCOST  DS    XL3                 COST (PENNIES)                               
DETEFF   DS    XL2                 COST EFFECTIVE DATE (COMPRESSED)             
DETCTL   DS    X                   X'80'=BWS COST OVERRIDE                      
*                                  X'40'-X'08'=DEMO OVERRIDE INDS               
DETDEMS  DS    0CL(MAXDEMS*4)                                                   
DETVAL1  DS    XL3                                                              
DETRNK1  DS    X                                                                
DETVAL2  DS    XL3                                                              
DETRNK2  DS    X                                                                
DETVAL3  DS    XL3                                                              
DETRNK3  DS    X                                                                
DETVAL4  DS    XL3                                                              
DETRNK4  DS    X                                                                
DETDA    DS    XL4                 SIR DETAIL RECORD D/A                        
DETLN    EQU   *-DETD                                                           
         SPACE 2                                                                
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINENT   DS    X                   DETAIL TABLE ENTRY NUMBER (1-255)            
BINKEY   DS    0XL5                                                             
BINCPP   DS    XL3                 CPP/CPM VALUE FOR PRIMARY DEMO               
BINSEQ   DS    H                   SEQUENCE NUMBER                              
BINRECL  EQU   *-BINRECD                                                        
         SPACE 2                                                                
* DSECT TO COVER SORT ENTRIES FOR SECONDARY DEMOS                               
*                                                                               
SORTRECD DSECT                                                                  
SORTENT  DS    X                                                                
SORTKEY  DS    0XL3                                                             
SORTCPP  DS    XL3                                                              
SORTRECL EQU   *-SORTRECD                                                       
         SPACE 2                                                                
* DSECT TO COVER PRINT LINES                                                    
*                                                                               
PRTD     DSECT                                                                  
PRT1     DS    0C                                                               
PRTSEL   DS    CL(L'RNKSEL1)                                                    
         DS    CL2                 SPARE                                        
PRTDET   DS    CL(L'RNKDET1)                                                    
         DS    CL2                 SPARE                                        
PRTDEM   DS    CL(L'RNKDEM1)                                                    
         ORG   PRT1+L'P                                                         
PRT2     DS    0C                                                               
PRTPRG   DS    CL(L'RNKPRG1)                                                    
         DS    CL2                 SPARE                                        
PRTCOS   DS    CL(L'RNKCOS1)                                                    
         DS    CL2                 SPARE                                        
PRTCPP   DS    CL(L'RNKCPP1)                                                    
         ORG   PRT2+L'P                                                         
PRT3     DS    0C                                                               
PRTCOM   DS    CL(L'RNKCOM1)                                                    
         DS    CL8                 SPARE                                        
PRTIDX   DS    CL(L'RNKIDX1)                                                    
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXENTS  EQU   6144/DETLN          TABLE ENTRY LIMIT                            
MAXDETS  EQU   4                   MAX N'DETAIL LINES ON SCREEN                 
MAXDEMS  EQU   4                   MAXIMUM DEMOS ON SCREEN                      
MAXSTAS  EQU   60                                                               
MAXSLNS  EQU   3                   MAXIMUM SLN COST ELEMENTS                    
FORCEDIS EQU   X'01'                                                            
REGULAR  EQU   X'02'                                                            
*                                                                               
PF1      EQU   X'01'               COMPETITION                                  
PF2      EQU   X'02'               ESTIMATE                                     
PF5      EQU   X'05'               PF5=RE-RANK                                  
PF7      EQU   X'07'               PF7=SCROLL BACKWARDS                         
PF8      EQU   X'08'               PF8=SCROLL FORWARDS                          
*                                                                               
DISPSEL  EQU   RNKSEL2H-RNKSEL1H                                                
DISPDET  EQU   RNKDET1H-RNKSEL1H                                                
DISPDEM  EQU   RNKDEM1H-RNKSEL1H                                                
DISPPRG  EQU   RNKPRG1H-RNKSEL1H                                                
DISPCOS  EQU   RNKCOS1H-RNKSEL1H                                                
DISPCPP  EQU   RNKCPP1H-RNKSEL1H                                                
DISPCOM  EQU   RNKCOM1H-RNKSEL1H                                                
DISPIDX  EQU   RNKIDX1H-RNKSEL1H                                                
*                                                                               
USERMOD  EQU   33                                                               
WGTMOD   EQU   63                                                               
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
RANSIDD  DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
* SPGENSIR                                                                      
* SPGENCOMP                                                                     
* DDCOMFACS                                                                     
* FATIOB                                                                        
* SPSFMFFD                                                                      
* DDGENTWA                                                                      
* SPSFMB6D (COMPETITION SCREEN)                                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENSIR                                                       
CMPRECD  DSECT                                                                  
       ++INCLUDE SPGENCOMP                                                      
EQURECD  DSECT                                                                  
       ++INCLUDE SPGENEQU                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPSFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB6D                                                       
         PRINT ON                                                               
         EJECT                                                                  
* NSID RANK SCREEN                                                              
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB8D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPSFM18   08/11/11'                                      
         END                                                                    
