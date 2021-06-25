*          DATA SET SPLFM17    AT LEVEL 033 AS OF 05/01/02                      
*PHASE T21917A                                                                  
         TITLE 'SPLFM17 - MARKET GROUP ASSIGNMENTS'                             
T21917   CSECT                                                                  
         NMOD1 0,T21917                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING MKGRECD,R8                                                       
*                                                                               
         CLI   SVFMTSW,0                                                        
         BE    FMT                                                              
         B     EDT                                                              
         EJECT                                                                  
FMT      DS    0H                                                               
* READ GRP DEF'N REC                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(3),SVKEY        TY/A-M                                       
         CLI   SVREC,X'18'         TEST CSO PMGR                                
         BNE   *+10                                                             
         MVC   KEY(2),=X'0D02'                                                  
         CLI   SVKEY+8,C'F'                                                     
         BH    *+10                                                             
         MVC   KEY+3(2),SVKEY+3    CLT FOR TYPES A-F ONLY                       
         MVC   KEY+8(1),SVKEY+8    MGRPID                                       
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
* DISPLAY BREAK NAME                                                            
         LA    R6,MKGEL                                                         
         USING MKGEL01,R6                                                       
* SAVE BREAK LENGTHS                                                            
         MVC   SVBKLNS+3(1),MKGBK1LN                                            
         MVC   SVBKLNS+4(1),MKGBK2LN                                            
         MVC   SVBKLNS+5(1),MKGBK3LN                                            
*                                                                               
         LA    R2,LFMBK1H                                                       
         FOUT  (R2),MKGBK1,12                                                   
*                                                                               
         LA    R2,LFMBK2H                                                       
         FOUT  (R2),MKGBK2,12                                                   
*                                                                               
         LA    R2,LFMBK3H                                                       
         FOUT  (R2),MKGBK3,12                                                   
         DROP  R6                                                               
*                                                                               
         FOUT  LFMNM1H,SPACES,24                                                
*                                                                               
         FOUT  LFMNM2H,SPACES,24                                                
*                                                                               
         FOUT  LFMNM3H,SPACES,24                                                
*                                                                               
*                                                                               
         CLI   SVFMTSW,1           TEST EDIT                                    
         BE    *+12                YES                                          
         CLI   SVACT,C'A'          TEST FMT BEFORE ADD                          
         BE    FMT2                YES - SKIP READ                              
* READ THIS REC                                                                 
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,MKGEL                                                         
         CLI   0(R6),X'10'         FIND 10 ELEM                                 
         BE    FMT1                                                             
         MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST FIND 10                                 
*                                                                               
FMT1     DS    0H                                                               
         USING MKGEL10,R6                                                       
*                                                                               
         LA    R2,LFMNM1H                                                       
         FOUT  (R2),MKGNAM1,24                                                  
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,LFMNM2H                                                       
         FOUT  (R2),MKGNAM2,24                                                  
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,LFMNM3H                                                       
         FOUT  (R2),MKGNAM3,24                                                  
         OI    4(R2),X'20'                                                      
*                                                                               
         DROP  R6                                                               
*                                                                               
FMT1C    DS    0H                                                               
         XC    LFMLACC,LFMLACC                                                  
         LA    R6,MKGEL                                                         
         CLI   0(R6),X'20'                                                      
         BE    FMT1E                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FMT1X                                                            
*                                                                               
FMT1E    DS    0H                                                               
         USING MKGEL20,R6                                                       
         MVC   LFMLACC(3),MKGLTACC                                              
*                                                                               
FMT1X    FOUT  LFMLACCH                                                         
         OI    LFMLACCH+4,X'20'                                                 
         EJECT                                                                  
* CLEAR ADDED MKT FIELDS                                                        
FMT2     LA    R2,LFMMKT1H         FIRST MKT                                    
         LA    R0,LFMMKTXH         LAST MKT                                     
*                                                                               
FMT2A    OC    8(4,R2),8(R2)                                                    
         BZ    FMT2B                                                            
         XC    8(4,R2),8(R2)                                                    
         FOUT  (R2)                                                             
         NI    4(R2),X'DF'                                                      
         MVI   5(R2),0             CLEAR INPUT LENGTH                           
FMT2B    SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,R0                                                            
         BNH   FMT2A                                                            
*                                                                               
* CLEAR DISPLAY LINES                                                           
         LA    R2,LFMLST1H                                                      
         SR    R0,R0                                                            
FMT4A    OC    8(75,R2),8(R2)                                                   
         BZ    FMT4B                                                            
         XC    8(75,R2),8(R2)                                                   
         FOUT  (R2)                                                             
FMT4B    IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FMT4A                                                            
         EJECT                                                                  
* SET UP TO DISPLAY MARKETS VIA PASSIVE POINTERS                                
*                                                                               
         MVC   HALF,SVMKT          SET STARTING MKT NUM                         
         MVC   BYTE2,SVAGYMD        RESET FOR DISPLAY                           
         BAS   R9,BLDPSSV                                                       
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,LFMLST1H                                                      
         LA    R4,8(R2)                                                         
         ZAP   HALF,=P'15'         SET FOR 15 MKTS PER LINE                     
         B     FMT14                                                            
*                                                                               
FMT12    GOTO1 SEQ                                                              
*                                                                               
FMT14    CLC   KEY(MKGPMKT-MKGKEY),KEYSAVE   TEST SAME THRU MKTGRP              
         BNE   EXXMOD                                                           
         EJECT                                                                  
         LA    RE,KEY+(MKGPMKT-MKGKEY)  POINT TO MARKET                         
         MVC   DUB(2),0(RE)                                                     
         LH    RE,DUB                                                           
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R4),DUB                                                      
         LA    R4,5(R4)                                                         
         FOUT  (R2)                                                             
         SP    HALF,=P'1'                                                       
         BP    FMT12                                                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST E-O-S                                   
         BE    FMTX                                                             
         LA    R4,8(R2)                                                         
         ZAP   HALF,=P'15'                                                      
         B     FMT12                                                            
*                                                                               
FMTX     B     EXXMOD                                                           
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   SVACT,C'A'                                                       
         BNE   EDT2                                                             
*                                                                               
* READ GRP DEF REC FOR BREAK LENGTHS                                            
         XC    KEY,KEY                                                          
         MVC   KEY(3),SVKEY        TY/A-M                                       
         CLI   SVREC,X'18'         TEST CSO PMGR RECORDS                        
         BNE   *+10                                                             
         MVC   KEY(2),=X'0D02'                                                  
         CLI   SVKEY+8,C'F'                                                     
         BH    *+10                                                             
         MVC   KEY+3(2),SVKEY+3    CLT FOR TYPES A-F ONLY                       
         MVC   KEY+8(1),SVKEY+8    MGRPID                                       
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
* SAVE BREAK LENGTHS                                                            
         LA    R6,MKGEL                                                         
         USING MKGEL01,R6                                                       
         MVC   SVBKLNS+3(1),MKGBK1LN                                            
         MVC   SVBKLNS+4(1),MKGBK2LN                                            
         MVC   SVBKLNS+5(1),MKGBK3LN                                            
         XC    REC(256),REC                                                     
         MVC   MKGKEY,SVKEY                                                     
         MVC   MKGLEN,=H'98'                                                    
         MVC   MKGAGYA,AGYALPHA                                                 
         MVI   MKGEL,X'10'                                                      
         MVI   MKGEL+1,74                                                       
         B     EDT4                                                             
         SPACE 2                                                                
EDT2     DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         EJECT                                                                  
EDT4     LA    R6,MKGEL                                                         
         CLI   0(R6),X'10'         FIND 10 ELEM                                 
         BE    EDT4A0                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST FIND 10                                 
*                                                                               
EDT4A0   DS    0H                                                               
         USING MKGEL10,R6                                                       
*                                                                               
         LA    R2,LFMNM1H                                                       
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   MKGNAM1,WORK                                                     
*                                                                               
         LA    R2,LFMNM2H                                                       
         CLI   SVBKLNS+4,0         TEST 2 LEVELS                                
         BNE   EDT4A               YES                                          
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),0                                                          
         BNE   LFMERR                                                           
         B     EDT4B                                                            
*                                                                               
EDT4A    GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   MKGNAM2,WORK                                                     
*                                                                               
EDT4B    LA    R2,LFMNM3H                                                       
         CLI   SVBKLNS+5,0         TEST 3 LEVELS                                
         BNE   EDT4C                                                            
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),0                                                          
         BNE   LFMERR                                                           
         B     EDT5                                                             
*                                                                               
EDT4C    GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   MKGNAM3,WORK                                                     
*                                                                               
*                                                                               
EDT5     DS    0H                                                               
         XC    ELEM(30),ELEM                                                    
         LA    R2,LFMLACCH                                                      
         CLI   5(R2),0                                                          
         BE    EDT5B               NO INPUT                                     
         CLI   5(R2),3                                                          
         BH    LFMERR                                                           
         MVC   ELEM(2),=X'2010'                                                 
         MVC   ELEM+2(3),8(R2)                                                  
         LA    R4,ELEM+2                                                        
         LA    R5,3                                                             
EDT5A    CLI   0(R4),0                                                          
         BE    EDT5A2                                                           
         CLI   0(R4),C'A'                                                       
         BL    LFMERR                                                           
         CLI   0(R4),C'9'                                                       
         BH    LFMERR                                                           
EDT5A2   LA    R4,1(R4)                                                         
         BCT   R5,EDT5A                                                         
*                                                                               
*                                                                               
EDT5B    LA    R6,MKGEL            SEE IF THERE WAS AN 20 ELEM                  
         CLI   0(R6),X'20'                                                      
         BE    EDT5C               FOUND                                        
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL                                                        
         BE    EDT5C                                                            
         CLI   ELEM,0              SEE IF I HAVE TO ADD ONE                     
         BE    EDT5X               NO                                           
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R6)                                   
         B     EDT5X                                                            
*                                                                               
EDT5C    CLI   ELEM,0              SEE IF CHANGING OLD ELEM                     
         BE    EDT5E               NO NEED TO DELETE IT                         
         MVC   0(16,R6),ELEM       JUST SWITCH ELEMS                            
         B     EDT5X                                                            
*                                                                               
EDT5E    GOTO1 VRECUP,DMCB,(0,REC),0(R6),0    NEED TO DELETE IT                 
*                                                                               
EDT5X    CLI   SVACT,C'A'                                                       
         BNE   EDT6                                                             
         GOTO1 ADDREC                                                           
         MVC   SVKEY+14(4),KEY+14  SAVE DISK ADDRESS                            
         GOTO1 CNADDSPT                                                         
         B     EDT8                                                             
*                                                                               
EDT6     TM    LFMNM1H+4,X'20'     TEST DATA CHANGED                            
         BZ    EDT6X                                                            
         TM    LFMNM2H+4,X'20'     TEST DATA CHANGED                            
         BZ    EDT6X                                                            
         TM    LFMNM3H+4,X'20'     TEST DATA CHANGED                            
         BZ    EDT6X                                                            
         TM    LFMLACCH+4,X'20'                                                 
         BZ    EDT6X                                                            
         B     EDT8                                                             
EDT6X    GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 CNCHASPT                                                         
         EJECT                                                                  
EDT8     OI    LFMNM1H+4,X'20'     SET VALID                                    
         OI    LFMNM2H+4,X'20'     SET VALID                                    
         OI    LFMNM3H+4,X'20'     SET VALID                                    
*                                                                               
* NOW EDIT MARKET CODES                                                         
         LA    R2,LFMMKT1H                                                      
         MVC   BYTE2,SVAGYMD        SAVE AGY MD IA IN BYTE2                     
         B     EDT24                                                            
*                                                                               
EDT10    GOTO1 PACK                                                             
         MVI   ERRCD,INVERR                                                     
         LTR   R0,R0                                                            
         BZ    LFMERR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL,DUB            SAVE EBCDIC MKT                              
*                                                                               
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
* FIRST CHECK FOR A MKT ASSGNMENT REC                                           
         XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING MKARECD,R8                                                       
         MVC   MKAKTYP,=X'0D03'                                                 
         MVC   MKAKAGMD,BYTE2                                                   
         MVC   MKAKCLT,SVCLT                                                    
         MVC   MKAKMKT,HALF                                                     
         MVC   WORK2(20),KEY       SAVE THIS KEY                                
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EDT11                                                            
         GOTO1 GETREC                                                           
         B     EDT14                                                            
         EJECT                                                                  
*                                                                               
EDT11    CLI   SVEBCMED,C'N'       CHK FOR NETWORK                              
         BNE   EDT12                                                            
         MVI   BYTE,X'62'                                                       
GETD     LA    R6,REC2                                                          
         GOTO1 VCALLOV,DMCB,(BYTE,(R6)),(RA)                                    
         CLI   4(R1),X'FF'         NOT FOUND                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
EDT11D   CLC   HALF,0(R6)          HALF STILL HAS MKT                           
         BE    EDT11X              FOUND                                        
         CLI   0(R6),X'FF'         END OF TABLE                                 
         BNE   EDT11F                                                           
         MVI   ERRCD,NOMKTERR                                                   
         B     LFMERR                                                           
*                                                                               
EDT11F   LA    R6,32(R6)           NEXT MKT                                     
         B     EDT11D                                                           
*                                                                               
EDT11X   B     EDT13                                                            
* HAVE TO CHECK STATION FILE TO SEE IF MARKET EXISTS                            
EDT12    XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(4),FULL                                                    
         MVC   KEY+6(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
*                                                                               
         MVI   ERRCD,NOMKTERR                                                   
         CLC   KEY(8),REC                                                       
         BNE   LFMERR                                                           
* CREATE A NEW MKT ASSGN REC                                                    
EDT13    LA    R8,REC              RESTORE ADDRESS                              
         USING MKARECD,R8                                                       
         XC    REC(256),REC                                                     
         MVC   MKAKEY,WORK2                                                     
         MVC   MKAAGYA,AGYALPHA                                                 
         MVC   MKALEN,=H'24'                                                    
         EJECT                                                                  
* BUILD NEW ASSGN ELEM                                                          
*                                                                               
EDT14    XC    ELEM(8),ELEM                                                     
         MVI   ELEM,5                                                           
         MVI   ELEM+1,8                                                         
         LA    RE,SVKEY+MKGKPID-MKGKEY  POINT TO PRDGRP                         
         MVC   ELEM+2(6),0(RE)                                                  
*                                                                               
         LA    R8,REC              RESTORE ADDRESS                              
         USING MKARECD,R8                                                       
         LA    R6,MKAEL                                                         
         USING MKAEL05,R6                                                       
         CLI   0(R6),5                                                          
         BE    EDT17                                                            
*                                                                               
EDT16    MVI   ELCODE,5                                                         
         BAS   RE,NEXTEL                                                        
         BE    EDT17                                                            
* ADD NEW ELEM                                                                  
         GOTO1 VRECUP,DMCB,(R8),ELEM,(R6)                                       
*                                                                               
         L     RF,ADDREC                                                        
         CLC   MKAKEY,KEY                                                       
         BNE   *+8                                                              
         L     RF,PUTREC                                                        
         GOTO1 (RF)                                                             
*                                                                               
         B     EDT20                                                            
         EJECT                                                                  
*                                                                               
EDT17    CLC   MKAPGRP,ELEM+2      TEST SAME PRDGRP                             
         BNE   EDT16                                                            
         CLC   MKAMGRP(1),ELEM+5   TEST SAME MKTGRP ID                          
         BNE   EDT16                                                            
         CLC   MKAMGRP(3),ELEM+5   TEST DUP ASSGN                               
         BNE   EDT18               GO CHANGE MGRP ASSGN                         
         MVI   ERRCD,GRPASSGN      DUPLICATE GRP ASSGN                          
         B     LFMERR                                                           
         SPACE 2                                                                
* MARKET WAS ASSGND FOR THIS PRDGRP.                                            
*                                                                               
EDT18    XC    0(8,R6),ELEM        SWAP OLD AND NEW ELEMENTS                    
         XC    ELEM(8),0(R6)                                                    
         XC    0(8,R6),ELEM                                                     
* UPDATE RECORD                                                                 
         GOTO1 PUTREC                                                           
* DELETE OLD PASSIVE POINTER                                                    
         BAS   R9,BLDPSSV                                                       
         LA    RE,KEY+MKGPMID-MKGKEY    POINT TO MKTGRP                         
         MVC   0(3,RE),ELEM+5           MOVE OLD MKTGRP                         
         BAS   R9,DELPSSV                                                       
*                                                                               
* ADD NEW PASSIVE POINTER                                                       
*                                                                               
EDT20    BAS   R9,BLDPSSV                                                       
         LA    RE,KEY+MKGPMID-MKGKEY                                            
         MVC   0(3,RE),5(R6)       MOVE NEW MKTGRP                              
         BAS   R9,ADDPSSV                                                       
*                                                                               
*                                                                               
*              IF CANADIAN TV - REPEAT STEPS EDT10 THRU EDT20 FOR               
*              THE OTHER MEDIAS                                                 
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   EDT20A                                                           
         CLI   SVEBCMED,C'T'                                                    
         BNE   EDT20A                                                           
*                                                                               
         MVC   BYTE,BYTE2          SEE WHICH MEDIA I JUST FINISHED              
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'08'          LAST WAS COMBINED - SO DONE                  
         BE    EDT20A                                                           
         CLI   BYTE,X'03'                                                       
         BE    DOCOMB              LAST WAS NETWORK - DO COMBINED               
*                                                                               
         NI    BYTE2,X'F0'         LAST WAS TV - SO DO NETWORK                  
         OI   BYTE2,X'03'                                                       
         B     EDT10                                                            
*                                                                               
DOCOMB   NI    BYTE2,X'F0'         LAST WAS NETWORK - SO DO COMBINED            
         OI    BYTE2,X'08'                                                      
         B     EDT10                                                            
*                                                                               
EDT20A   MVC   BYTE2,SVAGYMD       RESTORE BYTE2 FOR NEXT MKT                   
*                                                                               
EDT22    OI    4(R2),X'20'         SET VALID BIT                                
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
EDT24    CLI   0(R2),9                                                          
         BE    EDTX                                                             
         CLI   5(R2),0             TEST DATA                                    
         BE    EDT22                                                            
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDT22                                                            
         B     EDT10                                                            
*                                                                               
EDTX     LA    R8,REC              RESTORE ADDRESS                              
         B     FMT                 RE-DISPLAY                                   
         EJECT                                                                  
BLDPSSV  XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING MKGRECD,R8                                                       
*                                                                               
         MVC   MKGPTYP(2),=X'0D82'                                              
         CLI   SVREC,X'18'         TEST CSO PMGR RECORDS                        
         BNE   *+10                                                             
         MVC   MKGPTYP(2),=X'0D87'                                              
         MVC   MKGPAGMD,BYTE2                                                   
         MVC   MKGPCLT,SVCLT                                                    
         LA    RE,SVKEY+(MKGKPID-MKGKEY)                                        
         MVC   MKGPPID(6),0(RE)                                                 
         MVC   MKGPMKT,HALF                                                     
         BR    R9                                                               
         DROP  R8                                                               
         SPACE 2                                                                
ADDPSSV  OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BE    ADDPSSV2                                                         
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   COMMAND,=C'DMADD'                                                
         GOTO1 DIR                                                              
         BR    R9                                                               
*                                                                               
ADDPSSV2 MVI   KEY+13,0                                                         
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         BR    R9                                                               
         SPACE 2                                                                
DELPSSV  GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         BR    R9                                                               
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCODE,0(R6)                                                     
         BNE   NEXTEL                                                           
         BR    RE                  EXIT WITH CC=                                
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT =                           
         SPACE 2                                                                
LFMERR   GOTO1 ERROR                                                            
*                                                                               
EXXMOD   XMOD1 1                                                                
         LTORG                                                                  
* SPLFMWRK                                                                      
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
* SPLFMF7D                                                                      
       ++INCLUDE SPLFMF7D                                                       
         EJECT                                                                  
* SPGENMKG                                                                      
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
* SPGENMKA                                                                      
       ++INCLUDE SPGENMKA                                                       
         EJECT                                                                  
* SPGENPRG                                                                      
       ++INCLUDE SPGENPRG                                                       
 END                                                                            
