*          DATA SET SPLFM16    AT LEVEL 023 AS OF 05/01/02                      
*PHASE T21916A                                                                  
*INCLUDE CLPACK                                                                 
         TITLE 'SPLFM16 - MARKET GROUP DEFINITIONS'                             
T21916   CSECT                                                                  
         NMOD1 0,T21916,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING MKGRECD,R8                                                       
         EJECT                                                                  
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,MKGEL                                                         
         USING MKGEL01,R6                                                       
*                                                                               
         LA    R2,LFMBK1H                                                       
         LA    R4,MKGBK1                                                        
         BAS   R9,FMTBK                                                         
*                                                                               
         LA    R2,LFMBK2H                                                       
         LA    R4,MKGBK2                                                        
         BAS   R9,FMTBK                                                         
*                                                                               
         LA    R2,LFMBK3H                                                       
         LA    R4,MKGBK3                                                        
         BAS   R9,FMTBK                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SVBKLNS+3                                                     
         SR    RE,RE                                                            
         IC    RE,SVBKLNS+4                                                     
         AR    R0,RE                                                            
         IC    RE,SVBKLNS+5                                                     
         AR    R0,RE                                                            
         STC   R0,BYTE2            SAVE TOTAL SCHEME DIGITS                     
*                                                                               
FMT2     LA    R2,LFMPGAH                                                       
         MVC   8(3,R2),SPACES                                                   
         MVI   8(R2),C'N'                                                       
         FOUT  (R2)                                                             
         CLI   MKGPGA,0                                                         
         BE    FMT3                                                             
         MVC   8(1,R2),MKGPGA                                                   
         B     FMT3                                                             
         EJECT                                                                  
FMT3     LA    R2,LFMALLMH                                                      
         MVC   8(3,R2),SPACES                                                   
         MVI   8(R2),C'N'                                                       
         FOUT  (R2)                                                             
         CLI   MKGALLM,0                                                        
         BE    FMT4                                                             
         MVC   8(1,R2),MKGALLM                                                  
         B     FMT4                                                             
FMTBK    DS    0H                                                               
         FOUT  (R2),0(R4),12       BREAK NAME                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   12(R4),0                                                         
         BE    FMTBKX                                                           
         MVC   BYTE,12(R4)                                                      
         OI    BYTE,X'F0'                                                       
         FOUT  (R2),BYTE,1                                                      
FMTBKX   OI    4(R2),X'20'                                                      
*                                                                               
         BR    R9                                                               
         EJECT                                                                  
FMT4     LA    R2,LFMADEXH                                                      
         MVC   8(5,R2),SPACES                                                   
         FOUT  (R2)                                                             
*                                                                               
* FORMAT EXCEPTION LIST - FIRST CLEAR LINES WITH DATA                           
         LA    R2,LFMEXLH                                                       
FMT4A    CLI   0(R2),80                                                         
         BNE   FMT4X                                                            
         OC    8(72,R2),8(R2)                                                   
         BZ    FMT4B                                                            
         XC    8(72,R2),8(R2)                                                   
         FOUT  (R2)                                                             
FMT4B    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FMT4A                                                            
*                                                                               
FMT4X    LA    R2,LFMEXLH                                                       
         LA    R6,MKGEL                                                         
         MVI   ELCODE,2                                                         
         B     FMT6C                                                            
*                                                                               
FMT6A    BAS   RE,NEXTEL                                                        
         BNE   FMT6X                                                            
         MVC   0(3,R4),2(R6)       ASSUME DATA IS CLT CODE                      
         CLI   SVKEY+8,C'F'        TEST GRPID A-F                               
         BH    FMT6B               G-K ARE CLIENT EXCEPTIONS                    
* BUILD PGRDEF KEY                                                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(3),SVKEY+2    A-M/CLT                                      
         MVC   KEY+5(1),2(R6)      PGRPID                                       
         CLC   KEY(13),KEYSAVE     SAME AS LAST TIME                            
         BE    *+8                                                              
         BAS   RE,RDPGRDEF         GET PRDGRP BREAK LENGTHS                     
*                                                                               
         XC    0(3,R4),0(R4)       CLEAR TO FORMAT PRDGRP                       
         MVC   0(1,R4),2(R6)                                                    
         IC    RE,SVBKLNS          GET PRDGRP BREAK 1 DIGITS                    
         BCTR  RE,0                                                             
         UNPK  DUB,3(3,R6)         UNPACK PRDGRP                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),DUB+3 **EXECUTED**                                       
FMT6B    FOUT  (R2)                                                             
         LA    R4,6(R4)                                                         
         SP    HALF,=P'1'                                                       
         BP    FMT6A                                                            
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
FMT6C    LA    R4,8(R2)            FIRST DISPLAY POS                            
         ZAP   HALF,=P'12'                                                      
         B     FMT6A                                                            
*                                                                               
FMT6X    B     EXXMOD                                                           
         SPACE 2                                                                
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                  EXIT WITH CC EQ                              
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE               EXIT WITH CC NOT EQ                          
         BR    RE                                                               
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   SVACT,C'A'                                                       
         BNE   EDT2                                                             
*                                                                               
EDT1     MVC   MKGKEY,SVKEY                                                     
         MVC   MKGLEN,=H'72'                                                    
         MVC   MKGAGYA,AGYALPHA                                                 
         MVI   MKGEL,1                                                          
         MVI   MKGEL+1,48                                                       
         B     EDT4                                                             
         SPACE 2                                                                
EDT2     MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         CLI   MKGEL+1,48          CHECK FOR NEW 01 ELEM LEN                    
         BL    EDT1                NO - REBUILD ELEM                            
         EJECT                                                                  
EDT4     LA    R6,MKGEL                                                         
         USING MKGEL01,R6                                                       
*                                                                               
         LA    R2,LFMBK1H                                                       
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    LFMERR                                                           
*                                                                               
         LA    R4,MKGBK1                                                        
         BAS   R9,EDTBK                                                         
         MVI   ERRCD,INVERR                                                     
         CLI   MKGBK1LN,0                                                       
         BE    LFMERR                                                           
*                                                                               
         LA    R2,LFMBK2H                                                       
         LA    R4,MKGBK2                                                        
         BAS   R9,EDTBK                                                         
*                                                                               
         LA    R2,LFMBK3H                                                       
         LA    R4,MKGBK3                                                        
         BAS   R9,EDTBK                                                         
*                                                                               
         CLI   MKGBK3LN,0          TEST LEVEL 3 INPUT                           
         BE    EDT4X               NO                                           
         CLI   MKGBK2LN,0          TEST LEVEL 2 INPUT                           
         BNE   EDT4X               YES-OK                                       
         LA    R2,LFMBK2H          CAN'T HAVE 3 WITHOUT 2                       
         MVI   ERRCD,MSSNGERR                                                   
         B     LFMERR                                                           
*                                                                               
EDT4X    SR    RE,RE                                                            
         IC    RE,MKGBK1LN                                                      
         SR    R0,R0                                                            
         IC    R0,MKGBK2LN                                                      
         AR    RE,R0                                                            
         IC    R0,MKGBK3LN                                                      
         AR    RE,R0                                                            
         MVI   ERRCD,GRPLNERR                                                   
         LA    R2,LFMBK1H                                                       
         CH    RE,=H'4'                                                         
         BH    LFMERR                                                           
*                                                                               
         LA    R2,LFMPGAH                                                       
         MVI   ERRCD,INVERR                                                     
         MVI   MKGPGA,0            SET FOR NO PRDGRP ASSGNS                     
         CLI   8(R2),C'N'                                                       
         BE    EDT5                                                             
         CLI   SVKEY+8,C'F'        TEST ID G-K                                  
         BH    LFMERR              G-K MUST SAY 'N'                             
         CLI   8(R2),C'Y'                                                       
         BNE   LFMERR                                                           
         MVC   MKGPGA,8(R2)                                                     
         SPACE 2                                                                
EDT5     LA    R2,LFMALLMH                                                      
         MVI   MKGALLM,0                                                        
         CLI   5(R2),0                                                          
         BE    EDT6                                                             
         CLI   8(R2),C'N'                                                       
         BE    EDT6                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   LFMERR                                                           
         MVC   MKGALLM,8(R2)                                                    
* EDIT EXCEPTION ENTRY                                                          
*                                                                               
EDT6     LA    R2,LFMADEXH                                                      
         CLI   5(R2),0                                                          
         BE    EDT10                                                            
*                                                                               
         CLI   SVKEY+8,C'F'                                                     
         BH    EDT8                                                             
* FOR ID A-F, EDIT PRDGRP EXCEPTION CODE                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(3),SVKEY+2    A-M/CLT                                      
         MVC   KEY+5(1),8(R2)      PRDGRP ID                                    
*                                                                               
         BAS   RE,RDPGRDEF         GET BREAK LENGTHS                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,SVBKLNS          GET PRDGRP BREAK 1 DIGITS                    
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,0                GET INPUT DIGITS                             
         MVI   ERRCD,INVERR                                                     
         CR    RE,R0               SHOULD HAVE BREAK 1 DIGITS                   
         BNE   LFMERR                                                           
         MVC   WORK(4),=4C'0'                                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),9(R2) **EXECUTED**                                       
         PACK  DUB,WORK(5)                                                      
         MVC   KEY+6(2),DUB+5                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLC   KEY(6),KEYSAVE      TY/A-M/CLT/PGRPID                            
         BNE   LFMERR                                                           
*                                                                               
         UNPK  DUB,KEY+6(3)                                                     
         UNPK  WORK(8),KEYSAVE+6(3)                                             
         IC    RE,SVBKLNS                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   DUB+3(0),WORK+3 **EXECUTED**                                     
         BNE   LFMERR                                                           
* BUILD NEW ELEM                                                                
         XC    ELEM(5),ELEM                                                     
         MVI   ELEM,2                                                           
         MVI   ELEM+1,5                                                         
         MVC   ELEM+2(3),KEYSAVE+5                                              
         B     EDT9                                                             
         SPACE 2                                                                
* EDIT EXCEPTION CLIENT FOR ID'S G-K                                            
*                                                                               
EDT8     XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVKEY+2    A-M                                          
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),2                                                          
         BL    LFMERR                                                           
         BNE   EDT8A                                                            
* CHECK FOR OFFICE CODE                                                         
         CLI   8(R2),C'*'                                                       
         BNE   EDT8A                                                            
         CLI   9(R2),C'A'                                                       
         BL    LFMERR                                                           
         CLI   9(R2),C'Z'                                                       
         BL    EDT8B                                                            
         CLI   9(R2),C'0'                                                       
         BL    LFMERR                                                           
         CLI   9(R2),C'9'                                                       
         BL    EDT8B                                                            
         B     LFMERR                                                           
*                                                                               
EDT8A    CLI   5(R2),3                                                          
         BH    LFMERR                                                           
         GOTO1 MOVE                                                             
         GOTO1 =V(CLPACK),DMCB,WORK,KEY+2,RR=RELO                               
*                                                                               
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOFNDERR                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LFMERR                                                           
* BUILD ELEM                                                                    
EDT8B    MVI   ELEM,2                                                           
         MVI   ELEM+1,5                                                         
         GOTO1 MOVE                                                             
         MVC   ELEM+2(3),WORK                                                   
* FIND INSERTION POINT                                                          
EDT9     MVI   ELCODE,2                                                         
         LA    R6,MKGEL                                                         
         MVI   ERRCD,INVERR                                                     
*                                                                               
EDT9A    BAS   RE,NEXTEL                                                        
         BNE   EDT9B                                                            
         CLC   0(5,R6),ELEM                                                     
         BE    LFMERR                                                           
         BH    EDT9B               INSERT HERE                                  
         B     EDT9A                                                            
*                                                                               
EDT9B    GOTO1 VRECUP,DMCB,AREC,ELEM,(R6)                                       
         EJECT                                                                  
EDT10    CLI   SVACT,C'A'                                                       
         BNE   EDT12                                                            
*                                                                               
         GOTO1 ADDREC                                                           
         MVC   SVKEY+14(4),KEY+14  SAVE DISK ADDRESS                            
         GOTO1 CNADDSPT                                                         
         B     EDTX                                                             
*                                                                               
EDT12    MVC   KEY(18),SVKEY       RESTORE KEY AND DISK ADDRESS                 
         LA    R8,REC2             DO GETREC TO REBUILD GETREC TABLE            
         ST    R8,AREC             READ INTO REC2                               
         GOTO1 GETREC                                                           
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 CNCHASPT                                                         
EDTX     B     FMT                 GO RE-DISPLAY                                
         EJECT                                                                  
EDTBK    MVC   BYTE,5(R2)          SAVE INPUT LENGTH                            
         GOTO1 MOVE                                                             
         MVC   0(12,R4),WORK                                                    
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   ERRCD,NOCHGERR                                                   
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),X'20'                                                      
         BZ    LFMERR                                                           
*                                                                               
         CLI   BYTE,0              TEST DESC INPUT                              
         BNE   EDTBK2              YES                                          
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),0             NO DESC SO SHOULD NOT ENTER LEN              
         BNE   LFMERR                                                           
         BR    R9                                                               
*                                                                               
EDTBK2   MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C'0'                                                       
         BNH   LFMERR                                                           
         CLI   8(R2),C'4'                                                       
         BH    LFMERR                                                           
         MVC   12(1,R4),8(R2)                                                   
         NI    12(R4),X'0F'                                                     
*                                                                               
         BR    R9                                                               
         EJECT                                                                  
RDPGRDEF NTR1                                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         USING PRGRECD,R8                                                       
         LA    R6,PRGEL                                                         
         USING PRGEL01,R6                                                       
*                                                                               
         MVC   SVBKLNS+0(1),PRGBK1LN                                            
         MVC   SVBKLNS+1(1),PRGBK2LN                                            
         MVC   SVBKLNS+2(1),PRGBK3LN                                            
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC             RESTORE AREC                                 
         XIT1                                                                   
         EJECT                                                                  
LFMERR   GOTO1 ERROR                                                            
*                                                                               
         LTORG                                                                  
* SPLFMWRK                                                                      
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
*SPLFMF6D                                                                       
       ++INCLUDE SPLFMF6D                                                       
         EJECT                                                                  
*SPGENMKG                                                                       
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
* SPGENPRG                                                                      
       ++INCLUDE SPGENPRG                                                       
 END                                                                            
