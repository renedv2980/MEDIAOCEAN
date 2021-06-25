*          DATA SET SPGOL04    AT LEVEL 064 AS OF 02/26/20                      
*PHASE T20204C                                                                  
*INCLUDE SPAUTH                                                                 
**********************************************************************          
* 07DEC05 EJOR      REMOVE HARDCODE FOR AGY GB                                  
* 22APR04 MHER/ABEA FIX TRANSFER TO/FROM SOLO TO PIGGYBACKS                     
* 06SEP01 MHER      FIX SPAUTH CALL TO POINT R1 TO DMCB BEFORE DATCON           
* 07JUL00 SPRI/AATK CALL SPAUTH TO ADD SUPERDESK AUTH MKT LEVEL RECS *          
**********************************************************************          
         TITLE 'SPGOL04 - SPOTPAK GOALS TRANSFER PROGRAM'                       
T20204   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20204                                                         
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T20204+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T202FFD,RA                                                       
         SPACE 1                                                                
* TEST FOR ANY CHANGED FIELDS                                                   
         SPACE 1                                                                
         TM    XFRPERH+4,X'20'                                                  
         BZ    GL2                                                              
         TM    XFROMKTH+4,X'20'                                                 
         BZ    GL2                                                              
         TM    XFRODPTH+4,X'20'                                                 
         BZ    GL2                                                              
         TM    XFRNMKTH+4,X'20'                                                 
         BZ    GL2                                                              
         TM    XFRNDPTH+4,X'20'                                                 
         BZ    GL2                                                              
         TM    XFRNPRDH+4,X'20'                                                 
         BZ    GL2                                                              
         B     GL100               GO DO TRANSFER                               
         SPACE 1                                                                
* EDIT PERIOD *                                                                 
         SPACE 1                                                                
GL2      XC    XDATA,XDATA         CLEAR OLD DATA VALUES                        
         BAS   RE,CLRSCR           CLEAR DISPLAY AREA                           
*                                                                               
         LA    R2,XFRPERH                                                       
GL4      NI    4(R2),X'DF'         RESET ALL EDITED BITS                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   GL4                                                              
*                                                                               
         MVI   ERRCD,MSSNGERR                                                   
         LA    R2,XFRPERH                                                       
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         GOTO1 USER3               'EDTPER'                                     
         OI    4(R2),X'20'         SET PERIOD EDITED                            
         SPACE 1                                                                
         EJECT                                                                  
* EDIT 'OLD' DATA *                                                             
         SPACE 1                                                                
         LA    R2,XFROMKTH                                                      
         NI    4(R2),X'DF'         UNSET 'EDITED'                               
         GOTO1 USER1               'EDTMKT'                                     
         MVC   XOLDMKT,BMKT                                                     
         MVC   XOLDMKNM,SVMKTNAM                                                
         MVC   XNEWMKT,XOLDMKT     SET NEW = OLD                                
         MVC   XNEWMKNM,XOLDMKNM                                                
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,XFRODPTH                                                      
         BAS   RE,VALDPT                                                        
         MVC   XOLDDPT,XNEWDPT                                                  
         MVC   XOLDSLN,XNEWSLN                                                  
         MVC   XOLDTLN,XNEWTLN                                                  
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   XOLDSLN,C'*'        TEST 'ALL' LENGTHS                           
         BE    GL10                                                             
         CLI   SVPRD2,0            TEST PIGGYBACK PRD                           
         BNE   GL6                 YES                                          
         CLC   XOLDSLN,XOLDTLN     NO - TLN SHOULD = SLN                        
         BNE   GLERR                                                            
         B     GL10                                                             
*                                                                               
GL6      CLC   XOLDSLN,XOLDTLN     TEST TLN=SLN                                 
         BNE   GL10                NO - SPLIT LENGTH ENTERED                    
         ZIC   R1,XOLDSLN          ELSE ASSUME INPUT IS TLN                     
         SRL   R1,1                SO SPLIT IT EVENLY                           
         GOTO1 USER5               MAKE SURE LENGTH VALID                       
         STC   R1,XOLDSLN                                                       
*                                                                               
GL10     OI    4(R2),X'20'                                                      
         SPACE 1                                                                
         BAS   RE,DSPOLD           DISPLAY 'FROM' DATA                          
         B     GL20                                                             
         EJECT                                                                  
* READ 'FROM' DATA AND FORMAT TO SCREEN  *                                      
         SPACE 1                                                                
DSPOLD   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(1),SVPRD                                                   
         MVC   KEY+5(2),XOLDMKT                                                 
         MVC   KEY+7(1),SVEST                                                   
*                                                                               
DSPOLD2  LA    R2,XFRDSPH                                                       
         USING DSPLD,R2                                                         
         MVC   DSPLTTL(8),=C'* FROM *'                                          
*                                                                               
         MVC   DSPLPRD(3),QPRD                                                  
         CLI   SVPRD2,0                                                         
         BE    DSPOLD4                                                          
         MVI   DSPLPRD+3,C'-'                                                   
         MVC   DSPLPRD+4(L'QPRD2),QPRD2                                         
*                                                                               
DSPOLD4  LH    R0,XOLDMKT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPLMKT,DUB                                                      
         MVC   DSPLMKNM,XOLDMKNM                                                
*                                                                               
         GOTO1 HIGH                                                             
         B     DSPOLD8                                                          
*                                                                               
DSPOLD6  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
DSPOLD8  CLC   KEY(8),KEYSAVE      TY/A-M/C/P/MK/EST                            
         BNE   DSPOLD10                                                         
         CLC   KEY+12(1),SVPRD2    TEST PARTNERS MATCH                          
         BNE   DSPOLD6                                                          
         TM    KEY+11,X'80'        TEST PASSIVE                                 
         BO    DSPOLD6                                                          
*                                                                               
         CLI   XOLDDPT,C'*'                                                     
         BE    *+14                                                             
         CLC   KEY+8(1),XOLDDPT                                                 
         BNE   DSPOLD6                                                          
*                                                                               
         CLI   XOLDSLN,C'*'                                                     
         BE    *+14                                                             
         CLC   KEY+9(1),XOLDSLN                                                 
         BNE   DSPOLD6                                                          
*                                                                               
         CLI   XOLDTLN,C'*'                                                     
         BE    DSPOLD9A                                                         
         CLI   SVPRD2,0            TEST PIGGYBACKS                              
         BNE   DSPOLD9             YES                                          
         CLC   KEY+10(1),XOLDSLN   NO - TLN SHOULD = SLN                        
         BNE   DSPOLD6                                                          
         B     DSPOLD9A                                                         
*                                                                               
DSPOLD9  CLI   XOLDTLN,C'*'                                                     
         BE    *+14                                                             
         CLC   KEY+10(1),XOLDTLN                                                
         BNE   DSPOLD6                                                          
*                                                                               
DSPOLD9A LA    RE,XLIST            ADD RECORD TO DPT/SLN LIST                   
         CLI   0(RE),0                                                          
         BE    DSPOLD9B                                                         
         LA    RE,3(RE)                                                         
         B     *-12                                                             
*                                                                               
DSPOLD9B MVC   0(3,RE),KEY+8       SAVE DPT/SLN/TLN                             
*                                                                               
         BAS   RE,GETGLS           DISPLAY GOAL RECORD DATA                     
*                                                                               
         OC    BPTS,BPTS           TEST ANY POINTS                              
         BNZ   *+14                                                             
         OC    BDOLS,BDOLS         TEST ANY DOLLARS                             
         BZ    DSPOLD6             NEITHER                                      
*                                                                               
         L     R0,XPTS                                                          
         A     R0,BPTS                                                          
         ST    R0,XPTS                                                          
         L     R0,XDOLS                                                         
         A     R0,BDOLS                                                         
         ST    R0,XDOLS                                                         
*                                                                               
         OI    6(R2),X'80'         SET TRANSMIT                                 
         ZIC   R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
         B     DSPOLD6                                                          
*                                                                               
DSPOLD10 MVI   ERRCD,NODTAERR                                                   
         LA    R2,XFRPERH          SET CURSOR POSN IF ERR                       
         OC    XPTS,XPTS           TEST ANY PTS OR DOLLARS                      
         BNZ   *+14                                                             
         OC    XDOLS,XDOLS                                                      
         BZ    GLERR                                                            
         B     EXIT                                                             
         EJECT                                                                  
* EDIT 'NEW' DATA                                                               
         SPACE 1                                                                
GL20     DS    0H                  TEST FOR SOME NEW DATA                       
         CLI   XFRNMKTH+5,0                                                     
         BNE   GL20X                                                            
         CLI   XFRNDPTH+5,0                                                     
         BNE   GL20X                                                            
         CLI   XFRNPRDH+5,0                                                     
         BNE   GL20X                                                            
         MVI   ERRCD,MSSNGERR                                                   
         LA    R2,XFRNMKTH                                                      
         B     GLERR                                                            
*                                                                               
GL20X    LA    R2,XFRNMKTH                                                      
         CLI   5(R2),0             TEST NEW MARKET ENTERED                      
         BE    GL22                NO                                           
         NI    4(R2),X'DF'         UNSET 'EDITED'                               
         GOTO1 USER1                                                            
         MVC   XNEWMKT,BMKT                                                     
         MVC   XNEWMKNM,SVMKTNAM                                                
*                                                                               
GL22     OI    4(R2),X'20'         SET NEW MARKET VALIDATED                     
*                                                                               
         LA    R2,XFRNDPTH                                                      
         MVC   XNEWDPT,XOLDDPT     SET DEFAULT VALUES                           
         MVC   XNEWSLN,XOLDSLN                                                  
         MVC   XNEWTLN,XOLDTLN                                                  
         CLI   5(R2),0                                                          
         BE    GL24                                                             
         BAS   RE,VALDPT                                                        
         SPACE 1                                                                
* '*'S MUST CORRESPOND BETWEEN OLD AND NEW *                                    
         SPACE 1                                                                
         MVI   ERRCD,INVERR                                                     
         CLI   XOLDDPT,C'*'                                                     
         BE    GL23A                                                            
         CLI   XNEWDPT,C'*'        OLD NOT= *, NEW SHOULD NOT= *                
         BE    GLERR                                                            
         B     GL23B                                                            
*                                                                               
GL23A    CLI   XNEWDPT,C'*'        OLD = *, NEW SHOULD = *                      
         BNE   GLERR                                                            
*                                                                               
GL23B    CLI   XOLDSLN,C'*'                                                     
         BE    GL23C                                                            
         CLI   XNEWSLN,C'*'                                                     
         BE    GLERR                                                            
         B     GL24                                                             
*                                                                               
GL23C    CLI   XNEWSLN,C'*'                                                     
         BNE   GLERR                                                            
*                                                                               
GL24     OI    4(R2),X'20'         SET NEW DPT/LEN VALID                        
         EJECT                                                                  
* READ CLIENT HEADER SO CAN EDIT PRODUCTS *                                     
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R8,AREC3                                                         
         ST    R8,AREC                                                          
         USING CLTHDRD,R8                                                       
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         LA    R5,XNEWPRD1                                                      
         USING XNEWD,R5                                                         
*                                                                               
         MVC   XPRD1,SVPRD           DEFAULT TO NEW PRD = OLD PRD               
         MVC   XPRD2,SVPRD2                                                     
         MVC   XQPRD1,QPRD                                                      
         MVC   XQPRD2,QPRD2                                                     
         MVI   XFACTOR,1             SET DEFAULT WEIGHT                         
*                                                                               
         LA    R2,XFRNPRDH                                                      
         CLI   5(R2),0                                                          
         BE    GL40                                                             
*                                                                               
         XC    ELEM,ELEM           CLEAR SCANNER TABLE                          
         GOTO1 VSCANNER,DMCB,(R2),ELEM                                          
         LA    R4,ELEM                                                          
         LA    R5,XNEWPRD1                                                      
         USING XNEWD,R5                                                         
*                                                                               
GL32     BAS   RE,EDTPRD           'PARSE' PRODUCT CODE(S)                      
*                                                                               
         LA    R1,XQPRD1                                                        
         BAS   RE,VALPRD                                                        
         BNE   GLERR                                                            
         MVC   XPRD1,FULL+3                                                     
*                                                                               
         LA    R1,XQPRD2                                                        
         CLI   0(R1),C' '                                                       
         BE    GL32A                                                            
         BAS   RE,VALPRD                                                        
         BNE   GLERR                                                            
         MVC   XPRD2,FULL+3                                                     
* PRD CODES SHOULD BE IN ALPHA SEQ                                              
         MVI   ERRCD,INVERR                                                     
         CLC   XQPRD1(3),XQPRD2                                                 
         BNL   GLERR                                                            
         EJECT                                                                  
* MAKE SURE NOT A DUPLICATE ENTRY *                                             
         SPACE 1                                                                
GL32A    LA    RE,XNEWPRD1                                                      
*                                                                               
GL32B    CR    R5,RE                                                            
         BE    GL32X                                                            
         CLC   0(2,RE),0(R5)                                                    
         BE    GLERR                                                            
         LA    RE,XNEWNEXT-XNEWD(RE)                                            
         B     GL32B                                                            
*                                                                               
GL32X    MVI   XFACTOR,1           SET DEFAULT WEIGHT                           
         CLI   1(R4),0             TEST WEIGHT ENTERED                          
         BE    GL34                NO                                           
         ICM   R0,15,8(R4)         TEST NON-ZERO VALUE                          
         BZ    GLERR                                                            
         STC   R0,XFACTOR          SET WEIGHT                                   
*                                                                               
GL34     LA    R5,XNEWNEXT         NEXT SLOT                                    
         LA    R4,32(R4)           NEXT SCANNER ENTRY                           
         CLI   0(R4),0                                                          
         BNE   GL32                                                             
*                                                                               
GL40     LA    R5,XNEWPRD1                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                                                               
GL42     IC    R0,XFACTOR          SUM WEIGHTS                                  
         AR    R1,R0                                                            
         LA    R5,XNEWNEXT                                                      
         CLI   XPRD1,0                                                          
         BNE   GL42                                                             
         ST    R1,XDIVSOR                                                       
         OI    4(R2),X'20'         SET PRODUCT VALIDATED                        
         EJECT                                                                  
* MAKE SURE THERE IS REALLY SOMETHING BEING CHANGED *                           
         SPACE 1                                                                
         CLC   XOLDMKT,XNEWMKT     TEST CHANGE OF MKT                           
         BNE   GL50                                                             
         CLC   SVPRD(2),XNEWPRD1   TEST CHANGE OF PRD                           
         BNE   GL50                                                             
         OC    XNEWPRD2,XNEWPRD2   OR AT LEAST 2 PRD ENTRIES                    
         BNZ   GL50                                                             
         CLC   XOLDDPT(3),XNEWDPT  TEST DPT/LEN ENTRIES EQUAL                   
         BNE   GL50                NO                                           
*                                                                               
         MVI   ERRAREA,X'FF'       SET FLAG FOR MESSAGE PRESENT                 
         MVC   GOLMSG(33),=C'* ERROR * NEW DATA LOOKS LIKE OLD'                 
         NI    XFRPERH+4,X'DF'     FORCE NEW EDIT                               
         LA    R2,XFRNMKTH                                                      
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     EXIT                                                             
*                                                                               
GL50     LA    R2,XFRDSPH                                                       
*                                                                               
GL50A    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             IF NO MORE ROOM, TFB !                       
         BE    GL50B                                                            
         OC    8(60,R2),8(R2)      FIND FIRST UNUSED DISPLAY LINE               
         BNZ   GL50A                                                            
*                                                                               
         BAS   RE,DSPNEW                                                        
*                                                                               
GL50B    LA    R2,XFRPERH                                                       
         OI    6(R2),X'40'         SET CURSOR POSITION                          
         MVI   ERRAREA,X'FF'       SET MESSAGE PRESENT FLAG                     
         MVC   GOLMSG(37),=CL37'** PRESS ''ENTER'' TO COPY GOALS **'            
         CLI   SVOPT1,C'C'         TEST COPY                                    
         BE    *+10                                                             
         MVC   GOLMSG(37),=C'** PRESS ''ENTER'' TO TRANSFER GOALS **'           
*                                                                               
         LA    RE,GOLMSGH                                                       
         ZIC   R0,0(RE)                                                         
         AR    RE,R0               POINT TO SERVICE REQUEST FIELD               
         OI    1(RE),X'01'         FORCE MODIFIED                               
         OI    6(RE),X'80'         XMT                                          
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* SUBROUTINE TO EDIT PIGGYBACK PRODUCT ENTRIES *                                
         SPACE 1                                                                
EDTPRD   NTR1                                                                   
         MVI   ERRCD,INVERR                                                     
         MVI   XPRD1,0                                                          
         MVI   XPRD2,0                                                          
         MVC   XQPRD1,SPACES                                                    
         MVC   XQPRD2,SPACES                                                    
*                                                                               
         LA    RE,12(R4)           PRODUCT INPUT                                
         ZIC   RF,0(R4)            INPUT LENGTH                                 
         LA    R1,XQPRD1           SET 'TO' ADDRESS                             
*                                                                               
EDPRD2   MVC   0(1,R1),0(RE)       MOVE INPUT CHAR                              
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BE    EDPRD4                                                           
         CLI   0(RE),C'-'                                                       
         BE    EDPRD4                                                           
         BCT   RF,EDPRD2                                                        
*                                                                               
EDPRD4   LA    R0,XQPRD1                                                        
         SR    R1,R0               GIVES DATA LENGTH                            
         CH    R1,=H'3'                                                         
         BH    GLERR                                                            
*                                                                               
         CLI   0(RE),C'-'                                                       
         BNE   EXIT                                                             
*                                                                               
         LA    R1,XQPRD2-1         SET 'TO' ADDRESS - 1                         
         B     *+10                AND SKIP OVER '-'                            
*                                                                               
EDPRD6   MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '          SPACE?                                       
         BE    EDPRD8              YES                                          
         BCT   RF,EDPRD6                                                        
*                                                                               
EDPRD8   LA    R0,XQPRD2                                                        
         SR    R1,R0                                                            
         CH    R1,=H'3'                                                         
         BH    GLERR                                                            
         B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE TO READ AND DISPLAY 'NEW' DATA *                                   
         SPACE 1                                                                
DSPNEW   NTR1                                                                   
         LA    R5,XNEWPRD1         POINT TO FIRST 'NEW' PRODUCT                 
         USING XNEWD,R5                                                         
*                                                                               
         MVC   DSPLTTL(8),=C'** TO **'                                          
         LH    R0,XNEWMKT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPLMKT,DUB                                                      
         MVC   DSPLMKNM,XNEWMKNM                                                
*                                                                               
DSPNEW2  XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(1),XPRD1                                                   
         MVC   KEY+5(2),XNEWMKT                                                 
         MVC   KEY+7(1),SVEST                                                   
*                                                                               
         GOTO1 HIGH                                                             
         B     DSPNEW6                                                          
*                                                                               
DSPNEW4  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
DSPNEW6  CLC   KEY(8),KEYSAVE        TY/A-M/C/P/MKT/EST                         
         BNE   DSPNEW10                                                         
         CLC   KEY+12(1),XPRD2       TEST PARTNERS MATCH                        
         BNE   DSPNEW4                                                          
         TM    KEY+11,X'80'          TEST PASSIVE                               
         BO    DSPNEW4                                                          
*                                                                               
         CLI   XNEWDPT,C'*'        TEST NEW DPT/LEN = 'ALL'                     
         BNE   DSPNEW8                                                          
         CLI   XNEWSLN,C'*'                                                     
         BNE   DSPNEW8                                                          
         SPACE 1                                                                
* NEW DPT LEN = ALL, TEST RECORD IN LIST *                                      
         SPACE 1                                                                
         LA    RE,XLIST                                                         
*                                                                               
DSPNEW7  CLC   KEY+8(3),0(RE)      TEST REC IN 'OLD' LIST                       
         BE    DSPNEW8X                                                         
         LA    RE,3(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   DSPNEW7                                                          
         B     DSPNEW4                                                          
*                                                                               
DSPNEW8  CLI   XNEWDPT,C'*'                                                     
         BE    *+14                                                             
         CLC   KEY+8(1),XNEWDPT                                                 
         BNE   DSPNEW4                                                          
*                                                                               
         CLI   XNEWSLN,C'*'                                                     
         BE    *+14                                                             
         CLC   KEY+9(1),XNEWSLN                                                 
         BNE   DSPNEW4                                                          
*                                                                               
         CLI   XPRD2,0             TEST PIGGYBACK ENTRY                         
         BNE   DSPNEW8A            YES                                          
         CLC   KEY+10(1),KEY+9     NO - TOTAL LEN MUST = PRD1 LEN               
         BNE   DSPNEW4                                                          
         B     DSPNEW8X                                                         
*                                                                               
DSPNEW8A CLI   XNEWTLN,C'*'                                                     
         BE    *+14                                                             
         CLC   KEY+10(1),XNEWTLN                                                
         BNE   DSPNEW4                                                          
*                                                                               
DSPNEW8X MVC   DSPLPRD(3),XQPRD1   SET PRODUCT IN DISPLAY                       
         CLI   XPRD2,0                                                          
         BE    *+14                                                             
         MVI   DSPLPRD+3,C'-'                                                   
         MVC   DSPLPRD+4(3),XQPRD2                                              
*                                                                               
         BAS   RE,GETGLS           DISPLAY DATA                                 
         OC    BDOLS,BDOLS                                                      
         BNZ   *+14                                                             
         OC    BPTS,BPTS                                                        
         BZ    DSPNEW4                                                          
*                                                                               
         OI    6(R2),X'80'         SET TRANSMIT                                 
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    DSPNEW4                                                          
         B     EXIT                IF NO MORE LINES, STOP READING               
*                                                                               
DSPNEW10 LA    R5,XNEWNEXT         NEXT PRODUCT                                 
         CLI   XPRD1,0                                                          
         BNE   DSPNEW2                                                          
         B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE READS GOAL RECORDS AND FORMATS DATA TO SCREEN *                    
         SPACE 1                                                                
GETGLS   NTR1                                                                   
         LA    R8,GOALREC                                                       
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         XC    BDOLS,BDOLS                                                      
         XC    BPTS,BPTS                                                        
*                                                                               
         LA    R3,BWEEKS           POINT TO DATE LIST                           
         MVI   ELCODE,X'21'                                                     
*                                                                               
GETGL2   LA    R6,GDELEM                                                        
*                                                                               
GETGL4   BAS   RE,NEXTEL                                                        
         BNE   GETGL6                                                           
         CLC   2(2,R6),0(R3)       MATCH WEEK                                   
         BNE   GETGL4                                                           
         ICM   R0,15,4(R6)         GET POINTS                                   
         N     R0,=X'3FFFFFFF'                                                  
         ICM   R1,15,BPTS                                                       
         N     R1,=X'3FFFFFFF'                                                  
*                                                                               
         TM    4(R6),X'40'         GOAL WEEK HAS 2 DECIMAL DEMO VALUE?          
         JZ    GETGL4A             NO, GOAL WEEK HAS 1 DECIMAL DEMO VAL         
         TM    BPTS,X'40'          YES, TOTAL DEMO HAS 2 DECIMAL VALUE?         
         JNZ   GETGL4M                  YES, NO ADJUSTMENT NEEDED               
         MHI   R1,10                    NO, ADJUST TOTAL TO BE 2 DEC            
         J     GETGL4M                      AND THEN ADDED GOAL WEEK            
*                                                                               
GETGL4A  TM    BPTS,X'40'          IS TOTAL DEMO ALSO 1 DECIMAL?                
         JZ    GETGL4M             YES, NO ADJUSTMENT NEEDED                    
         MHI   R0,10               NO, TOTAL IS 2 DEC SO ADJUST GOAL WK         
         OI    4(R6),X'40'         MAKE IT LOOK LIKE GOAL WK IS 2 DEC           
*                                                                               
GETGL4M  AR    R0,R1               ADD LIKE PRECISION DEMO VALUES               
         ST    R0,BPTS                                                          
*                                                                               
         TM    4(R6),X'40'         IF GOAL WEEK IS 2 DECIMAL                    
         JZ    *+8                                                              
         OI    BPTS,X'40'          THEN TOTAL DEMO IS 2 DECIMAL                 
*                                                                               
         ICM   R0,15,8(R6)         GET DOLLARS                                  
         A     R0,BDOLS                                                         
         ST    R0,BDOLS                                                         
*                                                                               
GETGL6   LA    R3,2(R3)            NEXT WEEK                                    
         OC    0(2,R3),0(R3)                                                    
         BNZ   GETGL2                                                           
*                                                                               
         OC    BDOLS,BDOLS                                                      
         BNZ   *+14                                                             
         OC    BPTS,BPTS                                                        
         BZ    EXIT                                                             
         EJECT                                                                  
* FORMAT DATA TO SCREEN *                                                       
         SPACE 1                                                                
         CLI   0(R2),9             TEST DISPLAY LINE AVAILABLE                  
         BNH   EXIT                NO - EXIT                                    
*                                                                               
         USING DSPLD,R2                                                         
*                                                                               
         MVC   DSPLDPT,KEY+8                                                    
         ZIC   R0,KEY+9                                                         
         EDIT  (R0),(3,DSPLSLN),ALIGN=LEFT                                      
*                                                                               
         CLC   KEY+9(1),KEY+10     TEST PIGGYBACK                               
         BE    GETGL8                                                           
         MVI   DSPLSLN+2,C'-'                                                   
         ZIC   R0,KEY+10                                                        
         ZIC   R1,KEY+9                                                         
         SR    R0,R1                                                            
         LA    R4,DSPLSLN+3                                                     
         EDIT  (R0),(3,(R4)),ALIGN=LEFT                                         
*                                                                               
GETGL8   L     R0,BPTS             POINTS                                       
         SRDL  R0,32                                                            
         TM    BPTS,X'40'          DO WE HAVE A 2 DEC VALUE?                    
         JZ    GETGL8O                                                          
         D     R0,=F'100'          DIVIDE BY 100                                
         J     GETGL8R                                                          
*                                                                               
GETGL8O  D     R0,=F'10'           DIVIDE BY 10                                 
*                                                                               
GETGL8R  LTR   R0,R0               TEST REMAINDER 0                             
         BNZ   GETGL10             NO - FORMAT WITH DECIMAL                     
         LR    R0,R1                                                            
         EDIT  (R0),(8,DSPLPTS)    YES, WE HAVE A WHOLE NUMBER                  
         B     GETGL12                                                          
*                                                                               
GETGL10  L     R0,BPTS                                                          
         TM    BPTS,X'40'          2 DECIMAL VALUE?                             
         JNZ   GETGL10D            YES                                          
GETGL10A EDIT  (R0),(8,DSPLPTS),1  NO, FORMAT WITH 1 DECIMAL                    
         J     GETGL12                                                          
*                                                                               
GETGL10D N     R0,=X'3FFFFFFF'     2 DEC TOTAL TOO BIG TO DISPLAY?              
         C     R0,=F'9999999'                                                   
         JH    GETGL10H            YES, MAKE IT A 1 DECIMAL VALUE               
         EDIT  (R0),(8,DSPLPTS),2                                               
         J     GETGL12                                                          
*                                                                               
GETGL10H CVD   R0,DUB                                                           
         SRP   DUB,63,5            DIVIDE BY 10 AND ROUND                       
         CVB   R0,DUB                                                           
         J     GETGL10A            DISPLAY AS 1 DECIMAL VALUE                   
*                                                                               
GETGL12  L     R0,BDOLS                                                         
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(6,DSPLDOLS)                                                
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
* TRANSFER GOALS *                                                              
         SPACE 1                                                                
GL100    LA    R2,XFRPERH          POINT TO PERIOD                              
         NI    4(R2),X'DF'         RESET 'EDITED' FLAG                          
         GOTO1 USER3               REBUILD WEEK LIST                            
*                                                                               
         BAS   RE,CLRSCR           CLEAR DISPLAY AREA                           
         LA    R2,XFRDSPH          POINT TO FIRST DISPLAY LINE                  
         USING DSPLD,R2                                                         
*                                                                               
         MVC   DSPLTTL(9),=C'AFTER XFR'                                         
         LH    R0,XNEWMKT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPLMKT,DUB                                                      
         MVC   DSPLMKNM,XNEWMKNM                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(1),SVPRD                                                   
         MVC   KEY+5(2),XOLDMKT                                                 
         MVC   KEY+7(1),SVEST                                                   
         GOTO1 HIGH                                                             
         B     GL104                                                            
*                                                                               
GL102    DS    0H                                                               
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         GOTO1 SEQ                                                              
         EJECT                                                                  
GL104    CLC   KEY(8),KEYSAVE      TY/A-M/C/P/MK/EST                            
         BNE   GL170                                                            
         CLC   KEY+12(1),SVPRD2    TEST PARTNERS MATCH                          
         BNE   GL102                                                            
         TM    KEY+11,X'80'        TEST PASSIVE                                 
         BO    GL102                                                            
*                                                                               
         CLI   XOLDDPT,C'*'                                                     
         BE    *+14                                                             
         CLC   KEY+8(1),XOLDDPT                                                 
         BNE   GL102                                                            
*                                                                               
         CLI   XOLDSLN,C'*'                                                     
         BE    *+14                                                             
         CLC   KEY+9(1),XOLDSLN                                                 
         BNE   GL102                                                            
*                                                                               
         CLI   XOLDTLN,C'*'                                                     
         BE    GL105X                                                           
         CLI   SVPRD2,0            TEST PIGGYBACKS                              
         BNE   GL105                                                            
         CLC   KEY+10(1),XOLDSLN                                                
         BNE   GL102                                                            
         B     GL105X                                                           
*                                                                               
GL105    CLC   KEY+10(1),XOLDTLN                                                
         BNE   GL102                                                            
*                                                                               
GL105X   LA    R8,GOALREC                                                       
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         MVC   XOLDKEY,KEY         SAVE KEY                                     
*                                                                               
         LA    R5,XNEWPRD1         POINT TO NEW PRODUCT LIST                    
         USING XNEWD,R5                                                         
         EJECT                                                                  
* GET NEW RECORD *                                                              
         SPACE 1                                                                
GL106    MVC   KEY(13),XOLDKEY                                                  
         MVC   KEY+4(1),XPRD1      SET NEW PRODUCT                              
         MVC   KEY+5(2),XNEWMKT      AND NEW MKT                                
         MVC   KEY+12(1),XPRD2     SET PRD 2                                    
*                                                                               
         CLI   XNEWDPT,C'*'                                                     
         BE    *+10                                                             
         MVC   KEY+8(1),XNEWDPT                                                 
*                                                                               
         CLI   XNEWSLN,C'*'                                                     
         BE    GL106X                                                           
         MVC   KEY+9(1),XNEWSLN                                                 
         MVC   KEY+10(1),XNEWTLN                                                
         CLI   XPRD2,0             TEST PIGGYBACK                               
         BE    GL106A              NO                                           
         CLC   XNEWSLN,XNEWTLN     TEST LENGTHS EQUAL                           
         BNE   GL106X              NO - THEY ARE ALREADY SPLIT                  
         SR    R1,R1               NO MORE INFO - ASSUME EQUAL SPLIT            
         IC    R1,XNEWTLN                                                       
         SRL   R1,1                                                             
         STC   R1,KEY+9            SET SLN FOR THIS PRD                         
         GOTO1 USER5               'CHKSLN'                                     
         B     GL106X                                                           
                                                                                
GL106A   MVC   KEY+9(1),XNEWTLN    FORCE SLN = TOTAL LENGTH                     
         MVC   KEY+10(1),XNEWTLN                                                
*                                                                               
GL106X   LA    R8,REC2             PRESET I/O ADDRESS                           
         ST    R8,AREC                                                          
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GL110                                                            
         GOTO1 GETREC                                                           
         B     GL120                                                            
*                                                                               
GL110    L     RE,AREC2            MOVE OLD RECORD TO NEW                       
         LHI   RF,REC2-REC                                                      
         LA    R0,GOALREC                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   REC2(13),KEYSAVE    SET KEY IN NEW RECORD                        
*                                                                               
         LA    R6,GDELEM-GKEY+REC2                                              
         LA    RE,GREDATE-GKEY+REC2                                             
         XC    0(2,RE),0(RE)       CLEAR CREATION DATE                          
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
GL112    CLI   0(R6),0             DELETE ALL THE ELEMENTS                      
         BE    GL120                                                            
         GOTO1 VRECUP,DMCB,REC2,(R6)                                            
         B     GL112                                                            
         EJECT                                                                  
GL120    LA    R8,BWEEKS                                                        
*                                                                               
GL121    MVI   ELCODE,X'21'                                                     
         LA    R6,GDELEM                                                        
*                                                                               
GL122    BAS   RE,NEXTEL                                                        
         BNE   GL128                                                            
         CLC   2(2,R6),0(R8)       MATCH WEEK                                   
         BNE   GL122                                                            
* GOT IT - CALCULATE ALLOCATION FOR THIS BRAND *                                
         MVC   ELEM(12),0(R6)      MOVE ELEMENT                                 
*                                                                               
         L     R1,ELEM+4                                                        
         N     R1,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         LLC   R0,XFACTOR          GET WEIGHT                                   
         AR    R0,R0               X 2                                          
         MR    R0,R0                                                            
         D     R0,XDIVSOR                                                       
         SRL   R1,1                                                             
         TM    ELEM+4,X'40'        TEST 2-DEC                                   
         JZ    *+8                                                              
         O     R1,=X'40000000'     SET 2-DEC FLAG IN ON PREVIOUSLY              
         ST    R1,ELEM+4           STORE POINTS IN TENTHS                       
*                                                                               
         L     R1,ELEM+8                                                        
         ZIC   R0,XFACTOR          GET WEIGHT                                   
         AR    R0,R0               X 2                                          
         MR    R0,R0                                                            
         D     R0,XDIVSOR                                                       
         SRL   R1,1                ROUND                                        
         ST    R1,ELEM+8                                                        
*                                                                               
         LR    R3,R6               SAVE ELEMENT POINTER                         
         LA    R6,GDELEM-GKEY+REC2                                              
*                                                                               
GL124    BAS   RE,NEXTEL                                                        
         BNE   GL126               NO CORRESPONDING ELEMENT                     
         CLC   2(2,R6),0(R8)       MATCH WEEK                                   
         BL    GL124                                                            
         BH    GL126                                                            
*                                                                               
         CLC   XOLDKEY,KEY         TEST XFR TO SAME RECORD                      
         BNE   GL124X                                                           
         MVI   XOLDFLAG,C'Y'       SET FLAG FOR LATER                           
         GOTO1 VRECUP,DMCB,REC2,(R6) DELETE ELEMENT                             
         B     GL126                                                            
*                                                                               
GL124X   L     R0,ELEM+4           ELEMENTS MATCH - ADD THEM                    
         N     R0,=X'3FFFFFFF'                                                  
         L     RE,4(R6)                                                         
         N     RE,=X'3FFFFFFF'                                                  
********                                                                        
         TM    ELEM+4,X'40'        GOAL WEEK HAS 2 DECIMAL DEMO VALUE?          
         JZ    GL125A              NO, GOAL WEEK HAS 1 DECIMAL DEMO VAL         
         TM    4(R6),X'40'         YES, REC DEMO HAS 2 DECIMAL VALUE?           
         JNZ   GL125S                   YES, NO ADJUSTMENT NEEDED               
         MHI   RE,10                    NO, ADJUST REC DEM TO 2 DEC             
         J     GL125S                       AND THEN ADDED GOAL WEEK            
*                                                                               
GL125A   TM    4(R6),X'40'         IS REC DEMO ALSO 1 DECIMAL?                  
         JZ    GL125S              YES, NO ADJUSTMENT NEEDED                    
         MHI   R0,10               NO, REC IS 2 DEC SO ADJUST GOAL WK           
         OI    ELEM+4,X'40'        MAKE IT LOOK LIKE GOAL WK IS 2 DEC           
*                                                                               
GL125S   AR    R0,RE               ADD LIKE PRECISION DEMO VALUES               
         ST    R0,4(R6)                                                         
         TM    ELEM+4,X'40'        DO WE HAVE 2 DEC                             
         JZ    *+8                                                              
         OI    4(R6),X'40'         KEEP REC DEMO AS 2 DEC                       
********                                                                        
         L     R0,ELEM+8                                                        
         A     R0,8(R6)                                                         
         ST    R0,8(R6)                                                         
         B     GL128                                                            
*                                                                               
GL126    DS    0H                                                               
         GOTO1 VRECUP,DMCB,REC2,ELEM,(R6)   ADD NEW ELEMENT                     
*                                                                               
GL128    LA    R8,2(R8)            NEXT WEEK                                    
         OC    0(2,R8),0(R8)                                                    
         BNZ   GL121                                                            
         SPACE 1                                                                
* ALL WEEKS PROCESSED - WRITE RECORD *                                          
         SPACE 1                                                                
         GOTO1 VDATCON,DMCB,(5,0),(2,HALF)                                      
         MVC   GACTDATE-GKEY+REC2(2),HALF                                       
         LA    RE,GREDATE-GKEY+REC2                                             
         OC    0(2,RE),0(RE)                                                    
         BNZ   *+10                                                             
         MVC   0(2,RE),HALF                                                     
*                                                                               
         CLC   KEY(13),KEYSAVE     TEST RECORD IS ON FIL                        
         BNE   GL129               NO                                           
*                                                                               
         TM    REC2+15,X'80'       TEST REC IS DELETED                          
         BO    GL128A                                                           
         GOTO1 PUTREC                                                           
         B     GL130                                                            
*                                                                               
GL128A   NI    REC2+15,X'7F'       UNSET DELETE BIT                             
         GOTO1 PUTREC                                                           
         NI    KEY+13,X'7F'        UNSET DELETE                                 
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
         CLI   XPRD2,0                                                          
         BE    *+14                                                             
         MVC   COMMAND,=C'UND'                                                  
         BAS   RE,PASSIVE                                                       
*                                                                               
         CLI   SVADVAGY,0          TEST ADVTSR CLIENT                           
         BZ    GL130               NO                                           
         SR    R0,R0                                                            
         IC    R0,KEY+1                                                         
         SRL   R0,4                                                             
         STC   R0,KEY+11                                                        
         NI    KEY+1,X'0F'                                                      
         OC    KEY+1(1),SVADVAGY                                                
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
         CLI   XPRD2,0                                                          
         BE    *+14                                                             
         MVC   COMMAND,=C'UND'                                                  
         BAS   RE,PASSIVE                                                       
         B     GL130                                                            
         SPACE 1                                                                
* ADD NEW RECORD *                                                              
         SPACE 1                                                                
GL129    DS    0H                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY OF RECORD BEING ADDED            
         GOTO1 ADDREC                                                           
*                                                                               
         CLI   XPRD2,0             TEST NEED PIGGYBACK                          
         BE    GL130               NO                                           
         MVC   COMMAND,=C'DMADD'                                                
         BAS   RE,PASSIVE                                                       
         SPACE 1                                                                
* DISPLAY UPDATED RECORD *                                                      
         SPACE 1                                                                
GL130    CLI   0(R2),9             TEST MORE SCREEN LINES                       
         BNH   GL138               NO                                           
         XC    BDOLS,BDOLS                                                      
         XC    BPTS,BPTS                                                        
*                                                                               
         LA    R3,BWEEKS           POINT TO DATE LIST                           
         MVI   ELCODE,X'21'                                                     
*                                                                               
GL132    LA    R6,(GDELEM-GKEY)+REC2                                            
*                                                                               
GL134    BAS   RE,NEXTEL                                                        
         BNE   GL136                                                            
         CLC   2(2,R6),0(R3)       MATCH WEEK                                   
         BNE   GL134                                                            
*********                                                                       
         ICM   R0,15,4(R6)         GET POINTS                                   
         N     R0,=X'3FFFFFFF'                                                  
         ICM   R1,15,BPTS                                                       
         N     R1,=X'3FFFFFFF'                                                  
*                                                                               
         TM    4(R6),X'40'         GOAL WEEK HAS 2 DECIMAL DEMO VALUE?          
         JZ    GL134A              NO, GOAL WEEK HAS 1 DECIMAL DEMO VAL         
         TM    BPTS,X'40'          YES, TOTAL DEMO HAS 2 DECIMAL VALUE?         
         JNZ   GL134S                   YES, NO ADJUSTMENT NEEDED               
         MHI   R1,10                    NO, ADJUST TOTAL TO BE 2 DEC            
         J     GL134S                       AND THEN ADDED GOAL WEEK            
*                                                                               
GL134A   TM    BPTS,X'40'          IS TOTAL DEMO ALSO 1 DECIMAL?                
         JZ    GL134S              YES, NO ADJUSTMENT NEEDED                    
         MHI   R0,10               NO, TOTAL IS 2 DEC SO ADJUST GOAL WK         
         OI    4(R6),X'40'         MAKE IT LOOK LIKE GOAL WK IS 2 DEC           
*                                                                               
GL134S   AR    R0,R1               ADD LIKE PRECISION DEMO VALUES               
         ST    R0,BPTS                                                          
         TM    4(R6),X'40'                                                      
         JZ    *+8                                                              
         OI    BPTS,X'40'                                                       
*********                                                                       
         ICM   R0,15,8(R6)         GET DOLLARS                                  
         A     R0,BDOLS                                                         
         ST    R0,BDOLS                                                         
*                                                                               
GL136    LA    R3,2(R3)            NEXT WEEK                                    
         OC    0(2,R3),0(R3)                                                    
         BNZ   GL132                                                            
         EJECT                                                                  
* FORMAT DATA TO SCREEN *                                                       
         SPACE 1                                                                
         USING DSPLD,R2                                                         
*                                                                               
         MVC   DSPLPRD(3),XQPRD1   PRODUCT CODE                                 
         CLI   1(R5),0                                                          
         BE    *+14                                                             
         MVI   DSPLPRD+3,C'-'                                                   
         MVC   DSPLPRD+4(3),XQPRD2                                              
*                                                                               
         MVC   DSPLDPT,REC2+8                                                   
         ZIC   R0,REC2+9                                                        
         CVD   R0,DUB                                                           
         EDIT  (R0),(3,DSPLSLN),ALIGN=LEFT                                      
*                                                                               
         CLC   REC2+9(1),REC2+10   TEST PIGGYBACK                               
         BE    GL137                                                            
         MVI   DSPLSLN+2,C'-'                                                   
         SR    R0,R0                                                            
         IC    R0,REC2+10                                                       
         SR    R1,R1                                                            
         IC    R1,REC2+9                                                        
         SR    R0,R1                                                            
         LA    R4,DSPLSLN+3                                                     
         EDIT  (R0),(3,(R4)),ALIGN=LEFT                                         
*********                                                                       
GL137    L     R0,BPTS             POINTS                                       
         SRDL  R0,32                                                            
         TM    BPTS,X'40'          DO WE HAVE A 2 DEC VALUE?                    
         JZ    GL137A                                                           
         D     R0,=F'100'          DIVIDE BY 100                                
         J     GL137B                                                           
*                                                                               
GL137A   D     R0,=F'10'           DIVIDE BY 10                                 
*                                                                               
GL137B   LTR   R0,R0               TEST REMAINDER 0                             
         BNZ   GL137C              NO - FORMAT WITH DECIMAL                     
         LR    R0,R1                                                            
         EDIT  (R0),(8,DSPLPTS)    YES, WE HAVE A WHOLE NUMBER                  
         B     GL137X                                                           
*                                                                               
GL137C   L     R0,BPTS                                                          
         TM    BPTS,X'40'          2 DECIMAL VALUE?                             
         JNZ   GL137E              YES                                          
GL137D   EDIT  (R0),(8,DSPLPTS),1  NO, FORMAT WITH 1 DECIMAL                    
         J     GL137X                                                           
*                                                                               
GL137E   N     R0,=X'3FFFFFFF'     2 DEC TOTAL TOO BIG TO DISPLAY?              
         C     R0,=F'9999999'                                                   
         JH    GL137H              YES, MAKE IT A 1 DECIMAL VALUE               
         EDIT  (R0),(8,DSPLPTS),2                                               
         J     GL137X                                                           
*                                                                               
GL137H   CVD   R0,DUB                                                           
         SRP   DUB,63,5            DIVIDE BY 10 AND ROUND                       
         CVB   R0,DUB                                                           
         J     GL137D              DISPLAY AS 1 DECIMAL VALUE                   
*********                                                                       
GL137X   L     R0,BDOLS                                                         
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(6,DSPLDOLS)                                                
*                                                                               
         OI    6(R2),X'80'         SET TRANSMIT BIT                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT DISPLAY LINE                   
         EJECT                                                                  
GL138    LA    R5,XNEWNEXT         NEXT PRODUCT                                 
         CLI   XPRD1,0                                                          
         BNE   GL106                                                            
         SPACE 1                                                                
* ALL NEW RECORDS WRITTEN - REREAD OLD REC                                      
* AND DELETE APPROPRIATE ELEMENTS                                               
         SPACE 1                                                                
         MVC   KEY,XOLDKEY                                                      
         GOTO1 HIGH                RESTORE DIR FOR SEQ NOW                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   XOLDFLAG,C'Y'       TEST REC ALREADY WRITTEN                     
         BE    GL160                                                            
*                                                                               
         CLI   SVOPT1,C'C'         TEST ACTION=COPY                             
         BE    GL160               YES - NO DELETES NEEDED                      
*                                                                               
         LA    R8,GOALREC                                                       
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         SPACE 1                                                                
* DELETE WEEKLY ELEMENTS *                                                      
         SPACE 1                                                                
GL141X   LA    R8,BWEEKS                                                        
*                                                                               
GL142    LA    R6,GDELEM                                                        
         MVI   ELCODE,X'21'                                                     
GL144    BAS   RE,NEXTEL                                                        
         BNE   GL146                                                            
         CLC   2(2,R6),0(R8)                                                    
         BNE   GL144                                                            
         GOTO1 VRECUP,DMCB,GOALREC,(R6)                                         
GL146    LA    R8,2(R8)                                                         
         OC    0(2,R8),0(R8)                                                    
         BNZ   GL142                                                            
*                                                                               
GL146X   DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),GACTDATE                                      
*                                                                               
         LA    R6,GDELEM                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST ANY ELEMENTS LEFT                       
         BE    GL148               NO                                           
         GOTO1 PUTREC                                                           
         B     GL160                                                            
         SPACE 1                                                                
* NO ELEMENTS LEFT - DELETE RECORD *                                            
         SPACE 1                                                                
GL148    DS    0H                                                               
         OI    GCNTRLS,X'80'                                                    
         GOTO1 PUTREC                                                           
*                                                                               
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
         CLI   SVPRD2,0            TEST PIGGYBACK                               
         BE    *+14                NO                                           
         MVC   COMMAND,=C'DEL'                                                  
         BAS   RE,PASSIVE                                                       
         SPACE 1                                                                
* CHECK FOR ADVTSR *                                                            
         SPACE 1                                                                
         CLI   SVADVAGY,0                                                       
         BZ    GL160                                                            
         SR    R0,R0                                                            
         IC    R0,KEY+1                                                         
         SRL   R0,4                                                             
         STC   R0,KEY+11                                                        
         NI    KEY+1,X'0F'                                                      
         OC    KEY+1(1),SVADVAGY                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
         CLI   SVPRD2,0                                                         
         BE    *+14                                                             
         MVC   COMMAND,=C'DEL'                                                  
         BAS   RE,PASSIVE                                                       
*                                                                               
GL160    MVI   XOLDFLAG,0          CLEAR FLAG                                   
         B     GL102               AND SEE IF MORE RECS TO XFR                  
*                                                                               
GL170    DS    0H                                                               
         NI    XFROMKTH+4,X'DF'                                                 
         NI    XFRODPTH+4,X'DF'                                                 
         NI    XFRNMKTH+4,X'DF'                                                 
         NI    XFRNDPTH+4,X'DF'                                                 
         NI    XFRNPRDH+4,X'DF'                                                 
         MVI   ERRAREA,X'FF'       SET MESSAGE PRESENT                          
         MVC   GOLMSG(24),=CL24'** COPY COMPLETED **'                           
         CLI   SVOPT1,C'C'                                                      
         BE    *+10                                                             
         MVC   GOLMSG(24),=C'** TRANSFER COMPLETED **'                          
         LA    R2,XFROMKTH                                                      
         OI    6(R2),X'40'                                                      
*                                                                               
         BAS   RE,GLAUTH           CALL SPAUTH                                  
         BAS   RE,GLREQ                                                         
         B     EXIT                                                             
         EJECT                                                                  
* ADD REQUESTS FOR EACH MARKET AND PRODUCT AS NEEDED *                          
         SPACE 1                                                                
GLREQ    NTR1                                                                   
         CLI   SVPPROF1,C'M'       TEST T/A BY MKT                              
         BNE   GLREQ2              NO                                           
         CLC   SVQOLD(2),XOLDMKT   TEST OLD MKT CHANGED                         
         BE    *+16                                                             
         XC    SVQOLD,SVQOLD       CLEAR REQ LIST ON                            
         MVC   SVQOLD(2),XOLDMKT      CHANGE OF MKT                             
*                                                                               
         CLC   SVQNEW(2),XNEWMKT   TEST NEW MKT CHANGED                         
         BE    *+16                                                             
         XC    SVQNEW,SVQNEW                                                    
         MVC   SVQNEW(2),XNEWMKT                                                
*                                                                               
GLREQ2   XC    WORK,WORK                                                        
         MVC   WORK(2),XOLDMKT                                                  
         MVC   WORK+2(3),QPRD                                                   
         MVC   WORK+5(1),SVPRD                                                  
         BAS   RE,GENREQ                                                        
*                                                                               
         MVC   WORK(2),XNEWMKT                                                  
         LA    R5,XNEWPRD1                                                      
         USING XNEWD,R5                                                         
*                                                                               
GLREQ4   CLI   XPRD1,0                                                          
         BE    EXIT                                                             
         MVC   WORK+2(3),XQPRD1                                                 
         MVC   WORK+5(1),XPRD1                                                  
         BAS   RE,GENREQ                                                        
         CLI   XPRD2,0                                                          
         BE    GLREQ6                                                           
         MVC   WORK+2(3),XQPRD2                                                 
         MVC   WORK+5(1),XPRD2                                                  
         BAS   RE,GENREQ                                                        
*                                                                               
GLREQ6   LA    R5,XNEWNEXT                                                      
         B     GLREQ4                                                           
         SPACE 2                                                                
GENREQ   NTR1                                                                   
         MVC   DUB(5),GOLPLNR                                                   
         OC    DUB(5),SPACES                                                    
         CLC   =C'SPOT ',DUB                                                    
         BE    EXIT                                                             
         CLI   SVPPROF1,C'N'       TEST NO T/A                                  
         BE    EXIT                                                             
*                                                                               
         LA    R1,SVQOLD+2                                                      
         CLI   SVPPROF1,C'M'       TEST T/A BY MKT                              
         BNE   GENREQ2             NO                                           
         CLC   WORK(2),SVQOLD                                                   
         BE    GENREQ2                                                          
         LA    R1,SVQNEW+2                                                      
*                                                                               
GENREQ2  ZIC   RE,WORK+5           GET PRD CODE                                 
         BCTR  RE,0                                                             
         SRDL  RE,3                                                             
         AR    RE,R1               POINT TO BYTE IN TABLE                       
         SRL   RF,29                                                            
         IC    RF,QBITS(RF)                                                     
         EX    RF,*+8              TEST PRD ALREADY REQUESTED                   
         B     *+8                                                              
         TM    0(RE),0 *EXECUTED*                                               
         BO    EXIT                YES - DONE                                   
         STC   RF,BYTE                                                          
         OC    0(1,RE),BYTE                                                     
         B     GENREQ3                                                          
QBITS    DC    X'8040201008040201'                                              
*                                                                               
GENREQ3  XC    ELEM,ELEM                                                        
         LA    R4,ELEM+26                                                       
         MVI   0(R4),C' '                                                       
         MVC   1(79,R4),0(R4)                                                   
         MVC   0(2,R4),=C'U9'                                                   
         MVC   2(2,R4),AGYALPHA                                                 
         MVC   4(1,R4),GOLMD                                                    
         MVC   5(3,R4),QCLT                                                     
         MVC   11(3,R4),WORK+2                                                  
*                                                                               
         LH    R0,WORK             GET MARKET                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  14(4,R4),DUB                                                     
*                                                                               
         CLI   SVPPROF1,C'M'       TEST T/A'S BY MKT                            
         BE    *+10                                                             
         MVC   14(4,R4),=C'ALL '                                                
*                                                                               
         ZIC   R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R4),DUB                                                     
*                                                                               
         MVC   31(2,R4),=C'ES'                                                  
*                                                                               
         MVI   62(R4),C'G'                                                      
         MVC   68(12,R4),GOLPLNR                                                
         OC    68(12,R4),SPACES                                                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',ELEM,ELEM                    
         B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE TO VALIDATE PRODUCT CODE AND TEST EST ON FILE *                    
* ON ENTRY R1 HAS PRODUCT ADDRESS *                                             
         SPACE 1                                                                
VALPRD   NTR1                                                                   
*                                                                               
         LA    RF,CLIST                                                         
*                                                                               
VALPRD2  CLC   0(3,R1),0(RF)                                                    
         BE    VALPRD4                                                          
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    VALPRD2                                                          
         B     GLERR                                                            
VALPRD4  MVC   FULL(4),0(RF)       MOVE PRD DATA                                
         CLI   3(RF),X'FF'         TEST POL                                     
         BE    GLERR                                                            
         SPACE 1                                                                
* NOW VALIDATE ESTIMATE ON FILE *                                               
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),FULL                                                    
         MVC   KEY+7(1),SVEST                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    VALPRD10                                                         
         MVI   ERRAREA,X'FF'       INDICATE MESSAGE PRESENT                     
         MVC   GOLMSG(41),=C'** ERROR - NO ESTIMATE FOR PRODUCT XXX **'         
         MVC   GOLMSG+35(3),KEYSAVE+4                                           
         B     NEQXIT                                                           
*                                                                               
VALPRD10 L     R0,AREC             SAVE CURRENT AREC                            
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC             RESTORE REC ADDRESS                          
         SPACE 1                                                                
* NOW MAKE SURE DATES ARE IN EST PERIOD FOR NEW PRODUCT *                       
         SPACE 1                                                                
         MVI   ERRCD,PERERR                                                     
         LA    R4,(ESTART-ESTHDR)+REC2                                          
         GOTO1 VDATCON,DMCB,(R4),(2,HALF)                                       
         CLC   BWEEKS(2),HALF                                                   
         BL    GLERR                                                            
*                                                                               
         LA    R4,(EEND-ESTHDR)+REC2                                            
         GOTO1 VDATCON,DMCB,(R4),(2,HALF)                                       
         LA    RE,BWEEKS                                                        
         OC    2(2,RE),2(RE)                                                    
         BZ    *+12                                                             
         LA    RE,2(RE)                                                         
         B     *-14                                                             
         CLC   0(2,RE),HALF                                                     
         BH    GLERR                                                            
         B     EQXIT                                                            
         SPACE 1                                                                
EQXIT    CR    RE,RE                                                            
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RE,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE TO VALIDATE DAYPART/SPOTLENGTH                                     
* '*' MEANS 'ALL' FOR EITHER ENTRY                                              
* RETURN EDITED VALUES IN XNEWDPT/XNEWSLN                                       
         SPACE 1                                                                
VALDPT   NTR1                                                                   
         MVC   XNEWDPT,8(R2)                                                    
         MVI   XNEWSLN,0                                                        
         MVI   XNEWTLN,0                                                        
         CLI   XNEWDPT,C'*'                                                     
         BE    VALDPT10                                                         
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         MVI   ERRCD,DPTERR                                                     
         LA    R1,SVMENU                                                        
*                                                                               
VALDPT2  CLC   XNEWDPT,0(R1)                                                    
         BE    VALDPT10                                                         
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   VALDPT2                                                          
         B     GLERR                                                            
*                                                                               
VALDPT10 LA    R4,9(R2)            POINT TO SLN                                 
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                ADJUST FOR DPT                               
         STM   R4,R5,DUB           SAVE ADDRESS/LEN                             
         MVI   XNEWSLN,C'*'        SET DEFAULT = ALL                            
         MVI   XNEWTLN,C'*'                                                     
         LTR   R5,R5                                                            
         BZ    EXIT                                                             
         MVI   ERRCD,SLNERR                                                     
         CH    R5,=H'1'                                                         
         BNE   VALDPT16                                                         
         CLI   0(R4),C'*'                                                       
         BE    EXIT                                                             
         B     VALDPT16                                                         
*                                                                               
VALDPT14 CLI   0(R4),C'-'                                                       
         BE    VALDPT18                                                         
*                                                                               
VALDPT16 CLI   0(R4),C'0'                                                       
         BL    GLERR                                                            
         CLI   0(R4),C'9'                                                       
         BH    GLERR                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,VALDPT14                                                      
*                                                                               
VALDPT18 LR    RE,R4               SAVE DATA POINTER                            
         L     RF,DUB              GET START ADDRESS                            
         SR    RE,RF               GIVES LENGTH                                 
         BNP   GLERR                                                            
         BCTR  RE,0                SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)    * EXECUTED *                                      
         CVB   R1,DUB                                                           
         GOTO1 USER5               'CHKSLN'                                     
         STC   R1,XNEWSLN          SET AS PRODUCT SLN                           
         STC   R1,XNEWTLN          AND TOTAL SLN                                
*                                                                               
         CLI   0(R4),C'-'          TEST PIGGYBACK ENTERED                       
         BNE   EXIT                                                             
*                                                                               
         LA    R4,1(R4)            SKIP PAST -                                  
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BZ    GLERR                                                            
         STM   R4,R5,DUB           SAVE START ADDRESS/LEN                       
*                                                                               
VALDPT20 CLI   0(R4),C'0'                                                       
         BL    GLERR                                                            
         CLI   0(R4),C'9'                                                       
         BH    GLERR                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,VALDPT20                                                      
*                                                                               
         LR    RE,R4                                                            
         L     RF,DUB              GET DATA START ADDRESS                       
         SR    RE,RF               GIVES LENGTH                                 
         BNP   GLERR                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF) *EXECUTED*                                           
*                                                                               
         MVI   ERRCD,SLNERR                                                     
         CVB   R1,DUB                                                           
         GOTO1 USER5               'CHKSLN'                                     
*                                                                               
         ZIC   R0,XNEWSLN          ADD SLNS TO GET TOTAL LEN                    
         AR    R1,R0                                                            
         STC   R1,XNEWTLN                                                       
         GOTO1 USER5               'CHKSLN'                                     
         B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINES TO ADD AND DELETE PASSIVE PIGGYBACK POINTERS                      
* ACTIVE KEY IS IN 'KEY'                                                        
         SPACE 1                                                                
PASSIVE  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY+4(1),KEYSAVE+12   MOVE PRD                                   
         ZIC   R1,KEYSAVE+10         GET TLN                                    
         ZIC   R0,KEYSAVE+9          GET ACTIVE SLN                             
         SR    R1,R0                                                            
         STC   R1,KEY+9              SET PASSIVE SLN                            
         OI    KEY+11,X'80'          INDICATE PASSIVE POINTER                   
         MVC   KEY+12(1),KEYSAVE+4   MOVE ACTIVE PRD                            
*                                                                               
         CLC   =C'DMADD',COMMAND                                                
         BNE   DELPSSV                                                          
* ADD NEW PASSIVE POINTER *                                                     
         MVC   WORK,KEYSAVE        SAVE ORIGINAL KEY                            
         GOTO1 HIGH                CHECK TO MAKE SURE IT DOESN'T EXIST          
         CLC   KEY(13),KEYSAVE     DOES PASSIVE EXIST?                          
         BE    ADDPSSX             YES, DON'T ADD IT                            
         MVC   COMMAND,=C'DMADD'   THIS IS BEYOND COMMENT !                     
         MVC   KEY,KEYSAVE         PASSIVE WE WANT TO ADD                       
         GOTO1 DIR                                                              
ADDPSSX  MVC   KEY,WORK            RESTORE ORIGINAL KEY                         
         B     EXIT                                                             
*                                                                               
DELPSSV  CLC   =C'DEL',COMMAND                                                  
         BNE   UNDPSSV                                                          
* DELETE PASSIVE POINTER *                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         B     EXIT                                                             
*                                                                               
UNDPSSV  CLC   =C'UND',COMMAND                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
* UNDELETE PASSIVE                                                              
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         B     EXIT                                                             
         EJECT                                                                  
CLRSCR   NTR1                                                                   
         LA    R2,XFRDSPH                                                       
*                                                                               
CLRSCR2  ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'            SET FOR EX                                   
         EX    RE,CLROC                                                         
         BZ    CLRSCR4                                                          
         EX    RE,CLRCLC                                                        
         BE    CLRSCR4                                                          
         EX    RE,CLRXC                                                         
         OI    6(R2),X'80'         SET XMT BIT                                  
*                                                                               
CLRSCR4  LA    R2,9(R2,RE)         POINT TO NEXT DISPLAY LINE                   
         CLI   0(R2),9                                                          
         BH    CLRSCR2                                                          
         B     EXIT                                                             
*                                                                               
CLROC    OC    8(0,R2),8(R2)                                                    
CLRCLC   CLC   8(0,R2),SPACES                                                   
CLRXC    XC    8(0,R2),8(R2)                                                    
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCODE                                                   
         BNE   NEXTEL                                                           
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 2                                                                
GLERR    GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
********************************************************************            
* CALL SPAUTH TO ADD OR UPDATE SUPERDESK AUTH MKT LEVEL RECORDS    *            
********************************************************************            
GLAUTH   NTR1                                                                   
         TM    SVEFLAG1,EF1SDE           IS THIS A SUPERDESK EST?               
         BNO   GLAUTHX                                                          
*                                                                               
         PUSH  USING                                                            
         USING SPAUTHD,WORK                                                     
         XC    SPAUTHD(SPAUTHLQ),SPAUTHD                                        
         MVC   SPACOM,VCOMFACS                                                  
         MVC   SPAKAM,SVAGYMD                                                   
         MVC   SPAKCLT,SVCLT                                                    
         MVC   SPAKPRD,SVPRD                                                    
         MVC   SPAKPRD2,SVPRD2                                                  
         MVC   SPAKEST,SVEST                                                    
         MVC   SPAKMKT,BMKT                                                     
*                                                                               
         CLI   SVOPT1,C'X'         TEST XFR OR                                  
         BE    *+12                                                             
         CLI   SVOPT1,C'C'         TEST COPY                                    
         BNE   *+10                                                             
         MVC   SPAKMKT,XNEWMKT     USE NEW MKT                                  
*                                                                               
         LA    RF,REC2                                                          
         ST    RF,SPAIO                                                         
         LA    RF,BWEEKS                                                        
         MVC   SPASDTE,0(RF)       GOAL START DATE                              
         CLI   2(RF),0                                                          
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     *-12                                                             
         MVC   SPAEDTE,0(RF)       GOAL END DATE                                
         GOTO1 VDATCON,DMCB,(2,SPAEDTE),(0,WORK2) END DATE (CHAR)               
         GOTO1 VADDAY,(R1),(C'D',WORK2),WORK2+6,6 +6 FOR END OF WEEK            
         GOTO1 VDATCON,(R1),WORK2+6,(2,SPAEDTE)   END DATE (COMPRESSED)         
*                                                                               
         MVI   SPAUPDT,SPAUPGOL    UPDATE GOAL DATES                            
*                                                                               
         GOTO1 =V(SPAUTH),WORK,RR=YES                                           
         CLI   SPAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GLAUTHX  XIT1  ,                                                                
         POP   USING                                                            
* DSECT FOR 'NEW' PRODUCT ENTRIES                                               
         SPACE 1                                                                
XNEWD    DSECT                                                                  
XPRD1    DS    XL1                 PRD1 BINARY                                  
XPRD2    DS    XL1                 PRD2 BINARY                                  
XQPRD1   DS    CL3                 PRD1 EBCDIC                                  
XQPRD2   DS    CL3                 PRD2 EBCDIC                                  
XFACTOR  DS    XL1                 WEIGHT OF THIS ENTRY                         
XNEWNEXT EQU   *                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGOLWRK                                                       
       ++INCLUDE SPGENAUTH                                                      
       ++INCLUDE SPAUTHD                                                        
         EJECT                                                                  
T202FFD  DSECT                                                                  
         ORG   GOLOPTS+L'GOLOPTS                                                
       ++INCLUDE SPGOLF4D                                                       
         SPACE 2                                                                
DSPLD    DSECT                                                                  
DSPLHDR  DS    XL8                                                              
DSPLTTL  DS    CL10                FROM/TO                                      
         DS    CL1                                                              
DSPLPRD  DS    CL7                                                              
         DS    CL2                                                              
DSPLMKT  DS    CL4                                                              
         DS    CL1                                                              
DSPLMKNM DS    CL16                                                             
         DS    CL2                                                              
DSPLDPT  DS    CL1                                                              
DSPLSLN  DS    CL6                                                              
         DS    CL2                                                              
DSPLPTS  DS    CL8                                                              
         DS    CL2                                                              
DSPLDOLS DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064SPGOL04   02/26/20'                                      
         END                                                                    
