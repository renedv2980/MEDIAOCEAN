*          DATA SET NSSLDEXT   AT LEVEL 020 AS OF 05/01/02                      
*PHASE NSSLDEXT,+0                                                              
*INCLUDE CLPACK                                                                 
*INCLUDE MSUNPK                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE DEMOVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'NSSLDEXT - CONVERT OLD BEER TO NEW BEER LESS FILLING'           
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
         NMOD1 20,DMLDEXT,RR=R5                                                 
         USING WORKD,RC                                                         
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING DMLDEXT+4096,RA                                                  
*                                                                               
         ST    R5,RELO                                                          
         B     DMXCTL                                                           
RELO     DC    A(0)                                                             
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     R9,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,R9                                                        
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
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*                                                                               
         MVC   TITLE(24),=CL24'SPTFILE DEMO CONVERSION'                         
*                                                                               
         LA    R1,ADCONS                                                        
         LA    RF,ADCONX                                                        
INIT1    L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         CR    R1,RF                                                            
         BL    INIT1                                                            
* CLEAR CLTTAB                                                                  
         LA    R0,4                                                             
         L     RE,ACLTTAB                                                       
         XC    0(256,RE),0(RE)                                                  
         LA    RE,256(RE)                                                       
         BCT   R0,*-10                                                          
         B     DMXIT                                                            
*                                                                               
         L     R1,APARAMC                                                       
         LA    RE,CNSPOT4                                                       
         CLI   0(R1),C'4'                                                       
         BE    INIT2                                                            
         LA    RE,CNSPOT5                                                       
         CLI   0(R1),C'5'                                                       
         BE    INIT2                                                            
         LA    RE,CNSPOT6                                                       
         CLI   0(R1),C'6'                                                       
         BE    INIT2                                                            
         LA    RE,CNSPOT0                                                       
*                                                                               
INIT2    ST    RE,ACNTAB           SAVE CN AGY LOOK-UP ADDRESS                  
*                                                                               
INIT4    B     DMXIT                                                            
*                                                                               
CNSPOT0  DC    X'000000000000000000000000000000'                                
CNSPOT4  DC    X'0000000000060008090A00000D0E00'                                
CNSPOT5  DC    X'000200000506000000000B000D0E00'                                
CNSPOT6  DC    X'0002030405000008000A0B00000000'                                
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
         LA    R3,0(R3)            CLEAR HOB                                    
         SR    RE,RE                                                            
         ICM   RE,3,13(R3)                                                      
         AR    RE,R3                                                            
         XC    0(2,RE),0(RE)       SET X'00' AT E-O-R                           
*                                                                               
         CLI   0(R3),0             TEST SPOT HEADER REC                         
         BNE   XX30                                                             
         OC    4(9,R3),4(R3)       TEST CLIENT HEADER                           
         BNZ   XX4                                                              
* CLIENT HEADER - SAVE IT                                                       
         USING CLTHDR,R3                                                        
*                                                                               
         CLC   2(2,R3),=C'MB'                                                   
         BNE   DMXPURGE                                                         
* CHANGE CLIENT LENGTH TO 1000                                                  
         MVC   CLEN(2),=X'03E8'                                                 
         XC    500(250,R3),500(R3)                                              
         XC    750(250,R3),750(R3)                                              
* ALTER THE CLIENT CODE TOO 2 BYTE PACKED                                       
         MVC   DUB(2),CKEYCLT                                                   
         MVI   DUB+2,X'40'                                                      
         GOTO1 VCLPACK,DMCB,DUB,CKEYCLT                                         
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
XX4      CLI   7(R3),0             TEST ESTIMATE = 0                            
         BE    XX25                PROCESS PRODUCT RECORD                       
         OC    8(5,R3),8(R3)       TEST BILL RECORD                             
         BNZ   DMXKEEP                                                          
*                                                                               
* ESTIMATE HEADER - CONVERT                                                     
*                                                                               
         CLC   2(2,R3),=C'MB'                                                   
         BNE   DMXPURGE                                                         
*                                                                               
         CLC   EKEYSAVE(7),0(R3)   TEST SAME A-M/C/P                            
         CLC   EKEYSAVE(4),0(R3)   TEST SAME A-M/C                              
         BE   *+8                                                               
         BAS   RE,PRTBFR                                                        
* SAVE THE ESTIMATE HEADER                                                      
         USING ESTHDRD,R3                                                       
* ALTER THE CLIENT CODE TOO 2 BYTE PACKED                                       
         MVC   DUB(2),EKEYCLT                                                   
         MVI   DUB+2,X'40'                                                      
         GOTO1 VCLPACK,DMCB,DUB,EKEYCLT                                         
*                                                                               
         LA    R4,14                                                            
         LA    R5,EDEMOS                                                        
         LA    R6,NEWDEMS                                                       
         XC    NEWDEMS,NEWDEMS                                                  
*                                                                               
XX6      CLI   0(R5),0                                                          
         BE    XX20                                                             
         CLI   0(R5),59                                                         
         BL    XX10                                                             
         CLI   0(R5),63                                                         
         BH    XX10                                                             
* USER OR WEIGHTED DEMO                                                         
         MVI   1(R6),X'21'                                                      
         ZIC   RE,0(R5)                                                         
         SH    RE,=H'58'                                                        
         STC   RE,2(R6)            SET USER DEMO NUM                            
         CLI   0(R5),63            TEST WEIGHTED DEMO                           
         BNE   *+10                                                             
         MVC   1(2,R6),=X'3F01'                                                 
         BCTR  RE,0                                                             
         MH    RE,=H'7'            TREAT WTD DEMO AS USER DEMO                  
         LA    RE,NEWDEMS+EUSRNMS-EDEMOS(RE)                                    
         MVC   0(7,RE),2(R5)       MOVE USER DEMO NAME                          
         B     XX12                                                             
*                                                                               
XX10     ZIC   RE,0(R5)                                                         
         BCTR  RE,0                                                             
         AR    RE,RE               X 2                                          
         L     RF,VUSDEMS                                                       
         B     XX11                ON ORDERS FROM MEL                           
*                                                                               
XX11     AR    RF,RE                                                            
         MVC   1(2,R6),0(RF)       MOVE VALUE FROM TABLE                        
*                                                                               
XX12     CLI   1(R5),0             TEST WEIGHT PRESENT                          
         BE    XX14                                                             
         SPACE 2                                                                
* CALCULATE POSITION FOR WEIGHT                                                 
         LA    RE,14                                                            
         SR    RE,R4               GIVES DEMO POSITION (0 RELATIVE)             
         LA    RE,NEWDEMS+EWGTLST-EDEMOS(RE)                                    
         MVC   0(1,RE),1(R5)       MOVE WEIGHT                                  
*                                                                               
XX14     LA    R5,9(R5)                                                         
         LA    R6,3(R6)                                                         
         BCT   R4,XX6                                                           
*                                                                               
XX20     OI    ECNTRL,X'01'        SET CONVERTED IND                            
         MVC   EDEMOS,NEWDEMS                                                   
*                                                                               
         CLC   EKEYSAVE(7),0(R3)   TEST SAME A-M/C/P                            
         CLC   EKEYSAVE(4),0(R3)   TEST SAME A-M/C                              
         BE    *+14                                                             
         BAS   RE,PRTAFT                                                        
         MVC   EKEYSAVE,0(R3)      SAVE KEY OF PRINTED ESTHDR                   
*                                                                               
         B     DMXKEEP                                                          
         SPACE 2                                                                
         EJECT                                                                  
PRTBFR   ST    RE,SVRE                                                          
         ZIC   RE,1(R3)                                                         
         SLL   RE,28                                                            
         SRL   RE,28                                                            
         IC    RE,MDTAB-1(RE)                                                   
         STC   RE,LMED                                                          
*                                                                               
         GOTO1 VCLUNPK,DMCB,2(R3),LCLT                                          
         MVC   LPRD,4(R3)                                                       
*                                                                               
         ZIC   R0,7(R3)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LSTA(3),DUB                                                      
*                                                                               
         MVC   LEST(3),=C'BFR'                                                  
*                                                                               
         LA    R4,EDEMOS                                                        
         LA    R5,14                                                            
         LA    R6,LCOM                                                          
*                                                                               
PRTBFR2  CLI   0(R4),0                                                          
         BE    PRTBFRX                                                          
         ZIC   R0,0(R4)            DEMO NUMBER                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R6),DUB                                                      
         LA    R6,4(R6)                                                         
*                                                                               
         CLI   1(R4),0             TEST WEIGHT                                  
         BE    PRTBFR6                                                          
         IC    R0,1(R4)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         BCTR  R6,0                                                             
         MVC   0(5,R6),=C'(XXX)'                                                
         UNPK  1(3,R6),DUB                                                      
         LA    R6,6(R6)                                                         
*                                                                               
PRTBFR6  MVC   0(7,R6),2(R4)                                                    
         LA    R6,8(R6)                                                         
         LA    R0,P+120                                                         
         CR    R6,R0                                                            
         BL    PRTBFR8                                                          
         GOTO1 VPRINTER                                                         
         LA    R6,LCOM                                                          
PRTBFR8  LA    R4,9(R4)            NEXT DEMO                                    
         BCT   R5,PRTBFR2                                                       
*                                                                               
PRTBFRX  CLC   P,SPACES                                                         
         BE    PRTBFRXX                                                         
         GOTO1 VPRINTER                                                         
PRTBFRXX L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
PRTAFT   ST    RE,SVRE                                                          
         MVC   LEST,=C'AFT'                                                     
         LA    R4,EDEMLST                                                       
         LA    R6,LCOM                                                          
PRTAFT2  CLI   1(R4),0             TEST E-O-L                                   
         BE    PRTAFT20                                                         
         CLI   1(R4),63            TEST WEIGHTED DEMO                           
         BE    PRTAFT6                                                          
         CLI   1(R4),X'21'         TEST USER DEMO                               
         BNE   PRTAFT10                                                         
* PRINT USER DEMO                                                               
         MVC   0(2,R6),=C'U/'                                                   
         ZIC   RE,2(R4)                                                         
         BCTR  RE,0                                                             
         MH    RE,=H'7'                                                         
         LA    RE,EUSRNMS(RE)                                                   
         MVC   2(7,R6),0(RE)                                                    
         LA    R6,10(R6)                                                        
         B     PRTAFT12                                                         
* PRINT WEIGHTED DEMO                                                           
PRTAFT6  MVC   0(2,R6),=C'W/'                                                   
         MVC   2(7,R6),EWGTNM                                                   
         LA    R6,10(R6)                                                        
         B     PRTAFT12                                                         
* PRINT REGULAR DEMO                                                            
PRTAFT10 DS    0H                                                               
         MVC   0(1,R6),1(R4)                                                    
         ZIC   R0,2(R4)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R6),DUB                                                      
         LA    R6,6(R6)                                                         
PRTAFT12 LA    R4,3(R4)            NEXT DEMO                                    
         LA    R0,P+124                                                         
         CR    R6,R0                                                            
         BL    PRTAFT2                                                          
         GOTO1 VPRINTER                                                         
         LA    R6,LCOM                                                          
         B     PRTAFT2                                                          
PRTAFT20 CLC   P,SPACES                                                         
         BE    PRTAFT22                                                         
         GOTO1 VPRINTER                                                         
         EJECT                                                                  
* TEST TO PRINT OUT WEIGHTS *                                                   
*                                                                               
PRTAFT22 OC    EWGTLST,EWGTLST                                                  
         BZ    PRTAFTX                                                          
         LA    R4,EWGTLST                                                       
         LA    R5,1                                                             
         LA    R6,LCOM                                                          
PRTAFT24 CLI   0(R4),0                                                          
         BE    PRTAFT26                                                         
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R6),DUB                                                      
         MVI   2(R6),C'='                                                       
         ZIC   R0,0(R4)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(3,R6),DUB                                                      
         LA    R6,7(R6)                                                         
PRTAFT26 LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         CH    R5,=H'14'                                                        
         BNH   PRTAFT24                                                         
         GOTO1 VPRINTER                                                         
PRTAFTX  L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
*  PROCESS PRODUCT RECORD                                                       
* PRODUCT HEADER - SAVE IT                                                      
XX25     EQU   *                                                                
         USING PRDHDR,R3                                                        
*                                                                               
         CLC   2(2,R3),=C'MB'                                                   
         BNE   DMXPURGE                                                         
* ALTER THE CLIENT CODE TOO 2 BYTE PACKED                                       
         MVC   DUB(2),PKEYCLT                                                   
         MVI   DUB+2,X'40'                                                      
         GOTO1 VCLPACK,DMCB,DUB,PKEYCLT                                         
*                                                                               
         B     DMXKEEP                                                          
         EJECT                                                                  
XX30     CLI   0(R3),X'10'                                                      
         BL    XX70                                                             
* TEST RECORD CAN BE CONVERTED                                                  
         XC    ADDBYTES,ADDBYTES                                                
         MVI   ELCDLO,2            SEARCH FOR DEMO ELEMENTS                     
         MVI   ELCDHI,3                                                         
         LA    R2,24(R3)                                                        
XX32     BAS   RE,NEXTEL                                                        
         BNE   XX34                                                             
         ZIC   RE,1(R2)            GET ELEM LEN                                 
         SH    RE,=H'24'           SUB CONSTANT                                 
         A     RE,ADDBYTES                                                      
         ST    RE,ADDBYTES                                                      
         B     XX32                                                             
*                                                                               
XX34     SR    RE,RE                                                            
         ICM   RE,3,13(R3)                                                      
         A     RE,ADDBYTES                                                      
         SH    RE,=H'1976'         COMPARE TO MAX REC SIZE                      
         BNP   XX60                NO - GO CONVERT                              
*                                                                               
         ST    RE,ADDBYTES         SAVE NUMBER THAT WON'T FIT                   
         BAS   RE,XXFMT                                                         
* CLEAR COMMENT SAVE AREA                                                       
         LA    R0,5                                                             
         L     R1,ASVCMT                                                        
         XC    0(84,R1),0(R1)                                                   
         LA    R1,84(R1)                                                        
         BCT   R0,*-10                                                          
         MVI   COMDELSW,C'N'                                                    
* SAVE ALL THE COMMENTS                                                         
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         L     R1,ASVCMT                                                        
         LA    R2,24(R3)                                                        
XX36     BAS   RE,NEXTEL                                                        
         BNE   XX38                                                             
         MVI   COMDELSW,C'Y'       SET HAVE COMMENTS                            
         ST    R2,0(R1)            SAVE COMMENT ADDRESS                         
         MVC   4(80,R1),0(R2)      AND MOVE COMMENT TO SAVE AREA                
         LA    R1,84(R1)                                                        
         B     XX36                                                             
*                                                                               
XX38     L     R1,ASVCMT5          SET ADDRESS OF LAST COMMENT                  
         LA    R0,5                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
XX40     IC    RF,5(R1)            GET COMMENT LEN                              
         AR    RE,RF               BUMP TOTAL REMOVED                           
         ST    R1,DELADDR          SAVE COMMENT LIST POINTER                    
         C     RE,ADDBYTES         HAVE WE SAVED ENOUGH YET                     
         BH    XX44                YES - DONE                                   
         SH    R1,=H'84'           ELSE BACK UP TO PREVIOUS COMMMENT            
         BCT   R0,XX40             AND TRY AGAIN                                
         EJECT                                                                  
* WILL NOT FIT EVEN IF WE TAKE OUT ALL COMMENTS                                 
* SO LETS TRY TO DELETE SELF CANCELLING SPOT PAIRS                              
*                                                                               
         LR    R7,RE               SAVE BYTES SALVAGED FROM COMMENTS            
         LA    R2,24(R3)                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
XX41     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
XX41B    DS    0H                                                               
         BNE   XX43                NO MORE ELEMS-REC UNCONVERTABLE              
*                                  ORIGINAL ELEM                                
         TM    6(R2),X'40'         MUST BE MINUSED                              
         BZ    XX41                                                             
         TM    6(R2),X'02'         AND NOT MADE GOOD ELSEWHERE                  
         BNZ   XX41                                                             
         MVC   SVPDAT,4(R2)        SAVE PAID DATE                               
*                                                                               
         ZIC   R0,1(R2)                                                         
         LR    R4,R2                                                            
         AR    R4,R0               NEXT ELEM                                    
         CLI   0(R4),X'0C'         MUST BE AN OTO                               
         BNE   XX41                                                             
*                                                                               
         TM    6(R4),X'80'         AND A MINUS SPOT                             
         BZ    XX41                                                             
*                                                                               
         CLC   SVPDAT,4(R4)        OK IF SAME PAID DATE                         
         BE    XX41D                                                            
         CLI   SVPDAT,0            REJECT IF EITHER NOT PAID                    
         BE    XX41                                                             
         CLI   4(R4),0                                                          
         BE    XX41                                                             
*                                                                               
XX41D    DS    0H                                                               
         AR    R7,R0               ADD LENGTH OF ORIG ELEM                      
         MVC   LCOM(12),=C'PAIRED ELEM='                                        
         GOTO1 VHEXOUT,DMCB,(R2),LCOM+12,(R0),=C'N'                             
         GOTO1 VDATCON,DMCB,(2,2(R2)),(5,LCOM+42)                               
         CLI   4(R2),0                                                          
         BE    XX41F                                                            
         MVC   LCOM+54(4),=C'PAID'                                              
         GOTO1 (RF),(R1),(2,4(R2)),(5,LCOM+59)                                  
XX41F    DS    0H                                                               
         MVC   LMSG,=C'PE'                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         ZIC   R0,1(R4)                                                         
         AR    R7,R0               ADD LENGTH OF 2ND ELEM                       
         GOTO1 VHEXOUT,DMCB,(R4),LCOM+12,(R0),=C'N'                             
         GOTO1 VDATCON,DMCB,(2,2(R2)),(5,LCOM+42)                               
         MVC   LCOM+50(3),=C'(-)'                                               
         CLI   4(R4),0                                                          
         BE    XX41H                                                            
         MVC   LCOM+54(4),=C'PAID'                                              
         GOTO1 (RF),(R1),(2,4(R4)),(5,LCOM+59)                                  
XX41H    DS    0H                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(R3),(R2)    DELETE ORIG ELEM                        
         GOTO1 (RF),(R1)                AND SECOND                              
*                                                                               
         C     R7,ADDBYTES         HAVE WE SAVED ENOUGH                         
         BH    XX44                YES - DONE                                   
*                                  NO - GET ANOTHER ELEM                        
XX42     DS    0H                                                               
         BAS   RE,NEXTEL2          R2 LEFT POINTING AT NEXT ELEM                
         B     XX41B                                                            
*                                                                               
XX43     DS    0H                                                               
*        STILL CANNOT CONVERT - DELETE SPOT ELEMS FROM END                      
*                                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
XX43B    DS    0H                                                               
         XC    SAVELAD,SAVELAD                                                  
         LA    R2,24(R3)                                                        
XX43D    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   XX43F                                                            
*                                                                               
         ST    R2,SAVELAD                                                       
         B     XX43D                                                            
*                                                                               
XX43F    DS    0H                                                               
         L     R2,SAVELAD                                                       
         LTR   R2,R2                                                            
         BZ    XX43N               NO SPOT ELEMS - CANNOT CONVERT               
*                                                                               
         MVC   LCOM(12),=C'DELETD ELEM='                                        
         ZIC   R0,1(R2)                                                         
         AR    R7,R0               ADD TO BYTE COUNTER                          
         GOTO1 VHEXOUT,DMCB,(R2),LCOM+12,(R0),=C'N'                             
         GOTO1 VDATCON,DMCB,(2,2(R2)),(5,LCOM+42)                               
         CLI   4(R2),0                                                          
         BE    XX43H                                                            
         MVC   LCOM+54(4),=C'PAID'                                              
         GOTO1 (RF),(R1),(2,4(R2)),(5,LCOM+59)                                  
XX43H    DS    0H                                                               
         MVC   LMSG,=C'DE'                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(R3),(R2)                                            
         C     R7,ADDBYTES         TEST HAVE DONE ENOUGH                        
         BNH   XX43B               NO - TRY AGAIN                               
         L     RF,APARAMC                                                       
         CLI   0(RF),C'D'                                                       
         BNE   XX44                                                             
         DC    H'0'                                                             
*                                                                               
XX43N    DS    0H                                                               
*        GIVE UP ON THIS RECORD                                                 
*                                                                               
         BAS   RE,XXFMT                                                         
         MVC   LCOM(25),=C'** CANNOT BE CONVERTED **'                           
         MVC   LMSG,=C'**'                                                      
* DISPLAY BUY START DATE                                                        
         USING BUYRECD,R3                                                       
         GOTO1 VDATCON,DMCB,(3,BDSTART),(5,LCOM+28)                             
         DROP  R3                                                               
* DISPLAY NEEDED BYTES                                                          
         L     RE,ADDBYTES                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LCOM+37(4),DUB                                                   
* DISPLAY REC LEN                                                               
         ICM   RE,3,13(R3)                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LCOM+42(4),DUB                                                   
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         SPACE 2                                                                
* REC CAN BE CONVERTED IF SOME COMMENTS DELETED                                 
* SO PRINT THEM OUT AND THEN DELETE THEM                                        
*                                                                               
XX44     DS    0H                                                               
         CLI   COMDELSW,C'Y'                                                    
         BNE   XX60                NO COMMENTS TO DELETE                        
         BAS   RE,XXFMT            FORMAT BUY DATA                              
         L     R5,DELADDR                                                       
XX45     ZIC   RE,5(R5)            GET COMMENT LEN                              
         SH    RE,=H'4'                                                         
         BM    XX45B                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LCOM(0),7(R5) *EXECUTED*                                         
         MVC   LMSG,=C'CC'                                                      
         GOTO1 VPRINTER                                                         
XX45B    DS    0H                                                               
         LA    R5,84(R5)           POINT TO NEXT COMMENT                        
         C     R5,ASVCMTX          TEST AT END OF LIST                          
         BNH   XX45                                                             
* NOW DELETE COMMENTS FROM BUYREC (START AT LAST)                               
         L     R5,ASVCMT5          GET ADDRESS OF LAST COMMENT                  
XX46     ICM   R4,15,0(R5)         GET ADDRESS OF COMMENT ELEMENT               
         BZ    XX48                                                             
         GOTO1 VRECUP,DMCB,(R3),(R4)                                            
XX48     SH    R5,=H'84'                                                        
         C     R5,DELADDR          HAVE WE DELETED ENOUGH                       
         BL    XX60                THEN GO CONVERT                              
         B     XX46                                                             
         EJECT                                                                  
XXFMT    NTR1                                                                   
         MVC   LAGY,20(R3)         ALPHA AGY                                    
         ZIC   RE,0(R3)            GET AGY/MED                                  
         SLL   RE,28                                                            
         SRL   RE,28               DROP AGY                                     
         IC    RE,MDTAB-1(RE)                                                   
         STC   RE,LMED                                                          
*                                                                               
         GOTO1 VCLUNPK,DMCB,1(R3),LCLT                                          
*                                                                               
         MVC   LPRD,=C'POL'                                                     
         CLI   3(R3),X'FF'                                                      
         BE    XXFMT2                                                           
         ZIC   R0,3(R3)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LPRD,DUB                                                         
*                                                                               
XXFMT2   DS    0H                                                               
         GOTO1 VMSUNPK,DMCB,4(R3),WORK,LSTA                                     
*                                                                               
         ZIC   R0,9(R3)            EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST,DUB                                                         
*                                                                               
         ZIC   R0,10(R3)           LINE                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LLIN,DUB                                                         
         B     DMXIT                                                            
*                                                                               
MDTAB    DC    C'TRNX'                                                          
         EJECT                                                                  
* CONVERT BUY RECORD                                                            
*                                                                               
XX60     DS    0H                                                               
*                                                                               
         CLC   1(2,R3),=C'MB'                                                   
         BNE   DMXPURGE                                                         
*                                                                               
* ALTER THE CLIENT CODE TOO 2 BYTE PACKED                                       
         MVC   DUB(2),1(R3)                                                     
         MVI   DUB+2,X'40'                                                      
         GOTO1 VCLPACK,DMCB,DUB,1(R3)                                           
*                                                                               
         LA    R2,24(R3)                                                        
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,3                                                         
*                                                                               
XX62     BAS   RE,NEXTEL                                                        
         BNE   DMXKEEP                                                          
         XC    NEWDEMS,NEWDEMS                                                  
         MVC   NEWDEMS(24),0(R2)   MOVE START OF ELEMENT                        
         ZIC   R0,1(R2)            GET OLD ELEM LEN                             
         SH    R0,=H'24'                                                        
         BZ    XX62                                                             
         SRL   R0,2                SET FOR BCT                                  
         LA    R4,NEWDEMS+24       NEW DEMO EL                                  
         LA    R5,24(R2)           OLD DEMO EL                                  
XX64     BAS   RE,GETDEM                                                        
         LA    R4,8(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,XX64                                                          
* CALCULATE LENGTH OF NEW DEMO ELEMENT                                          
         LA    R0,NEWDEMS                                                       
         SR    R4,R0                                                            
         STC   R4,NEWDEMS+1                                                     
*                                                                               
         CLC   BKEYSAVE,0(R3)      TEST TO PRINT BUY                            
         BE    XX66                                                             
         BAS   RE,PRTBUY                                                        
         MVC   BKEYSAVE,0(R3)                                                   
*                                                                               
XX66     GOTO1 VRECUP,DMCB,(R3),(R2)   DELETE OLD ELEM                          
         GOTO1 (RF),(R1),,NEWDEMS,(R2) ADD NEW                                  
         B     XX62                                                             
         EJECT                                                                  
PRTBUY   ST    RE,SVRE                                                          
         BAS   RE,XXFMT                                                         
*                                                                               
         LR    R4,R2               PRINT OLD DEMO ELEM FIRST                    
         ZIC   R5,1(R4)                                                         
         MVC   LCOM(3),=C'BFR'                                                  
*                                                                               
PRTBUY2  LA    R0,16                                                            
         CR    R5,R0                                                            
         BH    *+6                                                              
         LR    R0,R5                                                            
         GOTO1 VHEXOUT,DMCB,(R4),LCOM+4,(R0),=C'TOG'                            
*                                                                               
         LA    R4,16(R4)                                                        
         SR    R5,R0                                                            
         BM    PRTBUY4                                                          
         LA    R0,16                                                            
         CR    R5,R0                                                            
         BH    *+6                                                              
         LR    R0,R5                                                            
         GOTO1 VHEXOUT,DMCB,(R4),LCOM+38,(R0),=C'TOG'                           
*                                                                               
PRTBUY4  GOTO1 VPRINTER                                                         
         LA    R4,16(R4)                                                        
         SR    R5,R0                                                            
         BP    PRTBUY2                                                          
         EJECT                                                                  
         LA    R4,NEWDEMS          PRINT NEW DEMO ELEM                          
         ZIC   R5,1(R4)                                                         
         MVC   LCOM(3),=C'AFT'                                                  
*                                                                               
PRTBUY12 LA    R0,16                                                            
         CR    R5,R0                                                            
         BH    *+6                                                              
         LR    R0,R5                                                            
         GOTO1 VHEXOUT,DMCB,(R4),LCOM+4,(R0),=C'TOG'                            
*                                                                               
         LA    R4,16(R4)                                                        
         SR    R5,R0                                                            
         BM    PRTBUY14                                                         
         LA    R0,16                                                            
         CR    R5,R0                                                            
         BH    *+6                                                              
         LR    R0,R5                                                            
         GOTO1 VHEXOUT,DMCB,(R4),LCOM+38,(R0),=C'TOG'                           
*                                                                               
PRTBUY14 GOTO1 VPRINTER                                                         
         LA    R4,16(R4)                                                        
         SR    R5,R0                                                            
         BP    PRTBUY12                                                         
*                                                                               
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
GETDEM   MVC   DUB(1),0(R5)                                                     
         NI    DUB,X'7F'           DROP OVRD IND                                
         CLI   DUB,59                                                           
         BL    GETDEM1                                                          
         CLI   DUB,63                                                           
         BNH   GETDEM10            GO PROCESS USR/WGTD DEMO                     
*                                                                               
GETDEM1  L     RF,VUSDEMS                                                       
         ZIC   R1,0(R3)            GET AGY/MED                                  
         SRL   R1,4                                                             
         BCTR  R1,0                                                             
         A     R1,ACNTAB                                                        
         CLI   0(R1),0             TEST CANADIAN AGY                            
         BE    GETDEM4             NO                                           
* MUST CHECK US CLIENT OF CANADIAN AGENCY                                       
         L     RF,VCANDEMS                                                      
         L     R1,ACLTPTR                                                       
GETDEM2  CLI   0(R1),0             TEST E-O-L                                   
         BE    GETDEM6                                                          
         CLC   0(3,R3),0(R1)                                                    
         BL    GETDEM6                                                          
         BE    GETDEM4                                                          
         LA    R1,3(R1)                                                         
         ST    R1,ACLTPTR                                                       
         B     GETDEM2                                                          
GETDEM4  L     RF,VUSDEMS                                                       
*                                                                               
GETDEM6  SR    R1,R1                                                            
         IC    R1,0(R5)                                                         
         SLL   R1,25                                                            
         SRL   R1,25               DROP X'80' BIT                               
         BCTR  R1,0                                                             
         AR    R1,R1               X 2                                          
         AR    RF,R1                                                            
GETDEM8  MVC   1(2,R4),0(RF)       MOVE TYPE/NUM                                
         MVC   3(1,R4),3(R5)       MOVE HUT                                     
         MVC   6(2,R4),1(R5)       MOVE VALUE                                   
         NI    6(R4),X'7F'         DROP OVERRIDE BIT                            
         TM    0(R5),X'80'         TEST OVRD IN OLD ELEM                        
         BZ    *+8                 NO                                           
         OI    4(R4),X'80'         SET IN NEW ELEM                              
         BR    RE                  RETURN                                       
*                                                                               
GETDEM10 ZIC   RF,0(R5)            GET USER DEMO NUM                            
         SH    RF,=H'58'                                                        
         MVI   DUB+1,X'21'                                                      
         STC   RF,DUB+2                                                         
         CLI   0(R5),63            TEST WGTD DEMO                               
         BNE   *+10                                                             
         MVC   DUB+1(2),=X'3F01'                                                
         LA    RF,DUB+1            POINT TO DEMO                                
         B     GETDEM8             AND RESUME PROCESSING                        
         EJECT                                                                  
XX70     CLC   =X'0D14',0(R3)      TEST CANAD NTWK DEMO OVRD                    
         BNE   XX80                                                             
         B     DMXKEEP             DONT PROCESS FOR THIS RUN                    
         CLC   =C'CTV XXX',3(R3)   TEST BRUCE'S TEST RECORD                     
         BE    DMXKEEP             YES - IGNORE                                 
         MVC   LAGY+20(3),=C'BFR'                                               
         BAS   RE,PRTDEM                                                        
*                                                                               
         LA    R2,24(R3)                                                        
         CLI   0(R2),1             TEST 01 EL                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    NEWDEMS,NEWDEMS                                                  
         MVC   NEWDEMS(12),0(R2)                                                
         ZIC   R0,NEWDEMS+1        GET OLD ELEM LEN                             
         SH    R0,=H'12'           SUB CONSTANT                                 
         BNP   XX71X                                                            
         LR    RE,R0               SAVE BCT VALUE                               
         MH    R0,=H'3'                                                         
         AH    R0,=H'12'                                                        
         STC   R0,NEWDEMS+1                                                     
*                                                                               
         LR    R0,RE               SET FOR BCT                                  
         LA    R4,NEWDEMS+12                                                    
         LA    R5,12(R2)                                                        
*                                                                               
XX71A    ZIC   RF,0(R5)                                                         
         N     RF,=X'0000007F'     DROP HOB                                     
         BCTR  RF,0                                                             
         AR    RF,RF               X 2                                          
         A     RF,VCANDEMS                                                      
         MVC   1(2,R4),0(RF)       MOVE DEMO                                    
         TM    0(R5),X'80'         TEST 'INPUT' IND                             
         BZ    *+8                 NO                                           
         OI    0(R4),X'80'         SET IN NEW ELEM TOO                          
         LA    R4,3(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,XX71A                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(R3),(R2)                                            
         GOTO1 (RF),(R1),,NEWDEMS,(R2)                                          
         SPACE 1                                                                
* PROCESS 02 ELEM                                                               
         SPACE 1                                                                
XX71X    MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R2,24(R3)                                                        
         BAS   RE,NEXTEL                                                        
         BNE   XX74X                                                            
*                                                                               
         XC    NEWDEMS,NEWDEMS                                                  
         MVC   NEWDEMS(2),0(R2)                                                 
         ZIC   R0,1(R2)                                                         
         SH    R0,=H'2'                                                         
         BNP   XX74X                                                            
         SRL   R0,2                DIVIDE BY 4                                  
         LR    RE,R0               SAVE BCT REG                                 
         MH    R0,=H'5'            X 5                                          
         AH    R0,=H'2'                                                         
         STC   R0,NEWDEMS+1        SET NEW ELEM LENGTH                          
         LR    R0,RE               RESTORE BCT REG                              
         LA    R4,NEWDEMS+2                                                     
         LA    R5,2(R2)                                                         
*                                                                               
XX72     SR    RF,RF                                                            
         IC    RF,0(R5)                                                         
         BCTR  RF,0                                                             
         AR    RF,RF                                                            
         A     RF,VCANDEMS                                                      
         MVC   1(2,R4),0(RF)       MOVE DEMO                                    
         MVC   3(2,R4),1(R5)       MOVE VALUE                                   
         LA    R4,5(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,XX72                                                          
*                                                                               
XX74     GOTO1 VRECUP,DMCB,(R3),(R2)                                            
         GOTO1 (RF),(R1),,NEWDEMS,(R2)                                          
*                                                                               
XX74X    MVC   LAGY+20(3),=C'AFT'                                               
         BAS   RE,PRTDEM                                                        
         B     DMXKEEP                                                          
         EJECT                                                                  
         DS    F                                                                
PRTDEM   ST    RE,*-4                                                           
         MVC   LAGY,20(R3)                                                      
         MVC   LAGY+4(4),3(R3)     NTWK                                         
         MVC   LAGY+10(4),7(R3)    PGM                                          
         MVC   LAGY+16(1),11(R3)   RTG SVC                                      
*                                                                               
         LA    R2,24(R3)                                                        
         BAS   RE,PRTEL            PRINT 01 ELEM                                
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    PRTDEMX                                                          
         BAS   RE,PRTEL            PRINT 02 ELEM                                
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    PRTDEMX                                                          
         BAS   RE,PRTEL            PRINT 05 ELEM                                
*                                                                               
PRTDEMX  L     RE,PRTDEM-4                                                      
         BR    RE                                                               
         SPACE 2                                                                
PRTEL    NTR1                                                                   
         ZIC   R5,1(R2)                                                         
         LA    R0,40               80 CHARS PER LINE                            
PRTEL2   CR    R5,R0                                                            
         BH    *+6                                                              
         LR    R0,R5                                                            
         GOTO1 VHEXOUT,DMCB,(R2),LCOM,(R0),=C'TOG'                              
         GOTO1 VPRINTER                                                         
         AR    R2,R0                                                            
         SR    R5,R0                                                            
         BP    PRTEL2                                                           
         XIT1                                                                   
         EJECT                                                                  
XX80     CLC   =X'0D15',0(R3)      TEST PEPSI RECORD                            
         BNE   XX90                                                             
         B     DMXKEEP                                                          
         CLC   EKEYSAVE(4),0(R3)                                                
         BE    XX81                                                             
         GOTO1 VHEXOUT,DMCB,(R3),LAGY,13,=C'TOG'                                
         MVC   LLIN+4,=C'BFR'                                                   
         BAS   RE,PRTPEP                                                        
*                                                                               
XX81     DS    0H                                                               
         MVI   ELCDLO,X'10'                                                     
         MVI   ELCDHI,X'10'                                                     
         LA    R2,24(R3)                                                        
         BAS   RE,NEXTEL2          DON'T SKIP FIRST ELEMENT                     
         B     *+8                                                              
*                                                                               
XX86     BAS   RE,NEXTEL                                                        
         BNE   XX89                                                             
         XC    NEWDEMS,NEWDEMS                                                  
         MVC   NEWDEMS(12),0(R2)                                                
         ZIC   R0,NEWDEMS+1                                                     
         SH    R0,=H'12'                                                        
         SRL   R0,2                SET FOR BCT                                  
         LR    RE,R0                                                            
         MH    RE,=H'5'                                                         
         LA    RE,12(RE)            ADD BACK CONSTANT                           
         STC   RE,NEWDEMS+1        SET NEW ELEM LEN                             
         LA    R4,NEWDEMS+12                                                    
         LA    R5,12(R2)                                                        
*                                                                               
XX88     SR    RF,RF                                                            
         IC    RF,0(R5)                                                         
         BCTR  RF,0                                                             
         AR    RF,RF                                                            
         A     RF,VUSDEMS                                                       
         MVC   0(2,R4),0(RF)       MOVE NEW DEMO CODE                           
         MVC   2(3,R4),1(R5)       MOVE VALUE                                   
         LA    R4,5(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,XX88                                                          
*                                                                               
         GOTO1 VRECUP,DMCB,(R3),(R2)                                            
         GOTO1 (RF),(R1),,NEWDEMS,(R2)                                          
         B     XX86                                                             
*                                                                               
XX89     DS    0H                                                               
         CLC   EKEYSAVE(4),0(R3)                                                
         BE    DMXKEEP                                                          
         MVC   LLIN+4,=C'AFT'                                                   
         BAS   RE,PRTPEP                                                        
         MVC   EKEYSAVE,0(R3)                                                   
         B     DMXKEEP                                                          
         EJECT                                                                  
         DS    F                                                                
PRTPEP   ST    RE,*-4                                                           
         MVI   ELCDLO,X'10'                                                     
         MVI   ELCDHI,X'10'                                                     
         LA    R2,24(R3)                                                        
         BAS   RE,NEXTEL2                                                       
         BNE   PRTPEPX                                                          
         BAS   RE,PRTEL                                                         
         BAS   RE,NEXTEL                                                        
         BNE   PRTPEPX                                                          
         BAS   RE,PRTEL                                                         
PRTPEPX  L     RE,PRTPEP-4                                                      
         BR    RE                                                               
         EJECT                                                                  
XX90     CLI   0(R3),2             TEST GOAL REC                                
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   2(2,R3),=C'MB'                                                   
         BNE   DMXPURGE                                                         
*                                                                               
* ALTER THE CLIENT CODE TOO 2 BYTE PACKED                                       
         MVC   DUB(2),2(R3)                                                     
         MVI   DUB+2,X'40'                                                      
         GOTO1 VCLPACK,DMCB,DUB,2(R3)                                           
*                                                                               
* SEARCH FOR LOCK-IN ELEMENTS                                                   
         MVI   ELCDLO,X'30'                                                     
         MVI   ELCDHI,X'31'                                                     
         LA    R2,24(R3)                                                        
         SR    R5,R5               CLEAR COUNTER                                
XX91A    BAS   RE,NEXTEL                                                        
         BNE   XX91B                                                            
         BCTR  R5,0                BUMP COUNTER                                 
         B     XX91A                                                            
*                                                                               
XX91B    LPR   R5,R5                                                            
         BZ    DMXKEEP                                                          
* TEST TO PRINT                                                                 
         CLC   EKEYSAVE(4),0(R3)   02/A-M/CLT                                   
         BE    XX91C                                                            
         MVC   LCOM-4(3),=C'BFR'                                                
         BAS   RE,GLFMT                                                         
         BAS   RE,PRTGL                                                         
*                                                                               
XX91C    MH    R5,=H'3'            ADD 3 BYTES/ELEM                             
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)         GET REC LEN                                  
         AR    R5,R0                                                            
         CH    R5,=H'1976'         WILL IT FIT                                  
         BH    XX99                NO                                           
         SPACE 1                                                                
* CONVERT RECORD *                                                              
         SPACE 1                                                                
         LA    R2,24(R3)                                                        
XX92     BAS   RE,NEXTEL                                                        
         BNE   XX98                                                             
         XC    NEWDEMS(20),NEWDEMS                                              
         MVC   NEWDEMS(12),0(R2)                                                
         MVI   NEWDEMS+1,19        SET NEW ELEM LEN                             
         MVC   DUB(1),12(R2)       MOVE DEMO CODE TO WORK                       
         NI    DUB,X'7F'           DROP OVRD IND                                
         CLI   DUB,59              TEST FOR USER                                
         BL    XX93                                                             
         CLI   DUB,63              OR WEIGHTED DEMO                             
         BNH   XX96                                                             
*                                                                               
XX93     L     RF,VUSDEMS                                                       
         ZIC   R1,1(R3)            AGY-MED                                      
         SRL   R1,4                                                             
         BCTR  R1,0                                                             
         A     R1,ACNTAB                                                        
         CLI   0(R1),0             TEST CANAD AGY                               
         BE    XX94                                                             
* CHECK FOR US CLIENT                                                           
         L     RF,VCANDEMS                                                      
         L     R1,ACLTTAB          ** START AT TOP OF TABLE **                  
XX93A    CLI   0(R1),0             TEST E-O-L                                   
         BE    XX94                                                             
         CLC   0(3,R3),0(R1)       SAME CLT                                     
         BL    XX94                                                             
         BE    XX93B                                                            
         LA    R1,3(R1)                                                         
         B     XX93A                                                            
*                                                                               
XX93B    L     RF,VUSDEMS                                                       
*                                                                               
XX94     SR    R1,R1                                                            
         IC    R1,12(R2)                                                        
         SLL   R1,25               DROP OVRD BIT                                
         SRL   R1,25                                                            
         BCTR  R1,0                                                             
         AR    R1,R1               X 2                                          
         AR    RF,R1               POINT TO TABLE ENTRY                         
*                                                                               
XX95     MVC   NEWDEMS+13(2),0(RF)  MOVE TYPE/NUM                               
*                                                                               
XX95A    MVC   NEWDEMS+16(3),13(R2)                                             
         TM    12(R2),X'80'                                                     
         BZ    *+8                                                              
         OI    NEWDEMS+15,X'80'    SET OVRD IND                                 
         GOTO1 VRECUP,DMCB,(R3),(R2)                                            
         GOTO1 (RF),(R1),,NEWDEMS,(R2)                                          
         B     XX98                                                             
*                                                                               
XX96     ZIC   RF,DUB              USER DEMO NUM                                
         SH    RF,=H'58'                                                        
         MVI   DUB+1,X'21'                                                      
         STC   RF,DUB+2                                                         
         CLI   DUB,63                                                           
         BNE   *+10                                                             
         MVC   DUB+1(2),=X'3F01'                                                
         LA    RF,DUB+1                                                         
         B     XX95                                                             
*                                                                               
XX98     BAS   RE,NEXTEL                                                        
         BNE   XX98A                                                            
         MVC   NEWDEMS(12),0(R2)   MOVE ELEM DETAILS                            
         MVI   NEWDEMS+1,19        SET NEW LENGTH                               
         MVI   NEWDEMS+15,0        RESET OVRD IND                               
         B     XX95A                                                            
*                                                                               
XX98A    CLC   EKEYSAVE(4),0(R3)   02/A-M/CLT                                   
         BE    *+14                                                             
         MVC   LCOM-4(3),=C'AFT'                                                
         BAS   RE,PRTGL                                                         
         MVC   EKEYSAVE,0(R3)                                                   
         B     DMXKEEP                                                          
         EJECT                                                                  
* GOAL RECORD CANNOT BE CONVERTED *                                             
         SPACE 1                                                                
XX99     DS    0H                                                               
         BAS    RE,GLFMT                                                        
* PRINT DATE OF FIRST 30/31 ELEM                                                
         MVI   ELCDLO,X'30'                                                     
         MVI   ELCDHI,X'31'                                                     
         LA    R2,24(R3)                                                        
         BAS   RE,NEXTEL                                                        
         GOTO1 VDATCON,DMCB,(2,2(R2)),(5,LCOM+28)                               
* DISPLAY NEEDED BYTES                                                          
         SH    R5,=H'1976'                                                      
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LCOM+35(4),DUB                                                   
*                                                                               
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
         DS    A                                                                
GLFMT    ST    RE,*-4                                                           
         MVC   LAGY,20(R3)         ALPHA AGY                                    
         ZIC   RE,1(R3)            GET AGY/MED                                  
         SLL   RE,28                                                            
         SRL   RE,28               DROP AGY                                     
         IC    RE,MDTAB-1(RE)                                                   
         STC   RE,LMED                                                          
*                                                                               
         GOTO1 VCLUNPK,DMCB,2(R3),LCLT                                          
*                                                                               
         MVC   LPRD,=C'POL'                                                     
         CLI   4(R3),X'FF'                                                      
         BE    GLFMT2                                                           
         ZIC   R0,4(R3)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LPRD,DUB                                                         
*                                                                               
GLFMT2   SR    R0,R0                                                            
         ICM   R0,3,5(R3)          MKT                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LSTA(4),DUB                                                      
*                                                                               
         MVC   LEST(1),8(R3)       DPT                                          
         ZIC   R0,9(R3)            SLN                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST+5(2),DUB                                                    
*                                                                               
         ZIC   R0,7(R3)            EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST+1(3),DUB                                                    
*                                                                               
         L     RE,GLFMT-4                                                       
         BR    RE                                                               
         SPACE 2                                                                
         DS    A                                                                
PRTGL    ST    RE,*-4                                                           
         BAS   RE,GLFMT                                                         
         LA    R2,24(R3)                                                        
         BAS   RE,NEXTEL           SRCH FOR 30/31 IS SET UP                     
         BNE   *+8                                                              
         BAS   RE,PRTEL                                                         
         L     RE,PRTGL-4                                                       
         BR    RE                                                               
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
NEXTEL2  CLI   0(R2),0                                                          
         BE    NEXTELX                                                          
         CLC   0(1,R2),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R2),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE               SET CC EQUAL                                 
         BR    RE                                                               
NEXTELX  LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
         SPACE 2                                                                
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         DS    0D                                                               
         EJECT                                                                  
*******************************************************************             
ADCONS   DS    0F                  **** FIRST RELOCATABLE ADCON ****            
ACLTPTR  DC    A(CLTTAB)                                                        
ACLTTAB  DC    A(CLTTAB)                                                        
ACLTTABX DC    A(CLTTABX)                                                       
VUSDEMS  DC    V(SPDEMTAB)                                                      
VCANDEMS DC    V(CNDEMTAB)                                                      
VRECUP   DC    V(RECUP)                                                         
VMSUNPK  DC    V(MSUNPK)                                                        
VCLUNPK  DC    V(CLUNPK)                                                        
VCLPACK  DC    V(CLPACK)                                                        
VDATCON  DC    V(DATCON)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
ASVCMT   DC    A(SVCMT)                                                         
ASVCMTX  DC    A(SVCMTX)                                                        
ASVCMT5  DC    A(SVCMT5)                                                        
ADCONX   EQU   *-1                 **** LAST RELOCATABLE ADCON ****             
*******************************************************************             
ACNTAB   DC    A(0)                                                             
DELADDR  DS    A                                                                
NEWDEMS  DS    CL256                                                            
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*CLTTAB*'                                                      
CLTTAB   DS    128D                                                             
CLTTABX  EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    C'**SVCMT*'                                                      
SVCMT    DS    0D                                                               
SVCMT1   DS    A,CL80                                                           
SVCMT2   DS    A,CL80                                                           
SVCMT3   DS    A,CL80                                                           
SVCMT4   DS    A,CL80                                                           
SVCMT5   DS    A,CL80                                                           
SVCMTX   EQU   *-1                                                              
*                                                                               
SVRE     DS    F                                                                
ADDBYTES DS    F                                                                
SAVBYTES DS    F                                                                
ELCDLO   DS    C                                                                
ELCDHI   DS    C                                                                
COMDELSW DS    C                                                                
SVPDAT   DS    XL2                                                              
EKEYSAVE DS    CL7                 A-M/C/P (ESTHDR)                             
BKEYSAVE DS    CL4                 A-M/C/P (BUYREC)                             
SAVELAD  DS    F                                                                
         EJECT                                                                  
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
WORK     DS    CL24                                                             
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
*SPGENCLT                                                                       
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
*SPGENPRD                                                                       
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
*SPGENEST                                                                       
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
DEMOVD   DSECT                                                                  
*SPGENDOV                                                                       
       ++INCLUDE SPGENDOV                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
*SPGENBUY                                                                       
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
GLRECD   DSECT                                                                  
*SPGENGOAL                                                                      
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         ORG   P                                                                
*                                                                               
LAGY     DS    CL2                                                              
         DS    CL2                                                              
LMED     DS    CL1                                                              
         DS    CL1                                                              
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LSTA     DS    CL4                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LLIN     DS    CL3                                                              
         DS    CL1                                                              
         DS    CL5                                                              
LCOM     DS    CL80                                                             
LMSG     DS    CL2                                                              
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020NSSLDEXT  05/01/02'                                      
         END                                                                    
