*          DATA SET SPLFM1A    AT LEVEL 021 AS OF 05/12/04                      
*PHASE T2191AA                                                                  
         TITLE 'SPLFM1A - POL SUB-EST ADD/CHANGE - T2191A'                      
         PRINT NOGEN                                                            
T2191A   CSECT                                                                  
         NMOD1 0,T2191A                                                         
         L     RC,0(R1)                                                         
         LA    R9,2048(RC)                                                      
         LA    R9,2048(R9)                                                      
         USING GENOLD,RC,R9                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING ESTHDRD,R8                                                       
*                                                                               
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
LFMERR   GOTO1 ERROR               (DOES NOT RETURN HERE)                       
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVACT,C'A'                                                       
         BNE   FMT00                                                            
         MVI   ERRCD,BADMSTR1      ALREADY A MASTER EST                         
         CLI   EMSTRIND,C'M'                                                    
         BE    LFMERR                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING MASRECD,R6                                                       
         MVI   MASKTYPE,MASKTYPQ   X'0D'                                        
         MVI   MASKSTYP,MASKSTPQ   X'6F'                                        
         MVC   MASKAM,EKEYAM       A/M                                          
         MVC   MASKCLT,EKEYCLT                                                  
         MVC   MASKDATE,ESTART                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     MASTER REC EXISTS?                           
         BNE   ERRMASTR            NO...ERROR                                   
         MVC   KEY,SVKEY           READ MASTER EST                              
         GOTO1 GETREC                                                           
*                                                                               
FMT00    MVI   ERRCD,BADMSTR2      IS IT A SUB-EST                              
         CLI   EMSTRIND,C'S'                                                    
         BE    LFMERR                                                           
*                                                                               
FMT1     GOTO1 VDATCON,DMCB,ESTART,(5,WORK)                                     
         GOTO1 (RF),(R1),EEND,(5,WORK+9)                                        
         MVI   WORK+8,C'-'                                                      
         LA    R2,SUBMDTSH                                                      
         MVC   37(17,R2),WORK                                                   
         FOUT  (R2)                                                             
*                                                                               
         CLI   SVACT,C'A'                                                       
         BNE   *+12                                                             
         CLI   SVFMTSW,0           TEST FMT BEFORE ADD                          
         BE    EXIT                                                             
*                                                                               
FMT2     LA    R2,SUBEST1H         POINT TO FIRST UNP FIELD                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),SVKEY        A-M/CLT/PRD                                  
         GOTO1 HIGH                                                             
         B     FMT6                                                             
*                                                                               
FMT4     GOTO1 SEQ                                                              
*                                                                               
FMT6     CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   FMTX                                                             
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   FMT4                                                             
* ESTHDR                                                                        
         CLC   KEY(13),SVKEY       IS IT US                                     
         BE    FMT4                YES - SKIP                                   
         GOTO1 GETREC                                                           
         CLI   EMSTRIND,C'S'       TEST SUB-EST                                 
         BNE   FMT4                                                             
         CLC   EMSTREST,SVKEY+7    RIGHT MASTER                                 
         BNE   FMT4                                                             
         EJECT                                                                  
         ZIC   R0,EKEY+7                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(3,R2),DUB                                                      
         OI    4(R2),X'20'         SET VALID                                    
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(20,R2),EDESC                                                   
         OI    4(R2),X'20'                                                      
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         GOTO1 VDATCON,DMCB,ESTART,(5,8(R2))                                    
         OI    4(R2),X'20'                                                      
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         GOTO1 VDATCON,DMCB,EEND,(5,8(R2))                                      
         OI    4(R2),X'20'                                                      
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(6,R2),=C'LATEST'                                               
         OC    EBOOK,EBOOK                                                      
         BZ    FMT10                                                            
         GOTO1 VDATCON,DMCB,(3,EBOOK),(6,8(R2))                                 
FMT10    OI    4(R2),X'20'                                                      
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(4,R2),=C'AUTO'                                                 
         CLI   EHUTADJ,0                                                        
         BE    FMT12                                                            
         XC    8(4,R2),8(R2)                                                    
         XC    DUB,DUB                                                          
         ZIC   R0,EHUTADJ                                                       
         SRL   R0,4                                                             
         STC   R0,DUB+1                                                         
         GOTO1 VDATCON,DMCB,(3,DUB),(4,WORK)                                    
         MVC   8(3,R2),WORK                                                     
*                                                                               
FMT12    OI    4(R2),X'20'                                                      
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   0(R2),9                                                          
         BE    EXIT                                                             
         B     FMT4                                                             
         EJECT                                                                  
* CLEAR ALL REMAINING UNP FIELDS                                                
*                                                                               
FMTX     ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,FMTOC                                                         
         BE    FMTX2                                                            
         EX    RE,FMTXC                                                         
         FOUT  (R2)                                                             
FMTX2    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BNE   FMTX                                                             
         B     EXIT                                                             
*                                                                               
FMTOC    OC    8(0,R2),8(R2)                                                    
FMTXC    XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
EDT      DS    0H                                                               
         MVC   KEY,SVKEY           READ MASTER EST                              
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVACT,C'A'          TEST ADD                                     
         BNE   EDT2                NO                                           
         MVI   ERRCD,BADMSTR1                                                   
         CLI   EMSTRIND,C'M'                                                    
         BE    LFMERR                                                           
         MVI   ERRCD,BADMSTR2                                                   
         CLI   EMSTRIND,C'S'                                                    
         BE    LFMERR                                                           
*                                                                               
EDT2     LA    R7,SUBLIST                                                       
         LA    R0,12                                                            
         XC    0(48,R7),0(R7)                                                   
         LA    R7,48(R7)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         LA    R7,SUBLIST                                                       
         USING SUBLISTD,R7                                                      
*                                                                               
         LA    R2,SUBEST1H                                                      
EDT4     DS    0H                                                               
         GOTO1 ANY                                                              
         GOTO1 PACK                                                             
         STC   R0,SUBLEST                                                       
         BAS   RE,TESTCHG                                                       
         MVI   ERRCD,INVERR                                                     
         LTR   R0,R0                                                            
         BZ    LFMERR                                                           
         CH    R0,=H'255'                                                       
         BH    LFMERR                                                           
* TEST ESTIMATE NUMBERS ASCEND                                                  
         LA    R0,SUBLIST                                                       
         CR    R7,R0               TEST FIRST ENTRY                             
         BE    EDT6                                                             
         SH    R7,=H'48'           BACK UP TO PREVIOUS                          
         MVI   ERRCD,INVERR                                                     
         CLC   0(1,R7),48(R7)                                                   
         BNL   LFMERR                                                           
         LA    R7,48(R7)                                                        
*                                                                               
* NOW TEST ESTIMATE ALREADY ON FILE                                             
*                                                                               
EDT6     CLI   SVACT,C'A'                                                       
         BNE   EDT10                                                            
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         XC    KEY,KEY                                                          
         MVC   KEY(7),SVKEY                                                     
         MVC   KEY+7(1),SUBLEST                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EDT10                                                            
         MVI   ERRCD,DUPERR                                                     
         TM    KEY+13,X'80'                                                     
         BZ    LFMERR                                                           
         MVI   ERRCD,DELERR                                                     
         B     LFMERR                                                           
EDT10    BAS   RE,NEXTUF           DESCRIPTION                                  
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   SUBLDESC,WORK                                                    
*                                                                               
         BAS   RE,NEXTUF           START DATE                                   
         GOTO1 ANY                                                              
         GOTO1 VDATVAL,DMCB,8(R2),SUBLSTDT                                      
         BAS   RE,TESTCHG                                                       
         MVI   ERRCD,INVERR                                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    LFMERR                                                           
*                                                                               
         LA    R0,SUBLIST                                                       
         CR    R7,R0               TEST FIRST ENTRY                             
         BNE   EDT12               NO                                           
* FIRST SUBEST MUST MATCH ESTART                                                
         CLC   ESTART,SUBLSTDT                                                  
         BNE   LFMERR                                                           
         B     EDT20                                                            
* OTHERWISE MATCH PREVIOUS END DATE + 1                                         
EDT12    SR    R0,R0                                                            
         BCTR  R0,0                SET R0 = -1                                  
         GOTO1 VADDAY,DMCB,SUBLSTDT,WORK,(R0)                                   
*                                                                               
         SH    R7,=H'48'                                                        
         MVI   ERRCD,INVERR                                                     
         CLC   SUBLNDDT,WORK                                                    
         BNE   LFMERR                                                           
         LA    R7,48(R7)                                                        
*                                                                               
EDT20    BAS   RE,NEXTUF           END DATE                                     
         GOTO1 ANY                                                              
         GOTO1 VDATVAL,DMCB,8(R2),SUBLNDDT                                      
         BAS   RE,TESTCHG                                                       
         MVI   ERRCD,INVERR                                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    LFMERR                                                           
*                                                                               
         MVI   ERRCD,EBSERR                                                     
         CLC   SUBLNDDT,SUBLSTDT                                                
         BL    LFMERR                                                           
         MVI   ERRCD,NOTINEST                                                   
         CLC   SUBLNDDT,EEND                                                    
         BH    LFMERR                                                           
*                                                                               
EDT30    BAS   RE,NEXTUF           RTG BOOK                                     
         GOTO1 ANY                                                              
         CLC   =C'LATEST',8(R2)                                                 
         BE    EDT35                                                            
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         MVI   ERRCD,INVERR                                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    LFMERR                                                           
         GOTO1 VDATCON,DMCB,WORK,(3,WORK+6)                                     
         MVC   SUBLBOOK,WORK+6                                                  
*                                                                               
EDT35    BAS   RE,NEXTUF           HUT                                          
         GOTO1 ANY                                                              
         CLC   =C'AUTO',8(R2)                                                   
         BE    EDT40                                                            
* EDIT MONTH                                                                    
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),3                                                          
         BNE   LFMERR                                                           
         MVC   WORK(3),8(R2)                                                    
         MVC   WORK+3(4),=C'/78 '                                               
         GOTO1 VDATVAL,DMCB,(2,WORK),WORK+12                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    LFMERR                                                           
         PACK  DUB,WORK+14(2)                                                   
         CVB   R0,DUB                                                           
         SLL   R0,4                                                             
         STC   R0,SUBLHUT                                                       
*                                                                               
EDT40    BAS   RE,NEXTUF                                                        
         CLI   0(R2),9             TEST LAST FIELD                              
         BNH   EDT42               YES-DONE                                     
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    EDT42                                                            
         LA    R7,48(R7)           NEXT SUB-LIST                                
         B     EDT4                                                             
*                                                                               
EDT42    MVI   ERRCD,SUBPERD                                                    
         CLC   EEND,SUBLNDDT                                                    
         BNE   LFMERR                                                           
         EJECT                                                                  
* MAKE SURE MSTR/SUB EST CODES ARE UNUSED                                       
* FOR ALL BRANDS                                                                
*                                                                               
         CLI   SVACT,C'A'                                                       
         BNE   EDT47                                                            
         LA    R6,REC2                                                          
         ST    R6,AREC                                                          
         USING CLTHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),SVCLTDA                                                
         GOTO1 GETREC                                                           
         LA    R6,CLIST-4                                                       
         ST    R8,AREC             RESTORE I/O ADDRESS                          
         B     EDT46                                                            
         DROP  R6                                                               
*                                                                               
EDT44    XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY        A-M/CLT                                      
         MVC   KEY+4(3),0(R6)      PRD                                          
         MVC   KEY+7(1),SUBLEST    EST                                          
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IF KEY IS ON FILE                            
         BE    ERREST               GO BUILD USER ERROR MESSAGE                 
         LA    R7,48(R7)                                                        
         CLI   0(R7),0                                                          
         BNE   EDT44                                                            
* CHECK FOR MASTER TOO                                                          
         MVC   KEY,SVKEY                                                        
         MVC   KEY+4(3),0(R6)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    ERREST                                                           
*                                                                               
EDT46    LA    R6,4(R6)                                                         
         LA    R7,SUBLIST                                                       
         CLI   3(R6),X'FF'                                                      
         BE    EDT46                                                            
         CLI   3(R6),0                                                          
         BNE   EDT44                                                            
***                                                                             
* MAKE SURE THE CSO MASTER REC EXISTS BEFORE WE DO PUTREC OF SUB-EST!!!         
***                                                                             
EDT47    XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING MASRECD,R5                                                       
         MVI   MASKTYPE,MASKTYPQ   X'0D'                                        
         MVI   MASKSTYP,MASKSTPQ   X'6F'                                        
         MVC   MASKAM,EKEYAM       A/M                                          
         MVC   MASKCLT,EKEYCLT                                                  
         MVC   MASKDATE,ESTART                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     MASTER REC EXISTS?                           
         BNE   ERRMASTR            NO...ERROR                                   
         DROP  R5                                                               
         EJECT                                                                  
* WRITE OR ADD SUB-ESTIMATES                                                    
*                                                                               
         LA    R7,SUBLIST                                                       
         XC    KEY,KEY                                                          
*                                                                               
EDT48    CLI   SVACT,C'A'                                                       
         BE    EDT50                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(7),SVKEY                                                     
         MVC   KEY+7(1),SUBLEST                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         B     EDT51                                                            
*                                                                               
EDT50    DS    0H                                                               
         ZAP   ECURPDN,=P'0'                                                    
*                                                                               
         LHI   R0,13                                                            
         LA    R1,EAUTH                                                         
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LHI   R0,26                                                            
         LA    R1,EORD                                                          
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LHI   R0,26                                                            
         LA    R1,EPAID                                                         
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
EDT51    DS    0H                                                               
         MVC   ESTART(12),SUBLSTDT                                              
         GOTO1 VDATCON,DMCB,EEND,(2,EMGDTE)                                     
         MVC   EBOOK,SUBLBOOK                                                   
         MVC   EHUTADJ,SUBLHUT                                                  
         MVC   EDESC,SUBLDESC                                                   
         MVI   EMSTRIND,C'S'                                                    
         MVC   EMSTREST,SVKEY+7                                                 
         MVC   EKEY+7(1),SUBLEST                                                
         MVC   KEY(8),EKEY                                                      
*                                                                               
         L     RF,ADDREC                                                        
         CLI   SVACT,C'A'                                                       
         BE    *+8                                                              
         L     RF,PUTREC                                                        
         BASR  RE,RF                                                            
*                                                                               
         LA    R7,48(R7)                                                        
         CLI   0(R7),0                                                          
         BNE   EDT48                                                            
* SET FLAG IN MASTER                                                            
         CLI   SVACT,C'A'        ACTION = ADD?                                  
         BNE   EDTX              NO...EXIT                                      
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         MVI   EMSTRIND,C'M'                                                    
         MVI   EMSTREST,0                                                       
         GOTO1 PUTREC                                                           
***                                                                             
* UPDATE THE MASTER ESTIMATES IN CSO (ON ACTION ADD)                            
***                                                                             
EDT100   XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING MASRECD,R6                                                       
         MVI   MASKTYPE,MASKTYPQ   X'0D'                                        
         MVI   MASKSTYP,MASKSTPQ   X'6F'                                        
         MVC   MASKAM,EKEYAM       A/M                                          
         MVC   MASKCLT,EKEYCLT                                                  
         MVC   MASKDATE,ESTART                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     MASTER REC EXISTS?                           
         BNE   ERRMASTR            NO...ERROR                                   
         LA    R6,REC2                                                          
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,MELCODEQ                                                  
         MVC   DATADISP,=H'24'                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                 SHOULD ALWAYS BE THERE, EVEN IFTHE           
         DC    H'0'                ELEMENT IS BLANK (SEE SPCSO0B)               
         USING MELELEM,R6                                                       
*                                                                               
         LA    R7,MELLIST          POINT TO MASTER EST LIST                     
         USING MESTLSTD,R7                                                      
         LA    R1,28               MAX ENTRIES                                  
*                                                                               
EDT105   CLI   0(R7),0             END OF LIST?                                 
         BE    EDT110              YES                                          
         LA    R7,MESTLSTL(R7)     POINT TO NEXT MASTER ESTIMATE                
         BCT   R1,EDT105                                                        
*                                                                               
EDT110   CHI   R1,0                OUT OF SPACE IN THE LIST?                    
         BE    ERRNROOM            YES...GIVE AN ERROR                          
*                                                                               
         MVC   MESTNUM,EKEYEST     MASTER ESTIMATE NUMBER                       
         MVI   MESTTYPE,C'C'       CASH/TRADE (DEFAULT TO CASH)                 
         MVI   MESTSPLN,30         SPOT LENGTH (DEFAULT = 30)                   
         LA    R1,SUBLIST          SUB-ESTIMATE LIST ENTRIES                    
         USING SUBLISTD,R1                                                      
         LA    R2,MESTSUBS         SUB-ESTIMATE LIST IN MASTER REC              
*                                                                               
EDT115   CLI   0(R1),0             END OF SUB-ESTIMATE LIST?                    
         BE    EDT120              YES                                          
         MVC   0(1,R2),0(R1)       MOVE FROM SUB-EST REC TO MASTER REC          
         LA    R1,48(R1)           BUMP SUB-ESTIMATE LIST                       
         LA    R2,1(R2)            NEXT SUB-ESTIMATE ENTRY IN MASTER            
         B     EDT115                                                           
*                                                                               
EDT120   GOTO1 PUTREC                                                           
*                                                                               
         ST    R8,AREC             RESTORE I/O ADDRESS                          
*                                                                               
EDTX     B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
TESTCHG  CLI   SVACT,C'A'                                                       
         BER   RE                                                               
         TM    4(R2),X'20'                                                      
         BZ    LFMERR                                                           
         BR    RE                                                               
         SPACE 2                                                                
FNDUF    TM    1(R2),X'20'         TEST PROTECTED                               
         BCR   8,RE                                                             
NEXTUF   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         BR    RE                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
ERREST   LA    R2,LFMMSGH                                                       
         MVC   8(42,R2),=C'* ERROR * EST 999 IS ON FILE FOR BRAND XXX'          
         ZIC   R0,KEY+7                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  22(3,R2),DUB                                                     
         MVC   47(3,R2),KEY+4                                                   
         MVI   ERRAREA,X'FF'       SET ERROR FLAG FOR BASE                      
         LA    R2,LFMRECH                                                       
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     EXIT                                                             
*                                                                               
ERRNROOM LA    R2,LFMMSGH                                                       
         XC    LFMMSG,LFMMSG                                                    
         MVC   8(41,R2),=C'NO ROOM TO ADD SUB-EST LIST TO MASTER REC'           
         MVI   ERRAREA,X'FF'       SET ERROR FLAG FOR BASE                      
         LA    R2,LFMRECH                                                       
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     EXIT                                                             
*                                                                               
ERRMASTR LA    R2,LFMMSGH                                                       
         XC    LFMMSG,LFMMSG                                                    
         MVC   8(39,R2),=C'THIS DATE DOES NOT EXIST FOR CSO MASTER!'            
         MVI   ERRAREA,X'FF'       SET ERROR FLAG FOR BASE                      
         LA    R2,LFMRECH                                                       
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DSECT FOR SUB-ESTIMATE LIST ENTRIES                                           
*                                                                               
SUBLISTD DSECT                                                                  
*                                                                               
SUBLEST  DS    XL1                                                              
SUBLSTDT DS    XL6                                                              
SUBLNDDT DS    XL6                                                              
SUBLBOOK DS    XL2                                                              
SUBLHUT  DS    XL1                                                              
SUBLDESC DS    XL20                                                             
         DS    XL12                SPARE                                        
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFMFAD                                                                       
       ++INCLUDE SPLFMFAD                                                       
         EJECT                                                                  
GENOLD   DSECT                                                                  
*                                                                               
         ORG   DEMAREA                                                          
*                                                                               
SUBLIST  DS    12CL48              NUM/DESC/START/END/BOOK/HUT (36 TOT)         
SUBLISTX EQU   *-1                                                              
DATADISP DS    H                                                                
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
* SPGENCLT                                                                      
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
* SPGENEST                                                                      
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCSO                                                       
 END                                                                            
