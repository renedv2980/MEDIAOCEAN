*          DATA SET SRTXT00    AT LEVEL 011 AS OF 01/12/04                      
*PHASE T14400A                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE TIMEOUT                                                                
*INCLUDE BROAD                                                                  
         TITLE '$TEXT - MESSAGE INPUT/OUTPUT HANDLER'                           
         PRINT NOGEN                                                            
TEXT     CSECT                                                                  
         NMODL WORKX-WORKD,**$TEXT*,RA,RR=RE                                    
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         ST    RD,BASERD                                                        
         MVC   SRPARS,0(R1)        SAVE CALLING PARMS                           
         L     R2,SRPAR6                                                        
         USING SRTXTFFD,R2         R2=A(TWA)                                    
         L     R3,SRPAR1                                                        
         USING SYSFACD,R3          R3=A(SYSFACS)                                
*                                                                               
         L     R1,SRPAR4           EXTRACT V-TYPES FROM COMFACS                 
         USING COMFACSD,R1                                                      
         MVC   ATERMVAL,CTERMVAL                                                
         MVC   ADICTATE,CDICTATE                                                
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ACALLOV,CCALLOV                                                  
         MVC   ADATVAL,CDATVAL                                                  
         MVC   ADATCON,CDATCON                                                  
         MVC   AGETTXT,CGETTXT                                                  
         MVC   ASWITCH,CSWITCH                                                  
         L     RF,CGETFACT                                                      
         DROP  R1                                                               
*                                                                               
         GOTO1 (RF),DMCB,0         GET A(SYSTEMS LIST) FROM GETFACT             
         L     RE,0(R1)                                                         
         MVC   TODAY,FADATE-FACTSD(RE)                                          
         MVC   TODAYB,FADATEB-FACTSD(RE)                                        
         L     R1,FASYSLST-FACTSD(RE)                                           
         LA    R1,6(R1)                                                         
         ST    R1,ASYSTAB                                                       
         MVC   ACTRYTAB,FAACTRY-FACTSD(RE)                                      
*                                                                               
         L     RE,=V(HELLO)        RELOCATE INCLUDES                            
         A     RE,RELO                                                          
         ST    RE,AHELLO                                                        
         L     RE,=V(TIMEOUT)                                                   
         A     RE,RELO                                                          
         ST    RE,ATIMEOUT                                                      
         L     RE,=V(BROAD)                                                     
         A     RE,RELO                                                          
         ST    RE,ABROAD                                                        
*                                                                               
         L     R8,=A(PQIOA-WORKD)                                               
         AR    R8,RC                                                            
         ST    R8,APQIO            R8=A(IOA USED TO WRITE PQ)                   
         L     RE,=A(IOA1-WORKD)   BUILD A(WORKING STORAGE AREAS)               
         AR    RE,RC                                                            
         ST    RE,AIOA1                                                         
         L     R8,=A(TWAIOA-WORKD)                                              
         AR    R8,RC                                                            
         ST    R8,ATWAMSG          R8=A(IOA USED TO READ/WRITE TWA)             
*                                                                               
         GOTO1 ADICTATE,DMCB,C'LU  ',DDDCLST,DDDSLST                            
         EJECT                                                                  
         MVI   ERROR,0             CLEAR ERROR INDICATOR                        
         MVI   DDS,0               CLEAR FLAGS                                  
         MVI   MODE,0              CLEAR MODE                                   
         MVI   MSGNUM,X'FF'        SET LAST MESSAGE                             
         MVI   TEXXH,L'TEXX+8                                                   
*                                                                               
         L     RE,VSSB             EXTRACT SSB DATA                             
         USING SSBD,RE                                                          
         MVC   SYSID,SSBSYSID      SAVE FACPAK SYSTEM ID NUM                    
         MVC   RECLEN,SSBTWAL                                                   
         MVC   CHKDSP,=Y(CHKPTDSP)                                              
         CLC   RECLEN,=H'14336'    FIX FOR OLD 14K TEMPSTR                      
         BNE   *+10                                                             
         MVC   CHKDSP,=H'12800'    CHECKPOINT STARTS AT 12800 FOR 14K           
         MVC   ABCTAB,SSBABC       GET A(BRDCST TABLE)                          
*                                                                               
         L     R5,SRPAR3           EXTRACT UTL INFO                             
         USING UTLD,R5                                                          
*                                                                               
         ST    R5,AUTL                                                          
         TM    TSTAT1,TSTATDDS     SET DDS INDIC                                
         BZ    *+8                                                              
         OI    DDS,DDSTRM                                                       
         MVC   MYTNUM,TNUM         SAVE TERMINAL NUMBER                         
         MVC   TERMNUM,TNUM        SAVE TERMINAL NUMBER                         
         MVC   TBRSNUM,TBRSYS                                                   
         MVI   TBRSYS,0            CLEAR TBRSYS ONCE SAVED                      
         MVC   SVLANG,TLANG                                                     
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BNO   *+12                                                             
         OI    TSVCREQ,X'02'       SET CURSOR FLAG CLEAR BC BITS                
         NI    TSTAT2,255-TSTATBCP-TSTATBCS                                     
*                                                                               
         MVC   PRTUSER,TUSER       SET CONNECT USER                             
         MVC   PRTID,=C'BRD'       SUBID=BRD                                    
         MVI   PRTCLASS,C'S'       CLASS=S                                      
         OC    PRTUSER,PRTUSER     TEST CONNECTED                               
         BNZ   *+18                                                             
         TM    DDS,DDSTRM          IF DDS USE DEFAULT                           
         BZ    *+10                                                             
*&&UK*&& MVC   PRTUSER,=H'38'      U=DDS1                                       
*&&US*&& MVC   PRTUSER,=H'43'      U=TCH1                                       
*                                                                               
         L     RF,SRPAR8                                                        
         USING TIOBD,RF                                                         
         MVC   PFKEY,TIOBAID                                                    
*                                                                               
         SR    R0,R0               SAVE JULIAN DATE AND BINARY TIME             
         SR    R1,R1                                                            
         TIME  BIN                                                              
         STM   R0,R1,MVSTIME                                                    
         OI    MVSDATE+3,X'0F'                                                  
         EJECT                                                                  
*************************************************************                   
*        WHY AM I HERE ?                                    *                   
*************************************************************                   
         SPACE 1                                                                
         LA    R1,BCRSRV           R1 POINTS TO S/R DATA FLD                    
         LR    R4,R1               SET R4 FOR CURSOR                            
         CLI   1(R1),C'R'          TEST FOR =RB                                 
         BNE   *+12                                                             
         OI    MODE,MODEDISP                                                    
         B     RBC                                                              
*                                                                               
         CLI   1(R1),C'B'          TEST FOR =BRD                                
         BNE   *+16                                                             
         OI    MODE,MODEDISP                                                    
         OI    MODE,MODEMNTR       SET AUTO CALL FROM MONITOR                   
         B     RBC                                                              
*                                                                               
         CLI   1(R1),C'L'          TEST FOR =LB                                 
         BNE   *+12                                                             
         OI    MODE,MODELIST                                                    
         B     LISTBC                                                           
*                                                                               
         CLI   1(R1),C'S'          DEAD $SB/$SM                                 
         BE    ERR2                                                             
         B     ERR2                                                             
*                                                                               
LISTBC   BAS   RE,READSTR          READ SAVED STORAGE                           
*                                                                               
         TM    SAVMODE,MODESEL     CLEAR P1 AFTER SELECT                        
         BNO   *+14                                                             
         XC    BCRP1,BCRP1                                                      
         MVI   BCRP1H+5,0                                                       
*                                                                               
         CLC   SAVSRV,BCRSRV       TEST FIRST 2 FIELDS                          
         BNE   *+14                                                             
         CLC   SAVP1+8(16),BCRP1+8                                              
         B     *+10                                                             
         XC    SAVAREA,SAVAREA     RESET ALL SAVE STORAGE IF CHANGED            
*                                                                               
         MVC   SAVSRV,BCRSRV       SAVE FIRST 2 FIELDS                          
         MVC   SAVP1,BCRP1H                                                     
*                                                                               
         BAS   RE,VALSEL           VALIDATE SELECT FIELDS                       
         CLI   ERROR,0                                                          
         BNE   LBC                 DISPLAY SCREEN BEFORE ERROR                  
         TM    DDS,DDSSEL                                                       
         BNO   LBC                                                              
         EJECT                                                                  
SELECT   LA    R0,16                                                            
         LA    R9,SAVTAB                                                        
SEL000   CLI   0(R9),1             TEST FOR SELECT                              
         BE    SEL030                                                           
         CLI   0(R9),2             TEST FOR PRINT                               
         BNE   SEL020                                                           
         MVI   0(R9),0                                                          
         MVC   MSGNUM,1(R9)                                                     
         MVC   DADDR,2(R9)                                                      
         BAS   RE,BCPRINT          OUTPUT TO PRINT QUEUE                        
SEL020   LA    R9,L'SAVTAB(R9)                                                  
         BCT   R0,SEL000                                                        
         B     LBC                                                              
*                                                                               
SEL030   MVC   DADDR,2(R9)                                                      
         MVC   MSGNUM,1(R9)                                                     
         MVI   0(R9),0                                                          
         OI    MODE,MODESEL                                                     
         B     RBC                                                              
         EJECT                                                                  
*************************************************************                   
*        LOAD SCREEN AND VALIDATE P1 TO GET MSGNUM          *                   
*************************************************************                   
         SPACE 1                                                                
RBC      MVC   SAVSRV,BCRSRV       SAVE FIRST 2 FIELDS                          
         MVC   SAVP1,BCRP1H                                                     
         GOTO1 ACALLOV,DMCB,(0,64(R2)),X'D90144FE'                              
         MVC   BCRSRV,SAVSRV       RESTORE FIELDS                               
         MVC   BCRP1H(24),SAVP1                                                 
*                                                                               
         TM    MODE,MODEMNTR       NO MESSAGE NUM FOR MNTR CALL                 
         BO    *+10                                                             
         MVCDD BCRA1,SR#MSGN       SET MESSAGE NUMBER TEXT                      
         LA    R4,BCRP1H           SET R4 TO P1                                 
         ST    R4,CURSOR                                                        
         TM    MODE,MODESEL                                                     
         BO    RP1VX                                                            
         MVI   MSGNUM,255          DEFAULT TO LAST                              
*                                                                               
RP1V     CLI   5(R4),0             VALIDATE P1                                  
         BE    RP1VX                                                            
RP1V1    ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         TM    4(R4),X'08'         TEST NUMERIC                                 
         BO    RP1V2                                                            
         MVI   MSGNUM,255                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),SR@LAST     ALLOW LAST                                   
         BE    RP1VX                                                            
         MVI   MSGNUM,0                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),SR@1ST      ALLOW FIRST                                  
         BE    RP1VX                                                            
         B     ERR1                                                             
*                                                                               
RP1V2    CHI   R1,2                NO MORE THAN 3 DIGITS OR ERROR               
         BH    ERR1                                                             
         EX    R1,*+8              SET NUMBER (IF P1 NUMERIC)                   
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1               NUMBER 1 THRU 128                            
         BZ    ERR1                                                             
         CH    R1,=H'128'                                                       
         BH    ERR1                                                             
         STC   R1,MSGNUM                                                        
*                                                                               
RP1VX    TM    MODE,MODESEL                                                     
         BNO   RBC1                                                             
         CLI   MSGNUM,0            IF MODESEL AND NO MSGNUM GO DIRECT           
         BE    RBCB1                                                            
         EDIT  (B1,MSGNUM),(3,BCLP1),FILL=0                                     
         EJECT                                                                  
*************************************************************                   
*        READ TWA0 AND WORK OUT WHAT BROADCASTS ARE PENDING *                   
*************************************************************                   
         SPACE 1                                                                
RBC1     BAS   RE,TWAREAD          READ IN TWA0 AND INIT IF 1ST TODAY           
         AH    R8,CHKDSP                                                        
         USING CHKPTD,R8           R8=A(TWA0 CHECKPOINT AREA)                   
*                                                                               
RBC2     CLI   TBRSNUM,0           LOOK FOR A DIRECT BROACAST FIRST             
         BNE   RBCB1                                                            
         GOTO1 ABROAD,DMCB,ABCTAB,AUTL,(R8) BUILD CHECK POINT                   
         TM    4(R1),X'80'                                                      
         BZ    *+8                                                              
         OI    TWA0UPDT,X'02'      SET CHECKPOINT UPDATED FLAG                  
*                                                                               
RBC4     XC    BCBYTE(8),BCBYTE    SET TO COMPARE PENDING WITH SENT             
         LA    R6,CHBCPNDG                                                      
         LA    R7,CHBCSENT                                                      
         XC    BCCNTS(BCCNTSL),BCCNTS                                           
         XC    BCLIST(BCLISTL),BCLIST                                           
         MVC   BCLISTX(2),=X'FFFF'                                              
*                                                                               
RBC5     LH    R1,BCACNT           BUMP ACTUAL COUNT                            
         LA    R1,1(R1)                                                         
         STH   R1,BCACNT                                                        
         LH    RE,BCBIT            BUMP TO NEXT BIT                             
         LH    RF,BCBYTE                                                        
         LA    RE,1(RE)                                                         
         CH    RE,=H'7'                                                         
         BNH   RBC5A                                                            
         SR    RE,RE                                                            
         LA    RF,1(RF)                                                         
         LA    R6,1(R6)                                                         
         LA    R7,1(R7)                                                         
         LA    R0,CHBCSENT                                                      
         CR    R6,R0               TEST END OF PENDING LIST                     
         BNL   RBC8                YES                                          
*                                                                               
RBC5A    STH   RE,BCBIT                                                         
         STH   RF,BCBYTE                                                        
         IC    RE,BITMASK(RE)      RE=BIT MASK                                  
*                                                                               
RBC5B    EX    RE,*+8              TEST IF PENDING BIT ON                       
         B     *+8                                                              
         TM    0(R6),0                                                          
         BZ    RBC5                NO IGNORE                                    
         LH    R1,BCPCNT           BUMP PENDING COUNT                           
         LA    R1,1(R1)                                                         
         STH   R1,BCPCNT                                                        
         STC   R1,BCNUMS           SET NUM OF ENTRYS IN ZEROTH SLOT             
         LA    R1,BCNUMS(R1)                                                    
         MVC   0(1,R1),BCACNT+1    SET ACTUAL NUM IN NUMBERS LIST               
*                                                                               
RBC5C    EX    RE,*+8              TEST SENT BIT ON                             
         B     *+8                                                              
         TM    0(R7),0                                                          
         BZ    RBC5D                                                            
         OI    0(R1),X'80'         FLAG ENTRY IN NUMBERS LIST AS SENT           
         LH    R1,BCSCNT           BUMP SENT COUNT                              
         LA    R1,1(R1)                                                         
         STH   R1,BCSCNT                                                        
         B     RBC5                                                             
*                                                                               
RBC5D    STC   RE,BCMASK           HAVE FOUND PENDING NOT YET SENT              
         MVC   BCPNUM,BCPCNT+1                                                  
         LH    RF,BCBYTE           POINT TO ENTRY IN BCTAB                      
         SLL   RF,3                                                             
         AH    RF,BCBIT                                                         
         LA    RE,BCTABL                                                        
         MR    RE,RE                                                            
         L     R5,ABCTAB                                                        
         LA    R5,6(R5,RF)                                                      
         STCM  R5,7,BCTABA                                                      
*                                                                               
RBC6     MVC   BCLTIME,BCBYTE      SAVE LAST TIMED MSG (NEWEST)                 
         OC    BCFTIME,BCFTIME                                                  
         BNZ   *+10                                                             
         MVC   BCFTIME,BCLTIME     SAVE FIRST TIMED MSG (OLDEST)                
         B     RBC5                                                             
         EJECT                                                                  
*************************************************************                   
*        FIND ACTUAL BCTAB ENTRY                            *                   
*************************************************************                   
         SPACE 1                                                                
RBC8     TM    MODE,MODEMNTR       IF MODEMNTR THEN AUTO                        
         BO    RBCA                                                             
*                                                                               
         CLI   BCNUMS,0            EXIT IF NO MESSAGE FOUND                     
         BE    RBCU                                                             
RBC8A    CLI   MSGNUM,0            TEST IF CALLER WANTS FIRST                   
         BNE   RBC8B                                                            
         MVI   MSGNUM,1                                                         
         B     RBC8D                                                            
RBC8B    CLI   MSGNUM,255          TEST IF CALLER WANTS LAST                    
         BNE   RBC8C                                                            
         MVC   MSGNUM,BCNUMS                                                    
         B     RBC8D                                                            
RBC8C    CLC   MSGNUM,BCNUMS       TEST REQUESTED WITH MAXIMUM                  
         BNH   RBC8D                                                            
         LA    R4,BCRP1H                                                        
         B     ERR1                                                             
RBC8D    EQU   *                                                                
*                                                                               
RBC9     SR    R1,R1               GET ACTUAL MESSAGE NUMBER                    
         IC    R1,MSGNUM                                                        
         STC   R1,BCPNUM                                                        
         LA    R1,BCNUMS(R1)                                                    
         MVC   DUB(1),0(R1)                                                     
         MVC   DUB+1(1),DUB                                                     
         NI    DUB,255-X'80'       TURN OFF SENT FLAG                           
         SR    R1,R1                                                            
         IC    R1,DUB              R1=ACTUAL MESSAGE NUMBER                     
         SR    RE,RE                                                            
         LR    RF,R1                                                            
         D     RE,=F'8'                                                         
         STH   RF,BCBYTE                                                        
         STH   RE,BCBIT                                                         
         L     R5,ABCTAB                                                        
         USING BCTABD,R5                                                        
         MH    R1,0(R5)                                                         
         LA    R5,6(R1,R5)                                                      
         STCM  R5,7,BCTABA                                                      
         TM    DUB+1,X'80'         TEST IF MSG FLAGGED AS SENT                  
         BZ    RBCA2               NO THEN LETS DO IT NOW                       
         B     RBCB                                                             
         EJECT                                                                  
RBCA     LA    R1,BCLIST           CALLED BY MNTR - SEND 1ST IN BCLIST          
RBCA1    CLC   0(2,R1),=X'FFFF'                                                 
         BE    RBCU                                                             
         OC    0(8,R1),0(R1)                                                    
         BNZ   *+12                                                             
         LA    R1,8(R1)                                                         
         B     RBCA1                                                            
         MVC   BCBYTE(8),0(R1)     SET BYTE/BIT/PNUM/ENTRY                      
         MVC   MSGNUM,BCPNUM                                                    
RBCA2    LH    RE,BCBIT            GET MASK FROM BIT                            
         IC    RE,BITMASK(RE)                                                   
         STC   RE,BCMASK                                                        
         LH    R7,BCBYTE                                                        
         LA    R7,CHBCSENT(R7)                                                  
         CLC   MYTNUM,TERMNUM      IS THIS MY TERMINAL                          
         BNE   RBCAX                                                            
         OC    0(1,R7),BCMASK      TURN ON SENT BIT                             
         OI    TWA0UPDT,X'04'      SET HAVE UPDATED SENT LIST                   
*                                                                               
         CLC   CHBCPNDG,CHBCSENT   TEST & SET LAST MESSAGE                      
         BNE   *+8                                                              
         OI    DDS,DDSLAST                                                      
RBCAX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        BUILD BROADCAST IN TWA                             *                   
*************************************************************                   
         SPACE 1                                                                
RBCB     SR    R5,R5               POINT TO ENTRY IN BCTAB                      
         ICM   R5,7,BCTABA                                                      
         MVI   WORK,0              SET LEN OF MESSAGE HEADING                   
         MVC   WORK+1(79),BLANKS                                                
         TM    BCTFLAG,BCTFDEL     DONT SEND DELETED ENTRIES                    
         BNZ   RBCM                MESSAGE NO LONGER APPLICABLE                 
*                                                                               
RBCB1    GOTO1 ASWITCH,DUB,X'0AFFFFFF',0                                        
         CLI   4(R1),0                                                          
         BNE   RBCN                MESSAGES NOT AVAILABLE                       
         CLI   TBRSNUM,0                                                        
         BE    *+8                                                              
         BAS   RE,SYSMSG           GET DIRECT BROADCAST                         
         L     R7,AIOA1            R7=A(BRDCST RECORD)                          
         USING BRDKEYD,R7                                                       
         TM    MODE,MODESEL        IS MODESEL DA IS ALREADY SET                 
         BO    *+10                                                             
         MVC   DADDR,BCTDADR                                                    
         GOTO1 ADATAMGR,DMCB,GETREC,GENFIL,DADDR,(R7),DMWORK                    
         GOTO1 ASWITCH,DUB,X'01FFFFFF',0                                        
         CLI   4(R1),0                                                          
         BNE   RBCN                MESSAGES NOT AVAILABLE                       
         CLI   DMCB+8,0                                                         
         BNE   RBCM                MESSAGE NO LONGER APPLICABLE                 
*                                                                               
RBCB2    TM    BRDSTAT,X'80'       DONT SENT DELETED RECORDS                    
         BNZ   RBCM                MESSAGE NO LONGER APPLICABLE                 
*                                                                               
RBCB3    MVC   SAVNUM,BRDKMSGN     SAVE MESSAGE DISK NUMBER                     
         LA    R7,BRDFSTEL         R7=A(BRDCST RECORD ELEMENT)                  
         LA    R4,BCRL1H           R4=A(SCREEN DISPLAY LINE)                    
*                                                                               
RBCC     CLI   0(R7),X'00'         END OF RECORD                                
         BE    RBCS                                                             
RBCC1    CLI   0(R7),BRDHEDEQ      MSG HEADING ELEMENT                          
         BNE   RBCC2                                                            
         SR    RF,RF                                                            
         ICM   RF,1,BRDHEDTL-BRDHEDD(R7)                                        
         BZ    RBCD                                                             
         STC   RF,WORK             SAVE LENGTH OF HEADING TEXT                  
         BCTR  RF,0                                                             
         EX    RF,*+8              SAVE HEADING TEXT                            
         B     *+10                                                             
         MVC   WORK+1(0),BRDHEDTX-BRDHEDD(R7)                                   
*                                                                               
* HOW ABOUT USING THE HEADING THAT WAS SAVED                                    
         LA    RF,75               POSITION TO CENTER                           
         ZIC   RE,WORK                                                          
         SR    RF,RE                                                            
         SRL   RF,1                                                             
         N     RF,=F'-2'           BUT ALWAYS EVEN                              
         LA    R1,BCRTOP                                                        
         LA    R1,1(R1,RF)                                                      
         BCTR  RE,0                DEC FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),WORK+1      INSERT HEADER                                
         B     RBCD                                                             
*                                                                               
RBCC2    CLI   0(R7),BRDTXTEQ      MSG TEXT ELEMENT                             
         BE    RBCE                                                             
*                                                                               
RBCD     SR    R1,R1               BUMP TO NEXT BRDCST RECORD ELEMENT           
         IC    R1,1(R7)                                                         
         AR    R7,R1                                                            
         B     RBCC                                                             
*                                                                               
RBCE     SR    RF,RF               MOVE TEXT TO SCREEN LINE                     
         IC    RF,1(R7)                                                         
         LA    RE,BRDTXTTX-BRDTXTD                                              
         SR    RF,RE                                                            
         BNP   RBCF                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R4),BRDTXTTX-BRDTXTD(R7)                                    
*                                                                               
RBCF     LA    R4,BCRL2H-BCRL1H(R4) BUMP TO NEXT SCREEN LINE                    
         LA    R0,BCRBOTH                                                       
         CR    R4,R0               TEST IF STILL ROOM ON SCREEN                 
         BL    RBCD                YES BACK FOR NEXT TEXT ELEMENT               
         B     RBCS                                                             
         EJECT                                                                  
*************************************************************                   
*        WRITE MESSAGES ECT AND EXIT                        *                   
*************************************************************                   
         SPACE 1                                                                
RBCM     MVI   MSG,12              MESSAGE NO LONGER APPLICABLE                 
         B     *+8                                                              
RBCN     MVI   MSG,11              MESSAGES NOT AVAILABLE                       
         LA    RE,TEXXH                                                         
         ST    RE,FLD                                                           
         XC    TXT,TXT                                                          
         BAS   RE,GTXT                                                          
         MVC   BCRL2+2(L'BCRL2-4),TEXX                                          
*                                                                               
RBCS     OI    TWA0UPDT,X'80'      SET MESSAGE MOVED TO SCREEN                  
         TM    MODE,MODESEL        WRITE BACK SAVED STR IF SELECT MODE          
         BNO   *+8                                                              
         BAS   RE,WRITESTR                                                      
*                                                                               
RBCU     TM    TWA0UPDT,X'0F'      TEST IF TWA 0 UPDATED                        
         BZ    RBCW                                                             
         MVC   CHBCDATE,MVSDATE    SET DATE/TIME LAST UPDATED                   
         MVC   CHBCTIME,MVSTIME                                                 
         L     R5,ABCTAB           R5=A(BRDCST TABLE)                           
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         MVC   CHBCCNUM,BCTLCNUM   SET LAST BC NUMBER                           
         MVC   DMCB+8(2),=X'0000'                                               
         MVC   DMCB+10(2),TERMNUM                                               
         MVC   DMCB+20(2),=C'L='   SPECIAL PARM FOR TWA0 READ/WRITES            
         MVC   DMCB+22(2),RECLEN                                                
         L     R0,ATWAMSG                                                       
         GOTO1 ADATAMGR,DMCB,DMWRT,TEMPSTR,,(R0)                                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RBCW     TM    TWA0UPDT,X'80'      TEST IF SENT A MESSAGE                       
         BO    RBCW1                                                            
         LA    RE,205              NO MESSAGES FOUND                            
         OI    DDS,DDSLAST         SET LAST FLAG                                
         MVI   WORK,0                                                           
         B     OKX                                                              
*                                                                               
RBCW1    TM    MODE,MODEMNTR       TEST IF CALLED BY MNTR                       
         BZ    RBCW2                                                            
         GOTO1 ATIMEOUT,DMCB,(1,MVSTIME),(X'46',DUB1)                           
         LA    RE,DUB1                                                          
         ST    RE,TXT                                                           
         MVI   TXT,8                                                            
         MVI   MSG,8               THIS IS A SYSTEM BROACAST                    
         CLI   TBRSNUM,0                                                        
         BE    *+12                                                             
         MVI   MSG,18              THIS IS A DIRECT BROADCAST                   
         OI    DDS,DDSLAST         SET LAST FLAG                                
         XC    WORK,WORK                                                        
         LA    RE,BCRINF2H                                                      
         ST    RE,FLD                                                           
         BAS   RE,GTXT             THIS IS A ? FROM ?                           
*                                                                               
RBCW2    CLI   MSGNUM,0            DO WE HAVE A MESSAGE NUMBER                  
         BE    RBCW2A                                                           
         MVI   MSG,10                                                           
         XC    TXT,TXT                                                          
         MVC   WORK(6),=C' 01 01'  MSG 01 OF 01                                 
         MVI   WORK,3                                                           
         MVI   WORK+3,3                                                         
         MVI   WORK+6,0                                                         
         CLI   TBRSNUM,0           DISPLAY 1 OF 1 IF SYSTEM MSG                 
         BNE   RBCW2X                                                           
*                                                                               
         SR    R1,R1               ELSE EDIT OUT NUMBERS                        
         IC    R1,MSGNUM                                                        
         CVD   R1,DUB                                                           
         UNPK  WORK+1(2),DUB       SET MESSAGE NUMBER                           
         OI    WORK+2,X'F0'                                                     
         LH    R1,BCPCNT           SET TOTAL NUMBER OF MESSAGES                 
         CVD   R1,DUB                                                           
         UNPK  WORK+4(2),DUB                                                    
         OI    WORK+5,X'F0'                                                     
         B     RBCW2X                                                           
*                                                                               
RBCW2A   MVI   MSG,21              MESSAGE NUMBER XXXX DISPLAYED                
         EDIT  (B2,SAVNUM),(5,DUB1),ALIGN=LEFT                                  
         LA    R1,DUB1                                                          
         ST    R1,TXT                                                           
         STC   R0,TXT                                                           
         XC    WORK,WORK                                                        
*                                                                               
RBCW2X   LA    RE,BCRINF3H         CALL GETTXT FOR MESSAGE                      
         ST    RE,FLD                                                           
         BAS   RE,GTXT                                                          
*                                                                               
RBCW4    TM    MODE,MODEMNTR       MONITOR CALL TO DISPLAY MESSAGE              
         BZ    RBCW5                                                            
         LA    RE,200              PRESS ENTER TO RECALL SCREEN                 
         TM    DDS,DDSLAST                                                      
         BO    *+8                                                              
         LA    RE,206              MESSAGE DISPLAYED - ENTER FOR NEXT           
         MVI   WORK,0                                                           
         B     OKX                                                              
*                                                                               
RBCW5    TM    MODE,MODESEL        TEST FOR SELECT MODE                         
         BO    RBCW6                                                            
         LA    RE,204              ENTER NEXT MESSAGE NUMBER                    
         MVI   WORK,0                                                           
         B     OKX                                                              
*                                                                               
RBCW6    LA    RE,206              MESSAGE DISPLAYED - ENTER F NEXT             
         MVI   WORK,0                                                           
         B     OKX                                                              
         EJECT                                                                  
*************************************************************                   
*        VTAM LIST BROADCASTS                               *                   
*************************************************************                   
LBC      EQU   *                                                                
         MVCDD BCLA1,SR#MSGN       SET MESSAGE NUMBER TEXT                      
         XC    BCCNTS(BCCNTSL),BCCNTS                                           
         LA    R9,SAVTAB           SET R9 TO SAVE TABLE                         
         XC    SAVTAB(16*L'SAVTAB),SAVTAB                                       
*                                                                               
LP1V     LA    R4,BCLP1H           VALIDATE MESSAGE NUMBER (P1)                 
         MVI   MSGNUM,0            DEFAULT IS FIRST                             
         CLI   5(R4),0                                                          
         BE    LP1VX                                                            
LP1V1    ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         TM    4(R4),X'08'         TEST NUMERIC                                 
         BO    LP1V2                                                            
         MVI   MSGNUM,255                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),SR@LAST     ALLOW LAST                                   
         BE    LP1VX                                                            
         MVI   MSGNUM,0                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),SR@1ST      ALLOW FIRST                                  
         BE    LP1VX                                                            
         B     ERR1                                                             
*                                                                               
LP1V2    EX    R1,*+8              SET NUMBER (IF P1 NUMERIC)                   
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1               NUMBER 1 THRU 128                            
         BZ    ERR1                                                             
         CH    R1,=H'128'                                                       
         BH    ERR1                                                             
         STC   R1,MSGNUM                                                        
*                                                                               
LP1VX    SR    R1,R1                                                            
         IC    R1,MSGNUM                                                        
         CLI   PFKEY,0                                                          
         BE    LP1VXX                                                           
         CLI   PFKEY,7                                                          
         BNE   LP1VX1                                                           
         SH    R1,=H'16'                                                        
         BNM   LP1VXX                                                           
         SR    R1,R1                                                            
         B     LP1VXX                                                           
LP1VX1   CLI   PFKEY,8                                                          
         BNE   LP1VXX                                                           
         LA    R1,16(R1)                                                        
LP1VXX   STC   R1,MSGNUM                                                        
         XC    BCLP1,BCLP1                                                      
         CLI   MSGNUM,0            IF ZERO DON'T EDIT                           
         BE    LBC1                                                             
         EDIT  (B1,MSGNUM),(3,BCLP1),FILL=0                                     
*                                                                               
LBC1     MVI   MSG,13              SET MSG NUM FOR NORMAL DATA                  
         TM    DDS,DDSTRM          DDS TERMS CAN SAY $LB,......                 
         BZ    LBC1X                                                            
*                                                                               
LBC1A    CLC   BCLSRV+3(4),=C',ALL'                                             
         BNE   LBC1B                                                            
         MVI   MSG,15              SET MSG NUM FOR DDS DATA                     
         OI    DDS,DDSBCT                                                       
         L     R5,ABCTAB           R5=A(BRDCST TABLE)                           
         USING BCTABD,R5                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         LH    R0,BCTLCNUM         R0=NUM OF ENTRYS IN CORE BC TABLE            
         LTR   R0,R0                                                            
         BZ    LBC1X                                                            
         STH   R0,BCPCNT                                                        
         STC   R0,BCNUMS                                                        
         LA    R1,1                                                             
         LA    RE,BCNUMS+1                                                      
LBC1A1   STC   R1,0(RE)            BUILD BCNUMS TO CONTAIN ALL BC MSGS          
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         CR    R1,R0                                                            
         BNH   LBC1A1                                                           
         B     LBC1X                                                            
*                                                                               
LBC1B    CLI   BCLSRV+3,C','       $LB,TERMLUID                                 
         BNE   LBC1X                                                            
         LA    R4,BCLSRVH                                                       
         MVC   DUB(8),BCLSRV+4                                                  
         GOTO1 ATERMVAL,DMCB,(X'80',DUB)                                        
         SR    RF,RF                                                            
         ICM   RF,7,DMCB+5         RF=A(UTL ENTRY)                              
         BZ    ERR1                                                             
         OC    TPRNT-UTLD(3,RF),TPRNT-UTLD(RF)                                  
         BNZ   ERR1                                                             
         MVC   TERMNUM,TNUM-UTLD(RF)                                            
*                                                                               
LBC1X    LA    R4,BCLH1H           SET HEAD LINE FOR LIST MSGS                  
         ST    R4,FLD                                                           
         XC    TXT,TXT                                                          
         BAS   RE,GTXT                                                          
         IC    R1,MSG              AND UNDERLINE IT                             
         LA    R1,1(R1)                                                         
         STC   R1,MSG                                                           
         LA    R4,BCLH2H                                                        
         ST    R4,FLD                                                           
         BAS   RE,GTXT                                                          
         TM    DDS,DDSBCT          TEST IF DISPLAYING BCTAB MSGS                
         BO    LBC3                YES                                          
*                                                                               
LBC2     BAS   RE,TWAREAD          READ IN TWA0 INTO ATWAMSG                    
         AH    R8,CHKDSP                                                        
         USING CHKPTD,R8           R8=A(TWA0 CHECKPOINT AREA)                   
         TM    TWA0UPDT,X'01'      TEST IF TWA WAS INITIALISED                  
         BZ    LBC2A0              NO THEN OK                                   
*                                                                               
         GOTO1 ABROAD,DMCB,ABCTAB,AUTL,(R8) BUILD CHECK POINT                   
         TM    4(R1),X'80'                                                      
         BZ    *+8                                                              
         OI    TWA0UPDT,X'02'      SET CHECKPOINT UPDATED FLAG                  
*                                                                               
LBC2A0   XC    BCBYTE(8),BCBYTE                                                 
         LA    R6,CHBCPNDG         R6=A(TRM BRDCST PENDING LIST)                
         LA    R7,CHBCSENT         R7=A(TRM BRDCST SENT LIST)                   
*                                                                               
LBC2A    LH    R1,BCACNT           BUMP ACTUAL COUNT                            
         LA    R1,1(R1)                                                         
         STH   R1,BCACNT                                                        
         LH    RE,BCBIT            BUMP TO NEXT BIT                             
         LH    RF,BCBYTE                                                        
         LA    RE,1(RE)                                                         
         CH    RE,=H'7'                                                         
         BNH   LBC2B                                                            
         SR    RE,RE                                                            
         LA    RF,1(RF)                                                         
         LA    R6,1(R6)                                                         
         LA    R7,1(R7)                                                         
         LA    R0,CHBCSENT                                                      
         CR    R6,R0               TEST END OF PENDING LIST                     
         BNL   LBC3                YES                                          
LBC2B    STH   RE,BCBIT                                                         
         STH   RF,BCBYTE                                                        
         IC    RE,BITMASK(RE)      RE=BIT MASK                                  
*                                                                               
LBC2C    EX    RE,*+8              TEST IF PENDING BIT ON                       
         B     *+8                                                              
         TM    0(R6),0                                                          
         BZ    LBC2A               NO IGNORE                                    
         LH    R1,BCPCNT           BUMP PENDING COUNT                           
         LA    R1,1(R1)                                                         
         STH   R1,BCPCNT                                                        
         STC   R1,BCNUMS           SET NUM OF ENTRYS IN ZEROTH SLOT             
         LA    R1,BCNUMS(R1)                                                    
         MVC   0(1,R1),BCACNT+1    SET ACTUAL NUM IN NUMBERS LIST               
*                                                                               
LBC2D    EX    RE,*+8              TEST SENT BIT ON                             
         B     *+8                                                              
         TM    0(R7),0                                                          
         BZ    LBC2A                                                            
         OI    0(R1),X'80'         FLAG ENTRY IN NUMBERS LIST AS SENT           
         LH    R1,BCSCNT           BUMP SENT COUNT                              
         LA    R1,1(R1)                                                         
         STH   R1,BCSCNT                                                        
         B     LBC2A                                                            
*                                                                               
LBC3     CLI   BCNUMS,0            EXIT IF NO BRDCST MESSAGES                   
         BNE   LBC4                                                             
         LA    RE,205              NO MESSAGES FOUND                            
         MVI   WORK,0                                                           
         B     OKX                                                              
*                                                                               
LBC4     LA    R4,BCLS1H           R4=A(FIRST DISPLAY LINE)                     
         ST    R4,CURSOR                                                        
         USING LSTD,R4                                                          
         LA    R8,BCNUMS           R8=A(FIRST BRDCST MSG NUM)                   
         XC    FULL1,FULL1                                                      
         CLI   MSGNUM,255                                                       
         BNE   *+10                                                             
         MVC   MSGNUM,BCNUMS       SET LAST MESSAGE NUMBER                      
         CLC   MSGNUM,BCNUMS                                                    
         BNH   LBC5                                                             
         LA    R4,BCLP1H                                                        
         B     ERR4                NO MESSAGES FOUND                            
*                                                                               
LBC5     CLI   ERROR,0             UNLESS THIS IS AN ERROR DISPLAY              
         BNE   *+10                                                             
         XC    LSTSEL,LSTSEL       CLEAR SELECT FIELDS                          
         SR    R1,R1               BUMP RELATIVE MESSAGE NUMBER                 
         IC    R1,FULL1                                                         
         LA    R1,1(R1)                                                         
         STC   R1,FULL1                                                         
         LA    R8,1(R8)            BUMP TO NEXT BCNUM LIST ENTRY                
         CLI   0(R8),0             TEST IF END OF LIST                          
         BE    LBCD                                                             
         MVC   FULL1+1(1),0(R8)    FULL1+1 HAS ACTUAL MSG NUM                   
         MVC   FULL1+2(1),0(R8)                                                 
         NI    FULL1+1,255-X'80'   TURN OFF SENT FLAG                           
         NI    FULL1+2,X'80'       FULL1+2 HAS MSG FLAGS                        
         SR    R1,R1                                                            
         IC    R1,FULL1+1          R1=ACTUAL MESSAGE NUMBER                     
         SR    RE,RE                                                            
         LR    RF,R1                                                            
         D     RE,=F'8'                                                         
         STH   RF,BCBYTE                                                        
         STH   RE,BCBIT                                                         
         L     R5,ABCTAB                                                        
         MH    R1,0(R5)                                                         
         LA    R5,6(R1,R5)                                                      
         STCM  R5,7,BCTABA         R5=A(BCTAB ENTRY FOR THIS MSG)               
         CLC   FULL1(1),MSGNUM                                                  
         BL    LBC5                IGNORE IF LESS THAT INPUT VALUE              
*                                                                               
LBC6     MVI   WORK,0              SET LEN OF MESSAGE HEADING                   
         MVC   WORK+1(79),BLANKS                                                
         MVI   TEMP,0              SET LEN OF FIRST TEXT LINE                   
         MVC   TEMP+1(79),BLANKS                                                
         TM    BCTFLAG,BCTFDEL                                                  
         BZ    *+12                                                             
         OI    FULL1+2,X'40'       SET BCTAB ENTRY DELETED                      
         B     LBC8                                                             
*                                                                               
LBC6A    GOTO1 ASWITCH,DUB,X'0AFFFFFF',0                                        
         CLI   4(R1),0                                                          
         BE    *+12                                                             
         OI    FULL1+2,X'20'       SET CANT READ BRDCST RECORD                  
         B     LBC8                                                             
         L     R7,AIOA1            R7=A(BRDCST RECORD)                          
         USING BRDKEYD,R7                                                       
         MVC   DADDR,BCTDADR                                                    
         GOTO1 ADATAMGR,DMCB,GETREC,GENFIL,DADDR,(R7),DMWORK                    
         GOTO1 ASWITCH,DUB,X'01FFFFFF',0                                        
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB+8,0                                                         
         BE    *+12                                                             
         OI    FULL1+2,X'20'       SET CANT READ BRDCST RECORD                  
         B     LBC8                                                             
*                                                                               
LBC6B    TM    BRDSTAT,X'80'       TEST IF DELETED                              
         BZ    *+8                                                              
         OI    FULL1+2,X'10'       SET BRDCST RECORD IS DELETED                 
         LA    R7,BRDFSTEL         R7=A(BRDCST RECORD ELEMENT)                  
*                                                                               
LBC7     CLI   0(R7),X'00'         END OF RECORD                                
         BE    LBC8                                                             
LBC7A    CLI   0(R7),BRDHEDEQ      MSG HEADING ELEMENT                          
         BNE   LBC7B                                                            
         SR    RF,RF                                                            
         ICM   RF,1,BRDHEDTL-BRDHEDD(R7)                                        
         BZ    LBC7C                                                            
         STC   RF,WORK             SAVE LENGTH OF HEADING TEXT                  
         BCTR  RF,0                                                             
         EX    RF,*+8              SAVE HEADING TEXT                            
         B     LBC7C                                                            
         MVC   WORK+1(0),BRDHEDTX-BRDHEDD(R7)                                   
LBC7B    CLI   0(R7),BRDTXTEQ      MSG TEXT ELEMENT                             
         BE    LBC7E                                                            
*                                                                               
LBC7C    SR    R1,R1               BUMP TO NEXT BRDCST RECORD ELEMENT           
         IC    R1,1(R7)                                                         
         AR    R7,R1                                                            
         B     LBC7                                                             
*                                                                               
LBC7E    SR    RF,RF               SAVE FIRST TEXT LINE                         
         IC    RF,1(R7)                                                         
         LA    RE,BRDTXTTX-BRDTXTD                                              
         SR    RF,RE                                                            
         BNP   LBC7C                                                            
         STC   RF,TEMP                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+1(0),BRDTXTTX-BRDTXTD(R7)                                   
*                                                                               
LBC8     EQU   *                                                                
*                                                                               
LBCA     SR    R0,R0               NORMAL LIST DISPLAY LINE                     
         IC    R0,FULL1                                                         
         OC    BCFSTLST,BCFSTLST                                                
         BNZ   *+8                                                              
         STH   R0,BCFSTLST         SET FIRST MSG NUM TO BE DISPLAYED            
         STH   R0,BCLSTLST         SET LAST MSG NUM TO BE DISPLAYED             
         TM    DDS,DDSBCT                                                       
         BO    LBCB                                                             
*                                                                               
LBCA1    CVD   R0,DUB              MSG NUM (RELATIVE TO TERMINAL)               
         OI    DUB+7,X'0F'                                                      
         UNPK  LSTNUM,DUB                                                       
*                                                                               
LBCA2    MVC   LSTSTAT,DOTS        MESSAGE STATUS                               
         TM    FULL1+2,X'80'                                                    
         BZ    *+8                                                              
         MVI   LSTSTAT+0,C'S'      SET SENT CHARACTER                           
         TM    FULL1+2,X'70'                                                    
         BZ    *+8                                                              
         MVI   LSTSTAT+1,C'D'      SET NOT AVAILABLE CHR                        
*                                                                               
LBCA3    MVC   LSTMHDG(15),DOTS    MESSAGE HEADING                              
         TM    FULL1+2,X'70'                                                    
         BNZ   LBCA3X              NOT AVAILABLE                                
         CLI   WORK,0                                                           
         BE    LBCA3X              NO DATA                                      
         MVC   LSTMHDG,WORK+1                                                   
LBCA3X   EQU   *                                                                
*                                                                               
LBCA4    MVC   LSTMTXT(12),DOTS    MESSAGE TEXT                                 
         TM    FULL1+2,X'70'                                                    
         BZ    LBCA4A                                                           
         MVI   MSG,12              MESSAGE NO LONGER APPLICABLE                 
         LA    RE,TEXXH                                                         
         ST    RE,FLD                                                           
         XC    TXT,TXT                                                          
         BAS   RE,GTXT                                                          
         MVC   LSTMTXT,TEXX                                                     
         B     LBCA4X                                                           
LBCA4A   SR    R1,R1               GET DATA LENGTH                              
         ICM   R1,1,TEMP                                                        
         BZ    LBCA4X                                                           
         LA    RE,TEMP+1                                                        
LBCA4B   CLI   0(RE),C' '          FIND FIRST SIGNIFICANT CHR                   
         BH    LBCA4C                                                           
         LA    RE,1(RE)                                                         
         BCT   R1,LBCA4B                                                        
         B     LBCA4X                                                           
LBCA4C   LA    R0,L'LSTMTXT        CHECK SIG LEN WITH DISPLAY LEN               
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         MVC   LSTMTXT,BLANKS                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTMTXT(0),0(RE)                                                 
LBCA4X   B     LBCC                                                             
*                                                                               
LBCB     EQU   *                   DDS LIST DISPLAY LINE                        
*                                                                               
LBCB1    SR    R0,R0               MSG NUM (FROM BCTAB)                         
         IC    R0,FULL1+1                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LS1NUM,DUB                                                       
*                                                                               
LBCB2    MVC   LS1STAT,DOTS        MESSAGE STATUS                               
         TM    FULL1+2,X'40'                                                    
         BZ    *+10                                                             
         MVC   LS1STAT,=C'D '      DELETED FROM CORE                            
         TM    FULL1+2,X'20'                                                    
         BZ    *+10                                                             
         MVC   LS1STAT,=C'NA'      NOT AVAILABLE                                
         TM    FULL1+2,X'10'                                                    
         BZ    *+10                                                             
         MVC   LS1STAT,=C'DD'      DELETED FROM DISK                            
*                                                                               
LBCB3    LH    R0,BCTDNUM          DISK MESSAGE NUMBER                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LS1DNUM,DUB                                                      
*                                                                               
LBCB4    MVC   LS1NAME,BCTNAME     DISK MESSAGE NAME                            
*                                                                               
LBCB5    MVC   LS1CTRY,DOTS        COUNTRY FILTER                               
         CLI   BCTCTRY,X'FF'                                                    
         BNE   *+14                                                             
         MVC   LS1CTRY,=C'ALL'                                                  
         B     LBCB5X                                                           
         L     R1,ACTRYTAB                                                      
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
LBCB5A   CLC   BCTCTRY,CTRYCODE-CTRYTABD(R1)                                    
         BE    *+12                                                             
         BXLE  R1,RE,LBCB5A                                                     
         B     LBCB5X                                                           
         MVC   LS1CTRY,CTRYSHR-CTRYTABD(R1)                                     
LBCB5X   EQU   *                                                                
*                                                                               
LBCB6    MVC   LS1SYS(3),DOTS      SYSTEM/SE FILTER                             
         XC    DUB,DUB                                                          
         MVC   DUB+4(1),BCTOVSYS                                                
         CLI   BCTOVSYS,0                                                       
         BNE   *+12                                                             
         CLI   BCTSYS,0                                                         
         BE    LBCB6X                                                           
LBCB6A   L     R1,VSELIST          SEARCH SE LIST FOR OVSYS OR SE               
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
LBCB6B   CLC   BCTOVSYS,SEOVSYS-SELISTD(R1)                                     
         BNE   *+10                                                             
         MVC   DUB(4),SEPGMS-SELISTD(R1)                                        
         CLC   BCTSYS,SESYS-SELISTD(R1)                                         
         BE    *+12                                                             
         BXLE  R1,RE,LBCB6B                                                     
         B     LBCB6C                                                           
         MVC   DUB(4),SEPGMS-SELISTD(R1)                                        
         MVC   DUB+4(1),SEOVSYS-SELISTD(R1)                                     
         MVC   DUB+5(1),SEFILSET-SELISTD(R1)                                    
LBCB6C   CLI   DUB+4,0                                                          
         BE    LBCB6X                                                           
         L     R1,ASYSTAB          POINT TO SYSTEM NAMES TABLE                  
LBCB6D   CLI   SYSLNUM-SYSLSTD(R1),0                                            
         BE    LBCB6X                                                           
         CLC   DUB+4(1),SYSLNUM-SYSLSTD(R1)                                     
         BE    *+12                                                             
         LA    R1,SYSLLEN(R1)                                                   
         B     LBCB6D                                                           
         MVC   LS1SYS(3),SYSLSHRT-SYSLSTD(R1)                                   
         SR    R1,R1                                                            
         IC    R1,DUB+5            GET FILE SET NUMBER                          
         LA    R1,LBCB6F(R1)                                                    
         MVC   LS1SYS+3(1),0(R1)                                                
         B     LBCB6X                                                           
LBCB6F   DC    C' 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'                          
LBCB6X   EQU   *                                                                
*                                                                               
LBCB7    MVC   LS1PRG,DOTS         PROGRAM FILTER                               
         CLI   BCTPRG,0                                                         
         BE    LBCB7X                                                           
         ICM   R1,15,DUB           GET A(PROGRAMS LIST)                         
         BZ    LBCB7X                                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
LBCB7A   CLC   BCTPRG,PGMNUM-PGMLSTD(R1)                                        
         BE    *+12                                                             
         BXLE  R1,RE,LBCB7A                                                     
         B     LBCB7X                                                           
         MVC   LS1PRG,PGMNAME-PGMLSTD(R1)                                       
LBCB7X   EQU   *                                                                
*                                                                               
LBCB8    MVC   LS1LUID,DOTS        LUID FILTER                                  
         OC    BCTLUID,BCTLUID                                                  
         BZ    LBCB8X                                                           
         CLC   BCTLUID,BLANKS                                                   
         BE    LBCB8X                                                           
         MVC   LS1LUID,BCTLUID                                                  
LBCB8X   EQU   *                                                                
*                                                                               
LBCB9    MVC   LS1TIME,DOTS        START/END TIMES                              
         OC    BCTSTTM,BCTSTTM                                                  
         BZ    LBCB9X              NO TIME FILTER                               
         MVC   LS1TIME,BLANKS                                                   
         GOTO1 ACALLOV,DMCB,0,X'D9000A11'                                       
         CLI   DMCB+4,X'FF'        GET A(UNTIME)                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,BCTSTTM          START HOUR                                   
         MH    RE,=H'100'                                                       
         ZIC   RF,BCTSTTM+1        START MINUTES                                
         AR    RE,RF                                                            
         STH   RE,FULL2            START TIME (MILITARY)                        
         ZIC   RE,BCTENTM          END HOUR                                     
         MH    RE,=H'100'                                                       
         ZIC   RF,BCTENTM+1        END MINUTES                                  
         AR    RE,RF                                                            
         STH   RE,FULL2+2          END TIME (MILITARY)                          
         L     RF,DMCB             A(UNTIME)                                    
         GOTO1 (RF),DMCB,FULL2,LS1TIME                                          
LBCB9X   EQU   *                                                                
*                                                                               
LBCB10   MVC   LS1MHDG(15),DOTS    MESSAGE HEADING                              
         TM    FULL1+2,X'70'                                                    
         BNZ   LBCB10X             NOT AVAILABLE                                
         CLI   WORK,0                                                           
         BE    LBCB10X             NO DATA                                      
         MVC   LS1MHDG,WORK+1                                                   
LBCB10X  EQU   *                                                                
*                                                                               
LBCC     MVI   0(R9),0             SAVE BRDCST DA IN SAVE TAB                   
         MVC   2(4,R9),BCTDADR                                                  
         MVI   1(R9),0                                                          
         TM    DDS,DDSBCT          SAVE NUMBER IF NOT DDSBCT                    
         BO    *+10                                                             
         MVC   1(1,R9),FULL1                                                    
         LA    R9,L'SAVTAB(R9)                                                  
*                                                                               
         LA    R4,BCLS2H-BCLS1H(R4)                                             
         LA    R1,BCLTABH          END OF SCREEN                                
         CR    R4,R1                                                            
         BL    LBC5                                                             
*                                                                               
LBCD     CLI   ERROR,0                                                          
         BNE   LBCERR                                                           
         TM    MODE,MODEPQI                                                     
         BO    LBCE                                                             
         MVI   MSG,17              MSG NUMS II THRU JJ LISTED                   
         XC    TXT,TXT                                                          
         LH    R1,BCFSTLST                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+1(2),DUB       SET FIRST MSG NUM LISTED                     
         LH    R1,BCLSTLST         SET LAST MSG NUM LISTED                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+4(2),DUB                                                    
         MVI   WORK,3                                                           
         MVI   WORK+3,3                                                         
         LA    RE,BCLMSGH                                                       
         ST    RE,FLD                                                           
         BAS   RE,GTXT                                                          
         B     LBCF                                                             
*                                                                               
LBCE     MVI   MSG,22              REPORT &T HAS BEEN SPOOLED                   
         MVC   TEMP(3),PRTID                                                    
         MVI   TEMP+3,C','                                                      
         EDIT  (B2,PRTREPNO),(5,TEMP+4),ALIGN=LEFT                              
         LA    RE,TEMP                                                          
         ST    RE,TXT                                                           
         AH    R0,=H'4'                                                         
         STC   R0,TXT                                                           
         XC    WORK,WORK                                                        
         LA    RE,BCLMSGH                                                       
         ST    RE,FLD                                                           
         BAS   RE,GTXT                                                          
*                                                                               
LBCF     BAS   RE,WRITESTR         WRITE SAVE STORAGE                           
*                                                                               
LBCX     B     XMOD                                                             
*                                                                               
LBCERR   L     R4,ERRCUR                                                        
         B     ERR3                                                             
         EJECT                                                                  
*************************************************************                   
*        VALIDATE SELECT FIELDS                             *                   
*************************************************************                   
         SPACE 1                                                                
VALSEL   NTR1                                                                   
         LA    R4,BCLS1H           POINT TO SEL FIELD                           
         ST    R4,CURSOR                                                        
         LA    R6,SAVTAB           POINT TO SEL TABLE                           
*                                                                               
VALS010  CLI   0(R6),0             CLEAR ACTION BYTE                            
         BNE   VALS040A                                                         
         OC    0(L'SAVTAB,R6),0(R6)                                             
         BZ    VALS090             IGNORE EMPTY ENTRIES                         
         SR    R1,R1                                                            
         ICM   R1,1,5(R4)          GET LEN INTO R1                              
         BZ    VALS090                                                          
         BCTR  R1,0                                                             
*                                                                               
         LA    RE,ACTTABL                                                       
VALS020  EX    0,0(RE)             RF=A(KEYWORD)                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),0(RF)       TEST KEYWORD                                 
         BE    VALS040                                                          
         LA    RE,8(RE)            NEXT SUBACTION                               
         CLI   0(RE),0                                                          
         BNE   VALS020                                                          
         ST    R4,ERRCUR                                                        
         MVI   ERROR,3             INVALID SUB ACTION                           
         B     XIT                                                              
*                                                                               
VALS040  MVC   0(1,R6),4(RE)       SAVE ACTION NUMBER                           
VALS040A OI    DDS,DDSSEL          SET SELECT INPUT FOUND                       
*                                                                               
VALS090  LA    R4,94(R4)                                                        
         LA    R6,L'SAVTAB(R6)                                                  
         LA    R1,BCLTABH          TEST FOR END OF SCREEN                       
         CR    R4,R1                                                            
         BL    VALS010             NEXT SEL FIELD                               
         B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
*        READ IN SAVED STORAGE                              *                   
*************************************************************                   
         SPACE 1                                                                
READSTR  NTR1                                                                   
         L     R5,SRPAR2                                                        
         USING SRSD,R5                                                          
         LA    R2,SRPAGENO         READ IN TWA11                                
         SLL   R2,32-8                                                          
         ICM   R2,3,MYTNUM                                                      
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R2),SRSD                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   SRCOMWRK(4),=C'$TXT'                                             
         BE    READX                                                            
         XC    SRCOMWRK(L'SAVAREA),SRCOMWRK XC SAVE STORAGE                     
READX    MVC   SAVAREA,SRCOMWRK                                                 
         B     XIT                                                              
         SPACE 1                                                                
*************************************************************                   
*        WRITE OUT SAVED STORAGE                            *                   
*************************************************************                   
         SPACE 1                                                                
WRITESTR NTR1                                                                   
         MVC   SAVMODE,MODE                                                     
         L     R5,SRPAR2                                                        
         USING SRSD,R5                                                          
         MVC   IDENT,=C'$TXT'                                                   
         MVC   SRCOMWRK(L'SAVAREA),SAVAREA                                      
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,MYTNUM                                                      
         GOTO1 ADATAMGR,DMCB,DMWRT,TEMPSTR,(R2),SRSD                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* SUBROUTINE TO READ TWA#0 AND INITIALISE IF FIRST TIME TODAY                   
*                                                                               
TWAREAD  NTR1                                                                   
         MVI   TWA0UPDT,0          SET TWA0 NOT UPDATED                         
         L     R8,ATWAMSG                                                       
         MVC   DMCB+8(2),=X'0000'  READ IN TWA0 INTO ATWAMSG                    
         MVC   DMCB+10(2),TERMNUM                                               
         MVC   DMCB+20(2),=C'L='   SPECIAL PARM FOR TWA0 READ/WRITES            
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADATAMGR,DMCB,DMREAD,TEMPSTR,,(R8)                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         AH    R8,CHKDSP                                                        
         USING CHKPTD,R8           R8=A(TWA0 CHECKPOINT AREA)                   
*                                                                               
TWAR1    MVC   DUB(4),CHBCDATE     GET DATE FROM CHECKPOINT RECORD              
         OI    DUB+3,X'0F'                                                      
         CLC   DUB(4),MVSDATE      OK IF ITS TODAY                              
         BE    TWARX                                                            
         L     RE,DUB              MAKE SURE CHECKPOINT DATE VALID              
         SRL   RE,4                                                             
         LA    R0,7                                                             
TWAR1A   SRDL  RE,4                                                             
         SRL   RF,28                                                            
         CH    RF,=H'9'                                                         
         BH    TWAR3                                                            
         BCT   R0,TWAR1A                                                        
         SP    DUB(4),=P'1'                                                     
         CP    DUB(4),MVSDATE      MAY BE OK IF ITS YESTERDAYS                  
         BNE   TWAR3                                                            
         L     R0,MVSTIME                                                       
         A     R0,=A(24*60*60*100) ADJUST FOR MIDNIGHT                          
         ICM   RF,15,CHBCTIME                                                   
         SR    R0,RF                                                            
         C     R0,=A(07*60*60*100) TEST IF OLDER THAT 7 HOURS                   
         BNL   TWAR3               YES ASSUME INITIALISE                        
*                                                                               
TWAR2    L     R5,ABCTAB           R5=A(BRDCST TABLE)                           
         USING BCTABD,R5                                                        
         LA    R5,6(R5)                                                         
         CLC   CHBCCNUM,BCTLCNUM   TEST LAST TRM WITH LAST BCTAB                
         BNH   TWARX                                                            
*                                                                               
TWAR3    XC    CHBCDATA,CHBCDATA   INITIALISE BRDCST INFO FOR THIS TRM          
         MVC   CHBCDATE,MVSDATE                                                 
         MVC   CHBCTIME,MVSTIME                                                 
         OI    TWA0UPDT,X'01'      SET TWA0 UPDATED - INITIALISED               
*                                                                               
TWARX    CLI   TWA0UPDT,0          EXIT WITH CC NEQ IF TWA#0 INIT               
         XIT1                                                                   
         EJECT                                                                  
SYSMSG   ST    RE,SAVERE                                                        
         LA    R5,BCDUMMY          CREATE DUMMY BC TABLE ENTRY                  
         SR    RF,RF                                                            
         IC    RF,TBRSNUM                                                       
         BCTR  RF,0                                                             
         SLL   RF,1                RF=(BROADCAST NUM-1)*2                       
         CLI   SVLANG,7                                                         
         BNH   *+6                                                              
         DC    H'0'                ENGLISH -> DUTCH ONLY                        
         SR    R1,R1                                                            
         IC    R1,SVLANG           INDEX USING LANGUAGE                         
         SLL   R1,2                                                             
         EX    0,*+8(R1)                                                        
         B     SYSM1                                                            
         LA    R1,SYSBENG(RF)      R1 = A(MESSAGE NUMBER)                       
         LA    R1,SYSBENG(RF)                                                   
         LA    R1,SYSBENG(RF)                                                   
         LA    R1,SYSBGER(RF)                                                   
         LA    R1,SYSBFRE(RF)                                                   
         LA    R1,SYSBSPA(RF)                                                   
         LA    R1,SYSBITA(RF)                                                   
         LA    R1,SYSBDUT(RF)                                                   
SYSM1    L     R7,AIOA1                                                         
         USING BRDKEYD,R7                                                       
         XC    BRDKEY,BRDKEY                                                    
         MVI   BRDKSTYP,BRDKSTYQ                                                
         MVI   BRDKTYPE,BRDKPERQ                                                
         MVC   BRDKMSGN,0(R1)                                                   
         GOTO1 ADATAMGR,DMCB,DMREAD,GENDIR,(R7),(R7),0                          
         CLI   8(R1),0                                                          
         BNE   ERR4                                                             
*                                                                               
SYSM2    MVC   BCTDADR,BRDDA                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        SEND BROADCAST PANEL TO PRINT QUEUE                *                   
*************************************************************                   
         SPACE 1                                                                
BCPRINT  NTR1                                                                   
         GOTO1 ASWITCH,DUB,X'0AFFFFFF',0                                        
         CLI   4(R1),0                                                          
         BNE   PBCN                MESSAGES NOT AVAILABLE                       
         L     R7,AIOA1            R7=A(BRDCST RECORD)                          
         USING BRDKEYD,R7                                                       
         GOTO1 ADATAMGR,DMCB,GETREC,GENFIL,DADDR,(R7),DMWORK                    
         GOTO1 ASWITCH,DUB,X'01FFFFFF',0                                        
         CLI   4(R1),0                                                          
         BNE   PBCN                MESSAGES NOT AVAILABLE                       
         CLI   DMCB+8,0                                                         
         BNE   PBCM                MESSAGE NO LONGER APPLICABLE                 
         TM    BRDSTAT,X'80'       DONT SENT DELETED RECORDS                    
         BNZ   PBCM                MESSAGE NO LONGER APPLICABLE                 
         MVC   SAVNUM,BRDKMSGN     SAVE MESSAGE DISK NUMBER                     
*                                                                               
         MVC   WORK,SPACES                                                      
         LA    R7,BRDFSTEL         R7=A(BRDCST RECORD ELEMENT)                  
BCP001   CLI   0(R7),BRDFLTCQ                                                   
         BE    BCP010                                                           
         CLI   0(R7),0                                                          
         BE    BCP020                                                           
         SR    R1,R1               BUMP TO NEXT BRDCST RECORD ELEMENT           
         IC    R1,1(R7)                                                         
         AR    R7,R1                                                            
         B     BCP001                                                           
*                                                                               
BCP010   MVC   WORK(8),BRDFNAME-BRDFLTD(R7)                                     
*                                                                               
BCP020   TM    MODE,MODEPQI        TEST INITIALISED                             
         BO    BCP030                                                           
         BAS   RE,PQOPEN                                                        
         OI    MODE,MODEPQI                                                     
*                                                                               
BCP030   MVC   PLINE,SPACES                                                     
         MVI   DUMDLC,X'89'        NEW PAGE                                     
         BAS   RE,PQPRINT                                                       
         MVI   DUMDLC,X'09'                                                     
         MVC   PLINE+29(20),SR@SYSBR                                            
         BAS   RE,PQPRINT                                                       
         MVC   PLINE+29(20),DASHES                                              
         BAS   RE,PQPRINT                                                       
         MVC   PLINE,SPACES                                                     
         BAS   RE,PQPRINT                                                       
         MVC   PLINE,SPACES                                                     
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF+1,MSGNUM                                                    
         CLI   MSGNUM,0                                                         
         BNE   *+10                                                             
         MVC   HALF,SAVNUM                                                      
         EDIT  (B2,HALF),(5,DUB1),ALIGN=LEFT                                    
         LA    RE,DUB1                                                          
         ST    RE,TXT                                                           
         STC   R0,TXT                                                           
         LA    RE,21                                                            
         GOTO1 AGETTXT,DMCB,(RE),(80,PLINE),(C'T',0),TXT,(X'08',0)              
         BAS   RE,PQPRINT                                                       
         MVC   PLINE,SPACES                                                     
         BAS   RE,PQPRINT                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,9                                                           
         GOTO1 ADATCON,DMCB,(5,0),(17,WORK+1)                                   
         MVI   WORK+9,9                                                         
         GOTO1 ATIMEOUT,DMCB,(1,MVSTIME),(X'46',WORK+10)                        
         LA    RE,23                                                            
         GOTO1 AGETTXT,DMCB,(RE),(80,PLINE),(C'T',0),0,(X'08',WORK)             
         BAS   RE,PQPRINT                                                       
         MVC   PLINE,SPACES                                                     
         BAS   RE,PQPRINT                                                       
*                                                                               
         L     R7,AIOA1            R7=A(BRDCST RECORD)                          
         LA    R7,BRDFSTEL         R7=A(BRDCST RECORD ELEMENT)                  
         LA    R0,16                                                            
PBCC1    CLI   0(R7),BRDHEDEQ      MSG HEADING ELEMENT                          
         BNE   PBCC2                                                            
         MVC   PLINE(2),=C'* '                                                  
         MVC   PLINE+2(74),PLINE                                                
         LA    RF,75               POSITION TO CENTER                           
         SR    RE,RE                                                            
         ICM   RE,1,BRDHEDTL-BRDHEDD(R7)                                        
         BZ    PBCD                                                             
         SR    RF,RE                                                            
         SRL   RF,1                                                             
         O     RF,=F'1'            BUT ALWAYS ODD                               
         LA    R1,PLINE                                                         
         LA    R1,1(R1,RF)                                                      
         BCTR  RE,0                DEC FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),BRDHEDTX-BRDHEDD(R7)                                     
         B     PBCD                                                             
*                                                                               
PBCC2    CLI   0(R7),BRDTXTEQ      MSG TEXT ELEMENT                             
         BE    PBCE                                                             
         CLI   0(R7),0             END OF TEXT                                  
         BE    PBCE                                                             
         SR    R1,R1               BUMP TO NEXT ELEMENT                         
         IC    R1,1(R7)                                                         
         AR    R7,R1                                                            
         B     PBCC1                                                            
*                                                                               
PBCE     CH    R0,=H'1'            LAST LINE IS * * * * *                       
         BNE   PBCE01                                                           
         MVC   PLINE(2),=C'* '                                                  
         MVC   PLINE+2(74),PLINE                                                
         B     PBCD                                                             
*                                                                               
PBCE01   MVI   PLINE,C'*'          MOVE TEXT TO PRINT LINE                      
         MVI   PLINE+74,C'*'                                                    
         CLI   0(R7),0                                                          
         BE    PBCD                                                             
         SR    RF,RF                                                            
         IC    RF,1(R7)                                                         
         LA    RE,BRDTXTTX-BRDTXTD                                              
         SR    RF,RE                                                            
         BNP   PBCD                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PLINE+2(0),BRDTXTTX-BRDTXTD(R7)                                  
*                                                                               
PBCD     BAS   RE,PQPRINT                                                       
         MVC   PLINE,SPACES                                                     
         SR    R1,R1               BUMP TO NEXT BRDCST RECORD ELEMENT           
         IC    R1,1(R7)                                                         
         CLI   0(R7),0             TEST FOR END OF RECORD                       
         BE    *+6                                                              
         AR    R7,R1                                                            
         BCT   R0,PBCC1                                                         
         B     XIT                                                              
*                                                                               
PBCM     EQU   *                                                                
PBCN     B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
*        PQ OPEN & CLOSE AND PRINT                          *                   
*************************************************************                   
         SPACE 1                                                                
PQCLOSE  NTR1                                                                   
         LA    R4,DUMDLC                                                        
         MVI   DUMDLC,X'FF'        CLOSE REPORT                                 
         XC    DUMDL,DUMDL                                                      
         GOTO1 ADATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,(R4),APQIO                
         B     XIT                                                              
*                                                                               
PQOPEN   NTR1                                                                   
         OC    PRTUSER,PRTUSER                                                  
         BZ    ERR5                                                             
         XC    DUMDLH,DUMDLH       NO SO DO IT                                  
         MVI   DUMDLH,8+79                                                      
         MVI   DUMDLC,0                                                         
         XC    DUMDL,DUMDL         INITIALISE DUMMY DISPLAY LINE                
         XC    DUMDLX,DUMDLX                                                    
         LA    R4,DUMDLC                                                        
         USING PQPLD,R4                                                         
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLSRCID,PRTUSER                                                  
         MVC   QLSUBID,PRTID                                                    
         MVC   QLCLASS,PRTCLASS                                                 
         MVC   QLDESC+0(4),=C'$RB/'                                             
         MVC   QLDESC+4(7),WORK                                                 
         MVC   QLMAKER,=C'DSR  '                                                
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,80                                                       
         MVC   QLRETNL,=H'48'                                                   
         MVC   QLRETND,=H'24'                                                   
         GOTO1 ADATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,(R4),APQIO                
         CLI   8(R1),0                                                          
         BNE   ERR10                                                            
         MVC   PRTREPNO,QLREPRNO   SAVE RETURNED REPORT NUMBER                  
         MVC   PRTID,QLSUBID       SAVE RETURNED REPORT ID                      
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 2                                                                
PQPRINT  NTR1                                                                   
         MVC   DUMDL,PLINE                                                      
         GOTO1 ADATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,DUMDLC,APQIO              
         CLI   8(R1),0                                                          
         BNE   ERR10                                                            
         B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
*        ERROR AND INFO EXITS                               *                   
*************************************************************                   
         SPACE 1                                                                
ERR0     LA    RE,SREMIF           MISSING INPUT FIELD                          
         B     ERRX                                                             
ERR1     LA    RE,SREIIF           INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    RE,SREACTNA         ACTION NOT AVAILABLE                         
         B     ERRX                                                             
ERR3     LA    RE,57               INVALID SUB ACTION                           
         B     ERRX                                                             
ERR4     LA    RE,200              NO MESSAGES FOUND                            
         B     ERRX                                                             
ERR5     LA    RE,30               MUST BE CONNECTED                            
         B     ERRX                                                             
ERR10    LA    RE,186              DISK ERROR ON PQ                             
         B     ERRX                                                             
         SPACE 1                                                                
ERRX     L     RD,BASERD                                                        
         ST    R4,CURSOR                                                        
         TM    MODE,MODEMNTR       ANY ERROR IN MONITOR MODE                    
         BNO   ERRXX                                                            
         MVC   BCRSRV,SRVRE        SET =RE                                      
*                                                                               
ERRXX    GOTO1 AGETTXT,DMCB,(RE),0,(C'E',0),0,0,X'000100'                       
         B     XMOD                                                             
         SPACE 1                                                                
GTXT     ST    RE,SAVERE                                                        
         ZIC   RE,MSG                                                           
         GOTO1 AGETTXT,DMCB,(RE),FLD,(C'T',0),TXT,WORK,X'000100'                
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
OKX      TM    MODE,MODEMNTR       MONITOR MODE                                 
         BNO   OKXX                                                             
         TM    DDS,DDSLAST         WAS THIS THE LAST MESSAGE                    
         BNO   OKXX                                                             
         MVC   BCRSRV,SRVRE        SET =RE                                      
*                                                                               
OKXX     GOTO1 AGETTXT,DMCB,(RE),0,(C'I',0),0,WORK,X'000100'                    
*                                                                               
XMOD     TM    MODE,MODEPQI        TEST PQ INITIALISED                          
         BNO   *+8                                                              
         BAS   RE,PQCLOSE                                                       
         L     R4,CURSOR           SET CURSOR FLAG                              
         OI    6(R4),X'40'                                                      
         XMOD1 1                   AND EXIT                                     
*                                                                               
XIT      XIT1                      XIT POINT                                    
         EJECT                                                                  
BLANKS   DC    80C' '                                                           
DOTS     DC    80C'.'                                                           
DASHES   DC    40C'-'                                                           
BITMASK  DC    X'8040201008040201'                                              
DEFCTRY  DS    0X                                                               
*&&UK*&& DC    X'01'                                                            
*&&US*&& DC    X'02'                                                            
GETREC   DC    CL8'GETREC'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
GENDIR   DC    CL8'GENDIR'                                                      
GENFIL   DC    CL8'GENFIL'                                                      
TEMPSTR  DC    CL8'TEMPSTR'                                                     
SPACES   DC    CL80' '                                                          
SRVRE    DC    CL17'=RE'                                                        
*                                                                               
DDDCLST  DS    0C                                                               
         DCDDL SR#1ST,8,L                                                       
         DCDDL SR#LAST,8,L                                                      
         DCDDL SR#OK,2,L                                                        
         DCDDL SR#YES,3,L                                                       
         DCDDL SR#SLECT,3,L                                                     
         DCDDL SR#PRINT,3,L                                                     
         DCDDL SR#SYSBR,20,C                                                    
         SPACE 1                                                                
MODEDISP EQU   X'01'               NORMAL MODE CALLED BY =RB                    
MODEMNTR EQU   X'02'               CALLED BY MONITOR TO DO RECALL               
MODELIST EQU   X'04'               NORMAL MODE CALLED BY =LB                    
MODEAUTO EQU   X'08'               AUTO CALLED BY SRTIM00 FOR DISK SB           
MODESEL  EQU   X'10'               SELECT MODE                                  
MODEPQI  EQU   X'20'               PQ INITIALISED FOR PRINTING                  
         LTORG                                                                  
         EJECT                                                                  
*                1     2     3    4                                             
*&&UK                                                                           
SYSBENG  DC    H'315',H'316',H'0',H'0'                                          
SYSBGER  DC    H'031',H'035',H'0',H'0'                                          
SYSBFRE  DC    H'315',H'316',H'0',H'0'                                          
SYSBSPA  DC    H'315',H'316',H'0',H'0'                                          
SYSBITA  DC    H'315',H'316',H'0',H'0'                                          
SYSBDUT  DC    H'315',H'316',H'0',H'0'                                          
*&&                                                                             
*&&US                                                                           
SYSBENG  DC    H'00',H'00',H'0',H'0'                                            
SYSBGER  DC    H'00',H'00',H'0',H'0'                                            
SYSBFRE  DC    H'00',H'00',H'0',H'0'                                            
SYSBSPA  DC    H'00',H'00',H'0',H'0'                                            
SYSBITA  DC    H'00',H'00',H'0',H'0'                                            
SYSBDUT  DC    H'00',H'00',H'0',H'0'                                            
*&&                                                                             
ACTTABL  DC    X'41F0',S(SR@SLECT),X'01',AL3(0)                                 
         DC    X'41F0',S(SR@PRINT),X'02',AL3(0)                                 
         DC    X'00'                                                            
*                                                                               
PQTESTL  DC    CL79'TEST PQ BROADCAST DATA '                                    
         EJECT                                                                  
* DSECT TO COVER W/S                                                            
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
PFKEY    DS    X                                                                
DDS      DS    X                                                                
DDSBCT   EQU   X'80'               DISPLAY BCTAB                                
DDSSEL   EQU   X'40'               SEL INPUT FOUND                              
DDSLAST  EQU   X'20'               LAST MESSAGE (SET =RE)                       
DDSTRM   EQU   X'01'               DDS TERMINAL                                 
*                                                                               
DMCB     DS    6F                                                               
RELO     DS    A                                                                
BASERD   DS    A                                                                
FLD      DS    A                                                                
TXT      DS    A                                                                
SAVERE   DS    A                                                                
SVR567   DS    3A                                                               
CURSOR   DS    A                                                                
DADDR    DS    XL4                                                              
SAVNUM   DS    XL2                                                              
ERROR    DS    X                                                                
ERRCUR   DS    A                                                                
*                                                                               
         DS    0F                                                               
SRPARS   DS    0CL32                                                            
SRPAR1   DS    F                   A(SYSFAC)                                    
SRPAR2   DS    F                   A(TIA)                                       
SRPAR3   DS    F                   A(UTL)                                       
SRPAR4   DS    F                   A(COMFACS)                                   
SRPAR5   DS    F                   A(SELIST)                                    
SRPAR6   DS    F                   A(TWA)                                       
SRPAR7   DS    F                   A(PHASE MAP)                                 
SRPAR8   DS    F                   A(TIOB)                                      
*                                                                               
ATWAMSG  DS    A                   A(AREA FOR TWA5)                             
AIOA1    DS    A                   A(IOAREA1 FOR CONTROL FILE)                  
APQIO    DS    A                   A(IOAREA FOR PQ)                             
*                                                                               
AMSGEL   DS    A                                                                
ASYSTAB  DS    V                                                                
ACTRYTAB DS    V                                                                
ADATAMGR DS    V                                                                
ADICTATE DS    V                                                                
ATERMVAL DS    V                                                                
ACALLOV  DS    V                                                                
ADATVAL  DS    V                                                                
AGETTXT  DS    V                                                                
ADATCON  DS    V                                                                
AHELLO   DS    V                                                                
ASWITCH  DS    V                                                                
ATIMEOUT DS    V                                                                
ABCTAB   DS    V                                                                
AUTL     DS    V                                                                
ABROAD   DS    V                                                                
*                                                                               
RECLEN   DS    H                                                                
CHKDSP   DS    H                                                                
*                                                                               
MVSTIME  DS    F                                                                
MVSDATE  DS    F                                                                
BCADDR   DS    A                                                                
TWA0UPDT DS    X                                                                
BCMASK   DS    X                                                                
         DS    2X                                                               
*                                                                               
BCBYTE   DS    H                                                                
BCBIT    DS    H                                                                
BCPNUM   DS    X                                                                
BCTABA   DS    AL3                                                              
*                                                                               
BCCNTS   DS    0C                                                               
BCFSTLST DS    H                                                                
BCLSTLST DS    H                                                                
BCACNT   DS    H                                                                
BCPCNT   DS    H                                                                
BCSCNT   DS    H                                                                
BCNUMS   DS    XL128                                                            
BCCNTSL  EQU   *-BCCNTS                                                         
*                                                                               
BCLIST   DS    0C                  ORDER THIS LIST IN REQUIRED SEQ              
BCFTIME  DS    XL8                                                              
BCLTIME  DS    XL8                                                              
BCFURGNT DS    XL8                                                              
BCLURGNT DS    XL8                                                              
BCFSYS   DS    XL8                                                              
BCLSYS   DS    XL8                                                              
BCLISTX  DS    XL8                                                              
BCLISTL  EQU   BCLISTX-BCLIST                                                   
*                                                                               
MODE     DS    X                                                                
MSG      DS    X                                                                
TBRSNUM  DS    X                                                                
SVLANG   DS    X                                                                
SYSID    DS    X                                                                
TERMNUM  DS    H                                                                
MYTNUM   DS    H                                                                
MSGNUM   DS    C                                                                
TODAY    DS    CL8                                                              
TODAYB   DS    CL3                                                              
SELCOPY  DS    CL6                                                              
*                                                                               
PRTUSER  DS    XL2                 PRT USER ID NUM                              
PRTID    DS    CL3                 PRT REPORT ID                                
PRTCLASS DS    C                   PRT CLASS                                    
PRTREPNO DS    XL2                 PRT REPORT NUMBER ASSIGNED                   
         DS    CL4                 N/D                                          
*                                                                               
BCDUMMY  DS    CL(BCTABL)                                                       
*                                                                               
DUMDLH   DS    XL7                                                              
DUMDLC   DS    XL1                 DUMMY DISPLAY LINE CC CHR                    
DUMDL    DS    CL79                DUMMY DISPLAY LINE                           
DUMDLX   DS    CL79                                                             
*                                                                               
PLINE    DS    CL79                                                             
WORK     DS    CL80                                                             
TEMP     DS    CL80                                                             
DMWORK   DS    XL80                                                             
TEXXH    DS    XL8                                                              
TEXX     DS    CL80                                                             
         ORG   DMWORK                                                           
BCMSG    DS    CL256                                                            
*                                                                               
SAVAREA  DS    0CL148              AREA TO BE SAVED                             
IDENT    DS    CL4                                                              
SAVSRV   DS    CL17                                                             
SAVP1    DS    CL24                                                             
SAVTAB   DS    16XL6                                                            
SAVMODE  DS    X                                                                
SAVCOUNT DS    X                                                                
SAVSPARE DS    CL12                                                             
*                                                                               
DDDSLST  DS    0C                                                               
         DSDDL PRINT=YES                                                        
*                                                                               
         DS    0D                                                               
IOA1     DS    4096C                                                            
TWAIOA   DS    14336C                                                           
PQIOA    DS    14336C                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DSECT TO COVER BROADCAST LIST LINE                                            
*                                                                               
LSTD     DSECT                                                                  
LSTSELH  DS    XL8                 SELECT HEADER                                
LSTSEL   DS    CL3                 SELECT DATA                                  
LSTDATAH DS    XL8                 DISPLAY HEADER                               
*                                                                               
LSTDATA  DS    0CL75               DISPLAY DATA (NORMAL DISPLAY)                
LSTNUM   DS    CL3                 NUMBER                                       
         DS    C                                                                
LSTSTAT  DS    CL2                 STATUS                                       
         DS    C                                                                
LSTMHDG  DS    CL26                MESSAGE HEADING                              
         DS    C                                                                
LSTMTXT  DS    CL40                MESSAGE TEXT                                 
         DS    C                                                                
*                                                                               
         ORG   LSTDATA                                                          
LSTDATA1 DS    0CL75               DISPLAY DATA ($LB,ALL FORMAT)                
LS1NUM   DS    CL3                 NUMBER                                       
         DS    C                                                                
LS1STAT  DS    CL2                 STATUS                                       
         DS    C                                                                
LS1DNUM  DS    CL4                 DISK NUMBER                                  
         DS    C                                                                
LS1NAME  DS    CL8                 MESSAGE NAME                                 
         DS    C                                                                
LS1CTRY  DS    CL3                 COUNTRY                                      
         DS    C                                                                
LS1SYS   DS    CL4                 SYSTEM                                       
         DS    C                                                                
LS1PRG   DS    CL3                 PROGRAM                                      
         DS    C                                                                
LS1LUID  DS    CL8                 TERMINAL LUID                                
         DS    C                                                                
LS1TIME  DS    CL11                TIMES                                        
         DS    C                                                                
LS1MHDG  DS    CL18                MESSAGE HEADING                              
         DS    C                                                                
         SPACE 2                                                                
* DSECT TO COVER MESSAGE ELEMENT                                                
*                                                                               
MSGD     DSECT                                                                  
MSGEL    DS    CL1                 MESSAGE NUMBER                               
MSGLEN   DS    CL1                 ELEMENT LENGTH                               
MSGTYPE  DS    CL1                 MESSAGE TYPE ('B' OR 'M')                    
MSGSRCE  DS    CL2                 SOURCE TERMINAL                              
MSGTIME  DS    CL4                 TIME MESSAGE SENT                            
MSGMESS  DS    0CL245              MESSAGE TEXT                                 
         EJECT                                                                  
* FABCTAB                                                                       
* CTGENBRD                                                                      
* DDCOMFACS                                                                     
* DMPRTQL                                                                       
* SRDDEQUS                                                                      
* FASYSLSTD                                                                     
* FAFACTS                                                                       
* FACHKPT                                                                       
* SRERREQUS                                                                     
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FABCTAB                                                        
       ++INCLUDE CTGENBRD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE SRDDEQUS                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FACHKPT                                                        
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SRTXTFFD DSECT                                                                  
         DS    CL64                                                             
* SRTXTFFD                                                                      
       ++INCLUDE SRTXTFFD                                                       
         ORG   SRTXTFFD+64                                                      
* SRTXTFED                                                                      
       ++INCLUDE SRTXTFED                                                       
         SPACE 1                                                                
HELEN    CSECT                                                                  
         DC    AL1(5),C'MSGTAB  ',AL2(6100),AL1(6),AL1(4)                       
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SRTXT00   01/12/04'                                      
         END                                                                    
