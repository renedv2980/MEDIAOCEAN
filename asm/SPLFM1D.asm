*          DATA SET SPLFM1D    AT LEVEL 041 AS OF 05/01/02                      
*PHASE T2191DA                                                                  
         TITLE 'T2191D - COMMENT HEADER RECORD'                                 
T2191D   CSECT                                                                  
         NMOD1 0,T2191D                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING COMHDRD,R8                                                       
         CLI   SVFMTSW,0                                                        
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
FMT0     MVI   BYTE2,X'00'         CLEAR SWITCHES                               
         LA    R2,COMPROFH                                                      
         LA    R3,8(R2)                                                         
         MVC   0(30,R3),SPACES                                                  
         MVI   BYTE,0                                                           
         TM    COMPROF1,X'80'      PAGE=ALL BIT                                 
         BZ    FMT0A                                                            
         MVC   0(8,R3),=C'PAGE=ALL'                                             
         LA    R3,8(R3)            ADVANCE FOR FOLLOWING OPTIONS                
         OI    BYTE,X'01'                                                       
* ANY OTHER OPTIONS WOULD TEST BYTE AND INSERT COMMA IF ON                      
*                                                                               
FMT0A    OI    BYTE2,X'80'         ON END OF SCREEN GO TO FMT4                  
         FOUT  (R2)                                                             
         GOTO1 VDATCON,DMCB,(3,COMCREAT),(5,DUB)                                
         FOUT  COMCDTEH,DUB,8                                                   
         GOTO1 VDATCON,DMCB,(3,COMACTIV),(5,DUB)                                
         FOUT  COMADTEH,DUB,8                                                   
         LA    R2,COML01H                                                       
         LA    R6,REC+24                                                        
         MVI   ELCODE,X'05'        PRINT=TOP ELEMENTS FIRST                     
         BAS   R9,NEXTEL2                                                       
         BNE   FMT3                                                             
         CLI   0(R6),X'05'                                                      
         BNE   FMT2                                                             
         LR    R7,R6               CHECK AHEAD FOR ANY 15 ELEMENTS              
FMT0B    ZIC   R3,1(R7)                                                         
         LTR   R3,R3               SAFETY CATCH                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R7,R3                                                            
         CLI   0(R7),X'00'         AT LEAST ONE 15 -                            
         BE    FMT1B               PRINT=TOP SHOWS                              
         CLI   0(R7),X'15'                                                      
         BNE   FMT0B                                                            
         XC    8(70,R2),8(R2)      CLEAR OUTPUT AREA                            
         FOUT  (R2),=C'PRINT=TOP',9                                             
         OI    4(R2),II1C                                                       
FMT1A    BAS   R9,ADVANCE          MOVE SCREEN POINTER                          
FMT1B    ZIC   R3,1(R6)            USE LEN OF ELEM - 3                          
         XC    8(70,R2),8(R2)      CLEAR OUTPUT AREA                            
         LA    R4,2                                                             
         SR    R3,R4                                                            
         FOUT  (R2),2(R6),(R3)                                                  
         OI    4(R2),II1C                                                       
         BAS   R9,NEXTEL                                                        
         BNE   FMT3                                                             
         CLI   0(R6),X'05'                                                      
         BE    FMT1A               MORE PRINT=TOP                               
         BAS   R9,ADVANCE          MOVE SCREEN POINTER                          
*                                                                               
FMT2     MVI   ELCODE,X'15'                                                     
         XC    8(70,R2),8(R2)      CLEAR OUTPUT AREA                            
         FOUT  (R2),=C'PRINT=BOTTOM',12                                         
         OI    4(R2),II1C                                                       
FMT2A    BAS   R9,ADVANCE                                                       
         ZIC   R3,1(R6)                                                         
         XC    8(70,R2),8(R2)                                                   
         LA    R4,2                                                             
         SR    R3,R4                                                            
         FOUT  (R2),2(R6),(R3)                                                  
         OI    4(R2),II1C                                                       
         BAS   R9,NEXTEL                                                        
         BNE   FMT3                                                             
         CLI   0(R6),X'15'                                                      
         BE    FMT2A                                                            
*                                                                               
FMT3     BAS   R9,ADVANCE                                                       
         LA    R4,COML14H                                                       
         CR    R4,R2                                                            
         BL    FMT4                END OF COMMENT TEXT AREA                     
         XC    8(70,R2),8(R2)                                                   
         FOUT  (R2)                                                             
         B     FMT3                                                             
*                                                                               
FMT4     B     EXXMOD                                                           
         EJECT                                                                  
EDT      DS    0H                                                               
         MVI   ERRCD,INVERR                                                     
         MVI   BYTE2,X'00'         EXIT TO OUTPUT ON                            
         MVI   HALF,0              SET LINE COUNT TO ZERO                       
         CLI   SVACT,C'A'                                                       
         BE    EDT0                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         B     EDT1                                                             
*                                                                               
EDT0     MVC   COMLEN,=AL2(36)                                                  
         MVC   COMEL(2),=X'010C'   SET UP NEW RECORD                            
         GOTO1 VDATCON,DMCB,(5,0),(3,COMCREAT)                                  
*                                  CREATION DATE - TODAY                        
*                                                                               
EDT1     LA    R2,COMPROFH                                                      
         CLI   5(R2),0                                                          
         BE    EDT1A                                                            
         GOTO1 VSCANNER,DMCB,(R2),REC2                                          
         LA    R3,REC2                                                          
EDTPROF  CLI   0(R3),0             ANYTHING THERE                               
         BE    EDT1A                                                            
         CLC   12(10,R3),=CL10'PAGE'                                            
         BE    EDTPAGE                                                          
* INSERT FURTHER OPTIONS HERE                                                   
         B     LFMERR                                                           
*                                                                               
EDTPAGE  CLC   22(10,R3),=CL10'FIRST'                                           
         BNE   *+12                                                             
         NI    COMPROF1,X'7F'                                                   
         B     EDT1A                                                            
         CLC   22(10,R3),=CL10'ALL'                                             
         BNE   LFMERR                                                           
         OI    COMPROF1,X'80'                                                   
*                                                                               
EDT1A    GOTO1 VDATCON,DMCB,(5,0),(3,COMACTIV)                                  
         CLC   COML01(7),=C'&&DELETE'                                           
         BE    DELREC              DELETE ENTIRE COMMENT                        
         LA    R2,COML01H                                                       
         LA    R6,REC+36                                                        
         CLI   SVACT,C'A'          ADDS GO ELSEWHERE                            
         BE    EDT4                                                             
         MVI   ELCODE,X'05'        PRINT=TOP FIRST                              
         BAS   R9,NEXTEL2          GET FIRST COMMENT ELEMENT                    
         BNE   EDT4                END OF ELEMENTS                              
         CLI   0(R6),X'05'         IF NOT TOP ELEMENT - DO BOTTOM               
         BNE   EDT3                                                             
         LR    R7,R6               CHECK AHEAD FOR 15 ELEMENTS                  
EDT1B    ZIC   R3,1(R7)                                                         
         LTR   R3,R3               SAFETY CATCH                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R7,R3                                                            
         CLI   0(R7),X'15'         IF AT LEAST ONE 15 -                         
         BE    EDT1C               PRINT=TOP ALREADY THERE                      
         CLI   0(R7),X'00'                                                      
         BNE   EDT1B                                                            
         OI    BYTE2,X'01'         NO PRINT=TOP SHOWING                         
         B     EDT2A                                                            
EDT1C    CLC   8(2,R2),=C'&&+'     INSERT LINES AFTER                           
         BE    INSERTA                                                          
         CLC   8(2,R2),=C'&&-'      CANNOT DELETE PRINT= LINE                   
         BE    LFMERR                                                           
         CLC   8(9,R2),=C'PRINT=TOP' CANNOT CHANGE PRINT= LINE                  
         BNE   LFMERR                                                           
         BAS   R9,ADVANCE                                                       
EDT2A    CLC   8(2,R2),=C'&&+'      INSERT LINES AFTER                          
         BE    INSERT                                                           
         CLC   8(2,R2),=C'&&-'      DELETE THIS LINE                            
         BE    DELETE                                                           
         TM    4(R2),II1C          CHANGE THIS LINE                             
         BZ    CHANGE                                                           
         BAS   R9,ADVANCE                                                       
         BAS   R9,NEXTEL                                                        
         BNE   EDT4                                                             
         CLI   0(R6),X'05'                                                      
         BE    EDT2A                                                            
*                                                                               
EDT3     MVI   ELCODE,X'15'                                                     
         CLC   8(2,R2),=C'&&+'      INSERT LINES AFTER                          
         BE    INSERTA                                                          
         CLC   8(2,R2),=C'&&-'      CANNOT DELETE THIS LINE                     
         BE    LFMERR                                                           
         CLC   8(12,R2),=C'PRINT=BOTTOM' CANNOT CHANGE PRINT= LINE              
         BNE   LFMERR                                                           
         CLI   SVKEY+3,C'3'        DISSALLOW BOTTOM FOR A3 COMMENTS             
         BE    LFMERR                                                           
         BAS   R9,ADVANCE                                                       
EDT3A    CLC   8(2,R2),=C'&&+'      INSERT LINES AFTER                          
         BE    INSERT                                                           
         CLC   8(2,R2),=C'&&-'      DELETE THIS LINE                            
         BE    DELETE                                                           
         TM    4(R2),II1C          CHANGE THIS LINE                             
         BZ    CHANGE                                                           
         BAS   R9,ADVANCE                                                       
         BAS   R9,NEXTEL                                                        
         BNE   EDT4                                                             
         CLI   0(R6),X'15'                                                      
         BE    EDT3A                                                            
         DC    H'0'                                                             
*                                                                               
EDT4     MVC   REC2(70),8(R2)      END OF USED SCREEN                           
         OC    REC2(70),SPACES                                                  
         CLC   REC2(70),SPACES                                                  
         BE    OUTPUT                                                           
         CLC   8(6,R2),=C'PRINT='                                               
         BE    PRNTCHNG                                                         
         B     ADDLINE2                                                         
PRNTCHNG CLC   8(12,R2),=C'PRINT=BOTTOM'                                        
         BNE   CHNGTOP                                                          
         CLI   SVKEY+3,C'3'        DISSALLOW BOTTOM FOR A3 COMMENTS             
         BE    LFMERR                                                           
         CLI   ELCODE,X'15'                                                     
         BE    LFMERR                                                           
         MVI   ELCODE,X'15'                                                     
         BAS   R9,ADVANCE                                                       
         B     ADDLINE                                                          
CHNGTOP  CLC   8(9,R2),=C'PRINT=TOP'                                            
         BNE   LFMERR                                                           
         CLI   ELCODE,X'05'                                                     
         BE    LFMERR                                                           
         MVI   ELCODE,X'05'                                                     
         LA    R6,REC+24                                                        
         BAS   R9,NEXTEL2                                                       
         CLI   0(R6),X'05'                                                      
         BE    LFMERR                                                           
         NI    BYTE2,X'FE'         TURN OFF "NO PRINT=TOP"                      
         BAS   R9,ADVANCE                                                       
         B     ADDLINE                                                          
         EJECT                                                                  
INSERT   BAS   R9,NEXTEL                                                        
         BNE   EDT4                                                             
INSERTA  MVC   ELEM(3),BLANKEL                                                  
         MVC   ELEM(1),ELCODE      SET UP IMBEDDED BLANK LINES                  
         LA    R3,1                DEFAULT IS ONE BLANK LINE                    
         CLI   10(R2),C'1'                                                      
         BL    INSERTB                                                          
         CLI   10(R2),C'9'                                                      
         BH    INSERTB                                                          
         ZIC   R3,10(R2)                                                        
         N     R3,=X'0000000F'     NO. OF LINES TO INSERT                       
INSERTB  GOTO1 VRECUP,DMCB,(0,REC),ELEM,(R6)                                    
         BAS   R9,NEXTEL                                                        
         BCT   R3,INSERTB          NEXT INSERT LINE                             
         B     EDTEXIT                                                          
*                                                                               
DELETE   GOTO1 VRECUP,DMCB,(0,REC),(R6),(R6)                                    
         B     EDTEXIT                                                          
*                                                                               
ADDLINE  MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0             MUST BE NON-BLANK LINE HERE                  
         BE    LFMERR                                                           
ADDLINE2 CLI   ELCODE,X'00'        FIRST TIME THROUGH                           
         BNE   *+12                                                             
         OI    BYTE2,X'01'         SET "NO PRINT=TOP"                           
         MVI   ELCODE,X'05'        DEFAULT IS PRINT=TOP                         
         MVI   ERRCD,INVERR                                                     
         CLC   8(2,R2),=C'&&+'     NEW LINES CANNOT HAVE SPECIAL FORMAT         
         BE    LFMERR                                                           
         CLC   8(2,R2),=C'&&-'                                                  
         BE    LFMERR                                                           
         XC    ELEM,ELEM           NEW ELEMENT                                  
         CLC   8(6,R2),=C'PRINT='  NOT VALID AFTER PRINT=                       
         BE    LFMERR                                                           
         MVC   ELEM(1),ELCODE                                                   
         BAS   R9,SHRINK           TAKE OFF TRAILING BLANKS                     
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,MOVEELEM                                                      
         LA    R3,3(R3)                                                         
         STC   R3,ELEM+1                                                        
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,(R6)                                    
         BAS   R9,ADVANCE                                                       
         BAS   R9,NEXTEL                                                        
         B     EDT4                                                             
*                                                                               
CHANGE   CLC   8(6,R2),=C'PRINT='                                               
         BE    LFMERR              THIS TYPE OF CHANGE ILLEGAL                  
         BAS   R9,SHRINK           TAKE OFF TRAILING BLANKS                     
CHANGE1  GOTO1 VRECUP,DMCB,(0,REC),(R6),(R6)        DELETE OLD LINE             
         XC    ELEM,ELEM           OLD COPY OF COMMENT                          
         MVC   ELEM(1),ELCODE      CREATE NEW ONE                               
         ZIC   R3,5(R2)            GET LENGTH                                   
         BCTR  R3,0                                                             
         EX    R3,MOVEELEM                                                      
         LA    R3,3(R3)            ELEMENT LENGTH                               
         STC   R3,ELEM+1                                                        
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,(R6)    ADD NEW COMMENT                 
         BAS   R9,NEXTEL                                                        
         B     EDTEXIT                                                          
*                                                                               
MOVEELEM MVC   ELEM+2(0),8(R2)                                                  
         EJECT                                                                  
EDTEXIT  BAS   R9,ADVANCE                                                       
         BAS   R9,NEXTEL2                                                       
         BNE   EDT4                                                             
         CLI   0(R6),X'05'                                                      
         BE    EDT2A                                                            
         CLI   ELCODE,X'05'                                                     
         BE    EDT3                                                             
         CLI   ELCODE,X'15'                                                     
         BE    EDT3A                                                            
         DC    H'0'                                                             
*                                                                               
OUTPUT   MVI   ERRCD,TOOMANY                                                    
         LA    R6,REC+24                                                        
         LA    R8,0                                                             
COUNT1   BAS   R9,NEXTEL                                                        
         BNE   COUNT4                                                           
         CLI   0(R6),X'05'                                                      
         BNE   COUNT2                                                           
         LA    R8,1(R8)                                                         
         B     COUNT1                                                           
*                                                                               
COUNT2   LTR   R8,R8                                                            
         BZ    *+8                                                              
         LA    R8,1(R8)                                                         
         LA    R8,2(R8)                                                         
*                                                                               
COUNT3   BAS   R9,NEXTEL                                                        
         BNE   COUNT4                                                           
         CLI   0(R6),X'15'                                                      
         BNE   COUNT4                                                           
         LA    R8,1(R8)                                                         
         B     COUNT3                                                           
*                                                                               
COUNT4   MVI   ERRCD,BLNKCMT                                                    
         LTR   R8,R8                                                            
         BZ    LFMERR                                                           
         MVI   ERRCD,TOOMANY                                                    
         LA    R7,16                                                            
         CR    R8,R7                                                            
         BH    LFMERR                                                           
         MVC   KEY,SVKEY                                                        
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         CLI   SVACT,C'A'                                                       
         BNE   OUT1                                                             
*                                                                               
         MVC   REC(13),SVKEY                                                    
         MVI   DMINBTS,X'08'      WAS RECORD PREV DELETED                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),SVKEY                                                    
         BE    OUT0                YES - UNDELETE IT                            
         GOTO1 ADDREC                                                           
         B     REQ0                                                             
*                                                                               
OUT0     LA    RE,REC2             READ IN OLD REC                              
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         LA    RE,REC              WRITE OUT NEW ONE                            
         ST    RE,AREC                                                          
         GOTO1 PUTREC                                                           
         GOTO1 READ                                                             
         MVI   KEY+13,0            UNDELETE DIRECTORY ENTRY                     
         MVC   COMMAND,=C'DMWRT '                                               
         GOTO1 DIR                                                              
         B     REQ0                                                             
*                                                                               
OUT1     GOTO1 PUTREC                                                           
         B     REQ0                                                             
*                                                                               
DELREC   MVI   ERRCD,INVERR                                                     
         CLI   SVACT,C'A'          CANNOT DELETE AND ADD                        
         BE    LFMERR                                                           
         GOTO1 READ                                                             
         OI    KEY+13,X'80'        DELETE DIRECTORY ENTRY                       
         MVC   COMMAND,=C'DMWRT '                                               
         GOTO1 DIR                                                              
         FOUT  LFMMSGH,=CL30'RECORD HAS BEEN DELETED',30                        
         MVI   ERRAREA,1                                                        
         LA    R2,LFMRECH                                                       
         B     REQ0                                                             
         EJECT                                                                  
REQ0     XC    REC2(150),REC2                                                   
         LA    R1,REC2                                                          
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,REC2+26                                                       
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'L6'                                                   
         MVC   2(2,R1),AGYALPHA                                                 
         MVC   4(1,R1),SVEBCMED                                                 
         MVC   61(1,R1),SVKEY+3    COMMENT TYPE                                 
         MVC   5(3,R1),=C'ALL'     CLIENT                                       
         MVI   8(R1),C' '          PRODUCT GROUP                                
         MVC   11(3,R1),=C'ALL'    PRODUCT                                      
         MVC   23(3,R1),=C'ALL'    ESTIMATE                                     
         CLC   SVKEY+4(2),=X'0000' CLIENT SPECIFIED                             
         BE    REQ1                                                             
         MVC   5(3,R1),SVEBCCLT    YES                                          
         CLC   SVKEY+6,=X'000000'  PRODUCT OR GROUP SPECIFIED                   
         BE    REQ0B                                                            
         CLI   SVKEY+6,X'00'       PRODUCT NUMBER USED                          
         BE    REQ0A                                                            
         MVC   8(1,R1),SVKEY+6                                                  
         ZIC   R3,SVBKLNS                                                       
         BCTR  R3,0                                                             
         MVC   HALF,SVKEY+7                                                     
         OI    HALF+1,X'0F'                                                     
         UNPK  WORK(3),HALF                                                     
         MVC   11(3,R1),SPACES                                                  
         EX    R3,*+8              SHORTEN TO BREAK LEN                         
         B     REQ0B                                                            
         MVC   11(0,R1),WORK       ** EXECUTED **                               
REQ0A    MVC   11(3,R1),SVEBCPRD   YES                                          
REQ0B    CLI   SVKEY+9,X'00'       ESTIMATE SPECIFIED                           
         BE    REQ1                                                             
         ZIC   R3,SVKEY+9          YES                                          
         CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB+6(2)                                                
REQ1     MVI   65(R1),C'*'                                                      
         MVC   68(7,R1),=C'CONTROL'                                             
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC2,REC2                    
         B     FMT0                                                             
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         EJECT                                                                  
ADVANCE  ZIC   R3,0(R2)            ADVANCE SCREEN TO NEXT LINE                  
         AR    R2,R3                                                            
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         LA    R3,COML14H          END OF COMMENT TEXT                          
         CR    R2,R3                                                            
         BNHR  R9                                                               
         TM    BYTE2,X'80'         WHICH EXIT TO TAKE                           
         BO    FMT4                NO-WRITE EXIT                                
         B     OUTPUT              WRITE REC AND EXIT                           
*                                                                               
NEXTEL   CLI   0(R6),0             END                                          
        BE    NEXTELX                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                 ZERO LENGTH TRAP                             
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),X'05'         FIND 05 OR 15 ELEMENTS                       
         BER   R9                                                               
         CLI   0(R6),X'15'                                                      
         BER   R9                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   R9,R9                                                            
         BR    R9                                                               
*                                                                               
SHRINK   DS    0H                                                               
         LA    R7,70               MAX LINE LENGTH                              
         LA    R3,77(R2)                                                        
SHRINK2  CLI   0(R3),X'40'         BLANK OR ZERO                                
         BE    *+12                                                             
         CLI   0(R3),X'00'                                                      
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         BCT   R7,SHRINK2                                                       
         STC   R7,5(R2)            REVISED INPUT LENGTH                         
         MVC   REC2(70),8(R2)      CHECK IF WHOLE LINE BLANK                    
         OC    REC2(70),SPACES                                                  
         CLC   REC2(70),SPACES                                                  
         BNER  R9                                                               
         MVI   5(R2),1             BLANK LINE = ONE SPACE                       
         BR    R9                                                               
*                                                                               
BLANKEL  DC    X'000340'                                                        
TOOMANY  EQU   173                                                              
BLNKCMT  EQU   175                 NO LINES IN THIS COMMENT                     
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMFDD                                                       
         EJECT                                                                  
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041SPLFM1D   05/01/02'                                      
         END                                                                    
