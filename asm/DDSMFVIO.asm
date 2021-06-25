*          DATA SET DDSMFVIO   AT LEVEL 006 AS OF 12/17/14                      
*PHASE SMFVIOA                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
         SPACE 1                                                                
         TITLE 'DDSFMVIO - SMF VIOLATE DATA '                                   
         PRINT NOGEN                                                            
         EJECT                                                                  
TRACE    CSECT                                                                  
         NBASE WORKX-WORKD,XXTRACEX,WORK=A(SAVCHAIN),CLEAR=YES                  
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         J     TRACE001            USE RB FOR CONSTANTS AND LITERALS            
         EJECT                                                                  
*************************************************************                   
*        DCBS                                               *                   
*************************************************************                   
*                                                                               
*        SYSPRINT LRECL=(137)  RDW,CC,CL132                                     
*        ADRIN    BFTEK=A ENSURES SPANNED RECORDS RE-ASSEMBLED                  
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=VBM,LRECL=(137)          
ADRIN    DCB   DSORG=PS,MACRF=GL,BFTEK=A,DDNAME=ADRIN,EODAD=ADREND              
*                                                                               
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
         SPACE 1                                                                
MAXLINE  DC    P'60'                                                            
SPACES   DC    CL132' '                                                         
ZEROS    DC    132X'00'                                                         
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
CTFILE   DC    C'CTFILE '                                                       
CONTROL  DC    C'CONTROL'                                                       
DAYS     DC    C'MonTueWedThuFriSatSun'                                         
WHYS     DC    C'0Vio1C=62Pid3>504R=35Pix ???'                                  
       ++INCLUDE FACIDTAB                                                       
*************************************************************                   
*        OPEN INPUT AND PRINT    **BASE REG REQD**          *                   
*************************************************************                   
         SPACE 1                                                                
OPENADR  NTR1                                                                   
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         OPEN  (ADRIN,INPUT)                                                    
         J     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        CLOSE FILES AND EXIT    **BASE REG REQD**          *                   
*************************************************************                   
         SPACE 1                                                                
DISKEND  EQU   *                                                                
*                                                                               
TSTEND   CLOSE (SYSPRINT)                                                       
         CLOSE (ADRIN)                                                          
         XBASE                                                                  
         EJECT                                                                  
*************************************************************                   
* CREATE AREA FOR LITERAL POOL UP TO 4K BOUNDARY            *                   
*************************************************************                   
         SPACE 1                                                                
LITERALS DS    0D                                                               
         ORG   TRACE+4096                                                       
         EJECT                                                                  
*************************************************************                   
*        START OF MAIN CODE - NO BASE REGISTER USED         *                   
*************************************************************                   
*                                                                               
TRACE001 BRAS  RE,OPENADR          OPEN ADR SOURCE                              
         BRAS  RE,PRINTI           INIT PRINTING                                
         BRAS  RE,INIT             READ CARDS ECT                               
         BRAS  RE,MAIN             MAIN LOOP                                    
*                                                                               
XBASE    L     RD,SAVERD           GET HERE FROM ANYWHERE                       
         XBASE 1                                                                
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
         LARL  R1,TITLE1           PRINT PARAMETER CARDS TITLE                  
         BRAS  RE,PRINTT                                                        
*                                                                               
         LA    R3,IO                                                            
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         MVC   PRDW,=Y(80+3,0)                                                  
         MVI   PCTL,X'09'                                                       
         MVC   PLINE(80),0(R3)                                                  
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
         CLC   =C'/*',0(R3)                                                     
         JE    INIT020                                                          
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BRAS  RE,VALCARD          READ KEYWORD=VALUES                          
         JNE   XBASE               NEQ MEANS INVALID KEYWORD                    
         J     INIT010                                                          
*                                                                               
INIT020  ZAP   LINE,MAXLINE        FORCE NEW PAGE                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        MAIN                                               *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
         LARL  RA,CARDBASE         GET CARDBASE FOR FILTERS                     
         USING CARDBASE,RA                                                      
*                                                                               
MAIN010  MVC   PLINE,SPACES                                                     
         GET   ADRIN                                                            
         LR    R6,R1                                                            
*                                                                               
         CLC   0(2,R6),=X'0068'    LENGTH X'68'                                 
         JNE   MAIN010                                                          
         CLC   4(2,R6),=X'DEF9'    F9=ADR REC                                   
         JNE   MAIN010                                                          
         CLC   24(3,R6),=C'$CT'                                                 
         JNE   MAIN010                                                          
         MVC   SMFHDR,0(R6)        SAVE HEADER                                  
         CLC   SMF_DTE,SAVEDTE                                                  
         JE    MAIN011                                                          
         GOTO1 =V(DATCON),DMCB,(6,SMF_DTE),(16,SMFDATE)                         
         MVC   SAVEDTE,SMF_DTE                                                  
*                                                                               
MAIN011  LA    R6,24(R6)           SKIP HEADER                                  
*                                                                               
         USING LOGRECD,R6                                                       
         BRAS  RE,FILTER                                                        
         JNE   MAIN010                                                          
*                                                                               
         BRAS  RE,PRINT                                                         
         MVC   PLINE,SPACES                                                     
         J     MAIN010                                                          
*                                                                               
*                                                                               
ADREND   J     EXIT                                                             
*                                                                               
ADRENDX  J     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
*        FILTER                                                 *               
*****************************************************************               
         SPACE 1                                                                
FILTER   NTR1                                                                   
*                                                                               
         LARL  RA,CARDBASE         GET CARDBASE FOR FILTERS                     
         USING CARDBASE,RA                                                      
*                                                                               
         USING LOGRECD,R6                                                       
*                                                                               
         LA    R1,FLUIDC           LUID FILTERS                                 
         CLI   0(R1),0                                                          
         JE    FILT020                                                          
FILT010  CLI   0(R1),X'FF'         TEST FOR EOT                                 
         JE    EXITNEQ                                                          
         CLI   0(R1),0             TEST FOR EOT                                 
         JE    EXITNEQ                                                          
*                                                                               
FILT011  LA    RF,8(R1)            CLUID                                        
         CLI   0(RF),C' '                                                       
         JNE   *+8                                                              
         JCT   RF,*-8                                                           
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         BRAS  RE,*+10                                                          
         CLC   LOGLUID(0),1(R1)                                                 
         EX    RF,0(RE)            COMPARE FOR ENTRY LEN-1                      
         IC    RF,0(R1)                                                         
         BRAS  RE,*+8                                                           
         JC    0,FILT020                                                        
         EX    RF,0(RE)                                                         
         LA    R1,9(R1)                                                         
         J     FILT010                                                          
*                                                                               
FILT020  LA    R1,FSTIME                                                        
         CLI   0(R1),0                                                          
         JE    FILT040                                                          
FILT021  CLI   0(R1),0                                                          
         JE    FILT040                                                          
         CLI   0(R1),X'FF'                                                      
         JE    FILT040                                                          
         CLC   1(4,R1),LOGTIME                                                  
         IC    RF,0(R1)                                                         
         BRAS  RE,*+8                                                           
         JC    0,EXITNEQ                                                        
         EX    RF,0(RE)                                                         
         LA    R1,5(R1)                                                         
         J     FILT021                                                          
*                                                                               
FILT040  LA    R1,FETIME                                                        
         CLI   0(R1),0                                                          
         JE    FILT060                                                          
FILT041  CLI   0(R1),0                                                          
         JE    FILT060                                                          
         CLI   0(R1),X'FF'                                                      
         JE    FILT060                                                          
         CLC   1(4,R1),LOGTIME                                                  
         IC    RF,0(R1)                                                         
         BRAS  RE,*+8                                                           
         JC    0,EXITNEQ                                                        
         EX    RF,0(RE)                                                         
         LA    R1,5(R1)                                                         
         J     FILT041                                                          
*                                                                               
FILT060  J     EXITEQU                                                          
*                                                                               
FILT999  J     EXITEQU                                                          
         EJECT                                                                  
*************************************************************                   
*        BUILD A PRINT LINE FROM ADRREC @ R6                *                   
*************************************************************                   
         SPACE 1                                                                
PRINT    NTR1                                                                   
         USING LOGRECD,R6                                                       
         LA    R7,PLINE            R7=A(PLINE)                                  
         USING PRLINE,R7                                                        
*                                                                               
         MVC   DATE,SMFDATE                                                     
*                                                                               
         CLI   LOGDAYNO,7                                                       
         JH    PRN010                                                           
         LLC   R1,LOGDAYNO                                                      
         MHI   R1,3                                                             
         LA    R1,DAYS-3(R1)                                                    
         MVC   DAY,0(R1)                                                        
*                                                                               
PRN010   UNPK  DUB,LOGTIME                                                      
         OI    DUB+L'DUB-1,X'F0'                                                
         MVC   TIME(2),DUB+2                                                    
         MVI   TIME+2,C':'                                                      
         MVC   TIME+3(2),DUB+4                                                  
         MVI   TIME+5,C':'                                                      
         MVC   TIME+6(2),DUB+6                                                  
*                                                                               
         MVC   TRMID,LOGLUID                                                    
*                                                                               
         GOTOR GETFACID,DMCB,LOGSYSIX,FACPAK                                    
*                                                                               
         LA    R1,LOGID+3                                                       
         BRAS  RE,GETWHY                                                        
*                                                                               
         MVC   USER,LOGUSER                                                     
         MVC   SYS,LOGSYS                                                       
         MVC   PRG,LOGPRG                                                       
         MVC   PWD,LOGPWD                                                       
*                                                                               
         CLI   SHOWPW,C'Y'                                                      
         JE    PRN240                                                           
*        J     PRN240                                                           
         MVC   PWD,=10C'*'                                                      
         LA    R1,LOGPWD                                                        
         LA    R0,L'PWD                                                         
PRN020   CLI   0(R1),C' '                                                       
         JH    PRN240                                                           
         LA    R1,1(,R1)                                                        
         JCT   R0,PRN020                                                        
         MVC   PWD,SPACES                                                       
*                                                                               
PRN240   MVC   PRDW,=Y(PRLINELQ+3,0)                                            
         MVI   PCTL,X'09'                                                       
         BRAS  RE,PRINTL                                                        
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         SPACE 1                                                                
PRINTI   ST    RE,SAVERE                                                        
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT FIRST TITLES                           
         ST    R1,SAVER1                                                        
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         LARL  R0,NEWPGE                                                        
         PUT   SYSPRINT,(R0)       SKIP PAGE                                    
         L     R0,SAVER1                                                        
         PUT   SYSPRINT,(R0)       PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         JL    PRINTL2                                                          
*                                                                               
         ZAP   LINE,=P'1'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         LARL  R0,NEWPGE                                                        
         PUT   SYSPRINT,(R0)       SKIP PAGE                                    
         LARL  R0,TITLE                                                         
         PUT   SYSPRINT,(R0)       PRINT TITLE                                  
*                                                                               
PRINTL2  LA    R1,PLINE+L'PLINE-1  SET WRITE LENGTH                             
         CLI   0(R1),C' '                                                       
         JNE   *+8                                                              
         JCT   R1,*-8                                                           
         LA    R0,POUT-1                                                        
         SR    R1,R0                                                            
         SLL   R1,16                                                            
         STCM  R1,15,PRDW                                                       
         PUT   SYSPRINT,POUT       PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
         EJECT                                                                  
*************************************************************                   
*        PRINT TITLE                                        *                   
*************************************************************                   
         SPACE 1                                                                
         DS    0H                                                               
NEWPGE   DC    AL2(NEWPGELQ),XL2'00',X'89'                                      
NEWPGELQ EQU   *-NEWPGE                                                         
         DS    0H                                                               
TITLE    DC    AL2(TITLELQ),XL2'00',X'09'                                       
         DC    C'Date  Day Time     Terminal FacId Why '                        
         DC    CL18'User'                                                       
         DC    CL18'System'                                                     
         DC    CL18'Program'                                                    
         DC    C'Password'                                                      
TITLELQ  EQU   *-TITLE                                                          
         DS    0H                                                               
TITLE1   DC    AL2(TITLE1LQ),XL2'00',X'09'                                      
         DC    C'-------------------------------'                               
         DC    C' Parameter Cards '                                             
         DC    C'--------------------------------'                              
TITLE1LQ EQU   *-TITLE1                                                         
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        EXPAND FACPAK ID                                   *                   
*************************************************************                   
         SPACE 1                                                                
GETFACID NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         LA    R1,FACIDTAB                                                      
         MVC   BYTE,0(R2)                                                       
         NI    BYTE,X'0F'                                                       
GFI10    CLC   BYTE,4(R1)          FIND ADV SYSTEM                              
         JE    GFI11                                                            
         LA    R1,8(R1)                                                         
         CLI   5(R1),X'FF'         CHECK EOT                                    
         JNE   GFI10                                                            
         DC    H'0'                                                             
GFI11    MVC   0(4,R3),0(R1)                                                    
         LLC   R1,0(,R2)           DO AOR NUMBER TOO                            
         SRL   R1,4                                                             
         LTR   R1,R1                                                            
         JZ    EXIT                                                             
         LA    R1,X'C0'(,R1)                                                    
         CLI   3(R3),C' '                                                       
         JNE   *+12                                                             
         STC   R1,3(,R3)                                                        
         J     EXIT                                                             
         STC   R1,4(,R3)                                                        
         J     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        TRANSLATE $CTX TO REASON R1=A(X) FROM $CTX         *                   
*************************************************************                   
         SPACE 1                                                                
GETWHY   NTR1                                                                   
         LA    RE,WHYS                                                          
GWHY10   CLC   0(1,R1),0(RE)                                                    
         JE    GWHY20              FIND ADV SYSTEM                              
         LA    RE,4(,RE)                                                        
         CLI   0(RE),C' '                                                       
         JNE   GWHY10                                                           
GWHY20   MVC   CTWHY,1(RE)                                                      
         J     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN-1),X'FLAGS',AL3(OUTPUT)           
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE PL4'HHMMSS'                       
*                                                                               
         SPACE 1                                                                
CARDBASE DS    0F                                                               
CARDTAB  DS    0F                                                               
         DC    C'SHOWPW ',AL1(5,0),X'4000',AL3(SHOWPW)                          
         DC    C'LUID   ',AL1(3,8),X'4400',AL3(FLUIDC)                          
         DC    C'STIME  ',AL1(4,4),X'6600',AL3(FSTIME)                          
         DC    C'ETIME  ',AL1(4,4),X'6600',AL3(FETIME)                          
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
SHOWPW   DC    C'N'                SHOWPW=N                                     
*                                                                               
FLUIDC   DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'FF'                                                            
*                                                                               
FSTIME   DC    X'00',XL4'00'                                                    
         DC    X'00',XL4'00'                                                    
         DC    X'FF'                                                            
FETIME   DC    X'00',XL4'00'                                                    
         DC    X'00',XL4'00'                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
VALCARD  NTR1                                                                   
         LARL  RA,CARDBASE                                                      
         USING CARDBASE,RA                                                      
*                                                                               
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         JE    EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         J     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         JE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         JNE   VALC010                                                          
         J     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         J     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         JE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         JE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         JZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         JNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         J     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         JE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         JE    VALC030                                                          
         CLI   0(R1),0                                                          
         JE    VALC030                                                          
         LA    R1,1(R1)                                                         
         J     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         JZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         J     VALC090                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         JNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         JE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         JE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         JZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         J     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         JZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         JNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         JZ    CERRHEX                                                          
         J     VALC090                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         JZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BRAS  RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         JE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         ST    R1,0(RF)            SAVE FULLWORD (DEFAULT)                      
         J     VALC090                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         JZ    VALC080                                                          
         BRAS  RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         J     VALC090                                                          
*                                                                               
VALC080  CLI   8(R4),0             DONT CARE                                    
         JE    *+12                                                             
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         JNL   CERRMAX                                                          
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC090  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         JE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         JL    VALC090                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         J     EXIT                                                             
*                                                                               
         EJECT                                                                  
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         J     CERRX                                                            
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         J     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         J     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         J     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         J     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         J     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         J     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE                                                         
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         JE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         JE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         J     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         LA    RF,16(,RF)                                                       
         LA    R1,PLINE                                                         
         SR    RF,R1                                                            
         SLL   RF,16                                                            
         ST    RF,PRDW                                                          
         MVI   PCTL,X'09'                                                       
         BRAS  RE,PRINTL                                                        
*                                                                               
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         J     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS     *                   
*************************************************************                   
         SPACE 1                                                                
VALTIME  NTR1                                                                   
         XC    FULL,FULL                                                        
         LA    R3,2                PREPARE VALNUM                               
         LA    R4,HALF                                                          
*                                                                               
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         JNE   VALT010                                                          
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         J     VALT020                                                          
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
VALT020  BRAS  RE,VALNUM           VALIDATE HOURS                               
         SRP   DUB,4,0                                                          
         ZAP   FULL,DUB                                                         
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BRAS  RE,VALNUM                                                        
         SRP   DUB,2,0                                                          
         AP    FULL,DUB                                                         
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         JNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE SECS                                
         AP    FULL,DUB                                                         
         J     EXITEQU                                                          
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
*************************************************************                   
* LITERALS                                                  *                   
*************************************************************                   
         SPACE 1                                                                
         ORG   LITERALS                                                         
         LTORG                                                                  
SAVCHAIN DS    1000D               MINIMAL RD CHAIN                             
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    F                                                                
SAVERE   DS    F                                                                
SAVER1   DS    F                                                                
CARDRD   DS    F                                                                
CARDR2   DS    F                                                                
SAVER0   DS    F                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
WORK1    DS    CL64                                                             
SDUB     DS    D                                                                
SWORK1   DS    CL64                                                             
*                                                                               
SMFHDR   DS    0CL24                                                            
SMF_LEN  DS    AL2                 LENGTH INCLUDING THIS FIELD                  
SMF_SEG  DS    XL2                 SEGMENT DESCRIPTOR (SET TO ZERO)             
SMF_FLG  DS    X                   FLAGS 80=SUBSYSTEM,40=SUBTYPE FORMAT         
SMF_RTY  DS    AL1                 RECORD TYPE                                  
SMF_TME  DS    XL4                 TIME IN 1/100 SEC                            
SMF_DTE  DS    PL4                 DATE P'0CYYMMMF'                             
SMF_SID  DS    CL4                 SYSTEM ID                                    
SMF_SSI  DS    CL4                 SUBSYSTEM ID                                 
SMF_STY  DS    XL2                 SUBTYPE                                      
*                                                                               
SMFDATE  DS    CL5                 SMF DTE AS DDMMM                             
SAVEDTE  DS    XL4                                                              
*                                                                               
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
CARDEND  DS    A                                                                
*                                                                               
POUT     DS    0C                                                               
PRDW     DS    XL4                                                              
PCTL     DS    X                                                                
PLINE    DS    CL132                                                            
*                                                                               
         DS    0F                                                               
IOL      DS    XL4                                                              
IO       DS    CL2048                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 1                                                                
LOGRECD  DSECT ,                   LOG RECORD FOR SECURITY VIOLATIONS           
LOGREC   DS    0CL80               CURRENT ADRFILE RECORD SIZE                  
LOGID    DS    CL4                 $CT. IDENTIFIES A SRCON LOG REC              
LOGLUID  DS    CL8                                                              
LOGTIME  DS    PL4                                                              
LOGUSER  DS    CL16                1ST 16 BYTES USERID/PID FIELD                
LOGSYS   DS    CL16                1ST 16 BYTES SYSTEM FIELD                    
LOGPRG   DS    CL16                1ST 16 BYTES PROGRAM FIELD                   
LOGPWD   DS    CL10                INPUT PASSWORD                               
         DS    CL4                                                              
LOGDAYNO DS    XL1                 DAY NUMBER FROM SSB MON=1,SUN=7              
LOGSYSIX DS    XL1                 FACPAK AOR/TOR ID                            
         SPACE 1                                                                
PRLINE   DSECT                                                                  
DATE     DS    CL5                                                              
         DS    CL1                                                              
DAY      DS    CL3                                                              
         DS    CL1                                                              
TIME     DS    CL8                                                              
         DS    CL1                                                              
TRMID    DS    CL8                                                              
         DS    CL1                                                              
FACPAK   DS    CL5                                                              
         DS    CL1                                                              
CTWHY    DS    CL3                                                              
         DS    CL1                                                              
USER     DS    CL16                                                             
         DS    CL2                                                              
SYS      DS    CL16                                                             
         DS    CL2                                                              
PRG      DS    CL16                                                             
         DS    CL2                                                              
PWD      DS    CL10                                                             
PRLINELQ EQU   *-PRLINE                                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DDSMFVIO  12/17/14'                                      
         END                                                                    
