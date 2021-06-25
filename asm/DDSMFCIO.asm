*          DATA SET DDSMFCIO   AT LEVEL 003 AS OF 05/12/15                      
*PHASE SMFCIOA                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*                                                                               
         TITLE 'DDSFMCIO - CROSSFILE DATA '                                     
         PRINT NOGEN                                                            
*                                                                               
TRACE    CSECT                                                                  
         NBASE WORKX-WORKD,XXTRACEX,WORK=A(SAVCHAIN),CLEAR=YES                  
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         LARL  RA,COMMON                                                        
         USING COMMON,RA                                                        
         USING PRLINE,PLINE                                                     
*                                                                               
TRACE001 BRAS  RE,OPENEM           OPEN INPUT AND SYSPRINT                      
         BRAS  RE,PRINTI           INIT PRINTING                                
         BRAS  RE,INIT             READ CARDS ECT                               
         BRAS  RE,MAIN             MAIN LOOP                                    
         BRAS  RE,CLOSEM           CLOSE INPUT AND SYSPRINT                     
*                                                                               
XBASE    L     RD,SAVERD           GET HERE FROM ANYWHERE                       
         XBASE 1                                                                
                                                                                
***********************************************************************         
* OPEN INPUT AND PRINT                                                          
***********************************************************************         
OPENEM   NTR1                                                                   
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         OPEN  (ADRIN,INPUT)                                                    
         B     EXIT                                                             
                                                                                
***********************************************************************         
* CLOSE FILES AND EXIT                                                          
***********************************************************************         
CLOSEM   NTR1                                                                   
         CLOSE (SYSPRINT)                                                       
         CLOSE (ADRIN)                                                          
         B     EXIT                                                             
                                                                                
***********************************************************************         
* INITIALISE                                                                    
***********************************************************************         
INIT     NTR1                                                                   
*                                                                               
         LARL  R1,TITLE1           PRINT PARAMETER CARDS TITLE                  
         BRAS  RE,PRINTT                                                        
*                                                                               
         LA    R3,IO                                                            
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
         XC    SMFHEAD,SMFHEAD                                                  
         CLI   DETAIL,C'Y'                                                      
         BE    EXIT                                                             
         MVC   TITLET+PRSHORT(20),SPACES                                        
         MVC   TITLE2T+PRSHORT(20),SPACES                                       
         B     EXIT                                                             
                                                                                
***********************************************************************         
* MAIN                                                                          
***********************************************************************         
MAIN     NTR1                                                                   
*                                                                               
MAIN010  MVC   PLINE,SPACES                                                     
         GET   ADRIN                                                            
         LR    R6,R1                                                            
*                                                                               
         USING SMFRECD,R6                                                       
         CLI   SMFIRTY,248         Record type    248                           
         BNE   MAIN010                                                          
         CLC   SMFISTY,=X'0003'    Record Subtype   3                           
         BNE   MAIN010                                                          
         GOTO1 DATCON,DMCB,(6,SMFIDTE),(2,SMFDATE)                              
*                                                                               
         BRAS  RE,FILTER                                                        
         BNE   MAIN010                                                          
*                                                                               
         CLI   DUMP,C'Y'                                                        
         BE    *+12                                                             
         BRAS  RE,PRINTIT                                                       
         B     MAIN010                                                          
         BRAS  RE,DUMPIT                                                        
         B     MAIN010                                                          
*                                                                               
ADREND   B     EXIT                                                             
                                                                                
***********************************************************************         
* FILTER                                                                        
***********************************************************************         
FILTER   NTR1                                                                   
         LARL  R9,CARDTAB          GET CARDTAB FOR FILTERS                      
*                                                                               
FILTX    J     EXITEQ                                                           
                                                                                
***********************************************************************         
* BUILD A PRINT LINE FROM ADRREC @ R6                                           
***********************************************************************         
         USING SMFRECD,R6                                                       
PRINTIT  NTR1                                                                   
         CLC   SMFHEAD,0(R6)                                                    
         BE    PITX                                                             
         MVC   SMFHEAD,0(R6)                                                    
*                                        DATE                                   
         GOTO1 DATCON,DMCB,(2,SMFDATE),(5,PDATE)                                
*                                                                               
         XR    R2,R2                     TIME                                   
         ICM   R3,15,SMFITME                                                    
         D     R2,=F'100'                                                       
         XR    R2,R2                                                            
         D     R2,=F'60'                                                        
         EDIT  (R2),(2,PTIME+6),ZERO=NOBLANK,FILL=0                             
         MVI   PTIME+5,C':'                                                     
         XR    R2,R2                                                            
         D     R2,=F'60'                                                        
         EDIT  (R2),(2,PTIME+3),ZERO=NOBLANK,FILL=0                             
         MVI   PTIME+2,C':'                                                     
         EDIT  (R3),(2,PTIME),ZERO=NOBLANK,FILL=0                               
*                                                                               
         EDIT  SMFIRTY,PTYPE             TYPE                                   
         EDIT  SMFISTY,PSTYPE                                                   
*                                                                               
         MVC   PJOB,SMFDJOB                                                     
         MVC   PSTEP,SMFDSTEP                                                   
         MVC   PJES,SMFDJES                                                     
*                                                                               
         LA    R7,SMFDATA-2                                                     
         USING CROSD,R7                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,CROSYSN,PDSYS,1                                      
*                                                                               
         LA    R1,SYSLST+6                                                      
         ICM   R2,14,SYSLST+2                                                   
         USING SYSLSTD,R1                                                       
PIT012   CR    R1,R2                                                            
         BNL   PIT016                                                           
         CLC   CROSYSN,SYSLNUM                                                  
         BE    PIT014                                                           
         LA    R1,SYSLLEN(0,R1)                                                 
         B     PIT012                                                           
PIT014   MVC   PDSYS(7),SYSLNAME                                                
         DROP  R1                                                               
*                                                                               
PIT016   GOTO1 HEXOUT,DMCB,CROSPGM,PDPGM,1                                      
         CLI   CROSPGM+1,C' '                                                   
         BNH   *+10                                                             
         MVC   PDPGM(2),CROSPGM                                                 
*                                                                               
         EDIT  CROSUID,PDUID,ZERO=NOBLANK,FILL=0                                
*                                                                               
         CLC   CROSSAGY,SPACES                                                  
         BNH   PIT018                                                           
         MVC   PDSAGY,CROSSAGY                                                  
*                                                                               
PIT018   OC    CROSPID,CROSPID                                                  
         BZ    PIT020                                                           
         GOTO1 HEXOUT,DMCB,CROSPID,PDPID,2                                      
*                                                                               
PIT020   MVC   PDMAGY,CROSMAGY                                                  
         MVC   PDAAGY,CROSAAGY                                                  
         GOTO1 HEXOUT,DMCB,CROSMSE,PDMSE,1                                      
         GOTO1 HEXOUT,DMCB,CROSASE,PDASE,1                                      
*                                                                               
         CLI   DETAIL,C'Y'                                                      
         BE    PIT030                                                           
         CLC   PLINE(PRSHORT),SVPLINE                                           
         MVC   SVPLINE,PLINE                                                    
         BNE   PIT040                                                           
         MVC   PLINE,SPACES                                                     
         B     PITX                                                             
*                                                                               
PIT030   MVC   PDOFF,CROSOFF                                                    
         MVC   PDMED,CROSMED                                                    
         MVC   PDCLI,CROSCLI                                                    
         MVC   PDPRD,CROSPRD                                                    
*                                                                               
PIT040   MVC   PRDW,=Y(PRLINELQ+3,0)                                            
         MVI   PCTL,X'09'                                                       
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,SPACES                                                     
*                                                                               
PIT050   CLI   HEXP,C'Y'                 HEX PRINT?                             
         BNE   PITX                      NO                                     
         XR    R1,R1                     RECORD DATA                            
         ICM   R1,3,SMFDATAL                                                    
         AHI   R1,-3                                                            
         BM    PIT060                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),SMFDATA                                                 
*                                                                               
PIT060   MVC   PRDW,=Y(PRLINELQ+3,0)                                            
         MVI   PCTL,X'09'                                                       
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,SPACES                                                     
*                                                                               
PIT065   MVC   PRDW,=Y(PRLINELQ+3,0)                                            
         MVI   PCTL,X'09'                                                       
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,SPACES                                                     
*                                                                               
PITX     B     EXITEQ                                                           
         DROP  R6,R7                                                            
                                                                                
***********************************************************************         
* DUMP A RECORD FROM ADRREC @ R6                                                
***********************************************************************         
DUMPIT   NTR1                                                                   
*                                                                               
         LR    R2,R6                                                            
         LHI   R3,8                                                             
DUMPIT1  GOTO1 HEXOUT,DMCB,(R2),PLINE,16                                        
         MVC   PLINE+36(16),0(R2)                                               
         MVC   PRDW,=Y(132,0)                                                   
         MVI   PCTL,X'09'                                                       
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,SPACES                                                     
         LA    R2,16(,R2)                                                       
         BCT   R3,DUMPIT1                                                       
*                                                                               
         MVC   PLINE,SPACES                                                     
         MVC   PRDW,=Y(132,0)                                                   
         MVI   PCTL,X'09'                                                       
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,SPACES                                                     
         B     EXIT                                                             
                                                                                
***********************************************************************         
* EXITS, COMMON ROUTINES, AND CONSTANTS                                         
***********************************************************************         
COMMON   DS    Y                                                                
*                                                                               
EXITNE   DS    0H                  SET CC NEQ                                   
EXITLO   MVI   EXITCC,0            SET CC LOW                                   
         B     EXITC                                                            
EXITHI   MVI   EXITCC,2            SET CC HIGH                                  
         B     EXITC                                                            
EXITEQ   MVI   EXITCC,1            SET CC EQUAL                                 
EXITC    CLI   EXITCC,1                                                         
EXIT     XIT1                                                                   
                                                                                
***********************************************************************         
* PRINT ROUTINES                                                                
***********************************************************************         
PRINTI   ST    RE,SAVERE                                                        
         ZAP   LINE,=PL1'0'                                                     
         ZAP   PAGE,=PL1'1'                                                     
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT FIRST TITLES                           
         ST    R1,SAVER1                                                        
         ZAP   LINE,=PL1'0'        RESET LINECOUNT                              
         AP    PAGE,=PL1'1'        BUMP PAGECOUNT                               
         LARL  R0,NEWPGE                                                        
         PUT   SYSPRINT,(R0)       SKIP PAGE                                    
         L     R0,SAVER1                                                        
         PUT   SYSPRINT,(R0)       PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=PL1'1'        BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         JL    PRINTL2                                                          
*                                                                               
         ZAP   LINE,=PL1'1'        RESET LINECOUNT                              
         AP    PAGE,=PL1'1'        BUMP PAGECOUNT                               
         LARL  R0,NEWPGE                                                        
         PUT   SYSPRINT,(R0)       SKIP PAGE                                    
         LARL  R0,TITLE                                                         
         PUT   SYSPRINT,(R0)       PRINT TITLE                                  
         LARL  R0,TITLE2                                                        
         PUT   SYSPRINT,(R0)       PRINT UNDERLINE                              
*                                                                               
PRINTL2  LA    R1,PLINE+L'PLINE-1  SET WRITE LENGTH                             
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,POUT-1                                                        
         SR    R1,R0                                                            
         SLL   R1,16                                                            
         STCM  R1,15,PRDW                                                       
         PUT   SYSPRINT,POUT       PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
                                                                                
***********************************************************************         
* PRINT TITLE                                                                   
***********************************************************************         
         DS    0H                                                               
NEWPGE   DC    AL2(NEWPGELQ),XL2'00',X'89'                                      
NEWPGELQ EQU   *-NEWPGE                                                         
*                                                                               
         DS    0H                                                               
TITLE    DC    AL2(TITLELQ),XL2'00',X'09'                                       
TITLET   DC    CL20'Date      Time'                                             
         DC    CL8'SMF  #'                                                      
         DC    CL28'Job Name Step     Job#'                                     
         DC    CL15'UID#  SA PID#'                                              
         DC    CL14'System  Prgm'                                               
         DC    CL14'MA AA M# A#'                                                
         DC    CL20'OFF MED CLT PRD'                                            
TITLELQ  EQU   *-TITLE                                                          
*                                                                               
         DS    0H                                                               
TITLE2   DC    AL2(TITLE2LQ),XL2'00',X'09'                                      
TITLE2T  DC    CL20'--------- ---------'                                        
         DC    CL8'--- --'                                                      
         DC    CL28'-------- -------- --------'                                 
         DC    CL15'----- -- ----'                                              
         DC    CL14'------- ----'                                               
         DC    CL14'-- -- -- --'                                                
         DC    CL20'--- --- --- ---'                                            
TITLE2LQ EQU   *-TITLE2                                                         
*                                                                               
         DS    0H                                                               
TITLE1   DC    AL2(TITLE1LQ),XL2'00',X'09'                                      
         DC    C'-------------------------------'                               
         DC    C' Parameter Cards '                                             
         DC    C'--------------------------------'                              
TITLE1LQ EQU   *-TITLE1                                                         
         ORG                                                                    
                                                                                
***********************************************************************         
* PARAMETER CARDS AND HANDLING ROUTINE                                          
***********************************************************************         
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN-1),X'FLAGS',AL3(OUTPUT)           
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE PL4'HHMMSS'                       
*        X'0000'                   SET TEXT VALUE                               
*                                                                               
CARDTAB  DS    0F                                                               
         DC    C'DUMP   ',AL1(3,0),X'0000',AL3(DUMP)                            
         DC    C'HEX    ',AL1(2,0),X'0000',AL3(HEXP)                            
         DC    C'LUID   ',AL1(3,8),X'4400',AL3(FLUIDC)                          
         DC    C'STIME  ',AL1(4,4),X'6600',AL3(FSTIME)                          
         DC    C'ETIME  ',AL1(4,4),X'6600',AL3(FETIME)                          
         DC    C'DETAIL ',AL1(5,0),X'0000',AL3(DETAIL)                          
         DC    X'0000'                                                          
*                                                                               
* CARD OUTPUT AREAS SET WITH DEFAULTS                                           
*                                                                               
DUMP     DC    C'N'                                                             
HEXP     DC    C'N'                                                             
DETAIL   DC    C'Y'                                                             
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
*                                                                               
VALCARD  NTR1                                                                   
         LARL  R9,CARDTAB                                                       
*                                                                               
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         JE    EXITEQ                                                           
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
         GOTO1 HEXIN,DMCB,(R2),(RF),(R0)                                        
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
         J     EXITEQ                                                           
*                                                                               
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
         J     EXITNE                                                           
                                                                                
*************************************************************                   
* GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS                                
*************************************************************                   
VALTIME  NTR1                                                                   
*                                                                               
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
         JNE   EXITEQ                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE SECS                                
         AP    FULL,DUB                                                         
         B     EXITEQ                                                           
       ++INCLUDE DDVALNUM                                                       
                                                                                
***********************************************************************         
* CONSTANTS & LITERALS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
DATCON   DC    V(DATCON)                                                        
HEXIN    DC    V(HEXIN)                                                         
HEXOUT   DC    V(HEXOUT)                                                        
*                                                                               
MAXLINE  DC    P'60'                                                            
SPACES   DC    CL132' '                                                         
ZEROS    DC    132X'00'                                                         
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
CTFILE   DC    C'CTFILE '                                                       
CONTROL  DC    C'CONTROL'                                                       
*                                                                               
       ++INCLUDE FASYSLST                                                       
*                                                                               
***********************************************************************         
* DCBS - SYSPRINT LRECL=(137)  RDW,CC,CL132                                     
*      - ADRIN BFTEK=A ENSURES SPANNED RECORDS RE-ASSEMBLED                     
***********************************************************************         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=VBM,LRECL=(137)          
ADRIN    DCB   DSORG=PS,MACRF=GL,BFTEK=A,DDNAME=ADRIN,EODAD=ADREND              
                                                                                
***********************************************************************         
* RD CHAIN                                                                      
***********************************************************************         
SAVCHAIN DS    1000D               MINIMAL RD CHAIN                             
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
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
EXITCC   DS    X                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
*                                                                               
SMFHEAD  DS    XL24                                                             
SMFDATE  DS    XL2                 SMF DTE                                      
*                                                                               
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
CARDEND  DS    A                                                                
*                                                                               
POUT     DS    0C                                                               
PRDW     DS    XL4                                                              
PCTL     DS    X                                                                
PLINE    DS    CL132                                                            
SVPLINE  DS    CL132                                                            
*                                                                               
         DS    0F                                                               
IOL      DS    XL4                                                              
IO       DS    CL2048                                                           
*                                                                               
WORKX    EQU   *                                                                
                                                                                
***********************************************************************         
* SMF RECORD DSECT                                                              
***********************************************************************         
SMFRECD  DSECT                                                                  
SMFREC   DS    0D                  SMF RECORD                                   
SMFIHDR  DS    0XL24               IBM RECORD HEADER WITH SUBTYPES              
SMFILEN  DS    XL2                 LENGTH INCLUDING THIS FIELD                  
SMFISEG  DS    XL2                 SEGMENT DESCRIPTOR (SET TO ZERO)             
SMFIFLG  DS    XL1                 FLAGS 80=SUBSYSTEM,40=SUBTYPE FORMAT         
SMFIRTY  DS    AL1                 RECORD TYPE                                  
SMFITME  DS    XL4                 TIME IN 1/100 SEC                            
SMFIDTE  DS    PL4                 DATE P'0CYYMMMF'                             
SMFISID  DS    CL4                 SYSTEM ID                                    
SMFISSI  DS    CL4                 SUBSYSTEM ID                                 
SMFISTY  DS    XL2                 SUBTYPE                                      
SMFIHDRL EQU   *-SMFIHDR                                                        
SMFIHDRX EQU   *                                                                
*                                                                               
SMFDMAP  DS    0XL8                MAP OF DDS RECORD DATA                       
SMFDHDRO DS    XL2                 OFFSET TO DDS HEADER                         
SMFDHDRL DS    XL2                 LENGTH OF DDS HEADER                         
SMFDATAO DS    XL2                 OFFSET TO DDS DATA 1 AREA                    
SMFDATAL DS    XL2                 LENGTH OF DDS DATA 1 AREA                    
SMFDMAPX EQU   *                                                                
*                                                                               
SMFDHDR  DS    0CL32               DDS HEADER                                   
SMFDJOB  DS    CL8                 JOB NAME                                     
SMFDSTEP DS    CL16                STEP/PROC NAME                               
SMFDJES  DS    CL8                 JES NAME                                     
SMFDHDRX EQU   *                                                                
*                                                                               
SMFDATA  DS    XL192               DDS DATA AREA FOR SHORT MAXREC=256           
SMFDATAX EQU   *                                                                
                                                                                
***********************************************************************         
* PRINT LINE DSECT                                                              
***********************************************************************         
PRLINE   DSECT                                                                  
PDATE    DS    CL8                                                              
         DS    CL2                                                              
PTIME    DS    CL8                                                              
         DS    CL2                                                              
PTYPE    DS    CL3                                                              
         DS    CL1                                                              
PSTYPE   DS    CL2                                                              
         DS    CL2                                                              
PJOB     DS    CL8                                                              
         DS    CL1                                                              
PSTEP    DS    CL8                                                              
         DS    CL1                                                              
PJES     DS    CL8                                                              
         DS    CL2                                                              
PDUID    DS    CL5                                                              
         DS    CL1                                                              
PDSAGY   DS    CL2                                                              
         DS    CL1                                                              
PDPID    DS    CL4                                                              
         DS    CL2                                                              
PDSYS    DS    CL7                                                              
         DS    CL1                                                              
PDPGM    DS    CL2                                                              
         DS    CL4                                                              
PDMAGY   DS    CL2                                                              
         DS    CL1                                                              
PDAAGY   DS    CL2                                                              
         DS    CL1                                                              
PDMSE    DS    CL2                                                              
         DS    CL1                                                              
PDASE    DS    CL2                                                              
PRSHORT  EQU   *-PRLINE                                                         
         DS    CL3                                                              
PDOFF    DS    CL1                                                              
         DS    CL3                                                              
PDMED    DS    CL1                                                              
         DS    CL3                                                              
PDCLI    DS    CL3                                                              
         DS    CL1                                                              
PDPRD    DS    CL3                                                              
PRLINELQ EQU   *-PRLINE                                                         
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
       ++INCLUDE DDCROSSD                                                       
       ++INCLUDE FASYSLSTD                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DDSMFCIO  05/12/15'                                      
         END                                                                    
