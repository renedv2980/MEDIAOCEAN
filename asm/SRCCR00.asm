*          DATA SET SRCCR00    AT LEVEL 003 AS OF 11/09/18                      
*PHASE T15600A                                                                  
*INCLUDE FORMULA                                                                
*INCLUDE BRACKET                                                                
*INCLUDE CUREDIT                                                                
         TITLE '$CC - CALULATOR MODULE'                                         
         PRINT NOGEN                                                            
CCR      CSECT                                                                  
         NMOD1 DATX-DATD,**$CCR**,R9,RR=R5,CLEAR=YES                            
         USING DATD,RC                                                          
         USING SRPARMD,R1                                                       
         ST    R1,APLIST           R1=A(S/R PARAM LIST)                         
         ST    R5,RELO                                                          
*                                                                               
         MVC   ATIA,SRPARM2                                                     
         MVC   ACOMFACS,SRPARM4                                                 
         L     R2,SRPARM3          R2=A(UTL)                                    
         USING UTLD,R2                                                          
         MVI   DDS,0                                                            
         TM    TSTAT1,TSTATDDS     TEST AND SET DDS TERMINAL FLAG               
         BZ    *+8                                                              
         OI    DDS,X'01'                                                        
         L     RA,SRPARM6          RA=A(TWA)                                    
         USING SRCCRFFD,RA                                                      
         L     R8,SRPARM1          R8=A(SYSFACS)                                
         USING SYSFACD,R8                                                       
         L     RE,VSSB                                                          
         MVC   RECLEN,SSBTWAL-SSBD(RE)                                          
         NI    CCRINPH+6,X'BF'     TURN OFF CURSOR BITS                         
         NI    CCRSRVH+6,X'BF'                                                  
*                                                                               
CCREND   CLI   CCRINPH+5,0         NO INPUT DATA MEANS END OF $CC               
         BNE   VALINP                                                           
         XC    CCRINP,CCRINP       CLEAR INP FIELD AND SET PROT/NORM            
         MVI   CCRINPH+1,X'20'                                                  
         XC    CCRSRV,CCRSRV       CLEAR SRV FIELD AND SET UNP/NORM             
         MVI   CCRSRVH+1,X'00'                                                  
         NI    TSTAT2,255-TSTATCC  SET TERMINAL NOT IN $CC MODE                 
         LA    R4,CCRSRV                                                        
*                                                                               
         CLI   TSYS,0              TEST IF TERM IS CONNECTED                    
         BE    CCRX                                                             
         LH    R0,TNUM                                                          
         L     R5,ATIA                                                          
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',(R0),(R5)                   
         CLI   8(R1),0                                                          
         BNE   CCRX                                                             
         LA    R5,64(R5)                                                        
         MVC   CCRINP,8(R5)        COPY LAST HEADER MESSAGE                     
         SR    R0,R0                                                            
         IC    R0,0(R5)                                                         
         AR    R5,R0                                                            
         MVC   CCRSRV,8(R5)        COPY LAST SERVICE MESSAGE                    
         B     CCRX                                                             
*                                                                               
CCRX     OI    CCRINPH+6,X'80'     TRANSMIT TWO LINE ONE FIELDS                 
         OI    CCRSRVH+6,X'80'                                                  
         OI    6(R4),X'40'         POSITION CURSOR AS REQUESTED                 
         XMOD1 1                                                                
*                                                                               
INPVAL   XC    CCRSRV,CCRSRV                                                    
         MVC   CCRSRV(17),=CL17'$CC-ENTER CALC'                                 
         LA    R4,CCRINPH                                                       
         B     CCRX                                                             
*                                                                               
INPERR   XC    CCRSRV,CCRSRV                                                    
         MVC   CCRSRV(17),=CL17'$CC-ERROR'                                      
         LA    R4,CCRINPH                                                       
         B     CCRX                                                             
         DROP  R1,R2,R8                                                         
*                                                                               
VALINP   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*  CONTROL BLOCK                                                      *         
***********************************************************************         
C1       BAS   RE,PREL             PRELIMINARIES                                
*                                                                               
C2       BAS   RE,VALSTR           VALIDATE STRING(LOCATION,LENGTH)             
         CLC   LSTR,=H'0'          LSTR-L'STRING                                
         BNE   C2A                                                              
         MVI   ERR,X'FF'           MISSING INPUT                                
         B     EXIT                                                             
*                                                                               
C2A      GOTO1 INDMOVE,DMCB,(LSTR+1,ASTART)      SEARCH MOVING FUNCTION         
         CLI   DMCB,0                                                           
         BE    C2B                 NO MOVING                                    
         MVI   MOVE,1              YES                                          
*                                                                               
C2B      BAS   RE,VALSTR                                                        
         CLC   LSTR,=H'0'                                                       
         BNE   C2C                                                              
         MVI   ERR,X'FF'                                                        
         B     EXIT                                                             
*                                                                               
C2C      GOTO1 CLEQ,DMCB,(LSTR+1,ASTART)      CLEAN OLD ANSWER                  
*                                                                               
C3       BAS   RE,DETFUN           RETRIEVAL FUNCTION                           
         CLC   LSTR,LFUN                                                        
         BNE   C4                                                               
         MVI   ERR,X'FF'           INV FUNCTION,NO OPERAND                      
         B     EXIT                                                             
*                                                                               
C4       CLI   DELIM,C' '                                                       
         BE    C5                  MAY BE FUNCTION                              
         MVC   LFUN,=H'0'          NOT FUNCTION,IT IS OPERAND                   
         BAS   RE,CALC             CALCULATION FORMULA                          
         CLI   ERR,0                                                            
         BNE   EXIT                ERROR                                        
         BAS   RE,MOVEANS          MOVE ANSWER                                  
         B     C8                                                               
*                                                                               
C5       BAS   RE,RETTAB           RETRIEVAL FUNCTION IN THE TABLE              
         C     RF,=F'2'            RF=0 NOT EXIST                               
         BNE   C6                  RF=1 EXIST                                   
         MVI   ERR,X'FF'           RF=2 FORBIDDEN FUNCTION                      
         B     EXIT                REF=BLOCK EXECUTION ADDRESS                  
*                                                                               
C6       C     RF,=F'1'                                                         
         BE    C7                                                               
         MVC   LFUN,=H'0'          IT IS NOT FUNCTION IT IS OPERAND             
         BAS   RE,CALC                                                          
         CLI   ERR,0                                                            
         BNE   EXIT                ERROR                                        
         BAS   RE,MOVEANS                                                       
         B     C8                                                               
*                                                                               
C7       L     RF,REF                                                           
         BASR  RE,RF                                                            
         CLI   ERR,0                                                            
         BNE   EXIT                                                             
         BAS   RE,MOVEANS                                                       
*                                                                               
C8       CLI   MOVE,0              MOVE ANSWER INTO FIELD ?                     
         BE    EXIT                NO                                           
         L     R1,APLIST           R1=A(S/R PARAM LIST)                         
         L     R2,4(R1)            R2=A(TIA)                                    
         GOTO1 GETFLD,DMCB,(R2)    LOC ADDRESS OF FIELD WIHT CURSER             
         CLI   DMCB,0                                                           
         BE    C9                                                               
         MVI   ERR,X'FF'                                                        
         B     EXIT                                                             
*                                                                               
C9       MVC   AHED(4),DMCB+4     AHED=A(FIELD HDR)                             
         GOTO1 MOVEFLD,DMCB,(LANS,ANS),AHED  LOC ADDRESS OF FIELD WIHT          
         CLI   DMCB,0                        CURSER                             
         BE    EXIT                                                             
         MVI   ERR,X'FF'                                                        
*                                                                               
EXIT     CLI   ERR,0               TEST FOR ERROR                               
         BNE   INPERR                                                           
         B     INPVAL                                                           
         EJECT                                                                  
***********************************************************************         
*  PRELIMINARIES                                                      *         
***********************************************************************         
PREL     NTR1                                                                   
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
*                                                                               
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
*&&DO                                                                           
         LT    RE,=V(DATCON)                                                    
         BZ    *+12                                                             
         A     RE,RELO                                                          
         ST    RE,VDATCON                                                       
         LT    RE,=V(DATVAL)                                                    
         BZ    *+12                                                             
         A     RE,RELO                                                          
         ST    RE,VDATVAL                                                       
*&&                                                                             
         L     RE,=V(FORMULA)                                                   
         A     RE,RELO                                                          
         ST    RE,VFORMULA                                                      
         L     RE,=V(BRACKET)                                                   
         A     RE,RELO                                                          
         ST    RE,VBRACKET                                                      
         L     RE,=V(CUREDIT)                                                   
         A     RE,RELO                                                          
         ST    RE,VCUREDIT                                                      
*                                                                               
         MVC   DATE,SPACES                                                      
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 STRING DETERMINATION                                   *         
*  INPUT     1 FIELD                                                  *         
*            2 DISPLACEMENT(DISPL)                                    *         
*            3 L'FIELD(LFIELD)                                        *         
*  OUTPUT    1 A(STRING)-(ASTR)                                       *         
*            2 A(STRING END)-(AEND)                                   *         
*            3 L'STRING-LSTR                                          *         
*            4 A(FIELD)-(AFIELD),A(FIELD END)-(AENDFLD)               *         
***********************************************************************         
VALSTR   NTR1                                                                   
         LA    R2,CCRINP                                                        
         AH    R2,DISPL            R2=A(FIELD)                                  
         ST    R2,AFIELD                                                        
         LH    R3,LFIELD           R3=L'FIELD                                   
*                                                                               
CL1      CLI   0(R2),X'40'                                                      
         BH    FOUNDSTR                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,CL1                                                           
         MVC   LSTR,=H'0'          L'INPUT=0                                    
         B     ENDVALST                                                         
*                                                                               
FOUNDSTR ST    R2,ASTART                                                        
         LA    R2,CCRINP                                                        
         AH    R2,DISPL                                                         
         AH    R2,LFIELD                                                        
         S     R2,=F'1'                                                         
         ST    R2,AENDFLD                                                       
*                                                                               
CL2      CLI   0(R2),X'40'                                                      
         BH    ENDSTR                                                           
         BCT   R2,CL2                                                           
*                                                                               
ENDSTR   ST    R2,AEND                                                          
         L     R3,ASTART                                                        
         SR    R2,R3                                                            
         LA    R2,1(R2)                                                         
         STH   R2,LSTR LSTR=L'STRING                                            
ENDVALST XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CLEAN  OLD ANSWER ,CORRECT LSTR&AEND                   *         
*  INPUT     P1   BYTE   0   L'STRING                                 *         
*                        1-3 A(STRING)                                *         
*  OUTPUT    P1          NEW ASTR AND LSTR                            *         
***********************************************************************         
CLEQ     NTR1                                                                   
         ZIC   R2,DMCB             R2=L'STR                                     
         L     R3,DMCB                                                          
         LA    R3,0(R3)            R3=A(STR)                                    
         XR    R6,R6                                                            
*                      LOC =                                                    
EQ1      CLI   0(R3),C'='                                                       
         BE    EQ2                 R3=A'=                                       
         LA    R3,1(R3)                                                         
         LA    R6,1(R6)            R6=NEW L                                     
         BCT   R2,EQ1                                                           
         B     ENDEQ               NO =                                         
*                      CLEANING                                                 
EQ2      ZIC   R2,DMCB                                                          
         SR    R2,R6               R2=L'PART OF STRING AFTER =                  
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES                                                   
*                       CORRECTION                                              
         BCTR  R3,0                                                             
         ST    R3,AEND                                                          
         STH   R6,LSTR                                                          
*                                                                               
ENDEQ    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 LOC INDICATOR OF MOVING                                *         
*  INPUT     P1   BYTE   0   L'STRING                                 *         
*                        1-3 A(STRING)                                *         
*  OUTPUT    P1          0   1-INDICATOR EXIST  0 DOSNT EXIST         *         
*                        NEW ASTART AND LSTR                          *         
***********************************************************************         
INDMOVE  NTR1                                                                   
         ZIC   R2,DMCB             R2=L'STR                                     
         L     R3,DMCB                                                          
         LA    R3,0(R3)            R3=A(STR)                                    
*                                                                               
         CLI   0(R3),C'@'                                                       
         BE    IN1                                                              
         AR    R3,R2                                                            
         BCTR  R3,0                R3=A(END)                                    
         CLI   0(R3),C'@'                                                       
         BE    IN2                                                              
         B     IN3                                                              
*                                                                               
IN1      MVI   0(R3),C' '                                                       
         L     R4,ASTART                                                        
         LA    R4,1(R4)                                                         
         ST    R4,ASTART           NEW A                                        
         LH    R5,LSTR                                                          
         BCTR  R5,0                                                             
         STH   R5,LSTR             NEW L                                        
         MVI   DMCB,1                                                           
         B     ENDIN                                                            
*                                                                               
IN2      MVI   0(R3),C' '                                                       
         L     R4,AEND                                                          
         BCTR  R4,0                                                             
         ST    R4,AEND             NEW A                                        
         LH    R5,LSTR                                                          
         BCTR  R5,0                                                             
         STH   R5,LSTR             NEW L                                        
         MVI   DMCB,1                                                           
         B     ENDIN                                                            
*                                                                               
IN3      ZIC   R2,DMCB                                                          
         L     R3,DMCB                                                          
         LA    R3,0(R3)                                                         
*                                                                               
IN4      CLI   0(R3),C'@'                                                       
         BE    IN5                                                              
         LA    R3,1(R3)                                                         
         BCT   R2,IN4                                                           
         MVI   DMCB,0             NO @                                          
         B     ENDIN                                                            
*                                                                               
IN5      MVI   0(R3),C' '                                                       
         MVI   DMCB,1              R2=L'PART OF STRING AFTER =                  
*                                                                               
ENDIN    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1  FUNCTION DETERMINATION                                *         
*  INPUT     1  A(STRING)-ASTART                                      *         
*            2  A(STRING END)-AEND                                    *         
*            3  L'STRING-LSTR,DELIMITER SET-SETDEL                    *         
*  OUTPUT    1  A(FUNCTION)-ASTARTF                                   *         
*            2  A(FUNCTION END)-AENDF                                 *         
*            3  L'FUNCTION-LFUN,DELIMITER-DELIM                       *         
***********************************************************************         
DETFUN   DS    0H                                                               
         NTR1                                                                   
         L     R2,ASTART                                                        
         LH    R3,LSTR                                                          
*                                                                               
MCL1     LA    R4,SETDEL                                                        
MCL2     CLC   0(1,R2),0(R4)                                                    
         BE    FOUNDDEL                                                         
         LA    R4,1(R4)                                                         
         CLI   0(4),X'FF'                                                       
         BNE   MCL2                                                             
         LA    R2,1(R2)                                                         
         BCT   R3,MCL1                                                          
*                                                                               
NOFOUND  MVC   LFUN,LSTR           NO OPERAND                                   
         B     ENDDETF                                                          
*                                                                               
FOUNDDEL MVC   DELIM(1),0(R2)      R2=A(DELIMETR)                               
         MVC   ASTARTF,ASTART                                                   
         BCTR  R2,0                                                             
         ST    R2,AENDF                                                         
         S     R2,ASTARTF                                                       
         LA    R2,1(R2)                                                         
         STH   R2,LFUN                                                          
ENDDETF  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CALCULATION FORMULA                                    *         
*  INPUT     1 ASTART                                                 *         
*            2 LFUN                                                   *         
*            3 AEND                                                   *         
*  OUTPUT    1 ANSWER-ANS                                             *         
*            2 L'ANSWER-LANS                                          *         
*            3 ERR=X'FF'  -INCORRECT                                  *         
***********************************************************************         
CALC     NTR1                                                                   
         L     R2,ASTART                                                        
         AH    R2,LFUN                                                          
         L     R3,AEND                                                          
*                                                                               
         CLC   0(3,R2),=C'HEX'     IF FUNCTION IS HEX                           
         BNE   BCL1                IN CONTROL BLOCK IT WAS RECOGNIZED           
         LA    R2,3(R2)            AS AN OPERAND                                
         MVI   TYPE,C'X'           WITH LFUN =0                                 
         MVC   LFUN,=H'3'                                                       
*                                                                               
BCL1     CLI   0(R2),X'40'                                                      
         BH    OPER                                                             
         LA    R2,1(R2)                                                         
         CR    R2,R3                                                            
         BNH   BCL1                                                             
         MVI   ERR,X'FF'           NO OPERAND                                   
         B     ENDCALC                                                          
*                                                                               
OPER     ST    R2,AFORMUL          R2=A(FORMULA)                                
         SR    R3,R2                                                            
         LA    R3,1(R3)                                                         
         STC   R3,LFORMUL                                                       
         CLI   TYPE,C'X'                                                        
         BE    CAL1                                                             
         MVI   TYPE,C'0'                                                        
*                                                                               
CAL1     GOTO1 BRACK,DMCB,(LFORMUL,AFORMUL)   LOC BRACKETS IN FORMULA           
         CLI   DMCB,0                                                           
         BE    BRA                FORMULA CONTAINS BRACKETS                     
*                                                                               
         GOTO1 VFORMULA,DMCB,(LFORMUL,AFORMUL),(TYPE,ANS),ACOMFACS,    *        
               VCUREDIT                                                         
         CLI   4(R1),X'FF'                                                      
         BNE   POP                                                              
         MVI   ERR,X'FF'           ERROR                                        
         B     ENDCALC                                                          
*                                                                               
POP      MVC   LANS(1),DMCB+4                                                   
         B     ENDCALC                                                          
*                                  CALC FORMULA WITH BRACKETS                   
BRA      GOTO1 VBRACKET,DMCB,(LFORMUL,AFORMUL),(TYPE,ANS),ACOMFACS,    *        
               VCUREDIT,VFORMULA                                                
         CLI   4(R1),X'FF'                                                      
         BNE   POP                                                              
         MVI   ERR,X'FF'           ERROR                                        
*                                                                               
ENDCALC  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 MOVE ANSWER AFTER THE END OF FORMULA/TO THE BEGINNING  *         
*               IF NO PLACE                                           *         
*  INPUT     1 ANS                                                    *         
*            2 LANS                                                   *         
*  OUTPUT    1 ANSWER IN TWA                                          *         
***********************************************************************         
MOVEANS  NTR1                                                                   
         L     R2,AEND             CALCULATION FREE SPACE                       
         L     R3,AENDFLD                                                       
         SR    R3,R2                                                            
         S     R3,=F'1'                                                         
         C     R3,=F'0'                                                         
         BNH   NOTEN               NOT ENOUGH SPACE                             
         STC   R3,FREE             R3=FREE SPACE                                
*                                                                               
         CLC   FREE(1),LANS                                                     
         BL    NOTEN               NOT ENOUGH SPACE                             
         XR    R4,R4                                                            
         IC    R4,LANS                                                          
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R2),ANS                                                      
         MVC   1(1,R2),=C'='       MOVE =                                       
         B     ENDMOVE                                                          
*                                                                               
NOTEN    L     R5,AFIELD                                                        
         LH    R6,LFIELD                                                        
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SPACES     SPACES CLEANING STRING                        
         XR    R4,R4                                                            
         IC    R4,LANS                                                          
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
GOP2     MVC   7(0,R5),ANS                                                      
         MVC   3(4,R5),=C'ANS='                                                 
ENDMOVE  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 LOC FUNCTION IN TABLE                                  *         
*  INPUT     1 TABLE                                                  *         
*            2 ASTARTF                                                *         
*            3 LFUN                                                   *         
*  OUTPUT    1 REF-A(BLOCK EXECUTION)                                 *         
*            2 RF=1-EXIST                                             *         
*            3 RF=0-NO                                                *         
*            4 RF=2-FORBIDDEN FUNCTION                                *         
***********************************************************************         
RETTAB   NTR1                                                                   
         LA    R2,TABLE                                                         
         L     R3,ASTARTF                                                       
         LH    R4,LFUN                                                          
         LR    R5,R4                                                            
         BCTR  R5,0                                                             
ROM      ZIC   R6,8(R2)            R6=MIN L'FUNCTION                            
         CR    R4,R6                                                            
         BL    BADL                L LESS LIMIT                                 
         ZIC   R6,9(R2)            R6=MAX L'FUNCTION                            
         CR    R4,R6                                                            
         BH    BADL                L MORE LIMIT                                 
*                                                                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R3)       IS CODE EQUAL                                
         BE    FCODE                                                            
BADL     LA    R2,16(R2)                                                        
         CLI   0(R2),X'FF'         END OF TABLE                                 
         BNE   ROM                                                              
         L     RF,=F'0'                                                         
         B     ENDRET                                                           
*                                                                               
FCODE    CLI   10(R2),X'80'        DDS FUNCTION ?                               
         BNE   NODDS                                                            
         CLI   DDS,0               DDS TERMINAL ?                               
         BNE   OK                                                               
*                                                                               
         L     RF,=F'2'            DDS FUNCTION,CLIENT TERMINAL                 
         B     ENDRET                                                           
*                                                                               
OK       EQU   *                                                                
NODDS    L     RF,=F'1'                                                         
         L     R3,12(R2)                                                        
         A     R3,RELO                                                          
         ST    R3,REF                                                           
ENDRET   XIT1  REGS=(RF)                                                        
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 HEXA DECIMAL CALCULATION                               *         
***********************************************************************         
HEX      NTR1                                                                   
         MVI   TYPE,C'X'                                                        
         BAS   RE,CALC                                                          
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CALC ADDRESS OF FIELD IN WHICH CURSER WAS              *         
*            2 READ SCREEN INTO CORE                                  *         
*  INPUT     P1   BYTE   1-3 A(6K AREA)                               *         
*  OUTPUT    P1          0   VALID  1  INVALID                        *         
*            P2          1-3 A(FIELD HDR)                             *         
***********************************************************************         
GETFLD   NTR1                                                                   
         LR    R2,RA               R2=A(TWA)                                    
         LA    R2,64(R2)           R2=A(HDR OF FIRST FIELD)                     
         ST    R2,DMCB+4                                                        
         MVI   DMCB,0                                                           
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 MOVE ANSWER INTO FIELD                                 *         
*  INPUT     P1   BYTE   0   L'ANS                                    *         
*                        1-3 A(ANS)                                   *         
*            P2          1-3 A(HDR)                                   *         
*  OUTPUT    P1          0   0-VALID   1-INVALID                      *         
***********************************************************************         
MOVEFLD  NTR1                                                                   
         ZIC   R2,DMCB             R2=L'ANS                                     
         L     R3,DMCB                                                          
         LA    R3,0(R3)            R3=A(ANS)                                    
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)            R4=A(HDR)                                    
*                  VALIDATION                                                   
         ZIC   R5,0(R4)            R5=L'HDR+MAX DATA                            
         S     R5,=F'8'            R5=L'MAX DATA                                
         CR    R5,R2                                                            
         BNL   MF1                 VALID                                        
         MVI   DMCB,1              INVALID                                      
         B     ENDMF                                                            
*                                                                               
MF1      BCTR  R5,0                R2=L'ANS-1                                   
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),=78C' '     CLEAN FIELD                                  
         BCTR  R2,0                R2=L'ANS-1                                   
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),0(R3)       MOVE ANSWER                                  
         MVI   DMCB,0                                                           
*                                                                               
ENDMF    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CONVERSION DECIMAL TO BINARY                           *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS                                                    *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
***********************************************************************         
CVB      NTR1                                                                   
         GOTO1 OPERA,DMCB,(LFUN+1,ASTARTF),AEND,SETOPERM    LOC OPERAND         
         CLI   DMCB,0                                                           
         BE    GOODB                                                            
         MVI   ERR,X'FF'                                                        
         B     ENDCVB                                                           
*                                                                               
GOODB    ZIC   R2,DMCB+4                                                        
         STC   R2,LOP              R2=L'OPERAND                                 
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP              R3=A(OPERAND)                                
         GOTO1 VCASHVAL,DMCB,(C'N',(R3)),(R2)  CONVERSION TO BINARY             
         CLI   DMCB,0                                                           
         BE    VAL                                                              
         MVI   ERR,X'FF'           INV NUMERIC                                  
         B     ENDCVB                                                           
*                                                                               
VAL      MVC   NUM(4),DMCB+4 NUM=NUMBER IN BINARY                               
         GOTO1 VHEXOUT,DMCB,NUM,ANS,4,=C'TOG'                                   
         CLC   DMCB+16(4),=F'0'                                                 
         BNE   CL                                                               
         MVI   ERR,X'FF'                                                        
         B     ENDCVB                                                           
*                                                                               
CL       L     R4,DMCB+16                                                       
         STC   R4,LANS             R4=L'ANSWER                                  
ENDCVB   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CONVERSION BINARY TO DECIMAL                           *         
*            2 DISPLAYING                                             *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS                                                    *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
***********************************************************************         
CVD      NTR1                                                                   
         GOTO1 OPERA,DMCB,(LFUN+1,ASTARTF),AEND,SETOPER                         
         CLI   DMCB,0                                                           
         BE    GOODD                                                            
         MVI   ERR,X'FF'                                                        
         B     ENDCVD                                                           
*                                                                               
GOODD    ZIC   R2,DMCB+4           R2=L'OPERAND                                 
         STC   R2,LOP                                                           
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP              R3=A(OPERAND)                                
*                                                                               
         GOTO1 CHECKD,DMCB,(LOP,AOP),HEXDEC  CHECK HEXA DECIMAL                 
         CLI   DMCB,0                                                           
         BE    VALHD1                                                           
         MVI   ERR,X'FF'           INVALID                                      
         B     ENDCVD                                                           
*                                                                               
VALHD1   GOTO1 VHEXIN,DMCB,AOP,FWORD,(R2)                                       
         CLC   DMCB+12(4),=F'0'                                                 
         BNE   CHIN1                                                            
         MVI   ERR,X'FF'                                                        
         B     ENDCVD                                                           
*                                                                               
CHIN1    L     RF,FWORD            MOVE FWORD                                   
         LA    R1,8                AFTER                                        
         SR    R1,R2               HEXIN                                        
         SLL   R1,2                                                             
         SRL   RF,0(R1)                                                         
         ST    RF,FWORD                                                         
ED1      EDIT  FWORD,(11,ANS),ALIGN=LEFT,FLOAT=-                                
         STC   R0,LANS             R0=L'ANSWER                                  
ENDCVD   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CONVERSION DATE INTO INTERNAL FORMAT                   *         
*            2 DISPLAYING                                             *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS                                                    *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
***********************************************************************         
DTP      NTR1                                                                   
         GOTO1 OPERA,DMCB,(LFUN+1,ASTARTF),AEND,ALPHANUM                        
         CLI   DMCB,0                                                           
         BE    GOODP                                                            
         MVI   ERR,X'FF'                                                        
         B     ENDDTP                                                           
*                                                                               
GOODP    ZIC   R2,DMCB+4                                                        
         STC   R2,LOP                                                           
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP                                                           
*                                                                               
         GOTO1 VDATVAL,DMCB,AOP,(X'20',DATE)  VALIDATE 1900-2059                
         CLC   DMCB(4),=F'0'                                                    
         BNE   VALP1                                                            
         MVI   ERR,X'FF'                                                        
         B     ENDDTP                                                           
*                                                                               
VALP1    MVC   ISOD,=C'2000-01-01' INIT ISO DATE                                
         MVC   ISOD+2(2),DATE                                                   
         MVC   ISOD+5(2),DATE+2                                                 
         MVC   ISOD+8(2),DATE+4                                                 
         TM    DMCB+4,X'40'        TEST 20XX YEAR INPUT OR ASSUMED              
         BO    VALP2                                                            
         MVC   ISOD+0(2),=C'19'                                                 
         B     VALP3                                                            
VALP2    LLC   RE,ISOD+2           CONVERT 1ST YEAR BYTE                        
         AHI   RE,-10                                                           
         STC   RE,ISOD+2                                                        
*                                                                               
VALP3    CLC   ISOD(4),=C'2027'    TEST DATE > 2027                             
         BNH   VALP4                                                            
         MVC   NUM(2),=X'0000'                                                  
         GOTO1 VDATCON,DMCB,(10,ISOD),(30,NUM+2) CONVERT TO NEW CMPRSD          
         B     VALP6                                                            
*                                                                               
VALP4    CLC   ISOD(4),=C'1964'    TEST DATE < 1964                             
         BNL   VALP5                                                            
         GOTO1 VDATCON,DMCB,(10,ISOD),(02,NUM+0) CONVERT TO OLD CMPRSD          
         MVC   NUM+2(2),=X'0000'                                                
         B     VALP6                                                            
*                                                                               
VALP5    GOTO1 VDATCON,DMCB,(10,ISOD),(02,NUM+0) CONVERT TO OLD CMPRSD          
         GOTO1 VDATCON,DMCB,(10,ISOD),(30,NUM+2) CONVERT TO NEW CMPRSD          
*                                                                               
VALP6    GOTO1 VHEXOUT,DMCB,NUM+0,ANS+0,2,=C'TOG'                               
         MVI   ANS+4,C'/'                                                       
         GOTO1 VHEXOUT,DMCB,NUM+2,ANS+5,2,=C'TOG'                               
         MVI   LANS,9                                                           
ENDDTP   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CONVERSION DATE INTO JULIAN FORMAT                     *         
*            2 DISPLAYING                                             *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS                                                    *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
***********************************************************************         
JDO      NTR1                                                                   
         GOTO1 OPERA,DMCB,(LFUN+1,ASTARTF),AEND,ALPHANUM                        
         CLI   DMCB,0                                                           
         BE    JDO02                                                            
         MVI   ERR,X'FF'                                                        
         B     JDOX                                                             
*                                                                               
JDO02    ZIC   R2,DMCB+4                                                        
         STC   R2,LOP                                                           
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP                                                           
*                                                                               
         GOTO1 VDATVAL,DMCB,AOP,(X'00',DATE)  VALIDATE 1964-2059                
         CLC   DMCB(4),=F'0'                                                    
         BNE   JDO04                                                            
         MVI   ERR,X'FF'                                                        
         B     JDOX                                                             
*                                                                               
JDO04    GOTO1 VDATCON,DMCB,(0,DATE),(19,NUM) CONVERT TO JULIAN (HEX)           
         GOTO1 VHEXOUT,DMCB,NUM,ANS,3,=C'TOG'                                   
         MVI   LANS,6                                                           
JDOX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CONVERSION DATE FROM COMPRESSED FORM TO D(DMMYY)       *         
*            2 DISPLAYING                                             *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS                                                    *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
***********************************************************************         
DTUO     MVI   PTY,02              OLD CMPRSD DATE                              
         J     DTU                                                              
DTUN     MVI   PTY,14              NEW CMPRSD DATE                              
         J     DTU                                                              
*                                                                               
DTU      NTR1                                                                   
         GOTO1 OPERA,DMCB,(LFUN+1,ASTART),AEND,SETOPER                          
         CLI   DMCB,0                                                           
         BE    GOODU                                                            
         MVI   ERR,X'FF'                                                        
         B     ENDDTU                                                           
*                                                                               
GOODU    ZIC   R2,DMCB+4                                                        
         C     R2,=F'5'            CHECK L'INPUT                                
         BL    GOODL                                                            
         MVI   ERR,X'FF'           INV L'INPUT                                  
         B     ENDDTU                                                           
*                                                                               
GOODL    STC   R2,LOP                                                           
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP                                                           
*                                                                               
         GOTO1 CHECKD,DMCB,(LOP,AOP),HEXDEC                                     
         CLI   DMCB,0                                                           
         BE    VALHD2                                                           
         MVI   ERR,X'FF'                                                        
         B     ENDDTU                                                           
*                                                                               
VALHD2   GOTO1 VHEXIN,DMCB,AOP,FWORD,4        FWORD=BINARY INPUT                
         CLC   DMCB+12(4),=F'0'                                                 
         BNE   CHIN2                                                            
         MVI   ERR,X'FF'                                                        
         B     ENDDTU                                                           
*                                                                               
CHIN2    GOTO1 VDATCON,DMCB,(PTY,FWORD),(8,ANS)                                 
*                                                                               
         GOTO1 VDATVAL,DMCB,ANS,(X'00',DATE)  VALIDATE 1964-2059                
         CLC   DMCB(4),=F'0'                                                    
         BNE   HIS                                                              
         MVI   ERR,X'FF'           INVAAAALID INPUT                             
         B     ENDDTU                                                           
*                                                                               
HIS      CLI   ANS,C' '            DD  OR  D  FOR DAY ?                         
         BNE   CHIN3               DD                                           
         MVC   ANS(6),ANS+1                                                     
         MVI   LANS,6              6 BYTE DMMMYY                                
         B     ENDDTU                                                           
*                                                                               
CHIN3    EQU   *                   WAS 7 FOR UK                                 
*&&UK*&& MVI   LANS,8              8 BYTE DDMMMYY - DD.MM.YY                    
*&&US*&& MVI   LANS,8              8 BYTE MMMDD/YY (US)                         
ENDDTU   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CONVERSION DATE FROM JULIAN FORM TO D(DMMYY)           *         
*            2 DISPLAYING                                             *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS                                                    *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
***********************************************************************         
JDI      NTR1                                                                   
         GOTO1 OPERA,DMCB,(LFUN+1,ASTART),AEND,SETOPER                          
         CLI   DMCB,0                                                           
         BE    JDI02                                                            
         MVI   ERR,X'FF'                                                        
         B     JDIX                                                             
*                                                                               
JDI02    ZIC   R2,DMCB+4                                                        
         CHI   R2,6                CHECK L'INPUT                                
         BE    JDI04                                                            
         MVI   ERR,X'FF'           INV L'INPUT                                  
         B     JDIX                                                             
*                                                                               
JDI04    STC   R2,LOP                                                           
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP                                                           
*                                                                               
         GOTO1 CHECKD,DMCB,(LOP,AOP),HEXDEC                                     
         CLI   DMCB,0                                                           
         BE    JDI06                                                            
         MVI   ERR,X'FF'                                                        
         B     JDIX                                                             
*                                                                               
JDI06    GOTO1 VHEXIN,DMCB,AOP,FWORD,6        FWORD=BINARY INPUT                
         CLC   DMCB+12(4),=F'0'                                                 
         BNE   JDI08                                                            
         MVI   ERR,X'FF'                                                        
         B     JDIX                                                             
*                                                                               
JDI08    GOTO1 VDATCON,DMCB,(8,FWORD),(8,ANS)                                   
*                                                                               
         GOTO1 VDATVAL,DMCB,ANS,(X'00',DATE)  VALIDATE 1964-2059                
         CLC   DMCB(4),=F'0'                                                    
         BNE   JDI10                                                            
         MVI   ERR,X'FF'           INVAAAALID INPUT                             
         B     JDIX                                                             
*                                                                               
JDI10    CLI   ANS,C' '            DD  OR  D  FOR DAY ?                         
         BNE   JDI12               DD                                           
         MVC   ANS(6),ANS+1                                                     
         MVI   LANS,6              6 BYTE DMMMYY                                
         B     JDIX                                                             
*                                                                               
JDI12    DS    0H                  WAS 7 FOR UK                                 
*&&UK*&& MVI   LANS,8              8 BYTE DDMMMYY - DD.MM.YY                    
*&&US*&& MVI   LANS,8              8 BYTE MMMDD/YY (US)                         
JDIX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CALCULATION DAY OF WEEK                                *         
*            2 DISPLAYING                                             *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS                                                    *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
***********************************************************************         
GETD     NTR1                                                                   
         GOTO1 OPERA,DMCB,(LFUN+1,ASTARTF),AEND,ALPHANUM                        
         CLI   DMCB,0                                                           
         BE    GET1                                                             
         MVI   ERR,X'FF'                                                        
         B     ENDGET                                                           
*                                                                               
GET1     ZIC   R2,DMCB+4                                                        
         STC   R2,LOP                                                           
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP                                                           
*                                                                               
GET2     GOTO1 VDATVAL,DMCB,AOP,(X'00',DATE)  VALIDATE 1964-2059                
         CLC   DMCB(4),=F'0'                                                    
         BNE   GET3                                                             
         MVI   ERR,X'FF'                                                        
         B     ENDGET                                                           
*                                                                               
GET3     GOTO1 VGETDAY,DMCB,DATE,ANS   GET DAY OF WEEK                          
         CLC   ANS(3),=3C' '                                                    
         BNE   GET4                                                             
         MVI   ERR,X'FF'                                                        
         B     ENDGET                                                           
*                                                                               
GET4     MVI   LANS,3                                                           
ENDGET   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CALCULATION DAY OF WEEK                                *         
*            2 CALCULATION NEW DATE                                   *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS=DAY,DATE                                           *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
***********************************************************************         
ADDAY    NTR1                                                                   
         GOTO1 OPERA,DMCB,(LFUN+1,ASTARTF),AEND,ALPHANUM  LOC FIRST OP          
         CLI   DMCB,0                                                           
         BE    ADA3                                                             
         MVI   ERR,X'FF'                                                        
         B     ENDADA                                                           
*                                                                               
ADA3     ZIC   R2,DMCB+4                                                        
         STC   R2,LOP1          R2=LOP1=L'FIRST OPERAND                         
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP1          R3=AOP1=A(FIRST OPERAND)                        
*                                                                               
ADA4     GOTO1 DEL,DMCB,(LOP1,AOP1),SET1,AEND      LOC                          
         CLI   DMCB,0             DELIMITER                                     
         BE    ADA5                                                             
         MVI   ERR,X'FF'                                                        
         B     ENDADA                                                           
*                                                                               
ADA5     L     R6,DMCB+4                                                        
         LA    R6,0(R6)                                                         
         ST    R6,ADEL1            R6=ADEL1=A(DELIMITER)                        
*                                                                               
ADA6     GOTO1 OPERA,DMCB,(1,ADEL1),AEND,SETNUM                                 
         CLI   DMCB,0                                                           
         BE    ADA7                                                             
         MVI   ERR,X'FF'           NO SECOND OPERAND                            
         B     ENDADA                                                           
*                                                                               
ADA7     ZIC   R4,DMCB+4                                                        
         STC   R4,LOP2             R4=LOP2=L'SECOND OPERAND                     
         L     R5,DMCB+4                                                        
         LA    R5,0(R5)                                                         
         ST    R5,AOP2             R5=AOP2=A(SECOND OPERAND)                    
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PC1,0(R3)                                                        
*                                                                               
ADA8     GOTO1 VDATVAL,DMCB,PC1,(X'00',DATE)  VALIDATE 1964-2059                
         CLC   DMCB(4),=F'0'                                                    
         BNE   ADA9                DATE=YYMMDD                                  
         MVI   ERR,X'FF'                                                        
         B     ENDADA                                                           
*                                                                               
ADA9     XC    PC1,PC1             DATE DEFINITION                              
         MVC   WORK(17),=17C' '                                                 
         MVI   ANS+3,C','          ANS=DAY,                                     
         MVC   PC1(1),0(R6)        MOVE SIGN                                    
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PC1+1(0),0(R5)      MOVE NUMBER                                  
         LA    R4,1(R4)                                                         
         GOTO1 VCASHVAL,DMCB,(C'N',PC1),(R4)  CONVERSION TO BINARY              
         CLI   DMCB,0                                                           
         BE    ADA10                                                            
         MVI   ERR,X'FF'           INV NUMERIC                                  
         B     ENDADA                                                           
*                                                                               
ADA10    L     R5,DMCB+4           NUM=NUMBER IN BINARY                         
         GOTO1 VADDAY,DMCB,DATE,ANS+4,(R5)         ANS=000,YYMMDD               
         GOTO1 VGETDAY,DMCB,ANS+4,ANS              ANS=DAY,YYMMDD               
         GOTO1 VDATCON,DMCB,(0,ANS+4),(8,WORK)     WORK=D(D)MMMYY               
*&&US                                                                           
         MVC   ANS+4(8),WORK       ANS=DAY,MMMDD/YY                             
         MVI   LANS,12                                                          
*&&                                                                             
*&&UK                                                                           
         MVC   ANS+4(7),WORK       ANS=DAY,DDMMMYY                              
         CLI   WORK,C' '                                                        
         BNE   ADA11                                                            
         MVI   ANS+4,C'0'                                                       
         MVI   LANS,11             6 BYTE DMMMYY                                
*&&                                                                             
         B     ENDADA                                                           
*                                                                               
ADA11    MVI   LANS,11             7 BYTE DDMMMYY                               
*                                                                               
ENDADA   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CONVERT DECIMAL SECS SINCE 2000-01-01 TO DATE/TIME     *         
*            2 DISPLAYING                                             *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS                                                    *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
***********************************************************************         
ACDUNPK  NTR1                                                                   
         GOTO1 OPERA,DMCB,(LFUN+1,ASTART),AEND,ALPHANUM                         
         CLI   DMCB,0                                                           
         BNE   ACDUERR                                                          
         SR    R2,R2                                                            
         IC    R2,DMCB+4                                                        
         STC   R2,LOP                                                           
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP                                                           
                                                                                
         GOTO1 CHECKN,DMCB,(LOP,AOP),SETNUM  CHECK NUMERIC, MAX 15              
         CLI   DMCB,0                                                           
         BNE   ACDUERR                                                          
                                                                                
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         SR    RE,RE               CLEAR CVB OVERFLOW                           
         CP    DUB,=P'2147483647'  MAXIMUM TO CVB                               
         BNH   ACDU010                                                          
         SP    DUB,=P'2147483647'                                               
         CP    DUB,=P'2147483647'  > MAXIMUM UNSIGNED FULLWORD                  
         BH    ACDUERR                                                          
         L     RE,=F'2147483647'   RE=CVB OVERFLOW                              
ACDU010  CVB   RF,DUB                                                           
         AR    RF,RE               ADD BACK ANY CVB OVERFLOW                    
         ST    RF,FWORD            UNSIGNED FULLWORD SECONDS                    
         GOTOR VDATCON,DMCB,(12,FWORD),(25,TEMP)                                
         GOTOR (RF),(R1),(3,TEMP),(23,ANS)                                      
         EDIT  (1,TEMP+3),(2,ANS+11),FILL=0                                     
         MVI   ANS+13,C':'                                                      
         EDIT  (1,TEMP+4),(2,ANS+14),FILL=0                                     
         MVI   ANS+16,C':'                                                      
         EDIT  (1,TEMP+5),(2,ANS+17),FILL=0                                     
                                                                                
         MVI   LANS,19             YYYY-MM-DD HH:MM:SS                          
         B     ACDUX                                                            
                                                                                
ACDUERR  MVI   ERR,X'FF'           INV L'INPUT                                  
                                                                                
ACDUX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CONVERT BINARY SECS SINCE 2000-01-01 TO DATE/TIME      *         
*            2 DISPLAYING                                             *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS                                                    *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
***********************************************************************         
ACBUNPK  NTR1                                                                   
         GOTO1 OPERA,DMCB,(LFUN+1,ASTART),AEND,ALPHANUM                         
         CLI   DMCB,0                                                           
         BNE   ACBUERR                                                          
         SR    R2,R2                                                            
         IC    R2,DMCB+4                                                        
         STC   R2,LOP                                                           
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP                                                           
                                                                                
         GOTO1 CHECKD,DMCB,(LOP,AOP),HEXDEC                                     
         CLI   DMCB,0                                                           
         BNE   ACBUERR                                                          
         GOTO1 VHEXIN,DMCB,AOP,FWORD,(R2)                                       
         CLC   DMCB+12(4),=F'0'                                                 
         BE    ACBUERR                                                          
         L     RF,FWORD            MOVE FWORD                                   
         LA    R1,8                AFTER                                        
         SR    R1,R2               HEXIN                                        
         SLL   R1,2                                                             
         SRL   RF,0(R1)                                                         
         ST    RF,FWORD                                                         
                                                                                
         GOTOR VDATCON,DMCB,(12,FWORD),(25,TEMP)                                
         GOTOR (RF),(R1),(3,TEMP),(23,ANS)                                      
         EDIT  (1,TEMP+3),(2,ANS+11),FILL=0                                     
         MVI   ANS+13,C':'                                                      
         EDIT  (1,TEMP+4),(2,ANS+14),FILL=0                                     
         MVI   ANS+16,C':'                                                      
         EDIT  (1,TEMP+5),(2,ANS+17),FILL=0                                     
                                                                                
         MVI   LANS,19             YYYY-MM-DD HH:MM:SS                          
         B     ACBUX                                                            
                                                                                
ACBUERR  MVI   ERR,X'FF'           INV L'INPUT                                  
                                                                                
ACBUX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CONVERT DATE/TIME TO SECONDS SINCE 2000-01-01          *         
*            2 DISPLAY - ACDPACK DECIMAL, E.G. 118775398              *         
*                      - ACBPACK BINARY, E.G. 07145E66                *         
*  INPUT     1 ASTARTF                                                *         
*            2 LFUN                                                   *         
*  OUTPUT    1 ANS                                                    *         
*            2 LANS                                                   *         
*            3 ERR=X'FF'-ERROR                                        *         
*                                                                     *         
*  INPUT     DATE VARIOUS DATVAL FORMATS                              *         
*            YYYY OK <= 2027 (DATVAL LIMIT), ELSE USE YY FORMAT       *         
*            TIME FIXED 8 BYTES, LEADING ZEROES, 24HR CLOCK           *         
*            E.G.  2001-12-31 09:12:44                                *         
*                  01JAN05 13-10-00                                   *         
*                  01.12.28 23.59.59                                  *         
***********************************************************************         
ACBPACK  LHI   R0,1                                                             
         J     *+6                                                              
ACDPACK  SR    R0,R0                                                            
         NTR1  ,                                                                
         GOTO1 OPERA,DMCB,(LFUN+1,ASTARTF),AEND,SETACP1  1ST OPERAND            
         CLI   DMCB,0                                                           
         BNE   ACPAERR                                                          
         SR    R2,R2                                                            
         IC    R2,DMCB+4                                                        
         STC   R2,LOP1             R2=LOP1=L'FIRST OPERAND                      
         L     R3,DMCB+4                                                        
         LA    R3,0(R3)                                                         
         ST    R3,AOP1             R3=AOP1=A(FIRST OPERAND)                     
                                                                                
         LR    R1,R3               LOOK FOR GAP                                 
         LHI   RE,1                                                             
         L     RF,AEND                                                          
         CLI   0(R1),C' '                                                       
         BNH   ACPA010                                                          
         BXLE  R1,RE,*-8                                                        
         B     ACPAERR             NO GAP                                       
ACPA010  ST    R1,ADEL1            R1=ADEL1=A(DELIMITER)                        
                                                                                
         GOTO1 OPERA,DMCB,(1,ADEL1),AEND,SETACP2         2ND OPERAND            
         CLI   DMCB,0                                                           
         BNE   ACPAERR                                                          
         SR    R4,R4                                                            
         IC    R4,DMCB+4                                                        
         CHI   R4,8                HH:MM:SS                                     
         BNE   ACPAERR                                                          
         STC   R4,LOP2             R4=LOP2=L'SECOND OPERAND                     
         L     R5,DMCB+4                                                        
         LA    R5,0(R5)                                                         
         ST    R5,AOP2             R5=AOP2=A(SECOND OPERAND)                    
                                                                                
         GOTO1 VDATVAL,DMCB,AOP1,(X'00',DATE)  VALIDATE 1964-2059               
         CLC   DMCB(4),=F'0'                                                    
         BE    ACPAERR                                                          
         XC    DATE,DATE           X'YYMMDD......'                              
         GOTO1 VDATCON,DMCB,(0,WORK),(3,DATE)                                   
         XC    DUB,DUB                                                          
         PACK  DUB+6(2),0(2,R5)                                                 
         CVB   RF,DUB              X'YYMMDDHH....'                              
         CHI   RF,23                                                            
         BH    ACPAERR                                                          
         STC   RF,DATE+3                                                        
         PACK  DUB+6(2),3(2,R5)                                                 
         CVB   RF,DUB                                                           
         CHI   RF,59                                                            
         BH    ACPAERR                                                          
         STC   RF,DATE+4           X'YYMMDDHHMM..'                              
         PACK  DUB+6(2),6(2,R5)                                                 
         CVB   RF,DUB                                                           
         CHI   RF,59                                                            
         BH    ACPAERR                                                          
         STC   RF,DATE+5           X'YYMMDDHHMMSS'                              
                                                                                
         GOTO1 VDATCON,DMCB,(13,DATE),(24,NUM)                                  
                                                                                
         LTR   R0,R0               TEST BINARY/DECIMAL FORMAT OUTPUT            
         BZ    ACPA020             DECIMAL                                      
         GOTO1 VHEXOUT,DMCB,NUM,ANS,4,=C'TOG'                                   
         ICM   RF,15,DMCB+16                                                    
         BZ    ACPAERR                                                          
         STC   RF,LANS                                                          
         B     ACPAX                                                            
                                                                                
ACPA020  SR    RE,RE               CVD MAX IS 2147483647                        
         L     RF,NUM              IF NUM > 2147483647                          
         SLDL  RE,1                SUBTRACT BUT REMEMBER IF THERE               
         SRL   RF,1                AND READJUST ROOT NUMBER                     
         CVD   RF,DUB                                                           
         LTR   RE,RE               AND IF THE X'80' BIT WAS ON                  
         BZ    *+10                                                             
         AP    DUB,=P'2147483648'  ADD THAT BACK                                
         EDIT  (P8,DUB),(15,ANS),ALIGN=LEFT                                     
         STC   R0,LANS             R0=L'ANSWER                                  
         B     ACPAX                                                            
                                                                                
ACPAERR  MVI   ERR,X'FF'                                                        
                                                                                
ACPAX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CHECKING HEXA DECIMAL,CHECKING LENGTH NOT MORE 8       *         
*  INPUT     P1  BYTE O     L'OPERAND                                 *         
*                     1-3   A(OPERAND)                                *         
*            P2       1-3   A(SET OF HEXA DECIMAL CHARACTERS)         *         
*  OUTPUT    P1       0     X'00'-CORRECT,X'FF'-INVALID               *         
***********************************************************************         
CHECKD   NTR1                                                                   
         ZIC   R2,DMCB             R2=L'OPERAND                                 
         C     R2,=F'8'                                                         
         BH    BADCH               INVALID LENGTH                               
         L     R3,DMCB             R3=A(OPERAND)                                
         LA    R3,0(R3)                                                         
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)            R4=A(H-D SET)                                
*                                                                               
CH1      CLC   0(1,R3),0(R4)                                                    
         BE    CH2                 GOOD SYMBOL                                  
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'         END OF SET?                                  
         BE    BADCH               INVALID                                      
         B     CH1                                                              
*                                                                               
CH2      LA    R3,1(R3)                                                         
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)                                                         
         BCT   R2,CH1                                                           
*                                                                               
         MVI   DMCB,0                                                           
         B     ENDCHD                                                           
*                                                                               
BADCH    MVI   DMCB,X'FF'                                                       
ENDCHD   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 CHECK VALID CHARACTERS, LENGTH NOT >15                 *         
*  INPUT     P1  BYTE O     L'OPERAND                                 *         
*                     1-3   A(OPERAND)                                *         
*            P2       1-3   A(SET OF CHECK CHARACTERS)                *         
*  OUTPUT    P1       0     X'00'-CORRECT,X'FF'-INVALID               *         
***********************************************************************         
CHECKN   NTR1                                                                   
         ZIC   R2,DMCB             R2=L'OPERAND                                 
         CHI   R2,15                                                            
         BH    BADCHN              INVALID LENGTH                               
         L     R3,DMCB             R3=A(OPERAND)                                
         LA    R3,0(R3)                                                         
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)            R4=A(H-D SET)                                
*                                                                               
CHN1     CLC   0(1,R3),0(R4)                                                    
         BE    CHN2                GOOD SYMBOL                                  
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'         END OF SET?                                  
         BE    BADCHN              INVALID                                      
         B     CHN1                                                             
*                                                                               
CHN2     LA    R3,1(R3)                                                         
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)                                                         
         BCT   R2,CHN1                                                          
*                                                                               
         MVI   DMCB,0                                                           
         B     ENDCHND                                                          
*                                                                               
BADCHN   MVI   DMCB,X'FF'                                                       
ENDCHND  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 RETRIEVAL OPERAND                                      *         
*  INPUT     P1  BYTE  0     L'PREVIOUS FUNCTION/DELIMITER            *         
*                      1-3   A(PREVIOUS FUNCTION/DELIMITER)           *         
*            P2        1-3   A'END OF STRING                          *         
*            P3        1-3   A(SET OF POSSIBLE SYMBOLS OF OPERAND)    *         
*  OUTPUT    P1        0     X'00'-OPERAND EXIST,X'FF'-NO OPERAND     *         
*            P2        O     L'OPERAND                                *         
*                      1-3   A(OPERAND)                               *         
***********************************************************************         
OPERA    NTR1                                                                   
         ZIC   R2,DMCB             R2=L'PREVIOUS FUNCTION/DELIMITER             
         L     R3,DMCB                                                          
         LA    R3,0(R3)            R3=A(PREVIOUS FUNCTION/DELIMITER)            
         AR    R3,R2               R3=START ADDRESS FOR LOCATION                
         L     R7,DMCB+4                                                        
         LA    R7,1(R7)            R7=A(END OF STRING)+1                        
*                                                                               
CLOP1    CLI   0(R3),X'40'                                                      
         BH    CLOP2               FIRST SYMBOL                                 
         LA    R3,1(R3)                                                         
         CR    R3,R7                                                            
         BNE   CLOP1                                                            
*                                                                               
         MVI   DMCB,X'FF'          NO OPERAND                                   
         B     ENDOP                                                            
*                                                                               
CLOP2    LR    R6,R3               R3=R6=A(OPERAND)                             
         XR    R8,R8                                                            
CLOP3    L     R4,DMCB+8                                                        
         LA    R4,0(R4)            R4=A(SET OF POSSIBLE SYMBOLS OF OP           
CLOP4    CLC   0(1,R3),0(R4)                                                    
         BE    CLOP5               GOOD CHARACTER                               
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BE    CLOP6               END OF OPERAND                               
         B     CLOP4                                                            
*                                                                               
CLOP5    LA    R3,1(R3)                                                         
         LA    R8,1(R8)                                                         
         CR    R3,R7                                                            
         BNE   CLOP3                                                            
*                                                                               
CLOP6    C     R8,=F'0'                                                         
         BNE   CLOP7               OPERAND EXISTS                               
         MVI   DMCB,X'FF'          NO OPERAND                                   
         B     ENDOP                                                            
*                                                                               
CLOP7    ST    R6,DMCB+4           R6=A(OPERAND)                                
         STC   R8,DMCB+4           R8=L'OPERAND                                 
         MVI   DMCB,0                                                           
ENDOP    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 VALIDATE L'INPUT  FOR DATE PACKING                     *         
*  INPUT     P1  BYTE  0     L'OPERAND                                *         
*                      1-3   A(OPERAND)                               *         
*            P2        1-3   A(NUMERIC SET)                           *         
*  OUTPUT    P1        0     X'00'-VALID         X'FF'-INVALID        *         
***********************************************************************         
VALLEN   NTR1                                                                   
         ZIC   R2,DMCB             R2=L'OPERAND                                 
         L     R3,DMCB                                                          
         LA    R3,0(R3)            R3=A(OPERAND)                                
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)            R4=A(NUMERIC SET)                            
*                                                                               
         LA    R3,1(R3)            R3=A(SECOND SYMBOL OF DATE)                  
V1       CLC   0(1,R3),0(R4)       IS SECOND SYMBOL NUMERIC                     
         BE    V2                  NUMERIC                                      
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BE    V3                  ALPHABETICAL                                 
         B     V1                                                               
*                                                                               
V2       C     R2,=F'7'            L'OPERAND=7 IF SECOND SYMBOL IS              
         BE    V4                  VALID                 NUMERIC                
         MVI   DMCB,X'FF'          INVALID                                      
         B     ENDVLL                                                           
*                                                                               
V3       C     R2,=F'6'            L'OPERAND=6 IF SECOND SYMBOL IS              
         BE    V4                                      ALPHABETICAL             
         MVI   DMCB,X'FF'          INVALID                                      
         B     ENDVLL                                                           
*                                                                               
V4       MVI   DMCB,0              VALID                                        
ENDVLL   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 LOC  DELIMITER                                         *         
*  INPUT     P1   BYTE   0   L'OPERAND                                *         
*                        1-3 A(OPERAND)                               *         
*            P2          1-3 A(DELIMITER SET)                         *         
*            P3          1-3 A(END OF FORMULA)                        *         
*  OUTPUT    P1          0   X'00'/X'FF' IF NO DELIMITER              *         
*            P2          1-3 A(DELIMITER                              *         
*                        0-4 ZERO IF NO DELIMITER AND ALL SPACES      *         
*                            AFTER OPERAND                            *         
***********************************************************************         
DEL      NTR1                                                                   
         ZIC   R2,DMCB             R2=L'OPERAND                                 
         L     R3,DMCB                                                          
         LA    R3,0(R3)            R3=A(OPERAND)                                
         AR    R3,R2               R3=START ADDRESS FOR LOCATION                
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)            R4=A(DELIMITER SET)                          
         L     R5,DMCB+8                                                        
         LA    R5,1(R5)            R5=A(END OF FORMULA +1)                      
*                                                                               
         CR    R3,R5                                                            
         BE    FIL                 END OF FORMULA,NO ERROR                      
D1       CLI   0(R3),X'40'         LOC OF DELIMITER                             
         BH    D2                                                               
         LA    R3,1(R3)                                                         
         B     D1                                                               
FIL      MVI   DMCB,X'FF'          NO DELIMITER BUT                             
         MVC   DMCB+4(4),=F'0'     ALL SPASEC                                   
         B     ENDDEL                                                           
*                                                                               
D2       CLC   0(1,R3),0(R4)       IS IT DELIMITER                              
         BE    D3                                                               
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'         END OF DELIMITER SET ?                       
         BNE   D2                                                               
*                                                                               
         MVI   DMCB,X'FF'          NO DELIMITER                                 
         B     ENDDEL              NOT ALL SPACES                               
*                                                                               
D3       ST    R3,DMCB+4           R3=A(DELIMITER)                              
         MVI   DMCB,0                                                           
ENDDEL   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  FUNCTIONS 1 LOC  BRACKETS IN FORMULA                               *         
*  INPUT     P1   BYTE   0   L'FORMULA                                *         
*                        1-3 A(FORMULA)                               *         
*  OUTPUT    P1          0   0-BRACKETS  EXIST,1-DONT EXIST           *         
***********************************************************************         
BRACK    NTR1                                                                   
         ZIC   R2,DMCB             R2=L'FORMULA                                 
         L     R3,DMCB                                                          
         LA    R3,0(R3)            R3=A(FORMULA)                                
*                                                                               
BR1      CLI   0(R3),C'('                                                       
         BE    BR2                                                              
         CLI   0(R3),C')'                                                       
         BE    BR2                                                              
         LA    R3,1(R3)                                                         
         BCT   R2,BR1                                                           
         B     BR3                                                              
*                                                                               
BR2      MVI   DMCB,0                                                           
         B     ENDBR                                                            
*                                                                               
BR3      MVI   DMCB,1                                                           
ENDBR    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DISPL    DC    H'0'                                                             
LFIELD   DC    H'60'                                                            
*                                                                               
SET      DC    X'0040FF'                                                        
SETDEL   DC    C'+-*X/%',X'4000FF'                                              
SPACES   DC    80C' '                                                           
*                                                                               
TABLE    DS    04F                                                              
         DC    CL8'HEX     ',AL1(1,3),X'8000',A(HEX)                            
         DC    CL8'CVB     ',AL1(3,3),X'8000',A(CVB)                            
         DC    CL8'CVD     ',AL1(3,3),X'8000',A(CVD)                            
         DC    CL8'DTUNPK  ',AL1(3,6),X'8000',A(DTUO)                           
         DC    CL8'DNUNPK  ',AL1(3,6),X'8000',A(DTUN)                           
         DC    CL8'DTPACK  ',AL1(3,6),X'8000',A(DTP)                            
         DC    CL8'DNPACK  ',AL1(3,6),X'8000',A(DTP)                            
         DC    CL8'CALC    ',AL1(1,4),X'0000',A(CALC)                           
         DC    CL8'GETDAY  ',AL1(4,6),X'8000',A(GETD)                           
         DC    CL8'ADDAY   ',AL1(4,5),X'8000',A(ADDAY)                          
         DC    CL8'ACDUNPK ',AL1(4,7),X'8000',A(ACDUNPK)                        
         DC    CL8'ACBUNPK ',AL1(4,7),X'8000',A(ACBUNPK)                        
         DC    CL8'ACDPACK ',AL1(4,7),X'8000',A(ACDPACK)                        
         DC    CL8'ACBPACK ',AL1(4,7),X'8000',A(ACBPACK)                        
         DC    CL8'JDIN    ',AL1(3,4),X'8000',A(JDI)                            
         DC    CL8'JDOUT   ',AL1(3,5),X'8000',A(JDO)                            
         DC    X'FF'                                                            
HEXDEC   DC    C'0123456789ABCDEF'                                              
         DC    X'FF'                                                            
*                                                                               
SETOPER  DC    C'0123456789ABCDEF.,'  POSSIBLE SYMBOLS OF OPERAND               
         DC    X'FF'                                                            
SETOPERM DC    C'0123456789ABCDEF.,-' POSSIBLE SYMBOLS OF NEGATIVE              
         DC    X'FF'                                      OPERAND               
ALPHANUM DC    C'0123456789QWERTYUIOPASDFGHJKLZXCVBNM/'       POSSIBLE          
         DC    X'FF'                  SYMBOLS OF OPERAND FOR DATE PACK          
SETNUM   DC    C'0123456789'                                                    
         DC    X'FF'                                                            
SET1     DC    C'+-'                                                            
         DC    X'FF'                                                            
SETACP1  DC    C'0123456789QWERTYUIOPASDFGHJKLZXCVBNM-/:.'                      
SETACP2  DC    C'0123456789-/:.\'                                               
         DC    X'FF'                                                            
         EJECT                                                                  
DATD     DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
NUM      DS    F                                                                
RELO     DS    F                                                                
REF      DS    A                                                                
*                                                                               
RECLEN   DS    H                                                                
         DS    H                                                                
APLIST   DS    F                                                                
ATIA     DS    A                                                                
ACOMFACS DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VCASHVAL DS    V                                                                
VDATCON  DS    V                                                                
VDATVAL  DS    V                                                                
VFORMULA DS    V                                                                
VBRACKET DS    V                                                                
VCUREDIT DS    V                                                                
VGETDAY  DS    V                                                                
VADDAY   DS    V                                                                
*                                                                               
ASTART   DS    A                                                                
AEND     DS    A                                                                
AFIELD   DS    A                                                                
AENDFLD  DS    A                                                                
AOP1     DS    A                                                                
AOP2     DS    A                                                                
ADEL1    DS    A                                                                
PC1      DS    CL16                                                             
AOP      DS    A                                                                
FWORD    DS    F                                                                
AFORMUL  DS    A                                                                
AHED     DS    A                                                                
*                                                                               
ASTARTF  DS    A                                                                
AENDF    DS    A                                                                
LFUN     DS    H                                                                
LSTR     DS    H                                                                
*                                                                               
MOVE     DS    CL1                                                              
ERR      DS    CL1                                                              
DELIM    DS    CL1                                                              
ANS      DS    CL60                                                             
LANS     DS    CL1                                                              
LFORMUL  DS    CL1                                                              
FREE     DS    CL1                                                              
TYPE     DS    CL1                                                              
DDS      DS    CL1                                                              
PTY      DS    XL1                                                              
*                                                                               
LOP      DS    CL1                                                              
LOP1     DS    CL1                                                              
LOP2     DS    CL1                                                              
*                                                                               
WORK     DS    CL17                                                             
         DS    0F                                                               
DATE     DS    CL12                                                             
ISOD     DS    CL10                YYYY-MM-DD                                   
         DS    CL2                                                              
TEMP     DS    CL32                                                             
DATX     DS    0C                                                               
         EJECT                                                                  
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SRCCRFFD DSECT                                                                  
         DS    CL64                                                             
*SRCCRFFD                                                                       
       ++INCLUDE SRCCRFFD                                                       
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRCCR00   11/09/18'                                      
         END                                                                    
