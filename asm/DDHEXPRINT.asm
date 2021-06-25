*          DATA SET DDHEXPRINT AT LEVEL 038 AS OF 05/01/02                      
*PHASE HEXPRINA HEXPRINT                                                        
*INCLUDE DECODE                                                                 
*INCLUDE HEXOUT                                                                 
         PRINT GEN                                                              
         EJECT                                                                  
HEXPRINT CSECT                                                                  
         NBASE ENDATA-DATA,XXHPRTXX,R9,WORK=A(WORK),CLEAR=YES                   
         LA    RA,DISKIN                                                        
         USING IHADCB,RA                                                        
         USING DATA,RC                                                          
         SPACE 1                                                                
*                                  EQUATES                                      
QMAXSRCH EQU   8                                                                
QMAXRNGE EQU   8                                                                
*                                  INITIALISE STORAGE                           
INIT01   MVI   CLEAR,C' '                                                       
         MVC   CLEAR+1(L'CLEAR-1),CLEAR                                         
         MVC   OAREA,CLEAR                                                      
         MVC   HEADER,CLEAR                                                     
         MVC   DSINFO,CLEAR                                                     
*                                  GET DATE & STORE IN TODAY                    
         LA    R1,2                                                             
         SVC   11                                                               
         BAS   RE,JULIAN                                                        
         MVC   TODAY,DATE                                                       
         EJECT                                                                  
***************************************************                             
*           OPEN OUTPUT DATA SET                  *                             
***************************************************                             
         SPACE 1                                                                
OUT1     OPEN  (SYSPRINT,OUTPUT)                                                
         EJECT                                                                  
***************************************************                             
*        READ AND VALIDATE CONTROL CARDS          *                             
***************************************************                             
         SPACE 1                                                                
         OPEN  (SYSIN,INPUT)                                                    
         MVI   PRASA,C' '                                                       
         MVC   PRINT+55(21),=C'--- CONTROL CARDS ---'                           
         PUT   SYSPRINT,OAREA                                                   
         MVI   PRASA,C'0'                                                       
CARDIN   GET   SYSIN                                                            
         LR    R6,R1                                                            
         AH    R1,=H'79'                                                        
         ST    R1,FULL1            FULL1=MAX POS OF R6                          
         MVC   OAREA+1(80),0(R6)                                                
         PUT   SYSPRINT,OAREA                                                   
         MVI   PRASA,C' '                                                       
         MVC   OAREA,CLEAR         IN CASE OF ERRORS                            
         CLI   0(R6),C'*'                                                       
         BE    CARDIN              *=IGNORE CARD                                
         B     CC2                                                              
CC1      C     R6,FULL1                                                         
         BNL   CARDIN                                                           
         LA    R6,1(R6)                                                         
CC2      CLI   0(R6),C' '          FIND NON SPACE                               
         BE    CC1                                                              
         LA    R5,KEYTAB                                                        
KEY1     L     RF,0(R5)            GET ADDRESS                                  
         LTR   RF,RF               IF ZERO                                      
         BZ    ERROR1              END OF TABLE                                 
         IC    R1,4(R5)            GET LENGTH                                   
         EX    R1,COMP             TEST FOR KEY                                 
         BER   RF                                                               
         LA    R5,L'KEYTAB(R5)     NEXT KEYWORD                                 
         B     KEY1                                                             
         EJECT                                                                  
***************************************************                             
*           MAXRECS KEYWORD ROUTINE               *                             
***************************************************                             
         SPACE 1                                                                
MXRCS    EQU   *                                                                
         LA    R6,7(R6)            SKIP KEYWORD                                 
         LA    R0,3                TRY 3 TIMES                                  
MR1      TM    0(R6),X'F0'         TO FIND NUMBER                               
         BO    MR2                                                              
         LA    R6,1(R6)                                                         
         BCT   R0,MR1                                                           
         B     ERROR2              NOT FOUND                                    
MR2      BAS   RE,CONVERT                                                       
         STH   R1,MAXRECS          SAVE MAXRECS                                 
         LTR   R1,R1                                                            
         BZ    ERROR3              ZERO ?                                       
         CH    R1,DEFMAX                                                        
         BNH   CC1                                                              
         B     ERROR4              TOO BIG ?                                    
         EJECT                                                                  
***************************************************                             
*           RANGE KEYWORD ROUTINE                 *                             
***************************************************                             
         SPACE 1                                                                
RNGE     EQU   *                                                                
         LA    R6,6(R6)                                                         
         LA    R0,3                                                             
RG1      TM    0(R6),X'F0'         FIND NUMBER                                  
         BO    RG2                                                              
         LA    R6,1(R6)                                                         
         BCT   R0,RG1                                                           
         B     ERROR5              NOT FOUND                                    
RG2      CLI   RNGENUM+3,QMAXRNGE                                               
         BNL   ERROR6                                                           
         BAS   RE,CONVERT                                                       
         LR    R2,R1               R2=1ST VALUE                                 
         LA    R6,1(R6)            SKIP DELIMITER                               
         BAS   RE,CONVERT                                                       
         LA    RF,RANGE                                                         
         L     RE,RNGENUM                                                       
         SLL   RE,3                RANGENUM * 8 = TABLE POSITION                
         ST    R2,0(RE,RF)         STORE 1ST VALUE                              
         ST    R1,4(RE,RF)         STORE 2ND VALUE                              
         CR    R1,R2                                                            
         BL    ERROR9              BACKWARDS !!!                                
         SH    RE,=H'4'                                                         
         BM    RG3                                                              
         L     R1,0(RE,RF)         LOOK AT LAST RANGE                           
         CR    R2,R1                                                            
         BL    ERROR9              MUST NOT BE HIGHER                           
RG3      L     RF,RNGENUM                                                       
         LA    RF,1(RF)            UPDATE RANGENUM                              
         ST    RF,RNGENUM                                                       
         B     CC1                                                              
         EJECT                                                                  
***************************************************                             
*           SEARCH KEYWORD ROUTINE                *                             
***************************************************                             
         SPACE 1                                                                
ASRCH    MVI   FLAG,X'01'          (AND) TYPE                                   
         LA    R6,4(R6)            TAB OVER KEY                                 
         B     SRCH1                                                            
SRCH     MVI   FLAG,X'00'          (SEARCH) TYPE                                
         LA    R6,7(R6)            TAB OVER KEY                                 
         B     SRCH1                                                            
SRCH1    EQU   *                                                                
         CLI   SRCHNUM+3,QMAXSRCH                                               
         BNL   ERROR7                                                           
         L     R1,SRCHNUM                                                       
         SLL   R1,5                CALCULATE                                    
         ST    R1,FULL             POSITIION IN TABLE                           
         LA    R1,2(R1)                                                         
         LA    RF,SRCHTAB                                                       
         LA    RF,0(R1,RF)         RF=FLAG POS                                  
         OC    0(1,RF),FLAG                                                     
         LA    RF,2(RF)            RF=KEY POS                                   
         GOTO1 =V(DECODE),PARMS,(R6),(RF),0                                     
         L     R0,8(R1)            READ LENGTH                                  
         LTR   R0,R0                                                            
         BL    ERROR10             INVALID                                      
         CH    R0,=H'28'                                                        
         BH    ERROR11             TOO LONG                                     
         L     R1,FULL                                                          
         LA    RF,SRCHTAB                                                       
         STH   R0,0(R1,RF)         SAVE LENGTH                                  
         L     RF,SRCHNUM                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SRCHNUM                                                       
         B     CARDIN              NO KEYS FOLLOW SEARCH                        
         SPACE 1                                                                
FRMT     EQU   *                                                                
         B     ERROR1                                                           
         EJECT                                                                  
***************************************************                             
*        NUMBER CONVERTER AND SEARCH CHECK        *                             
***************************************************                             
         SPACE 1                                                                
CONVERT  ST    R6,FULL             ZONED NUMBER AT (R6)                         
CV1      LA    R6,1(R6)                                                         
         TM    0(R6),X'F0'                                                      
         BO    CV1                                                              
         S     R6,FULL                                                          
         CH    R6,=H'7'            CHECK LENGTH                                 
         BH    ERROR8                                                           
         LR    R1,R6                                                            
         L     R6,FULL                                                          
         BCTR  R1,0                                                             
         EX    R1,MOVE             MOVE INTO DEC                                
         LA    R6,1(R1,R6)         (R6)=END OF NUMBER                           
         LA    R1,X'70'(R1)                                                     
         EX    R1,PACK             PACK INTO DUB                                
         CVB   R1,DUB              BINARY INTO R1                               
         C     R1,MAXNUM                                                        
         BH    ERROR8                                                           
         BR    RE                                                               
*                                                                               
CARDEND  EQU   *                                                                
         L     R3,SRCHNUM          SHOW SEARCH STRINGS                          
         LTR   R3,R3               IN HEX                                       
         BZ    RCHECK                                                           
SCKECK1  MVC   OAREA,CLEAR                                                      
         MVI   PRASA,C'-'                                                       
         MVC   PRINT+56(22),=C'--- SEARCH STRINGS ---'                          
         PUT   SYSPRINT,OAREA                                                   
         LA    R2,SRCHTAB                                                       
SCHECK2  LH    RF,0(R2)                                                         
         MVC   OAREA,CLEAR                                                      
         MVC   PRINT(7),=C'   AND '                                             
         TM    2(R2),X'01'                                                      
         BO    SCHECK3                                                          
         MVC   PRINT(7),=C'SEARCH '                                             
SCHECK3  LA    R2,4(R2)                                                         
         GOTO1 =V(HEXOUT),PARMS,(R2),PRINT+8,(RF),0,0                           
         PUT   SYSPRINT,OAREA                                                   
         LA    R2,28(R2)                                                        
         BCT   R3,SCHECK2                                                       
RCHECK   EQU   *                                                                
         EJECT                                                                  
***************************************************                             
*        OPEN INPUT DATA SET                      *                             
***************************************************                             
         SPACE 1                                                                
         OPEN  (DISKIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BNZ   ERROR12                                                          
         EJECT                                                                  
***************************************************                             
*        READ JFCB INFO FOR DATA SET              *                             
***************************************************                             
         SPACE 1                                                                
VAL1     RDJFCB (DISKIN,INPUT)                                                  
         XC    DATE(8),DATE                                                     
         MVC   DATE(3),JFCBAD+80   CREATION DATE                                
         BAS   RE,JULIB            CONVERT DATE                                 
         MVC   CRDATE,DATE                                                      
         XC    DATE(8),DATE                                                     
         MVC   DATE(3),JFCBAD+83   EXPIRY DATE                                  
         BAS   RE,JULIB                                                         
         MVC   EXDATE,DATE                                                      
         SR    R0,R0                                                            
         IC    R0,JFCBAD+117       HOW MANY VOLS                                
         STH   R0,VOLUMES                                                       
         LA    R1,VOLSER                                                        
         LA    RF,JFCBAD+118       ADDR OF VOLSERS                              
VOLS1    MVC   0(6,R1),0(RF)                                                    
         MVI   6(R1),C'.'                                                       
         LA    R1,7(R1)                                                         
         LA    RF,6(RF)                                                         
         BCT   R0,VOLS1                                                         
         MVC   DSNAME,JFCBAD       ADDR OF DSN                                  
         MVI   PDSNAME,C'('                                                     
         MVC   PDSNAME+1(8),JFCBAD+44                                           
         LA    R1,PDSNAME+10                                                    
         LA    R0,PDSNAME                                                       
PDS1     BCTR  R1,0                PUT ( ) IN RIGHT PLACE                       
         CLI   0(R1),C' '                                                       
         BE    PDS1                                                             
         CR    R1,R0                                                            
         BE    NOPDS                                                            
         MVI   1(R1),C')'                                                       
         B     PDS3                                                             
NOPDS    MVI   0(R1),C' '                                                       
PDS3     EQU   *                                                                
         EJECT                                                                  
***************************************************                             
*        CREATE HEADER LINE                       *                             
***************************************************                             
         SPACE 1                                                                
         MVC   HEADER+120(4),=C'PAGE'                                           
         MVC   HEADER(28),=C'1RECORD   LRECL OFFSET  DSN='                      
         MVC   HEADER+86(19),=C'DISK ADDRESS CCHHR='                            
         MVC   HEADER+28(44),DSNAME                                             
         LA    R1,HEADER+28+44                                                  
HDR1     CLI   0(R1),C' '                                                       
         BNE   HDR2                                                             
         BCT   R1,HDR1                                                          
HDR2     MVC   1(10,R1),PDSNAME                                                 
         EJECT                                                                  
***************************************************                             
*        CREATE FRONT PAGE                        *                             
***************************************************                             
         SPACE 1                                                                
FRONT    MVC   OAREA,CLEAR                                                      
         MVI   PRASA,C'1'                                                       
         PUT   SYSPRINT,OAREA                                                   
         MVI   PRASA,C' '                                                       
         MVC   OAREA(5),=C'0DATE'                                               
         MVC   OAREA+16(8),TODAY                                                
         PUT   SYSPRINT,OAREA                                                   
         MVC   OAREA,CLEAR                                                      
         MVC   OAREA(14),=C'0DATA SET NAME'                                     
         MVC   OAREA+16(52),HEADER+28                                           
         PUT   SYSPRINT,OAREA                                                   
         MVC   OAREA,CLEAR                                                      
         MVC   OAREA(8),=C'0CREATED'                                            
         MVC   OAREA+16(8),CRDATE                                               
         PUT   SYSPRINT,OAREA                                                   
         MVC   OAREA,CLEAR                                                      
         MVC   OAREA(8),=C'0VOLUMES'                                            
         LH    RE,VOLUMES                                                       
         CVD   RE,DUB                                                           
         UNPK  OAREA+16(3),DUB+6(2)                                             
         OI    OAREA+18,X'F0'                                                   
         PUT   SYSPRINT,OAREA                                                   
         MVC   OAREA,CLEAR                                                      
         MVC   OAREA+16(35),VOLSER                                              
         MVC   OAREA(12),=C'0VOL=SER(S)='                                       
         PUT   SYSPRINT,OAREA                                                   
         MVC   OAREA,CLEAR                                                      
         ICM   RE,8,DCBDSORG                                                    
         LA    R1,DSORG                                                         
DS1      LTR   RE,RE                                                            
         BM    DS2                                                              
         SLL   RE,1                                                             
         LA    R1,2(R1)                                                         
         B     DS1                                                              
DS2      MVC   OAREA(13),=C'0ORGANIZATION'                                      
         MVC   OAREA+16(2),0(R1)                                                
         PUT   SYSPRINT,OAREA                                                   
         MVC   OAREA,CLEAR                                                      
         ICM   RE,8,DCBRECFM                                                    
         N     RE,=X'C0000000'                                                  
         SRL   RE,30                                                            
         LA    R1,RECF1(RE)                                                     
         MVC   OAREA(14),=C'0RECORD FORMAT'                                     
         MVC   OAREA+16(1),0(R1)                                                
         ICM   RE,8,DCBRECFM                                                    
         LA    R1,OAREA+17                                                      
         N     RE,=X'10000000'                                                  
         LTR   RE,RE                                                            
         BZ    NBLOCK                                                           
         MVI   0(R1),C'B'                                                       
         LA    R1,1(R1)                                                         
NBLOCK   ICM   RE,8,DCBRECFM                                                    
         N     RE,=X'08000000'                                                  
         LTR   RE,RE                                                            
         BZ    NSPAN                                                            
         MVI   0(R1),C'S'                                                       
         LA    R1,1(R1)                                                         
NSPAN    ICM   RE,8,DCBRECFM                                                    
         N     RE,=X'06000000'                                                  
         SRL   RE,25                                                            
         C     RE,=X'00000001'                                                  
         BL    ECTRL                                                            
         MVI   0(R1),C'M'                                                       
         BE    ECTRL                                                            
         MVI   0(R1),C'A'                                                       
ECTRL    PUT   SYSPRINT,OAREA                                                   
         MVC   OAREA,CLEAR                                                      
         MVC   OAREA(14),=C'0RECORD LENGTH'                                     
         LH    RE,DCBLRECL                 READ LENGTH OF RECORD,               
         CVD   RE,DUB                                                           
         UNPK  OAREA+16(5),DUB+5(3)                                             
         OI    OAREA+20,X'F0'                                                   
         PUT   SYSPRINT,OAREA                                                   
         MVC   OAREA,CLEAR                                                      
         MVC   OAREA(11),=C'0BLOCK SIZE'                                        
         LH    RE,DCBBLKSI                 READ LENGTH OF BLOCK                 
         CVD   RE,DUB                                                           
         UNPK  OAREA+16(5),DUB+5(3)                                             
         OI    OAREA+20,X'F0'                                                   
         PUT   SYSPRINT,OAREA                                                   
         LA    R2,60               LINECOUNT=R2                                 
         SR    R1,R1                                                            
         ST    R1,RECNUM                                                        
         EJECT                                                                  
***************************************************                             
*        GET RECORD FROM DISK                     *                             
***************************************************                             
         SPACE 1                                                                
NEXTREC  GET   DISKIN                                                           
         LR    R6,R1               (R6)=RECORD                                  
         L     R1,RECNUM                                                        
         LA    R1,1(R1)            UPDATE RECNUM                                
         ST    R1,RECNUM                                                        
         LH    R1,DCBLRECL                                                      
         STH   R1,LRECL            GET RECORD LENGTH                            
         MVC   DUB,DISKAD                                                       
         MVC   DISKAD,DCBFDAD                                                   
         CLC   DUB,DISKAD                                                       
         BE    SAMEBLK                                                          
         MVI   NEWBLK,C'Y'                                                      
SAMEBLK  EQU   *                                                                
         EJECT                                                                  
***************************************************                             
*        CHECK RECORD IS IN RANGE                 *                             
***************************************************                             
         SPACE 1                                                                
PASRNGE  L     RE,RNGENUM                                                       
         LTR   RE,RE                                                            
         BZ    PASSRCH                                                          
         LA    R1,RANGE                                                         
PASS1    L     R0,RECNUM                                                        
         C     R0,0(R1)            IS IT => START                               
         BL    OUTRNGE                                                          
         C     R0,4(R1)            IS IT <= END                                 
         BNH   PASSRCH                                                          
         LA    R1,8(R1)            NEXT RANGE                                   
         BCT   RE,PASS1                                                         
         B     HEXEND              NO MORE                                      
OUTRNGE  B     NEXTREC                                                          
         EJECT                                                                  
***************************************************                             
*        CHECK SEARCH CHARS ARE IN RECORD         *                             
***************************************************                             
         SPACE 1                                                                
PASSRCH  EQU   *                                                                
         L     RE,SRCHNUM                                                       
         LA    R7,SRCHTAB                                                       
PS0      LTR   RE,RE                                                            
         BZ    PASSOK              NO SEARCHES OK                               
         LR    R5,R6               R5=A(RECORD)                                 
         LH    RF,LRECL                                                         
         LH    R1,0(R7)            R1=LEN SEARCH                                
         SR    RF,R1                                                            
         BNP   PS2                 IF L'SRCH > L'REC NO WAY                     
         LA    RF,1(RF)                                                         
         BCTR  R1,0                SUB 1 FOR EXECUTE                            
PS1      EX    R1,FIND             FIND IT                                      
         BE    FOUND                                                            
         LA    R5,1(R5)            TRY NEXT BYTE                                
         BCT   RF,PS1                                                           
PS2      LA    R7,32(R7)           NOT THERE                                    
         TM    2(R7),X'01'                                                      
         BO    ANDSOUT             SO CHUCK OUT ANDS                            
         BCT   RE,PS0              TRY NEXT SEARCH                              
         B     NEXTREC                                                          
ANDSOUT  BCT   RE,PS2                                                           
         B     NEXTREC                                                          
*                                                                               
FOUND    BCTR  RE,0                FOUND ONE                                    
         LTR   RE,RE               ONLY ONE?                                    
         BZ    PASSOK                                                           
         LA    R7,32(R7)                                                        
         TM    2(R7),X'01'         ANY ANDS?                                    
         BNO   PASSOK                                                           
         B     PS0                 IF SO FIND THEM TOO                          
         EJECT                                                                  
***************************************************                             
*        OUTPUT ONE RECORD                        *                             
***************************************************                             
         SPACE 1                                                                
PASSOK   EQU   *                                                                
         L     R1,STOTREC                                                       
         LA    R1,1(R1)                                                         
         ST    R1,STOTREC                                                       
         CH    R1,MAXRECS                                                       
         BH    ERROR4                                                           
*                                                                               
NEWREC   SR    R5,R5               ZERO INDEX                                   
         MVC   SAVEHEX,CLEAR                                                    
         MVI   SAME,C'N'                                                        
         LH    R1,LLRECL                                                        
         CH    R1,=H'32'           WAS LAST > 32                                
         BH    NEWREC1             YES THEN PRINT IT                            
         LH    R1,LRECL                                                         
         CH    R1,=H'32'           IS THIS > 32                                 
         BNH   NEWREC1                                                          
         PUT   SYSPRINT,CLEAR      ONE BLANK LINE                               
         LA    R2,1(R2)                                                         
NEWREC1  CLI   NEWBLK,C'Y'                                                      
         BNE   OLDBLK                                                           
         GOTO1 =V(HEXOUT),PARMS,DISKAD+3,HEADER+105,5,0,0                       
         MVI   PRINT,C'-'                                                       
         MVC   PRINT+1(L'PRINT-1),PRINT                                         
         PUT   SYSPRINT,OAREA                                                   
         MVC   OAREA,CLEAR                                                      
         PUT   SYSPRINT,OAREA                                                   
         LA    R2,2(R2)            LC=LC+2                                      
         MVI   NEWBLK,C'N'                                                      
OLDBLK   LH    R1,LRECL            WILL RECORD FIT ON PAGE                      
         SRA   R1,5                                                             
         LA    R1,1(R1)                                                         
         AR    R1,R2                                                            
         CH    R1,=H'60'                                                        
         BNP   NEWL                                                             
PAGE     LH    R1,PAGNUM           PAGE EJECT                                   
         LA    R1,1(R1)                                                         
         STH   R1,PAGNUM                                                        
         CVD   R1,DUB                                                           
         MVC   HEADER+124(6),EDWD                                               
         ED    HEADER+124(6),DUB+5                                              
         PUT   SYSPRINT,HEADER                                                  
         PUT   SYSPRINT,CLEAR                                                   
         LA    R2,2                                                             
*                                                                               
NEWL     EQU   *                                                                
         CH    R2,=H'60'           PAGE YET ?                                   
         BNL   PAGE                                                             
LINE1    MVC   OAREA,CLEAR                                                      
         LA    R4,PRCHR            BASE REGS FOR OUTPUT                         
         LA    R7,PRHEX                                                         
         LTR   R5,R5               FIRST LINE ?                                 
         BNZ   NOINFO                                                           
INFO     GOTO1 =V(HEXOUT),PARMS,RECNUM+1,PRINT+1,3,0,0                          
         GOTO1 =V(HEXOUT),PARMS,LRECL,PRINT+9,2,0,0                             
NOINFO   CH    R5,=H'32'           SECOND LINE ?                                
         BNZ   NODEC                                                            
DECIMAL  MVC   PRINT(15),=C'(     ) (     )'                                    
         L     RE,RECNUM                                                        
         CVD   RE,DUB                                                           
         UNPK  PRINT+1(5),DUB+5(3)                                              
         OI    PRINT+5,X'F0'                                                    
         LH    RE,LRECL                                                         
         CVD   RE,DUB                                                           
         UNPK  PRINT+9(5),DUB+5(3)                                              
         OI    PRINT+13,X'F0'                                                   
NODEC    ST    R5,FULL                                                          
         GOTO1 =V(HEXOUT),PARMS,FULL+2,PRINT+16,2,0,0                           
         SR    RF,RF                                                            
BYTE1    IC    RF,0(R5,R6)         GET A BYTE                                   
         STC   RF,0(R4)            CHAR OUT                                     
ROT1     SR    RE,RE                                                            
         SLDL  RE,28               CONVERT TO HEX ZONE                          
         CH    RE,=H'10'                                                        
         BNL   HIGH1                                                            
         LA    RE,57(RE)                                                        
HIGH1    LA    RE,183(RE)                                                       
         STC   RE,0(R7)            OUTPUT ZONE                                  
         SR    RE,RE                                                            
         SLDL  RE,4                CONVERT TO HEX DIGIT                         
         CH    RE,=H'10'                                                        
         BNL   HIGH2                                                            
         LA    RE,57(RE)                                                        
HIGH2    LA    RE,183(RE)                                                       
         STC   RE,1(R7)            OUTPUT DIGIT                                 
         LA    R7,2(R7)                                                         
         LA    R5,1(R5)                                                         
         LA    R4,1(R4)                                                         
         CH    R5,LRECL            END OF RECORD ?                              
         BNL   EOREC                                                            
         STC   R5,FULL                                                          
         TM    FULL,X'03'          FULLWORD YET                                 
         BNZ   NOWORD                                                           
         LA    R7,1(R7)                                                         
NOWORD   TM    FULL,X'0F'          HALFWAY YET ?                                
         BNZ   NOMID                                                            
         LA    R7,1(R7)                                                         
         LA    R4,2(R4)                                                         
NOMID    TM    FULL,X'1F'          WHOLE LINE YET                               
         BNZ   BYTE1                                                            
         CLC   SAVEHEX,PRHEX                                                    
         BNE   *+12                                                             
         MVI   SAME,C'Y'                                                        
         B     NEWL                                                             
         CLI   SAME,C'Y'                                                        
         BNE   OUT01                                                            
         MVC   WORKL,CLEAR                                                      
         MVC   WORKL+24(16),SAMEMSG                                             
         PUT   SYSPRINT,WORKL                                                   
         LA    R2,1(R2)                                                         
OUT01    TR    PRCHR(34),TTABLE    KILL NON DISPLAY CHARS                       
         PUT   SYSPRINT,OAREA                                                   
         MVC   SAVEHEX,PRHEX                                                    
         MVI   SAME,C'N'                                                        
         LA    R2,1(R2)                                                         
         B     NEWL                                                             
EOREC    CLI   SAME,C'Y'                                                        
         BNE   EORECA                                                           
         MVC   WORKL,CLEAR                                                      
         MVC   WORKL+24(16),SAMEMSG                                             
         PUT   SYSPRINT,WORKL                                                   
         LA    R2,1(R2)                                                         
EORECA   TR    PRCHR(34),TTABLE                                                 
         PUT   SYSPRINT,OAREA                                                   
         LA    R2,1(R2)                                                         
         LH    R1,LRECL                                                         
         STH   R1,LLRECL           SAVE THIS FOR ONE LOOP                       
         CH    R1,=H'32'           BIG RECORD ?                                 
         BNH   EOREC1                                                           
         PUT   SYSPRINT,CLEAR      YES THEN SPACE THEM OUT                      
         LA    R2,1(R2)                                                         
EOREC1   B     NEXTREC                                                          
         EJECT                                                                  
***************************************************                             
*        DATE CONVERSIONS                         *                             
***************************************************                             
         SPACE 1                                                                
JULIB    SR    R1,R1               CONVERT BIN JULIAN                           
         ICM   R1,3,DATE+1         INTO PACKED JULIAN                           
         CVD   R1,DUB                                                           
         SR    R1,R1                                                            
         IC    R1,DATE                                                          
         MH    R1,=H'10'                                                        
         CVD   R1,DATE                                                          
         L     R1,DUB+4                                                         
         ICM   R1,4,DATE+6                                                      
*                                  CONVERT JULIAN INTO DD/MM/YY                 
JULIAN   XC    DATE(8),DATE                                                     
         STCM  R1,3,DATE+6                                                      
         CVB   R0,DATE                                                          
         SRL   R1,12                                                            
         STCM  R1,3,DATE+6                                                      
         OI    DATE+7,X'0F'                                                     
         CVB   R1,DATE                                                          
         UNPK  DATE(2),DATE+6(2)                                                
         OI    DATE+1,X'F0'                                                     
         STC   R1,DATE+7                                                        
         LA    R1,1                                                             
         CH    R0,=H'31'                                                        
         BNH   *+104                                                            
         SH    R0,=H'31'                                                        
         LA    R1,1(R1)                                                         
         CH    R0,=H'28'                                                        
         BNH   *+88                                                             
         SH    R0,=H'28'                                                        
         LA    RF,*+58                                                          
         TM    DATE+7,3                                                         
         BNZ   *+40                                                             
         CH    R0,=H'1'                                                         
         BH    *+12                                                             
         AH    R0,=H'28'                                                        
         B     *+56                                                             
         SH    R0,=H'1'                                                         
         B     *+16                                                             
         CH    R0,0(RF)                                                         
         BNH   *+40                                                             
         SH    R0,0(RF)                                                         
         LA    R1,1(R1)                                                         
         LA    RF,2(RF)                                                         
         B     *-20                                                             
         DC    AL2(31,30,31,30,31,31,30,31,30,31)                               
         ICM   RF,12,DATE                                                       
         CVD   R1,DATE                                                          
         UNPK  DATE(2),DATE+6(2)                                                
         OI    DATE+1,X'F0'                                                     
         ICM   RF,3,DATE                                                        
         CVD   R0,DATE                                                          
         UNPK  DATE+4(2),DATE+6(2)                                              
         OI    DATE+5,X'F0'                                                     
         ST    RF,DATE                                                          
         MVC   DATE+6(2),=C'UK'                                                 
         CLI   DATE+7,C'K'         IS IT UK                                     
         MVC   DATE+6(2),DATE+0    CONVERT TO DD/MM/YY                          
         MVC   DATE+0(2),DATE+4                                                 
         MVC   DATE+4(1),DATE+3                                                 
         MVC   DATE+3(1),DATE+2                                                 
         MVI   DATE+2,C'/'                                                      
         MVI   DATE+5,C'/'                                                      
         BE    DT1                                                              
US       MVC   DATE+0(2),DUB       CONVERT TO MM/DD/YY                          
         MVC   DATE+3(2),DATE+0                                                 
         MVC   DUB(2),DATE+3                                                    
DT1      EQU   *                                                                
         BR    RE                                                               
         EJECT                                                                  
***************************************************                             
*        ERROR EXITS                              *                             
***************************************************                             
         SPACE 1                                                                
ERROR0   MVC   PRINT(L'ERR0),ERR0                                               
         B     ERROR                                                            
ERROR1   MVC   PRINT(L'ERR1),ERR1                                               
         B     ERROR                                                            
ERROR2   MVC   PRINT(L'ERR2),ERR2                                               
         B     ERROR                                                            
ERROR3   MVC   PRINT(L'ERR3),ERR3                                               
         B     ERROR                                                            
ERROR4   MVC   PRINT(L'ERR4),ERR4                                               
         B     ERROR                                                            
ERROR5   MVC   PRINT(L'ERR5),ERR5                                               
         B     ERROR                                                            
ERROR6   MVC   PRINT(L'ERR6),ERR6                                               
         B     ERROR                                                            
ERROR7   MVC   PRINT(L'ERR7),ERR7                                               
         B     ERROR                                                            
ERROR8   MVC   PRINT(L'ERR8),ERR8                                               
         B     ERROR                                                            
ERROR9   MVC   PRINT(L'ERR9),ERR9                                               
         B     ERROR                                                            
ERROR10  MVC   PRINT(L'ERR10),ERR10                                             
         B     ERROR                                                            
ERROR11  MVC   PRINT(L'ERR11),ERR11                                             
         B     ERROR                                                            
ERROR12  MVC   PRINT(L'ERR12),ERR12                                             
         B     ERROR                                                            
ERROR    PUT   SYSPRINT,OAREA                                                   
         EJECT                                                                  
***************************************************                             
         SPACE 1                                                                
DISKEND  EQU   *                                                                
HEXEND   CLOSE (DISKIN,,SYSPRINT,,SYSIN)                                        
         XBASE                                                                  
         EJECT                                                                  
***************************************************                             
*        KEYWORD TABLE                            *                             
***************************************************                             
         SPACE 1                                                                
         DS    0F                                                               
KEYTAB   DS    0CL12                                                            
         DC    A(MXRCS),X'06',CL7'MAXRECS'                                      
         DC    A(SRCH),X'05',CL7'SEARCH'                                        
         DC    A(RNGE),X'04',CL7'RANGE'                                         
         DC    A(FRMT),X'05',CL7'FORMAT'                                        
         DC    A(ASRCH),X'02',CL7'AND'                                          
KEYTABX  DC    A(0)                END MARKER                                   
         EJECT                                                                  
***************************************************                             
*        OTHER CONSTANTS                          *                             
***************************************************                             
         SPACE 1                                                                
TTABLE   DC    C'................................'                              
         DC    C'................................'                              
         DC    X'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F'                              
         DC    X'504B4B4B4B4B4B4B4B4B5A5B5C5D5E5F'                              
         DC    X'60614B4B4B4B4B4B4B4B6A6B6C6D6E6F'                              
         DC    X'4B4B4B4B4B4B4B4B4B797A7B7C7D7E7F'                              
         DC    C'.abcdefghi.......jklmnopqr......'                              
         DC    C'..stuvwxyz......................'                              
         DC    C'{ABCDEFGHI......}JKLMNOPQR......'                              
         DC    C'\.STUVWXYZ......0123456789|.....'                              
*                                                                               
EDWD     DC    X'402020202021'                                                  
DSORG    DC    C'ISPSDA      POU '                                              
RECF1    DC    C'DVFU'                                                          
DEFMAX   DC    H'20000'                                                         
MAXNUM   DC    F'8388607'                                                       
         EJECT                                                                  
***************************************************                             
*        ERROR TEXT                               *                             
***************************************************                             
         SPACE 1                                                                
ERR0     DC    C'***TEST ERROR*** ***TEST ERROR***'                             
ERR1     DC    C'INVALID KEYWORD'                                               
ERR2     DC    C'MAXRECS INVALID'                                               
ERR3     DC    C'MAXRECS ZERO'                                                  
ERR4     DC    C'MAXRECS TOO BIG'                                               
ERR5     DC    C'RANGE INVALID'                                                 
ERR6     DC    C'TOO MANY RANGES'                                               
ERR7     DC    C'TOO MANY SEARCHES'                                             
ERR8     DC    C'NUMBER TOO LARGE'                                              
ERR9     DC    C'RANGE SEQUENCE ERROR'                                          
ERR10    DC    C'SEARCH STRING INVALID'                                         
ERR11    DC    C'SEARCH STRING TOO LONG  MAX=28'                                
ERR12    DC    C'OPEN ERROR '                                                   
*                                                                               
SAMEMSG  DC    CL16'---SAME UNTIL---'                                           
         EJECT                                                                  
*****************************                                                   
*   EXECUTED INSTRUCTIONS   *                                                   
*****************************                                                   
         SPACE 1                                                                
MOVE     MVC   DEC(0),0(R6)                                                     
PACK     PACK  DUB(0),DEC(0)                                                    
COMP     CLC   0(0,R6),5(R5)                                                    
FIND     CLC   0(0,R5),4(R7)                                                    
         EJECT                                                                  
*****************************                                                   
*   DATA CONTROL BLOCKS     *                                                   
*****************************                                                   
         SPACE 1                                                                
DISKIN   DCB   DSORG=PS,MACRF=GL,DDNAME=DISKIN,EODAD=DISKEND,EXLST=LIST         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(133)          
SYSIN    DCB   DSORG=PS,MACRF=GL,DDNAME=SYSIN,EODAD=CARDEND                     
         SPACE 1                                                                
**********************************                                              
*  JOB FILE CONTROL BLOCK DISKIN *                                              
**********************************                                              
         SPACE 1                                                                
LIST     DS    0F                                                               
         DC    X'07'                                                            
         DC    AL3(JFCBAD)                                                      
JFCBAD   DS    0F                                                               
         DS    176C                                                             
         EJECT                                                                  
***************************************************                             
*        LITERALS                                 *                             
***************************************************                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*****************************                                                   
*        WORK AREA          *                                                   
*****************************                                                   
         SPACE 1                                                                
WORK     DC  1000D'0'                                                           
         EJECT                                                                  
***************************************************                             
*        WORKING STORAGE                          *                             
***************************************************                             
         SPACE 1                                                                
DATA     DSECT                                                                  
*                                                                               
OAREA    DS    0CL133                                                           
PRASA    DS    CL1                                                              
PRINT    DS    0CL132                                                           
         DS    CL23                                                             
PRHEX    DS    CL72                                                             
         DS    CL3                                                              
PRCHR    DS    CL34                                                             
*                                                                               
SAVEHEX  DS    CL72                                                             
*                                                                               
HEADER   DS    CL133                                                            
CLEAR    DS    CL133                                                            
WORKL    DS    CL133                                                            
*                                                                               
DSINFO   DS    0CL100                                                           
DSNAME   DS    CL44                                                             
PDSNAME  DS    CL10                                                             
VOLSER   DS    CL35                                                             
RECFM    DS    CL4                                                              
*                                                                               
DEC      DS    CL5                                                              
*                                                                               
FLAG     DS    1C                                                               
SAME     DS    1C                                                               
NEWBLK   DS    1C                                                               
TODAY    DS    1D                                                               
CRDATE   DS    1D                                                               
EXDATE   DS    1D                                                               
DATE     DS    1D                                                               
DUB      DS    1D                                                               
FULL     DS    1F                                                               
FULL1    DS    1F                                                               
RNGENUM  DS    1F                                                               
RANGE    DS    (QMAXRNGE*2)F                                                    
SRCHNUM  DS    1F                                                               
SRCHTAB  DS    (QMAXSRCH*16)H                                                   
DISKAD   DS    1D                                                               
LRECL    DS    1H                                                               
RECNUM   DS    1F                                                               
LLRECL   DS    1H                                                               
BLKSISE  DS    1H                                                               
MAXRECS  DS    1H                                                               
VOLUMES  DS    1H                   NUM OF VOLUMES                              
PARMS    DS    6F                                                               
DMCB     DS    6F                                                               
PARMTAB  DS    200C                                                             
*                                                                               
STOTREC  DS    1F                                                               
SLRGREC  DS    1H                                                               
SSMLREC  DS    1H                                                               
SRECNUM  DS    1H                                                               
*                                                                               
PAGNUM   DS    1H                                                               
LINCNT   DS    1H                                                               
ENDATA   EQU   *                                                                
         EJECT                                                                  
         DCBD  DSORG=QS,DEVD=DA                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038DDHEXPRINT05/01/02'                                      
         END                                                                    
