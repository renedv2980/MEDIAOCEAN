*          DATA SET RERCVXTR   AT LEVEL 004 AS OF 05/01/02                      
*          DATA SET RERCVXTR   AT LEVEL 001 AS OF 09/07/83                      
*PHASE RERCXTRA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*&&DO                                                                           
*INCLUDR IJDFYZZZ                                                               
*INCLUDR IJFVZZWZ                                                               
*&&                                                                             
         SPACE 2                                                                
*                                                                               
* THIS PROGRAM READS THE REPPAK RECOVERY TAPE AND EXTRACTS RECORDS              
* SELECTED BY APPLYING OPTIONS SPECIFIED BY CONTROL CARDS.  THE                 
* EXTRACTED RECORDS ARE WRITTEN TO SYSLST IN DUMP FORMAT.                       
*                                                                               
* CONTROL CARD OPTIONS ARE AS FOLLOWS                                           
*                                                                               
* RECORD=      CONTRACT,BUYS    PARAMETERS MAY BE INPUT IN                      
*              STRING OR ON INDIVIDUAL CARDS.  UP TO 6 RECORD                   
*              TYPES MAY BE FILTERED ON IN ONE RUN.                             
*                                                                               
* CONNUM=      XXXXX,XXXX-YYYY  UP TO 20 SINGLE NUMBERS AND/OR RANGES           
*              OF NUMBERS MAY BE SPECIFIED ON 1 OR MORE CARDS                   
*                                                                               
* TERMINAL=    XXXX   TO FILTER ON TRANSACTIONS FOR A SPECIFIED                 
*              TERMINAL NUMBER.  ONLY 1 SUPPORTED FOR A RUN.                    
*              *** THIS FILTER IS NO LONGER SUPPORTED, USE LUID= ***            
*                                                                               
* LUID=        XXXXXXXX   TO FILTER ON TRANSACTIONS FOR A SPECIFIED             
*              LUID.  ONLY 1 SUPPORTED FOR A RUN.                               
*                                                                               
* REP=         CC    REP CODE FILTER-ONLY 1 CAN BE SPECIFIED                    
*                                                                               
* STATION=     E.G. KGO-AM,KMODA,WBBM,WBBM-TV  ONLY 1 FOR A RUN.                
*                                                                               
* OLIMIT=      NNNN  OVERRIDES DEFAULT LIMIT ON RECORDS PRINTED-200.            
*              TOTALS AT END OF REPORT REFLECT RECORDS SELECTED                 
*              REGARDLESS OF OUTPUT LIMIT IN EFFECT.                            
         TITLE 'READ REP RECOVERY TAPE AND EXTRACT SELECTED RECORDS'            
RERCVXTR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**RERCVXTR,=V(REGSAVE),R9,RR=RE,CLEAR=YES            
         USING WORKD,RC                                                         
         USING RERCVXTR+4096,R9                                                 
         ST    RE,FACTOR           SAVE RELOCATION FACTOR                       
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK                             
         MVC   TITLE+13(25),=C'REP RECOVERY TAPE EXTRACT'                       
         GOTO1 PRINTER                                                          
         SPACE                                                                  
         MVC   P+10(13),=C'CONTROL CARDS'                                       
         GOTO1 PRINTER                                                          
         MVI   P+10,C'-'                                                        
         MVC   P+11(12),P+10                                                    
         GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         EJECT                                                                  
CARDREAD GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   CARD(2),=C'/*'                                                   
         BE    ENDCARD                                                          
         SPACE                                                                  
         MVC   P+10(L'CARD),CARD                                                
         GOTO1 PRINTER                                                          
         SPACE                                                                  
         LA    R0,80               CREATE A PSEUDO HEADER FOR SCANNER           
         LR    R1,R0                                                            
         LA    R1,8(R1)                                                         
         STC   R1,HEADER                                                        
         LA    R2,CARD+79                                                       
         CLI   0(R2),C' '          FIND INPUT DATA LENGTH                       
         BNE   *+10                                                             
         BCTR  R2,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,HEADER+5                                                      
         LTR   R0,R0               TEST FOR NO DATA ON CARD                     
         BZ    CONERR                                                           
         SPACE 2                                                                
* SCAN THE CONTROL CARD AND CHECK THE CONTROL OPTION                            
*                                                                               
SCAN     GOTO1 SCANNER,DMCB,(60,HEADER),(1,WORK),=X'6B7E007E'                   
         LA    R0,OPTENT           COUNTER                                      
         LA    R3,OPTABLE          TABLE POINTER                                
         LA    R2,WORK             SCAN BLOCK POINTER                           
         CLI   DMCB+4,0                                                         
         BE    CONERR                                                           
         CLI   0(R2),3             PRESUMED KEYWORD MUST                        
         BL    OPTERR              BE AT LEAST 3 AND NO MORE                    
         CLI   0(R2),8             THAN 8 BYTES                                 
         BH    OPTERR                                                           
         SPACE                                                                  
SCAN2    ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         SPACE                                                                  
SCAN3    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),0(R3)      CHECK AGAINST OPTION KEYWORD                 
         BE    SCAN4               FOUND                                        
         LA    R3,L'OPTABLE(R3)    POINT TO NEXT ENTRY                          
         BCT   R0,SCAN3                                                         
         B     OPTERR              INVALID KEYWORD                              
         SPACE                                                                  
SCAN4    LA    R3,8(R3)            BRANCH TO OPTION PROCESSING                  
         ICM   RE,15,0(R3)         ROUTINE                                      
         A     RE,FACTOR                                                        
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE RECORD TYPE                                                          
*                                                                               
VALREC   DS    0H                                                               
         CLI   1(R2),0                                                          
         BE    CONERR                                                           
         MVC   WORK2(80),22(R2)    PARAMETER STRING                             
         OC    WORK2(80),SPACES                                                 
         GOTO1 SCANNER,DMCB,(C'C',WORK2),(6,WORK),0                             
         CLI   DMCB+4,0            TEST FOR ERROR                               
         BE    CONERR                                                           
         ZIC   R5,DMCB+4           NUMBER OF PARAMETERS                         
         LA    R2,WORK             POINTER TO SCANNER BLOCK                     
*                                                                               
VALREC1  TM    2(R2),X'40'         TEST FOR ALPHA INPUT                         
         BZ    PARMERR                                                          
         LA    R3,RECTABLE         POINTER TO TABLE                             
         LA    R0,RECENT           COUNTER OF ENTRIES                           
         CLI   0(R2),3             L'PARAMETER MUST BE 3-8                      
         BL    PARMERR                                                          
         CLI   0(R2),8                                                          
         BH    PARMERR                                                          
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         SPACE                                                                  
VALREC2  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),0(R3)      TEST VS. TABLE                               
         BE    VALREC4             FOUND                                        
         LA    R3,L'RECTABLE(R3)   TRY NEXT ENTRY                               
         BCT   R0,VALREC2                                                       
         B     PARMERR             INVALID                                      
         SPACE                                                                  
VALREC4  LA    RE,TYPTABLE+2       POINT RE TO RECORD CODES                     
         ZIC   R1,TYPTABLE                                                      
         LA    R1,1(R1)            INCREMENT TABLE ENTRY                        
         CLM   R1,1,TYPTABLE+1     COUNTER AND CHECK AGAINST                    
         BH    CONERR              ENTRY LIMIT                                  
         STC   R1,TYPTABLE                                                      
         BCTR  R1,0                                                             
         LA    RE,0(R1,RE)         POINT RE TO NEXT FREE BYTE                   
         MVC   0(1,RE),8(R3)       ADD TO TABLE                                 
         LA    R2,32(R2)           POINT TO NEXT BLOCK                          
         BCT   R5,VALREC1                                                       
         B     ENDSCAN                                                          
         SPACE 2                                                                
* VALIDATE STRING OF CONTRACT NUMBERS                                           
*                                                                               
VALNUM   XC    WORK2,WORK2         PREPARE A PSEUDO HEADER FOR                  
         MVI   WORK2,88            NUMBER STRING                                
         ZIC   R1,1(R2)                                                         
         STC   R1,WORK2+5                                                       
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BNP   CONERR                                                           
         EX    R1,*+8              MOVE NUMBERS                                 
         B     *+10                                                             
         MVC   WORK2+8(0),22(R2)                                                
         OC    WORK2+8(80),SPACES                                               
         GOTO1 SCANNER,DMCB,(20,WORK2),WORK,C',=,-'                             
         CLI   DMCB+4,0                                                         
         BE    CONERR                                                           
         ZIC   R5,DMCB+4           BLOCK COUNTER                                
         LA    R2,WORK             POINTER                                      
         SPACE                                                                  
VALNUM2  TM    2(R2),X'80'         TEST FOR VALID NUMERIC DATA                  
         BZ    PARMERR                                                          
         ICM   RF,15,4(R2)                                                      
         BZ    PARMERR                                                          
         CVD   RF,DUB                                                           
         SRP   DUB+3(5),1,0        SHIFT NUMBER 1 DECIMAL POSITION              
         MVC   FULL,DUB+3          SAVE FIRST FIELD VALUE                       
         SPACE                                                                  
         XC    DUB,DUB                                                          
         CLI   1(R2),0             SECOND FIELD DATA                            
         BE    VALNUM4             NONE                                         
         TM    3(R2),X'80'                                                      
         BZ    PARMERR                                                          
         ICM   RF,15,8(R2)                                                      
         BZ    PARMERR                                                          
         CLM   RF,15,4(R2)         SECOND CANNOT BE LESS THAN FIRST             
         BL    PARMERR                                                          
         CVD   RF,DUB                                                           
         SRP   DUB+3(5),1,0                                                     
         B     VALNUM4                                                          
         SPACE                                                                  
VALNUM4  LA    RE,CONTABLE         RE POINTS TO TABLE OF K NUM FILTERS          
         LA    R1,L'CONTABLE                                                    
         LH    R0,CONENT           NUMBER OF ENTRIES IN TABLE                   
         AH    R0,=H'1'                                                         
         CH    R0,CONLIM                                                        
         BH    CONERR                                                           
         STH   R0,CONENT                                                        
         BCTR  R0,0                                                             
         MR    R0,R0               OFFSET TO NEXT AVAILABLE ENTRY               
         LA    RE,0(R1,RE)         POINT TO IT                                  
         MVC   0(4,RE),FULL        LOW NUMBER                                   
         OC    DUB,DUB                                                          
         BZ    *+14                                                             
         MVC   4(4,RE),DUB+3       HIGH NUMBER                                  
         B     VALNUM6                                                          
         MVC   4(4,RE),FULL        ONLY 1 NUMBER                                
         SPACE                                                                  
VALNUM6  LA    R2,42(R2)           POINT TO NEXT SCANNER BLOCK                  
         BCT   R5,VALNUM2                                                       
         OI    CONTROL,CONNUM      SET FILTER INDICATOR                         
         B     ENDSCAN                                                          
         SPACE 2                                                                
* VALIDATE TERMINAL FILTER VALUE                                                
*                                                                               
VALLUID  DS    0H                                                               
         CLI   1(R2),8                                                          
         BNE   PARMERR             SIZE OF NUMBER IS SUSPECT                    
         OC    LUIDFILT,LUIDFILT   DUPLICATE FILTER                             
         BNZ   DUPERR                                                           
         MVC   LUIDFILT,10(R2)                                                  
         OI    CONTROL,LUID                                                     
         B     ENDSCAN                                                          
         SPACE 2                                                                
* VALIDATE REP CODE FILTER VALUE                                                
*                                                                               
VALREP   DS    0H                                                               
         CLI   1(R2),2                                                          
         BH    PARMERR             LENGTH OF CODE CAN NOT BE GT 2               
         OC    REPFILT,REPFILT                                                  
         BNZ   DUPERR              DUPLICATE FILTER                             
         MVC   REPFILT,22(R2)                                                   
         OI    CONTROL,REPCODE                                                  
         B     ENDSCAN                                                          
         SPACE 2                                                                
* VALIDATE STATION CALL LETTERS                                                 
*                                                                               
VALSTA   DS    0H                                                               
         CLI   1(R2),7                                                          
         BH    PARMERR                                                          
         OC    STAFILT,STAFILT                                                  
         BNZ   DUPERR              DUPLICATE FILTER                             
         MVC   STAFILT,22(R2)                                                   
         MVC   WORK2(80),22(R2)    PREPARE TO RE-SCAN LINE                      
         OC    WORK2(80),SPACES                                                 
         GOTO1 SCANNER,DMCB,(C'C',WORK2),(1,WORK),C',=,-'                       
         CLI   DMCB+4,0                                                         
         BE    CONERR                                                           
         CLI   1(R2),0             TEST FOR UNDIVIDED FIELD                     
         BE    VALSTAX                                                          
         MVC   STAFILT(4),WORK+12  FIRST 3-4 CALL LETTERS                       
         MVC   STAFILT+4(1),WORK+22 BAND OR T AFTER DASH                        
*                                                                               
VALSTAX  DS    0H                                                               
         CLI   STAFILT+4,C'T'                                                   
         BNE   *+8                                                              
         MVI   STAFILT+4,C' '      ERASE 'T' FOR COMPARABILITY                  
         OI    CONTROL,CALLET                                                   
         B     ENDSCAN                                                          
         SPACE 2                                                                
* VALIDATE OUTPUT RECORD PRINTING LIMIT                                         
*                                                                               
VALOLIM  DS    0H                                                               
         CLI   1(R2),4                                                          
         BH    PARMERR             NO NUMBER MORE THAN 9999                     
         TM    3(R2),X'80'                                                      
         BZ    PARMERR             MUST BE NUMERIC                              
         OC    OLIMIT,OLIMIT                                                    
         BNZ   DUPERR                                                           
         SR    R1,R1                                                            
         ICM   R1,15,8(R2)                                                      
         CVD   R1,DUB                                                           
         MVC   OLIMIT,DUB+4                                                     
         B     ENDSCAN                                                          
         SPACE 2                                                                
ENDSCAN  B     CARDREAD                                                         
         SPACE 2                                                                
ENDCARD  CLI   TYPTABLE,0          TEST FOR RECORD TYPE CONTROL CARD            
         BE    MISREC              NONE                                         
         B     INITIAL             GO INITIALIZE TAPE READ                      
         SPACE 2                                                                
MISREC   DS    0H                                                               
         MVC   P+10(30),=CL30'**MISSING RECORD TYPE FILTER**'                   
         B     ERRPRT                                                           
         SPACE                                                                  
OPTERR   DS    0H                                                               
         MVC   P+10(30),=CL30'**INVALID OPTION KEYWORD**'                       
         B     ERRPRT                                                           
         SPACE 1                                                                
PARMERR  DS    0H                                                               
         MVC   P+10(30),=CL30'**INVALID PARAMETER VALUE**'                      
         B     ERRPRT                                                           
         SPACE 1                                                                
CONERR   DS    0H                                                               
         MVC   P+10(30),=CL30'**INVALID CONTROL CARD**'                         
         B     ERRPRT                                                           
         SPACE                                                                  
DUPERR   DS    0H                                                               
         MVC   P+10(30),=CL30'**DUPLICATE CONTROL CARD**'                       
         B     ERRPRT                                                           
         SPACE 1                                                                
ERRPRT   GOTO1 PRINTER                                                          
         GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   CARD(2),=C'/*'      FLUSH THE READER                             
         BE    *+14                EOF ENCOUNTERED                              
         MVC   P+10(80),CARD                                                    
         B     ERRPRT                                                           
*                                                                               
         GOTO1 PRINTER                                                          
         MVC   P+10(30),=CL30'**JOB ENDED DUE TO ERROR**'                       
         GOTO1 PRINTER                                                          
         B     ENDJOB                                                           
         SPACE 2                                                                
ENDJOB   XBASE                                                                  
         EJECT                                                                  
*&&DO                                                                           
INITIAL  OPEN  IN                                                               
*&&                                                                             
*&&OS                                                                           
INITIAL  OPEN  (IN,(INPUT))                                                     
*&&                                                                             
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK                             
         GOTO1 PRINTER                                                          
         OC    OLIMIT,OLIMIT                                                    
         BNZ   INPUT                                                            
         LA    RE,LIMIT            SET DEFAULT OUTPUT LIMIT                     
         CVD   RE,DUB                                                           
         MVC   OLIMIT,DUB+4                                                     
         B     INPUT                                                            
         SPACE 2                                                                
INPUT    LA    R2,RECVHDR-4                                                     
         GET   IN,(R2)                                                          
         SPACE                                                                  
         CLI   RFILTY,X'82'        MAKE SURE ITS REP FILE                       
         BE    *+12                                                             
         CLI   RFILTY,X'81'        OR REP DIRECTORY ITEM                        
         BNE   INPUT                                                            
         SPACE                                                                  
         LH    R1,RECVHDR-4                                                     
         LA    R1,RECVHDR-4(R1)    POINT TO EOR AND MARK IT                     
         XC    0(3,R1),0(R1)                                                    
         SPACE                                                                  
INPUT2   LA    R3,RECVHDR          R3 POINTS TO HEADER                          
         LA    R4,RKEY             AND R4 TO KEY                                
         SPACE                                                                  
INPUT3   DS    0H                                                               
         BAS   RE,REC                                                           
         BNE   INPUT                                                            
         TM    CONTROL,CONNUM      TEST FOR CONTRACT NUMBER FILTER              
         BZ    *+12                                                             
         BAS   RE,CNUM                                                          
         BNE   INPUT               READ ANOTHER RECORD                          
*                                                                               
         TM    CONTROL,REPCODE                                                  
         BZ    *+12                                                             
         BAS   RE,REP                                                           
         BNE   INPUT                                                            
*                                                                               
         TM    CONTROL,CALLET                                                   
         BZ    *+12                                                             
         BAS   RE,STA                                                           
         BNE   INPUT                                                            
*                                                                               
         TM    CONTROL,LUID        TERMINAL FILTERING                           
         BZ    INPUT4                                                           
*                                                                               
         LA    RE,RECVHDR-4        POINT TO RECORD LENGTH                       
         AH    RE,0(RE)            ADD RECORD LENGTH                            
         BCTR  RE,0                BACK UP TO THE LAST BYTE OF TRAILER          
         ZIC   RF,0(RE)            GET LENGTH OF TAILER                         
         BCTR  RF,0                                                             
         SR    RE,RF               BACK UP TO THE RECOVERY EXTENSION            
         USING RECVEXTD,RE                                                      
         CLC   RLUID,LUIDFILT                                                   
         BNE   INPUT               READ NEXT RECORD                             
         DROP  RE                                                               
*                                                                               
INPUT4   LH    R2,RECVHDR-4        LENGTH OF RECORD PLUS HEADER                 
         SH    R2,=H'4'            SUBTRACT FOR RECORD LENGTH                   
         MVC   MSG,SPACES                                                       
         MVC   MSG+20(5),=C'SIN ='                                              
         L     R5,RSIN                                                          
         EDIT  (R5),(6,MSG+26)                                                  
         CLI   0(R4),X'0C'                                                      
         BE    INPUT5                                                           
*                                                                               
         LA    RE,KDISTAB          TABLE OF RECORDS WITH                        
         LA    R1,KDISENT          CONTRACT NUMBER                              
         CLC   0(1,R4),0(RE)                                                    
         BE    INPUT5                                                           
         LA    RE,L'KDISTAB(RE)                                                 
         BCT   R1,*-14                                                          
         B     INPUT6              NOT IN TABLE                                 
*                                                                               
INPUT5   DS    0H                                                               
         XC    DUB2,DUB2                                                        
         CLI   0(R4),X'0C'                                                      
         BE    *+14                                                             
         MVO   DUB2+3(5),KNUM                                                   
         B     *+10                                                             
         USING RCONREC,R4          YES-PRINT THE CONTRACT NUMBER                
         MVO   DUB2+3(5),RCONKCON                                               
         DROP  R4                                                               
         SPACE                                                                  
         OI    DUB2+7,X'0F'                                                     
         MVC   MSG+40(10),=C'CONTRACT ='                                        
         EDIT  (P5,DUB2+3),(8,MSG+51),ALIGN=LEFT                                
         SPACE                                                                  
INPUT6   CLI   RRECTY,X'01'        IS IT A COPY                                 
         BNE   INPUT8                                                           
         MVC   MSG(4),=C'COPY'                                                  
         AP    COPY,=P'1'          INCREMENT COUNTER                            
         B     INPUT12                                                          
         SPACE                                                                  
INPUT8   CLI   RRECTY,X'02'        IS IT A CHANGE                               
         BNE   INPUT10                                                          
         MVC   MSG(6),=C'CHANGE'                                                
         AP    CHG,=P'1'                                                        
         B     INPUT12                                                          
         SPACE                                                                  
INPUT10  CLI   RRECTY,X'03'                                                     
         BNE   INPUT                                                            
         MVC   MSG(3),=C'ADD'                                                   
         AP    ADD,=P'1'                                                        
         B     INPUT12                                                          
         SPACE                                                                  
INPUT12  DS    0H                                                               
         CP    TOTAL,OLIMIT                                                     
         BNL   INPUTX              OVER OUTPUT LIMIT                            
         LA    R3,L'MSG                                                         
         GOTO1 PRNTBL,DMCB,((R3),MSG),RECVHDR,C'DUMP',(R2),=C'2D'               
*                                                                               
INPUTX   DS    0H                                                               
         AP    TOTAL,=P'1'                                                      
         B     INPUT               READ NEXT RECORD                             
         EJECT                                                                  
* SUB-ROUTINE TO FILTER BY RECORD TYPE.  CC SET TO EQ FOR MATCH                 
* NEQ FOR NOT FOUND                                                             
*                                                                               
REC      DS    0H                                                               
         STM   R0,R1,DUB                                                        
         MVI   BYTE,X'FF'          SET NOT FOUND VALUE                          
         ZIC   R0,TYPTABLE                                                      
         LA    R1,TYPTABLE+2                                                    
         CLC   RKEY(1),0(R1)                                                    
         BE    REC2                FOUND A FILTERED RECORD TYPE                 
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     RECX                NOT FOUND                                    
*                                                                               
REC2     DS    0H                                                               
         MVI   BYTE,0                                                           
*                                                                               
RECX     DS    0H                                                               
         LM    R0,R1,DUB                                                        
         CLI   BYTE,0                                                           
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER ON CONTRACT NUMBER                                      
* CC=EQ IF FOUND, NEQ IF NOT FOUND                                              
*                                                                               
CNUM     NTR1                                                                   
         MVI   BYTE,X'FF'          SET NOT FOUND VALUE                          
         CLI   0(R4),X'0C'                                                      
         BNE   CNUM6                                                            
         LH    R0,CONENT           COUNTER                                      
         LA    RE,CONTABLE                                                      
         SPACE                                                                  
         USING RCONREC,R4                                                       
CNUM2    CLC   RCONKCON,0(RE)      K NUM VS. FILTER TABLE                       
         BL    CNUM4               NOT IN RANGE OF ENTRY                        
         CLC   RCONKCON,4(RE)                                                   
         BH    CNUM4                                                            
         MVI   BYTE,0              SET FOUND VALUE                              
         B     CNUMX               FOUND A CONTRACT                             
         DROP  R4                                                               
         SPACE                                                                  
CNUM4    LA    RE,L'CONTABLE(RE)                                                
         BCT   R0,CNUM2                                                         
         B     CNUMX                                                            
         SPACE                                                                  
CNUM6    LA    R0,KDISENT          COUNTER                                      
         LA    R6,KDISTAB          POINTER TO TABLE OF OFFSETS                  
         CLC   0(1,R4),0(R6)       RECORD TYPE VS. TABLE                        
         BE    *+20                FOUND                                        
         LA    R6,L'KDISTAB(R6)    TRY AGAIN                                    
         BCT   R0,*-14                                                          
         MVI   BYTE,0              SKIP FILTER IF RECORD TYPE                   
         B     CNUMX               IS NOT IN TABLE                              
*                                                                               
         ZIC   R5,1(R6)            INSERT DISPLACEMENT OF K NUM                 
         AR    R5,R4               INTO KEY IN R5 AND POINT R5 TO NUM           
         MVC   FULL,0(R5)          SAVE K NUMBER                                
         BAS   RE,CONKNUM          CONVERT TO CONTRACT FORMAT                   
         LH    R0,CONENT           COUNTER                                      
         LA    RE,CONTABLE         POINTER                                      
         SPACE                                                                  
CNUM7    CLC   KNUM,0(RE)                                                       
         BL    CNUM8                                                            
         CLC   KNUM,4(RE)                                                       
         BH    CNUM8                                                            
         MVI   BYTE,0              SET FOUND VALUE                              
         B     CNUMX                                                            
         SPACE                                                                  
CNUM8    LA    RE,L'CONTABLE(RE)   TRY NEXT ENTRY                               
         BCT   R0,CNUM7                                                         
         B     CNUMX               READ NEXT RECORD                             
*                                                                               
CNUMX    DS    0H                                                               
         CLI   BYTE,0              SET CONDITION CODE ON EXIT                   
         XIT1                                                                   
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER ON REP CODE (ON EXIT CC=EQ FOR FOUND, NEQ FOR           
* NOT FOUND)                                                                    
*                                                                               
REP      NTR1                                                                   
         MVI   BYTE,X'FF'          SET NOT FOUND VALUE                          
         LA    RE,RDISTAB          POINT TO REP CODE OFFSETS                    
         LA    R0,RDISENT                                                       
         SR    R1,R1                                                            
*                                                                               
         CLC   RKEY(1),0(RE)                                                    
         BE    REP2                                                             
         LA    RE,L'RDISTAB(RE)                                                 
         BCT   R0,*-14                                                          
         MVI   BYTE,0              RECORD TYPE NOT IN TABLE                     
         B     REPX                SKIP FILTER                                  
*                                                                               
REP2     DS    0H                                                               
         IC    R1,1(RE)            DISPLACEMENT IN RECORD                       
         LA    RE,RKEY(R1)         POINT TO FIELD                               
         CLC   REPFILT,0(RE)       FILTER AGAINST RECORD                        
         BNE   REPX                                                             
         MVI   BYTE,0              SET FOUND VALUE                              
*                                                                               
REPX     DS    0H                                                               
         CLI   BYTE,0              SET CONDITION CODE                           
         XIT1                                                                   
         SPACE 2                                                                
* SUB-ROUTINE TO APPLY STATION FILTER (SETS CC AS ABOVE)                        
*                                                                               
STA      DS    0H                                                               
         STM   RE,R1,DMCB                                                       
         MVI   BYTE,X'FF'                                                       
         LA    RE,SDISTAB          TABLE OF STATION OFFSETS IN                  
         LA    R0,SDISENT          RECORD                                       
         SR    R1,R1                                                            
*                                                                               
         CLC   RKEY(1),0(RE)                                                    
         BE    STA2                                                             
         LA    RE,L'SDISTAB(RE)                                                 
         BCT   R0,*-14                                                          
         MVI   BYTE,0              RECORD TYPE NOT IN TABLE                     
         B     STAX                SKIP FILTER                                  
*                                                                               
STA2     DS    0H                                                               
         IC    R1,1(RE)            OFFSET                                       
         CLI   RKEY,X'12'          TEST FOR INVENTORY                           
         BNE   STA4                                                             
         CLI   (RINVKSTA+4-RINVKEY)+RKEY,C'T'                                   
         BNE   *+8                 ERASE 'T' FOR COMPARE                        
         MVI   (RINVKSTA+4-RINVKEY)+RKEY,C' '                                   
*                                                                               
STA4     DS    0H                                                               
         LA    RE,RKEY(R1)         POINT TO STATION                             
         CLC   STAFILT,0(RE)                                                    
         BNE   STAX                                                             
         MVI   BYTE,0              SET FOUND                                    
*                                                                               
STAX     DS    0H                                                               
         LM    RE,R1,DMCB                                                       
         CLI   BYTE,0              SET CC BEFORE EXIT                           
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO DEAL WITH EOF CONDITION                                            
*                                                                               
*&&DO                                                                           
ENDIN    CLOSE IN                                                               
*&&                                                                             
*&&OS                                                                           
ENDIN    CLOSE (IN,)                                                            
*&&                                                                             
         SPACE                                                                  
ENDIN2   LA    R0,L'INMSG                                                       
         GOTO1 LOGIO,DMCB,1,((R0),INMSG)                                        
         MVI   ANSWER,C' '                                                      
         GOTO1 (RF),(R1),0,(3,ANSWER)                                           
         CLC   =C'EOF',ANSWER                                                   
         BE    ENDIN4                                                           
         CLC   =C'EOV',ANSWER                                                   
         BNE   ENDIN2                                                           
*&&DO                                                                           
         OPEN  IN                                                               
*&&                                                                             
*&&OS                                                                           
         OPEN  (IN,(INPUT))                                                     
*&&                                                                             
         B     INPUT                                                            
         SPACE                                                                  
ENDIN4   DS    0H                                                               
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK FOR TOTALS                  
         MVC   MID1+10(28),=C'TOTALS FOR EXTRACTED RECORDS'                     
         MVI   MID2+10,C'-'                                                     
         MVC   MID2+11(27),MID2+10                                              
         GOTO1 PRINTER                                                          
         SPACE                                                                  
         MVC   P+10(14),TOTAL+4                                                 
         MVI   P+25,C'='                                                        
         EDIT  (P4,TOTAL),(7,P+27)                                              
         MVI   SPACING+3,C'2'                                                   
         GOTO1 PRINTER                                                          
         SPACE                                                                  
         MVC   P+10(14),COPY+4                                                  
         MVI   P+25,C'='                                                        
         EDIT  (P4,COPY),(7,P+27)                                               
         GOTO1 PRINTER                                                          
         SPACE                                                                  
         MVC   P+10(14),CHG+4                                                   
         MVI   P+25,C'='                                                        
         EDIT  (P4,CHG),(7,P+27)                                                
         GOTO1 PRINTER                                                          
         SPACE                                                                  
         MVC   P+10(14),ADD+4                                                   
         MVI   P+25,C'='                                                        
         EDIT  (P4,ADD),(7,P+27)                                                
         GOTO1 PRINTER                                                          
         SPACE                                                                  
         B     ENDJOB                                                           
         EJECT                                                                  
* SUB-ROUTINE TO CONVERT FROM REVERSE NINES COMPLEMENT TO                       
* REGULAR NINES COMPLEMENT                                                      
*                                                                               
CONKNUM  ZAP   DUB2(5),=P'99999999'                                             
         PACK  DUB(1),FULL+3(1)    REVERSE DIGIT SEQUENCE                       
         PACK  DUB+1(1),FULL+2(1)                                               
         PACK  DUB+2(1),FULL+1(1)                                               
         PACK  DUB+3(1),FULL(1)                                                 
         MVI   DUB+4,X'0C'                                                      
         SRP   DUB(5),64-1,0       SHIFT ONE POSITION TO RIGHT                  
         SP    DUB2(5),DUB(5)      SUBTRACT FROM NINES                          
         SRP   DUB2(5),1,0         SHIFT 1 TO LEFT                              
         MVC   KNUM,DUB2                                                        
         BR    RE                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
INMSG    DC    C'SYS007 (RECOVERY) IS IT EOF OR EOV  '                          
         SPACE 2                                                                
* EXTERNAL MODULE DIRECTORY                                                     
*                                                                               
CARDS    DC    V(CARDS)                                                         
LOGIO    DC    V(LOGIO)                                                         
PRINTER  DC    V(PRINTER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
SCANNER  DC    V(SCANNER)                                                       
         SPACE 2                                                                
* TRANSACTION TYPE COUNTERS                                                     
*                                                                               
TOTAL    DC    PL4'0',CL14'TOTAL RECORDS'                                       
COPY     DC    PL4'0',CL14'COPIES'                                              
CHG      DC    PL4'0',CL14'CHANGES'                                             
ADD      DC    PL4'0',CL14'ADDS'                                                
         SPACE 2                                                                
* TABLES BUILT FROM CONTROL CARDS                                               
*                                                                               
*                                                                               
*                                                                               
* TABLE OF CONTROL CARD OPTION KEYWORDS AND VALIDATION ROUTINES                 
*                                                                               
         DS    0D                                                               
OPTABLE  DS    0CL12                                                            
         DC    CL8'RECORD',AL4(VALREC)                                          
         DC    CL8'CONNUM',AL4(VALNUM)                                          
         DC    CL8'STATION',AL4(VALSTA)                                         
         DC    CL8'REP',AL4(VALREP)                                             
         DC    CL8'LUID',AL4(VALLUID)                                           
         DC    CL8'OLIMIT',AL4(VALOLIM)                                         
OPTENT   EQU   (*-OPTABLE)/L'OPTABLE                                            
         SPACE 2                                                                
* TABLE OF RECORD TYPE PARAMETERS                                               
*                                                                               
RECTABLE DS    0CL9                                                             
         DC    CL8'CONTRACT',X'0C'                                              
         DC    CL8'BUYS',X'0B'                                                  
         DC    CL8'INVENTORY',X'12'                                             
RECENT   EQU   (*-RECTABLE)/L'RECTABLE                                          
         SPACE 2                                                                
* TABLE OF RECORD TYPE FILTERS                                                  
*                                                                               
TYPTABLE DS    0CL8                                                             
         DC    X'00'               COUNTER                                      
         DC    AL1(6)              LIMIT                                        
         DS    CL6                                                              
         SPACE 2                                                                
* TABLE OF CONTRACT NUMBER FILTERS                                              
*                                                                               
         DS    0F                                                               
CONENT   DC    H'0'                                                             
CONLIM   DC    H'20'                                                            
CONTABLE DS    20CL8                                                            
         SPACE 2                                                                
* TABLE OF OFFSETS OF CONTRACT NUMBER INTO RECORD KEY                           
*                                                                               
KDISTAB  DS    0CL2                                                             
         DC    X'0B',AL1(RBUYKCON-RBUYKEY)                                      
         DC    X'14',AL1(RAVLKCON-RAVLKEY)                                      
KDISENT  EQU   (*-KDISTAB)/L'KDISTAB                                            
         SPACE 2                                                                
* TABLE OF OFFSETS OF REP CODE INTO RECORD KEY                                  
*                                                                               
RDISTAB  DS    0CL2                                                             
         DC    X'02',AL1(RSTAKREP-RSTAKEY)                                      
         DC    X'0C',AL1(RCONKREP-RCONKEY)                                      
         DC    X'0B',AL1(RBUYKREP-RBUYKEY)                                      
         DC    X'12',AL1(RINVKREP-RINVKEY)                                      
         DC    X'14',AL1(RAVLKREP-RAVLKEY)                                      
RDISENT  EQU   (*-RDISTAB)/L'RDISTAB                                            
         SPACE 2                                                                
* TABLE OF OFFSETS OF STATION INTO RECORD KEY                                   
*                                                                               
SDISTAB  DS    0CL2                                                             
         DC    X'02',AL1(RSTAKSTA-RSTAKEY)                                      
         DC    X'0C',AL1(RCONKSTA-RCONKEY)                                      
         DC    X'12',AL1(RINVKSTA-RINVKEY)                                      
SDISENT  EQU   (*-SDISTAB)/L'SDISTAB                                            
         SPACE 2                                                                
         EJECT                                                                  
* DTF AND BUFFER AREAS                                                          
*                                                                               
*&&DO                                                                           
IN       DTFMT DEVADDR=SYS007,BLKSIZE=2100,RECFORM=VARBLK,             X        
               TYPEFLE=INPUT,IOAREA1=INAREA,FILABL=STD,                X        
               EOFADDR=ENDIN,WORKA=YES,REWIND=UNLOAD                            
*&&                                                                             
*&&OS                                                                           
IN       DCB   DDNAME=IN,              DOS SYS007                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=02096,                                            X        
               BLKSIZE=02100,          DOS BLKSIZE=02100               X        
               MACRF=GM,                                               X        
               EODAD=ENDIN                                                      
*&&                                                                             
         SPACE 2                                                                
*&&DO                                                                           
INAREA   DS    CL2100                                                           
*&&                                                                             
         SPACE 2                                                                
* WORKING STORAGE                                                               
*                                                                               
WORKD    DSECT                                                                  
KNUM     DS    CL4                                                              
FACTOR   DS    A                                                                
CONTROL  DS    B                                                                
*                                                                               
ANSWER   DS    CL3                                                              
BYTE     DS    C                                                                
MSG      DS    CL60                                                             
         DS    0D                                                               
HEADER   DS    CL8                                                              
CARD     DS    CL80                                                             
*                                                                               
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
*                                                                               
WORK     DS    CL480                                                            
WORK2    DS    CL128                                                            
*                                                                               
OLIMIT   DS    PL4                                                              
STAFILT  DS    CL5                                                              
REPFILT  DS    CL2                                                              
LUIDFILT DS    CL8                                                              
*                                                                               
         DS    0D                                                               
RHLEN    DS    CL4                                                              
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0CL27                                                            
         DS    2050C                                                            
WORKX    EQU   *                                                                
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
CONNUM   EQU   X'01'               FILTER ON CONTRACT NUMBER                    
REPCODE  EQU   X'02'                                                            
CALLET   EQU   X'04'                                                            
LUID     EQU   X'08'                                                            
LIMIT    EQU   200                 DEFAULT OUTPUT LIMIT                         
         SPACE 2                                                                
* REP RECORD DSECTS                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DMRCVREXT                                                                     
*                                                                               
         PRINT OFF                                                              
RECVEXTD DSECT                                                                  
       ++INCLUDE DMRCVREXT                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004RERCVXTR  05/01/02'                                      
         END                                                                    
