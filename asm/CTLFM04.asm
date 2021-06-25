*          DATA SET CTLFM04    AT LEVEL 019 AS OF 05/01/02                      
*PHASE TA0204A                                                                  
*INCLUDE GETBOOK                                                                
         TITLE 'CTLFM04 - CONTROL FILE MAINT - BOOK RECORDS'                    
CTLFM04  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 400,**LFM4**,RR=R8                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING CTLFM04+4096,RA                                                  
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTCREC,R4           R4=A(IO)                                     
         USING WRKD,RC             RC=A(TEMP W/S)                               
         XC    BOKSTMT,BOKSTMT                                                  
         OI    BOKSTMTH+6,X'80'                                                 
         ST    R8,RELO                                                          
         B     START                                                            
RELO     DC    A(0)                                                             
         EJECT                                                                  
*              VALIDATE SEQUENCE                                                
*                                                                               
START    LA    R1,BOKIDH                                                        
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         MVI   READKEY,C'N'                                                     
         MVC   THISTYPE,BOKID                                                   
         BAS   RE,KEYVAL           VALIDATE AND BUILD KEY                       
         CLI   FERN,X'FF'          ALL KEY FIELDS MUST BE INPUT AND OK          
         BNE   EXIT                                                             
         MVC   KEYNEXT,KEY                                                      
*                                  VALIDATE SEQUENCE INPUT                      
         MVI   DISPTYPE,1          SET DISPLAY TO FI                            
         LA    R1,BOKSEQH                                                       
         XC    SEQALL,SEQALL       SET DEFAULT VALUES                           
         GOTO1 AFVAL                                                            
         MVI   SEQIN,1             SET TO NOT RESOLVE INCLUDES                  
         BZ    SEQ12                                                            
         GOTO1 VSCANNER,DMCB,(0,FLDH),(0,LINES),0                               
         MVI   FNDX,1                                                           
         MVC   NLINES,4(R1)        SAVE # OF INPUT LINES                        
         CLI   NLINES,3                                                         
         BH    ETMI                TOO MANY INPUT FIELDS                        
*                                                                               
         LA    R7,LINES                                                         
         TM    2(R7),X'80'                                                      
         BO    SEQ4                                                             
         CLI   12(R7),C'I'         I=X                                          
         BE    SEQ8                                                             
         CLI   12(R7),C'F'                                                      
         BE    SEQ2                                                             
         CLI   12(R7),C'N'                                                      
         BNE   EIIF                                                             
         CLI   1(R7),0             NE(,I=X)                                     
         BNE   EIIF                                                             
         SR    R6,R6                                                            
         IC    R6,0(R7)                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R7),=CL10'NEXT'                                             
         BNE   EIIF                                                             
         CLI   ACTN,3              NE ONLY VALID FOR DISPLAY                    
         BNE   EFNV                                                             
         CLI   LACTN,3             LAST ACTION S/B DISPLAY                      
         BNE   EIAS                                                             
         CLC   KEY,LKEY            AND KEY MUST NOT HAVE CHANGED                
         BNE   EIAS                                                             
         CLI   LASTST,X'FF'        DID WE REACH END OF BOOK                     
         BE    EIIF                                                             
         MVC   SEQLO,LASTHI       SET NEXT DISPLAY FROM LAST DISPLAYED          
         MVI   DISPTYPE,2          SET DISPLAY TO NE                            
         B     SEQ6                                                             
         SPACE 1                                                                
SEQ2     CLI   1(R7),0                                                          
         BNE   EIIF                FI(,I=X)                                     
         SR    R6,R6                                                            
         IC    R6,0(R7)                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R7),=CL10'FIRST'                                            
         BNE   EIIF                                                             
         B     SEQ6                                                             
         SPACE 1                                                                
SEQ4     CLC   4(4,R7),=F'250'     AAA(,BBB(,I=X))                              
         BH    EFTB                                                             
         MVC   SEQLO,7(R7)         SAVE AAA VALUE                               
         LA    R7,32(R7)                                                        
         MVI   FNDX,2                                                           
         MVI   DISPTYPE,2          SET DISPLAY TO NE                            
         CLC   FNDX,NLINES                                                      
         BH    ACTVAL                                                           
         TM    2(R7),X'80'                                                      
         BZ    SEQ8                                                             
         CLI   1(R7),0                                                          
         BNE   EIIF                                                             
         TM    2(R7),X'80'                                                      
         BNO   EFNN                                                             
         CLC   4(4,R7),=F'250'                                                  
         BH    EFTB                                                             
         MVC   SEQHI,7(R7)         SAVE BBB VALUE                               
         CLC   SEQLO,SEQHI                                                      
         BH    EIIF                                                             
         ZIC   R1,SEQLO                                                         
         ZIC   R0,SEQHI                                                         
         SR    R0,R1                                                            
         CH    R0,=H'14'           CAN ONLY DISPLAY 15 STMNTS                   
         BH    EIIF                                                             
         MVI   DISPTYPE,0          SET DISPLAY TO AAA,BBB                       
         SPACE 1                                                                
SEQ6     LA    R7,32(R7)           XXX(,I=X)                                    
         SR    R6,R6                                                            
         IC    R6,FNDX                                                          
         LA    R6,1(R6)                                                         
         STC   R6,FNDX                                                          
         CLC   FNDX,NLINES                                                      
         BH    ACTVAL                                                           
         SPACE 1                                                                
SEQ8     CLI   12(R7),C'I'                                                      
         BNE   EIIF                                                             
         CLI   1(R7),1                                                          
         BNE   EIIF                                                             
         CLI   22(R7),C'N'                                                      
         BE    SEQ10                                                            
         CLI   22(R7),C'Y'                                                      
         BNE   EIIF                                                             
         MVI   SEQIN,0             SET TO RESOLVE INCLUDES                      
         SPACE 1                                                                
SEQ10    CLI   SEQIN,0             I=Y IS ONLY VALID FOR DISPLAY                
         BNE   ACTVAL                                                           
         CLI   ACTN,3                                                           
         BE    ACTVAL                                                           
         B     EFNV                                                             
         SPACE 1                                                                
SEQ12    MVI   DISPTYPE,3          SET NO NO INPUT                              
         EJECT                                                                  
*                                                                               
*              VALIDATE THIS/LAST ACTIONS                                       
*                                                                               
ACTVAL   MVI   FNDX,0                                                           
         CLI   ACTN,7                                                           
         BE    ACTV0                                                            
         CLI   DISPTYPE,3                                                       
         BNE   *+12                                                             
         CLI   ACTN,1                                                           
         BE    ACTV0                                                            
         CLI   DISPTYPE,0                                                       
         BE    *+12                                                             
         CLI   ACTN,3              FI/NE ONLY VALID FOR DISPLAY                 
         BNE   EFNV                                                             
         CLI   ACTN,2                                                           
         BNE   ACTV0                                                            
         MVI   ACTN,3              SET ACTION TO DISPLAY                        
         CLC   KEY,LKEY                                                         
         BNE   ACTV0                                                            
         CLI   DISPTYPE,0                                                       
         BNE   ACTV0                                                            
         CLC   LASTLO(2),SEQLO                                                  
         BNE   ACTV0                                                            
         MVI   ACTN,2              RESET ACTION IF NOTHING CHANGED              
ACTV0    LA    R1,BOKIDH                                                        
         ST    R1,FADR                                                          
         CLI   ACTN,3                                                           
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST FOR N/F                                 
         BZ    ACTV2                                                            
         CLI   ACTN,1              ONLY VALID FOR ADD                           
         BE    ADD                                                              
         CLI   ACTN,7              AND COPY                                     
         BE    COPY                                                             
         B     ERNF                                                             
         SPACE 1                                                                
ACTV2    CLI   ACTN,1              RECORD CANT EXIST FOR ADD                    
         BE    ERAE                                                             
         CLI   ACTN,7              OR COPY                                      
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        TEST FOR DELETED                             
         BZ    *+12                                                             
         CLI   ACTN,3              DELETED REC CAN ONLY BE DISPLAYED            
         BNE   ERNF                                                             
         CLI   ACTN,2              CHANGE                                       
         BE    CHANGE                                                           
         MVC   BOOKSTAT,CTCSTAT    SAVE BOOK HEADER STATUS                      
         EJECT                                                                  
DISPLAY  TWAXC BOKDESCH,PROT=Y                                                  
         SPACE 1                                                                
DISP0E   DS    0H                                                               
         LA    R6,CTCDATA                                                       
         SR    R1,R1                                                            
         XC    TEMP,TEMP                                                        
         SPACE 1                                                                
DISP2    CLI   0(R6),0                                                          
         BE    DISP10                                                           
         CLI   0(R6),X'02'         DESCRIPTION ELEMENT                          
         BE    DISP6                                                            
         CLI   0(R6),X'04'         INDEX ELEMENT                                
         BE    DISP8                                                            
         SPACE 1                                                                
DISP4    IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISP2                                                            
         SPACE 1                                                                
DISP6    IC    R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOKDESC(0),2(R6)                                                 
         LA    R1,1(R1)                                                         
         STC   R1,BOKDESCH+5                                                    
         STC   R1,BOKDESCH+7                                                    
         B     DISP4                                                            
         SPACE 1                                                                
DISP8    MVC   TEMP,0(R6)          SAVE INDEX ELEMENT                           
         BAS   RE,DISPSTMT                                                      
         B     DISP4                                                            
         SPACE 1                                                                
DISP10   CLI   TEMP,0                                                           
         BNE   *+6                                                              
         DC    H'0'                DIE IF INDEX ELEMENT NOT FOUND               
         MVI   FNDX,1              CHECK IF SEQ #S ARE WITHIN LIMITS            
         LA    R1,BOKSEQH                                                       
         ST    R1,FADR                                                          
         CLC   SEQLO,BKHIVAL                                                    
         BH    EFTB                                                             
         MVI   FNDX,2                                                           
         CLC   SEQHI,BKHIVAL                                                    
         BH    EFTB                                                             
         OC    SEQLO(2),SEQLO                                                   
         BNZ   DISP12                                                           
         MVC   SEQLO(2),=X'010F'   SET TO DISPLAY FIRST 15 CARDS                
         B     DISP14                                                           
         SPACE 1                                                                
DISP12   CLI   SEQHI,0             ON NEXT SET SEQHI TO 15 PAST LAST            
         BNE   DISP14              HIGH DISPLAYED OR 255 IF GR MAX              
         SR    R1,R1                                                            
         IC    R1,SEQLO                                                         
         LA    R1,15(R1)                                                        
         C     R1,=F'250'                                                       
         BNH   *+8                                                              
         LA    R1,250                                                           
         STC   R1,SEQHI                                                         
         SPACE 1                                                                
DISP14   XC    DMCB(24),DMCB     SET UP GETBOOK PARAMETER LIST                  
         L     R1,AFACLIST                                                      
         L     R1,0(R1)                                                         
         MVC   DMCB+12(1),TRMNUM                                                
         ST    R1,DMCB+8                                                        
         LA    R1,CIO                                                           
         ST    R1,DMCB+4                                                        
         MVC   DMCB+4(1),SEQLO                                                  
         LA    R1,GETAREA                                                       
         ST    R1,DMCB                                                          
         MVC   DMCB(1),SEQIN                                                    
         L     R1,=V(GETBOOK)                                                   
         A     R1,RELO                                                          
         ST    R1,AGETBOOK                                                      
         MVI   LINCNT,0                                                         
         MVC   GETAREA(25),KEY     FOR FIRST CALL                               
         MVI   FNDX,0                                                           
         LA    R1,BOKIDH                                                        
         ST    R1,FADR                                                          
         SPACE 1                                                                
DISP16   GOTO1 AGETBOOK,DMCB                                                    
         TM    DMCB+8,X'80'        END OF BOOK                                  
         BO    DISP17                                                           
         CLI   DMCB+8,0            OK                                           
         BE    DISP17                                                           
         TM    DMCB+8,X'02'        DELETED                                      
         BZ    EIIO                                                             
         SPACE 1                                                                
DISP17   DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,LINCNT                                                        
         LA    R6,1(R1)            R6 = LINE # OF NEXT CARD                     
         STC   R6,LINCNT                                                        
         TM    DMCB+8,X'80'        END OF BOOK                                  
         BO    DISP18                                                           
         MH    R1,=H'92'                                                        
         LA    R1,BOKLINAH(R1)     POINT R1 TO SCREEN LINE                      
         CLC   CIO(10),=C'++INCLUDE '                                           
         BNE   *+10                                                             
         MVC   CIO+10(1),THISTYPE                                               
         MVC   8(72,R1),CIO        MOVE IN DATA                                 
         MVI   5(R1),72                                                         
         MVI   7(R1),72                                                         
         LA    R1,80(R1)           POINT R1 TO SEQ# HEADER                      
         MVC   8(4,R1),CIO+72                                                   
         MVI   5(R1),4                                                          
         MVI   7(R1),4                                                          
         OI    6(R1),X'80'                                                      
         CLI   LINCNT,14           IS SCREEN FULL                               
         BH    DISP18                                                           
         IC    R6,LINCNT                                                        
         SR    R1,R1                                                            
         IC    R1,SEQLO                                                         
         AR    R1,R6                                                            
         STC   R1,WORK                                                          
         CLC   WORK(1),SEQHI                                                    
         BH    DISP18                                                           
         B     DISP16              GET NEXT CARD IMAGE                          
         SPACE 1                                                                
DISP18   CLI   LINCNT,0                                                         
         BNE   *+14                                                             
         XC    LASTLO(2),LASTLO                                                 
         B     DISP20                                                           
         MVC   LASTLO,SEQLO        SAVE LOW SEQ#DISPLAYED                       
         SR    R1,R1                                                            
         SR    R6,R6                                                            
         IC    R1,LINCNT                                                        
         IC    R6,SEQLO                                                         
         AR    R1,R6                                                            
         BCTR  R1,0                                                             
         STC   R1,LASTHI           SAVE HIGH SEQ# DISPLAYED                     
         SPACE 1                                                                
DISP20   MVI   LASTST,0                                                         
         TM    DMCB+8,X'80'                                                     
         BNO   *+8                                                              
         MVI   LASTST,X'FF'                                                     
         TM    BOOKSTAT,X'80'      IS BOOK DELETED ?                            
         BO    DISP20B                                                          
         MVI   NACTN,0                                                          
         CLI   DISPTYPE,0                                                       
         BE    DISP20A                                                          
         MVI   LASTLO,0                                                         
         LA    R1,BASACTNH                                                      
* DO && HERE                                                                    
         MVI   NACTN,X'0A'         SET OK TO DELETE/COPY                        
         B     DISPXX                                                           
         SPACE 1                                                                
DISP20A  DS    0H                                                               
* DO && HERE                                                                    
         MVI   NACTN,X'0B'         SET OK TO CHANGE/DELETE/COPY                 
         LA    R1,BOKLINAH                                                      
         B     DISPXX                                                           
         SPACE 1                                                                
DISP20B  MVI   NACTN,4             SET OK TO RESTORE                            
         LA    R1,BASACTNH                                                      
         SPACE 1                                                                
DISPXX   ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              ADD A NEW BOOK                                                   
*                                                                               
ADD      MVC   KEYSAVE,KEY                                                      
         BAS   RE,DATAVAL                                                       
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         MVC   KEY,KEYSAVE                                                      
         XC    FLD,FLD             USE FLD TO BUILD INDEX ELEMENT               
         MVC   FLD(2),=X'04FE'                                                  
         LA    R6,GETAREA                                                       
         SR    R7,R7               R7=SEQ# OF DATA CARD                         
         LA    R8,15               R8=LOOP COUNT FOR TABLE                      
ADD2     TM    0(R6),X'90'         IGNORE NULLS AND ++D'S FOR ADD               
         BNZ   ADDNEXT                                                          
         LA    R7,1(R7)            BUMP INDEX COUNTER                           
         STC   R7,KEY+24           MOVE SEQ# TO KEY                             
         MVI   TEMP,0                                                           
         GOTO1 ABLDREC             BUILD KEY                                    
         MVC   TEMP(74),6(R6)      MOVE ELEMENT TO TEMP                         
         GOTO1 ABLDREC             ADD DATA/++I ELEMENT                         
         GOTO1 AADD                AND WRITE IT                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,FLD+2(R7)                                                     
         STC   R7,0(R1)            ADD NEW CARD TO INDEX                        
ADDNEXT  LA    R6,80(R6)           BUMP TO NEXT INPUT LINE                      
         BCT   R8,ADD2                                                          
         LTR   R7,R7                                                            
         BZ    ADDERR                                                           
         STC   R7,FLD+2            MAKE ENTRY FOR HIGHEST SEQ# USED             
         BAS   RE,BUILDBH          BUILD BOOK HEADER (KEY AT KEYSAVE)           
         GOTO1 AADD                AND WRITE IT                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   NACTN,X'0B'         SET OK TO CHANGE/DELETE/COPY                 
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
ADDERR   LA    R1,BOKLINAH                                                      
         ST    R1,FADR                                                          
         B     EMIF                                                             
         EJECT                                                                  
*              COPY AN EXISTING BOOK                                            
*                                                                               
COPY     MVC   KEYSAVE,KEY         SAVE COPY-TO KEY                             
         MVC   KEYNEXT,LKEY                                                     
         MVC   KEY,LKEY            READ COPY-FROM INDEX                         
         LA    R1,BOKIDH                                                        
         ST    R1,FADR                                                          
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0            MUST BE OK                                   
         BNE   EIIO                                                             
         LA    R6,CTCDATA                                                       
         SR    R5,R5                                                            
COPY2    CLI   0(R6),0             FIND INDEX ELEMENT                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'04'                                                      
         BE    *+14                                                             
         IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     COPY2                                                            
         MVC   TEMP,0(R6)          SAVE OLD INDEX                               
         XC    FLD,FLD             BUILD NEW INDEX INTO FLD                     
         MVC   FLD(2),=X'04FE'                                                  
         LA    R6,TEMP+3           R6=A(OLD INDEX)                              
         LA    R5,FLD+3            R5=A(NEW INDEX)                              
         SR    R7,R7               R7=NEW SEQUENCE NUMBER                       
COPY4    CLI   0(R6),0             END OF ACTIVE OLD INDEX                      
         BE    COPY6                                                            
         MVC   KEY,LKEY            NO - READ RECORD                             
         MVC   KEY+24(1),0(R6)                                                  
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0            MUST BE OK                                   
         BNE   EIIO                                                             
         MVC   CTCKEY,KEYSAVE      RESTIRE COPY-TO KEY                          
         LA    R7,1(R7)                                                         
         STC   R7,CTCKEY+24        SET SEQUENCE NUMBER IN KEY & INDEX           
         STC   R7,0(R5)                                                         
         MVC   KEY,CTCKEY          AND ADD THE RECORD                           
         GOTO1 AADD                                                             
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    R6,1(R6)            BUMP INDEX POINTERS                          
         LA    R5,1(R5)                                                         
         B     COPY4                                                            
COPY6    STC   R7,FLD+2            SET HIGH SEQUENCE IN NEW INDEX               
         BAS   RE,BUILDBH          BUILD INDEX & ADD RECORD                     
         MVC   KEYNEXT,KEY                                                      
         GOTO1 AADD                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   NACTN,X'0B'         SET OK TO CHANGE/DELETE/COPY                 
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              CHANGE BOOK DATA                                                 
*                                                                               
CHANGE   MVC   KEYSAVE,KEY         SAVE BOOK KEY (MASTER)                       
         BAS   RE,DATAVAL                                                       
         MVC   KEY,KEYSAVE                                                      
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         LA    R6,CTCDATA                                                       
         SR    R7,R7                                                            
         XC    FLD,FLD                                                          
CHANGE2  CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'04'                                                      
         BE    *+14                                                             
         IC    R7,1(R6)                                                         
         AR    R6,R7                                                            
         B     CHANGE2                                                          
         MVC   FLD(254),0(R6)      SAVE OLD INDEX ELEMENT                       
         CLC   LASTLO(2),SEQLO     ARE WE DEALING WITH SAME DATA                
         BE    CHANGE4                                                          
         LA    R1,BOKSEQH          NO-THEY HAVE CHANGED SEQ#'S                  
         ST    R1,FADR                                                          
         B     ESNC                                                             
CHANGE4  MVC   INDHI,FLD+2         SAVE HIGH SEQ#                               
         XC    INDACT,INDACT       CLEAR ACTIVE INDEX                           
         XC    INDPAS,INDPAS       CLEAR PASSIVE INDEX                          
         XC    WORK(2),WORK        CLEAR COUNTERS                               
         MVI   LINCNT,0                                                         
         XC    ADDLST,ADDLST       SPLIT UP MASTER INDEX                        
         LA    R6,FLD+3                                                         
         LR    R7,R6                                                            
         CLI   0(R6),0             LOOK FOR END OF ACTIVE INDEX                 
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         LA    R8,1(R6)            R8 POINTS TO START OF PASSIVE INDEX          
         SR    R6,R7               R6 L'ACTIVE INDEX+1                          
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   INDACT(0),0(R7)     MOVE IN ACTIVE INDEX                         
         STC   R6,WORK             SAVE # OF ACTIVE ENTRIES                     
         LR    R6,R8                                                            
         CLI   0(R6),0             LOOK FOR END OF PASSIVE INDEX                
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         SR    R6,R8               R6 L'PASSIVE INDEX+1                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   INDPAS(0),0(R8)     MOVE IN PASSIVE INDEX                        
         STC   R6,WORK+1           SAVE # OF PASSIVE ENTRIES                    
CHANGE6  LA    R6,GETAREA          LOOP DOWN INPUT TABLE FOR NEW CARDS          
         LA    R7,15                                                            
CHANGE8  CLI   1(R6),0             NEW CARDS HAVE NO SEQUENCE NUMBERS           
         BNE   CHANGE10                                                         
         TM    0(R6),X'60'         AND MUST BE DATA OR ++INCLUDE                
         BNZ   CHANGE12                                                         
CHANGE10 LA    R6,80(R6)                                                        
         BCT   R7,CHANGE8                                                       
         B     CHANGE18                                                         
CHANGE12 SR    R1,R1                                                            
         IC    R1,LINCNT                                                        
         LA    R1,1(R1)                                                         
         STC   R1,LINCNT           LINCNT=# OF ADDS                             
         CLI   WORK+1,0            ARE THERE ANY PASSIVE ENTRIES                
         BE    CHANGE14                                                         
         SR    R8,R8               YES-SUBTRACT PASSIVE COUNTER                 
         IC    R8,WORK+1                                                        
         BCTR  R8,0                                                             
         STC   R8,WORK+1           SAVE DECREMENTED COUNT                       
         LA    R8,INDPAS(R8)       AND POINT TO SEQ#                            
         MVC   KEY+24(1),0(R8)     MOVE ASSIGNED SEQ# TO KEY                    
         MVI   0(R8),0             DELETE PASSIVE INDEX SEQ#                    
         BAS   RE,GETCARD                                                       
         BAS   RE,BUILDCD          BUILD A CARD (KEY=KEY,DATA AT 6(R6))         
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BE    CHANGE16                                                         
         DC    H'0'                                                             
CHANGE14 SR    R8,R8               IF NO AVAILABLE RE-CYCLE CARDS               
         IC    R8,INDHI            ASSIGN NEXT HIGH SEQ#                        
         LA    R8,1(R8)                                                         
         C     R8,=F'250'                                                       
         BH    EBIF                BOOK INDEX FULL                              
         STC   R8,KEY+24                                                        
         STC   R8,INDHI                                                         
         BAS   RE,BUILDCD                                                       
         GOTO1 AADD                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
CHANGE16 MVI   0(R6),0                                                          
         BCTR  R1,0                                                             
         LA    R1,ADDLST(R1)                                                    
         MVC   0(1,R1),KEY+24      BUILD LIST OF ADDED CARDS                    
         B     CHANGE10                                                         
CHANGE18 CLI   LINCNT,0                                                         
         BE    CHANGE20                                                         
         SR    R1,R1                                                            
         IC    R1,SEQHI                                                         
         SR    R8,R8                                                            
         IC    R8,LINCNT                                                        
         LA    R1,INDACT(R1)       R1 POINTS TO INSERTION POSITION              
         LA    R6,INDACT+249                                                    
         SR    R6,R1               R6=# OF SAVE INDEX BYTES                     
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),0(R1)        SAVE RHS OF INDEX                            
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),ADDLST      MOVE IN ADD INDEX                            
         LA    R1,1(R8,R1)                                                      
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),FLD         RESTORE RHS OF INDEX                         
         SR    R1,R1                                                            
         IC    R1,WORK             GET ACTIVE COUNT                             
         LA    R1,1(R8,R1)         ADD IN NEW CARDS                             
         STC   R1,WORK             AND RESTORE COUNT                            
CHANGE20 LA    R6,GETAREA          RE-PROCESS TABLE FOR ++D'S                   
         LA    R7,15                                                            
CHANGE22 CLI   1(R6),0             IGNORE CARDS WITH NO SEQ#'S                  
         BE    CHANGE24                                                         
         TM    0(R6),X'80'         ++D CARDS                                    
         BO    CHANGE28                                                         
         TM    0(R6),X'60'         DATA CARDS                                   
         BNZ   CHANGE26                                                         
CHANGE24 LA    R6,80(R6)           BUMP TO NEXT INPUT FIELD                     
         BCT   R7,CHANGE22                                                      
         B     CHANGE30                                                         
CHANGE26 SR    R1,R1                                                            
         IC    R1,1(R6)            R1=SEQ# OF CARD                              
         BCTR  R1,0                                                             
         LA    R1,INDACT(R1)       R1 POINTS TO REAL SEQ#                       
         MVC   KEY+24(1),0(R1)                                                  
         BAS   RE,GETCARD                                                       
         BAS   RE,BUILDCD          BUILD DATA RECORD                            
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R6),0                                                          
         B     CHANGE24                                                         
CHANGE28 SR    R1,R1                                                            
         IC    R1,WORK+1                                                        
         LR    RE,R1                                                            
         SR    R8,R8                                                            
         IC    R8,1(R6)                                                         
         BCTR  R8,0                                                             
         LA    R8,INDACT(R8)                                                    
         LA    R1,INDPAS(R1)                                                    
         MVC   0(1,R1),0(R8)       MOVE ACTIVE ENTRY TO PASSIVE INDEX           
         MVI   0(R8),0             AND TURN OFF ACTIVE                          
         LA    RE,1(RE)                                                         
         STC   RE,WORK+1                                                        
         MVI   0(R6),0                                                          
         SR    R8,R8                                                            
         IC    R8,WORK             DECREMENT ACTIVE INDEX COUNTER               
         BCTR  R8,0                                                             
         STC   R8,WORK                                                          
         B     CHANGE24                                                         
CHANGE30 LA    R7,INDACT           RE-BUILD INDEX                               
         LA    R8,1(R7)                                                         
         SR    R1,R1                                                            
         IC    R1,WORK             R1=# OF ACTIVE ENTRIES                       
         LA    R6,251                                                           
         MVC   FLD(2),=X'04FE'                                                  
         XC    FLD+3(251),FLD+3    CLEAR INDEX                                  
         MVC   FLD+2(1),INDHI      MOVE IN NEW HIGH VALUE                       
         LTR   R1,R1                                                            
         BZ    CHANGE36                                                         
CHANGE32 CLI   0(R7),0                                                          
         BE    CHANGE34                                                         
         LA    R7,1(R7)                                                         
         BCT   R6,CHANGE32                                                      
         B     CHANGE36                                                         
CHANGE34 LA    R8,1(R7)            SQUASH UP ACTIVE INDEX                       
         CLI   0(R8),0                                                          
         BNE   *+16                                                             
         LA    R8,1(R8)                                                         
         BCT   R6,*-12                                                          
         B     CHANGE36                                                         
         MVC   0(1,R7),0(R8)                                                    
         LA    R7,1(R7)                                                         
         LA    R8,1(R8)                                                         
         B     CHANGE34+4                                                       
CHANGE36 MVI   0(R7),0                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD+3(0),INDACT                                                  
         LA    R1,FLD+5(R1)                                                     
         SR    R6,R6                                                            
         IC    R6,WORK+1           R6=# OF PASSIVE INDEX ENTRIES                
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),INDPAS                                                   
         BAS   RE,BUILDBH          BUILD A BOOK HEADER                          
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,X'0B'         SET OK TO CHANGE/DELETE/COPY                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              ROUTINE TO BUILD A BOOK HEADER                                   
*                                                                               
BUILDBH  NTR1                                                                   
         MVC   KEY,KEYSAVE         RESTORE SAVED KEY                            
         MVI   TEMP,0                                                           
         GOTO1 ABLDREC             BUILD KEY                                    
         MVI   TEMP,1                                                           
         GOTO1 ABLDREC             BUILD ACTIVITY ELEMENT                       
         CLI   BOKDESCH+5,0                                                     
         BE    BUILDB2                                                          
         SR    R1,R1               BUILD DESCRIPTION ELEMENT                    
         IC    R1,BOKDESCH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+2(0),BOKDESC                                                
         MVI   TEMP,2                                                           
         LA    R1,3(R1)                                                         
         STC   R1,TEMP+1                                                        
         GOTO1 ABLDREC                                                          
BUILDB2  MVC   TEMP,FLD            MOVE IN BUILT INDEX ELEMENT                  
         BAS   RE,DISPSTMT                                                      
         GOTO1 ABLDREC             AND ADD IT                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              ROUTINE TO BUILD A DATA RECORD                                   
*                                                                               
BUILDCD  NTR1                                                                   
         MVI   TEMP,0                                                           
         GOTO1 ABLDREC                                                          
         MVC   TEMP(74),6(R6)                                                   
         GOTO1 ABLDREC                                                          
         B     EXIT                                                             
*                                                                               
*              DO READ-FOR-UPDATE ON RECORD ABOUT TO BE WRITTEN                 
*                                                                               
GETCARD  NTR1                                                                   
         LA    R5,IOAREA2                                                       
         ST    R5,AREC                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,IOAREA                                                        
         ST    R5,AREC                                                          
         B     EXIT                                                             
*                                                                               
*              DISPLAY NUMBER OF STATEMENTS IN BOOK                             
*                                                                               
DISPSTMT NTR1                                                                   
         LA    R4,TEMP+3           R4=A(ACTIVE INDEX)                           
         SR    R5,R5                                                            
DISPSTM2 CLI   0(R4),0             END OF ACTIVE INDEX                          
         BE    DISPSTM4                                                         
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     DISPSTM2                                                         
DISPSTM4 STC   R5,BKHIVAL          R5=NUMBER OF ACTIVE STATEMENTS               
         EDIT  (R5),(3,BOKSTMT),ALIGN=LEFT,ZERO=NOBLANK                         
         LA    R4,BOKSTMT+1                                                     
         AR    R4,R0                                                            
         MVC   0(10,R4),=C'STATEMENTS'                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              ROUTINE TO BUILD 15X80 TABLE OF DATA FROM TWA                    
*                                                                               
DATAVAL  NTR1                                                                   
         LA    RE,GETAREA                                                       
         LA    RF,1200                                                          
         XCEF                                                                   
         LA    R6,GETAREA                                                       
         LA    R1,BOKLINAH                                                      
         LA    R5,15                                                            
         MVI   LINCNT,0                                                         
DATAV2   CLI   ACTN,1              IF ADD IGNORE INPUT THIS TIME BIT            
         BE    *+12                                                             
         TM    4(R1),X'80'                                                      
         BZ    *+12                                                             
         CLI   5(R1),0                                                          
         BNE   DATAV6                                                           
         OI    0(R6),X'10'         SET ON NOT INPUT BIT                         
         B     DATAVSQ                                                          
DATAV4   LA    R1,92(R1)                                                        
         LA    R6,80(R6)                                                        
         BCT   R5,DATAV2                                                        
         MVI   FERN,X'FF'          ALL INPUT IS VALID                           
         B     DATAVXX                                                          
DATAV6   MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         SR    R8,R8                                                            
         IC    R8,5(R1)                                                         
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+2(0),8(R1)                                                  
         ST    R1,FADR                                                          
         CLC   TEMP+2(3),=C'++D'                                                
         BNE   *+12                                                             
         OI    0(R6),X'80'                                                      
         B     DATAVS              INSERT SEQ#                                  
         CLC   TEMP+2(10),=C'++INCLUDE '                                        
         BE    DATAV8                                                           
         LA    R7,TEMP+73          HANDLE DATA CARDS                            
         XC    TEMP(2),TEMP                                                     
         CLI   0(R7),C' '                                                       
         BNE   *+8                                                              
         BCT   R7,*-8                                                           
         CLI   0(R7),0                                                          
         BNE   *+8                                                              
         LA    R7,1(R7)                                                         
         LA    R8,TEMP-1                                                        
         SR    R7,R8                                                            
         STC   R7,TEMP+1                                                        
         MVI   TEMP,X'10'                                                       
         MVC   6(74,R6),TEMP       MOVE DATA ELEMENT TO TABLE                   
         OI    0(R6),X'20'         SET ON DATA BIT                              
         B     DATAVS              INSERT SEQ#                                  
DATAV8   XC    FLDH,FLDH           HANDLE ++ INCLUDES                           
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD                                               
         MVC   FLD+4(1),4(R1)                                                   
         CLI   5(R1),13            INPUT MUST BE AT LEAST 13 BYTES LONG         
         BL    EIIF                                                             
         LA    R7,TEMP+11                                                       
         LA    R8,20                                                            
         LA    R7,1(R7)                                                         
         CLI   0(R7),C' '                                                       
         BNE   *+12                                                             
         BCT   R8,*-12                                                          
         B     EIIF                                                             
         LR    R8,R7               R7 POINTS TO FIRST NON-BLANK                 
         CLI   1(R8),C' '                                                       
         BE    *+12                                                             
         LA    R8,1(R8)                                                         
         B     *-12                                                             
         SR    R8,R7               R8 LENGTH OF INCLUDE INFO                    
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),0(R7)                                                     
         LA    R8,1(R8)                                                         
         STC   R8,FLDH+5           PLOP IN NEW LENGTH FOR SCANNER               
         MVI   READKEY,C'Y'                                                     
         BAS   RE,KEYVAL           AND BUILD A KEY                              
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         CLI   SAVEDMCB,0                                                       
         BNE   ERNF                                                             
         MVC   TEMP(2),=X'111A'    BUILD INCLUDE ELEMENT                        
         MVC   TEMP+2(24),CTCKEY                                                
         MVC   6(26,R6),TEMP                                                    
         OI    0(R6),X'40'         SET ON INCLUDE BIT                           
DATAVS   SR    R8,R8                                                            
         IC    R8,LINCNT                                                        
         LA    R8,1(R8)                                                         
         STC   R8,LINCNT                                                        
DATAVSQ  LA    R8,80(R1)           R8 POINTS TO SEQ# HEADER                     
         OC    8(4,R8),8(R8)                                                    
         BZ    DATAV4                                                           
         PACK  DUB,9(3,R8)                                                      
         CVB   R8,DUB                                                           
         STC   R8,1(R6)            INSERT BINARY SEQ# INTO TABLE                
         B     DATAV4                                                           
DATAVXX  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              ROUTINE TO VALIDATE AND BUILD AKEY                               
*                                                                               
KEYVAL   NTR1                                                                   
         XC    CTCKEY,CTCKEY       CLEAR DOWN KEY                               
         CLI   FLDH+5,3                                                         
         BL    EFTS                                                             
         LA    R0,FLDH                                                          
         GOTO1 VSCANNER,DMCB,(R0),(0,LINES),0                                   
         MVC   NLINES,4(R1)                                                     
         CLI   NLINES,2            MUST ALWAYS BE TWO                           
         BL    EIIF                                                             
         LA    R7,LINES            POINT TO FIRST LINE                          
         MVI   FNDX,1                                                           
         CLI   0(R7),1             VALIDATE KEY TYPE                            
         BNE   EIIF                                                             
         CLI   1(R7),0                                                          
         BNE   EIIF                                                             
         LA    R5,KEYLST           MATCH INPUT WITH LIST                        
         SPACE 1                                                                
KEYV2    CLI   0(R5),X'FF'                                                      
         BE    EIIF                                                             
         CLC   0(1,R5),12(R7)                                                   
         BE    *+12                                                             
         LA    R5,L'KEYLST(R5)                                                  
         B     KEYV2                                                            
         CLC   THISTYPE,12(R7)                                                  
         BNE   EIIF                                                             
         MVC   CTCKTYP,1(R5)       MOVE BOOK TYPE TO KEY                        
         MVI   FNDX,2                                                           
         LA    R7,32(R7)           BUMP TO NAME INPUT                           
         CLI   1(R7),0                                                          
         BNE   EIIF                                                             
         TM    2(R5),X'80'         CHECK DDS ONLY BOOK TYPE                     
         BZ    *+12                                                             
         CLI   DDS,0                                                            
         BE    EIIF                                                             
         CLC   0(1,R7),3(R5)       CHECK L'NAME                                 
         BL    EFTS                                                             
         CLC   0(1,R7),4(R5)                                                    
         BH    EFTL                                                             
         MVC   CTCKID,12(R7)       MOVE BOOKNAME TO KEY                         
         MVC   FUNCTION,5(R5)      SAVE KEY FUNCTIONS                           
         CLI   FUNCTION,0          ANY MORE KEY FIELDS                          
         BE    KEYV10                                                           
         TM    FUNCTION,X'01'      SPECIAL FOR TWX TEXT BOOKS                   
         BZ    KEYV4                                                            
         L     RE,AUTL                                                          
         USING UTLD,RE                                                          
         MVC   CTCKUSER,TUSER                                                   
         MVI   CTCKSYS,C'T'                                                     
         MVI   FUNCTION,0                                                       
         B     KEYV10                                                           
         DROP  RE                                                               
         EJECT                                                                  
KEYV4    LA    R7,32(R7)           POINT TO NEXT INPUT LINE                     
         SR    R6,R6                                                            
         IC    R6,FNDX                                                          
         LA    R6,1(R6)            UPDATE FNDX #                                
         STC   R6,FNDX                                                          
         CLC   FNDX,NLINES                                                      
         BH    KEYV8               FINISHED                                     
         CLI   0(R7),1             CHECK LENGTH OF SUB-FIELD ID                 
         BL    EFTS                                                             
         CLI   0(R7),8                                                          
         BH    EFTL                                                             
         CLI   1(R7),0             BOTH MUST BE PRESENT                         
         BE    EIIF                                                             
         LA    R5,SKEYLST                                                       
         SR    R6,R6                                                            
         IC    R6,0(R7)                                                         
         SH    R6,=H'1'                                                         
         SPACE 1                                                                
KEYV6    CLI   0(R5),X'FF'                                                      
         BE    EIIF                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   4(0,R5),12(R7)      MATCH INPUT WITH TABLE                       
         BE    *+12                                                             
         LA    R5,L'SKEYLST(R5)                                                 
         B     KEYV6                                                            
         MVC   WORK(1),FUNCTION                                                 
         NC    WORK(1),0(R5)       IS THIS INPUT A KEY FUNCTION FOR             
         BZ    EIIF                THIS RECORD TYPE                             
         L     RF,0(R5)                                                         
         A     RF,RELO             RELOCATE ADCON                               
         BASR  RE,RF                                                            
         CLI   WORK,0                                                           
         BNE   EIIF                                                             
         B     KEYV4                                                            
         SPACE 1                                                                
KEYV8    CLI   FUNCTION,0          ALL KEY FIELDS MUST BE INPUT                 
         BE    KEYV10                                                           
         B     EMIF                SET FERN TO MISSING INPUT FIELD              
         SPACE 1                                                                
KEYU     ST    RE,DUB                                                           
         ST    R5,DUB+4                                                         
         CLI   1(R7),0             L'SECOND FIELD                               
         BE    EMIF                                                             
         CLI   1(R7),3                                                          
         BNE   KEYU1                                                            
         CLC   22(3,R7),=C'ALL'    CHECK FOR USER=ALL                           
         BNE   KEYU1                                                            
         B     KEYU3                                                            
KEYU1    LA    R5,IOAREA2                                                       
         USING CTIREC,R5           BUILD AN ID KEY                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIREC,C'I'                                                      
         MVC   CTIKID,22(R7)                                                    
         MVC   KEY,CTIKEY                                                       
         ST    R5,AREC                                                          
         GOTO1 AREAD               READ ID RECORD                               
         BZ    EIIO                RECORD MUST BE PRESENT & CORRECT             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         ST    R4,AREC             RESTORE A(IO)                                
         LA    R5,CTIDATA                                                       
         DROP  R5                                                               
         SR    R6,R6                                                            
KEYU2    CLI   0(R5),0             LOOK FOR ID# ELEMENT                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'02'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     KEYU2                                                            
         MVC   CTCKUSER,2(R5)                                                   
KEYU3    L     RE,DUB                                                           
         L     R5,DUB+4                                                         
         XC    FUNCTION,0(R5)      TURN OFF BIT                                 
         MVI   WORK,0                                                           
         BR    RE                                                               
         SPACE 1                                                                
KEYS     CLI   1(R7),0             VALIDATE SYSTEM=                             
         BER   RE                                                               
         CLI   1(R7),7                                                          
         BH    EFTL                                                             
         LA    R6,SYSLST                                                        
         SR    R1,R1                                                            
         IC    R1,1(R7)                                                         
         BCTR  R1,0                                                             
         SPACE 1                                                                
KEYS2    CLI   0(R6),X'FF'                                                      
         BER   RE                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R6),22(R7)                                                   
         BE    *+12                                                             
         LA    R6,L'SYSLST(R6)                                                  
         B     KEYS2                                                            
         MVC   CTCKSYS,0(R6)                                                    
         XC    FUNCTION,0(R5)                                                   
         MVI   WORK,0                                                           
         BR    RE                                                               
         SPACE 1                                                                
KEYC     CLI   1(R7),0                                                          
         BE    EMIF                                                             
         CLI   1(R7),2                                                          
         BL    EFTS                                                             
         CLI   1(R7),5                                                          
         BH    EFTL                                                             
         CLC   22(3,R7),=C'ALL'                                                 
         BE    *+10                                                             
         MVC   CTCKID+3(5),22(R7)                                               
         XC    FUNCTION,0(R5)                                                   
         MVI   WORK,0                                                           
         BR    RE                                                               
         SPACE 1                                                                
KEYV10   MVC   KEY,CTCKEY                                                       
         CLI   READKEY,C'Y'                                                     
         BNE   KEYV12                                                           
         LA    R5,IOAREA2                                                       
         ST    R5,AREC                                                          
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         MVC   SAVEDMCB,DMCB+8                                                  
         ST    R4,AREC                                                          
         SPACE 1                                                                
KEYV12   DS    0H                                                               
         MVI   FERN,X'FF'                                                       
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
*              TABLE OF VALID RECORD TYPES                                      
*                                                                               
*              BYTE 0 - RECORD TYPE                                             
*                   1 - MIN NAME LENGTH                                         
*                   2 - MAX NAME LENGTH                                         
*                   3 - VALID KEY FIELDS     X'80'=USER                         
*                                            X'40'=SYSTEM                       
*                                            X'20'=CLIENT                       
*                                                                               
KEYLST   DS    0CL6                                                             
         DC    C'CC',X'80',AL1(02,03),X'E0'                                     
         DC    C'JJ',X'80',AL1(01,10),X'00'                                     
         DC    C'LL',X'80',AL1(01,10),X'00'                                     
         DC    C'TC',X'00',AL1(01,10),X'01'                                     
         DC    C'FF',X'80',AL1(01,10),X'00'                                     
         DC    X'FF'                                                            
*                                                                               
*              TABLE OF VALID SUB-KEY FIELD TYPES                               
*                                                                               
*              BYTE 0    - OR BYTE                                              
*                   1-3  - A (ROUTINE)                                          
*                   4-11 - NAME                                                 
         DS    0F                                                               
SKEYLST  DS    0CL12                                                            
         DC    X'80',AL3(KEYU),CL8'USER='                                       
         DC    X'40',AL3(KEYS),CL8'SYSTEM='                                     
         DC    X'20',AL3(KEYC),CL8'CLIENT='                                     
         DC    X'FF'                                                            
*                                                                               
*              TABLE OF VALID SYSTEMS                                           
*                                                                               
SYSLST   DS    0CL9                                                             
         DC    C'S',CL8'SPOT'                                                   
         DC    C'A',CL8'ACCOUNT'                                                
         DC    C'P',CL8'PRINT'                                                  
         DC    C'R',CL8'REP'                                                    
         DC    C'M',CL8'MEDIA'                                                  
         DC    C'C',CL8'CONTROL'                                                
         DC    C'D',CL8'CPP'                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*              LITERALS                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER EXTRA WORK AREA                                   
*                                                                               
WRKD     DSECT                                                                  
SEQALL   DS    0CL3                                                             
SEQLO    DS    CL1                                                              
SEQHI    DS    CL1                                                              
SEQIN    DS    CL1                                                              
*                                                                               
THISTYPE DS    CL1                                                              
BKHIVAL  DS    C                                                                
NLINES   DS    CL1                                                              
LINES    DS    6CL32                                                            
*                                                                               
CIO      DS    CL80                                                             
LINCNT   DS    CL1                                                              
FUNCTION DS    CL1                                                              
BOOKSTAT DS    CL1                                                              
DISPTYPE DS    CL1                                                              
SAVEDMCB DS    C                                                                
READKEY  DS    C                                                                
ADDLST   DS    CL16                                                             
*                                                                               
AGETBOOK DS    V                                                                
*                                                                               
INDHI    DS    CL1                                                              
INDACT   DS    CL256               ACTIVE INDEX ENTRIES                         
         DS    CL12                                                             
INDPAS   DS    CL256               PASSIVE INDEX ENTRIES                        
         DS    CL12                                                             
GETAREA  DS    1200C                                                            
IOAREA2  DS    1000C                                                            
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
LASTLO   DS    CL1                                                              
LASTHI   DS    CL1                                                              
LASTST   DS    CL1                                                              
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMFBD                                                                      
       ++INCLUDE CTLFMFBD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019CTLFM04   05/01/02'                                      
         END                                                                    
