*          DATA SET ACLFM32    AT LEVEL 043 AS OF 05/01/02                      
*PHASE T60332A,+0                                                               
         TITLE 'INTEREST RATES BY DATE'                                         
T60332   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**LFM32*,RR=R6,CLEAR=YES                               
         LR    R8,RC                                                            
         USING LWSD,R8                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         ST    R6,PRELO                                                         
         MVI   ERROR,X'FF'                                                      
*                                                                               
*-----------------------------------------------------------------*             
*              CHECK CONTROL MODES                                              
*-----------------------------------------------------------------*             
MODE1    CLI   MODE,BUILDKEY                                                    
         BNE   MODE2                                                            
         BAS   RE,BLDK100                                                       
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,DSPLYREC                                                    
         BNE   MODE3                                                            
         BAS   RE,DSPL100                                                       
         B     XIT                                                              
*                                                                               
MODE3    CLI   MODE,BUILDREC                                                    
         BNE   XIT                                                              
         BAS   RE,BLDR100                                                       
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------------------*             
*              BUILD RATE KEY (MODE=BUILDKEY)                                   
*-----------------------------------------------------------------*             
BLDK100  NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(2),RECSUBR           REC, SUB REC CODES                      
         MVC   KEY+2(1),COMPANY         COMPANY CODE                            
*                                                                               
         CLI   LOGACT,C'N'              IF ACTION IS NEW CHECK TO               
         BNE   BLDK200                  SEE IF REC ALREADY EXISTS               
         XC    IO2(42),IO2              IF YES PUT OUT ERROR MSG                
         XC    IO(49),IO                AND EXIT                                
         MVC   IO(L'KEY),KEY                                                    
         OI    RECOREL,FULLKEY                                                  
         GOTO1 HIGH                                                             
         NI    RECOREL,X'FF'-FULLKEY                                            
         CLC   KEYSAVE,KEY                                                      
         BNE   BLDK200                                                          
         MVI   ERROR,RECONFLE           RECORD ON FILE                          
         B     XIT                                                              
*                                                                               
BLDK200  MVI   ANYKEY,C'N'              ONLY POSSIBLE KEY FIELD                 
         TM    INTFLTDH+4,X'80'         CHANGE IS FILTER DATE                   
         BZ    XIT                                                              
         MVI   ANYKEY,C'Y'                                                      
         XC    FILTDAT,FILTDAT                                                  
BLDKXIT  DS    0H                                                               
*        BAS   RE,DSPL100                                                       
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              DISPLAY RATE RECORD (MODE=DSPLYREC)                              
*-------------------------------------------------------------------*           
DSPL100  NTR1                                                                   
         TWAXC INTDAT2H                 CLEAR SCREEN                            
*                                                                               
DSPL120  XC    FILTDAT,FILTDAT                                                  
         LA    R2,INTFLTDH              VALIDATE FILTER DATE FIELD              
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   WORK(3),=C'ALL'          DISPLAY ALL ELEMENTS                    
         BE    DSPL305                                                          
         CLC   WORK(3),SPACES           MUST ENTER SOMETHING                    
         BH    DSPL150                                                          
         MVC   LOGHEAD(19),=C'MISSING INPUT FIELD'                              
         MVI   ERROR,X'FE'                                                      
         OI    INTFLTDH+6,X'40'         POSITION CURSOR                         
         B     XIT                                                              
*                                                                               
DSPL150  DS    0H                       VALIDATE DATE ENTRY                     
         GOTO1 DATVAL,DMCB,(0,INTFLTD),CHARDAT                                  
         OC    0(4,R1),0(R1)                                                    
         BNZ   DSPL180                                                          
         MVI   ERROR,DATERR                                                     
         OI    INTFLTDH+6,X'40'         POSITION CURSOR                         
         B     XIT                                                              
*                                       CONVERT DATE TO COMPRESSED              
DSPL180  GOTO1 DATCON,DMCB,(0,CHARDAT),(2,FILTDAT)                              
*                                                                               
         USING ELEMTABD,R3                                                      
DSPL305  LA    R3,ELEMTAB1              CLEAR ELEMENT TABLE                     
         LA    R6,ELT1NTRS                                                      
DSPL310  XC    0(L'ELEMENTR,R3),0(R3)                                           
         LA    R3,L'ELEMENTR(R3)                                                
         BCT   R6,DSPL310                                                       
*                                                                               
         MVC   KEY,SPACES               BUILD KEY AND READ REC INTO             
         MVC   KEY(2),RECSUBR           IO                                      
         MVC   KEY+2(1),COMPANY                                                 
         GOTO1 READ                                                             
*                                                                               
         USING ACRATED,R4                                                       
         LA    R4,IO                    ADDRESS OF RECORD                       
         AH    R4,DATADISP              POINT PAST KEY TO ELEMENTS              
         LA    R3,ELEMTAB1                                                      
         MVI   0(R3),X'FF'              MARK END OF ELEMENT TABLE               
DSPL320  DS    0H                                                               
         CLI   0(R4),X'00'              ARE THERE ANY RATE ELEMENTS             
         BE    DSPL330                                                          
         CLI   0(R4),ACRIELQ            IS IT RATE ELEMENT                      
         BNE   DSPL322                                                          
*                                                                               
**** TOOK OUT THE FOLLOWING CODE BECAUSE =NFILE ADDS ADDITIONAL                 
**** ELEMENTS THAT THIS PROGRAM WAS NOT EXPECTING                               
*        CLI   0(R4),ACRIELQ            IS IT RATE ELEMENT                      
*        BE    *+6                                                              
*        DC    H'0'                                                             
****                                                                            
*                                                                               
         MVC   ELEMDAT,ACRIDAT          ELEMENT DATE TO ELEMTAB                 
         MVC   ELEMRAT,ACRIRAT          ELEMENT RATE TO ELEMTAB                 
         MVI   ELEMUSE,C'N'                                                     
         LA    R3,L'ELEMENTR(R3)        INCREMENT TABLE                         
DSPL322  ZIC   R5,ACRILEN               ELEMENT LENGTH                          
         AR    R4,R5                    BUMP RECORD                             
         B     DSPL320                                                          
DSPL330  MVI   0(R3),X'FF'              MARK NEW END OF TABLE                   
*                                                                               
         LA    R3,ELEMTAB1              PASS ADDR OF TAB TO BE SORTED           
         BAS   RE,SORTTAB               SORT TABLE                              
*                                                                               
         USING ELEMTABD,R3                                                      
         LA    R3,ELEMTAB1              CLEAR FIRST ELEMENT TABLE               
         USING MYRATED,R2                                                       
         LA    R2,INTDAT1H              POINT TO FIELD ON SCREEN                
         LA    R2,L'MYSCRENT(R2)        BUMP SCREEN DISPLAY                     
         ZIC   R6,SCENTRS               MAX NUM OF ELEMS PER SCREEN             
         BCTR  R6,0                     SKIP ONE BLANK SPACE                    
DSPL700  CLI   0(R3),X'FF'              IS IT END OF RECORD                     
         BE    DSPL900                                                          
         OC    FILTDAT,FILTDAT                                                  
         BZ    DSPL720                                                          
         CLC   FILTDAT,ELEMDAT                                                  
         BL    DSPL770                                                          
DSPL720  GOTO1 DATCON,DMCB,(2,ELEMDAT),(5,MYDATD)   DATE TO SCREEN              
         OI    MYDATHD+6,X'80'          TRANSMIT DATE                           
         EDIT  ELEMRAT,(7,MYRATD),4     RATE TO SCREEN                          
         OI    MYRATHD+6,X'80'          TRANSMIT RATE                           
         MVI   ELEMUSE,C'Y'             MARK AS USED ON SCREEN                  
DSPL750  LA    R2,L'MYSCRENT(R2)        BUMP SCREEN DISPLAY                     
DSPL770  LA    R3,L'ELEMENTR(R3)        BUMP ELEMENT TABLE                      
         BCT   R6,DSPL700                                                       
*                                                                               
DSPL900  MVI   ERROR,X'FF'                                                      
         MVI   ANYKEY,C'N'              RESET KEY CHANGE                        
         OI    LOGRECH+6,X'40'          FOR INQUIRY POSITION CURSOR             
         CLI   LOGACT,C'I'              TO RECORD FIELD                         
         BE    XIT                      FOR AMEND OR NEW POSITION TO            
         CLI   LOGACT,C'E'              FIRST INPUT FIELD                       
         BE    XIT                                                              
         LA    R2,INTDAT1H                                                      
         OI    INTDAT1H+6,X'40'         POSITION CURSOR                         
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        BUILD NEW REC, AMEND EXISTING REC (MODE=BUILDREC)                      
*                                                                               
*-------------------------------------------------------------------*           
BLDR100  NTR1                           STORE ELEMS FOR SORTING                 
         LA    R3,ELEMTAB2              CLEAR SECOND ELEMENT TABLE              
         LA    R6,ELT2NTRS                                                      
BLDR120  XC    0(L'ELEMENTR,R3),0(R3)                                           
         LA    R3,L'ELEMENTR(R3)                                                
         BCT   R6,BLDR120                                                       
*                                                                               
BLDR190  LA    R3,ELEMTAB2              INITIALIZE TABLE                        
         MVI   0(R3),X'FF'                                                      
         CLI   LOGACT,C'N'              IS ACTION 'NEW'                         
         BE    BLDR310                                                          
*                                                                               
         MVC   KEY,SPACES               BUILD KEY AND READ REC INTO             
         MVC   KEY(2),RECSUBR           IO                                      
         MVC   KEY+2(1),COMPANY                                                 
         GOTO1 READ                                                             
         BAS   RE,REMOVAL               REMOVE ALL 6E ELEMS                     
*                                                                               
         USING MYRATED,R2                                                       
BLDR310  LA    R2,INTDAT1H              PROCESS DATE                            
         CLI   LOGACT,C'N'                                                      
         BNE   BLDR315                                                          
         CLC   INTDAT1,SPACES           INSURE AT LEAST ONE ENTRY ON            
         BNH   BLDR325                  SCREEN FOR RECORD ADD                   
BLDR315  DS    0H                                                               
         ZIC   R6,SCENTRS               MAX NUM OF ENTRIES ON SCREEN            
BLDR320  DS    0H                                                               
         XC    COMPDAT,COMPDAT                                                  
         CLC   MYDATD,SPACES            VALIDATE ANY DATE CHANGES               
         BNH   BLDR350                                                          
         GOTO1 DATVAL,DMCB,(0,MYDATD),CHARDAT                                   
         OC    0(4,R1),0(R1)                                                    
         BNZ   BLDR330                                                          
BLDR325  MVI   ERROR,DATERR             SET DATE ERROR                          
         OI    MYDATHD+6,X'40'          POSITION CURSOR                         
         B     XIT                                                              
BLDR330  GOTO1 DATCON,DMCB,(0,CHARDAT),(2,COMPDAT)                              
*                                                                               
BLDR350  DS    0H                       PROCESS RATE                            
         CLC   MYRATD(3),=C'DEL'        INDICATES A DELETE                      
         BE    BLDR375                  SKIP ADDING ELEMENT TO TABLE            
         CLC   MYRATD,SPACES            IF BOTH SPACES ARE BLANK                
         BH    BLDR360                  SKIP THIS PLACE ON THE SCREEN           
         OC    COMPDAT,COMPDAT                                                  
         BZ    BLDR375                                                          
BLDR360  ZIC   R7,MYRATHD+5                                                     
         GOTO1 CASHVAL,DMCB,(4,MYRATD),(R7)   VALIDATE ANY RATE CHANGE          
         CLI   DMCB,X'FF'                                                       
         BNE   BLDR370                                                          
         MVI   ERROR,NOTNUMRC           SET ERROR MESSAGE                       
         OI    MYRATHD+6,X'40'          POSITION CURSOR                         
         B     XIT                                                              
BLDR370  MVC   BINRATE,DMCB+5           TEMPORARILY STORE RATE                  
         OC    COMPDAT,COMPDAT                                                  
         BNZ   BLDR372                                                          
         OI    MYDATHD+6,X'40'                                                  
         MVI   ERROR,DATERR                                                     
         B     XIT                                                              
*                                                                               
BLDR372  LA    R3,ELEMTAB2              SET ADDR OF TABLE TO ADD TO             
         ST    R3,TABADDR                                                       
         BAS   RE,ADDTOTAB              ADD ELEMENT TO TABLE                    
BLDR375  LA    R2,L'MYSCRENT(R2)        BUMP FIELD ON SCREEN                    
         BCT   R6,BLDR320               PROCESS NEXT SCREEN ENTRY               
*                                                                               
         CLI   LOGACT,C'N'              IF ACTION IS NEW ADD A                  
         BNE   BLDR400                  NEW RECORD                              
         BAS   RE,ADNEWREC                                                      
         B     BLDR900                  ELSE                                    
*                                                                               
BLDR400  BAS   RE,UPDATREC              UPDATE OLD RECORD                       
BLDR900  DS    0H                                                               
         MVI   ERROR,X'FF'              RESET TO NO ERROR                       
         OI    LOGRECH+6,X'40'          POSITION CURSOR                         
         MVI   ANYKEY,C'N'              RESET ANY KEY CHANGES                   
BLDRXIT  DS    0H                                                               
         MVC   INTDAT1,SPACES                                                   
         OI    INTDAT1H+6,X'80'         TRANSMIT RATE                           
         MVC   INTRAT1,SPACES                                                   
         OI    INTRAT1H+6,X'80'         TRANSMIT RATE                           
         BAS   RE,DSPL100               AFTER RECORD ADD OR AMEND               
         B     XIT                      REDISPLAY SCREEN                        
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        REMOVE ALL RATE ELEMENTS FROM RECORD                                   
*-------------------------------------------------------------------*           
REMOVAL  NTR1                                                                   
         LA    R2,IO2                   RECORD MUST BE IN IO2 FOR               
         LA    R3,IOLENQ                ELEMENT REMOVAL                         
         LA    R4,IO                                                            
         SR    R5,R5                                                            
         ICM   R5,3,IO+42                                                       
         MVCL  R2,R4                                                            
*                                                                               
         USING ACRATED,R4                                                       
         LA    R4,IO2                   STRIP ALL 6E ELEMENTS                   
         AH    R4,DATADISP              FROM RECORD                             
         ZIC   R5,ACRILEN                                                       
REM120   CLI   0(R4),0                                                          
         BE    XIT                                                              
REM150   GOTO1 REMANEL,DMCB,(X'6E',0)                                           
REM200   AR    R4,R5                                                            
         B     REM120                                                           
*-------------------------------------------------------------------*           
*        SORT ALL TABLE ENTRIES                                                 
*-------------------------------------------------------------------*           
SORTTAB  NTR1                                                                   
         SR    R7,R7                    ADDRESS IN TABLE TO BE SORTED           
         LR    R5,R3                    IS PASSED IN R3                         
SRT150   CLI   1(R5),X'DD'              COUNT NUMBER OF ENTRIES IN              
         BE    SRT200                   TABLE - AVOID GOING TO SORT             
         CLI   0(R5),X'FF'              WITH ONLY ONE TABLE ENTRY               
         BE    SRT200                                                           
         LA    R5,L'ELEMENTR(R5)                                                
         LA    R7,1(R7)                                                         
         B     SRT150                                                           
*                                                                               
SRT200   CH    R7,=H'1'                 NO SORT UNLESS MORE THAN                
         BNH   XIT                      ONE ENTRY                               
         GOTO1 CALLOV,DMCB,0,X'D900A12'    GET ADDR OF XSORT                    
         L     RF,DMCB                  ADDR OF XSORT                           
         LA    R6,L'ELEMENTR                                                    
         GOTO1 (RF),DMCB,(X'FF',(R3)),(R7),(R6),2,0                             
         B     XIT                                                              
*-------------------------------------------------------------------*           
*        ADD NEW ELEMNT TO TABLE                                                
*-------------------------------------------------------------------*           
ADDTOTAB NTR1                                                                   
         USING ELEMTABD,R3                                                      
         L     R3,TABADDR                                                       
ADDT100  CLI   1(R3),X'DD'              MAX ELEMENTS REACHED                    
         BE    ADDT900                                                          
         CLI   0(R3),X'FF'                                                      
         BE    ADDT200                                                          
         CLC   ELEMDAT,COMPDAT                                                  
         BNE   ADDT150                                                          
         MVC   ELEMDAT,COMPDAT          COMPRESSED DATE TO ELEMTAB              
         MVC   ELEMRAT,BINRATE          RATE TO ELEMENT TABLE                   
         B     ADDT900                                                          
ADDT150  LA    R3,L'ELEMENTR(R3)                                                
         B     ADDT100                                                          
*                                                                               
ADDT200  MVC   ELEMDAT,COMPDAT          COMPRESSED DATE TO ELEMTAB              
         MVC   ELEMRAT,BINRATE          RATE TO ELEMENT TABLE                   
         LA    R3,L'ELEMENTR(R3)                                                
         MVI   0(R3),X'FF'              MARK NEW END OF TABLE                   
ADDT900  B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        UPDATE RECORD WITH ELEMENTS FROM TABLE                                 
*-------------------------------------------------------------------*           
UPDATREC NTR1                                                                   
         USING ACRATED,R4                                                       
         MVC   IO2(42),IO                                                       
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         USING ELEMTABD,R3                                                      
         LA    R3,ELEMTAB2                                                      
UPD100   CLI   0(R3),X'FF'              END OF ELEMENTS IN TABLE                
         BE    UPD300                                                           
         CLI   1(R3),X'DD'              MAX AMOUNT OF ELEMS REACHED             
         BE    UPD300                                                           
         XC    ELEMENT,ELEMENT          BUILD ELEMENT                           
         MVI   ELEMENT,ACRIELQ                                                  
         MVI   ELEMENT+1,ACRILEQ                                                
         MVC   ELEMENT+2(5),ELEMDAT                                             
         GOTO1 ADDANEL                  ADD ELEMENT TO REC                      
         LA    R3,L'ELEMENTR(R3)                                                
         B     UPD100                                                           
*                                                                               
UPD300   DS    0H                                                               
         LA    R3,ELEMTAB1                                                      
UPD320   CLI   0(R3),X'FF'              END OF ELEMENTS IN TABLE                
         BE    UPD500                                                           
         CLI   1(R3),X'DD'              MAX AMOUNT OF ELEMS REACHED             
         BE    UPD500                                                           
         CLI   ELEMUSE,C'N'                                                     
         BNE   UPD380                                                           
         XC    ELEMENT,ELEMENT          BUILD ELEMENT                           
         MVI   ELEMENT,ACRIELQ                                                  
         MVI   ELEMENT+1,ACRILEQ                                                
         MVC   ELEMENT+2(5),0(R3)                                               
         GOTO1 ADDANEL                  ADD ELEMENT TO REC                      
UPD380   LA    R3,L'ELEMENTR(R3)                                                
         B     UPD320                                                           
*                                                                               
UPD500   DS    0H                                                               
         LA    R2,IO2                                                           
         USING ACKEYD,R2                                                        
         CLI   ACRECORD,0              IF NO ELEMENTS MARK RECORD               
         BNE   *+8                     DELETED AND WRITE BACK                   
         OI    ACSTATUS,X'80'                                                   
         GOTO1 PUTREC                                                           
         B     XIT                                                              
         DROP  R2                                                               
*-------------------------------------------------------------------*           
*        ADD NEW RATE RECORD TO FILE                                            
*-------------------------------------------------------------------*           
ADNEWREC NTR1                                                                   
         USING ACRATED,R4                                                       
         LA    R4,IO2                                                           
         XC    0(49,R4),0(R4)                                                   
         MVC   0(42,R4),SPACES                                                  
         MVC   0(2,R4),RECSUBR                                                  
         MVC   2(1,R4),COMPANY                                                  
         AH    R4,DATADISP                                                      
         USING ELEMTABD,R3                                                      
         LA    R3,ELEMTAB2                                                      
ADN100   CLI   0(R3),X'FF'              END OF ELEMENTS IN TABLE                
         BE    ADN300                                                           
         CLI   1(R3),X'DD'              MAX AMOUNT OF ELEMS REACHED             
         BE    ADN300                                                           
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,ACRIELQ                                                  
         MVI   ELEMENT+1,ACRILEQ                                                
         MVC   ELEMENT+2(5),0(R3)                                               
         GOTO1 ADDANEL                                                          
         LA    R3,L'ELEMENTR(R3)                                                
         B     ADN100                                                           
ADN300   GOTO1 ADDREC                                                           
         OI    LOGRECH+6,X'40'          POSITION CURSOR                         
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*           ** ERROR MESSAGES **                                                
*-------------------------------------------------------------------*           
INVERR   DS    0H                                                               
         MVI   ERROR,INVALID                                                    
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        LITERALS                                                               
*-------------------------------------------------------------------*           
RECSUBR  DC    XL2'2D05'           REC CODE - SUB REC CODE                      
SCENTRS  DC    AL1(52)             MAX NUMBER OF ENTRIES ON SCREEN              
FULLKEY  EQU   X'08'               USED TO ALTER RECOREL (FROM BASE)            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
LWSD     DSECT                                                                  
CHARDAT  DS    CL6                 USE FOR DATE VALIDATION                      
COMPDAT  DS    CL2                 STORE COMPRESSED ELEMENT DATE                
FILTDAT  DS    CL2                 STORE COMPRESSED FILTER DATE                 
BINRATE  DS    CL3                 STORE BINARY RATE                            
PRELO    DS    F                   RELOCATION FACTOR                            
TABADDR  DS    F                   ADDR OF TABLE TO ADD ENTRY TO                
*                                                                               
ELEMTAB1 DS    0H                  ELEMENT TABLE ONE                            
         DS    51XL6               A FLOATING 'FF' WILL MARK                    
         DS    84XL6               THEORETICAL TABLE END -                      
TAB1END  DC    XL2'DDDD'           'DD' ABSOLUTE TABLE END                      
ELT1NTRS EQU   (TAB1END-ELEMTAB1)/L'ELEMENTR                                    
*                                                                               
ELEMTAB2 DS    0H                  ELEMENT TABLE ONE                            
         DS    51XL6               A FLOATING 'FF' WILL MARK                    
         DS    84XL6               THEORETICAL TABLE END -                      
TAB2END  DC    XL2'DDDD'           'DD' ABSOLUTE TABLE END                      
ELT2NTRS EQU   (TAB2END-ELEMTAB2)/L'ELEMENTR                                    
LWSX     EQU   *                                                                
*                                                                               
ELEMTABD DSECT                     COVER ELEMENT TABLES                         
ELEMENTR DS    0CL6                LENGTH OF ONE ENTRY                          
ELEMDAT  DS    XL2                 DATE                                         
ELEMRAT  DS    XL3                 RATE                                         
ELEMUSE  DS    CL1                 WAS ELEMENT DISPLAYED ON SCREEN              
*                                                                               
*        DSECT TO COVER (SCREEN) DATE-RATE                                      
MYRATED  DSECT                                                                  
MYSCRENT DS    0CL31          LEN OF ONE SCREEN ENTRY (DATE + RATE)             
MYDATENT DS    0CL16          LEN OF ONE DATE ENTRY (DATE AND HEADER)           
MYDATHD  DS    CL8                   DATE HEADER                                
MYDATD   DS    CL8                   DATE DATA                                  
MYRATENT DS    0CL15          LEN OF ONE RATE ENTRY (RATE AND HEADER)           
MYRATHD  DS    CL8                   RATE HEADER                                
MYRATD   DS    CL7                   RATE DATA                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMD2D                                                       
*                                                                               
*        ACLFMWORK                                                              
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACLFMEQU                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043ACLFM32   05/01/02'                                      
         END                                                                    
