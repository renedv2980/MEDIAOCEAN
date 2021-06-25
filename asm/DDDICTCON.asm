*          DATA SET DDDICTCON  AT LEVEL 030 AS OF 05/01/02                      
*PHASE T00A31A                                                                  
         TITLE 'T00A31 - DATA DICTIONARY CONTROLLER'                            
DICTCON  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DICT**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    RB,DATADRB                                                       
         ST    RD,DATADRD                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING DICTCON+4096,R9                                                  
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,VALIDATA                                                      
         LA    R5,VCOUNT                                                        
         SPACE 1                                                                
DIN2     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,DIN2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ENTERABLE ROUTINES                                               
         SPACE 3                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=DATADRB                                                     
         ST    RD,DATADRD                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         L     R8,ASPOOLD          CALLER MAY NOT HAVE SET R8                   
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VVALDATA                                                         
         B     VDSPDATA                                                         
         B     VVALFILT                                                         
         B     VEXCFILT                                                         
         B     VVALLIST                                                         
         B     VDIGDATA                                                         
         B     VEDTDATA                                                         
         B     VDIGHEAD                                                         
         SPACE 2                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
*              DATA VALIDATION CONTROL                                          
         SPACE 3                                                                
VVALDATA CLI   MODE,DISPREC        SWITCH TO DISPLAY IF MODE SET                
         BE    VDSPDATA                                                         
         CLI   MODE,DISPKEY                                                     
         BE    VDSPDATA                                                         
         BAS   RE,FIND                                                          
         USING DICEL,R3                                                         
         BAS   RE,BUMP                                                          
         XC    WORK,WORK                                                        
         BAS   RE,VAL              GO AND VALIDATE INTO WORK                    
         LA    R4,KEY                                                           
         MVC   ELCODE,DICWHERE     SEE IF ELEMENT IS THERE YET                  
         CLI   ELCODE,0                                                         
         BE    VVAL2                                                            
         GOTO1 REMELEM                                                          
         LA    R4,ELEMENT                                                       
         SPACE 1                                                                
VVAL2    ZIC   R1,DICDISP          PICK UP DISPLACEMENT                         
         AR    R4,R1                                                            
         IC    R1,DICLEN                   AND LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),WORK        PUT IN DATA                                  
         CLI   ELCODE,0                                                         
         BE    XIT                                                              
         OC    ELEMENT+2(254),ELEMENT+2                                         
         BZ    XIT                 NO SIGNIFICANT DATA                          
         MVC   ELEMENT(1),DICWHERE                                              
         ZIC   R1,DICLEN           ELEMENT LENGTH IS AT LEAST LENGTH            
         ZIC   R0,DICDISP          PLUS DISPLACEMENT                            
         AR    R1,R0                                                            
         IC    R0,ELEMENT+1        UNLESS LENGTH IS BIGGER ALREADY              
         CR    R0,R1                                                            
         BH    *+8                                                              
         STC   R1,ELEMENT+1                                                     
         GOTO1 ADDELEM             RETURN TO RECORD                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO HANDLE VALIDATION                                    
         SPACE 3                                                                
VAL      NTR1                                                                   
         CLI   5(R2),0             ANY DATA THERE                               
         BNE   VAL2                                                             
         TM    DICFLAG,X'80'       IS FIELD COMPULSORY                          
         BNO   XIT                                                              
         SPACE 1                                                                
VAL2     GOTO1 ANY                                                              
         ZIC   RF,DICTYPE          SPLIT TYPE INTO ROUTINE (RF)                 
         SRL   RF,4                                                             
         SLL   RF,2                                                             
         IC    R1,DICTYPE                    AND PARAMETER (R1)                 
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         B     VALBRAN(RF)         GO TO SPECIALIZED ROUTINE                    
         SPACE 1                                                                
VALBRAN  B     VFREE                                                            
         B     VNUM                                                             
         B     VDEC                                                             
         B     VALID                                                            
         B     VYMD                                                             
         B     VYM                                                              
         B     VMD                                                              
         B     VPER                                                             
         B     VTIME                                                            
         B     VDAY                                                             
         SPACE 1                                                                
VALID    TM    DICFLAG,X'20'       ANY OK VALS AROUND                           
         BNO   XIT                                                              
         BAS   RE,OK                                                            
         B     XIT                                                              
         EJECT                                                                  
*              SPECIFIC VALIDATION ROUTINES - FREE FORM                         
         SPACE 3                                                                
VFREE    CH    R1,=H'4'            FREE FORM                                    
         BE    VALID                                                            
         BH    VHEX                                                             
         TM    4(R2),X'04'         ALPHABETIC                                   
         BO    VALID                                                            
         MVI   ERROR,NOTALPHA                                                   
         B     TRAPERR                                                          
         SPACE 1                                                                
VHEX     MVI   ERROR,NOTHEX        HEX                                          
         TM    4(R2),X'02'                                                      
         BNO   TRAPERR                                                          
         SR    R5,R5                                                            
         IC    R5,FHILD(R2)                                                     
         XC    WORK,WORK                                                        
         GOTO1 HEXIN,DMCB,FHDAD(R2),WORK,(R5)                                   
         B     VALID                                                            
         EJECT                                                                  
*              SPECIFIC VALIDATION ROUTINES - NUMERIC & DECIMAL                 
         SPACE 3                                                                
VNUM     MVI   ERROR,NOTNUM        NUMERIC                                      
         TM    4(R2),X'08'                                                      
         BNO   TRAPERR                                                          
         CLI   DICTYPE,X'12'       EBCDIC                                       
         BE    VEBCDIC                                                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         ST    R1,WORK             BINARY                                       
         LA    R1,4                                                             
         CLI   DICTYPE,X'14'                                                    
         BE    VSHIFT                                                           
         MVC   WORK(8),DUB         PACKED                                       
         LA    R1,8                                                             
         CLI   DICTYPE,X'16'                                                    
         BE    VSHIFT                                                           
         LM    RE,RF,WORK          PWOS                                         
         SRDL  RE,4                                                             
         STM   RE,RF,WORK                                                       
         SPACE 1                                                                
VSHIFT   ZIC   R0,DICLEN           SHIFT TO LEFT                                
         SR    R1,R0                                                            
         BNP   VALID                                                            
         LA    R1,WORK(R1)                                                      
         MVC   WORK(16),0(R1)                                                   
         B     VALID                                                            
         SPACE 1                                                                
VEBCDIC  MVC   WORK(16),=16X'F0'                                                
         SR    R1,R1                                                            
         IC    R1,FHILD(R2)                                                     
         LA    R5,WORK+16                                                       
         SR    R5,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),FHDAD(R2)                                                
         LA    R1,16                                                            
         B     VSHIFT                                                           
         SPACE 1                                                                
VDEC     STC   R1,MAX              NUMBER OF DECIMALS                           
         GOTO1 VALIDEC                                                          
         MVC   WORK(4),FULL                                                     
         LA    R1,4                                                             
         B     VSHIFT                                                           
         EJECT                                                                  
*              SPECIFIC VALIDATION ROUTINES - DATES                             
         SPACE 3                                                                
VYMD     SR    R5,R5               YMD                                          
         B     VDATE                                                            
         SPACE 1                                                                
VYM      LA    R5,2                YM                                           
         B     VDATE                                                            
         SPACE 1                                                                
VMD      LA    R5,1                MD                                           
         SPACE 1                                                                
VDATE    LR    R4,R1               SAVE OUTPUT TYPE                             
         GOTO1 DATVAL,DMCB,((R5),FHDAD(R2)),DUB                                 
         MVI   ERROR,INVDATE                                                    
         CLI   DMCB+3,0                                                         
         BE    TRAPERR                                                          
         CH    R4,=H'4'            FOR IPL                                      
         BE    VALID               WE'RE DONE ALREADY                           
         GOTO1 DATCON,DMCB,(0,DUB),((R4),WORK)                                  
         CLI   DICTYPE,X'60'                                                    
         BL    VALID               OK FOR YMD AND YM                            
         MVC   WORK(6),WORK+1      MD NEEDS TO SHIFT TO LEFT                    
         LTR   R4,R4                                                            
         BNZ   VALID                                                            
         MVC   WORK(6),WORK+1      AND ONCE MORE FOR EBCDIC                     
         B     VALID                                                            
         SPACE 1                                                                
VPER     DS    0H                                                               
*&&US*&& DC    H'0'                DEIS SAYS U.S. DOESN'T USE THIS              
*                                   (SHOULD USE PERVAL ANYWAY)                  
         LR    R4,R1               PERIOD                                       
         GOTO1 VALIPER,DMCB,WORK                                                
         CLI   DICTYPE,X'70'       EBCDIC - DONE                                
         BE    VALID                                                            
         GOTO1 DATCON,DMCB,(0,WORK),((R4),DUB)                                  
         GOTO1 DATCON,DMCB,(0,WORK+6),((R4),DUB+3)                              
         XC    WORK,WORK                                                        
         CLI   DICTYPE,X'72'                                                    
         BE    VPER2                                                            
         MVC   WORK(6),DUB         BINARY & PWOS                                
         B     VALID                                                            
         SPACE 1                                                                
VPER2    MVC   WORK(2),DUB         COMPRESSED                                   
         MVC   WORK+2(2),DUB+3                                                  
         B     VALID                                                            
         EJECT                                                                  
*              SPECIFIC VALIDATION ROUTINES - ODDMENTS                          
         SPACE 3                                                                
VTIME    SR    R4,R4                                                            
         IC    R4,FHILD(R2)                      TIME - MILITARY                
         GOTO1 TIMVAL,DMCB,((R4),FHDAD(R2)),WORK                                
         MVI   ERROR,INVTIME                                                    
         CLI   DMCB,X'FF'                                                       
         BE    TRAPERR                                                          
         B     VALID                                                            
         SPACE 1                                                                
VDAY     SR    R4,R4                                                            
         IC    R4,FHILD(R2)                      DAY                            
         GOTO1 DAYVAL,DMCB,((R4),FHDAD(R2)),WORK,WORK+1                         
         MVI   ERROR,INVDAY                                                     
         CLI   WORK,0                                                           
         BE    TRAPERR                                                          
         SR    R4,R4                                                            
         LA    R1,REPDAYL                                                       
         SPACE 1                                                                
VDAY2    STC   R4,WORK+1           LOOK UP SPOT TO REP                          
         CLC   WORK(1),0(R1)                                                    
         BE    VALID                                                            
         CLI   0(R1),X'FF'                                                      
         BE    VALID                                                            
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         B     VDAY2                                                            
         SPACE 1                                                                
REPDAYL  DC    X'7C402010080402017FFF'                                          
         EJECT                                                                  
*              CHECK DATA AGAINST OK VALUES                                     
         SPACE 3                                                                
OK       NTR1                                                                   
         SPACE 1                                                                
         LR    R0,R3               SAVE A(KEY ELEMENT)                          
OK2      BAS   RE,DBUMP            FIND OK ELEMENT                              
         CLI   DICELCOD,X'05'                                                   
         BNE   OK2                                                              
         MVC   LISTAR,SPACES       MOVE OUT STRING INTO LISTAR                  
         ZIC   R1,DICELLEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTAR(0),DICVALS                                                
         SPACE 1                                                                
         LR    R3,R0                                                            
         MVC   DMCB+8(4),=C',=,-'                                               
         GOTO1 SCANNER,DMCB,(C'C',LISTAR),(15,BLOCK),                           
         MVI   ERROR,INVOK                                                      
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    TRAPERR                                                          
         LA    R4,BLOCK                                                         
         B     OK6                                                              
         SPACE 1                                                                
OK4      LA    R4,32(R4)                                                        
         BCT   R0,OK6                                                           
         MVI   ERROR,NOTOK                                                      
         B     TRAPERR                                                          
         SPACE 1                                                                
OK6      CLI   DICTYPE,X'10'                                                    
         BL    OKFREE                                                           
         CLI   DICTYPE,X'20'                                                    
         BL    OKNUM                                                            
         B     OK4                                                              
         SPACE 1                                                                
OKFREE   ZIC   R1,5(R2)            FREE FORM                                    
         BCTR  R1,0                                                             
         EX    R1,OKCLC            DOES IT MATCH?                               
         BE    XIT                 YES - VALID                                  
         CLI   1(R4),0             WAS A RANGE SPECFIIED?                       
         BE    OK4                                                              
         EX    R1,OKCLC            YES - DOES IT FIT?                           
         BL    OK4                                                              
         EX    R1,OKCLC2                                                        
         BH    OK4                                                              
         B     XIT                 YES - VALID                                  
         SPACE 1                                                                
OKCLC    CLC   8(0,R2),12(R4)                                                   
OKCLC2   CLC   8(0,R2),22(R4)                                                   
         SPACE 1                                                                
OKNUM    ZIC   R1,5(R2)            NUMERIC                                      
         BCTR  R1,0                                                             
         EX    R1,OKPACK                                                        
         CVB   R1,DUB                                                           
         C     R1,4(R4)            DOES IT MATCH?                               
         BE    XIT                 YES - VALID                                  
         CLI   1(R4),0             RANGE SPECIFIED?                             
         BE    OK4                                                              
         C     R1,4(R4)            YES - DOES IT FIT?                           
         BL    OK4                                                              
         C     R1,8(R4)                                                         
         BH    OK4                                                              
         B     XIT                 YES - VALID                                  
         SPACE 1                                                                
OKPACK   PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
*              DATA DISPLAY CONTROL                                             
         SPACE 3                                                                
VDSPDATA CLI   MODE,VALREC                                                      
         BE    VVALDATA                                                         
         CLI   MODE,VALKEY                                                      
         BE    VVALDATA                                                         
         BAS   RE,FIND                                                          
         USING DICEL,R3                                                         
         BAS   RE,BUMP                                                          
         LA    R4,KEY                                                           
         MVC   ELCODE,DICWHERE                                                  
         CLI   ELCODE,0                                                         
         BE    VDSP2                                                            
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    VDSP2                                                            
         XC    WORK,WORK                                                        
         B     VDSP4                                                            
         SPACE 1                                                                
VDSP2    ZIC   R1,DICDISP                                                       
         AR    R4,R1                                                            
         IC    R1,DICLEN                                                        
         BAS   RE,DISP             GO AND GET DATA INTO WORK                    
         SPACE 1                                                                
VDSP4    SR    R1,R1                                                            
         IC    R1,FHLND(R2)                                                     
         SH    R1,=Y(FHDAD+1)                                                   
         TM    FHATD(R2),FHATXH    TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R1,=Y(FHDAD)                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FHDAD(0,R2),WORK        IS DATA THERE ALREADY                    
         BE    XIT                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FHDAD(0,R2),WORK        NO - SO SHIP IT OUT                      
         OI    FHOID(R2),FHOITR               AND TRANSMIT                      
         TM    FHATD(R2),FHATPR                                                 
         BNZ   XIT                                                              
         LR    R0,R1               SET INPUT LENGTH                             
         AH    R0,=Y(1)                                                         
         LA    RF,FHDAD(R1,R2)                                                  
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,FHILD(R2)                                                     
         B     XIT                                                              
         EJECT                                                                  
*              SELECT DATA DISPLAY ROUTINES                                     
         SPACE 3                                                                
DISP     NTR1                                                                   
         MVC   WORK,SPACES                                                      
         ZIC   RF,DICTYPE          GET ROUTINE NUMBER                           
         SRL   RF,4                                                             
         SLL   RF,2                                                             
         IC    R1,DICTYPE                                                       
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         ZIC   R5,DICLEN                                                        
         BCTR  R5,0                PASS L-1 AS PARAMETER                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)       MAKE SURE THERES DATA THERE                  
         BZ    XIT                 EXIT IF NONE                                 
         B     DSPBRAN(RF)                                                      
         SPACE 1                                                                
DSPBRAN  B     DFREE                                                            
         B     DNUM                                                             
         B     DDEC                                                             
         B     XIT                                                              
         B     DYMD                                                             
         B     DYM                                                              
         B     DMD                                                              
         B     DPER                                                             
         B     DTIME                                                            
         B     DDAY                                                             
         SPACE 1                                                                
DFREE    CLI   DICTYPE,X'06'                                                    
         BE    DHEX                                                             
         EX    R5,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),0(R4)                                                    
         SPACE 1                                                                
DHEX     GOTO1 HEXOUT,DMCB,(R4),WORK,(R5),=C'TOG'                               
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY NUMERIC                                                  
         SPACE 3                                                                
DNUM     CLI   DICTYPE,X'14'       EBCDIC                                       
         BE    DBIN                                                             
         BH    DPACK                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
         SPACE 1                                                                
DSHIFT   CLI   WORK,C' '                                                        
         BNE   DSHIFT2                                                          
         MVI   WORK,X'F0'                                                       
         B     XIT                                                              
         SPACE 1                                                                
DSHIFT2  CLI   WORK,X'F0'                                                       
         BNE   XIT                                                              
         MVC   WORK(L'WORK-1),WORK+1                                            
         B     DSHIFT                                                           
         SPACE 1                                                                
DBIN     XC    DUB,DUB             BINARY                                       
         LA    R1,DUB+3                                                         
         SR    R1,R5                                                            
         EX    R5,NUMMVC                                                        
         L     R1,DUB                                                           
         CVD   R1,DUB                                                           
         B     DNUMEND                                                          
         SPACE 1                                                                
DPACK    XC    DUB,DUB             PACKED                                       
         LA    R1,DUB+7                                                         
         SR    R1,R5                                                            
         EX    R5,NUMMVC                                                        
         CLI   DICTYPE,X'16'                                                    
         BE    DNUMEND                                                          
         LM    RE,RF,DUB           PWOS                                         
         SLDL  RE,4                          SHIFT                              
         O     RF,=F'12'                     AND POP IN A SIGN (X'0C')          
         STM   RE,RF,DUB                                                        
         SPACE 1                                                                
DNUMEND  EDIT  (P8,DUB),(16,WORK),ALIGN=LEFT,WRK=WORK                           
         B     DSHIFT                                                           
         EJECT                                                                  
*              DISPLAY DECIMAL (1-5 PLACES)                                     
         SPACE 3                                                                
DDEC     LR    RF,R1               N'DECIMALS                                   
         XC    DUB,DUB                                                          
         LA    R1,DUB+3                                                         
         SR    R1,R5                                                            
         EX    R5,NUMMVC                                                        
         L     R1,DUB                                                           
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     DDECBR(RF)                                                       
         SPACE 1                                                                
DDECBR   B     DDEC1                                                            
         B     DDEC2                                                            
         B     DDEC3                                                            
         B     DDEC4                                                            
         B     DDEC5                                                            
         SPACE 1                                                                
DDEC1    EDIT  (R1),(16,WORK),1,ALIGN=LEFT,WRK=DMCB                             
         B     XIT                                                              
         SPACE 1                                                                
DDEC2    EDIT  (R1),(16,WORK),2,ALIGN=LEFT,WRK=DMCB                             
         B     XIT                                                              
         SPACE 1                                                                
DDEC3    EDIT  (R1),(16,WORK),3,ALIGN=LEFT,WRK=DMCB                             
         B     XIT                                                              
         SPACE 1                                                                
DDEC4    EDIT  (R1),(16,WORK),4,ALIGN=LEFT,WRK=DMCB                             
         B     XIT                                                              
         SPACE 1                                                                
DDEC5    EDIT  (R1),(16,WORK),5,ALIGN=LEFT,WRK=DMCB                             
         B     XIT                                                              
         SPACE 1                                                                
NUMMVC   MVC   0(0,R1),0(R4)                                                    
         EJECT                                                                  
*              DISPLAY DATES                                                    
         SPACE 3                                                                
DYMD     LR    R5,R1               YMD                                          
         MVC   WORK,0(R4)                                                       
         CH    R5,=H'4'            IPL IS OK                                    
         BE    XIT                                                              
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,((R5),(R4)),(8,WORK)                                 
*&&UK*&& OI    WORK,X'F0'                                                       
         B     XIT                                                              
         SPACE 1                                                                
DYM      MVC   DUB(4),0(R4)        YM                                           
         MVC   DUB+4(2),=C'01'                                                  
         LTR   R5,R1                                                            
         BZ    *+8                                                              
         MVI   DUB+2,X'01'                                                      
         GOTO1 DATCON,DMCB,((R5),DUB),(9,WORK)                                  
         B     XIT                                                              
         SPACE 1                                                                
DMD      MVC   DUB(2),=C'01'       MD                                           
         MVC   DUB+2(4),0(R4)                                                   
         LTR   R5,R1                                                            
         BZ    DMD2                                                             
         MVI   DUB,1                                                            
         MVC   DUB+1(2),0(R4)                                                   
         SPACE 1                                                                
DMD2     GOTO1 DATCON,DMCB,((R5),DUB),(4,WORK)                                  
         B     XIT                                                              
         SPACE 1                                                                
DPER     LR    R5,R1               PERIOD                                       
         GOTO1 DATCON,DMCB,((R5),(R4)),(8,WORK)                                 
*&&US*&& LA    R6,WORK+8                                                        
*&&UK*&& LA    R6,WORK+7                                                        
*&&UK*&& OI    WORK,X'F0'                                                       
         MVI   0(R6),C'-'                                                       
         LA    R4,2(R4)                                                         
         CLI   DICTYPE,X'72'                                                    
         BE    DPER2                                                            
         LA    R4,1(R4)                                                         
         CLI   DICTYPE,X'70'                                                    
         BNE   DPER2                                                            
         LA    R4,3(R4)                                                         
         SPACE 1                                                                
DPER2    GOTO1 DATCON,DMCB,((R5),(R4)),(8,1(R6))                                
*&&UK*&& OI    1(R6),X'F0'                                                      
         B     XIT                                                              
         SPACE 1                                                                
DTIME    GOTO1 UNTIME,DMCB,(R4),WORK         TIME (MILITARY)                    
         B     XIT                                                              
         SPACE 1                                                                
DDAY     GOTO1 UNDAY,DMCB,(R4),WORK                                             
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE FILTER EXPRESSION                                       
         SPACE 3                                                                
VVALFILT XC    INTFILTS,INTFILTS   DEFAULT IS NONE                              
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         LA    R4,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         MVI   ERROR,INVFILT                                                    
         BZ    TRAPERR                                                          
         LA    R5,INTFILTS                                                      
         SPACE 1                                                                
VF2      MVC   WORK(8),12(R4)      DO WE HAVE A GOOD KEYWORD                    
         MVI   ERROPT,1            ENSURE WE COME BACK                          
         BAS   RE,KEYLOOK                                                       
         LTR   R3,R3                                                            
         BZ    TRAPERR                                                          
         MVC   0(4,R5),DICTYPE     SAVE TYPE/WHERE/DISP/LEN                     
         SPACE 1                                                                
         XC    LISTAR,LISTAR       BUILD PHONY HEADER                           
         MVC   LISTAR+5(1),1(R4)   LENGTH OF SECOND HALF                        
         ZIC   R1,3(R4)            VALIDITY BITS                                
         SRL   R1,4                                                             
         STC   R1,LISTAR+4                                                      
         MVC   LISTAR+8(10),22(R4)                                              
         LR    RF,R2                                                            
         LA    R2,LISTAR                                                        
         BAS   RE,VAL              SO WE CAN USE VALIDATE ROUTINES              
         LR    R2,RF                                                            
         CLI   ERROPT,2                                                         
         BE    TRAPERR                                                          
         ZIC   R1,DICLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R5),WORK        SAVE DATA IN FILTER POOL                     
         LA    R5,5(R1,R5)                                                      
         LA    R4,32(R4)                                                        
         BCT   R0,VF2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              EXECUTE FILTER EXPRESSION                                        
         SPACE 3                                                                
VEXCFILT LA    R5,INTFILTS                                                      
         MVI   ACTUAL,C'N'                                                      
         SPACE 1                                                                
VE2      CLI   0(R5),0                                                          
         BNE   VE4                                                              
         MVI   ACTUAL,C'Y'                                                      
         B     XIT                                                              
         SPACE 1                                                                
VE4      LA    R4,KEY              DATA FROM KEY                                
         CLI   1(R5),0                                                          
         BE    VE6                                                              
         MVC   ELCODE,1(R5)        OR FROM AN ELEMENT                           
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
VE6      ZIC   R1,2(R5)            ADD IN DISPLACEMENT                          
         AR    R4,R1                                                            
         ZIC   R1,3(R5)            PICK UP LENGTH                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   4(0,R5),0(R4)                                                    
         BNE   XIT                                                              
         LA    R5,5(R1,R5)                                                      
         B     VE2                                                              
         EJECT                                                                  
*              VALIDATE KEYWORD LIST                                            
         SPACE 3                                                                
VVALLIST L     R5,0(R1)            P1=ADDRESS OUTPUT AREA                       
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(15,BLOCK),0                                   
         ZIC   R0,DMCB+4                                                        
         STC   R0,ACTUAL                                                        
         LA    R4,BLOCK                                                         
         LA    R6,1                TRACK TOTAL WIDTH IN R6                      
         LTR   R0,R0                                                            
         BNZ   VL2                                                              
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
VL2      MVC   WORK(8),12(R4)      CHECK FOR GOOD KEYWORD                       
         BAS   RE,KEYLOOK                                                       
         LTR   R3,R3                                                            
         BZ    TRAPERR                                                          
         MVC   0(1,R5),DICNUM      YES - SAVE KEY NUMBER                        
         ZIC   R1,DICOLEN                                                       
         LA    R6,1(R1,R6)         ADD LENGTH OF THIS FIELD +1                  
         LA    R5,1(R5)                                                         
         LA    R4,32(R4)                                                        
         BCT   R0,VL2                                                           
         STC   R6,ACTUAL+1         PASS TOTAL LENGTH BACK                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DIG OUT DATA                                          
         SPACE 3                                                                
VDIGDATA L     R5,0(R1)            P1=ADDRESS OF KEY NUMBER OR NAME             
         MVC   WORK(8),0(R5)                                                    
         L     R5,4(R1)            P2=ADDRESS OF OUTPUT AREA                    
         BAS   RE,KEYLOOK          CHECK KEY NUMBER                             
         MVI   ACTUAL,0                                                         
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         MVC   ACTUAL,DICLEN       PASS BACK GOOD LENGTH                        
         LA    R4,KEY              PICK UP DATA FROM KEY                        
         CLI   DICWHERE,0                                                       
         BE    DIG2                                                             
         MVC   ELCODE,DICWHERE                                                  
         L     R4,AIO                                                           
         BAS   RE,GETEL            OR FROM ELEMENT                              
         BE    DIG2                                                             
         MVI   ACTUAL,0                                                         
         B     XIT                                                              
DIG2     ZIC   R0,DICDISP                                                       
         AR    R4,R0                                                            
         CLI   4(R1),C'E'          OPTION TO PASS BACK EDITED                   
         BE    VE1                                                              
         ZIC   R1,DICLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,DIGMVC                                                        
         B     XIT                                                              
         SPACE 1                                                                
DIGMVC   MVC   0(0,R5),0(R4)                                                    
         EJECT                                                                  
*              ROUTINE TO CONVERT DATA TO EDITED FORMAT                         
         SPACE 3                                                                
VEDTDATA L     R5,0(R1)            P1=ADDRESS OF KEY NUMBER/NAME                
         MVC   WORK(8),0(R5)                                                    
         BAS   RE,KEYLOOK                                                       
         LM    R4,R5,4(R1)         P2=A(DATA)                                   
*                                  P3=A(OUTPUT AREA)                            
         MVI   ACTUAL,0                                                         
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
VE1      BAS   RE,DISP             GET EDITED DATA INTO WORK                    
         ZIC   R1,DICOLEN          PICK UP OUTPUT LENGTH                        
         BCTR  R1,0                                                             
         EX    R1,EDOUT                                                         
         MVC   ACTUAL,DICOLEN      PASS BACK TO USER                            
         TM    DICFLAG,X'02'       IF OUTPUT IS NOT RIGHT ALIGNED               
         BNO   XIT                 WE'RE DONE                                   
         CLC   WORK,SPACES                                                      
         BE    XIT                                                              
         LA    RE,WORK+63          OTHERWISE FIND LENGTH OF DATA                
         LA    RF,63                                                            
         SPACE 2                                                                
VED2     CLI   0(RE),C' '                                                       
         BNE   VED4                                                             
         BCTR  RE,0                                                             
         BCT   RF,VED2                                                          
         SPACE 1                                                                
VED4     LA    R1,1(R1)                                                         
         CR    RF,R1                                                            
         BNL   XIT                 WONT FIT OR ALREADY ALIGNED                  
         BCTR  R1,0                                                             
         EX    R1,EDCLEAR          OTHERWISE CLEAR                              
         LA    R1,1(R1)                                                         
         SR    R1,RF                                                            
         AR    R5,R1               DISPLACE INTO OUTPUT AREA                    
         BCTR  RF,0                                                             
         EX    RF,EDOUT                                                         
         B     XIT                                                              
         SPACE 1                                                                
EDCLEAR  MVC   0(0,R5),SPACES                                                   
EDOUT    MVC   0(0,R5),WORK                                                     
         EJECT                                                                  
*              ROUTINE TO DIG OUT HEADLINE DETAILS                              
         SPACE 3                                                                
VDIGHEAD L     R5,0(R1)            A(KEY NUMBER)                                
         MVC   WORK(8),0(R5)                                                    
         BAS   RE,KEYLOOK                                                       
         MVI   ACTUAL,0                                                         
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         MVC   ACTUAL,DICOLEN      PASS BACK LENGTH                             
         L     R5,4(R1)            A(OUTPUT IN HEADLINE)                        
         ZIC   R2,4(R1)            DISPLACEMENT                                 
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         LA    R2,132              DEFAULT=132                                  
         LA    R6,DICKEY           DEFAULT IS TO OUTPUT KEYWORD                 
         LA    R1,8                                                             
         MVI   ELCODE,X'07'        UNLESS THERE IS A HEADLINE 1 EL              
         TM    DICFLAG,X'10'                                                    
         BO    DH2                                                              
         MVI   ELCODE,X'03'        OR A SCREEN HEADER                           
         TM    DICFLAG,X'40'                                                    
         BNO   DH4                                                              
         SPACE 1                                                                
DH2      LR    R4,R3               FIND ELEMENT                                 
         BAS   RE,NEXTEL                                                        
         LA    R6,2(R4)                                                         
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'2'                                                         
         SPACE 1                                                                
DH4      BAS   RE,DHOUT            DEFAULT IS TO UNDERLINE                      
         LA    R4,0(R5,R2)                                                      
         ZIC   R6,DICOLEN                                                       
         GOTO1 UNDERLIN,DMCB,                                          X        
               (R5),(R4),(R6)                                                   
         LR    R5,R4                                                            
         TM    DICFLAG,X'08'       UNLESS AN H2 ELEMENT IS COMING               
         BNO   DH6                                                              
         LR    R4,R3                                                            
         MVI   ELCODE,X'08'                                                     
         BAS   RE,NEXTEL                                                        
         LA    R6,2(R4)                                                         
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'2'                                                         
         BAS   RE,DHOUT                                                         
         SPACE 1                                                                
DH6      AR    R5,R2                                                            
         TM    DICFLAG,X'04'       ALSO SUPPORT H3                              
         BNO   XIT                                                              
         LR    R4,R3                                                            
         MVI   ELCODE,X'09'                                                     
         BAS   RE,NEXTEL                                                        
         LA    R6,2(R4)                                                         
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'2'                                                         
         BAS   RE,DHOUT                                                         
         B     XIT                                                              
         SPACE 1                                                                
DHOUT    MVC   WORK,SPACES         MOVE INTO WORK                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R6)                                                    
         ZIC   R1,DICOLEN          AND THEN INTO HEADLINES                      
         BCTR  R1,0                                                             
         EX    R1,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R5),WORK                                                     
         EJECT                                                                  
*              ROUTINE TO FIND KEY WORD                                         
         SPACE 3                                                                
FIND     NTR1                                                                   
         L     R3,ADTADICT                                                      
         MVI   ERROR,NODICT                                                     
         LTR   R3,R3                                                            
         BZ    TRAPERR                                                          
         MVI   ERROR,NOKEY                                                      
         B     FIND4                                                            
         SPACE 1                                                                
FIND2    BAS   RE,DBUMP                                                         
         BE    TRAPERR                                                          
         SPACE 1                                                                
FIND4    CLI   DICELCOD,X'01'                                                   
         BNE   FIND2                                                            
         TM    DICFLAG,X'40'                                                    
         BO    FIND6                                                            
         SR    R1,R1                                                            
         IC    R1,FHLND(R2)                                                     
         SH    R1,=Y(FHDAD+1)                                                   
         TM    FHATD(R2),FHATXH                                                 
         BZ    *+8                                                              
         SH    R1,=Y(FHDAD)                                                     
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        OC    8(0,R2),SPACES      MAY BE BINARY ZEROS                          
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        CLC   8(0,R2),DICKEY      TRY AND MATCH ON KEY WORD                    
         MVC   WORK,FHDAD(R2)      MOVE FROM SCREEN AS WE CAN HAVE              
         OC    WORK,SPACES         LOWER CASE TEXT WHICH                        
         EX    R1,*+8              IS CORRUPTED BY THE OC INSTRUCTION           
         B     *+10                                                             
         CLC   WORK(0),DICKEY      TRY AND MATCH ON KEY WORD                    
         BE    FOUND                                                            
         B     FIND2                                                            
         SPACE 1                                                                
FIND6    LR    R0,R3               UNLESS THERE'S A SCREEN ELEMENT              
         SPACE 1                                                                
FIND8    BAS   RE,DBUMP                                                         
         BE    TRAPERR                                                          
         CLI   DICELCOD,X'03'      LOOK FOR BOTH STYLES OF SCREEN ELT           
         BE    FIND9                                                            
         CLI   DICELCOD,X'13'                                                   
         BNE   FIND8                                                            
*                                                                               
         SR    RE,RE               HANDLE SCREEN=AAAAAAQ                        
         IC    RE,FHLND(R2)                                                     
         LA    RF,0(RE,R2)                                                      
         CLI   MODE,DISPREC        ALLOW PROTECTED DISPLAY FIELDS               
         BE    *+12                                                             
         TM    FHATD(RF),FHATPR                                                 
         BO    FIND2                                                            
         TM    FHATD(RF),FHATXH                                                 
         BNO   FIND2                                                            
         IC    RE,FHLND(RF)                                                     
         LA    RF,0(RE,RF)                                                      
         SH    RF,=Y(FHDAD)                                                     
         CLC   FHNU-FHDA(,RF),DICVALS   MATCH ON FIELD ID NUMBER                
         BNE   FIND2                                                            
         B     FIND10                                                           
*                                                                               
FIND9    SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FHDAD(0,R2),DICVALS     AND MATCH AGAINST THAT                   
         BNE   FIND2                                                            
*                                                                               
FIND10   LR    R3,R0                                                            
         SPACE 1                                                                
FOUND    XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*              LOOK UP KEYWORD                                                  
         SPACE 3                                                                
KEYLOOK  NTR1                                                                   
         CLI   WORK,C' '           ROUTINE MATCHES KEYWORD OR KEY               
         BNH   KL4                 NUMBER IN WORK AGAINST DICTIONARY            
         LA    R1,7                                                             
         LA    RE,WORK+7                                                        
         SPACE 1                                                                
KL2      CLI   0(RE),C' '          GET LENGTH OF KEY WORD -1 TO R1              
         BNH   KL4                                                              
         BCTR  R1,0                                                             
         BCT   RE,KL2                                                           
         SPACE 1                                                                
KL4      L     R3,ADTADICT                                                      
         MVI   ERROR,NODICT                                                     
         LTR   R3,R3                                                            
         BZ    KLNO                                                             
         MVI   ERROR,NOKEY                                                      
         B     KL8                                                              
         SPACE 1                                                                
KL6      BAS   RE,DBUMP                                                         
         BE    KLNO                                                             
         SPACE 1                                                                
KL8      CLI   DICELCOD,X'01'                                                   
         BNE   KL6                                                              
         EX    R1,KLCLC                                                         
         BE    KLYES                                                            
         CLC   DICNUM,WORK                                                      
         BNE   KL6                                                              
         SPACE 1                                                                
KLYES    XIT1  REGS=(R3)                                                        
         SPACE 1                                                                
KLNO     SR    R3,R3                                                            
         B     KLYES                                                            
         SPACE 1                                                                
KLCLC    CLC   WORK(0),DICKEY                                                   
         EJECT                                                                  
*              EXITS AND ODDMENTS                                               
         SPACE 3                                                                
TRAPERR  CLI   ERROPT,C'Y'                                                      
         BNE   TRAPERR2                                                         
         MVI   ERROPT,0                                                         
         B     XIT                                                              
         SPACE 1                                                                
TRAPERR2 CLI   ERROPT,1                                                         
         BNE   TRAPERR4                                                         
         MVI   ERROPT,2                                                         
         B     XIT                                                              
         SPACE 1                                                                
TRAPERR4 GOTO1 ERREX                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BR    RE                                                               
         SPACE 1                                                                
YES      SR    R0,R0                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R0,1                                                             
         LTR   R0,R0                                                            
         B     XIT                                                              
         SPACE 1                                                                
DBUMP    NTR1                                                                   
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),0                                                          
         XIT1  REGS=(R3)                                                        
         SPACE 1                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
       ++INCLUDE DDDICTD                                                        
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDGENFFD                                                       
       EJECT                                                                    
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDFH                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030DDDICTCON 05/01/02'                                      
         END                                                                    
