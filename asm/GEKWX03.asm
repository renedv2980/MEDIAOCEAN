*          DATA SET GEKWX03    AT LEVEL 002 AS OF 05/24/96                      
*PHASE TF2003A,+0                                                               
         TITLE '$KWX MK3 - FORMAT MODE ACTIONS ADD,DIS,CHA,INS,DEL'             
         PRINT NOGEN                                                            
KWX03    CSECT                                                                  
         NMOD1 0,**KWX03*,R9                                                    
         L     RC,0(R1)                                                         
         USING KWX03+4096,R9       R9 = 2ND BASE                                
         USING TWAD,RA             RA = TWA                                     
         USING GWS,RC              RC = GWS                                     
*                                                                               
T001     ST    RB,ABASE2                                                        
         ST    R9,A2NDBAS2                                                      
         ST    RD,AREGSAV2                                                      
         B     T010                                                             
         EJECT                                                                  
*              FURTHER PARAMETER CHECKS ON PRE-PROCESSED PARAMETERS             
*                                                                               
T010     ZIC   R0,FXREF            MUST BE ONE OF REF=N                         
         SR    R1,R1                              LAST                          
         AR    R1,R0                              NEXT - AND ONLY ONE           
         IC    R0,FXLAST                                                        
         AR    R1,R0                                                            
         IC    R0,FXNEXT                                                        
         AR    R1,R0                                                            
         BNZ   T012                                                             
         MVI   FERN,MISSING        NONE                                         
         MVI   FNDX,2                                                           
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
T012     CH    R1,=H'2'                                                         
         BE    T015                                                             
         IC    R0,FXREF            MORE THAN 1                                  
         IC    R1,FXLAST                                                        
         CR    R0,R1                                                            
         BH    *+6                                                              
         LR    R0,R1                                                            
         IC    R1,FXNEXT                                                        
         CR    R0,R1                                                            
         BH    *+6                                                              
         LR    R0,R1                                                            
         STC   R0,FNDX                                                          
         MVI   FERN,INCPARMS                                                    
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
*                                                                               
T015     CLI   FXREF,0             REF=N                                        
         BE    T020                                                             
         CLC   REFLO,REFHI                                                      
         BE    T017                                                             
         MVI   FERN,INVALID        N-N INVALID                                  
         MVC   FNDX,FXREF                                                       
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
T017     CLC   REFLO,FRMRECHI                                                   
         BNH   T019                                                             
         CLI   ACTION,ADF                                                       
         BE    *+12                                                             
         MVI   FERN,RECNFND        N BEYOND END OF BOOK AND NOT ADD             
         B     ERROR                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FRMRECHI                                                    
         LA    R1,1(R1)                                                         
         CH    R1,REFLO                                                         
         BE    T050                                                             
         MVI   FERN,TOOHIGH        N MORE THAN 1 BEYOND END AND ADD             
         MVC   FNDX,FXREF                                                       
         B     ERROR                                                            
T019     CLI   ACTION,ADF                                                       
         BNE   T050                                                             
         MVI   FERN,RECEXIST       N NOT BEYOND END AND ADD                     
         B     ERROR                                                            
*                                                                               
T020     CLI   FXLAST,0            LAST                                         
         BE    T030                                                             
         CLI   ACTION,ADF                                                       
         BNE   T024                                                             
         CLC   FRMRECHI,FRMRPEAT                                                
         BE    T028                                                             
         MVI   FERN,RECEXIST       ADD INVALID IF LAST EXIST  (1 BEYOND         
         B     ERROR               REPEAT CHUNK)                                
T024     CLC   FRMRECHI,FRMRPEAT                                                
         BH    *+12                                                             
         MVI   FERN,RECNFND        OTHER ACTIONS INVALID IF IT DOESN'T          
         B     ERROR                                                            
         MVC   REFLO,FRMRECHI                                                   
         B     T050                                                             
T028     SR    R1,R1                                                            
         ICM   R1,3,FRMRECHI                                                    
         LA    R1,1(R1)                                                         
         STH   R1,REFLO                                                         
         B     T050                                                             
*                                                                               
T030     CLC   DISPLOF,FRMRECHI    NEXT                                         
         BL    T034                                                             
         MVI   FERN,RECNFND        CURRENT DISPLAY CHUNK MUSTN'T BE END         
         B     ERROR                                                            
T034     LH    R1,DISPLOF                                                       
         LA    R1,1(R1)                                                         
         STH   R1,REFLO                                                         
         B     T050                                                             
         EJECT                                                                  
*              HANDLE DISPLAY OF REQUIRED CHUNK IN DEFINITION FORM              
*                                                                               
T050     MVI   FNDX,0              INITIALIZE                                   
         MVC   REFHI,REFLO                                                      
*                                                                               
T052     CLI   FRMSTAT,FORMAT      DO WE NEED THE SCREEN SET TO THE             
         BNE   T055                RIGHT FORMAT                                 
         CLI   ACTION,ADF                                                       
         BE    T100                                                             
         CLI   ACTION,INF                                                       
         BE    T100                                                             
         CLI   ACTION,DIF                                                       
         BE    T055                                                             
         CLC   REFLO,DISPLOF                                                    
         BE    T080                                                             
T055     XC    PARAS(12),PARAS     YES                                          
         GOTO1 ASETSCRN,PARAS                                                   
         CLI   ACTION,ADF          IF ADD/INSERT ASK FOR INPUT                  
         BE    *+12                                                             
         CLI   ACTION,INF                                                       
         BNE   T060                                                             
         LA    R2,KWXDATAH                                                      
         ST    R2,ACURSOR                                                       
         MVC   KWXHEAD(23),=C'ENTER FORMAT DEFINITION'                          
         B     MOREXIT                                                          
*                                                                               
T060     MVI   PARAS,FORMAT        NOW GET THE CHUNK AND DISPLAY IT             
         MVI   PARAS+1,0                                                        
         MVC   PARAS+2(2),REFLO                                                 
         GOTO1 AGETCHNK,PARAS                                                   
         BZ    ERROR                                                            
         L     RE,AIOB             MOVE REC FROM IOB TO IO                      
         LH    RF,0(RE)                                                         
         LA    R0,IO                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         MVI   PARAS,0                                                          
         GOTO1 ADISFORM,PARAS                                                   
T062     MVC   PARAS(4),REFLO                                                   
         MVC   PARAS+10(2),FRMRECHI                                             
         GOTO1 AEDITREF,PARAS,,KWXHEAD                                          
         L     RF,4(R1)                                                         
         MVC   0(11,RF),=C'DISPLAYED -'                                         
         LA    RF,12(RF)                                                        
*                                                                               
T065     CLI   ACTION,DIF          ACTION DISPLAY COMPLETED                     
         BNE   T070                                                             
T066     CLC   REFLO,FRMRECHI                                                   
         BNE   *+14                                                             
         MVC   0(17,RF),=C'ENTER NEXT ACTION'                                   
         B     OKXIT                                                            
         MVC   0(18,RF),=C'HIT ENTER FOR NEXT'                                  
         LA    R2,KWXTABH                                                       
         ST    R2,ACURSOR                                                       
         OI    KWXACTH+6,X'80'                                                  
         CLI   FXNEXT,0                                                         
         BNE   OKXIT                                                            
         XC    KWXACT,KWXACT                                                    
         MVC   KWXACT(12),=C'DISPLAY,NEXT'                                      
         B     OKXIT                                                            
*                                                                               
T070     CLI   ACTION,CHF          ACTION CHANGE - REQUEST CHANGES              
         BNE   T075                                                             
         MVC   0(17,RF),=C'NOW ENTER CHANGES'                                   
         LA    R2,KWXDATAH                                                      
         ST    R2,ACURSOR                                                       
         B     MOREXIT                                                          
*                                                                               
T075     CLI   ACTION,DEF          ACTION DELETE - HIT ENTER AGAIN              
         BNE   T07X                                                             
         MVC   0(19,RF),=C'HIT ENTER TO DELETE'                                 
         LA    R2,KWXTABH                                                       
         ST    R2,ACURSOR                                                       
         OI    KWXACTH+6,X'80'                                                  
         B     MOREXIT                                                          
T07X     DS    0H                                                               
         EJECT                                                                  
*              RECORD ALREADY ON DISPLAY                                        
*                                                                               
T080     CLI   ACTION,CHF          ACTION CHANGE - GO TO VALIDATE               
         BE    T100                                                             
         CLI   ACTION,DEF          ACTION DELETE                                
         BNE   T08X                                                             
         MVI   PARAS,FORMAT                                                     
         MVC   PARAS+2(2),REFLO                                                 
         MVC   PARAS+6(2),REFLO                                                 
         GOTO1 ADELCHNK,PARAS                                                   
         BZ    ERROR                                                            
         XC    DISPLOF(4),DISPLOF                                               
         B     T120                                                             
T08X     DS    0H                                                               
         EJECT                                                                  
*              VALIDATE FORMAT DEFINITION AND ADD/INSERT/AMEND RECORD           
*                                                                               
T100     GOTO1 VALFORM,PARAS,ADDBUFF    BUILD RECORD IN ADDBUFF                 
         BZ    ERROR                                                            
         MVI   PARAS,FORMAT                                                     
         MVI   PARAS+1,0                                                        
         MVC   PARAS+2(2),REFLO                                                 
         LA    RF,ADDBUFF                                                       
         ST    RF,PARAS+4                                                       
         CLI   ACTION,INF                                                       
         BNE   *+8                                                              
         MVI   PARAS+4,C'I'                                                     
*                                                                               
T105     CLI   ACTION,CHF          CHANGE                                       
         BNE   T110                                                             
         GOTO1 AGETCHNK            GET INTO IOB                                 
         BZ    ERROR                                                            
         L     RF,AIOB                                                          
         CLC   ADDBUFF(2),0(RF)    IF REC LEN CHANGED DELETE & ADD              
         BNE   T108                                                             
         GOTO1 APUTCHNK            PUT FROM IO                                  
         BZ    ERROR                                                            
         B     T120                                                             
*                                                                               
T108     MVC   4(4,R1),0(R1)       DELETE/ADD                                   
         GOTO1 ADELCHNK                                                         
         BZ    ERROR                                                            
         LA    RF,ADDBUFF                                                       
         ST    RF,4(R1)                                                         
         CLC   REFLO,FRMRECHI                                                   
         BH    T110                                                             
         MVI   4(R1),C'I'                                                       
*                                                                               
T110     GOTO1 AADDCHNK            ADD OR INSERT                                
         BZ    ERROR                                                            
         MVC   DISPLOF,REFLO                                                    
         MVC   DISPHIF,REFLO                                                    
*                                                                               
T120     MVC   PARAS(4),REFLO      COMPLETION MESSAGE                           
         SR    R2,R2               REF N OF N ACTIONED - ENTER NXT ACTN         
         ICM   R2,3,FRMRECHI                                                    
         CLI   ACTION,DEF                                                       
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         GOTO1 AEDITREF,PARAS,,KWXHEAD,(R2)                                     
         L     RF,4(R1)                                                         
         MVC   0(7,RF),=C'ADDED -'                                              
         LA    RE,8(RF)                                                         
         CLI   ACTION,ADF                                                       
         BE    T130                                                             
         MVC   0(9,RF),=C'DELETED -'                                            
         LA    RE,10(RF)                                                        
         CLI   ACTION,DEF                                                       
         BE    T130                                                             
         MVC   0(9,RF),=C'CHANGED -'                                            
         CLI   ACTION,CHF                                                       
         BE    T130                                                             
         MVC   0(10,RF),=C'INSERTED -'                                          
         LA    RE,11(RF)                                                        
*                                                                               
T130     MVC   0(17,RE),=C'ENTER NEXT ACTION'                                   
         CLI   ACTION,CHF                                                       
         BE    OKXIT                                                            
*                                                                               
T140     DS    0H                  ADD/INSERT/DELETE - UPDATE HDR               
         GOTO1 AGETCHNK,PARAS,(SAVMODE,0)                                       
         BZ    ERROR                                                            
         L     RF,AIOB                                                          
         USING HDRD,RF                                                          
         MVC   HDFRMHI,FRMRECHI                                                 
         MVC   HDFRMRPT,FRMRPEAT                                                
         DROP  RF                                                               
         GOTO1 APUTCHNK,(R1),,AIOB                                              
         B     OKXIT                                                            
         EJECT                                                                  
*              ROUTINE TO VALIDATE A FORMAT DEFINITION FOR A CHUNK AND          
*              BUILD A CHUNK RECORD                                             
*              ON ENTRY P1 = A(CHUNK RECORD)                                    
*              ON EXIT  CC = EQU IF ERROR AND ACURSOR IS SET TO FLD HDR         
*                            OF LINE IN ERROR                                   
*                                                                               
VALFORM  NTR1  WORK=(R8,2)                                                      
         USING VLFD,R8                                                          
         XC    VLFD(16),VLFD                                                    
         ST    R1,VLFAPARM                                                      
         L     R7,0(R1)            R7 = RECORD POINTER                          
         LA    R7,2(R7)                                                         
         USING FLDHDRD,R7                                                       
*                                                                               
VLF01    LA    R2,KWXDATAH         FIND CHUNK TERMINATOR (/&)                   
         LA    R0,8(R2)            R0 = A(FIRST DATA)                           
*                                                                               
VLF03    CLI   0(R2),0             END OF SCREEN                                
         BNE   VLF04                                                            
         MVI   FERN,NOTERM                                                      
         LA    R2,KWXDATAH                                                      
         B     VLFERR                                                           
VLF04    LA    R3,8(R2)            SCAN A ALINE FOR '/&'                        
         LR    R1,R3               R1 = A(FIRST DATA FOR LINE)                  
         LA    R4,1                                                             
         ZIC   R5,0(R2)                                                         
         AR    R5,R2                                                            
         SH    R5,=H'2'                                                         
         CLC   0(2,R3),=C'/&&'                                                  
         BE    VLF06                                                            
         BXLE  R3,R4,*-10                                                       
         LA    R2,1(R3)                                                         
         B     VLF03                                                            
VLF06    CR    R3,R0               FOUND TERMINATOR                             
         BNE   *+12                                                             
         MVI   FERN,MISSING        NO INPUT BEFORE TERMINATOR - ERROR           
         B     VLFERR                                                           
         CR    R3,R1               TERMINATOR AT START OF LINE - TREAT          
         BNE   *+8                 AS END OF PREVIOUS LINE                      
         SH    R3,=H'8'                                                         
         BCTR  R3,0                                                             
         ST    R3,VLFAEND          SAVE A(LAST CHARACTER BEFORE TERM)           
         LA    R2,KWXDATAH                                                      
*                                                                               
VLF10    LA    R3,8(R2)            NOW SCAN FOR PROT & UNPROT FIELDS            
         ZIC   R5,0(R2)            PER LINE                                     
         AR    R5,R2                                                            
         BCTR  R5,0                R5 = A(LAST CHAR IN LINE)                    
         C     R5,VLFAEND                                                       
         BNH   *+8                                                              
         L     R5,VLFAEND                                                       
         MVI   VLFUSE,0            CLEAR LINE USAGE INDICATOR                   
         MVI   FNDX,1                                                           
         LR    RF,R5                                                            
         SR    RF,R3                                                            
         EX    RF,*+8                                                           
         B     VLF12                                                            
         OC    0(0,R3),SPACES                                                   
*                                                                               
VLF12    CLC   0(4,R3),=C'&&EOL'   LOOK FOR &EOL - END OF PRINT LINE            
         BNE   VLF13               INDICATOR                                    
         CLI   VLFUSE,0                                                         
         BNE   *+12                                                             
         MVI   FERN,INVEOL         MUST FOLLOW A FLD ON SAME SCRN LINE          
         B     VLFERR                                                           
         SR    R7,R1                                                            
         OI    FLDOLEN,EOL                                                      
         AR    R7,R1                                                            
         LA    R3,3(R3)                                                         
         B     VLF14                                                            
*                                                                               
VLF13    CLI   0(R3),C' '          LOOK FOR START OF A FIELD                    
         BNE   VLF20                                                            
VLF14    BXLE  R3,R4,VLF12                                                      
         C     R3,VLFAEND          END OF LINE OR CHUNK                         
         BH    VLF16                                                            
         ZIC   R0,0(R2)            END OF LINE - BUMP TO NEXT                   
         AR    R2,R0                                                            
         B     VLF10                                                            
VLF16    CLI   VLFUSE,0            END OF CHUNK                                 
         BNE   VLFEND              IF NO FLD YET IN THIS LINE CREATE A          
         XC    FLDLEN(8),FLDLEN    1BYTE PROT                                   
         OI    FLDATB,X'20'                                                     
         BCTR  R3,0                                                             
         LA    RF,1(R3)            RF POINTS TO END OF FLD                      
         B     VLF70                                                            
*                                                                               
VLF20    XC    FLDLEN(8),FLDLEN    FOUND START OF A FLD                         
         CLC   0(3,R3),=C'&&NO'    IF ITS &NO MUSTNT BE AN UNPROT               
         BNE   VLF25               IMMEDIATELY BEFORE                           
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         CLI   0(RF),QUOT                                                       
         BNE   VLF25                                                            
         MVI   FERN,INVALID                                                     
         B     VLFERR                                                           
VLF25    MVI   VLFRULE,0           CLEAR WELL-FORMED-RULE MARKER                
         CLI   0(R3),QUOT                                                       
         BNE   VLF60                                                            
*                                                                               
VLF30    LA    R3,1(R3)            UNPROT FLD (WRAPPED IN QUOTES)               
         LR    RF,R3               RF = POINTER TO END OF FLD                   
         CLI   0(RF),C'&&'         RULES INDICATOR                              
         BE    VLF46                                                            
         CLI   0(RF),QUOT                                                       
         BNE   VLF54                                                            
         MVI   FERN,INVALID        ZERO-L UNPROT                                
         B     VLFERR                                                           
*                                                                               
VLF40    CLI   0(RF),QUOT          UNPROT CONTAINS RULES (INDICATED BY          
         BE    VLF50               & IN 1ST CHAR POS)                           
         CLI   0(RF),C' '                                                       
         BE    VLF50                                                            
         MVI   VLFRULE,1           RULE OK                                      
         L     RE,ARULETAB                                                      
VLF42    CLI   0(RE),X'FF'         CHECK RULE                                   
         BNE   *+12                                                             
VLF44    MVI   FERN,INVAMPRL       INVALID RULE                                 
         B     VLFERR                                                           
         CLC   0(1,RF),4(RE)                                                    
         BE    *+12                                                             
         LA    RE,8(RE)                                                         
         B     VLF42                                                            
         OC    FLDIIND(4),0(RE)    VALID RULE - SET BIT IN FLD HDR              
VLF46    BXLE  RF,R4,VLF40                                                      
VLF48    MVI   FERN,NOFLDTRM                                                    
         B     VLFERR                                                           
*                                                                               
VLF50    CLI   VLFRULE,0           END OF RULES                                 
         BE    VLF44               INVALID IF & AND NONE FOLLOWING              
         CLI   0(RF),QUOT          NOW LOOK FOR END OF FLD                      
         BE    VLF70                                                            
VLF54    BXLE  RF,R4,*-8                                                        
         B     VLF48               NO TERMINATOR                                
*                                                                               
VLF60    LR    RF,R3               PROTECTED FLD (NOT WRAPPED IN QUOTS)         
         OI    FLDATB,X'20'                                                     
         B     VLF63                                                            
VLF62    CLC   0(3,RF),=C'&&NO'    FIND END OF FLD BY FINDING &NO               
         BE    VLF65                                                            
         CLC   0(4,RF),=C'&&EOL'   OR &EOL                                      
         BE    VLF65                                                            
         CLI   0(RF),QUOT          OR START OF THE NEXT -                       
         BE    VLF65               UNPROT OR END OF LINE AND GOING BACK         
VLF63    BXLE  RF,R4,VLF62         TO LAST NON-SPACE PLUS 1                     
         B     VLF65                                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
VLF65    BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
*                                                                               
VLF70    LR    RE,RF               COMMON TO PROT & UNPROT                      
         SR    RE,R3               R3= FLD START, RF = FLD END + 1              
         BCTR  RE,0                                                             
         LR    R1,R3               MOVE FLDDATA UNLESS IT WAS RULES             
         CLI   VLFRULE,1                                                        
         BNE   *+8                                                              
         LA    R1,SPACES                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),0(R1)                                                 
         CH    RE,=H'2'            SET BIT FOR &NO (REF NUM)                    
         BNE   VLF72                                                            
         CLC   FLDDATA(3),=C'&&NO'                                              
         BNE   VLF72                                                            
         OI    FLDIIND,REFNUM                                                   
VLF72    LA    RE,9(RE)                                                         
         STC   RE,FLDLEN           SET FLDLEN                                   
         LH    R1,2(R2)            SET FLDADR AS CHUNK-RELATIVE                 
         AR    R1,R3                                                            
         SR    R1,R2                                                            
         SH    R1,=H'248'          240 = FLDADR OF KWXDATA, 8 = FLDHDR          
         STH   R1,FLDADR                                                        
*                                                                               
         MVI   VLFUSE,1            BUMP TO NEXT FLD                             
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         TM    FLDATB,X'20'        IF UNPROT GO BACK ONE                        
         BNO   *+6                                                              
         BCTR  RF,0                                                             
         IC    R1,FLDLEN           BUMP REC POINTER                             
         AR    R7,R1                                                            
         LR    R3,RF                                                            
         B     VLF14               GO AND LOOK FOR NEXT FLD                     
*                                                                               
VLFEND   MVI   0(R7),0             TERMINATE RECORD                             
         MVI   1(R7),X'FF'         AND BUFFER                                   
         L     R1,VLFAPARM                                                      
         L     R1,0(R1)            SET REC LEN                                  
         SR    R7,R1                                                            
         LA    R7,1(R7)                                                         
         STH   R7,0(R1)                                                         
         MVI   FNDX,0                                                           
         CH    R7,=H'1024'                                                      
         BNH   OKXIT                                                            
         LA    R2,KWXDATAH                                                      
         MVI   FERN,RECTOOBG       TOO LONG                                     
*                                                                               
VLFERR   ST    R2,ACURSOR          AT ERROR RETURN A(LINE HDR)                  
         B     ERROR                                                            
         DROP  R7,R8                                                            
         SPACE 3                                                                
VLFD     DSECT                     DSECT TO COVER VALFORM LOCAL W/S             
VLFAPARM DS    CL4                 A(PARAMETER LIST)                            
VLFAEND  DS    CL4                 A(LAST CHAR ON CHUNK ON SCREEN)              
VLFUSE   DS    C                   0 = NO FLD IN LINE, 1 = AT LEAST ONE         
VLFRULE  DS    C                   0 = NO RULE IN FLD, 1 = AT LEAST ONE         
         DS    6C                  SPARE                                        
KWX03    CSECT                                                                  
         EJECT                                                                  
*              EXITS BACK TO ROOT                                               
*                                                                               
ERROR    SR    R0,R0               CC = EQU FOR ERROR                           
         B     EXIT                                                             
*                                                                               
MOREXIT  LNR   RB,RB               CC = NEG FOR MORE INPUT                      
*                                                                               
OKXIT    LTR   RB,RB               CC = POS OK COMPLETED                        
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
* NESTED INCLUDES                                                               
* GEKWXDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE GEKWXDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GEKWX03   05/24/96'                                      
         END                                                                    
