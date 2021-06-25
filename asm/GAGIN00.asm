*          DATA SET GAGIN00    AT LEVEL 124 AS OF 08/22/00                      
*                                                                               
*PHASE TB1D00A                                                                  
*INCLUDE XSORT                                                                  
         SPACE 10                                                               
GIN      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 GINWRKX-GINWRK,GAGIN00,R9,RR=R2                                  
         USING GINWRK,RC                                                        
         L     RA,4(R1)                                                         
         USING TB1DFFD,RA                                                       
         ST    R2,RELO                                                          
         L     RF,8(R1)                                                         
         MVC   VDATAMGR,0(RF)                                                   
         MVC   HEXOUT,CHEXOUT-COMFACSD(RF)                                      
*                                                                               
*                                                                               
         TITLE '-- $@+% GIN $@+% --'                                            
********************************************************************            
*                          MAIN                                                 
********************************************************************            
MAIN     MVC   GINHEAD,BLANKLN                                                  
         OI    GINHEADH+6,X'80'                                                 
*                                                                               
         CLI   16(RA),0            0 FOR FIRST TIME THROUGH                     
         BNE   NOTNEW                                                           
         MVI   16(RA),1                                                         
         MVC   GININPT,BLANKLN     CLEAR INPUT FIELD                            
         OI    GININPTH+6,X'80'                                                 
         MVI   GININPTH+5,0                                                     
         MVI   SORTTP,C'N'                                                      
*                                                                               
         TBIN  MILLI               TIME IN MILLISECS IN R1                      
         M     R0,RN               PLANT RANDOM SEED                            
         ST    R1,RN                                                            
*                                                                               
NOTNEW   CLI   16(RA),1            1 FOR NEW GAME                               
         BNE   INPROGRS                                                         
         MVI   16(RA),2                                                         
         BAS   RE,DEAL                                                          
         GOTO1 =V(XSORT),DMCB,(1,CCARDS),8,2,2,0,RR=RELO                        
         BAS   RE,SORT                                                          
         MVC   GINCHND,BLANKLN                                                  
         OI    GINCHNDH+6,X'80'    XMIT                                         
         XC    HILITED,HILITED                                                  
         OI    GINDKSLH+6,X'C0'    CURSOR TO DECK SELECT                        
         B     EXIT                                                             
*                                                                               
INPROGRS BAS   RE,PRCINPT          GAME IN PROGRESS                             
*                                                                               
EXIT     BAS   RE,DISPLAY                                                       
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                         PRCINPT                                               
********************************************************************            
* PROCESS INPUT - WHICH DECK, WHICH DISCARD, CLAIMING GIN?                      
*                                                                               
PRCINPT  NTR1                                                                   
*                                                                               
* VALIDATE GIN FIELD, "GIN" OR NOTHING                                          
PRCGIN   CLI   GINGINH+5,0         SEE IF PLAYER CLAIMS GIN                     
         BE    PRCDK                                                            
         CLC   GINGIN(3),=C'GIN'                                                
         BE    PRCDK                                                            
         MVC   GINHEAD,ERRINPT     IF INVALID INPUT, COMPLAIN                   
         OI    GINGINH+6,X'C0'     CURSOR TO INVALID FIELD                      
         B     PRCDONE                                                          
*                                                                               
* DRAW CARD FROM PACK OR DISCARD PILE, OR CHANGE SORT TYPE                      
PRCDK    LH    R1,HILITED                                                       
         BAS   RE,PORD             ELSE PACK_OR_DISCARD_PILE?                   
         BNE   PRCDONE             AND EXIT                                     
*                                                                               
* VALIDATE DISCARD SELECTION                                                    
PRCDSC   BAS   RE,DSCSEL                                                        
         BE    PRCYESDC                                                         
*                                                                               
* IF NO DISCARD SELECTION MADE . . .                                            
         OI    GINDCSLH+6,X'C0'    CURSOR TO DISCARD SELECT                     
         OC    HILITED,HILITED     IS THERE A HIGHLIGHTED CARD?                 
         BNZ   PRCDONE                                                          
*                                                                               
* SORT CARDS AND HIGHLIGHT CARD JUST DRAWN                                      
         BAS   RE,ON8TH            DSPLAY CARD 8                                
         MVC   HILITED,=H'8'       COPY DRAWN CARD                              
         GOTO1 SAVNSORT,DMCB,PCARDS,HILITED,SORT,0                              
         GOTO1 TOGLCARD,HILITED                                                 
         B     PRCDONE                                                          
*                                                                               
* IF DISCARD SELECTION VALID . . .                                              
PRCYESDC OI    GINDKSLH+6,X'C0'    CURSOR TO DECK SELECT                        
         OC    HILITED,HILITED     IF THERE IS A HIGHLIGHTED CARD               
         BZ    PRCSKIP1                                                         
         GOTO1 TOGLCARD,HILITED                                                 
         XC    HILITED,HILITED       CLEAR THE HILITED CARD NUM                 
*                                                                               
PRCSKIP1 BAS   RE,SORT                                                          
         BAS   RE,OFF8TH             HIDE CARD 8                                
*                                                                               
* IF USER ENTERED GIN, VALIDATE HIS CLAIM                                       
         CLI   GINGINH+5,0           DID PLAYER SAY "GIN"?                      
         BE    COMPUTER                                                         
         XC    GINGIN,GINGIN             CLEAR CLAIM FIELD                      
         OI    GINGINH+6,X'80'           XMIT                                   
         LA    R1,PCARDS                                                        
         BAS   RE,CHKWIN               IF SO, DID HE WIN?                       
         BNE   PRCNOWIN                  DID HE WIN?                            
         MVC   GINHEAD,WINNER            WIN MSG                                
         BAS   RE,CSHOW                  SHOW COMPUTER'S HAND                   
         MVI   16(RA),0                  RESET NEW GAME FLAG                    
         OI    GININPTH+6,X'C0'           CURSOR TO INPUT FIELD                 
         B     PRCDONE                   EXIT                                   
*                                                                               
PRCNOWIN MVC   GINHEAD,NOTWIN            USER DID NOT WIN                       
*                                                                               
COMPUTER BAS   RE,CGO                                                           
         BE    PRCSHOW                                                          
         MVC   GINCHND,BLANKLN                                                  
         OI    GINCHNDH+6,X'80'    XMIT                                         
         CLC   GININPT(11),=C'PAPAMICHAEL'                                      
         BNE   PRCDONE                                                          
PRCSHOW  BAS   RE,CSHOW                                                         
*                                                                               
PRCDONE  XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                          DRAW                                                 
********************************************************************            
* MACRO THAT GETS THE NEXT CARD FROM THE DECK OR DISCARD PILE AND               
* PUTS IT INTO THE MEM LOC                                                      
*                                                                               
         MACRO                                                                  
&LABEL   DRAW  &FROM,&ADDR                                                      
&LABEL   LH    R1,&FROM.TOP                                                     
         SH    R1,=H'2'                                                         
         BNM   *+12                                                             
         BAS   RE,EO&FROM                                                       
         B     *-16                                                             
*                                                                               
         STH   R1,&FROM.TOP                                                     
         LH    R1,&FROM.(R1)                                                    
         STH   R1,&ADDR                                                         
         MEND                                                                   
         SPACE 10                                                               
********************************************************************            
*                          DSCRD                                                
********************************************************************            
* MACRO THAT PUTS THE CARD IN THE MEM LOC ONTO DISCARD PILE                     
*                                                                               
         MACRO                                                                  
&LABEL   DSCRD &ADDR                                                            
&LABEL   LH    R1,DSCPTOP                                                       
         LH    R0,&ADDR                                                         
         STH   R0,DSCP(R1)                                                      
         AH    R1,=H'2'                                                         
         STH   R1,DSCPTOP                                                       
         MEND                                                                   
         EJECT                                                                  
********************************************************************            
*                         TOGLCARD                                              
********************************************************************            
* TOGGLES THE HIGHLIGHTING OF THE CARD WHOSE NUMBER IS IN A                     
* HALFWORD POINTED TO BY R1                                                     
*                                                                               
TOGLCARD NTR1                                                                   
         LH    R1,0(R1)                                                         
         LA    R7,CARDLNQ                                                       
         LA    R6,CARD1ARY                                                      
         LA    R2,FDISPARY                                                      
         LA    R8,ATRBMASK                                                      
         BCTR  R1,0                DECR CARD NUM TO RESULT IN 0-7               
*                                                                               
TOGLOOP  L     R5,0(R2)            GET DSPLCMNT TO FIELD IN NEXT CARD           
         MR    R4,R1               MULTIPLY DISPLC BY CARD NUMBER               
         A     R5,0(R6)            ADD FIELD DISPLCMENT FOR CARD 1              
         AR    R5,RA               ADD ADDR OF TWA                              
         XC    1(1,R5),0(R8)       TOGGLE HIGHLIGHT                             
         OI    6(R5),X'80'         XMIT                                         
         LA    R6,4(R6)            BUMP ARRAY INDEX                             
         LA    R2,4(R2)                 DISP INDEX                              
         LA    R8,1(R8)                 ATTRIBUTE POINTER                       
         BCT   R7,TOGLOOP                                                       
*                                                                               
* TOGGLE ON/OFF '>' '<' ON EITHER SIDE OF CARD NUMBER                           
         L     R5,FDISPARY         GET DISPLCMENT TO NEXT CARD                  
         MR    R4,R1               X CARD NUMBER                                
         A     R5,CARD1ARY         + FIELD DISPLC FOR CARD 1                    
         AR    R5,RA               + TWZ                                        
         OC    8(3,R5),=C'   '     MAKE SURE THERE ARE SPACES                   
         XC    8(3,R5),=X'2E000C'  TOGGLE ON/OFF CHAR '>' '<'                   
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                         ON8TH                                                 
********************************************************************            
* MAKE ALL FIELDS OF THE 8TH CARD VISIBLE                                       
*                                                                               
ON8TH    NTR1                                                                   
         LA    R7,CARDLNQ                                                       
         LA    R6,CARD8ARY                                                      
*                                                                               
ON8LOOP  L     R1,0(R6)            GET ADDR FROM TABLE                          
         AR    R1,RA               ADD ADDR OF TWA                              
         NI    1(R1),X'F3'         NORMAL INTENSITY                             
         OI    6(R1),X'80'         XMIT                                         
         LA    R6,4(R6)            BUMP PTR                                     
         BCT   R7,ON8LOOP                                                       
*                                                                               
         OI    GINC8VAH+1,X'08'     CARD VALUES HI INTENSITY                    
         OI    GINC8VBH+1,X'08'                                                 
         XIT1                                                                   
         SPACE 10                                                               
********************************************************************            
*                         OFF8TH                                                
********************************************************************            
* MAKE ALL FIELDS OF THE 8TH CARD INVISIBLE                                     
*                                                                               
OFF8TH   NTR1                                                                   
         LA    R7,CARDLNQ                                                       
         LA    R6,CARD8ARY                                                      
*                                                                               
OFF8LOOP L     R1,0(R6)            GET ADDR FROM ARRAY                          
         AR    R1,RA               ADD ADDR OF TWA                              
         OI    1(R1),X'0C'         ZERO INTENSITY                               
         OI    6(R1),X'80'         XMIT                                         
         LA    R6,4(R6)            BUMP PTR                                     
         BCT   R7,OFF8LOOP                                                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                         PORD                                                  
********************************************************************            
* VALIDATE USER'S DECK SELECTION, P OR D AND DRAW CARD                          
* R1 SHOULD CONTAIN NON-ZERO IF PLAYER HAS ALREADY DRAWN                        
* CC IS SET TO EQUAL IF PLAYER CAN DISCARD                                      
*                                                                               
PORD     NTR1                                                                   
         LR    R8,R1                                                            
         CLI   GINDKSLH+5,0        ENTERED SOMETHING?                           
         BE    PORDTEST                                                         
         TM    GINDKSLH+4,X'80'    MODIFIED?                                    
         BZ    PORDTEST                                                         
*        OI    GINDKSLH+4,X'20'    SET PREVIOUSLY VALIDATED BIT                 
*                                                                               
PORDP    CLI   GINDKSL,C'P'        PACK?                                        
         BNE   PORDD                                                            
         LTR   R8,R8               DID HE ALREADY DRAW?                         
         BZ    PORDP1                                                           
         MVC   GINHEAD,DSCDMSG                                                  
         OI    GINDCSLH+6,X'C0'    CURSOR TO DISCARD SEL                        
         B     PORDNO                                                           
PORDP1   DRAW  DECK,PCARD8         DRAW FROM PACK                               
         B     PORDYES                                                          
*                                                                               
PORDD    CLI   GINDKSL,C'D'        DISCARD PILE?                                
         BNE   PORDS                                                            
         LTR   R8,R8               DID HE ALREADY DRAW?                         
         BZ    PORDD1                                                           
         MVC   GINHEAD,DSCDMSG                                                  
         OI    GINDCSLH+6,X'C0'    CURSOR TO DISCARD SEL                        
         B     PORDNO                                                           
PORDD1   DRAW  DSCP,PCARD8                                                      
         B     PORDYES                                                          
*                                                                               
PORDS    CLI   GINDKSL,C'S'        SEQ SORT?                                    
         BNE   PORDN                                                            
*                                                                               
         MVI   SORTTP,C'S'                                                      
         MVC   GINSORT,=CL8'SUIT'                                               
         OI    GINSORTH+6,X'80'    XMIT                                         
         OC    HILITED,HILITED     IF A CARD IS HILITED. . .                    
         BZ    PORDCUR                                                          
         GOTO1 TOGLCARD,HILITED    TOGGLE OFF OLD POSITION                      
         GOTO1 SAVNSORT,DMCB,PCARDS,HILITED,SORT,0                              
         GOTO1 TOGLCARD,HILITED    TOGGLE ON NEW POSITION                       
         B     PORDCUR                                                          
*                                                                               
PORDN    CLI   GINDKSL,C'N'        NUM SORT?                                    
         BNE   PORDERR                                                          
*                                                                               
         MVI   SORTTP,C'N'                                                      
         MVC   GINSORT,=C'NUMBER'                                               
         OI    GINSORTH+6,X'80'    XMIT                                         
         OC    HILITED,HILITED     IF A CARD IS HILITED. . .                    
         BZ    PORDCUR                                                          
         GOTO1 TOGLCARD,HILITED    TOGGLE OFF OLD POSITION                      
         GOTO1 SAVNSORT,DMCB,PCARDS,HILITED,SORT,0                              
         GOTO1 TOGLCARD,HILITED    TOGGLE ON NEW POSITION                       
         B     PORDCUR                                                          
*                                                                               
*                                                                               
PORDTEST LTR   R8,R8               SEE IF HE ALREADY DREW                       
         BNZ   PORDYES                                                          
*                                                                               
PORDERR  MVC   GINHEAD,ERRDECK                                                  
PORDCUR  OI    GINDKSLH+6,X'C0'    CURSOR TO DECK                               
PORDNO   CR    RB,RD               ON NOT P OR D, CC = NOT EQUAL                
         B     PORDEXIT                                                         
PORDYES  CR    R0,R0               ON P OR D, CC = EQUAL                        
PORDEXIT XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                           DSCSEL                                              
********************************************************************            
* VALIDATE USER'S DISCARD SELECTION, DISCARD IT.                                
*                                                                               
DSCSEL   NTR1                                                                   
         CLI   GINDCSLH+5,0        SELECTED CARD TO DISCARD?                    
         MVI   GINDCSLH+5,0        CLEAR INPUT FLAG                             
         BE    DSCCUR                                                           
         CLI   GINDCSL,C'1'        NUMBER BETWEEN 1-8?                          
         BL    DSCERR                                                           
         CLI   GINDCSL,C'8'                                                     
         BH    DSCERR                                                           
*                                                                               
         ZIC   R2,GINDCSL                                                       
         SH    R2,=X'00F1'         SUBTRACT CHAR '1'                            
         SLL   R2,1                *2 FOR 2BYTE CARDS                           
         LA    R2,PCARDS(R2)                                                    
         DSCRD 0(R2)                                                            
         XC    0(2,R2),0(R2)                                                    
         CR    R0,R0               ON INPUT, CC = EQUAL                         
         B     DSCDONE                                                          
                                                                                
DSCERR   MVC   GINHEAD,ERRSNUM     ELSE INVALID INPUT                           
         OI    GINHEADH+6,X'80'                                                 
DSCCUR   OI    GINDCSLH+6,X'C0'    CURSOR TO DISCARD SELECT                     
         CR    RB,RD               ON NO INPUT, CC = NOT EQUAL                  
DSCDONE  XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                         CHKNUMS                                               
********************************************************************            
* SETS UP FLAGS OF MUNBERS IN NFLAGS FOR THE HAND POINTED TO BY R1              
*                                                                               
CHKNUMS  NTR1                                                                   
         LR    R4,R1                                                            
         LA    R5,X'80'            CONSEQUTIVE TWO CARD MASK                    
         SR    R3,R3               FLAGS                                        
         LA    R6,NFLAGS                                                        
         XC    NFLAGS(14),NFLAGS                                                
*                                                                               
CNREINIT LR    R2,R5               SET UP SECOND MASK                           
         SRL   R2,1                OVER NEXT CARD                               
         LA    R8,2(R4)                                                         
*                                                                               
CNLOOP   CLC   1(1,R4),1(R8)       CARDS HAVE SAME NUMBER?                      
         BNE   CNSHIFT                                                          
         OR    R3,R5               FLAG BOTH CARDS                              
         OR    R3,R2                                                            
*                                                                               
CNSHIFT  LA    R8,2(R8)            BUMP SECOND CARD POINTER                     
         SRL   R2,1                ADJUST SECOND CARD MASK                      
         LTR   R2,R2               AT END OF CARDS?                             
         BNZ   CNLOOP                                                           
*                                                                               
         STC   R3,0(R6)            STORE FLAGS                                  
         LR    R1,R3                                                            
         BAS   RE,CNTBITS                                                       
         STC   R1,1(R6)                                                         
         LA    R6,2(R6)            BUMP FLAGS BYTE POINTER                      
         SR    R3,R3                                                            
         LA    R4,2(R4)            BUMP FIRST CARD POINTER                      
         SRL   R5,1                ADJUST FIRST MASK                            
         CH    R5,=H'1'            AT END OF CARDS?                             
         BH    CNREINIT                                                         
*                                                                               
CNDONE   XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                         CHKSUITS                                              
********************************************************************            
* SETS UP FLAGS OF SEQUENCES IN SFLAGS FOR THE HAND POINTED TO BY R1            
*                                                                               
CHKSUITS NTR1                                                                   
         LR    R4,R1                                                            
         LA    R5,X'C0'            CONSEQUTIVE TWO CARD MASK                    
         SR    R3,R3               FLAGS                                        
         LA    R6,SFLAGS                                                        
         XC    SFLAGS(14),SFLAGS                                                
         LA    R7,3                COUNT TO FOUR LEN SEQ MAX                    
*                                                                               
CSLOOP   LH    R2,0(R4)            COPY CARD                                    
         BCTR  R2,0                DECR FOR COMPARE                             
         CH    R2,2(R4)                                                         
         BNE   CSNEXT                                                           
         OR    R3,R5               FLAG BOTH CARDS                              
*        SH    R7,=H'1'            IF LENGTH OF SEQ REACHED 4, NEXT             
*        BZ    CSNEXT                                                           
         CH    R5,=X'0003'          B'00000011'                                 
         BH    CSHIFT                                                           
*                                                                               
CSNEXT   STC   R3,0(R6)            STORE FLAGS                                  
         LR    R1,R3                                                            
         BAS   RE,CNTBITS                                                       
         STC   R1,1(R6)                                                         
         LA    R6,2(R6)            BUMP FLAGS BYTE POINTER                      
         SR    R3,R3                                                            
         LA    R7,3                COUNT TO FOUR LEN SEQ MAX                    
*                                                                               
CSHIFT   LA    R4,2(R4)            BUMP CARD POINTER                            
         SRL   R5,1                ADJUST MASK                                  
         CH    R5,=H'1'            DONE?                                        
         BH    CSLOOP                                                           
*                                                                               
CSDONE   XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                           CHKWIN                                              
********************************************************************            
* CHECKS THE HAND POINTED TO BY R1 TO SEE IF ITS GIN                            
* SETS UP SUITS AND NUM FLAGS                                                   
*                                                                               
CHKWIN   NTR1                                                                   
         MVC   SCRAP(16),0(R1)                                                  
         GOTO1 =V(XSORT),DMCB,(1,SCRAP),8,2,2,0,RR=RELO                         
         LA    R1,SCRAP                                                         
         BAS   RE,CHKNUMS                                                       
         BAS   RE,CHKSUITS                                                      
         LA    R6,NFLAGSX          END OF NUM FLAGS                             
         LA    R2,SFLAGS           OUTER INDEX: START OF SUITS FLAGS            
CWLOOP01 LA    R3,2(R2)            INNER INDEX: OUTER+1                         
CWLOOP02 ZIC   R1,1(R2)            IS LEN OF SEQ 3 OR MORE?                     
         CH    R1,=H'3'            IF NOT, NO GOOD                              
         BL    CWNEXT02              necessary to disallow 5&2 seq win          
         ZIC   R0,1(R3)                                                         
         CH    R0,=H'3'                                                         
         BL    CWNEXT02                                                         
*                                                                               
         ZIC   R1,0(R2)            GET ONE SET OF FLAGS                         
         ZIC   R5,0(R3)            GET ANOTHER                                  
         OR    R1,R5               IF TOGETHER THEY INCLUDE 7 CARDS             
         BAS   RE,CNTBITS                                                       
         CH    R1,=H'7'                                                         
         BNL   CWWIN               . . . THEN WIN                               
CWNEXT02 LA    R3,2(R3)            BUMP INNER INDEX                             
         CR    R3,R6               DONE?                                        
         BL    CWLOOP02                                                         
*                                                                               
         LA    R2,2(R2)            BUMP OUTER INDEX                             
         CR    R2,R6                                                            
         BL    CWLOOP01                                                         
         CR    RB,RD               ON NO WIN, RETURN CC = NOT EQUAL             
         B     CWDONE                                                           
*                                                                               
CWWIN    CR    R0,R0               ON WIN, RETURN CC = EQUAL                    
CWDONE   XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                         CNTBITS                                               
********************************************************************            
* COUNT THE NUMBER OF 1 BITS IN THE LOW ORDER BYTE OF R1 AND RETURN             
* NUMBER IN R1                                                                  
CNTBITS  NTR1                                                                   
         SLL   R1,24                                                            
         SR    R2,R2                                                            
         LA    R3,8                                                             
*                                                                               
         SR    R0,R0                                                            
         SLDL  R0,1                                                             
         AR    R2,R0                                                            
         BCT   R3,*-8                                                           
*                                                                               
         LR    R1,R2                                                            
         XIT1  REGS=(R1)                                                        
         EJECT                                                                  
********************************************************************            
*                         CARDCNT                                               
********************************************************************            
* PUT THE SIZE OF THE LONGEST SEQ TO WHICH EACH CARD BELONGS IN CARDNF          
CARDCNT  NTR1                                                                   
         LA    R6,NFLAGSX          END OF FLAGS                                 
         LA    R5,SFLAGS                                                        
         XC    CARDNF(8),CARDNF                                                 
*                                                                               
CCLP01   IC    R1,0(R5)                                                         
         LA    R7,CARDNF                                                        
         SLL   R1,24                                                            
         LA    R3,8                                                             
*                                                                               
CCLP02   SR    R0,R0               FOR EACH BIT THAT IS ON . . .                
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BZ    CCNEXT02                                                         
         CLC   1(1,R5),0(R7)       CARDNF = MAX(CARDNF, THIS_SEQ)               
         BNH   CCNEXT02                                                         
         MVC   0(1,R7),1(R5)                                                    
CCNEXT02 LA    R7,1(R7)                                                         
         BCT   R3,CCLP02                                                        
*                                                                               
         LA    R5,2(R5)            BUMP FLAGS PTR                               
         CR    R5,R6                                                            
         BL    CCLP01                                                           
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                          ORFLAGS                                              
********************************************************************            
* ORS TOGETHER ALL THE FLAGS AND RETURNS RESULT IN BYTE POINTED TO              
* BY R1                                                                         
*                                                                               
ORFLAGS  NTR1                                                                   
         LA    R6,NFLAGSX          END OF NUM FLAGS                             
         LA    R2,SFLAGS           INDEX: START OF SUITS FLAGS                  
         SR    R4,R4                                                            
*                                                                               
* OR TOGETHER ALL THE FLAG BYTES                                                
OFLOOP01 ZIC   R5,0(R2)                                                         
         OR    R4,R5                                                            
         LA    R2,2(R2)            BUMP INDEX                                   
         CR    R2,R6               DONE?                                        
         BL    OFLOOP01                                                         
*                                                                               
         STC   R4,0(R1)                                                         
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                          SAVNSORT                                             
********************************************************************            
* SORT THE HAND AND RETURN NUMBER OF CARD WHOSE VALUE IS IN PARAM1              
* PARAM1 - A(8H CONTAINING HAND)                                                
* PARAM2 - A(NUM OF CARD TO SAVE. INPUT AND OUTPUT)                             
* PARAM3 - A(SORT ROUTINE)                                                      
* PARAM4 - RELOAD FACTOR                                                        
*                                                                               
SAVNSORT NTR1                                                                   
         LM    R3,R6,0(R1)                                                      
         LH    R7,0(R4)                                                         
         BCTR  R7,0                                                             
         SLL   R7,1                                                             
         AR    R7,R3                                                            
         LH    R7,0(R7)                                                         
         GOTO1 (R5),DMCB,(1,0(R3)),8,2,2,0,RR=R6                                
         LA    R2,14(R3)           ADDR OF LAST CARD                            
         LA    R1,8                8 CARDS                                      
*                                                                               
* FIND DRAWN CARD IN SORTED HAND                                                
SSLP01   CH    R7,0(R2)            DRAWN CARD?                                  
         BE    SSOUT01                                                          
         SH    R2,=H'2'            DECR POINTER                                 
         BCT   R1,SSLP01                                                        
SSOUT01  STH   R1,0(R4)            SAVE NUMBER OF CARD                          
         XIT1                                                                   
         SPACE 10                                                               
********************************************************************            
*                          SORT                                                 
********************************************************************            
*                                                                               
SORT     NTR1                                                                   
         CLI   SORTTP,C'S'                                                      
         BNE   SORTNUM                                                          
         GOTO1 =V(XSORT),DMCB,(1,PCARDS),8,2,2,0,RR=RELO                        
         B     SORTDONE                                                         
SORTNUM  GOTO1 =V(XSORT),DMCB,(1,PCARDS),8,2,1,1,RR=RELO                        
SORTDONE XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                          CGO                                                  
********************************************************************            
* COMPUTER'S TURN, SET CC TO EQUAL IF COMPUTER WON, ELSE NON-EQUAL              
*                                                                               
CGO      NTR1                                                                   
*                                                                               
* PICKUP DISCARD AND SEE IF IT IS BENEFICIAL TO OUR HAND                        
         DRAW  DSCP,CCARD8                                                      
         MVC   WHAT8,CCARD8                                                     
         MVC   WHERE8,=H'8'                                                     
         GOTO1 SAVNSORT,DMCB,CCARDS,WHERE8,V(XSORT),RELO                        
         GOTO1 CHKWIN,CCARDS                                                    
         BNE   *+10                                                             
         LR    R3,R1                                                            
         B     CGDSC                                                            
*                                                                               
         GOTO1 ORFLAGS,ORED                                                     
         ZIC   R2,ORED                                                          
         LA    R1,9                INVERT 1-8 -> 8-1                            
         SH    R1,WHERE8                                                        
*                                                                               
* SEE IF BIT REPRESENTING DISCARD IS 0.  IF SO, PUT IT BACK                     
         SR    R3,R3                                                            
         SRDL  R2,1                                                             
         BCT   R1,*-6                                                           
*                                                                               
         LTR   R3,R3               IF BIT=0, DISCARD WAS USELESS                
         BNZ   CGDSC                                                            
*                                                                               
* REPLACE DISCARD CHOSEN WITH CARD FROM PACK                                    
CGRPLDSC LH    R4,WHERE8                                                        
         BCTR  R4,0                1-8 -> 0-7                                   
         SLL   R4,1                2 BYTE CARDS                                 
         LA    R4,CCARDS(R4)                                                    
         DSCRD 0(R4)                                                            
         DRAW  DECK,0(R4)                                                       
         GOTO1 =V(XSORT),DMCB,(1,CCARDS),8,2,2,0,RR=RELO                        
         GOTO1 CHKWIN,CCARDS                                                    
         BNE   *+10                                                             
         LR    R3,R1                                                            
         B     CGDSC                                                            
*                                                                               
         GOTO1 ORFLAGS,ORED                                                     
         MVI   WHERE8,255          MARK WHAT WE'VE DONE                         
*                                                                               
* FIND A CARD WHOSE BIT IS ZERO AND DISCARD IT                                  
CGDSC    LA    R3,8                                                             
         ZIC   R4,ORED                                                          
CGLOOP02 SR    R5,R5                                                            
         SRDL  R4,1                                                             
         LTR   R5,R5               IF BIT SHIFTED=0 THEN CARD USELESS           
         BZ    CGDSC02                                                          
         BCT   R3,CGLOOP02                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
* IF WE MADE IT HERE, NONE OF THE CARDS IS COMPLETELY USELESS                   
         BAS   RE,CARDCNT          FIND SIZE OF SEQ FOR EACH CARD               
*                                                                               
* . SEE IF WE ARE HOLDING CARD FROM DISCARD AND IF IT IS IN 2CARD SEQ           
         CLI   WHERE8,255          IF DISCARD NOT REPLACED TO PILE              
         BE    CGSEQSZ                                                          
         LH    R1,WHERE8                                                        
         BCTR  R1,0                1-8 -> 0-7                                   
         LA    R1,CARDNF(R1)                                                    
         CLI   0(R1),2             IF DISCARD IS PART OF A 2CARD SEQ            
         BE    CGRPLDSC            THEN PUT IT BACK ON PILE                     
*                                                                               
* . FIND A CARD BELONGING TO THE SMALLEST SEQ (TIES BROKEN RANDOMLY)            
CGSEQSZ  LA    R4,CARDNF           CURRENT CARD                                 
         LA    R3,CARDNF           DEFAULT CARD FOR MIN                         
         LA    R6,8                8 CARDS                                      
*                                                                               
CGLOOP03 CLC   0(1,R4),0(R3)       R5=MIN(CURRENT_SEQ, MIN_SEQ)                 
         BH    CGNEXT03                                                         
         BL    CGDOIT03                                                         
         LA    R1,2                FLIP A COIN IF EQUAL                         
         BAS   RE,RANDCRDN                                                      
         LTR   R1,R1                                                            
         BZ    CGNEXT03                                                         
CGDOIT03 LR    R3,R4                                                            
CGNEXT03 LA    R4,1(R4)                                                         
         BCT   R6,CGLOOP03                                                      
*                                                                               
         LA    R0,CARDNF-1                                                      
         SR    R3,R0                                                            
         CH    R3,WHERE8           IF CARD FROM DISCARD PILE. . .               
         BE    CGRPLDSC            . . . REPLACE W/ CARD FROM PACK              
         SPACE 5                                                                
*                                                                               
* WE HAVE THE NUMBER OF A CARD TO DISCARD IN R3                                 
CGDSC02  CLI   WHERE8,255          IF KEEPING CARD FROM DISCARD PILE            
         BE    CGSKIP                                                           
*                                                                               
         MVC   GINHEAD,ITOOKMSG    NOTIFY PLAYER                                
         ZIC   R2,WHAT8+1                                                       
         MH    R2,VALSLEN                                                       
         LA    R2,VALS(R2)                                                      
         MVC   GINHEAD+ITVQ(L'VALS),0(R2)                                       
         ZIC   R2,WHAT8                                                         
         MH    R2,TXTSTLEN                                                      
         LA    R2,TXTSUITS(R2)                                                  
         MVC   GINHEAD+ITSQ(L'TXTSUITS),0(R2)                                   
*                                                                               
CGSKIP   BCTR  R3,0                                                             
         SLL   R3,1                X 2 FOR 2 BYTE CARDS                         
         LA    R3,CCARDS(R3)                                                    
         DSCRD 0(R3)                                                            
         XC    0(2,R3),0(R3)                                                    
         GOTO1 =V(XSORT),DMCB,(1,CCARDS),8,2,2,0,RR=RELO                        
         B     CGDONE                                                           
*                                                                               
COMPWINS MVC   GINHEAD,CWINNER     MSG THAT COMPUTER WON                        
         OI    GININPTH+6,X'C0'    CURSOR TO INPUT FIELD                        
         XC    CCARD8,CCARD8       CLEAR 8TH CARD                               
         BAS   RE,CSHOW            SHOW COMPUTER'S HAND                         
         MVI   16(RA),1            SET NEW GAME FLAG                            
         CR    R0,R0               SET CC = EQUAL                               
         B     CGEXIT                                                           
*                                                                               
CGDONE   GOTO1 CHKWIN,CCARDS                                                    
         BE    COMPWINS                                                         
CGEXIT   XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                          CSHOW                                                
********************************************************************            
* DISPLAY COMPUTER'S HAND                                                       
*                                                                               
CSHOW    NTR1                                                                   
         MVC   GINCHND(10),=C'MY HAND ->'                                       
         LA    R5,GINCHND+11                                                    
         LA    R6,CCARDS                                                        
         LA    R7,7                                                             
*                                                                               
SHOWLP   ZIC   R2,0(R6)                                                         
         LA    R2,SHRTSUIT(R2)                                                  
         MVC   0(1,R5),0(R2)       DISPLAY SUIT                                 
         ZIC   R2,1(R6)                                                         
         SLL   R2,1                X 2 FOR 2 CHARS                              
         LA    R2,VALS(R2)                                                      
         MVC   1(2,R5),0(R2)       DISPLAY VALUE                                
         LA    R5,4(R5)            BUMP SCREEN POINTER                          
         LA    R6,2(R6)                 CARD POINTER                            
         BCT   R7,SHOWLP                                                        
*                                                                               
         OI    GINCHNDH+6,X'80'    XMIT                                         
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                           DISPLAY                                             
********************************************************************            
DISPLAY  NTR1                                                                   
*                                                                               
* DISPLAY DISCARDED CARD                                                        
         LH    R2,DSCPTOP                                                       
         ZIC   R1,DSCP-2(R2)       GET SUIT                                     
         MH    R1,SUITSLEN                                                      
         LA    R1,SUITS(R1)                                                     
         MVC   GINDCS(L'SUITS),0(R1)                                            
         OI    GINDCSH+6,X'80'     XMIT                                         
*                                                                               
         ZIC   R1,DSCP-1(R2)       GET VALUE                                    
         MH    R1,VALSLEN                                                       
         LA    R6,VALS(R1)                                                      
         MVC   GINDCVA(L'VALS),0(R6)                                            
         LA    R6,VALS2(R1)                                                     
         MVC   GINDCVB(L'VALS2),0(R6)                                           
         OI    GINDCVAH+6,X'80'                                                 
         OI    GINDCVBH+6,X'80'                                                 
*                                                                               
* UPDATE PLAYER'S 8 CARDS (NOT CHANGING VISIBILITY OF 8TH CARD)                 
         LA    R7,GINC1SH          SUITS SCREEN POINTER                         
         LA    R2,GINC1VAH         VALUE SCREEN POINTERS                        
         LA    R3,GINC1VBH                                                      
         LA    R5,8                8 CARDS                                      
         LA    R4,PCARDS                                                        
         BAS   RE,SORT                                                          
*                                                                               
DISPLOOP ZIC   R1,0(R4)            GET SUIT                                     
         MH    R1,SUITSLEN         GET DISPLACEMENT                             
         LA    R1,SUITS(R1)        ADD ADDRESS OF CHAR REPS                     
         MVC   8(L'SUITS,R7),0(R1)                                              
         OI    6(R7),X'80'         XMIT                                         
         ZIC   R0,0(R7)            BUMP SUITS SCREEN POINTER                    
         AR    R7,R0                                                            
                                                                                
         ZIC   R1,1(R4)            GET VALUE                                    
         MH    R1,VALSLEN                                                       
         LA    R6,VALS(R1)         ADD ADDRESS OF CHAR REPS                     
         MVC   8(L'VALS,R2),0(R6)  COPY CHAR REP TO CARD                        
         LA    R6,VALS2(R1)        ADD ADDRESS OF CHAR REPS                     
         MVC   8(L'VALS,R3),0(R6)                                               
*                                                                               
         OI    6(R2),X'80'         XMIT                                         
         OI    6(R3),X'80'                                                      
         LA    R4,2(R4)            BUMP CARD POINTER                            
         LA    R2,GINC2VAH-GINC1VAH(R2) SCREEN POINTERS                         
         LA    R3,GINC2VBH-GINC1VBH(R3)                                         
         BCT   R5,DISPLOOP                                                      
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*                          SHUFFLE                                              
*********************************************************************           
* SHUFFLE CARDS POINTED IN DECK WHERE DECKTOP POINTS TO THE TOP                 
* SWAPPING EACH CARD WITH A RANDOM OTHER                                        
*                                                                               
SHUFFLE  NTR1                                                                   
         LA    R2,DECK                                                          
         LH    R4,DECKTOP                                                       
         AR    R4,R2               ADDRESS OF TOP OF DECK                       
         LH    R6,DECKTOP                                                       
         SRL   R6,1                NUMBER OF CARDS TO SHUFFLE                   
SHUFLOOP LR    R1,R6               N CARDS                                      
         BAS   RE,RANDCRDN                                                      
         SLL   R1,1                X2 (2 BYTES PER CARD)                        
         LA    R3,DECK(R1)         GET ADDRESS OF RANDOM CARD                   
*                                                                               
*        MVC   DECKLEN,=H'104'                                                  
*        GOTO1 VDATAMGR,DMCB,=C'DMTRACE',=C'DATA',DECKLEN                       
*                                                                               
         CR    R3,R2                                                            
         BE    SKIPSWAP                                                         
         XC    0(2,R3),0(R2)       SWAP                                         
         XC    0(2,R2),0(R3)                                                    
         XC    0(2,R3),0(R2)                                                    
*                                                                               
SKIPSWAP LA    R2,2(R2)                                                         
         CR    R2,R4               END OF DECK?                                 
         BL    SHUFLOOP                                                         
         XIT1                                                                   
         SPACE 10                                                               
*******************************************************************             
*                          RANDCRDN                                             
*******************************************************************             
* RETURN IN R1 A RANDOM NUMBER BETWEEN 0 AND CONTENTS OF R1 -1                  
RANDCRDN NTR1                                                                   
         L     R3,RN                                                            
         M     R2,=F'65541'                                                     
         ST    R3,RN                                                            
         MR    R2,R1                                                            
         SRL   R1,1                DIV 2                                        
         AR    R1,R2                                                            
         XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*********************************************************************           
*                            DEAL                                               
*********************************************************************           
* CREATE A STANDARD DECK IN "DECK" AND SHUFFLE THEM                             
* THEN DEAL PLAYER AND COMPUTER 7 CARDS, AND ONE FOR DISCARD                    
*                                                                               
DEAL     NTR1                                                                   
*                                                                               
* INITIALIZE A DECK WITH ALL THE CARDS                                          
         LA    R5,DECK                                                          
         LA    R3,SPADQ            HIGHEST SUIT                                 
NEXTSUIT LA    R2,13               NUMBER OF CARDS PER SUIT                     
NEXTCARD STC   R3,0(R5)            STORE CARD SUIT                              
         STC   R2,1(R5)            STORE CARD VALUE                             
         LA    R5,2(R5)            BUMP CARD POINTER                            
         BCT   R2,NEXTCARD                                                      
         BCT   R3,NEXTSUIT         NEXT HIGHEST SUIT                            
*                                                                               
         MVC   DECKTOP,=H'104'     NUM CARDS REMAINING * 2                      
         BAS   RE,SHUFFLE                                                       
         XC    DSCPTOP,DSCPTOP                                                  
         DRAW  DECK,HALF           PUT A CARD ON THE DISCARD PILE               
         DSCRD HALF                                                             
*                                                                               
* DEAL CARDS                                                                    
         LA    R2,12               (7 CARDS -1) * 2BYTES                        
         LA    R3,PCARDS                                                        
         LA    R4,CCARDS                                                        
DEALLOOP DRAW  DECK,0(R3,R2)       ONE TO PLAYER                                
         DRAW  DECK,0(R4,R2)       ONE TO COMPUTER                              
         SH    R2,=H'2'                                                         
         BNM   DEALLOOP                                                         
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*                         EODECK                                                
********************************************************************            
EODECK   NTR1                                                                   
         LH    R3,DSCPTOP                                                       
         SH    R3,=H'2'            DON'T PUT TOP DISCARD INTO DECK              
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STH   R3,DECKTOP          SAVE NEW DECKTOP                             
         BCTR  R3,0                DECR FOR ...                                 
         EX    R3,*+8              ... EXECUTED MOVE                            
         B     *+10                                                             
         MVC   DECK(0),DSCP                                                     
*                                                                               
         BAS   RE,SHUFFLE                                                       
         LA    R3,1(R3)            UNDO DECR WE DID FOR EX                      
         LA    R3,DSCP(R3)         GET LAST DISCARD                             
         MVC   DSCP(2),0(R3)       COPY TOP OF DSCPILE TO FIRST CARD            
         MVC   DSCPTOP,=H'2'                                                    
*                                                                               
         MVC   GINHEAD,SHUFMSG                                                  
         XIT1                                                                   
         SPACE 10                                                               
********************************************************************            
*                         EODSCP                                                
********************************************************************            
* THIS SHOULD NEVER GET CALLED!  IF IT DOES, THAT MEANS WE DREW                 
* BEYOND THE BOTTOM OF THE DISCARD PILE.                                        
EODSCP   DC    H'0'                                                             
         EJECT                                                                  
********************************************************************            
*                           CONSTANTS                                           
********************************************************************            
CLUBQ    EQU   1                                                                
DMNDQ    EQU   2                                                                
HARTQ    EQU   3                                                                
SPADQ    EQU   4                                                                
         DS    0H                                                               
VALSLEN  DC    AL2(L'VALS)                                                      
SUITSLEN DC    AL2(L'SUITS)                                                     
TXTSTLEN DC    AL2(L'TXTSUITS)                                                  
*                                                                               
SUITS    DC    C'º       º'                                                     
         DC    C'º % C % º'                                                     
         DC    C'º + D + º'                                                     
         DC    C'º @ H @ º'                                                     
         DC    C'º $ S $ º'                                                     
SHRTSUIT DC    C' %+@$'                                                         
TXTSUITS DC    CL8' '                                                           
         DC    CL8'clubs'                                                       
         DC    CL8'diamonds'                                                    
         DC    CL8'hearts'                                                      
         DC    CL8'spades'                                                      
*                                                                               
VALS     DS    0CL2                TEXT VALUES FOR UPPER-LEFT                   
         DC    C'  2 3 4 5 6 7 8 9 10J Q K A '                                  
VALS2    DS    0CL2                TEXT VALUES FOR LOWER-RIGHT                  
         DC    C'   2 3 4 5 6 7 8 910 J Q K A'                                  
*                                                                               
* MESSAGES                                                                      
BLANKLN  DC    CL60' '                                                          
ERRDECK  DC    CL60'Please enter ''P'',''D'',''S'' or ''N'' here'               
ERRSNUM  DC    CL60'Please enter ''1''-''8'' here'                              
ERRINPT  DC    CL60'INVALID INPUT'                                              
DSCDMSG  DC    CL60'MUST DISCARD FIRST!'                                        
WINNER   DC    CL60'CONGRATULATIONS!  YOU WON!  Enter to restart.'              
NOTWIN   DC    CL60'You did not win.  Please continue.'                         
CWINNER  DC    CL60'GIN!  I WIN!  Enter to restart.'                            
SHUFMSG  DC    CL60'We ran out of cards.  I reshuffled...'                      
ITOOKMSG DC    C'I took from the discard pile the    of '                       
ITVQ     EQU   33                  WHERE IN ITOOKMSG FOR VAL                    
ITSQ     EQU   39                  WHERE IN ITOOKMSG FOR SUIT                   
*                                                                               
* MASK FOR TURNING ON AND OFF HIGHLIGHTING FOR CERTAIN FIELDS                   
* IF A FIELD IS ALWAYS HIGHLIGHTED, ITS BYTE IS 00 ELSE 08                      
ATRBMASK DC    (CARDLNQ)X'080808000808080808000808'                             
*                                                                               
CARDLNQ  EQU   12                  NUMBER OF FIELDS                             
* ARRAY OF ALL FIELDS FOR 1ST CARD                                              
CARD1ARY DC    A(GINNUM1H-TB1DFFD)                                              
         DC    A(GINC1#1H-TB1DFFD)                                              
         DC    A(GINC1#2H-TB1DFFD)                                              
         DC    A(GINC1VAH-TB1DFFD)                                              
         DC    A(GINC1#4H-TB1DFFD)                                              
         DC    A(GINC1#5H-TB1DFFD)                                              
         DC    A(GINC1SH-TB1DFFD)                                               
         DC    A(GINC1#7H-TB1DFFD)                                              
         DC    A(GINC1#8H-TB1DFFD)                                              
         DC    A(GINC1VBH-TB1DFFD)                                              
         DC    A(GINC1#AH-TB1DFFD)                                              
         DC    A(GINC1#BH-TB1DFFD)                                              
*                                                                               
* ARRAY OF ALL FIELDS FOR 8TH CARD                                              
CARD8ARY DC    A(GINNUM8H-TB1DFFD)                                              
         DC    A(GINC8#1H-TB1DFFD)                                              
         DC    A(GINC8#2H-TB1DFFD)                                              
         DC    A(GINC8VAH-TB1DFFD)                                              
         DC    A(GINC8#4H-TB1DFFD)                                              
         DC    A(GINC8#5H-TB1DFFD)                                              
         DC    A(GINC8SH-TB1DFFD)                                               
         DC    A(GINC8#7H-TB1DFFD)                                              
         DC    A(GINC8#8H-TB1DFFD)                                              
         DC    A(GINC8VBH-TB1DFFD)                                              
         DC    A(GINC8#AH-TB1DFFD)                                              
         DC    A(GINC8#BH-TB1DFFD)                                              
*                                                                               
* DISPLACEMENTS FROM EACH FIELD IN A CARD TO CORRESPONDING FIELD IN             
* NEXT CARD                                                                     
FDISPARY DC    A(GINNUM2H-GINNUM1H)                                             
         DC    A(GINC2#1H-GINC1#1H)                                             
         DC    A(GINC2#2H-GINC1#2H)                                             
         DC    A(GINC2VAH-GINC1VAH)                                             
         DC    A(GINC2#4H-GINC1#4H)                                             
         DC    A(GINC2#5H-GINC1#5H)                                             
         DC    A(GINC2SH-GINC1SH)                                               
         DC    A(GINC2#7H-GINC1#7H)                                             
         DC    A(GINC2#8H-GINC1#8H)                                             
         DC    A(GINC2VBH-GINC1VBH)                                             
         DC    A(GINC2#AH-GINC1#AH)                                             
         DC    A(GINC2#BH-GINC1#BH)                                             
*                                                                               
*                                                                               
RN       DC    F'987654301'        RANDOM NUMBER SEED                           
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*                           WORKSPACE                                           
********************************************************************            
GINWRK   DSECT                                                                  
JUNK     DS    200F                                                             
DUB      DS    D                                                                
RELO     DS    A                                                                
VDATAMGR DS    V                                                                
HEXOUT   DS    V                                                                
DMCB     DS    6F                                                               
HALF     DS    H                                                                
WORK     DS    17C                                                              
SCRAP    DS    16X                                                              
*                                                                               
SFLAGS   DS    7H                  7 SETS OF BIT FLAGS FOR SEQ ON SUITS         
SFLAGSX  DS    0X                     2ND BYTE IS LENGTH OF SEQUENCE            
NFLAGS   DS    7H                  7 SETS OF BIT FLAGS FOR SEQ ON NUM           
NFLAGSX  DS    0X                                                               
ORED     DS    X                   WORKSPACE FOR ORED FLAGS                     
WHERE8   DS    H                   NUMBER OF CARD AFTER SORT                    
WHAT8    DS    H                   CARD FROM DISCARD PILE                       
CARDNF   DS    8X                  SIZES OF LONGEST SEQ W/ EACH CARD            
*                                                                               
GINWRKX  EQU   *                                                                
         EJECT                                                                  
********************************************************************            
*                              TWA                                              
********************************************************************            
         PRINT OFF                                                              
       ++INCLUDE GAGINFFD                                                       
         PRINT ON                                                               
*                                                                               
DECKLEN  DS    H                                                                
DECK     DS    52H                 PLAYING DECK                                 
DECKX    DS    0H                                                               
DECKTOP  DS    H                   DISPLACEMENT FROM DECK TO TOP O'DECK         
         DS    H                                                                
DSCP     DS    38H                 DISCARD PILE                                 
DSCPTOP  DS    H                   DISPLACEMENT FROM DSCPILE TO TOP             
PCARDS   DS    7H                  PLAYERS HAND                                 
PCARD8   DS    H                   PLAYER'S EIGHTH CARD                         
CCARDS   DS    7H                  COMPUTER'S HAND                              
CCARD8   DS    H                   COMPUTER'S EIGHTH CARD                       
HILITED  DS    H                   POSITION OF DRAWN CARD IN HAND               
SORTTP   DS    C                   TYPE OF CURRENT SORT                         
********************************************************************            
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'124GAGIN00   08/22/00'                                      
         END                                                                    
