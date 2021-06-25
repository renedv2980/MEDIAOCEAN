*          DATA SET PRREQ02S   AT LEVEL 098 AS OF 05/01/02                      
*PHASE T41202A,+0,NOAUTO                                                        
*INCLUDE GETPROF                                                                
*INCLUDE REQTWA                                                                 
         TITLE 'PRREQ02- CHANGE LOG'                                            
*                                                                               
* KWAN 4/99      CORRECT Y2K DISPLAY PROBLEM FOR DATES IN REQUEST               
*                                                                               
* SMYE 1/99      ADD WR TO WESTERN AGENCY LIST                                  
*                                                                               
* BPLA  9/98     ALLOW SOON REQUESTS FOR P46 AND P48 BY PUBLISHER               
*                                                                               
* BPLA  9/98     IN VALR14 AND VALR18, IF ALL PUBLISHER REQUEST                 
*                (P=ALL IN RPUB+1)  AND RSORT IS BLANK -SET IT TO 01            
*                LIKE WAS DONE FOR P19                                          
*                NEWREQ MOVED TO ITS OWN CSECT                                  
*                ENTRIES FOR MEDIA "I" ADDED TO SFORMTAB                        
*                                                                               
* BPLA 9/98      MINOR CHNAGE IN LK PROFILE CHECKING                            
*                                                                               
* SMYE 8/10/98   VALR10 - ALLOW MED * (ALL MEDIA) FOR AGENCY DF                 
*                  (SAATCHI) ONLY IF T IN RO4 (TAPE REQUEST)                    
*                                                                               
* SMYE 6/98      VALR19 - FOR PUBLISHER REQUEST P=ALL SET SORT TO               
*                  01 IF SORT NOT REQUESTED                                     
*                  ALSO ALLOW SOON FOR P=NNNN (ONE PUBLISHER REQUEST)           
*                                                                               
* SMYE 5/26/98   VALR48 - DISALLOW OFFICE (*N) ENTRY IN CLIENT                  
*                  IF CLIENT-ACTIVITY FILTER SPECIFIED                          
*                                                                               
* SMYE 4/03/98   EDIT FOR ENTRY IN PUB FIELD IN VALR46                          
*                                                                               
* BPLA 1/98      SJR NO LONGER CONSIDERED A WESTERN AGENCY                      
*                                                                               
* BPLA 5/97      REMOVE 3 MONTH SOON RESTRICTION FOR WESTERN                    
*                P52'S AND EC'S                                                 
*                                                                               
* BPLA 3/97      ADD MX TO WESTERN AGENCY LIST                                  
*                                                                               
* BPLA 4/96      IN VALR110 IF DIVISION WAS ENTERED - LEAVE IT                  
*                ALONE (EVEN IF CLIENT HAS DRD OVERRIDE)                        
*                                                                               
* SMYE 4/18/96   ADDED TEST FOR PUBLISHER TO VALR48                             
*                                                                               
* BPLA 2/22/95   ADD POST VALIDATION ROUTINE FOR Z5 (VALR228)                   
*                                                                               
* BPLA 10/6/94   P12 FAX OPTION MOVED TO RPAY (COL53)                           
*                FROM RO1-1 (COL 61)                                            
*                                                                               
* BPLA 9/14/94   WHEN FAXING CONTRACTS MUST SET OUTPUT                          
*                - CHECKING PW PROFILE TO SEE WHAT IS AGENCY                    
*                  DEFAULT                                                      
*                CANREQ MOVED TO IT'S OWN CSECT                                 
*                                                                               
* BPLA 9/6/94    CHANGES FOR FAXING CONTRACTS                                   
*                ALSO FIND AND CHANGE LOWER CASE COMMENTS                       
*                                                                               
* BPLA 8/26/94   CHANGES FOR MOVING DRD OVERRIDE CLT FROM                       
*                CLIPROF+6(3) PRECEEDED BY A "D"                                
*                TO A PCLTDRO (X'30') ELEMENT                                   
**********       NOTE THAT DRD OVERRIDE CLT IS FOUND IN                         
**********       PRREQ03 (CLIVAL) AND SAVED IN CLIPROF+20                       
*                                                                               
* BPLA 7/13/94   FOR BILLING REQUESTS - IF PRD = ALL                            
*                AND ESTIMATE = RANGE - SET RSORT TO 01                         
*                                                                               
* BPLA 6/94      FOR INVOICE DATE CHECKING ON BILLING                           
*                FIRST CHECK FOR SYMBOLIC EQUATE                                
*                                                                               
* BPLA 5/94      CHANGES IN VALRNV FOR NV FAXING                                
*                                                                               
* BPLA 3/94      FIX BUGS IN ENQRE WHEN HANDLING 2 CARD REQS                    
*                AT ENQR8 - BE SURE AT LEAST 2 LINES ARE AVAILABLE              
*                BEFORE READING THE NEXT REQUEST                                
*                                                                               
* BPLA 12/93     ADD ROUTINE (SOONCLT) FOR SOON REQUESTS TO BE SURE             
*                CLIENT IS NOT ALL,*N,&N,$N                                     
* BPLA 11/19/93  NEW POST VALIDATION CODE IN VALR14                             
*                                                                               
* BPLA 11/10/93  IN BILLING VALIDATION (VR110) IF CLIPROF+5 = "D"               
*                REQUIRE DRD CLIENT WHICH MUST MATCH CLIPROF+6(3)               
*                ALSO SET RDIV TO 000                                           
*                                                                               
* BPLA 3/18/93  CHANGES FOR RFP  (FERN AND VALIDATION)                          
*               MOVE GETFORM TO ITS OWN CSECT                                   
*                                                                               
* BPLA 2/5/93   VALIDATION ROUTINES FOR AR (217)                                
*                                                                               
* BPLA 9/25/92   CHANGES FOR B2B PROFILE BILLING CHECKS                         
*                                                                               
* BPLA 1/8/92    IN CKINVD ADD CHECK TO PREVENT INVOICE DATES                   
*                MORE THAN 90 DAYS INTO THE FUTURE                              
* BPLA 1/2/91    NO-OP FOR SJR AND SJX REMOVED                                  
*                                                                               
* BPLA 12/11/91  NO-OP GETFORM AND VALFORM EXCEPT FOR SJR AND SJX               
*                                                                               
* BPLA 12/3/91   CHANGE GETFORM  - SPECIAL SOON CODING                          
*                ALSO READ CONTROL FILE PROFILE TO GET FORMS                    
*                                                                               
* BPLA 11/21/91  IN VALR12 AND VALR72 CHECK FOR PROFILES AND POSSIBLY           
*                SET FORMS TYPE IN OUTPUT                                       
*                HAD TO MOVE ENDREQ TO ITS OWN CSECT                            
*                                                                               
* BPLA 11/19/91  VALR49 58 MONTHS MAXIMUM FOR RANGE                             
*                                                                               
* BPLA 11/11/91  VALR52 SEND INVALID ERROR NOT MISSING ERROR                    
*                IF SOON REQUEST IS MADE FOR EST FILTERS                        
*                                                                               
* BPLA 10/22/91  PP49  - SET DAY TO 01 IN START AND END                         
*                                                                               
* BPLA 10/9/91   DISALLOW OFFICE LIST REQUESTS FOR P27,P28                      
*                WHEN REQUESTED SOON                                            
*                                                                               
* BPLA 9/18/91   ANOTHER CHANGE TO VALRMY FORM SALLY S.                         
*                                                                               
* BPLA 7/26/91   CHANGE TO VALIDATION ROUTINE VALTMY (RT124)                    
*                                                                               
* BPLA 6/26/91   VALIDATION ROUTINE FOR MY (RT124)                              
*                                                                               
* BPLA 6/6/91    IF ESTIMATE RANGE (NNN-NNN) FOR P54 OR PL2                     
*                SET SORT CODE TO 01                                            
*                                                                               
* BPLA 3/26/91   LOGIC FOR RD - REBATE DRAFT  (RT123)                           
*                                                                               
* BPLA  2/8/91   SOON LIMITS FOR P49                                            
*                                                                               
* BPLA  11/7/90  DISALLOW "ES" AND PRD= ALL OR BLANK FOR 60 AND S2              
*                (VALR60)                                                       
*                                                                               
* BPLA  9/27/90  ENQD0F USED TO MOVE SPACES OVER BINARY                         
*                REVERSAL INVOICE NUMBER  - THIS FIELD IS NOW                   
*                CHARACTERS.                                                    
*                ALSO - 'X' PUT IN LAST COLUMN OF REQUEST CARD 2                
*                TO PREVENT CLOBBERING OF BINARY DATA                           
* BPLA  5/30/90  FOR SOON PBA MUST BE ONE CLT AND TEST RUN                      
*                                                                               
* BPLA  1/8/90   NO SOON FOR P12,P14,P18,P27,P28,P52,PEC,P60,P77                
*                PL1,PD1,PL2 GROUP REQUESTS (& IN RCLI)                         
*                                                                               
* BPLA  6/27/89  REPORT P37-- IF SOON THEN ONE PUB FOR MAX OF                   
*                ONE YEAR.                                                      
         TITLE 'PRREQ02- REQUEST - DISPLAY/UPDATE REQUEST FILE'                 
T41202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 100,T41202,RR=R9                                                 
         USING WORKD,RC                                                         
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                      R9=A(W/S)                          
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                       R3=A(TWA)                          
         LA    R8,T41202+4095                                                   
         LA    R8,1(R8)                                                         
         USING T41202+4096,R8      *** NOTE USE OF SECOND BASE                  
*                                                                               
         MVI   DMIN,X'20'                                                       
*                                                                               
         EJECT                                                                  
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
IOCTL    MVC   FERN,=AL2(FF)                                                    
         LA    R6,BVRNAMEH                                                      
         ST    R6,FADR                                                          
         CLI   STATUS,3                                                         
         BNE   *+16                                                             
         CLI   REQOPTN,C'T'                                                     
         BNE   ENQREQ                                                           
         B     TOTREQ                                                           
         CLI   REQACTN,C'A'                                                     
         BE    CHKIT                                                            
         CLI   REQACTN,C'D'                                                     
         BE    CANCEL                                                           
         CLI   REQACTN,C'N'                                                     
         BE    CHKIT                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
REQIOERR MVC   FERN,=AL2(0)                                                     
         SPACE 2                                                                
CLEARADR XC    LADR,LADR                                                        
         SPACE 2                                                                
         B     EXIT                                                             
         SPACE 2                                                                
SAVEADR  MVC   LADR,ADR                                                         
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
*                                                                               
ENQREQ   DS    0H                                                               
         GOTO1 =A(ENQRE),DMCB,(RC),(R9),RR=RELO                                 
         B     EXIT                                                             
CANCEL   DS    0H                                                               
         GOTO1 =A(CANREQ),DMCB,(RC),(R9),RR=RELO                                
         B     EXIT                                                             
*                                                                               
ADDREQ   DS    0H                                                               
         GOTO1 =A(NEWREQ),DMCB,(RC),(R9),RR=RELO                                
         B     EXIT                                                             
         EJECT                                                                  
*        ANY GLOBAL REQUEST POST VALIDATION CODE COMES HERE                     
*                                                                               
CHKIT    EQU   *                                                                
         CLI   LREQMAP,126                   CARD REQUEST                       
         BE    CHKREQX                       YES THEN NO VALIDATION             
         SPACE 2                                                                
*        CHECK IF REQUEST FIELDS REQUIRE FURTHER VALIDATION                     
*                                                                               
CHKREQ   LA    R7,VALROUTS                                                      
CHKREQ0  CLI   0(R7),0                                                          
         BE    CHKREQX                       NOT IN TBL NO VAL REQUIRED         
         CLC   REQNUM(2),0(R7)                                                  
         BE    *+12                                                             
         LA    R7,L'VALROUTS(R7)                                                
         B     CHKREQ0                                                          
         MVC   DUB+1(3),2(R7)                                                   
         L     RF,DUB                                                           
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         BASR  RA,RF                                                            
         CLC   FERN,=AL2(FF)                                                    
         BE    CHKREQX                       ALL FIELDS OK                      
         SPACE 2                                                                
         LA    R0,24                         SEARCH REQ MAP TABLE               
         LA    R1,LREQMAP                                                       
CHKREQ1  CLI   0(R1),127                                                        
         BE    CHKREQ2                                                          
         CLC   ROUTNUM,0(R1)                                                    
         BE    CHKREQ3                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,CHKREQ1                                                       
CHKREQ2  LA    R1,LREQMAP                    NOT IN TBL POSN TO 1ST FLD         
CHKREQ3  MVC   HALF,1(R1)                                                       
         LR    R6,R3                                                            
         AH    R6,HALF                                                          
CHKREQ4  ST    R6,FADR                       POSN CURSOR TO ROUTNUM FLD         
         B     EXIT                                                             
         SPACE 2                                                                
CHKREQX  CLI   REQACTN,C'N'                                                     
         BE    ADDREQ                                                           
         B     AMDREQ                                                           
         EJECT                                                                  
*        ROUTINES FOR FURTHER FIELD VALIDATION                                  
*                                                                               
         SPACE 2                                                                
VALR10   CLC   =C'ALL',RPRO                                                     
         BNE   *+10                                                             
         MVC   RPRO,=10X'40'                                                    
         CLC   =C'ALL',REST                                                     
         BNE   *+10                                                             
         MVC   REST,=10X'40'                                                    
         CLC   =C'DF',RAGY         SAATCHI ?                                    
         BNE   VR10X                                                            
         CLI   RMED,C'*'           ALL MEDIA ?                                  
         BNE   VR10X                                                            
         CLI   RO4,C'T'            TAPE REQUEST ?                               
         BE    VR10X               YES - OK                                     
         MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,X'71'       CURSOR TO "PRODUCE TAPE?" FIELD              
VR10X    BR    RA                                                               
         SPACE 2                                                                
*                                                                               
VALR12   DS    0H                                                               
         CLI   RPAY,C'Y'        FIRST SEE IF FAXING CONTRACTS                   
         BNE   VR120B                                                           
*                               READ PW PROFILE TO SEE IF FAXING                
*                               CONTRACTS IS ALLOWED                            
         MVC   HALF,=C'PW'                                                      
         BAS   RE,RDPROF                                                        
*                               RETURNS PROFILE IN TEMP                         
         MVC   TEMP+60(1),TEMP+4    SAVE DEFAULT JCL OUTPUT                     
*                               INDICATOR (D=DIRECT,C=CON,4=4AS)                
*                                                                               
         CLI   TEMP+1,C'Y'      SEE IF CONTRACT FAXING ALLOWED                  
         BE    VR120B                                                           
         MVC   FERN,=AL2(96)        NOT AUTHORIZED FOR FAXING                   
         MVI   ROUTNUM,128     (X'80)                                           
         BR    RA                                                               
*                                                                               
VR120B   DS    0H                                                               
         CLC   RCLI,=C'ALL'                                                     
         BE    VR12OD                                                           
         CLI   RCLI,C'*'                                                        
         BE    VR12OD                                                           
         CLI   RCLI,C'&&'                                                       
         BE    VR12OD                                                           
         CLC   RCLI,SPACES                                                      
         BE    VR12OD                                                           
*                                                                               
*        MUST BE ONE CLIENT - CHECK PROFILE UNLESS OUTPUT REQUESTED             
         OC    REQOUT(6),REQOUT                                                 
         BNZ   VR12OX                                                           
*                                                                               
         MVC   HALF,RNUM                                                        
         BAS   RE,RDPROF                                                        
******   BAS   RE,GETFORM            GET FORM TYPE                              
         GOTO1 =A(GETFORM),DMCB,(RC),(R9),RR=RELO                               
         CLC   REQOUT(6),TEMP+20                                                
         BE    VR12OX                                                           
         OC    REQOUT,REQOUT                                                    
         BZ    VR12OC                                                           
         MVC   FERN,=AL2(FLDINV)                                                
         LA    RE,BVROUTH                                                       
         ST    RE,FADR                                                          
         B     EXIT                                                             
*                                                                               
VR12OC   MVC   REQOUT(6),TEMP+20                                                
         B     VR12OX                                                           
*                                                                               
VR12OD   DS    0H                                                               
         MVC   HALF,RNUM                                                        
         BAS   RE,VALFORM             SEE IF ANY PROFILES HAVE                  
*                                     FORMS OVERRIDE, IF FOUND                  
*                                     DISALLOW REQUEST FOR A                    
*                                     NON-SPECIFIC CLIENT                       
VR12OX   DS    0H                                                               
         CLI   RPAY,C'Y'              SEE IF FAXING                             
         BNE   VALR12N                                                          
         CLC   REQOUT(4),=C'&&CON'                                              
         BNE   VR12OX5                                                          
         MVC   REQOUT(6),=C'&&BCKS '   SPECIAL FOR FAXING                       
         B     VALR12N                                                          
*                                                                               
VR12OX5  CLC   REQOUT(4),=C'&&4AS'                                              
         BNE   VR12OX7                                                          
         MVC   REQOUT(6),=C'&&B4AKS'  SPECIAL FOR FAXING                        
         B     VALR12N                                                          
*                                                                               
VR12OX7  DS    0H                                                               
         OC    REQOUT,REQOUT      CHECK FOR OUTPUT                              
         BNZ   VALR12N                                                          
         CLI   TEMP+60,C'D'       SEE IF DEFAULT JCL IS DIRECT                  
         BE    VALR12N                                                          
         CLI   TEMP+60,C'C'       SEE IF DEFAULT IS &&CON                       
         BNE   VR12OX8                                                          
         MVC   REQOUT(6),=C'&&BCKS '    SPECIAL FOR FAXING                      
         B     VALR12N                                                          
*                                                                               
VR12OX8  CLI   TEMP+60,C'4'        SEE IF DEFAULT IS &&4AS                      
         BNE   VALR12N                                                          
         MVC   REQOUT(6),=C'&&B4AKS'    SPECIAL FOR FAXING                      
         B     VALR12N                                                          
*                                                                               
VALR12N  DS    0H                                                               
*                                                                               
*        NOTE THAT OTHER REPORTS USE THIS CODE ALSO                             
*                                                                               
         CLC   BVROUT(4),=C'SOON'        SPECIAL CHECKS FOR SOON REQS           
         BNE   VR12M                                                            
         BAS   RE,SOONCLT                SOON MUST BE ONE CLT                   
VR12G    DS    0H                                                               
         CLC   REST,SPACES               SEE IF CONTRACT SPECIFIED              
         BNE   VR12M                                                            
         CLC   RSTRD,SPACES              MUST INPUT START AND END DATES         
         BH    VR12J                                                            
         MVC   FERN,=AL2(FLDMIS)         MISSING INPUT FIELD                    
         MVI   ROUTNUM,X'10'             POINT TO START, END DATE FIELD         
         B     VALR12X                                                          
VR12J    MVC   HALF,=X'0C00'             IF DATE INPUT SPREAD CAN'T BE          
         MVI   ROUTNUM,X'10'             MORE THAN ONE YEAR                     
         BAS   RE,VALMAX                                                        
*                                                                               
VR12M    CLC   RNUM,=C'AC'                                                      
         BE    VR12M5                                                           
         CLC   RNUM,=C'12'         SINCE 14,18,19,AU USE VALR12N                
         BNE   VALR12A             FOR SOON LIMITATIONS                         
*                                                                               
VR12M5   CLI   RDIV,C' '           SEE IF SLAVE CLT SPECIFIED                   
         BE    VR12N               NO                                           
         CLI   RO3,C'R'            MUST BE SPACE RESERVATION                    
         BE    VR12N                                                            
         MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,X'58'       CURSOR TO OPT3                               
         BR    RA                                                               
*                                                                               
VR12N    DS    0H                                                               
         CLI   RO5,C'F'                                                         
         BNE   VR12P                                                            
         CLC   RCNTDAT(3),SPACES                                                
         BH    VR12P                                                            
         MVI   ROUTNUM,X'46'                                                    
         MVC   FERN,=AL2(FLDMIS)                                                
         BR    RA                                                               
*                                                                               
VR12P    CLC   RCLI,=C'ALL'                                                     
         BNE   VALR12A                                                          
         CLC   RPUB(11),SPACES                                                  
         BNE   VALR12A                                                          
         MVC   FERN,=AL2(PUBREQ)   ERR 160 PUB REQUIRED                         
         MVI   ROUTNUM,14                                                       
         BR    RA                                                               
VALR12A  CLC   REST,=C'000'                                                     
         BE    VALR12B                                                          
         CLC   REST(3),SPACES        IF CONTRACT NUMBER IS BLANK                
         BNE   VALR12C              START AND END REQUIRED                      
VALR12B  CLC   RSTRD(12),=12C' '                                                
         BNE   VALR12C                                                          
         MVI   ROUTNUM,16                                                       
         MVC   FERN,=AL2(FLDMIS)                                                
         BR    RA                                                               
VALR12C  CLI   RO4,C'G'                                                         
         BNE   VALR12D                                                          
         CLC   RSTRD(12),=12C' '                                                
         BE    VALR12D                                                          
         MVC   HALF,=X'0B00'         12 MTHS MAX FOR GRID FORMAT                
         BAS   RE,VALMAX                                                        
*                                                                               
VALR12D  DS    0H                                                               
         CLI   RSORT,C'0'                                                       
         BL    VALR12E                                                          
         CLI   RPUB,C'0'                                                        
         BL    VALR12E                                                          
*                                  CAN'T SORT ONE PUB OR PUB,ALL                
         MVI   ROUTNUM,X'16'       SET CURSOR TO SORT                           
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VALR12E  DS    0H                                                               
         CLI   RO7,C'I'            COL68 FOR TOTALS OPTION                      
         BE    VALR12J             (I=INCHES/L=LINES)                           
         CLI   RO7,C'L'                                                         
         BNE   VALR12X                                                          
VALR12J  CLI   RMED,C'N'           FOR NEWSPAPERS ONLY                          
         BE    VALR12X                                                          
         MVI   ROUTNUM,X'4B'                                                    
         MVC   FERN,=AL2(FLDINV)                                                
         SPACE                                                                  
*                                                                               
VALR12X  BR    RA                                                               
         SPACE 2                                                                
VALR14   DS    0H                                                               
         CLC   RPUB+1(3),=C'RD='   SEE IF RD= CLT GIVEN                         
         BNE   VALR14B                                                          
         CLC   RSORT,=C'08'        MUST BE DRD SORT                             
         BE    VALR14B                                                          
         CLC   RSORT,=C'09'        OR DRD WITH NAME/MKT                         
         BE    VALR14B                                                          
         MVI   ROUTNUM,X'16'       CURSOR T0 SORT                               
         MVC   FERN,=AL2(FLDMIS)                                                
         CLC   RSORT,=C'  '        SEE IF SORT MISSING                          
         BER   RA                                                               
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VALR14B  DS    0H                                                               
         CLC   RSORT,=C'08'        SEE IF DRD SORT                              
         BE    VALR14B5                                                         
         CLC   RSORT,=C'09'        DRD WITH NAME/MKT                            
         BNE   VALR14C                                                          
*                                                                               
VALR14B5 CLI   RDIV,C' '           SEE IF DIVISION ENTERED                      
         BNE   VALR14B7                                                         
         CLC   RESTEND,SPACES     SEE IF RD= CLIENT GIVEN                       
         BNE   VALR14B7           IF DIV IS NOT REQUIRED                        
         MVC   FERN,=AL2(FLDMIS)   IF NOT DIVISION MUST BE GIVEN                
         MVI   ROUTNUM,4           CURSOR TO DIV                                
         BR    RA                                                               
*                                                                               
VALR14B7 DS    0H                                                               
         CLC   RREG,SPACES        SEE IF REGION GIVEN                           
         BNE   VALR14F            YES                                           
*                                                                               
         MVC   RREG,=C'ALL'        IF SPACES SET TO ALL                         
         B     VALR14F                                                          
*                                                                               
VALR14C  MVC   RDIV,SPACES     IF SORT IS NOT 08 CLEAR DIV/REG/DST              
         MVC   RREG,SPACES                                                      
         MVC   RDIS,SPACES                                                      
*                                                                               
VALR14F  DS    0H                                                               
         CLC   RPUB+1(5),=C'P=ALL'   SEE IF ALL PUBLISHER REQUEST               
         BNE   VALR14H                                                          
         CLI   RSORT,C'0'          SEE IF SORT ENTERED                          
         BNL   VALR14H                                                          
         MVC   RSORT(2),=C'01'     SET TO 01 FOR PUBLISHER SORT                 
*                                                                               
VALR14H  CLI   RO4,C'Y'            CHGS ONLY                                    
         BNE   VALR12N             REST SAME AS 12                              
         CLC   RCNTDAT(3),SPACES       YES - CONTROL DATE REQUIRED              
         BNE   VALR12N             OK                                           
         MVC   FERN,=AL2(FLDMIS)   MISSING                                      
         MVI   ROUTNUM,X'46'                                                    
VALR14X  BR    RA                                                               
         SPACE 2                                                                
VALR16   CLC   BVROUT(4),=C'SOON'                                               
         BNE   VALR16X                                                          
         CLI   RO3,C'N'        MEANS 'LIVE' RUN - INVALID FOR SOON              
         BNE   VALR16X                                                          
         MVC   FERN,=AL2(FLDINV)      INVALID                                   
         MVI   ROUTNUM,X'88'          CUSROR TO TEST OPTION                     
VALR16X  BR    RA                                                               
         SPACE 2                                                                
VALR18   CLI   BVRNUM+2,C'T'          IF REQUESTING TO INCLUDE TEST             
         BNE   VALR18C               BUYS SET QOPT6 TO 'Y'                      
         MVI   RO6,C'Y'                                                         
*                                                                               
VALR18C  CLC   BVROUT(4),=C'SOON'     SEE IF SOON REQ                           
         BNE   VALR14F                                                          
*                                     REST SAME AS VALR14F                      
*                                     WHICH IN TURN GOES TO VALR12N             
         CLC   RPUB+1(5),=C'P=ALL'    ALL PUBLISHERS - NOT FOR SOON             
         BE    SOONPBL                                                          
         CLI   RPUB+1,C'P'            PUBLISHER GIVEN                           
         BE    VALR14F                                                          
         CLI   RPUB,C'0'              YES - PUB MUST BE GIVEN                   
         BNL   VALR14F                                                          
         MVI   ROUTNUM,X'0E'          CURSOR TO PUB                             
         MVC   FERN,=AL2(FLDMIS)      FIELD MISSING                             
         BR    RA                                                               
*                                                                               
         SPACE 2                                                                
VALR19   CLI   BVRNUM+2,C'T'          IF REQUESTING TO INCLUDE TEST             
         BNE   VALR19B               BUYS SET QOPT6 TO 'Y'                      
         MVI   RO6,C'Y'                                                         
*                                                                               
VALR19B  DS    0H                                                               
         CLC   RPUB+1(5),=C'P=ALL'    ALL PUBLISHER PUBS REQUEST ?              
         BNE   VALR19C                NO                                        
         CLI   RSORT,C'0'             ANYTHING IN SORT ?                        
         BNL   VALR19C                YES                                       
         MVC   RSORT,=C'01'           TO FORCE PPG SORT FOR PUBLISHERS          
*                                                                               
VALR19C  CLC   BVROUT(4),=C'SOON'     SEE IF SOON REQ                           
         BNE   VALR19N                NO                                        
*                                                                               
         CLC   RPUB+1(5),=C'P=ALL'    ALL PUBLISHER PUBS REQUEST ?              
         BNE   VALR19D                DISALLOW FOR SOON                         
SOONPBL  MVI   ROUTNUM,X'CB'          CURSOR TO PUBLISHER                       
         MVC   FERN,=AL2(FLDINV)      FIELD INVALID                             
         BR    RA                                                               
*                                     REST SAME AS VALR14F                      
*                                     WHICH IN TURN GOES TO VALR12N             
VALR19D  CLI   RPUB+1,C'P'            ONE PUBLISHER REQQUEST                    
         BE    VALR19N                OK FOR SOON                               
VALR19F  CLI   RPUB,C'0'              PUB MUST BE GIVEN FOR SOON                
         BNL   VALR19N                                                          
         MVI   ROUTNUM,X'0E'          CURSOR TO PUB                             
         MVC   FERN,=AL2(FLDMIS)      FIELD MISSING                             
         BR    RA                                                               
*                                                                               
VALR19N  DS    0H                                                               
         CLI   RO4,C'Y'                                                         
         BNE   VALR12N             REST SAME AS VALR12N                         
         CLC   RPAY(3),SPACES      CONTROL DATE REQUIRED                        
         BNE   VALR12N                                                          
         MVC   FERN,=AL2(FLDMIS)   MISSING INPUT                                
         MVI   ROUTNUM,X'46'       CURSOR TO CONTROL DATE                       
         BR    RA                                                               
*                                                                               
         SPACE 2                                                                
VALR27   DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'        SPECIAL CHECKS FOR SOON REQS           
         BNE   VR27V                                                            
         BAS   RE,SOONCLT                SOON MUST BE ONE CLT                   
*                                                                               
VR27E5   MVC   HALF,=X'0500'           06 MONTHS                                
         BAS   RE,VALMAX                                                        
*                                                                               
VR27V    DS    0H                                                               
         LA    R5,RPAY             AS OF DATE                                   
VALR27A  CLC   REST,=C'ALL'                                                     
         BNE   *+10                                                             
         MVC   REST,SPACES         SET ALL TO SPACES                            
         CLC   0(6,R5),SPACES          AS OF DATE                               
         BE    VALR27E                                                          
         CLI   RO1,C' '         FOR PAID OR UNPAID ITEMS                        
         BNE   VALR27C                                                          
         CLI   RO5,C' '            OR BILLED/TRAFF OPTION                       
         BNE   VALR27C                                                          
         MVI   ROUTNUM,84        AS OF DATE FIELD                               
         MVC   FERN,=AL2(ASDINV)                                                
         B     VALR27X                                                          
*                                                                               
VALR27C  DS    0H                                                               
         CLC   0(6,R5),TODAY                                                    
         BNH   VALR27E                                                          
         MVC   FERN,=AL2(ASOFDPST)                                              
         MVI   ROUTNUM,84                                                       
         BR    RA                                                               
*                                                                               
VALR27E  DS    0H                                                               
         MVC   HALF,=X'2300'           36 MONTHS                                
         BAS   RE,VALMAX                                                        
*                                                                               
VALR27F  DS    0H                                                               
         CLC   RNUM,=C'36'                                                      
         BE    VALR27X                                                          
*                                  FOR P27, P28, P37                            
         TM    ESTSAVE,X'60'       SEE IF EST OR EST-EST WITH                   
         BZ    VALR27X             NON-SPECIFIC PRODUCT                         
         CLC   RSORT,=C'NO'                                                     
         BNE   VALR27X                                                          
         MVC   RSORT,=C'02'                                                     
*                                                                               
VALR27X  BR    RA                                                               
         SPACE 2                                                                
VALR28   DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'        SPECIAL CHECKS FOR SOON REQS           
         BNE   VR28V                                                            
         BAS   RE,SOONCLT           MUST BE ONE CLT                             
*                                                                               
VR28E5   MVC   HALF,=X'0500'           06 MONTHS                                
         BAS   RE,VALMAX                                                        
*                                                                               
*                                                                               
VR28V    DS    0H                                                               
         LA    R5,RPAY             AS OF DATE                                   
         B     VALR27A                                                          
*                                                                               
VALR36   CLI   RO4,C'P'            SEE IF SUPPRESSING PUB                       
         BNE   VALR36B             NO                                           
         MVC   RSORT,=C'NO'        MUST RESET FOR NO SORTING                    
*                                                                               
VALR36B  LA    R5,RREG             AS OF DATE                                   
         B     VALR27A                                                          
         SPACE 2                                                                
VALR37   DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'       FOR SOON REPORTS                        
         BNE   VALR37BB                                                         
         MVI   ROUTNUM,14             POSITION CURSOR TO PUB FIELD              
         CLI   RPUB,C'0'              PUB MUST BE SPECIFIED                     
         BNL   VALR37B                  (OR SPACES)                             
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VALR37B  DS    0H              CHECK REQUEST START & END DATE                   
         MVC   HALF,=X'0B00'       12 MTHS MAS FOR REQ                          
         BAS   RE,VALMAX           ON ERROR VALMAX RETRNS ON RA                 
*                                                                               
VALR37BB CLI   RO6,C'Y'                                                         
         BNE   VALR37C                                                          
         CLC   RSORT,=C'08'        MUST USE SORT CODE 08                        
         BE    VALR37C                                                          
         CLC   RSORT,=C'09'        OR 09                                        
         BE    VALR37C                                                          
         MVI   ROUTNUM,X'6A'       CURSOR TO PAY ADDR OPT                       
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VALR37C  LA    R5,RREG             AS OF DATE                                   
         B     VALR27A                                                          
*                                                                               
VALR46   MVI   ROUTNUM,14               POSITION CURSOR TO PUB FIELD            
         CLI   RPUB+2,C' '              SEE IF PUB OR PUBLISHER INPUT           
         BH    VALR46B                  YES                                     
         MVC   FERN,=AL2(FLDMIS)        MISSING                                 
         BR    RA                                                               
VALR46B  CLC   BVROUT(4),=C'SOON'       FOR SOON REPORTS                        
         BNE   VALR46C                                                          
         CLC   RPUB+1(2),=C'P='         ACCEPT PUBLISHER REQUEST                
         BE    VALR46C                                                          
         CLC   RPUB,=C'00000000'        FOR SOON PUB CAN'T BE ALL               
         BNL   VALR46C                  (OR SPACES)                             
         MVC   FERN,=AL2(FLDINV)                                                
VALR46C  CLC   RPUB(3),=C'ALL'                                                  
         BNE   VALR46E                                                          
         MVC   RPUB(11),SPACES                                                  
*                                                                               
VALR46E  CLC   RSORT,SPACES                                                     
         BE    VALR46X                                                          
         CLC   RPUB(11),SPACES                                                  
         BE    VALR46X                                                          
         MVC   RSORT,SPACES              SET SORT TO SPACES FOR                 
*                                        REQ FOR SPECIFIC PUB                   
VALR46X  BR    RA                                                               
*                                                                               
VALR48   CLC   RSTRD(4),SPACES                                                  
         BE    VALR48A                                                          
         MVC   RSTRD+4(2),=C'01'                                                
VALR48A  CLI   RPUB+3,C' '         SEE IF LIST OR PUBLISHER INPUT               
         BE    VR48B                                                            
         CLI   RPUB+1,C'P'         SEE IF PUBLISHER INPUT                       
         BE    VR48B                                                            
         MVC   RPUB+1(2),=C'L='                                                 
*                                                                               
VR48B    CLC   RSORT,=C'04'                                                     
         BNE   VR48E                                                            
         CLC   RCLI,SPACES         FOR SORT 04 CLT MUST BE THERE                
         BE    VR48BX                                                           
         CLC   RCLI,=C'ALL'       BUT NOT ALL                                   
         BNE   VR48C                                                            
         MVI   ROUTNUM,2                                                        
         MVC   FERN,=AL2(FLDINV)  INVALID                                       
         BR    RA                                                               
*                                                                               
VR48BX   MVI   ROUTNUM,2                                                        
         MVC   FERN,=AL2(FLDMIS)  MISSING                                       
         BR    RA                                                               
*                                                                               
VR48C    CLC   RDIV,SPACES         SO MUST DIVISION                             
         BE    VR48CX                                                           
         CLC   RDIV,=C'ALL'       BUT ALL IS INVALID                            
         BNE   VR48G              GO TEST VALIDITY OF SCHEME                    
         MVI   ROUTNUM,4                                                        
         MVC   FERN,=AL2(FLDINV)   INVALID                                      
         BR    RA                                                               
*                                                                               
VR48CX   MVI   ROUTNUM,4                                                        
         MVC   FERN,=AL2(FLDMIS)   MISSING                                      
         BR    RA                                                               
*                                                                               
VR48E    CLI   RO4,C' '           IF CLIENT ACTIVITY FILTER                     
         BE    VR48G              SPECIFIED                                     
         CLC   RCLI,SPACES        CLT MUST BE THERE                             
         BE    VR48EX                                                           
         CLC   RCLI,=C'ALL'       BUT NOT ALL                                   
         BE    VR48EB                                                           
         CLI   RCLI,C'*'          AND NOT OFFICE (*N)                           
         BNE   VR48G                                                            
VR48EB   MVI   ROUTNUM,2                                                        
         MVC   FERN,=AL2(FLDINV)  INVALID                                       
         BR    RA                                                               
*                                                                               
VR48EX   MVI   ROUTNUM,2                                                        
         MVC   FERN,=AL2(FLDMIS)   MISSING                                      
         BR    RA                                                               
*                                                                               
VR48G    CLI   RO7,C' '           SCHEME (ID) BLANK ?                           
         BE    VR48X              YES                                           
         CLI   RO1,C'G'           LIST TYPE G (PUB GROUP ASSIGNS) ?             
         BE    VR48X              YES                                           
         MVI   ROUTNUM,X'CC'      POSITION CURSOR TO SCHEME FIELD               
         MVC   FERN,=AL2(FLDINV)  INVALID                                       
         BR    RA                                                               
*                                                                               
VR48X    BR    RA                                                               
         SPACE 2                                                                
VALR49   DS    0H                                                               
*                                                                               
         CLI   RCLI,C'$'                                                        
         BNE   VR49B                                                            
         CLI   RCLI+1,C'*'               DISALLOW $N                            
         BE    VR49B                                                            
         MVI   ROUTNUM,2                                                        
         MVC   FERN,=AL2(FLDINV)                                                
         B     VALR49X                                                          
*                                                                               
VR49B    CLC   BVROUT(4),=C'SOON'        SPECIAL CHECKS FOR SOON REQS           
         BNE   VALR49D                                                          
         MVI   RO5,C' '                  MUST SUPPRESS SPECIAL TOTALS           
*                                        FOR SOON REQS                          
         BAS   RE,SOONCLT                MUST BE ONE CLIENT                     
*                                                                               
VALR49D  CLC   RENDD,SPACES         SEE IF START/END RANGE GIVEN                
         BE    VALR49E                                                          
         MVC   RSTRD+4(2),=C'01'         SET DAY TO 01                          
         MVC   RENDD+4(2),=C'01'                                                
         MVC   HALF,=X'2F00'            58 MONTHS                               
         BAS   RE,VALMAX                                                        
         MVC   RPAY(4),SPACES           BLANK OUT MONTHS BACK AND UP            
         B     VALR49X                                                          
*                                                                               
VALR49E  MVC   RSTRD+4(2),SPACES  ONLY MONTH GIVEN - SET DAY TO BLANKS          
*                                                                               
VALR49X  BR    RA                                                               
*                                                                               
VALR52   DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'       FOR SOON REPORTS                        
         BNE   VALR520X                                                         
         BAS   RE,SOONCLT              MUST BE ONE CLIENT                       
*                                                                               
         MVI   ROUTNUM,6                POSITION CURSOR TO PROD FIELD           
         CLC   RPRO,=C'ALL'             FOR SOON PROD CAN'T BE ALL              
         BE    VALR520F                 (OR SPACES)                             
         CLC   RPRO,SPACES                                                      
         BE    VALR520B                                                         
         B     VALR520D                                                         
*                                                                               
VALR520B MVC   FERN,=AL2(FLDMIS)        SET ERROR MESSAGE                       
         B     VALR52X                                                          
VALR520D MVI   ROUTNUM,12               POSITION CURSOR                         
         CLC   REST,=C'ALL'             FOR SOON EST CAN'T BE ALL               
         BE    VALR520F                 (OR SPACES)                             
         CLC   RESTEND,SPACES           CAN'T BE AN ESTIMATE RANGE              
         BNE   VALR520F                 OR FILERS                               
         CLC   REST,SPACES                                                      
         BE    VALR520B                 ESTIMATE CAN'T BE MISSING               
*                                       FOR SOON REQUESTS                       
         MVC   HALF,=X'0C00'                                                    
         CLC   RAGY,=C'WI'             FOR WESTERN - ALLOW 12 MONTHS            
         BE    VALR520E                                                         
         CLC   RAGY,=C'MX'             FOR WESTERN - ALLOW 12 MONTHS            
         BE    VALR520E                                                         
         CLC   RAGY,=C'WJ'             FOR WESTERN - ALLOW 12 MONTHS            
         BE    VALR520E                                                         
         CLC   RAGY,=C'WT'             FOR WESTERN - ALLOW 12 MONTHS            
         BE    VALR520E                                                         
         CLC   RAGY,=C'WR'             FOR WESTERN - ALLOW 12 MONTHS            
         BE    VALR520E                                                         
         MVC   HALF,=X'0200'                                                    
VALR520E BAS   RE,VALMAX                                                        
         B     VALR520Z                                                         
*                                                                               
VALR520F MVC   FERN,=AL2(FLDINV)        SET ERROR MESSAGE                       
         B     VALR52X                                                          
*                                                                               
VALR520X MVC   HALF,=X'0C00'       13 MONTHS                                    
         BAS   RE,VALMAX                                                        
VALR520Z CLI   RBPD,C' '                                                        
         BE    VALR52A                                                          
         B     VALR52B                                                          
*                                                                               
VALR52A  CLI   ESTDATES,0                                                       
         BE    VALR52B                                                          
         CLC   RSTRD(2),=C'ES'                                                  
         BE    VALR52B                                                          
*                                                                               
         LA    R5,RSTRD                                                         
         BAS   RE,VALRFP            CHK FOR SYMBOLIC EQU                        
         BE    VALR52B                                                          
*                                                                               
         CLC   ESTDATES(6),RENDD                                                
         BH    *+14                                                             
         CLC   ESTDATES+6(6),RSTRD                                              
         BNL   VALR52B                                                          
         MVC   FERN,=AL2(SEDNIE)   DATES NOT WITHIN EST PERIOD                  
         MVI   ROUTNUM,16                                                       
         B     VALR52X                                                          
*                                                                               
VALR52B  CLC   RSORT,SPACES                                                     
         BNE   VALR52C                                                          
         CLC   RPRO,SPACES                                                      
         BNE   VALR52C                                                          
         MVC   RSORT,=C'01'         IF PRODUCT IS BLANK SET SORT TO 01          
*                                                                               
VALR52C  DS    0H                                                               
         CLC   RNUM(2),=C'EC'       CHECK FOR ECT OR 52T                        
         BE    VALR52C5                                                         
         CLC   RNUM(2),=C'52'                                                   
         BNE   VALR52D                                                          
         CLC   BVRNUM(3),=C'EST'                                                
         BE    VALR52D                                                          
VALR52C5 CLI   BVRNUM+2,C'T'        IF REQUESTING TO INCLUDE TEST BUYS          
         BNE   VALR52D              MAKE FIRST CHAR OF REQUESTOR LOWER          
         NI    RNAME,X'BF'          TO INDICATE IT                              
*                                                                               
VALR52D  CLC   RPUB+1(3),=C'RD='    SEE IF USING CLT OVERRIDE                   
         BNE   VALR52E                                                          
         CLC   RREG(6),SPACES       REG +/OR DST MUST BE INPUT                  
         BNE   VALR52X                                                          
         MVI   ROUTNUM,8            CURSOR TO REG FIELD                         
         MVC   FERN,=AL2(FLDMIS)                                                
         B     VALR52X                                                          
*                                                                               
VALR52E  CLI   RO5,C'C'                                                         
         BE    VALR52G                                                          
         CLI   RO5,C'T'                                                         
         BNE   VALR52H                                                          
VALR52G  CLC   RCNTDAT(3),SPACES     CHG CONTROL DATE MUST BE SPECIFIED         
         BNE   VALR52H                                                          
         MVC   FERN,=AL2(FLDMIS)     MISSING                                    
         MVI   ROUTNUM,X'46'                                                    
         B     VALR52X                                                          
*                                                                               
VALR52H  TM    ESTSAVE,X'24'         TEST SPECIFIC EST INPUT                    
         BNZ   VALR52X                                                          
         CLC   RSTRD(2),=C'ES'       AND ES                                     
         BNE   VALR52X                                                          
         CLC   RCNTDAT(3),=X'000000' THEN CONTROL DATE MUST BE 'BILL'           
         BE    VALR52X                                                          
         MVC   FERN,=AL2(FLDINV)     INVALID FIELD                              
         MVI   ROUTNUM,16            CURSOR TO START/END                        
         B     VALR52X                                                          
*                                                                               
VALR52X  BR    RA                                                               
         SPACE 2                                                                
VALR54   DS    0H                                                               
         CLC   REST,=C'ALL'                                                     
         BE    VALR54X                                                          
         CLC   RESTEND,SPACES  IF SECOND EST GIVEN - SET SORT TO 01             
         BE    *+10           PPG WILL PUT EST INTO SORT                        
         MVC   RSORT,=C'01'                                                     
VALR54X  BR    RA                                                               
*                                                                               
VALR60   DS    0H                                                               
***                        NOTE THAT S2 (113) EDIT COMES HERE ALSO              
***                        BUT IS NOT SOONABLE YET                              
***                                                                             
         CLC   BVROUT(4),=C'SOON'       FOR SOON REPORTS                        
         BNE   VALR600X                                                         
         BAS   RE,SOONCLT               MUST BE ONE CLIENT                      
*                                                                               
         MVI   ROUTNUM,6                POSITION CURSOR TO PROD FIELD           
         CLC   RPRO,=C'ALL'             FOR SOON PROD CAN'T BE ALL              
         BE    VALR600F                 (OR SPACES)                             
         CLC   RPRO,SPACES                                                      
         BE    VALR600B                                                         
         B     VALR600D                                                         
VALR600B MVC   FERN,=AL2(FLDMIS)        SET ERROR MESSAGE - MISSING             
         B     VALR60X                                                          
VALR600D DS    0H                                                               
         B     VALR600X                 USED TO CHK FOR ONE ESTIMATE            
*                                                                               
*                                                                               
VALR600F MVC   FERN,=AL2(FLDINV)        SET ERROR MESSAGE                       
         B     VALR60X                                                          
*                                                                               
VALR600X MVC   HALF,=X'0D01'         13 WEEKS                                   
         CLI   RO3,C'W'                                                         
         BE    VALR60B                                                          
         MVC   HALF,=X'0C00'         13 MTHS                                    
         CLI   RO3,C'D'              DAYS                                       
         BNE   VALR60B                                                          
         CLC   RSTRD(2),=C'ES'       CAN'T ACCEPT ES                            
         BE    VALR60A                                                          
*                                                                               
         LA    R5,RSTRD                                                         
         BAS   RE,VALRFP              CHECK FOR SYMBOLIC EQU                    
         BE    VALR60C                                                          
*                                                                               
         XC    TEMP(6),TEMP                                                     
         GOTO1 ADDAY,DMCB,RSTRD,TEMP,12      MAX 13 DAYS                        
         CLC   TEMP(6),RENDD                                                    
         BNL   VALR60C                                                          
VALR60A  MVI   ROUTNUM,16                                                       
         MVC   FERN,=AL2(SEDBIG)             ERROR DURATION TOO LONG            
         BR    RA                                                               
*                                                                               
VALR60B  BAS   RE,VALMAX                                                        
VALR60C  CLC   RSTRD(2),=C'ES'                                                  
         BNE   VALR60D                                                          
         CLC   RPRO,SPACES                                                      
         BE    VALR60CX                                                         
         CLC   RPRO,=C'ALL'                                                     
         BNE   VALR60D                                                          
VALR60CX MVI   ROUTNUM,16      ES AND PRD ALL OR BLANK IS INVALID               
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VALR60D  CLC   RSORT,SPACES                                                     
         BNE   VALR60F                                                          
         CLC   RPRO,SPACES         IF PRD=BLANK SET SORT TO 01                  
         BNE   VALR60F                                                          
         MVC   RSORT,=C'01'                                                     
*                                                                               
VALR60F  CLI   BVRNUM+2,C'T'          IF REQUESTING TO INCLUDE TEST             
         BNE   VALR60X                BUYS SET QOPT7 TO 'Y'                     
         MVI   RO7,C'Y'                                                         
*                                                                               
VALR60X  BR    RA                                                               
         SPACE 2                                                                
*                                                                               
VALR72   DS    0H                                                               
         CLC   RCLI,=C'ALL'                                                     
         BE    VR72OD                                                           
         CLI   RCLI,C'*'                                                        
         BE    VR72OD                                                           
         CLI   RCLI,C'&&'                                                       
         BE    VR72OD                                                           
         CLC   RCLI,SPACES                                                      
         BE    VR72OD                                                           
*                                                                               
*        MUST BE ONE CLIENT - CHECK PROFILE OUTPUT REQUESTED                    
         OC    REQOUT(6),REQOUT                                                 
         BNZ   VALR72X                                                          
*                                                                               
         MVC   HALF,=C'72'                                                      
         BAS   RE,RDPROF                                                        
*******  BAS   RE,GETFORM            GET FORM TYPE                              
         GOTO1 =A(GETFORM),DMCB,(RC),(R9),RR=RELO                               
         CLC   REQOUT(6),TEMP+20                                                
         BE    VALR72X                                                          
         OC    REQOUT,REQOUT                                                    
         BZ    VR72OC                                                           
         MVC   FERN,=AL2(FLDINV)                                                
         LA    RE,BVROUTH                                                       
         ST    RE,FADR                                                          
         B     EXIT                                                             
*                                                                               
VR72OC   MVC   REQOUT(6),TEMP+20                                                
         MVI   REQOUT+4,C'K'          SET FOR KEEP                              
         B     VALR72X                                                          
*                                                                               
VR72OD   DS    0H                                                               
         MVC   HALF,=C'72'                                                      
         BAS   RE,VALFORM             SEE IF ANY PROFILES HAVE                  
*                                     FORMS OVERRIDE, IF FOUND                  
*                                     DISALLOW REQUEST FOR A                    
*                                     NON-SPECIFIC CLIENT                       
VALR72X  DS    0H                                                               
         BR    RA                                                               
*                                                                               
         SPACE 2                                                                
VALR74   MVI   RMED,C'M'         SET MEDIA TO M (SINCE ALL AGYS                 
         BR    RA                PROBABLY HAVE IT)                              
         SPACE 2                                                                
VALR77   DS    0H                                                               
*                                                                               
         CLC   BVROUT(4),=C'SOON'                                               
         BNE   VALR77H                                                          
         BAS   RE,SOONCLT           MUST BE ONE CLIENT                          
*                                                                               
VALR77G  MVC   HALF,=X'0200'                                                    
         MVI   ROUTNUM,X'10'                                                    
         BAS   RE,VALMAX                                                        
*                                                                               
VALR77H  CLI   RO3,C'A'                                                         
         BNE   VALR77M                                                          
         CLI   RPUB,C' '                                                        
         BNE   VALR77M                                                          
         MVC   RPUB(3),=C'ALL'                                                  
*                                                                               
VALR77M  CLI   BVRNUM+2,C'T'          IF REQUESTING TO INCLUDE TEST             
         BNE   VALR77X                BUYS SET QOPT7 TO 'Y'                     
         MVI   RO7,C'Y'                                                         
*                                                                               
VALR77X  BR    RA                                                               
         SPACE 2                                                                
*                                                                               
VALR91   DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'                                               
         BNE   VALR91C                                                          
         BAS   RE,SOONCLT           MUST BE ONE CLIENT                          
*                                                                               
VALR91C  CLI   RMED,C'M'                                                        
         BE    VR91A                                                            
         CLI   RMED,C'S'                                                        
         BE    VR91A                                                            
         CLI   RMED,C'T'                                                        
         BE    VR91A                                                            
         CLI   RMED,C'I'                                                        
         BE    VR91A                                                            
         CLI   RO6,C'5'            NEWS OR OUTDOOR - SORT CODE 5 ERROR          
         BNE   VR91B                                                            
*                                                                               
VR91SERR MVI   ROUTNUM,X'5D'       CURSOR TO SORT CODE                          
         MVC   FERN,=AL2(FLDINV)                                                
         B     VR91X                                                            
*                                                                               
VR91A    CLI   RO6,C'7'            MAGS - SORT CODE 7 IS ERROR                  
         BE    VR91SERR                                                         
VR91B    CLI   RREG,C' '                                                        
         BE    VR91C                                                            
         CLC   RPAY(2),=C'NO'     CUT-OFF DATE ONLY FOR 'NO' CUR MTH            
         BE    VR91C                                                            
         MVI   ROUTNUM,X'6E'                                                    
         MVC   FERN,=AL2(FLDINV)                                                
         B     VR91X                                                            
*                                                                               
VR91C    CLC   RPRO,=C'ALL'                                                     
         BE    VR91X                                                            
         MVI   RO2,C'N'            SET ALL PRDS TO NO                           
VR91X    BR    RA                                                               
         SPACE 2                                                                
VALR100  DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'                                               
         BNE   VALR100E                                                         
         BAS   RE,SOONCLT        MUST BE ONE CLIENT                             
*                                                                               
         CLI   RO1,C'Y'                                                         
         BE    VALR100E                                                         
         MVI   ROUTNUM,X'39'      CURSOR TO TEST OPTION                         
         B     VALR100F                                                         
*                                                                               
VALR100E CLC   RO2(2),=C'YY'                                                    
         BNE   VALR100X                                                         
         MVI   ROUTNUM,X'3D'       CURSOR TO OPT2                               
         MVC   FERN,=AL2(FLDINV)                                                
         B     VALR100X                                                         
*                                                                               
VALR100F MVC   FERN,=AL2(FLDINV)                                                
         B     VALR100X                                                         
*                                                                               
VALR100X BR    RA                                                               
         SPACE 2                                                                
VALR109  DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'                                               
         BNE   VALR109J                                                         
         BAS   RE,SOONCLT        MUST BE ONE CLIENT                             
*                                                                               
         CLC   RAGY(2),=C'DM'                                                   
         BE    VALR109J                                                         
         CLC   RAGY(2),=C'NE'                                                   
         BE    VALR109J                                                         
         CLC   RAGY(2),=C'NR'                                                   
         BE    VALR109J                                                         
         CLC   RAGY(2),=C'NW'                                                   
         BE    VALR109J                                                         
         CLC   RAGY(2),=C'SJ'                                                   
         BE    VALR109J                                                         
         MVI   ROUTNUM,6                                                        
         CLC   RPRO,=C'ALL'                                                     
         BE    VALR109F                                                         
         CLC   RPRO,SPACES                                                      
         BH    VALR109J                                                         
VALR109F MVC   FERN,=AL2(FLDINV)                                                
         B     VALR109X                                                         
*                                                                               
VALR109J CLC   RNUM(2),=C'L1'  NEEDED SINCE PPL2 ALSO USES VALR109              
         BE    VALR109L                                                         
         CLC   RNUM(2),=C'LB'                                                   
         BNE   VALR109X                                                         
*                                                                               
VALR109L CLI   BVRNUM+2,C'T'            IF REQUESTING TO INCLUDE TEST           
         BNE   VALR109X                 BUYS SET QOPT7 TO 'Y'                   
         MVI   RO7,C'Y'                                                         
*                                                                               
VALR109X BR    RA                                                               
         SPACE 2                                                                
VALR110  DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'       SOON CHECKS BEING DONE HERE             
         BNE   VR110A2            BECAUSE D1,RD, AND E1 ALSO USE                
         BAS   RE,SOONCLT             MUST BE ONE CLIENT                        
*                                                                               
         CLC   RNUM,=C'D1'              D1 NOT RESTRICTED TO ONE                
         BE    VR110A2                  PRODUCT                                 
         CLC   RNUM,=C'RD'              RD NOT RESTRICTED TO ONE                
         BE    VR110A2                  PRODUCT                                 
         MVI   ROUTNUM,6                                                        
         CLC   RPRO,=C'ALL'                                                     
         BNE   VR110A2                                                          
VR110A1  MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VR110A2  DS    0H                                                               
         CLC   RPRO,=C'ALL'        IF ALL PRDS AND ESTIMATE RANGE SET           
         BNE   VR110A5             RSORT TO 01                                  
         CLI   REST,C'0'           SEE IF ESTIMATE GIVEN                        
         BL    VR110A5                                                          
         CLI   RESTEND,C'0'        SEE IF RANGE                                 
         BL    VR110A5                                                          
         MVC   RSORT,=C'01'                                                     
*                                                                               
VR110A5  DS    0H                                                               
         CLC   CLIPROF+20(3),SPACES   IF SPECIAL DRD CLT REQUIRED               
         BNH   VR110A8                                                          
         CLC   RPUB+1(3),=C'RD='   DRD CLT MUST BE ENTERED                      
         BE    VR110A6                                                          
         MVI   ROUTNUM,2                                                        
         MVC   FERN,=AL2(FLDMIS)      FIELD MISSING                             
         BR    RA                                                               
*                                                                               
VR110A6  CLC   RPUB+4(3),CLIPROF+20  AND MUST MATCH CLT IN PROFILE              
         BNE   VR110AE                                                          
         CLC   RDIV,SPACES         SEE IF DIVISION ENTERED                      
         BNE   VR110A8             I SHOULD LEAVE IT ALONE                      
         MVC   RDIV,=C'000'        SET RDIV TO 000                              
         B     VR110A8                                                          
*                                                                               
VR110AE  DS    0H                                                               
         MVI   ROUTNUM,2                                                        
         B     VR110A1            INVALID DRD CLIENT                            
*                                                                               
VR110A8  CLC   RDIS,=C'ALL'        IF DIS=ALL, REG MUST NOT BE BLANK            
         BNE   VR110A                                                           
         CLI   RREG,C' '                                                        
         BNE   *+10                                                             
         MVC   RREG,=C'ALL'                                                     
VR110A   CLC   RREG,=C'ALL'        IF REG=ALL, DIV MUST NOT BE BLANK            
         BNE   VR110B                                                           
         CLI   RDIV,C' '                                                        
         BNE   VR110B                                                           
         CLC   RPRO,=C'ALL'                                                     
         BNE   VR110B                                                           
         MVC   RDIV,=C'ALL'                                                     
*                                                                               
VR110B   BAS   RE,CKINVD           CHK INVOICE DATE COL 28                      
*                                                                               
VR110F   MVC   HALF,=C'B2'         B1/D1 REPORTS READ B2 PROFILE                
         CLC   RNUM,=C'E1'         E1 REPORT READS E2 PROFFILE                  
         BNE   *+10                R1 READS B2 PROFILE                          
         MVC   HALF,=C'E2'                                                      
         BAS   RE,RDPROF                                                        
         OC    TEMP(16),TEMP                                                    
         BNZ   VR110F2                                                          
         MVI   ROUTNUM,2           CURSOR TO CLT                                
         MVC   FERN,=AL2(NOPROF)   NO PROFILE                                   
         BR    RA                                                               
*                                                                               
VR110F2  DS    0H                                                               
*                                                                               
VR110G   DS    0H                                                               
         CLC   TEMP+6(1),RO2                                                    
         BE    VR110H                                                           
         CLI   RPAY,C'P'           GROSS AND FEE CAN BE NEGATIVE                
         BNE   VR110H                                                           
VR110G5  TM    RPAY+1,X'F0'        CHK IF NEGATIVE                              
         BNO   VR110H                                                           
         MVI   ROUTNUM,X'30'       CURSOR TO MANUAL AMT                         
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VR110H   CLI   TEMP+8,C'0'         ALL TYPES ARE OK                             
         BL    VR110I                                                           
         CLC   RO2,TEMP+8          CHECK BILLING TYPE                           
         BNH   VR110I                                                           
         MVI   ROUTNUM,X'76'         CURSOR TO BILLING TYPE                     
         MVC   FERN,=AL2(PROFERR)                                               
         BR    RA                                                               
*                                                                               
VR110I   DS    0H                                                               
*                                                                               
VR110J   OC    TEMP+14(2),TEMP+14     CHK MOS DATE                              
         BZ    VR110JE                                                          
*                                                                               
         LA    R5,RSTRD                                                         
         BAS   RE,VALRFP           CHECK FOR SYMBOLIC EQU                       
         BE    VR110X                                                           
*                                                                               
         MVC   TEMP+30(4),RSTRD                                                 
         MVC   TEMP+34(2),=C'01'       SET DAY TO 01                            
         GOTO1 DATCON,PLIST,(0,TEMP+30),(3,TEMP+20)                             
         CLC   TEMP+20(2),TEMP+14                                               
         BNL   VR110X                                                           
*                                                                               
VR110JE  MVI   ROUTNUM,X'52'       CURSOR TO DATE                               
         MVC   FERN,=AL2(PROFERR)                                               
         BR    RA                                                               
*                                                                               
*                                                                               
VR110X   DS    0H                                                               
         CLI   RO8,C' '                SEE IF SEP COMM OPTION ENTERED           
         BE    VR110X3                                                          
         CLI   RMED,C'*'               DISALLOW FOR MEDIA *                     
         BE    VR110XE                                                          
         CLI   RCLI,C'&&'              DISALLOW FOR BILLING GROUPS              
         BE    VR110XE                                                          
         CLI   RCLI,C'*'               OR OFFICES                               
         BE    VR110XE                                                          
         CLC   RCLI,=C'ALL'            OR ALL CLIENTS                           
         BE    VR110XE                                                          
*                                                                               
VR110X3  MVC   TEMP+30(4),=C'PB2B'        READ B2B PROFILE                      
         NI    TEMP+30,X'BF'       LOWER CASE                                   
         MVC   TEMP+34(6),RAGY                                                  
         CLI   CLIPROF+27,C' '   OFFICE CODE WAS SAVE IN CLIPROF+27             
         BNH   VR110X6                                                          
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIPROF+27                                            
VR110X6  DS    0H                                                               
         L     R5,DATAMGR                                                       
         GOTO1 =V(GETPROF),DMCB,(0,TEMP+30),TEMP,(0(R3),(R5)),RR=RELO           
*                                                                               
         CLI   TEMP,C'Y'           SEPARATE COMMISION BILLING                   
         BNE   VR110X20                                                         
         CLI   RO8,C'C'                                                         
         BE    VR110X8                                                          
         CLI   RO8,C'N'                                                         
         BE    VR110X8                                                          
         CLI   RO8,C'R'                                                         
         BE    VR110X8                                                          
         B     VR110XE            SEP COMM CLIENT MUST HAVE                     
*                                                                               
VR110X20 DS    0H                                                               
         CLI   RO8,C' '            NONE SEP COMM CLIENT CAN'T USE               
         BE    VR110X8                                                          
*                                                                               
VR110XE  MVI   ROUTNUM,X'8C'       CURSOR TO OPTIONS FIELD                      
         MVC   FERN,=AL2(FLDINV)   INVALID                                      
         BR    RA                                                               
*                                                                               
VR110X8  CLI   TEMP+2,0            EST FILTER POSITION 0,1-3                    
         BE    VR110X50                                                         
* - MORE HERE LATER FOR B2B ?                                                   
VR110X50 DS    0H                                                               
*                                                                               
VR110XX  DS    0H                                                               
         BAS   RE,VALDAT                                                        
         BR    RA                                                               
         SPACE 2                                                                
VALR112  DS    0H                                                               
         MVC   RPRO,=C'ALL'                                                     
         MVI   RO5,C'Y'                                                         
         MVC   RCLI,=C'PM '                                                     
         CLC   RAGY,=C'YN'                  SEE IF YNR                          
         BNE   VR112C                                                           
         MVC   RCLI,=C'PMU'                                                     
         B     VR112D                                                           
*                                           YNR ALSO USEES (-X**)               
*                                           IN FILTER                           
VR112C   DS    0H                                                               
         CLC   RAGY,=C'BS'                  SEE IF BACKER                       
         BNE   VR112X                                                           
VR112D   MVC   RESTEND,=X'A75C5C'           X'A7' FOLLOWED BY 2 *'S             
*                                           (-X**)                              
VR112X   BR    RA                                                               
         SPACE 2                                                                
*                                                                               
VALR117  DS    0H                                                               
         CLC   REST,=C'ALL'                                                     
         BE    VALR117X                                                         
         CLC   RESTEND(3),SPACES      L2 - IF SECOND EST GIVEN -                
         BE    VALR117X               SET SORT TO 01                            
         MVC   RSORT,=C'01'           PPG WILL PUT EST INTO SORT                
*                                                                               
VALR117X B     VALR109                REST SAME AS VALR109                      
*                                     LIKE L1 SOON CHECKS                       
*                                                                               
VALR120  CLC   RO2(2),=C'YY'                                                    
         BNE   VALR120X                                                         
         MVI   ROUTNUM,X'82'                                                    
         MVC   FERN,=AL2(FLDINV)                                                
VALR120X BR    RA                                                               
         SPACE 2                                                                
*                                                                               
VALRMY   DS    0H                                                               
         CLC   RPUB(3),SPACES     SEE IF BILLING TYPE GIVEN                     
         BNE   *+10                                                             
         MVC   RPUB(3),=C'ALL'    SET TO ALL                                    
*                                                                               
         PACK  DUB,TODAY+4(2)                                                   
         CVB   R4,DUB                                                           
         LCR   R4,R4               MAKE NEGATIVE                                
*                                  GOTO ADDDAY TO GET LAST MTH                  
         GOTO1 ADDAY,DMCB,TODAY,TEMP+6,(R4)                                     
         MVC   TEMP+10(2),=C'15'      TEMP+6(6) NOW SHOULD BE                   
*                                     15th  DAY OF LAST MONTH                   
         LH    R4,=H'-150'      TO GO BACK 5 MONTHS                             
         GOTO1 ADDAY,DMCB,TEMP+6,TEMP+12,(R4)                                   
         MVC   TEMP+16(2),=C'01'      TEMP+12(6), NOW SHOULD BE                 
*                                 FIRST DAY OF MONTH BEFORE LAST                
         CLC   RPUB+3(6),TEMP+12                                                
         BNL   VALRMY5                                                          
         CLI   RO4,C'M'      MARK OR UNPOST  - SKIP DATE CHECK                  
         BE    VALRMY5                                                          
         CLI   RO4,C'U'                                                         
         BE    VALRMY5                                                          
         MVI   ROUTNUM,145    (X'91')                                           
         MVC   FERN,=AL2(FLDINV)                                                
         B     VALRMYX                                                          
*                                                                               
VALRMY5  CLC   REXTRA+12(6),SPACES                                              
         BE    VALRMYX                                                          
         CLC   REXTRA+12(6),TODAY      INTERFACE DATE                           
         BL    VALRMY10                                                         
VALRMYE  MVI   ROUTNUM,147    (X'93')                                           
         MVC   FERN,=AL2(FLDINV)                                                
         B     VALRMYX                                                          
*                                                                               
VALRMY10 CLC   REXTRA+12(6),TEMP+12     VS. FIRST DAY OF MONTH BEFORE           
         BL    VALRMYE                  LAST                                    
*                                                                               
VALRMYX  BR    RA                                                               
         SPACE 2                                                                
VALRNV   DS    0H                                                               
         CLI   RBPD,C'I'                                                        
         BNE   VALRNVA                                                          
         MVI   RBPD,C' '                                                        
         B     VALRNVB                                                          
VALRNVA  CLI   RBPD,C' '                                                        
         BNE   VALRNVB                                                          
         MVI   RBPD,C'P'                                                        
*                                                                               
VALRNVB  DS    0H                                                               
***************************************                                         
*****    NO-OPED **********************  PNV DOES NOT HAVE AN                   
*****                                    'AS OF DATE' REQUEST FIELD             
*****                                    FOR NOW                                
**                                                                              
**       LA    R5,RPAY             AS OF DATE                                   
**       CLC   REST,=C'ALL'                                                     
**       BNE   *+10                                                             
**       MVC   REST,SPACES         SET ALL TO SPACES                            
**       CLC   0(6,R5),SPACES          AS OF DATE                               
**       BE    VALRNVE                                                          
**       CLI   RO5,C' '                                                         
**       BNE   VALRNVC                                                          
**       MVI   ROUTNUM,84                                                       
**       MVC   FERN,=AL2(ASDINV)                                                
**       B     VALRNVX                                                          
**                                                                              
**LRNVC  DS    0H                                                               
**       CLC   0(6,R5),TODAY                                                    
**       BNH   VALRNVE                                                          
**       MVC   FERN,=AL2(ASOFDPST)                                              
**       MVI   ROUTNUM,84                                                       
**       BR    RA                                                               
**                                                                              
VALRNVE  DS    0H                                                               
         MVC   HALF,=X'2300'           36 MONTHS                                
         BAS   RE,VALMAX                                                        
*                                                                               
VALRNVF  DS    0H                                                               
         CLI   RO7,C'F'         SEE IF FAXING LETTERS                           
         BE    VALRNVG                                                          
         CLI   RO7,C'S'         FAXING LETTERS - NO REPORT                      
         BNE   VALRNVX                                                          
*                                                                               
VALRNVG  DS    0H                                                               
         MVC   HALF,=C'PW'                                                      
         BAS   RE,RDPROF                                                        
*                               RETURNS PROFILE IN TEMP                         
         CLI   TEMP+2,C'Y'      SEE IF NV FAXING ALLOWED                        
         BE    VALRNVH                                                          
VALRNVFE MVC   FERN,=AL2(96)        NOT AUTHORIZED FOR FAXING                   
         MVI   ROUTNUM,128     (X'80)                                           
         BR    RA                                                               
*                                                                               
VALRNVH  DS    0H                                                               
         OC    REQOUT,REQOUT   SEE IF OUTPUT ENTERED                            
         BZ    VALRNVI                                                          
         CLC   REQOUT(6),=CL6'BFX1PC'                                           
         BE    VALRNVX                                                          
         CLC   REQOUT(5),=CL5'DIRECT'                                           
         BE    VALRNVX                                                          
         MVC   FERN,=AL2(FLDINV)                                                
         LA    RE,BVROUTH                                                       
         ST    RE,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALRNVI  DS    0H                                                               
         CLI   TEMP+3,C'Y'         SEE IF JCL OUTPUT IS DIRECT                  
         BE    VALRNVX             YES - DO NOTHING                             
         CLI   TEMP+3,0            SEE IF OPTION NOT SPECIFIED                  
         BE    VALRNVFE                                                         
         MVC   REQOUT(6),=C'BFX1PC'   NO - SET OUTPUT TO BFX1PC                 
*                                                                               
VALRNVX  BR    RA                                                               
         SPACE 2                                                                
VALAR    DS    0H                                                               
         CLI   RO1-1,C'A'         SEE IF ANALYSIS                               
         BNE   VALARL                                                           
         CLI   BVRNUM+2,C'T'          IF REQUESTING TO INCLUDE TEST             
         BNE   VALAR5               BUYS SET QOPT6 TO 'Y'                       
         MVI   RO6,C'Y'                                                         
*                                                                               
VALAR5   CLC   BVROUT(4),=C'SOON'     SEE IF SOON REQ                           
         BNE   VALARL                                                           
*                                     WHICH IN TURN GOES TO VALR12N             
         CLI   RPUB,C'0'              YES - PUB MUST BE GIVEN                   
         BNL   VALARL                                                           
         MVI   ROUTNUM,X'0E'          CURSOR TO PUB                             
         MVC   FERN,=AL2(FLDMIS)      FIELD MISSING                             
         BR    RA                                                               
VALARL   DS    0H                                                               
         CLI   RO1-1,C'R'          SEE IF RATE CHANGE                           
         BE    VALARR                                                           
*                                                                               
*        THESE EDITS APPLY BOTH TO "L" -LISTING AND "A' ANALYSIS                
*                                                                               
*                                                                               
         CLI   RO3,C'Y'                                                         
         BE    VALARL5                                                          
         CLI   RO3,C'N'                                                         
         BE    VALARL5                                                          
         B     VALARL7                                                          
*                                                                               
VALARL5  MVI   ROUTNUM,X'97'                                                    
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VALARL7  DS    0H                                                               
         CLI   RO4,C'Y'            CHGS ONLY                                    
         BNE   VALR12N             REST SAME AS 12                              
         CLC   RCNTDAT(3),SPACES       YES - CONTROL DATE REQUIRED              
         BNE   VALR12N             OK                                           
         MVC   FERN,=AL2(FLDMIS)   MISSING                                      
         MVI   ROUTNUM,X'46'                                                    
VALARLX  BR    RA                                                               
         SPACE 2                                                                
VALARR   DS    0H              EDITS FOR RATE CHANGE                            
         CLI   RPUB,C'0'       PUB MUST BE GIVEN                                
         BNL   VALARR2                                                          
         MVI   ROUTNUM,X'0E'          CURSOR TO PUB                             
         MVC   FERN,=AL2(FLDMIS)      FIELD MISSING                             
         BR    RA                                                               
*                                                                               
VALARR2  CLI   REST,C'0'       CONTRACT MUST BE GIVEN                           
         BNL   VALARR3                                                          
         MVI   ROUTNUM,X'4E'          CURSOR TO CON                             
VALARMIS MVC   FERN,=AL2(FLDMIS)      FIELD MISSING                             
         BR    RA                                                               
*                                                                               
VALARR3  CLC   BVROUT(4),=C'SOON'                                               
         BNE   VALARR4                                                          
         CLI   RO3,C'N'        MEANS 'LIVE' RUN - INVALID FOR SOON              
         BNE   VALARR4                                                          
VALARRE  MVC   FERN,=AL2(FLDINV)      INVALID                                   
         MVI   ROUTNUM,X'97'          CUSROR TO TEST OPTION                     
         BR    RA                                                               
VALARR4  DS    0H                                                               
         CLI   RO3,C'B'         NOT ALLOWED FOR RATE CHANGE                     
         BE    VALARRE                                                          
         CLI   RO3,C' '                                                         
         BNE   VALARR4B                                                         
         MVI   ROUTNUM,X'97'                                                    
         B     VALARMIS                                                         
*                                                                               
VALARR4B CLI   RSTRD,C' '      NO START/END FOR RATE CHG                        
         BE    VALARR5                                                          
         MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,X'10'                                                    
         BR    RA                                                               
*                                                                               
VALARR5  CLC   RSORT,SPACES     NO SORT FOR RATE CHG                            
         BE    VALARR6                                                          
         MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,X'16'                                                    
         BR    RA                                                               
*                                                                               
VALARR6  CLC   RCNTDAT,SPACES    NO CONTROL DATE FOR RATE CHG                   
         BE    VALARR7                                                          
         MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,X'46'                                                    
         BR    RA                                                               
*                                                                               
VALARR7  CLI   RO1,C' '            NO DATE OPTION FOR RATE CHG                  
         BE    VALARR8                                                          
         CLI   RO1,C'N'            NO DATE OPTION FOR RATE CHG                  
         BE    VALARR8                                                          
         MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,X'5A'                                                    
         BR    RA                                                               
*                                                                               
VALARR8  CLI   RO4,C' '         NO CHG OPTION FOR RATE CHANGE                   
         BE    VALARRX                                                          
         MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,X'5F'                                                    
VALARRX  BR    RA                                                               
         SPACE 2                                                                
VALRTS   DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'        SPECIAL CHECKS FOR SOON REQS           
         BNE   VRTSV                                                            
         BAS   RE,SOONCLT                SOON MUST BE ONE CLT                   
*                                                                               
VRTSE5   MVC   HALF,=X'0500'           6 MONTHS                                 
         BAS   RE,VALMAX                                                        
*                                                                               
VRTSV    DS    0H                                                               
         CLC   RAGY,=C'WI'        SEE IF WESTERN                                
         BE    VRTSW                                                            
         CLC   RAGY,=C'WJ'        SEE IF WESTERN                                
         BE    VRTSW                                                            
         CLC   RAGY,=C'MX'        SEE IF WESTERN                                
         BE    VRTSW                                                            
         CLC   RAGY,=C'WT'        SEE IF WESTERN                                
         BE    VRTSW                                                            
         CLC   RAGY,=C'WR'        SEE IF WESTERN                                
         BE    VRTSW                                                            
*                                                                               
         CLI   RO1,C' '         NON-WESTERN MUST BE BLANK,A,N OR X              
         BE    VRTSX                                                            
         CLI   RO1,C'A'                                                         
         BE    VRTSX                                                            
         CLI   RO1,C'N'                                                         
         BE    VRTSX                                                            
         CLI   RO1,C'X'                                                         
         BE    VRTSX                                                            
*                                                                               
VRTSERR  MVI   ROUTNUM,X'99'          CURSOR TO STATUS                          
         MVC   FERN,=AL2(FLDINV)                                                
         BR    RA                                                               
*                                                                               
VRTSW    DS    0H                 SPECIAL VALUES FOR WESTERN                    
*                                 DISALLOW N                                    
         CLI   RO1,C'N'    (ACCEPT BLANK,A,I,M,R AND X)                         
         BE    VRTSERR                                                          
VRTSX    BR    RA                                                               
         EJECT                                                                  
*                                                                               
VALRZ5   DS    0H                                                               
         L     RF,APARM                                                         
         L     RF,16(RF)         ACOMFACS                                       
*                                                                               
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         MVC   0(4,R1),=X'FEFFFFFF' SYS=FE GETS SYSFAC ADD                      
         BASR  RE,RF                                                            
         L     RE,0(R1)                                                         
         L     RF,VSSB-SYSFACD(RE)                                              
         MVC   RNUM+59(1),SSBSYSN1-SSBD(RF)    SYSTEM ID SEE SSB                
*                                                                               
         CLC   =C'SOON',BVROUT                                                  
         BNE   VVZ5X                                                            
         CLI   RO1,C'Y'            MUST BE TEST FOR SOON                        
         BE    VVZ5X                                                            
         MVI   ROUTNUM,X'39'       CURSOR TO TEST RUN                           
         MVI   FERN,FLDINV                                                      
VVZ5X    BR    RA                                                               
         EJECT                                                                  
         DC    F'0'                                                             
RDPROF   ST    RE,*-4                                                           
*                                  GET PROFILE AND RETURN IT IN TEMP            
         MVC   TEMP+30(2),=C'P0'                                                
         MVC   TEMP+32(2),HALF        HALF HAS REPORT NUMBER                    
         MVC   TEMP+34(6),RAGY       AGY/MED/CLT                                
         CLI   CLIPROF+27,C' '                                                  
         BNH   RDP2                                                             
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIPROF+27    OFFICE CODE WAS SAVED IN                
*                                       PCLIPROF+27                             
RDP2     DS    0H                                                               
         L     R5,DATAMGR                                                       
         GOTO1 =V(GETPROF),DMCB,(0,TEMP+30),TEMP,(0(R3),(R5)),RR=RELO           
*                                                                               
RDPROFX  L     RE,RDPROF-4                                                      
         BR    RE                                                               
         EJECT                                                                  
         DC     F'0'                                                            
CKINVD   ST    RE,*-4                                                           
         XC    TEMP,TEMP                                                        
         MVC   TEMP+60(6),TODAY    SET FOR TODAY                                
         CLC   RPAY(6),SPACES                                                   
         BE    CKINV2                                                           
*                                                                               
         LA    R5,RPAY                                                          
         BAS   RE,VALRFP           CHECK FOR SYMBOLIC EQUATE                    
         BE    CKINVX                                                           
*                                                                               
         MVC   TEMP+60(6),RPAY     INVOICE DATE SPECIFIED                       
*                                  USE IT INSTEAD OF TODAY                      
*                                  GET PROFILE AND RETURN IT IN TEMP            
CKINV2   DS    0H                                                               
*                                    FIRST TRY FOR PRINTPAK LK PROFILE          
*                                    NOTE THAT BILLING GROUP REQUESTS           
*                                    AND MEDIA * REQUESTS WILL NOT              
*                                    FIND ANY PRINT LK PROFILES                 
*                                                                               
         MVC   HALF(2),=C'LK'                                                   
         BAS   RE,RDPROF                                                        
         OC    TEMP(16),TEMP                                                    
         BNZ   CKINV09               IF NOT FOUND - TRY FOR ACC                 
*                                                                               
         XC    TEMP(60),TEMP         WILL LEAVE DATE ALONE                      
         MVC   TEMP+30(2),=C'A0'                                                
         MVC   TEMP+32(2),=C'LK'     LOOK FOR LK PROFILE                        
         MVC   TEMP+42(2),RAGY       AGY                                        
CKINV8   DS    0H                                                               
         L     R5,DATAMGR                                                       
         GOTO1 =V(GETPROF),DMCB,(0,TEMP+30),TEMP,(0(R3),(R5)),RR=RELO           
*                                                                               
CKINV09  OC    TEMP(2),TEMP      SEE IF I FOUND PROFILE WITH DATE               
         BZ    CKINV10           OTHER PROFILE OPTIONS IGNORED                  
         MVI   TEMP+2,X'01'                                                     
*                             TEMP(3) IS 3 BYTE PACKED                          
*                                                                               
         GOTO1 DATCON,PLIST,(1,TEMP),(0,TEMP+6)                                 
*    NOTE: SINCE PROFILE DATE WAS PACKED WITHOUT SIGN                           
*          DATCON RETURNS 00 FOR Y2K                                            
*          I MUST GO AGAIN TO GET NEW FORMAT                                    
         GOTO1 DATCON,PLIST,(0,TEMP+6),(0,TEMP+12)                              
*                                                                               
         CLC   TEMP+12(4),TEMP+60  YR AND MTH                                   
         BL    CKINV10              EVEN IF PASSES THIS TEST                    
*                                   STILL CHECK VS LAST MONTH                   
         B     CKINVERR                                                         
*                                                                               
CKINV10  PACK  DUB,TODAY+4(2)                                                   
         CVB   R4,DUB                                                           
         LCR   R4,R4               MAKE NEGATIVE                                
*                                  GOTO ADDDAY TO GET LAST MTH                  
         GOTO1 ADDAY,DMCB,TODAY,TEMP+6,(R4)                                     
         CLC   TEMP+6(4),TEMP+60   YR AND MTH                                   
         BNH   CKINV25                                                          
CKINVERR MVC   FERN,=AL2(LOCKERR)                                               
         MVI   ROUTNUM,X'28'       CURSOR TO INV DATE                           
         BR    RA                                                               
*                                                                               
CKINV25  DS    0H                  STILL CAN'T BE MORE THAN 90 DAYS             
*                                  INTO THE FUTURE                              
         LA    R4,90                                                            
         GOTO1 ADDAY,DMCB,TODAY,TEMP+6,(R4)                                     
         CLC   TEMP+60(4),TEMP+6                                                
         BNH   CKINVX                                                           
CKINVDE  MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,X'28'       CURSOR TO INV DATE                           
         BR    RA                                                               
*                                                                               
CKINVX   L     RE,CKINVD-4                                                      
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
         DS    F                                                                
VALMAX   EQU   *                             START,END DATES DURATION           
*                                                                               
         ST    RE,VALMAX-4           SAVE RETURN REGISTER                       
*                                                                               
         LA    R5,RSTRD                                                         
         BAS   RE,VALRFP             CHECK FOR SYMBOLIC EQU                     
         BE    VALMAXX                                                          
*                                                                               
         MVI   ROUTNUM,16                                                       
         TM    SEDSAVE,X'10'            'ES'                                    
         BO    VALMAXX                                                          
         TM    SEDSAVE,X'20'            'ES' AND NON-SPECIFIC EST               
         BO    VALMAXX                                                          
*                                                                               
         CLI   HALF+1,0                                                         
         BNE   VALMAX4                                                          
         LA    R5,6                          CONVERT TO BINARY AT TEMP          
         LA    R6,RSTRD                                                         
         LA    R7,TEMP                                                          
VALMAX1  CLI   0(R6),X'F9'           SEE IF YEAR IS PAST 1999                   
         BNH   VALMAX3                                                          
         PACK  DUB,1(1,R6)    FIRST JUST DO SECOND DIGIT OF YEAR                
         MVI   DUB+6,X'10'                                                      
         CLI   0(R6),X'FA'    2000'S                                            
         BE    VALMAX3X                                                         
         MVI   DUB+6,X'11'                                                      
         CLI   0(R6),X'FB'    2010'S                                            
         BE    VALMAX3X                                                         
         MVI   DUB+6,X'12'                                                      
         CLI   0(R6),X'FC'    2020'S                                            
         BE    VALMAX3X                                                         
         MVI   DUB+6,X'13'                                                      
         CLI   0(R6),X'FD'    2030'S                                            
         BE    VALMAX3X                                                         
         MVI   DUB+6,X'14'                                                      
         CLI   0(R6),X'FE'    2040'S                                            
         BE    VALMAX3X                                                         
         MVI   DUB+6,X'15'    MUST BE X'FF' - 2050'S                            
         B     VALMAX3X                                                         
*                                                                               
VALMAX3  PACK  DUB,0(2,R6)                                                      
VALMAX3X CVB   R0,DUB                                                           
         STC   R0,0(R7)                                                         
         LA    R6,2(R6)                                                         
         LA    R7,1(R7)                                                         
         BCT   R5,VALMAX1                                                       
*                                                                               
         SR    R5,R5                         CALCULATE DELTA MONTHS             
         SR    R6,R6                                                            
         IC    R5,TEMP+1                                                        
         IC    R6,TEMP+4                                                        
         SR    R6,R5                         R6=(END-STR) MONTH                 
         SR    R7,R7                                                            
         IC    R5,TEMP                                                          
         IC    R7,TEMP+3                                                        
         SR    R7,R5                         R7=(END-STR) YEAR                  
         BM    VALMAXE                                                          
         LR    R5,R6                                                            
         LA    R6,12                                                            
         MR    R6,R6                         R7=(END-STR) YEAR * 12             
         AR    R5,R7                                                            
         CH    R5,=H'255'                    MAX IS X'FF' IN ONE BYTE           
         BH    VALMAXE                       MUST BE ERROR                      
*                                                                               
         STC   R5,HALF1                      HALF1(1) = DELTA MONTHS            
*                                                                               
         CLC   HALF(1),HALF1                                                    
         BNL   VALMAXX                                                          
         B     VALMAXE                                                          
*                                                                               
VALMAX4  DS    0H                                                               
         SR    R7,R7                                                            
         IC    R7,HALF             DOING WEEKS                                  
         SR    R6,R6                                                            
         LA    R6,7                                                             
         MR    R6,R6                                                            
         GOTO1 ADDAY,PLIST,RSTRD,TEMP,(R7)                                      
         CLC   RENDD(6),TEMP                                                    
         BL    VALMAXX                                                          
         B     VALMAXE                                                          
*                                                                               
*                                                                               
VALMAXE  MVC   FERN,=AL2(SEDBIG)             ERROR DURATION TOO LONG            
         BR    RA                                                               
*                                                                               
VALMAXX  L     RE,VALMAX-4          RESTORE RETURN REGISTER                     
         BR    RE                                                               
         SPACE                                                                  
*                                                                               
*              ROUTINE TO CHECK FOR MONTH OF SERVICE BILL DATE LOCKOUT          
*                                                                               
         SPACE 2                                                                
         PRINT NOGEN                                                            
VALDAT   NTR1                                                                   
         LA    R7,PRTREC                                                        
         XC    0(25,R7),0(R7)                                                   
         USING CTIKEY,R7                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAUSRID                                             
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMREAD'),=CL8'CTFILE',(R7),(R7)             
         CLI   DMCB+8,0                                                         
         BNE   LOCKBOK             RECORD NOT FOUND IMPLIES OK                  
*                                                                               
         SPACE                                                                  
         LA    R7,CTIDATA                                                       
VALDATA  ZIC   R1,1(R7)                                                         
         LTR   R1,R1                                                            
         BZ    VALDATB                                                          
         AR    R7,R1                                                            
         CLI   0(R7),X'21'                                                      
         BNE   VALDATA                                                          
         USING CTSYSEL,R7                                                       
         CLI   CTSYSNUM,X'06'                                                   
         BNE   VALDATA                                                          
         MVC   HALF(1),CTSYSAGB                                                 
         SPACE 2                                                                
         USING CTPVD,R5                                                         
         USING CTUREC,R7                                                        
VALDATB  LA    R7,PRTREC           READ LK PROFILE RECORD                       
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,C'U'        USER PROFILE RECORD                          
         MVI   CTUKSYS,C'A'        ACCOUNT SYSTEM                               
         MVC   CTUKPROG+1(2),=C'LK' LK PROGRAM                                  
         MVC   CTUKAGY(1),HALF     COMPANY HEXCOMP                              
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMREAD'),=CL8'CTFILE',(R7),(R7)             
         CLI   DMCB+8,0                                                         
         BNE   LOCKBOK             RECORD NOT FOUND IMPLIES OK                  
         SPACE 1                                                                
         LA    R5,CTUDATA                                                       
         XR    R7,R7                                                            
LOCKB2   CLI   CTPVEL,0                                                         
         BNE   *+6                                                              
         DC    H'0'                NO VALUE ELEMENT FOUND FOR PROFILE           
         CLI   CTPVEL,X'72'                                                     
         BE    LOCKB4                                                           
         IC    R7,CTPVLEN                                                       
         AR    R5,R7                                                            
         B     LOCKB2                                                           
         SPACE 1                                                                
LOCKB4   CLC   RPAY,SPACES         IS THERE AN INVOICE DATE                     
         BE    LOCKB8              IN RPAY(6)                                   
*                                                                               
         ST    R5,FULL             SAVE R5                                      
         LA    R5,RPAY                                                          
         BAS   RE,VALRFP           CHECK FOR SYMBOLIC EQU                       
         BE    LOCKBX                                                           
         L     R5,FULL             RESTORE R5                                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,RPAY),(1,FULL)                                    
LOCKB5   CLC   CTPVALUE(2),FULL    COMPARE YEAR/MONTH PACKED                    
         BL    LOCKBOK                                                          
         MVC   FERN,=AL2(LOCKERR)  MOS BILLING DATE LOCK                        
         MVI   ROUTNUM,X'28'                                                    
         LTR   RB,RB               RETURN BAD CC                                
         B     LOCKBX                                                           
         SPACE 1                                                                
**                           NO INVOICE DATE OVERRIDE - USE TODAY               
LOCKB8   GOTO1 DATCON,DMCB,(0,TODAY),(1,FULL)                                   
         B     LOCKB5                                                           
*                                                                               
*                                                                               
LOCKBOK  CR    RB,RB               RETURN CC EQUAL                              
         DROP  R5,R7                                                            
LOCKBX   XIT1                                                                   
         EJECT                                                                  
         DC    F'0'                                                             
VALFORM  ST    RE,VALFORM-4                                                     
*                             FIRST BE SURE OUTPUT WASN'T SPECIFIED             
*                                                                               
VALF1    DS    0H                                                               
         USING CTPVD,R5                                                         
         USING CTUREC,R7                                                        
VALF5    LA    R7,PRTREC                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,C'U'        USER PROFILE RECORD                          
         MVI   CTUKSYS,C'P'                                                     
         MVC   CTUKSYS+2(2),HALF                                                
         MVC   CTUKAGY,RAGY                                                     
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=CL8'CTFILE',(R7),(R7)                   
         B     VALF12                                                           
*                                                                               
VALF10   GOTO1 DATAMGR,DMCB,(0,DMRSEQ),=CL8'CTFILE',(R7),(R7)                   
*                                                                               
VALF12   CLI   CTUKSYS,C'P'                                                     
         BNE   VALFX                                                            
         CLI   CTUKSYS+1,0                                                      
         BNE   VALFX                                                            
         CLC   CTUKSYS+2(2),HALF                                                
         BNE   VALFX                                                            
         CLC   CTUKAGY(2),RAGY                                                  
         BNE   VALFX                                                            
         CLI   CTUKMED,0              ALL MEDIA - PROCESS                       
         BE    VALF13                                                           
         CLC   CTUKMED,RMED                                                     
         BE    VALF13                OR THIS MEDIA                              
         B     VALF10                                                           
*                                                                               
VALF13   LA    R5,CTUDATA                                                       
         XR    R0,R0                                                            
VALF15   CLI   CTPVEL,0                                                         
         BNE   *+6                                                              
         DC    H'0'                NO VALUE ELEMENT FOUND FOR PROFILE           
         CLI   CTPVEL,X'72'                                                     
         BE    VALF20                                                           
         IC    R0,CTPVLEN                                                       
         AR    R5,R0                                                            
         B     VALF15                                                           
         SPACE 1                                                                
VALF20   DS    0H                                                               
         CLI   CTPVALUE+15,0                                                    
         BE    VALF10              DO SEQ READ                                  
         CLI   CTPVALUE+15,C'*'                                                 
         BE    VALF10              DO SEQ READ                                  
         MVC   FERN,=AL2(FLDINV)                                                
         MVI   ROUTNUM,2           CURSOR TO CLIENT                             
         BR    RA                                                               
*                                                                               
VALFX    L     RE,VALFORM-4                                                     
         BR    RE                                                               
*                                                                               
         DS    F                                                                
SOONCLT  ST    RE,*-4                                                           
         MVI   ROUTNUM,2                 CLIENT CAN'T BE ALL                    
         CLC   RCLI,=C'ALL'                                                     
         BE    SOONC5                                                           
         CLI   RCLI,C'*'                 OR OFFICE                              
         BE    SOONC5                                                           
         CLI   RCLI,C'&&'                OR GROUP                               
         BE    SOONC5                                                           
         CLI   RCLI,C'$'                 OR OFFICE LIST                         
         BE    SOONC5                                                           
         CLC   RCLI,SPACES               OR BLANK                               
         BNE   SOONCX                                                           
SOONC5   MVC   FERN,=AL2(FLDINV)         INVALID INPUT FIELD                    
         BR    RA                                                               
*                                                                               
SOONCX   L     RE,SOONCLT-4                                                     
         BR    RE                                                               
         DROP  R5,R7                                                            
         EJECT                                                                  
*        READ LAST REQUEST RECORD AND UPDATE IT WITH AMENDED DATA               
*                                                                               
AMDREQ   MVC   ADR,LADR                                                         
         GOTO1 DATAMGR,DMCB,(DMIN,DMRDIR),REQUEST,ADR,TEMP,,L'REQREC            
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         MVC   TEMP+26(80),REQREC+26                                            
         GOTO1 DATAMGR,DMCB,(DMIN,DMWRT),REQUEST,ADR,TEMP,,L'REQREC             
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         B     SAVEADR                                                          
         EJECT                                                                  
*        DISPLAY TOTAL REQUEST COUNTS                                           
*                                                                               
TOTREQ   XC    TOTCTR(256),TOTCTR                                               
         XC    TOTCTR+256(256),TOTCTR+256                                       
         XC    ADR,ADR                                                          
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         GOTO1 ,DMCB,(DMIN,DMRSEQ),REQUEST,ADR,REQREC,PRTREC                    
         SPACE 2                                                                
TOTR1    GOTO1 DATAMGR,DMCB,,,,,PRTREC        READ NEXT REQ REC                 
         CLI   DMCB+8,0                                                         
         BE    TOTR2                                                            
         TM    DMCB+8,X'80'                                                     
         BO    TOTR3                                                            
         B     REQIOERR                                                         
         SPACE 2                                                                
TOTR2    CLC   RNUM,=X'FFFF'       IGNORE DUMMYS                                
         BE    TOTR1                                                            
         CLC   RNUM,=C'99'         IGNORE CANCELLED                             
         BE    TOTR1                                                            
*                                                                               
         CLI   LREQOHDR+28,C'*'                                                 
         BE    *+14                                                             
         CLC   RAGY,LREQOHDR+28      FILTER ON AGY                              
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQOHDR(4),LREQOHDR                                             
         BZ    *+14                                                             
         CLC   REQOFFC,LREQOHDR    FILTER ON OFFICE                             
         BNE   TOTR1                                                            
*                                                                               
         TM    REQFLTR,X'01'                                                    
         BZ    *+14                                                             
         CLC   RNAME,LREQOHDR+94                                                
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQOHDR+4(6),LREQOHDR+4                                         
         BZ    *+14                                                             
         CLC   REQOUT,LREQOHDR+4   FILTER ON OUTPUT TYPE                        
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQOHDR+11(2),LREQOHDR+11                                       
         BZ    *+14                                                             
         CLC   REQDEST,LREQOHDR+11 FILTER ON DESTINATION                        
         BNE   TOTR1                                                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,REQNUMB                                                       
         SLL   RE,1                                                             
         LA    RF,TOTCTR(RE)       POINT TO COUNTER AND BUMP                    
         LH    R1,0(RF)                                                         
         LA    R1,1(R1)                                                         
         STH   R1,0(RF)                                                         
         B     TOTR1                                                            
         SPACE 2                                                                
TOTR3    SR    R5,R5               DISPLAY COUNTERS ON MENU SCREEN              
         LA    R6,TOTCTR                                                        
TOTR4    CH    R5,=H'256'                                                       
         BE    TOTRX                                                            
         STC   R5,DUB                                                           
         BAS   RE,GETREQID         SEARCH REQTBL FOR BINARY REQ NUM             
         CLI   DUB+3,0                                                          
         BE    TOTR7               NOT IN REQTBL                                
         SPACE 2                                                                
         LA    RE,BVRFRSTH                                                      
         SR    RF,RF                                                            
TOTR5    CLI   0(RE),0             SEARCH SCREEN FOR ALPHA REQ ID               
         BE    TOTR7                                                            
         CLC   8(2,RE),DUB+1                                                    
         BNE   TOTR6                                                            
         LH    R0,0(R6)            MOVE COUNT TO SCREEN FIELD                   
         CVD   R0,DUB                                                           
         UNPK  10(4,RE),DUB+5(3)                                                
         OI    13(RE),X'F0'                                                     
         CLI   10(RE),C'0'                                                      
         BNE   *+8                                                              
         MVI   10(RE),C'='                                                      
         B     TOTR7                                                            
TOTR6    IC    RF,0(RE)            BUMP SCREEN FIELD                            
         AR    RE,RF                                                            
         B     TOTR5                                                            
         SPACE 2                                                                
TOTR7    LA    R5,1(R5)            BUMP REQNUM                                  
         LA    R6,2(R6)            BUMP TABLE                                   
         B     TOTR4                                                            
         SPACE 2                                                                
TOTRX    XC    TEMP(60),TEMP                                                    
         MVC   TEMP(30),=C'TOTAL REQUEST COUNTS DISPLAYED'                      
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         MVC   BVRHDR,TEMP                   SET HDR MSG                        
         MVC   FERN,=AL2(FF)                                                    
         B     EXIT                                                             
         EJECT                                                                  
*        ADD A NEW REQUEST RECORD TO THE REQUEST CHAIN                          
GETREQID NTR1                                                                   
         SR    R0,R0                                                            
         IC    R0,DUB                                                           
         CVD   R0,DUB                                                           
         STC   R0,DUB                                                           
         UNPK  DUB+1(2),DUB+6(2)                                                
         OI    DUB+2,X'F0'                                                      
         MVI   DUB+3,0             SET NOT FOUND FLAG AND NUMBER VALUE          
         SR    R1,R1                                                            
         L     R7,AREQTBL                                                       
         SPACE 2                                                                
GETRID1  CLI   0(R7),0             SEARCH REQTBL FOR BINARY REQ NUM             
         BE    GETRIDX                                                          
         CLC   DUB(1),1(R7)                                                     
         BE    GETRID2                                                          
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         B     GETRID1                                                          
         SPACE 2                                                                
GETRID2  ST    R7,DUB+4            SET FOUND ADR AND FLAG                       
         MVI   DUB+3,1                                                          
         IC    R1,0(R7)            POINT TO LAST TWO BYTES OF ENTRY             
         AR    R7,R1                                                            
         SH    R7,=H'2'                                                         
         MVC   DUB+1(2),0(R7)      RETURN REQ ALPHA ID                          
         SPACE 2                                                                
GETRIDX  XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
*  VALIDATES SYMBOLIC ESCAPE SEQUENCE - EXPECTS R5=SYMBOLIC NAME FIELD          
VALRFP   NTR1                                                                   
         TM    RFPSTAT,RFPINUSE    ..$RPF                                       
         BZ    VRFPNO                                                           
         L     R4,AIORFP                                                        
         USING RFPBLK,R4                                                        
         ZIC   R0,RFPVNUMS                                                      
VALRFP5  CLC   0(3,R5),RFPVSYME                                                 
         BE    VALRFPX                                                          
         LA    R4,RFPVSYML(R4)                                                  
         BCT   R0,VALRFP5                                                       
VRFPNO   LTR   RE,RE                                                            
*                                                                               
VALRFPX  B     EXIT                                                             
         EJECT                                                                  
DISPMAX  DC    H'13'                                                            
REQUEST  DC    CL8'PREQUEST'                                                    
DMRDIR   DC    CL8'DMRDIR'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
DMRDHI   DC    CL8'DMRDHI'                                                      
*                                                                               
REQDDS   EQU   X'04'                                                            
         SPACE 2                                                                
         LTORG                                                                  
*        EACH REQUEST THAT REQUIRES INDIVIDUAL POST VALIDATION HAS THE          
*        ADDRESS OF THE ROUTINE IN THIS TABLE.                                  
*                                                                               
VALROUTS DS    0CL5                                                             
         DC    AL1(10,0),AL3(VALR10)                                            
         DC    AL1(12,0),AL3(VALR12)                                            
         DC    AL1(14,0),AL3(VALR14)                                            
         DC    AL1(16,0),AL3(VALR16)                                            
         DC    AL1(18,0),AL3(VALR18)                                            
         DC    AL1(19,0),AL3(VALR19)                                            
         DC    AL1(27,0),AL3(VALR27)                                            
         DC    AL1(28,0),AL3(VALR28)                                            
         DC    AL1(36,0),AL3(VALR36)                                            
         DC    AL1(37,0),AL3(VALR37)                                            
         DC    AL1(46,0),AL3(VALR46)                                            
         DC    AL1(48,0),AL3(VALR48)                                            
         DC    AL1(49,0),AL3(VALR49)                                            
         DC    AL1(52,0),AL3(VALR52)                                            
         DC    AL1(54,0),AL3(VALR54)                                            
         DC    AL1(60,0),AL3(VALR60)                                            
*****    DC    AL1(66,0),AL3(VALR66)                                            
         DC    AL1(72,0),AL3(VALR72)                                            
         DC    AL1(74,0),AL3(VALR74)                                            
         DC    AL1(77,0),AL3(VALR77)                                            
         DC    AL1(91,0),AL3(VALR91)                                            
         DC    AL1(100,0),AL3(VALR100)                                          
         DC    AL1(109,0),AL3(VALR109)                                          
         DC    AL1(110,0),AL3(VALR110)                                          
         DC    AL1(112,0),AL3(VALR112)                                          
         DC    AL1(113,0),AL3(VALR60)        SAME AS 60                         
         DC    AL1(115,0),AL3(VALR110)       SAME AS 110                        
         DC    AL1(116,0),AL3(VALR110)       SAME AS 110                        
         DC    AL1(117,0),AL3(VALR117)                                          
         DC    AL1(118,0),AL3(VALR52)        EC  - SAME AS 52                   
         DC    AL1(119,0),AL3(VALR110)       SAME AS 110                        
         DC    AL1(120,0),AL3(VALR120)       RA                                 
         DC    AL1(121,0),AL3(VALRNV)                                           
         DC    AL1(123,0),AL3(VALR110)       RD - SAME AS 110                   
         DC    AL1(124,0),AL3(VALRMY)                                           
         DC    AL1(129,0),AL3(VALR109)       SAME AS 109                        
         DC    AL1(130,0),AL3(VALRTS)                                           
         DC    AL1(212,0),AL3(VALR12)        AU SAME AS 12                      
         DC    AL1(219,0),AL3(VALR19)        AU SAME AS 19                      
         DC    AL1(217,0),AL3(VALAR)                                            
         DC    AL1(228,0),AL3(VALRZ5)                                           
VALROUTX DC    AL1(00,0)                                                        
         EJECT                                                                  
*                                                                               
NEWREQ   CSECT                                                                  
         NMOD1 0,NEWREQ,RR=R9                                                   
         ST    R9,NRELO                                                         
         B     *+8                                                              
*                                                                               
NRELO    DC    F'0'                                                             
*                                                                               
*                                                                               
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         L     R9,4(R1)                                                         
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
*                                                                               
         BAS   RE,SETREQ        SET REMAINING REQ REC DETAILS                   
*                                                                               
         CLC   BVROUT(4),=C'SOON'                                               
         BE    SOONREQ                                                          
*                                                                               
         OI    RHDR+15,X'02'       SET REQ CHAIN FLAG                           
*                                                                               
         TM    RFPSTAT,RFPINUSE    ADDING REQUEST TO GROUP                      
         BZ    NWQ4                                                             
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPRADD                                                
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    NEWCLEAR                                                         
         MVC   BVRHDR,SPACES                                                    
         MVC   BVRHDR(60),=CL60'Request cannot be added to a submitted X        
               group'                                                           
         MVC   FERN,=AL2(FE)                                                    
         CLI   QRFPMODE,QRFPNORO                                                
         BNE   NEWCLEAR                                                         
         MVC   BVRHDR(60),=CL60'Request cannot be added - maximum # of X        
               requests is 50'                                                  
         B     NEWCLEAR                                                         
*                                                                               
NWQ4     DS    0H                                                               
         MVC   ADR,=X'AAAAAAAA'                                                 
         GOTO1 DATAMGR,DMCB,(DMIN,NDMADD),NREQUEST,ADR,REQREC,,L'REQREC         
         CLI   DMCB+8,0                                                         
         BNE   NEWIOERR                                                         
         B     NEWSAVE                                                          
*                                                                               
SOONREQ  DS    0H                                                               
         USING SPOOK,R2                                                         
         XC    TEMP(SPOOKL),TEMP   BUILD SPOOK BLOCK                            
         LA    R2,TEMP                                                          
         MVC   SPOOKUID,USRID      CONNECT ID                                   
         LR    R5,R3                                                            
         USING TWAD,R5                                                          
         MVC   SPOOKAGY,TWAAGY     TWO CHARACTER ID CODE                        
***      MVC   SPOOKAGX,AGYB       HEXCOMP  (USED IN ACC)                       
         USING T412FFD,R5                                                       
         MVC   SPOOKDID,BVROUT+5   USER INITIALS (ID)                           
         CLI   SPOOKDID+2,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+2,C'*'                                                  
         CLI   SPOOKDID+1,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+1,C'*'                                                  
         MVC   SPOOKSYS,=C'PP'     PRINT SYSTEM                                 
         MVC   SPOOKEOD,RNUM                                                    
         MVC   SPOOKJCL,RNUM                                                    
         MVI   SPOOKWEN,2          SET SOON STATUS                              
         OC    REQOUT(4),REQOUT                                                 
         BZ    SOONR5                                                           
*                                 SET OUTPUT FOR SOON REQUESTS                  
         MVC   SPOOKRLD(3),=C'FO='                                              
         MVC   SPOOKRLD+3(4),REQOUT                                             
*                                                                               
SOONR5   L     R4,APARM                                                         
         L     R4,16(R4)           A(COMFACS)                                   
         L     RF,=V(REQTWA)                                                    
         A     RF,NRELO                                                         
         GOTO1 (RF),DMCB,(5,(R3)),REQREC+54,DATAMGR,(R4),(R2)                   
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(38),=C'REPORT XXX,9999 WILL BE PROCESSED SOON'            
         L     RE,8(R1)            GET A(PRTQUE) KEY                            
         MVC   BVRHDR+7(3),2(RE)                                                
         LH    RF,6(RE)                                                         
         LA    R4,BVRHDR+11                                                     
         EDIT  (RF),(4,(R4)),ALIGN=LEFT,WRK=TEMP+60                             
         MVC   FERN,=AL2(FE)       SET FOR MY MESSAGE                           
*                                                                               
NEWRXX   XIT1                                                                   
*                                                                               
NEWIOERR MVC   FERN,=AL2(0)                                                     
         SPACE 2                                                                
NEWCLEAR XC    LADR,LADR    NEWREQ'S CLEARADR                                   
         B     NEWRXX                                                           
*                                                                               
NEWSAVE  MVC   LADR,ADR     NEWREQ'S SAVEADR                                    
         B     NEWRXX                                                           
*                                                                               
         LTORG                                                                  
NREQUEST DC    CL8'PREQUEST'                                                    
NDMADD   DC    CL8'DMADD '                                                      
*                                                                               
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
SETREQ   NTR1                                                                   
         MVI   RHDR+15,X'01'      SET CHAIN REQUEST                             
         CLC   RCARD2,SPACES                                                    
         BE    SETREQX                                                          
         OI    RHDR+15,X'10'      SET 2 REQ CARDS                               
         MVI   RCONT,C'*'                                                       
         MVI   RCARD2+79,C'X' SHOULD PREVENT CLOBBERING OF BINARY DATA          
         B     SETREQX                                                          
*                                                                               
SETREQX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        GETFORM                HALF(2) HAS PROGRAM                             
*                               TEMP(16) HAS REPORT PROFILE                     
*                               RETURNS FORM IN TEMP+20(6)                      
*                                                                               
GETFORM  CSECT                                                                  
         NMOD1 0,GETFORM                                                        
*                                                                               
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         L     R9,4(R1)                                                         
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         XC    TEMP+20(6),TEMP+20                                               
*                                                                               
GETF1    DS    0H                                                               
         MVC   TEMP+30(1),TEMP+15                                               
         LA    RF,FORMTAB                                                       
GETF5    CLC   0(2,RF),=X'FFFF'                                                 
         BE    GETF20                                                           
         CLC   0(1,RF),TEMP+30                                                  
         BE    GETF10                                                           
         LA    RF,7(RF)                                                         
         B     GETF5                                                            
*                                                                               
GETF10   MVC   TEMP+20(6),1(RF)                                                 
         B     GETF20                                                           
*                                                                               
GETF20   DS    0H                                                               
         CLC   BVROUT(4),=C'SOON'                                               
         BNE   GETFX                                                            
         OC    TEMP+20(6),TEMP+20        SEE IF I HAVE A FORM                   
         BZ    GETF40                    NO - MUST READ CONTROL FILE            
*                                                                               
GETF22   MVC   TEMP+30(1),RMED                                                  
         MVI   TEMP+31,C'L'                                                     
         CLC   TEMP+20(4),=C'&&CON'                                             
         BE    GETF23                                                           
         MVI   TEMP+31,C'N'                                                     
         CLC   TEMP+20(4),=C'&&4AS'                                             
         BE    GETF23                                                           
         B     GETF90                   SKIP IF NEITHER OF THOSE                
*                                                                               
GETF23   LA    RF,SFORMTAB                                                      
*                                                                               
GETF25   CLC   0(2,RF),=X'FFFF'          END OF TABLE                           
         BE    GETF90                                                           
         CLC   0(2,RF),TEMP+30                                                  
         BE    GETF30                                                           
         LA    RF,8(RF)                                                         
         B     GETF25                                                           
*                                                                               
GETF30   MVC   TEMP+20(6),2(RF)         MUST SET FORMS TYPE                     
         B     GETFX                                                            
*                                                                               
GETF40   DS    0H                       READ CONTROL FILE                       
*                     FIRST TRY FOR ORIGINATING OFFICE OVERRIDE                 
         LA    R4,PRTREC                                                        
         USING CTPREC,R4                                                        
         XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,C'P'                                                     
         MVI   CTPKSYS,C'P'             PRINTPAK                                
         MVC   CTPKPROG,HALF                                                    
         MVC   CTPKORIG,T412FFD+10                                              
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMREAD'),=CL8'CTFILE',(R4),(R4)             
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    GETF45                                                           
*                                                                               
*                                       NOW TRY FOR DEFAULT                     
         XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,C'P'                                                     
         MVI   CTPKSYS,C'P'             PRINTPAK                                
         MVC   CTPKPROG,HALF                                                    
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMREAD'),=CL8'CTFILE',(R4),(R4)             
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    GETF45                                                           
         B     GETF90            NOT FOUND - JUST LEAVE AS ZEROS                
*                                                                               
GETF45   DS    0H                                                               
         LA    R5,CTPDATA                                                       
GETF47   CLI   0(R5),X'42'                                                      
         BE    GETF48                                                           
         CLI   0(R5),0                                                          
         BE    GETF90             OUTPUT ELEM NOT FOUND                         
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETF47                                                           
*                                                                               
GETF48   DS    0H                                                               
         USING CTOCOD,R5                                                        
         MVC   TEMP+20(6),CTOCODE                                               
         OC    TEMP+20(6),SPACES                                                
         B     GETF22                                                           
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
GETF90   XC    TEMP+20(6),TEMP+20                                               
*                                                                               
GETFX    XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
FORMTAB  DC    C'N',C'&&4AS  '                                                  
         DC    C'L',C'&&CON  '                                                  
         DC    X'FFFF'                                                          
*                                                                               
SFORMTAB DC    C'ML',C'5MAG  '                                                  
         DC    C'SL',C'5MAG  '                                                  
         DC    C'TL',C'5MAG  '                                                  
         DC    C'IL',C'5MAG  '         INTERACTIVE                              
         DC    C'NL',C'5NEW  '                                                  
         DC    C'OL',C'5OUT  '                                                  
*                                                                               
         DC    C'MN',C'XMZN  '                                                  
         DC    C'SN',C'XMZN  '                                                  
         DC    C'TN',C'XMZN  '                                                  
         DC    C'IN',C'XMZN  '          INTERACTIVE                             
         DC    C'NN',C'XNPR  '                                                  
         DC    C'ON',C'XODR  '                                                  
         DC    X'FFFF'                                                          
*                                                                               
         EJECT                                                                  
*        CANCELING REQUESTS                                                     
*                                                                               
CANREQ   CSECT                                                                  
         NMOD1 0,CANREQ,RR=R9                                                   
         ST    R9,CRELO                                                         
         B     *+8                                                              
*                                                                               
CRELO    DC    F'0'                                                             
*                                                                               
*                                                                               
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         L     R9,4(R1)                                                         
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
*                                                                               
         LA    R7,BVRFRSTH                                                      
         LA    R6,BVRHDRH                                                       
         SR    R7,R6                                                            
         AR    R2,R7                                                            
         USING T412FED,R2                                                       
         LA    R4,ENQFRSTH                   R4=A(NEXT TWA LINE)                
         USING DISPLD,R4                                                        
         LH    R5,DISPFLDS                   R5=NUM OF TWA LINES                
         LA    R6,DISPFLDS+4                 R6=A(DISK ADR)                     
         LTR   R5,R5                                                            
         BNZ   CANR1                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
CANR1    TM    DLHDR+4,X'C0'                 ANY INPUT IN CANCEL FIELD          
         BZ    CANR6                         NO                                 
         MVC   ADR,0(R6)                                                        
         GOTO1 DATAMGR,DMCB,(DMIN,CDMRDIR),CREQUEST,ADR,REQREC                  
         CLI   DMCB+8,0                                                         
         BNE   CANR5                                                            
         CLI   DLCANC,0                                                         
         BE    *+12                                                             
         CLI   DLCANC,C' '                                                      
         BNE   CANR1B                                                           
CANR1A   CLC   RNUM,=C'99'         UNCANCELL REQUIRED                           
         BNE   CANR6                                                            
         MVC   DUB(1),REQNUMB                                                   
         BAS   RE,GETREQID                                                      
         MVC   RNUM,DUB+1                                                       
         MVI   DLCANC,C' '                                                      
         B     CANR4                                                            
*                                                                               
CANR1B   CLI   DLCANC,C'U'                                                      
         BNE   *+18                                                             
         CLC   RNUM,=C'99'         ALLOW EXPLICIT UNCANCEL                      
         BE    CANR1A                                                           
         B     CANR1C                                                           
         CLI   DLCANC,C'A'                                                      
         BE    CANR3                                                            
         CLI   DLCANC,C'C'                                                      
         BNE   CANR1C                                                           
         CLC   RNUM,=C'99'         CANCELL REQUIRED                             
         BE    CANR6                                                            
         MVC   RNUM,=C'99'                                                      
         B     CANR4                                                            
*                                                                               
CANR1C   MVC   FERN,=AL2(FLDINV)   INVALID CODE                                 
         ST    R4,FADR                                                          
         B     CANRXX                                                           
         SPACE 2                                                                
CANR3    CLI   DDS,1               ONLY DDS CAN AMEND                           
         BNE   CANR1C                                                           
         CLC   RNUM,=C'99'         CAN ONLY AMEND ACTIVE REQ                    
         BE    CANR1C                                                           
*        CLI   5(R4),78                                                         
*        BE    CANR3B                                                           
*        SR    R1,R1               REDISPLAY IF TRUNC INPUT                     
*        IC    R1,5(R4)                                                         
*        SH    R1,=H'1'                                                         
*        BM    CANR3A                                                           
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        MVC   TEMP(0),8(R4)                                                    
*ANR3A   BAS   RE,ENQDISP                                                       
*        LTR   R1,R1                                                            
*        BM    *+18                                                             
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        MVC   8(0,R4),TEMP                                                     
CANR3B   DS    0H                                                               
         CLI   RHDR+10,110                B1                                    
         BE    CANR3B3                                                          
         CLI   RHDR+10,115                D1                                    
         BE    CANR3B3                                                          
         CLI   RHDR+10,119                R1                                    
         BNE   CANR3B4                                                          
*                                                                               
CANR3B3  MVC   RCARD2(26),DLREQ2          CAN'T ALTER COL 27-35                 
         MVC   RCARD2+35(42),DLREQ2+35                                          
         B     CANR3B5                                                          
*                                                                               
CANR3B4  MVC   RCARD2(77),DLREQ2                                                
*                                                                               
CANR3B5  CLI   RHDR+10,4                                                        
         BNE   CANR3C                                                           
         MVC   RNUM+2(50),DLINE+3     CAN'T CHANGE BINARY FIELDS                
         MVC   RNUM+56(8),DLINE+57                                              
         MVC   RNUM+68(9),DLINE+69                                              
         B     CANR4                                                            
*                                                                               
CANR3C   CLI   RHDR+10,52                                                       
         BE    CANR3C1                                                          
         CLI   RHDR+10,118               EC                                     
         BE    CANR3C1                                                          
         CLI   RHDR+10,18                                                       
         BE    CANR3C1                                                          
         CLI   RHDR+10,217               AR                                     
         BE    CANR3C1                                                          
         CLI   RHDR+10,14                                                       
         BE    CANR3C1                                                          
         CLI   RHDR+10,12                                                       
         BE    CANR3C1                                                          
         CLI   RHDR+10,212               AC                                     
         BE    CANR3C1                                                          
         CLI   RHDR+10,19                 UTL                                   
         BE    CANR3C3                                                          
         CLI   RHDR+10,219               AU                                     
         BE    CANR3C3                                                          
         B     CANR3C5                                                          
*                                                                               
CANR3C1  DS    0H                                                               
         MVC   RNUM+2(55),DLINE+2                                               
*                                      CAN'T CHANGE COL 61                      
*                                      CONTINUATION COLUMN                      
*                                      OR COLS 58-60                            
         MVC   RNUM+61(16),DLINE+61    CAN'T CHANGE BINARY FIELD                
         B     CANR4                                                            
*                                                                               
CANR3C3  DS    0H                                                               
         MVC   RNUM+2(50),DLINE+2                                               
         MVC   RNUM+55(05),DLINE+55    CAN'T CHANGE BINARY FIELD                
         MVC   RNUM+61(16),DLINE+61    CAN'T CHANGE BINARY FIELD                
         B     CANR4                                                            
*                                                                               
CANR3C5  CLI   RHDR+10,110     B1     SEE IF NEW BILLING REQS                   
         BE    CANR3C6                                                          
         CLI   RHDR+10,115     D1     SEE IF NEW BILLING REQS                   
         BE    CANR3C6                                                          
         CLI   RHDR+10,119     R1     SEE IF NEW BILLING REQS                   
         BE    CANR3C6                                                          
         B     CANR3X                                                           
*                                                                               
CANR3C6  MVC   RNUM+2(58),DLINE+2    CAN ONLY CHANGE FIRST 60 BYTES             
         MVC   RNUM+61(3),DLINE+61  AND OPTIONS 1-3                             
         MVC   RNUM+68(9),DLINE+68   AND REQUESTOR                              
*                                TO PREVENT CHANGING OF BINARY DATA             
*                                MANUAL CD IN OPTION 4-7                        
         B     CANR4                                                            
         SPACE 2                                                                
CANR3X   MVC   RNUM+2(58),DLINE+2     NO BINARY FIELDS                          
         MVC   RNUM+61(16),DLINE+61   STILL CAN'T CHANGE COL 61                 
*                                  SO MOVE WHOLE RECORD MINUS LAST 3            
*                                  OF REQUESTOR NAME                            
CANR4    GOTO1 DATAMGR,DMCB,(DMIN,CDMWRT),CREQUEST,ADR,REQREC                   
         CLI   DMCB+8,0                                                         
         BE    CANR6                                                            
CANR5    MVC   FERN,=AL2(0)                  DISK ERROR                         
         ST    R4,FADR                                                          
         B     CANRXX                                                           
         SPACE 2                                                                
CANR6    LA    R4,DLNEXT                     BUMP TO NEXT LINE                  
         TM    DLCANCH+1,X'20'               SEE IF PROTECTED                   
         BZ    *+8                                                              
         LA    R4,DLNEXT                                                        
         ST    R4,DISPADR                                                       
         LA    R6,4(R6)                      BUMP TO NEXT DISK ADR              
         BCT   R5,CANR1                                                         
         SPACE 2                                                                
CANRX    XC    TEMP,TEMP                                                        
         MVC   TEMP(29),=C'REQUEST CANCEL STATUS AMENDED'                       
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         MVC   BVRHDR,TEMP                   SET HDR MSG                        
         MVC   FERN,=AL2(FF)                                                    
*                                                                               
CANRXX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
CREQUEST DC    CL8'PREQUEST'                                                    
CDMRDIR  DC    CL8'DMRDIR'                                                      
CDMWRT   DC    CL8'DMWRT'                                                       
*                                                                               
         EJECT                                                                  
*        SEARCH REQUEST CHAIN AND COUNT/DISPLAY                                 
*                                                                               
ENQRE    CSECT                                                                  
         NMOD1 0,ENQRE,RR=R9                                                    
         ST    R9,ERELO                                                         
         B     *+8                                                              
ERELO    DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         L     R9,4(R1)                                                         
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING TWAD,R3                                                          
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
*                                                                               
         LA    R7,BVRFRSTH                                                      
         LA    R6,BVRHDRH                                                       
         SR    R7,R6                                                            
         AR    R2,R7                                                            
         USING T412FED,R2                    R2=A(1ST FLD IN ENQ)-64            
         LA    R4,ENQFRSTH                                                      
         ST    R4,DISPADR                    R4=A(NEXT TWA LINE NUM)            
*                                                                               
         TWAXC (R4),PROT=Y                 CLEAR SCREEN                         
         LA    RE,28                                                            
         LR    RF,R4                                                            
LOOP1    NI    1(RF),X'DF'                 UNPROTECT                            
         OI    6(RF),X'80'                                                      
         ZIC   R1,0(RF)                                                         
         AR    RF,R1                                                            
         BCT   RE,LOOP1                                                         
*                                                                               
         USING DISPLD,R4                                                        
         XC    SKIPCTR(08),SKIPCTR           SET COUNTERS                       
         SPACE 2                                                                
         XC    ADR,ADR                                                          
         LA    R0,EDMRDIR                                                       
         CLI   REQOPTN,C'N'                                                     
         BNE   ENQR1                                                            
         CLC   DISPFLDS(2),EDISPMAX          WAS THERE PREVIOUS                 
         BL    ENQRE2                        NO                                 
         MVC   REQINCR,=H'2'                 SET TO SKIP 1                      
         LH    R6,DISPFLDS+2                 SEQ OF 1ST = LAST +1               
         AH    R6,DISPFLDS                                                      
         STH   R6,DISPFLDS+2                                                    
         LH    R6,DISPFLDS                   SET ADR TO A(LAST)                 
         SLA   R6,2                                                             
         LA    R6,DISPFLDS(R6)                                                  
         MVC   ADR,0(R6)                                                        
         B     ENQR3                                                            
ENQR1    MVC   DISPFLDS+2(2),REQINCR         SEQ OF 1ST = INPUT VALUE           
         CLI   REQNUM,255                                                       
         BNE   *+12                                                             
         LA    R0,EDMRSEQ                                                       
         B     ENQR3                                                            
         MVC   ADR,=X'000000FF'                                                 
         MVC   ADR(2),LREQOHDR+26                                               
         B     ENQR3                                                            
ENQR2    LA    R0,EDMRSEQ                                                       
         CLI   REQNUM,255                                                       
         BE    ENQR3                                                            
         MVC   ADR,RHDR+16                   SET ADR TO READ NEXT               
         LA    R0,EDMRDIR                                                       
ENQR3    XC    REQREC,REQREC                                                    
         GOTO1 DATAMGR,DMCB,(DMIN,(R0)),EREQUEST,ADR,REQREC,PRTREC              
         CLI   DMCB+8,0                                                         
         BE    ENQR4                                                            
         TM    DMCB+8,X'80'                                                     
         BO    ENQREOF                                                          
         B     EREQIOER                                                         
         SPACE 2                                                                
ENQR4    CLI   REQNUM,255          FILTER OUT CANCELLED FROM ALL OPTION         
         BNE   ENQR4A                                                           
         CLC   RNUM,=X'FFFF'                                                    
         BE    ENQR2                                                            
         CLC   RNUM,=C'99'                                                      
         BE    ENQR2                                                            
         TM    REQFLAG,X'01'       FILTER OUT UNLINKED FROM ALL OPTION          
         BZ    ENQR2                                                            
         B     ENQR4B                                                           
ENQR4A   CLC   RNUM,LREQOHDR+26    FILTER OUT ANY SUNDRY REQUESTS               
         BE    *+14                                                             
         CLC   RNUM,=C'99'                                                      
         BNE   ENQR2                                                            
*                                                                               
ENQR4B   CLI   LREQOHDR+28,C'*'                                                 
         BE    ENQR4C                                                           
         CLC   RAGY,LREQOHDR+28              NO FILTER ON AGENCY                
         BNE   ENQR2                                                            
         SPACE 2                                                                
ENQR4C   OC    LREQOHDR(4),LREQOHDR                                             
         BZ    ENQR4G                                                           
         CLC   REQOFFC,LREQOHDR    FILTER ON OFFICE CODE                        
         BNE   ENQR2                                                            
*                                                                               
ENQR4G   ST    R2,FULL             SAVE DISPLAY SCREEN'S R2                     
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         CLC   BVRNUM(3),=C'EST'    SEE IF LOOKING FOR PP52                     
         BE    ENQR4H                                                           
         CLC   BVRNUM(2),=C'EC'     SEE IF LOOKING FOR PPEC                     
         BE    ENQR4G5                                                          
         CLC   BVRNUM(2),=C'52'     SEE IF LOOKING FOR PP52                     
         BNE   ENQR4K                                                           
ENQR4G5  CLI   BVRNUM+2,C'T'       SEE IF LOOKING FOR 52T'S OR ECT'S            
         BNE   ENQR4H                                                           
*                                                                               
         TM    RNAME,X'40'    FILTER ON REQS WITH LOWER CASE                    
         BO    ENQR4XX             X'40' ON MEANS NORMAL 52                     
         TM    REQFLTR,X'01'         SEE IF FILTERING ON NAME                   
         BZ    ENQR4X5                                                          
         MVC   SVRNAME,LREQOHDR+94                                              
         NI    SVRNAME,X'BF'       SET OFF X'40' FOR COMPARE                    
         CLC   SVRNAME,RNAME                                                    
         BNE   ENQR4XX                                                          
         B     ENQR4X5                                                          
*                                                                               
ENQR4H   TM    RNAME,X'40'                                                      
         BNO   ENQR4XX             SKIP LOWER CASE (52T) OR (ECT)               
         B     ENQR4X              PROCEED WITH CHECKS                          
*                                                                               
SVRNAME  DS    CL12                                                             
*                                                                               
ENQR4K   LA    R1,RO6                                                           
         CLC   BVRNUM(2),=C'18'                                                 
         BE    ENQR4M                                                           
         CLC   BVRNUM(2),=C'19'                                                 
         BE    ENQR4M                                                           
         CLC   BVRNUM(2),=C'AR'                                                 
         BE    ENQR4M                                                           
         CLC   BVRNUM(2),=C'AU'                                                 
         BE    ENQR4M                                                           
         LA    R1,RO7                                                           
         CLC   BVRNUM(2),=C'S2'                                                 
         BE    ENQR4M                                                           
         CLC   BVRNUM(2),=C'L1'                                                 
         BE    ENQR4M                                                           
         CLC   BVRNUM(2),=C'LB'                                                 
         BE    ENQR4M                                                           
         CLC   BVRNUM(2),=C'77'                                                 
         BE    ENQR4M                                                           
         CLC   BVRNUM(2),=C'60'                                                 
         BE    ENQR4M                                                           
         LA    R1,RO6                                                           
         CLC   BVRNUM(3),=C'CAN'       PP18                                     
         BE    ENQR4O                                                           
         CLC   BVRNUM(3),=C'UTL'       PP19                                     
         BE    ENQR4O                                                           
         LA    R1,RO7                                                           
         CLC   BVRNUM(3),=C'TRA'       PP77                                     
         BE    ENQR4O                                                           
         CLC   BVRNUM(3),=C'MSR'       PP60                                     
         BE    ENQR4O                                                           
         CLC   BVRNUM(3),=C'SPS'       PPS2                                     
         BE    ENQR4O                                                           
         CLC   BVRNUM(3),=C'USR'       PPL1                                     
         BE    ENQR4O                                                           
         CLC   BVRNUM(3),=C'BSR'       PPLB                                     
         BE    ENQR4O                                                           
         B     ENQR4X                                                           
*                                                                               
ENQR4M   CLI   BVRNUM+2,C'T'                                                    
         BE    ENQR4P                                                           
ENQR4O   CLI   0(R1),C'Y'                                                       
         BE    ENQR4XX               SKIP TEST BUY REQUESTS                     
         B     ENQR4X                                                           
*                                                                               
ENQR4P   CLI   0(R1),C'Y'                                                       
         BNE   ENQR4XX                                                          
         DROP  R2                                                               
*****                                                                           
ENQR4X   L     R2,FULL                 RESTORE R2                               
         B     ENQR5                  CONTINUE DISPLAY CHECKS                   
*                                                                               
ENQR4X5  L     R2,FULL                                                          
         B     ENQR5AA                SKIP TO MEDIA CHECK                       
*                                                                               
ENQR4XX  L     R2,FULL                                                          
         B     ENQR2                         SKIP THIS REQUEST                  
         USING T412FED,R2                    R2=A(1ST FLD IN ENQ)-64            
*                                                                               
ENQR5    TM    REQFLTR,X'01'                 REQUESTOR FILTER REQUIRED          
         BZ    ENQR5AA                       NO                                 
         MVC   TEMP(12),RNAME                SINCE RNAME MAY HAVE               
         MVC   TEMP+12(12),LREQOHDR+94       SOME LOWER CASES                   
         OC    TEMP(24),=24C' '                                                 
         CLC   TEMP(12),TEMP+12                                                 
         BNE   ENQR2                                                            
ENQR5AA  TM    REQFLTR,X'02'                 MEDIA FILTER REQUIRED              
         BZ    ENQR5B                        NO                                 
         CLC   RMED,LREQOHDR+30              YES TEST MEDIA                     
         BNE   ENQR2                         IGNORE IF DIFFERENT                
         SPACE 2                                                                
ENQR5B   OC    LREQOHDR+4(6),LREQOHDR+4                                         
         BZ    ENQR5C                                                           
         CLC   REQOUT,LREQOHDR+4       FILTER ON OUTPUT TYPE                    
         BNE   ENQR2                                                            
         SPACE 2                                                                
ENQR5C   OC    LREQOHDR+11(2),LREQOHDR+11                                       
         BZ    ENQR5D                                                           
         CLC   REQDEST,LREQOHDR+11      FILTER ON DESTINATION                   
         BNE   ENQR2                                                            
*                                                                               
ENQR5D   DS    0H                                                               
         SPACE 2                                                                
ENQR6    LH    R4,REQINCR                    IGNORE (REQINCR-1) REQS            
         BCTR  R4,0                                                             
         CH    R4,SKIPCTR                                                       
         BE    ENQR7                                                            
         LH    R4,SKIPCTR                                                       
         LA    R4,1(R4)                                                         
         STH   R4,SKIPCTR                                                       
         B     ENQR2                                                            
         SPACE 2                                                                
ENQR7    LH    R5,READCTR                    UPDATE REQ READ COUNTER            
         LA    R5,1(R5)                                                         
         STH   R5,READCTR                                                       
         MVC   TEMP(L'REQREC),REQREC         SAVE REQ REC                       
         CLC   RNUM,=C'99'                                                      
         BNE   *+16                                                             
         LH    R5,CANCCTR                    UPDATE CANCELLED COUNTER           
         LA    R5,1(R5)                                                         
         STH   R5,CANCCTR                                                       
         CLI   REQOPTN,C'L'                  TOTAL OPTION                       
         BE    ENQR2                         YES DO NOT DISPLAY                 
         SPACE 2                                                                
ENQR8    BAS   RE,ENQDISP                    DISPLAY REQUEST                    
         LH    R0,EDISPMAX                                                      
         LH    R1,DISPCTR                                                       
         SR    R0,R1                                                            
         CH    R0,=H'2'                                                         
         BL    ENQRE3                   MUST HAVE 2 LINE AVAILABLE              
         B     ENQR2                         GO FOR ANOTHER REQUEST             
*                                                                               
         SPACE 2                                                                
ENQREOF  OC    READCTR,READCTR                                                  
         BZ    ENQRE2                        NO REQUESTS FOUND                  
         CLI   REQOPTN,C'L'                                                     
         BE    ENQRE1                                                           
         OC    DISPCTR,DISPCTR                                                  
         BZ    ENQRE2                        NO REQUESTS DISPLAYED              
         B     ENQRE3                                                           
         SPACE 2                                                                
ENQRE1   MVC   REQREC(L'REQREC),TEMP         OPTION TOTAL                       
         GOTO1 ENQDISP                       DISPLAY THE LAST REQ               
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(24),=C'REQUEST TOTAL = NNN LIVE'                            
         MVC   TEMP+24(33),=C' - LAST REQUEST NUM NNN DISPLAYED'                
         LH    R6,READCTR                                                       
         SH    R6,CANCCTR                                                       
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+16(3),DUB                                                   
         LH    R6,READCTR                                                       
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+44(3),DUB                                                   
         B     ENQRX                                                            
         SPACE 2                                                                
ENQRE2   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(18),=C'REQUESTS NOT FOUND'                                  
         XC    DISPFLDS(2),DISPFLDS                                             
         B     ENQRX                                                            
         SPACE 2                                                                
ENQRE3   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(31),=C'REQUESTS NNN THRU NNN DISPLAYED'                     
         MVC   TEMP+31(25),=C' - CHANGE CANCEL STATUS ?'                        
         LH    R6,DISPFLDS+2                 GET FIRST SEQ NUM                  
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+09(3),DUB                                                   
         AH    R6,DISPFLDS                   GET LAST = FIRST+TOTAL-1           
         BCTR  R6,0                                                             
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+18(3),DUB                                                   
         MVI   STATUS,1                      SET STATUS FOR INPUT               
         B     ENQRX                                                            
         SPACE 2                                                                
ENQRX    DS    0H              WAS CLEAR REST OF SCREEN                         
*                                                                               
ENQRX4   LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         MVC   BVRHDR,TEMP                   SET HDR MSG                        
         MVC   FERN,=AL2(FF)                                                    
         B     EEXIT                                                            
*                                                                               
EEXIT    XMOD1 1                                                                
         SPACE 2                                                                
EREQIOER MVC   FERN,=AL2(0)                                                     
         XC    LADR,LADR                                                        
         SPACE 2                                                                
         B     EEXIT                                                            
*                                                                               
***********************************************************************         
*                                                                               
*        ROUTINE TO FORMAT REQUEST DATA IN SCREEN DISPLAY LINE                  
*                                                                               
***********************************************************************         
*                                                                               
ENQDISP  NTR1                                                                   
         L     R4,DISPADR          R4=A(NEXT SCR DISP LINE)                     
*                                                                               
         XC    DLINE,DLINE         DISPLAY CANCELLED FLAG                       
*                                                                               
         OC    RCARD2,RCARD2                                                    
         BZ    *+10                                                             
         XC    DLINE2,DLINE2                                                    
*                                                                               
         MVI   DLCANC,C' '                                                      
         MVC   DLNUM,RNUM                                                       
         CLC   RNUM,=C'99'                                                      
         BNE   ENQD0                                                            
         MVI   DLCANC,C'C'                                                      
         MVC   DUB(1),REQNUMB                                                   
         BAS   RE,GETREQD                                                       
         MVC   DLNUM,DUB+1                                                      
*                                                                               
ENQD0    CLI   REQNUM,255          DONT DISPLAY REQ ID FOR SPECIFICS            
         BE    *+10                                                             
         MVC   DLNUM,SPACES                                                     
*                                                                               
         MVC   DLREQ,RAGY          MOVE ENTIRE REQUEST CARD IN                  
*                                                                               
         LA    R6,DLREQ+35         START DATE'S YY                              
         BAS   RE,ENQY2KD                                                       
*                                                                               
         LA    R6,DLREQ+41         END DATE'S YY                                
         BAS   RE,ENQY2KD                                                       
*                                                                               
         LA    R6,DLREQ+12         AS OF DATE'S YY FOR 36, 37                   
         CLI   RHDR+10,36                                                       
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
         CLI   RHDR+10,37                                                       
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
*                                                                               
         LA    R6,DLREQ+50         AS OF DATE'S YY FOR 27, 28                   
         CLI   RHDR+10,27                                                       
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
         CLI   RHDR+10,28                                                       
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
*                                                                               
         LA    R6,DLREQ+50         INV DATE'S YY FOR B1,D1,E1,R1,RD             
         CLI   RHDR+10,110         B1                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
         CLI   RHDR+10,115         D1                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
         CLI   RHDR+10,116         E1                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
         CLI   RHDR+10,119         R1                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
         CLI   RHDR+10,123         RD                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
*                                                                               
         LA    R6,DLREQ+50         CUR MONTH'S YY FOR A8                        
         CLI   RHDR+10,91          A8                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
*                                                                               
         LA    R6,DLREQ+12         CUT-OFF DATE'S YY FOR A8                     
         CLI   RHDR+10,91                                                       
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
*                                                                               
         LA    R6,DLREQ+27         DATE'S YY FOR MY                             
         CLI   RHDR+10,124         MY                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
*                                                                               
         CLC   RCARD2,SPACES                                                    
         BE    ENQD0AH                                                          
         MVC   DLREQ2,RCARD2                                                    
*                                                                               
         LA    R6,DLREQ2+27        REVERSAL DATE'S YY FOR B1,D1,R1              
         CLI   RHDR+10,110         B1                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
         CLI   RHDR+10,115         D1                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
         CLI   RHDR+10,119         R1                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
*                                                                               
         LA    R6,DLREQ2+32        INTERFACE DATE'S YY FOR MY                   
         CLI   RHDR+10,124         MY                                           
         BNE   *+8                                                              
         BAS   RE,ENQY2KD                                                       
*                                                                               
ENQD0AH  OC    DLINE+23(3),SPACES      NEGATIVE EST FILTERS                     
         CLI   RHDR+10,47                                                       
         BNE   *+10                                                             
         OC    DLINE+52(6),SPACES      NEGATIVE FILTER FOR PP47                 
*                                                                               
         CLI   RHDR+10,4                                                        
         BNE   ENQD0AK                                                          
         MVC   DLINE+52(4),SPACES  CLEAR BINARY FIELDS                          
         MVC   DLINE+64(4),SPACES  CLEAR BINARY FIELDS                          
         B     ENQD0X                                                           
*                                                                               
ENQD0AK  CLI   RHDR+10,52                                                       
         BE    ENQD0B                                                           
         CLI   RHDR+10,118           EC                                         
         BE    ENQD0B                                                           
         CLI   RHDR+10,18                                                       
         BE    ENQD0B                                                           
         CLI   RHDR+10,217           AR                                         
         BE    ENQD0B                                                           
         CLI   RHDR+10,14                                                       
         BE    ENQD0B                                                           
         CLI   RHDR+10,12                                                       
         BE    ENQD0B                                                           
         CLI   RHDR+10,212           AC                                         
         BE    ENQD0B                                                           
         CLI   RHDR+10,19            UTL                                        
         BE    ENQD0B5                                                          
         CLI   RHDR+10,219           AU                                         
         BE    ENQD0B5                                                          
         B     ENQD0C                                                           
*                                                                               
ENQD0B   MVC   DLINE+57(3),SPACES     BINARY CONTROL DATE                       
         B     ENQD0X                                                           
ENQD0B5  MVC   DLINE+52(3),SPACES                                               
         B     ENQD0X                                                           
*                                                                               
ENQD0C   CLI   RHDR+10,110           B1                                         
         BE    ENQD0D                                                           
         CLI   RHDR+10,115           D1                                         
         BE    ENQD0D                                                           
         CLI   RHDR+10,119           R1                                         
         BE    ENQD0D                                                           
         B     ENQD0F                                                           
ENQD0D   MVC   DLINE+64(4),SPACES    CLEAR BINARY MANUAL CD                     
*                                                                               
         CLI   DLREQ2+26,C'G'                                                   
         BE    ENQD0E                                                           
         CLI   DLREQ2+26,C'F'                                                   
         BE    ENQD0E                                                           
         CLI   DLREQ2+26,C'P'                                                   
         BE    ENQD0E                                                           
         B     ENQD0F                                                           
*                                                                               
ENQD0E   MVC   DLREQ2+27(4),SPACES    CLEAR BINARY FIELDS                       
         B     ENQD0X                                                           
*                                                                               
ENQD0F   CLI   RHDR+10,79                                                       
         BNE   ENQD0G                                                           
         CLI   DLINE+61,C'Y'                                                    
         BE    ENQD0X                                                           
         CLI   DLINE+61,C'N'                                                    
         BE    ENQD0X                                                           
         MVI   DLINE+61,C' '           MIGHT BE BINARY                          
         B     ENQD0X                                                           
*                                                                               
ENQD0G   B     ENQD0X                                                           
*                                                                               
*                                                                               
ENQD0X   FOUT  DLCANCH                                                          
         FOUT  DLHDR                                                            
         OC    RCARD2,RCARD2                                                    
         BZ    ENQD0X5                                                          
         OC    DLINE,SPACES                                                     
         FOUT  DLHDR2                                                           
*                                                                               
ENQD0X5  CLI   STATUS,3                                                         
         BNE   ENQDISPX                                                         
         LA    R4,DLNEXT                                                        
         OC    DLINE,SPACES                                                     
         CLC   DLINE,SPACES       DO WE HAVE SOMETHING IN NEXT LINE             
         BE    ENQD1                                                            
         OI    DLCANCH+1,X'20'      YES - PROTECT CANCEL FIELD                  
         CLI   DDS,1             IF NOT DDS PROTECT LINE 2 AS WELL              
         BE    *+8                                                              
         OI    DLHDR+1,X'20'                                                    
         LA    R4,DLNEXT            NEXT LINE                                   
         LH    R6,EDISPMAX                                                      
         BCTR  R6,0                                                             
         STH   R6,EDISPMAX                                                      
*                                                                               
         SPACE 2                                                                
ENQD1    DS    0H                                                               
         ST    R4,DISPADR                                                       
         LH    R6,DISPCTR                    UPDATE DISPLAY COUNTER             
         LA    R6,1(R6)                                                         
         STH   R6,DISPCTR                                                       
         STH   R6,DISPFLDS                                                      
         SLA   R6,2                                                             
         LA    R6,DISPFLDS(R6)                                                  
         MVC   0(4,R6),ADR                   SAVE DISK ADR                      
         B     ENQDISPX                                                         
*                                                                               
*  R6 POINTS TO YEAR DIGIT TO BE CORRECTED                                      
*                                                                               
ENQY2KD  CLI   0(R6),X'FA'         CORRECT Y2K DISPLAY PROBLEM                  
         BNE   *+8                                                              
         MVI   0(R6),C'0'          YEAR 2000                                    
         CLI   0(R6),X'FB'                                                      
         BNE   *+8                                                              
         MVI   0(R6),C'1'          YEAR 2010                                    
         CLI   0(R6),X'FC'                                                      
         BNE   *+8                                                              
         MVI   0(R6),C'2'          YEAR 2020                                    
         CLI   0(R6),X'FD'                                                      
         BNE   *+8                                                              
         MVI   0(R6),C'3'          YEAR 2030                                    
         CLI   0(R6),X'FE'                                                      
         BNE   *+8                                                              
         MVI   0(R6),C'4'          YEAR 2040                                    
         CLI   0(R6),X'FF'                                                      
         BNE   *+8                                                              
         MVI   0(R6),C'5'          YEAR 2050                                    
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
ENQDISPX XIT1                                                                   
         EJECT                                                                  
*        CONVERT BINARY REQNUM IN DUB(1) TO ALPHA IN DUB+1(2)                   
*                                                                               
GETREQD  NTR1                                                                   
         SR    R0,R0                                                            
         IC    R0,DUB                                                           
         CVD   R0,DUB                                                           
         STC   R0,DUB                                                           
         UNPK  DUB+1(2),DUB+6(2)                                                
         OI    DUB+2,X'F0'                                                      
         MVI   DUB+3,0             SET NOT FOUND FLAG AND NUMBER VALUE          
         SR    R1,R1                                                            
         L     R7,AREQTBL                                                       
         SPACE 2                                                                
GETRD1   CLI   0(R7),0             SEARCH REQTBL FOR BINARY REQ NUM             
         BE    GETRDX                                                           
         CLC   DUB(1),1(R7)                                                     
         BE    GETRD2                                                           
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         B     GETRD1                                                           
         SPACE 2                                                                
GETRD2   ST    R7,DUB+4            SET FOUND ADR AND FLAG                       
         MVI   DUB+3,1                                                          
         IC    R1,0(R7)            POINT TO LAST TWO BYTES OF ENTRY             
         AR    R7,R1                                                            
         SH    R7,=H'2'                                                         
         MVC   DUB+1(2),0(R7)      RETURN REQ ALPHA ID                          
         SPACE 2                                                                
GETRDX   XIT1                                                                   
         SPACE 2                                                                
EDISPMAX DC    H'14'                                                            
EREQUEST DC    CL8'PREQUEST'                                                    
EDMRDIR  DC    CL8'DMRDIR'                                                      
EDMRSEQ  DC    CL8'DMRSEQ'                                                      
         LTORG                                                                  
*                                                                               
FLDMIS   EQU   01                            MISSING INPUT FLD                  
FLDINV   EQU   02                            INVALID INPUT FLD                  
ESTERR   EQU   16                                                               
INVLST   EQU   25                                                               
SEDSGE   EQU   80                            START DATE GT END DATE             
SEDNIE   EQU   81            DATES NOT WITHIN EST PERIOD                        
MOSERR   EQU   81            DATES NOT WITHIN EST PERIOD                        
SEDBIG   EQU   69                            DATE SPREAD TOO LONG               
ASDINV   EQU   95       AS OF DATE FOR PAID OR UNPAID ITEMS ONLY                
PUBREQ   EQU   160                                                              
ESTREQ   EQU   164                                                              
ESTBLK   EQU   165                                                              
SAMMTH   EQU   169                                                              
ASOFDPST EQU   116                                                              
LOCKERR  EQU   206            BILLING MONTH LOCKOUT                             
NOPROF   EQU   250                                                              
PROFERR  EQU   251                                                              
         EJECT                                                                  
WORKD    DSECT                                                                  
DISPADR  DS    F                             A(NEXT DISP LINE ON SCR)           
SKIPCTR  DS    H                             NUM OF RECS SKIPPED                
READCTR  DS    H                             NUM OF RECS READ                   
CANCCTR  DS    H                             NUM OF RECS CANCELLED              
DISPCTR  DS    H                             NUM OF RECS DISPLAYED              
TOTCTR   DS    256H                                                             
*                                                                               
         SPACE 2                                                                
DISPLD   DSECT                                                                  
DLCANCH  DS    CL8                                                              
DLCANC   DS    CL1                                                              
DLHDR    DS    CL8                                                              
DLINE    DS    0CL77                                                            
DLNUM    DS    CL2                                                              
DLREQ    DS    CL75                                                             
DLNEXT   EQU   *                                                                
         ORG   *+DLHDR-DISPLD                                                   
DLHDR2   DS    CL8                                                              
DLINE2   DS    0CL77                                                            
DLREQ2   DS    CL77                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRREQSAVE                                                      
       ++INCLUDE PRREQTEMP                                                      
         EJECT                                                                  
       ++INCLUDE PRREQFFD                                                       
         EJECT                                                                  
       ++INCLUDE PRREQFED                                                       
       ++INCLUDE FLDIND                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE GERFPIOD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASSB                                                          
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098PRREQ02S  05/01/02'                                      
         END                                                                    
