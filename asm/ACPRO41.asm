*          DATA SET ACPRO41    AT LEVEL 031 AS OF 03/03/11                      
*PHASE T60B41A                                                                  
         TITLE 'T60B41 - CATEGORY MAINT/LIST'                                   
T60B41   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B41**,R8                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK AREA                    
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    CAT020                                                           
         CLI   MODE,VALREC                                                      
         BE    CAT040                                                           
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
* VALKEY  LOGIC                                                                 
*                                                                               
*                                                                               
CAT020   LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALHED           VALIDATE SCREEN                              
*                                                                               
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       IS RECORD/ACTION CHANGE ?                    
         BNE   *+8                 NO                                           
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         CLI   KEYCHG,C'Y'         DID KEY FIELDS CHANGE ?                      
         BNE   *+8                 NO                                           
         MVI   INTMODE,FSTLIST     YES, SET FIRST TIME LIST                     
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
         XC    CLEARNEW,CLEARNEW                                                
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
* VALREC  LOGIC - DISPLAY OR CHANGE                                             
*                                                                               
*                                                                               
CAT040   BAS   RE,SETSCR           SET SCREEN LINE ADDRESSES                    
         CLI   INTMODE,FSTLIST     IS THIS FIRST TIME LIST ?                    
         BE    CAT060              YES                                          
         BAS   RE,PROCPF           NO, PROCESS PFKEYS                           
         BE    CAT080              FORCE EDIT IF PF10                           
         BAS   RE,TSTEDT           ANYTHING TO EDIT ?                           
         BE    CAT080              YES                                          
*                                                                               
CAT060   BAS   RE,DISLOGIC                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
*                                                                               
CAT080   BAS   RE,EDT              EDIT SCREEN AND HANDLE MOVES AND/OR          
         BAS   RE,TSTMOVE           UPDATES, IF ANY                             
         BE    MVELOGIC                                                         
         CLI   UPDATE,C'Y'                                                      
         BE    UPDLOGIC                                                         
         L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         MVI   INTMODE,DISLIST                                                  
         MVC   NXTSEQ,FSTSEQ                                                    
         BAS   RE,DISLOGIC                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CLEAR SCREEN, READ RECORDS AND DISPLAY THEM. MAKE ANY UNUSED LINES  *         
* AVAILABLE FOR ADDS.                                                 *         
***********************************************************************         
*                                                                               
DISLOGIC NTR1                                                                   
         GOTO1 VCLEARF,DMCB,AFSTSEL,AENDSCR    CLEAR FIELDS AND TURN            
         GOTO1 (RF),(R1),(1,AFSTSEL),APFFLD    ON TRANSMIT BIT                  
         MVI   NUMLINES,0                                                       
         MVI   NEWLINES,0                                                       
         LA    RE,LSELTAB          CLEAR TABLE                                  
         LA    RF,L'LSELTAB                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   INTMODE,NEWLIST                                                  
         BNE   DISL020                                                          
         SR    R1,R1               IF NEW SCREEN REQUESTED, GET TO              
         IC    R1,LSTSEQ            END MARKER AND UNPROTECT LINES              
         LA    R1,1(R1)                                                         
         STC   R1,FSTSEQ                                                        
         MVI   INTMODE,EDTLIST                                                  
         MVC   CONHEAD(L'NEWMSG),NEWMSG                                         
         B     DISL080                                                          
*                                                                               
DISL020  CLI   INTMODE,FSTLIST     IF FIRST TIME, CLEAR LAST SCHEME             
         BNE   DISL040                                                          
         XC    LLASTSCH,LLASTSCH                                                
*                                                                               
DISL040  BAS   RE,READLST          READ AND DISPLAY                             
*                                                                               
         L     R2,AFSTSEL                                                       
         CLI   NUMLINES,MAXLINE    IS SCREEN IS FULL ?                          
         BNE   DISL060             NO                                           
         CLC   NXTSEQ,LSTSEQ       YES, ANY SEQUENCE CODES LEFT                 
         BL    FULLMESS            YES, INDICATE MORE TO COME                   
*                                                                               
DISL060  SR    R3,R3               PRINT MESSAGE AND CLEAR LAST                 
         ICM   R3,1,NUMLINES        SCHEME                                      
         BNZ   *+8                                                              
         LA    R2,CATSCHMH                                                      
         XC    LLASTSCH,LLASTSCH                                                
         MVC   CONHEAD(L'DONEMSG),DONEMSG                                       
*                                                                               
DISL080  L     R2,AFSTSEL          CLEAR BLANK LINES, IS SPACE                  
         LA    R3,MAXLINE           AVAILABLE                                   
         SR    R1,R1                                                            
         ICM   R1,1,NUMLINES                                                    
         BZ    DISL100                                                          
*                                                                               
         SR    R3,R1                                                            
         BNP   DISLOGX                                                          
*                                                                               
         MH    R1,=Y(MAXFIELD)     GET TO NEXT AVAILABLE LINE                   
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
*                                                                               
DISL100  STC   R3,NEWLINES                                                      
*                                                                               
DISL120  BAS   RE,SETLINE          IF LAST SCREEN, UNPROTECT REMAINING          
         L     R2,ASEL              LINES (ALL IF INTMODE=NEWLIST)              
         NI    1(R2),X'FF'-X'20'                                                
         L     R2,ACODE                                                         
         NI    1(R2),X'FF'-X'20'                                                
         L     R2,ANEXTSEL                                                      
         BCT   R3,DISL120                                                       
*                                                                               
         L     R2,AFSTSEL                                                       
*                                                                               
DISLOGX  BAS   RE,TSTMOVE                                                       
         BE    MVELOGIC                                                         
         ST    R2,ACURFORC                                                      
         B     XIT                                                              
         SPACE 3                                                                
FULLMESS MVC   CONHEAD(L'FULLMSG),FULLMSG                                       
         B     DISLOGX                                                          
         EJECT                                                                  
***********************************************************************         
* BUILD NEW SEQUENCE TABLE AND ADD A6 ELEMENT TO SCHEME RECORD        *         
***********************************************************************         
*                                                                               
MVELOGIC CLC   MOVE,BEFORE         IF MOVE IS SAME AS BEFORE OR AFTER           
         BE    MVEERR              WE HAVE A PROBLEM                            
         CLC   MOVE,AFTER                                                       
         BE    MVEERR                                                           
*                                                                               
         BAS   RE,GETA6                                                         
         LA    R6,ELEMENT                                                       
         USING ACSCD,R6                                                         
         LA    R5,SEQTABLE                                                      
         LA    R3,0                COUNT OF RECORDS IN NEW SEQUENCE             
         LA    R1,0                INDEX INTO SEQUENCE TABLE                    
*                                                                               
MVEL020  OC    0(2,R5),0(R5)       IS ANYTHING HERE ?                           
         BZ    DELETEIT            NO, MUST HAVE BEEN DELETED                   
         CLC   0(2,R5),MOVE        YES, IS THIS TO BE MOVED ?                   
         BE    MOVEIT              YES, FIND WHERE                              
         CLC   0(2,R5),BEFORE      NO, SHOULD SOMETHING COME BEFORE ?           
         BE    BEFOREIT            YES, FIND OUT WHAT                           
*                                                                               
MVEL040  MVC   ACSCAT,0(R5)        NO, PUT INTO NEW ELEMENT                     
         LA    R6,L'ACSCAT(R6)                                                  
         LA    R3,1(R3)                                                         
         CLC   0(2,R5),AFTER       SHOULD SOMETHING FOLLOW THIS ?               
         BE    AFTERIT             YES, FIND OUT WHAT                           
*                                                                               
MVEL060  LA    R5,L'ACSCAT(R5)                                                  
         LA    R1,2(R1)                                                         
         CLI   0(R5),X'EE'                                                      
         BNE   MVEL020                                                          
*                                                                               
         BAS   RE,REWRITE                                                       
         XC    ACTTABLE,ACTTABLE                                                
         BAS   RE,DISLOGIC                                                      
         MVC   CONHEAD(L'MOVEMSG),MOVEMSG                                       
         B     XIT                                                              
*                                                                               
MVEERR   MVC   CONHEAD(L'WAITMSG),WAITMSG                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINES FOR MVELOGIC                                            *         
***********************************************************************         
*                                                                               
AFTERIT  BAS   RE,FINDIT                                                        
DELETEIT STC   R1,NXTSEQ                                                        
         B     MVEL060                                                          
         SPACE 3                                                                
BEFOREIT BAS   RE,FINDIT                                                        
         STC   R1,NXTSEQ                                                        
         B     MVEL040                                                          
         SPACE 3                                                                
MOVEIT   MVC   MOVEHOLD,0(R5)      SAVE MOVE DATA UNTIL WE FIND A               
         XC    0(L'ACSCAT,R5),0(R5)    PLACE FOR IT                             
         B     MVEL060                                                          
         SPACE 3                                                                
FINDIT   OC    MOVEHOLD,MOVEHOLD   DO WE HAVE DATA TO BE MOVED ?                
         BZ    FIND040             NO, GO FIND IT                               
*                                                                               
FIND020  MVC   ACSCAT,MOVEHOLD     YES, GET IT FROM HOLD                        
         XC    MOVEHOLD,MOVEHOLD                                                
         LA    R6,L'ACSCAT(R6)                                                  
         LA    R3,1(R3)                                                         
         BR    RE                                                               
*                                                                               
FIND040  LR    R7,R5               SAVE CURRENT SEQUENCE ADDRESS                
*                                                                               
FIND060  CLC   0(2,R7),MOVE        IS THIS THE ONE TO MOVE ?                    
         BE    FIND080             YES                                          
         LA    R7,L'ACSCAT(R7)     NO, KEEP LOOKING                             
         CLI   0(R7),X'EE'                                                      
         BNE   FIND060                                                          
         DC    H'0'                MOVE MUST BE THERE                           
*                                                                               
FIND080  MVC   MOVEHOLD,0(R7)      SAVE THE CODE AND CLEAR THE SLOT             
         XC    0(L'ACSCAT,R7),0(R7)                                             
         B     FIND020                                                          
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE SEQUENCE TABLE WITH ADDS AND/OR DELETES                      *         
***********************************************************************         
*                                                                               
UPDLOGIC BAS   RE,GETA6                                                         
         LA    R6,ELEMENT                                                       
         USING ACSCD,R6                                                         
         LA    R5,SEQTABLE                                                      
         LA    R3,0                KEEP TRACK OF # OF RECORDS IN NEW            
         LA    R1,0                INDEX INTO SEQUENCE TABLE                    
*                                                                               
UPDL020  OC    0(2,R5),0(R5)                                                    
         BNZ   UPDL040                                                          
         BAS   RE,SETNXT                                                        
         B     UPDL060                                                          
*                                                                               
UPDL040  CLC   0(2,R5),=X'FFFF'    SKIP OVER "SLUSH" ACCOUNT                    
         BE    UPDL060                                                          
         MVC   ACSCAT,0(R5)                                                     
         LA    R3,1(R3)                                                         
         LA    R6,L'ACSCAT(R6)                                                  
*                                                                               
UPDL060  LA    R5,L'ACSCAT(R5)                                                  
         LA    R1,2(R1)                                                         
         CLI   0(R5),X'EE'                                                      
         BNE   UPDL020                                                          
*                                                                               
         MVC   ACSCAT,=X'FFFF'     PUT "SLUSH" BACK AT END                      
         LA    R3,1(R3)                                                         
         BAS   RE,REWRITE                                                       
         MVI   INTMODE,FSTLIST                                                  
         BAS   RE,DISLOGIC                                                      
         B     XIT                                                              
         SPACE 3                                                                
SETNXT   CLM   R1,1,NXTSEQ         GET DISPLACEMENT OF FIST SEQUENCE            
         BNL   *+8                  DELETED TO USE FOR REDISPLAY                
         STC   R1,NXTSEQ                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO VALIDATE THE HEADING FIELD(S)                         *         
***********************************************************************         
*                                                                               
VALHED   NTR1                                                                   
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,0                                                         
         LA    R2,CATSCHMH         SCHEME IS REQUIRED                           
         BAS   RE,TSTKEY                                                        
         GOTO1 VALSCH                                                           
         MVC   QSCHM,SCHEME                                                     
*                                                                               
         LA    R2,CATSNAMH                                                      
         MVC   8(L'ACSDNAME,R2),SCHMNAME                                        
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         TRANSMIT THE FIELD                           
*                                                                               
         LA    R2,CATSCRLH         SCROLL AMOUNT IS OPTIONAL                    
         CLI   5(R2),0                                                          
         BE    VALHEDX                                                          
         GOTO1 VALINUM                                                          
         MVC   SCROLL,ACTUAL                                                    
*                                                                               
VALHEDX  CLI   KEYCHG,C'Y'         DID SOMETHING CHANGE ?                       
         BNE   XIT                 NO, EXIT                                     
         XC    CLEARNEW,CLEARNEW   YES, CLEAR FIELDS                            
         B     XIT                                                              
         SPACE 3                                                                
TSTKEY   TM    4(R2),X'20'         HAS FIELD CHANGED ?                          
         BOR   RE                  NO, EXIT                                     
         OI    4(R2),X'20'         YES, INDICATE VALIDATED                      
         MVI   KEYCHG,C'Y'          AND SET SWITCH                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO PROCESS PFKEYS                                        *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         L     R2,AFSTSEL                                                       
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    PROCPFN             NOTHING ON SCREEN TO PROCESS                 
*                                                                               
         CLI   PFKEY,PF10                                                       
         BNE   PROCPF9                                                          
         XC    ACTTABLE,ACTTABLE                                                
         BAS   RE,CLEARSEL                                                      
         B     PROCPFY                                                          
*                                                                               
PROCPF9  CLI   PFKEY,PF9                                                        
         BNE   PROCPF8                                                          
         OC    LLASTSCH,LLASTSCH                                                
         BNZ   NOTLAST                                                          
         MVI   INTMODE,NEWLIST                                                  
         B     PROCPFN                                                          
*                                                                               
PROCPF8  CLI   PFKEY,PF8           SCROLL FORWARDS                              
         BNE   PROCPF7                                                          
         SR    R3,R3                                                            
         IC    R3,FSTSEQ                                                        
         SR    R1,R1                                                            
         IC    R1,SCROLL           USE SCROLL OR MAXIMUM                        
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,MAXLINE          MULTIPLY NUMBER OF LINES TIMES 2             
         SLL   R1,1                 TO GET MAXIMUM CODES PER SCREEN             
         AR    R3,R1               ADD TO STARTING DISPLACEMENT                 
         SR    R1,R1                                                            
         IC    R1,LSTSEQ                                                        
         CR    R3,R1               IF HIGHER THAN LAST SEQUENCE                 
         BNH   *+8                 FORCE DISPLACEMENT OF 0                      
         LA    R3,0                                                             
         B     PROCPF78                                                         
*                                                                               
PROCPF7  CLI   PFKEY,PF7           SCROLL BACKWARDS                             
         BNE   PROCPFN                                                          
         SR    R3,R3                                                            
         IC    R3,FSTSEQ                                                        
         SR    R1,R1                                                            
         IC    R1,SCROLL           USE SCROLL OR MAXIMUM                        
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,MAXLINE          MULTIPLY NUMBER OF LINES TIMES 2             
         SLL   R1,1                 TO GET MAXIMUM CODES PER SCREEN             
         SR    R3,R1               SUBTRACT FROM STARTING DISPLACEMENT          
         BNM   *+8                 IF NEGATIVE, FORCE DISPLACEMENT OF 0         
         LA    R3,0                                                             
*                                                                               
PROCPF78 STC   R3,NXTSEQ                                                        
         MVI   INTMODE,DISLIST                                                  
*                                                                               
PROCPFN  LTR   R8,R8                                                            
         B     PROCPFX                                                          
*                                                                               
PROCPFY  CR    R8,R8                                                            
*                                                                               
PROCPFX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO CLEAR MOVE PARAMETERS FROM SEL FIELDS. USED TO RESET *         
* MOVE DATA FOR PF10                                                  *         
***********************************************************************         
*                                                                               
CLEARSEL NTR1                                                                   
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    CLEARX                                                           
*                                                                               
CLEA020  CLI   5(R2),0             SELECT FIELD ENTERED ?                       
         BE    CLEA060             NO, GET NEXT SELECT                          
         CLI   8(R2),C'M'          YES, CLEAR IF IF 'MOVE';                     
         BE    CLEA040              'BEFORE'                                    
         CLI   8(R2),C'B'           OR 'AFTER'                                  
         BE    CLEA040                                                          
         CLI   8(R2),C'A'                                                       
         BNE   CLEA060                                                          
*                                                                               
CLEA040  MVI   8(R2),0                                                          
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CLEA060  LA    R1,MAXFIELD-1       GET NUMBER OF FIELDS                         
*                                                                               
CLEA080  BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         BAS   RE,BUMP             GET TO NEXT SELECT                           
         BCT   R3,CLEA020                                                       
*                                                                               
CLEARX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                  *         
* ON EXIT: CC=EQ IF EDIT NEEDED; CC=NEQ IF EDIT NOT NEEDED            *         
***********************************************************************         
*                                                                               
TSTEDT   NTR1                                                                   
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    TSTE080                                                          
*                                                                               
TSTE020  CLI   5(R2),0             SELECT FIELD ENTERED ?                       
         BE    TSTE040             NO, TEST IF ANYTHING ELSE WAS                
         CLI   8(R2),C'*'          YES, WAS IT ALREADY PROCESSED ?              
         BE    TSTE040             YES, TEST FOR ANYTHING ELSE                  
         B     TSTEDTY             NO, INDICATE EDIT NEEDED                     
*                                                                               
TSTE040  LA    R1,MAXFIELD-1       GET NUMBER OF FIELDS                         
*                                                                               
TSTE060  BAS   RE,BUMP                                                          
         TM    1(R2),X'20'         IS THIS A PROTECTED FIELD ?                  
         BO    *+12                YES, SKIP IT                                 
         TM    4(R2),X'20'         NO, WAS IT CHANGED ?                         
         BZ    TSTEDTY             YES, INDICATE EDIT NEEDED                    
         BCT   R1,TSTE060          NO, TRY NEXT FIELD                           
         BAS   RE,BUMP             THIS LINE DONE, TRY NEXT                     
         BCT   R3,TSTE020                                                       
*                                                                               
TSTE080  SR    R3,R3               TEST FOR NEW DATA                            
         ICM   R3,1,NEWLINES                                                    
         BZ    TSTEDTN                                                          
*                                                                               
TSTE100  CLI   5(R2),0             TEST THESE LINES TO SEE IF FIELD             
         BE    TSTE120              IS PROTECTED OR WAS INPUT RATHER            
         CLI   8(R2),C'*'           THAN IF IT WAS VALIDATED                    
         BE    TSTE120                                                          
         B     TSTEDTY                                                          
*                                                                               
TSTE120  LA    R1,MAXFIELD-1                                                    
*                                                                               
TSTE140  BAS   RE,BUMP                                                          
         TM    1(R2),X'20'                                                      
         BO    *+12                                                             
         TM    4(R2),X'80'                                                      
         BO    TSTEDTY                                                          
         BCT   R1,TSTE140                                                       
         BAS   RE,BUMP                                                          
         BCT   R3,TSTE100                                                       
*                                                                               
TSTEDTN  LTR   R8,R8               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    R8,R8               SET CC=EQ                                    
*                                                                               
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO TEST FOR PENDING MOVE                                 *         
***********************************************************************         
*                                                                               
TSTMOVE  NTR1                                                                   
         L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         OC    ACTTABLE,ACTTABLE                                                
         BZ    TSTMNO                                                           
         OC    MOVE,MOVE                                                        
         BNZ   TSTM040                                                          
*                                                                               
TSTM020  MVC   CONHEAD(L'WAITMSG),WAITMSG                                       
         B     TSTMNO                                                           
*                                                                               
TSTM040  OC    AFTER(4),AFTER                                                   
         BZ    TSTM020                                                          
*                                                                               
TSTMYES  CR    R8,R8                                                            
         B     TSTMX                                                            
*                                                                               
TSTMNO   LTR   R8,R8                                                            
*                                                                               
TSTMX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO READ THE FIRST/NEXT RECORD                            *         
***********************************************************************         
*                                                                               
READLST  NTR1                                                                   
         L     R2,AFSTSEL          FIND NEXT AVAILABLE LINE                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    READL020                                                         
         MH    R3,=Y(MAXFIELD)                                                  
         BAS   RE,BUMP                                                          
         BCT   R3,*-4                                                           
*                                                                               
READL020 ST    R2,ATHISLIN         SAVE LINE ADDRESS                            
         LA    R6,KEY                                                           
         SR    R7,R7                                                            
         IC    R7,NXTSEQ                                                        
         CLI   INTMODE,DISLIST                                                  
         MVI   INTMODE,FSTLIST                                                  
         BE    READL060                                                         
         OC    LLASTSCH,LLASTSCH   IS THIS A CONTINUATION ?                     
         BZ    READL040            NO, DO FIRST TIME LOGIC                      
         STC   R7,FSTSEQ           SAVE STARTING DISPLACEMENT                   
         LA    R4,SEQTABLE(R7)      AND ADDRESS THE TABLE                       
         B     READL100                                                         
*                                                                               
*                                                                               
* FIRST TIME LOGIC - BUILD KEY                                                  
*                                                                               
*                                                                               
         USING ACSHKEY,R6                                                       
READL040 LA    R7,0                FORCE DISPLACEMENT OF 0 FIRST TIME           
*                                                                               
READL060 STC   R7,FSTSEQ                                                        
         LA    R4,SEQTABLE(R7)                                                  
         LA    R2,CATSCHMH         RE-READ SCHEME HEADER                        
         XC    ACSHKEY,ACSHKEY                                                  
         MVI   ACSHRTYP,ACSHEQU                                                 
         MVI   ACSHSREC,ACSHSEQU                                                
         MVC   ACSHCUL,CUL                                                      
         MVC   ACSHCODE,QSCHM                                                   
         GOTO1 READ                                                             
*                                                                               
         MVI   ELCODE,ACSCELQ      MAKE SURE WE HAVE A CATEGORY                 
         BAS   RE,GETELIO           SEQUENCE ELEMENT                            
         BNE   NOA6ERR                                                          
         XC    SEQTABLE,SEQTABLE                                                
         USING ACSCD,R6                                                         
         SR    R3,R3                                                            
         IC    R3,ACSCLEN          GET ELEMENT LENGTH                           
         SH    R3,=H'2'                                                         
         BZ    NOA6ERR                                                          
         BCTR  R3,0                MVC SEQUENCES TO TABLE                       
         STC   R3,LSTSEQ           SAVE DISPLACEMENT OF LAST ENTRY              
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SEQTABLE(0),ACSCAT                                               
         LA    R1,SEQTABLE(R3)                                                  
         MVI   1(R1),X'EE'         MARK END OF TABLE                            
*                                                                               
         USING ACCTKEY,R6                                                       
         LA    R6,KEY                                                           
         XC    ACCTKEY,ACCTKEY                                                  
         MVI   ACCTRTYP,ACCTEQU                                                 
         MVI   ACCTSREC,ACCTSEQU                                                
         MVC   ACCTCUL,CUL                                                      
         MVC   ACCTSCH,QSCHM                                                    
         MVC   ACCTCODE,0(R4)                                                   
         B     READL120                                                         
*                                                                               
READL100 CLI   NUMLINES,MAXLINE    IS SCREEN FULL ?                             
         BE    READLX              YES, SAVE REGISTERS                          
         XC    ACCTKEY,ACCTKEY                                                  
         MVC   ACCTKEY(L'LLASTSCH),LLASTSCH                                     
         MVC   ACCTCODE,0(R4)                                                   
*                                                                               
*                                                                               
* FILE READING LOGIC                                                            
*                                                                               
*                                                                               
READL120 OI    DMINBTS,X'08'       READ DELETED RECORDS AS WELL                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         GOTO1 CATCHIOS                                                         
*                                                                               
         CLC   KEY(15),KEYSAVE     WE MUST HAVE RECORD IF IT IS                 
         BE    *+6                  SEQUENCE ELEMENT                            
         DC    H'0'                                                             
         MVC   LLASTSCH,ACCTKEY    SAVE ACCOUNT KEY                             
         L     R2,ATHISLIN         ADDRESS CURRENT LINE                         
         BAS   RE,SETLINE          SET ADDRESSES                                
         BAS   RE,DISPLAY          DISPLAY RECORD                               
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
         SR    RE,RE               INCREMENT LIST RECORD COUNT                  
         IC    RE,NUMLINES                                                      
         LR    R5,RE                                                            
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
         MH    R5,=Y(SELTABL)                                                   
         LA    R5,LSELTAB(R5)      PUT ACCOUNT KEY INTO NEXT TABLE SLOT         
         USING SELTABD,R5                                                       
         MVC   SELKEY,ACCTKEY                                                   
         MVI   SELACT,C' '                                                      
         LA    R7,2(R7)            INCREMENT DISPLACEMENT AND GET NEXT          
         LA    R4,SEQTABLE(R7)      SEQUENCE, IF ANY                            
         CLI   0(R4),X'EE'                                                      
         BNE   READL100                                                         
*                                                                               
READLX   STC   R7,NXTSEQ           SAVE NEXT STARTING DISPLACEMENT              
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO READ X'A7' ELEMENT AND DISPLAY DATA. ALSO REDISPLAYS  *         
* SELECT FIELD DATA IF ANY.                                           *         
***********************************************************************         
*                                                                               
DISPLAY  NTR1                                                                   
         L     R2,ASEL                                                          
         OI    4(R2),X'20'         SELECT VALIDATED                             
         NI    1(R2),X'DF'         UNPROTECT SELECT FIELD                       
*                                                                               
         L     R6,AIO                                                           
         USING ACCTKEY,R6                                                       
         OC    ACTTABLE,ACTTABLE                                                
         BZ    DISP080                                                          
         CLC   ACCTCODE,MOVE       REDISPLAY SELECT FIELD FOR MOVE,             
         BNE   DISP020              BEFORE AND/OR AFTER                         
         MVI   9(R2),C'M'                                                       
         B     DISP060                                                          
*                                                                               
DISP020  CLC   ACCTCODE,AFTER                                                   
         BNE   DISP040                                                          
         MVI   9(R2),C'A'                                                       
         B     DISP060                                                          
*                                                                               
DISP040  CLC   ACCTCODE,BEFORE                                                  
         BNE   DISP080                                                          
         MVI   9(R2),C'B'                                                       
*                                                                               
DISP060  MVI   8(R2),C'*'                                                       
         B     DISP100                                                          
*                                                                               
DISP080  CLC   ACCTCODE,=X'FFFF'   SLUSH ACCOUNT ?                              
         BNE   DISP100             NO                                           
         OI    1(R2),X'20'         YES PROTECT ON DISPLAY                       
         MVI   5(R2),X'00'         CLEAR LENGTH FROM PREVIOUS                   
*                                                                               
DISP100  L     R2,ACODE                                                         
         OI    4(R2),X'20'         CODE VALIDATED                               
         OI    1(R2),X'20'         AND PROTECTED ON DISPLAY                     
         MVC   8(L'CATCODE,R2),ACCTCODE                                         
*                                                                               
         MVI   ELCODE,CADELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DISPLAYX                                                         
         USING CADELD,R6                                                        
*                                                                               
         L     R2,ANAME                                                         
         OI    4(R2),X'20'         NAME VALIDATED                               
         MVC   8(L'CADNAME,R2),CADNAME                                          
*                                                                               
         L     R2,APRNT                                                         
         OI    4(R2),X'20'         PRINT VALIDATED                              
         MVI   8(R2),C'N'                                                       
         TM    CADSTAT,CADSNPRT                                                 
         BO    *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
         L     R2,ATOTS                                                         
         OI    4(R2),X'20'         TOTALS VALIDATED                             
         MVI   8(R2),C'N'                                                       
         TM    CADSTAT,CADSSUPT                                                 
         BO    *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
         L     R2,ATNAM                                                         
         OI    4(R2),X'20'         TOTAL NAME VALIDATED                         
         MVC   8(L'CADTNAM,R2),CADTNAM                                          
*                                                                               
         L     R2,AINST                                                         
         OI    4(R2),X'20'         INSTRUCTIONS VALIDATED                       
         CLC   CADLN,=YL1(CADINST-CADELD)                                       
         BE    DISP120             NO INSTRUCTIONS - SEE IF AGENCY              
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,CADLINP                                                     
         BZ    DISPLAYX                                                         
         BCTR  R3,0                MINUS 1 FOR LENGTH                           
         BCTR  R3,0                MINUS 1 FOR MOVE                             
         EX    R3,*+8                                                           
         B     DISPLAYX                                                         
         MVC   8(0,R2),CADINP                                                   
*                                                                               
DISP120  TM    CADTYPE,X'02'       IS THIS AGENCY COMMISSION?                   
         BZ    DISP140             NO                                           
         MVC   8(3,R2),=C'AGY'     YES, PRINT IT OUT                            
*                                                                               
DISP140  TM    CADTYPE,CADTBUCK    IS THIS A BUCKET?                            
         BZ    DISPLAYX            NO                                           
         MVC   8(6,R2),=C'BUCKET'  YES, PRINT IT OUT                            
*                                                                               
DISPLAYX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EDIT THE SCREEN.                                      *         
***********************************************************************         
*                                                                               
EDT      NTR1                                                                   
         MVI   UPDATE,C'N'         CLEAR UPDATE SWITCH                          
*                                                                               
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R7,R7               GET STARTING SEQUENCE CODE                   
         IC    R7,FSTSEQ                                                        
         LA    R7,SEQTABLE(R7)                                                  
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES       NUMBER OF LINES ON SCREEN                    
         BZ    EDT180                                                           
*                                                                               
         LA    R5,LSELTAB          ADDRESS OF SELECT TABLE                      
         USING SELTABD,R5                                                       
*                                                                               
EDT020   BAS   RE,SETLINE          GET FIELD ADDRESSES FOR THIS LINE            
         LA    R6,SELKEY                                                        
         USING ACCTKEY,R6                                                       
         L     R2,ASEL                                                          
         CLI   5(R2),0             GET READY FOR UPDATE FIRST ON                
         BE    EDT100               ALL BUT MOVE/BEFORE/AFTER                   
         CLI   8(R2),C'*'                                                       
         BE    EDT100                                                           
         CLI   8(R2),C'B'                                                       
         BE    EDT040                                                           
         CLC   0(2,R7),=X'FFFF'                                                 
         BE    SLUSHERR                                                         
         CLI   8(R2),C'S'                                                       
         BE    EDT100                                                           
         CLI   8(R2),C'D'                                                       
         BE    EDT100                                                           
         CLI   8(R2),C'M'          HANDLE MOVE DATA FIRST, THEN CHECK           
         BE    EDT080              FOR CHANGES                                  
         CLI   8(R2),C'A'                                                       
         BNE   INVEND                                                           
*                                                                               
EDT040   OC    BEFORE,BEFORE       SAVE MOVE, BEFORE AND/OR AFTER               
         BNZ   MOVEERR              DATA                                        
         OC    AFTER,AFTER                                                      
         BNZ   MOVEERR                                                          
         CLI   8(R2),C'A'                                                       
         BE    EDT060                                                           
         MVC   BEFORE,ACCTCODE                                                  
         B     EDT100                                                           
*                                                                               
EDT060   MVC   AFTER,ACCTCODE                                                   
         B     EDT100                                                           
*                                                                               
EDT080   OC    MOVE,MOVE                                                        
         BNZ   MOVEERR                                                          
         MVC   MOVE,ACCTCODE                                                    
*                                                                               
EDT100   LA    R6,KEY              READ AND UPDATE FOR CHANGES                  
         MVC   ACCTKEY(L'SELKEY),SELKEY                                         
         MVC   KEYSAVE,ACCTKEY                                                  
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),SYSFIL,KEY,AIO,0                 
         L     RE,AIO                                                           
         MVC   KEY,0(RE)                                                        
         CLC   ACCTKEY(ACCTCODE-ACCTKEY+2),KEYSAVE                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VSAVPTRS,DMCB,(X'80',AIO),POINTERS                               
*                                                                               
         CLI   8(R2),C'D'                                                       
         BNE   EDT120                                                           
*                                                                               
         MVI   ERROR,CANTDEL                                                    
         MVI   ELCODE,ACCWELQ      ERROR IF WORKCODES EXIST                     
         BAS   RE,GETELIO                                                       
         BE    ERREXIT                                                          
*                                                                               
         USING ACKEYD,R6                                                        
         L     R6,AIO                                                           
         OI    ACSTATUS,X'80'                                                   
         BAS   RE,WRITEIT                                                       
         MVI   UPDATE,C'Y'                                                      
         XC    0(2,R7),0(R7)                                                    
         B     EDT140                                                           
         DROP  R6                                                               
*                                                                               
EDT120   BAS   RE,ANYCHG           IS THERE ANYTHING TO CHANGE?                 
         BNE   EDT130              NO                                           
         BAS   RE,EDTCHG           EDIT AND UPDATE RECORD                       
         GOTO1 HELLO,DMCB,(C'D',ACCFIL),(X'A7',AIO)                             
         GOTO1 ADDELEM                                                          
         BAS   RE,WRITEIT                                                       
*                                                                               
EDT130   L     R2,ASEL                                                          
         CLI   5(R2),0                                                          
         BE    EDT160                                                           
         CLI   8(R2),C'*'                                                       
         BE    EDT160                                                           
*                                                                               
EDT140   MVC   9(1,R2),8(R2)                                                    
         MVC   SELACT,8(R2)                                                     
         MVI   8(R2),C'*'                                                       
         CLI   9(R2),C'S'                                                       
         BNE   EDT160                                                           
         LA    R6,SELKEY                                                        
         USING ACCTKEY,R6                                                       
         MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'CATEGORY',=C'DETAIL',(8,ACCTSCH),(2,ACCTCOX        
               DE),0                                                            
*                                                                               
EDT160   L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         LA    R7,2(R7)            GET NEXT SEQUENCE CODE                       
         BCT   R3,EDT020                                                        
*                                                                               
EDT180   SR    R3,R3               EDIT THE NEW LINES                           
         ICM   R3,1,NEWLINES                                                    
         BZ    EDTX                                                             
*                                                                               
EDT200   BAS   RE,SETLINE                                                       
         L     R2,ASEL                                                          
         CLI   5(R2),0                                                          
         BE    EDT320                                                           
         CLI   8(R2),C'*'                                                       
         BE    EDT280                                                           
         CLI   8(R2),C'N'                                                       
         BNE   INVEND                                                           
         L     R2,ACODE                                                         
         CLI   5(R2),0                                                          
         BE    NOCODE                                                           
*                                                                               
EDT220   CLI   0(R7),X'EE'         MUST BE AT END OF SEQUENCE TABLE             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,KEY                                                           
         XC    ACCTKEY,ACCTKEY                                                  
         MVI   ACCTRTYP,ACCTEQU                                                 
         MVI   ACCTSREC,ACCTSEQU                                                
         MVC   ACCTCUL,CUL                                                      
         MVC   ACCTSCH,QSCHM                                                    
         MVC   ACCTCODE,BLANKS                                                  
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACCTCODE(0),8(R2)                                                
         USING ACKEYD,R6                                                        
         OI    DMINBTS,X'08'       READ DELETED RECORDS AS WELL                 
         MVI   DELETED,C'N'        CLEAR DELETED SWITCH                         
         GOTO1 HIGH                                                             
         L     R6,AIO                                                           
         CLC   KEYSAVE(ACCTCODE-ACCTKEY+2),KEY                                  
         BNE   EDT240              DIDN'T FIND IT                               
         TM    ACSTATUS,X'80'      FOUND IT, IS IT DELETED ?                    
         BZ    CANTADD             NO, ERROR                                    
         MVI   DELETED,C'Y'        YES, MARK TO SKIP OVER ADD                   
         NI    ACSTATUS,X'FF'-X'80'     AND UNDELETE IT                         
         MVI   ELCODE,CADELQ       DELETE X'A7' ELEMENTS                        
         GOTO1 REMELEM                                                          
         MVI   ELCODE,ACCWELQ      AND X'A8' ELEMENTS                           
         GOTO1 REMELEM                                                          
*                                                                               
EDT240   BAS   RE,EDTCHG           OK, VERIFY THE A7 ELEMENT                    
*                                                                               
         CLI   DELETED,C'Y'        IS RECORD ALREADY THERE ?                    
         BE    EDT260              YES                                          
         NI    DMINBTS,X'F7'       NO, ADD THE RECORD                           
         L     RE,AIO                                                           
         L     RF,SIZEIO                                                        
         XCEF                                                                   
         L     R6,AIO                                                           
         MVC   ACCTKEY,KEYSAVE                                                  
         MVC   ACLENGTH,=Y(ACRECORD-ACKEYD+1)                                   
         MVC   KEY(L'ACCTKEY),ACCTKEY                                           
         GOTO1 ADD                                                              
*                                                                               
EDT260   GOTO1 ADDELEM             ADD THE ELEMENT                              
*                                                                               
         GOTO1 VSAVPTRS,DMCB,(X'80',0),POINTERS                                 
*                                                                               
         BAS   RE,WRITEIT          UPDATE THE RECORD                            
*                                                                               
         MVC   0(2,R7),BLANKS      ALL OK, SET FLAG, UPDATE TABLE, ETC          
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),8(R2)       ADD ENTRY TO SEQUENCE TABLE                  
         MVI   2(R7),X'EE'          MARK NEW END                                
         SR    R4,R4                AND UPDATE IT'S DISPLACMENT                 
         IC    R4,LSTSEQ                                                        
         LA    R4,2(R4)                                                         
         STC   R4,LSTSEQ                                                        
*                                                                               
         L     R2,ASEL                                                          
         CLI   5(R2),0                                                          
         BE    EDT280                                                           
         MVC   9(1,R2),8(R2)                                                    
         MVI   8(R2),C'*'                                                       
*                                                                               
EDT280   MVI   UPDATE,C'Y'                                                      
         LA    R7,2(R7)                                                         
EDT300   L     R2,ANEXTSEL                                                      
         BCT   R3,EDT200                                                        
         B     EDTX                                                             
*                                                                               
EDT320   L     R1,ACODE            ALL FIELDS MUST BE BLANK IF NO               
         CLI   5(R1),0              SELECT CODE ENTERED                         
         BNE   INVEND                                                           
         L     R1,ANAME                                                         
         CLI   5(R1),0                                                          
         BNE   INVEND                                                           
         L     R1,APRNT                                                         
         CLI   5(R1),0                                                          
         BNE   INVEND                                                           
         L     R1,ATOTS                                                         
         CLI   5(R1),0                                                          
         BNE   INVEND                                                           
         L     R1,ATNAM                                                         
         CLI   5(R1),0                                                          
         BNE   INVEND                                                           
         L     R1,AINST                                                         
         CLI   5(R1),0                                                          
         BNE   INVEND                                                           
         B     EDT300                                                           
*                                                                               
EDTX     BAS   RE,SETEDT           SET BITS AFTER COMPLETED UPDATE              
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON LINE                    *         
***********************************************************************         
*                                                                               
ANYCHG   NTR1                                                                   
         L     R2,ANAME                                                         
         TM    4(R2),X'20'         CHECK IF ANY FIELDS WERE CHANGED             
         BZ    ANYCHGY                                                          
*                                                                               
         L     R2,ATOTS                                                         
         TM    4(R2),X'20'                                                      
         BZ    ANYCHGY                                                          
*                                                                               
         L     R2,APRNT                                                         
         TM    4(R2),X'20'                                                      
         BZ    ANYCHGY                                                          
*                                                                               
         L     R2,ATNAM                                                         
         TM    4(R2),X'20'                                                      
         BZ    ANYCHGY                                                          
*                                                                               
         L     R2,AINST                                                         
         TM    4(R2),X'20'                                                      
         BZ    ANYCHGY                                                          
*                                                                               
ANYCHGN  LTR   R8,R8               SET CC=NEQ                                   
         B     ANYCHGX                                                          
*                                                                               
ANYCHGY  CR    R8,R8               SET CC=EQ                                    
*                                                                               
ANYCHGX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EDIT FIELD CHANGES AND SET A7 ELEMENT FOR ADD         *         
***********************************************************************         
*                                                                               
EDTCHG   NTR1                                                                   
         USING CADELD,R6                                                        
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT                                
         MVI   CADEL,CADELQ        PUT IN ELCODE AND SAVE LENGTH                
         SR    R1,R1                                                            
         IC    R1,=YL1(CADINST-CADELD)                                          
*                                                                               
         L     R2,ANAME                                                         
         GOTO1 ANY                                                              
         MVC   CADNAME,WORK        UPDATE ELEMENT                               
         OC    WORK,BLANKS                                                      
         CLC   WORK(L'CADNAME),=CL20'CONTINUE'                                  
         BNE   *+8                                                              
         OI    CADSTAT,CADSCONT                                                 
*                                                                               
         L     R2,APRNT                                                         
         CLI   5(R2),0             IF NOT ENTEREDED OR 'YES' LEAVE AS           
         BE    EDTC010               BINARY ZERO                                
         CLI   8(R2),C'Y'                                                       
         BE    EDTC010                                                          
         CLI   8(R2),C'N'          IS 'NO' SET TO DON'T PRINT                   
         BNE   INVEND                                                           
         OI    CADSTAT,CADSNPRT    UPDATE ELEMENT                               
*                                                                               
EDTC010  L     R2,ATOTS                                                         
         TM    CADSTAT,CADSCONT    IS THIS A CONTINUATION ?                     
         BZ    EDTC012             NO                                           
         CLI   5(R2),0             YES, IF NOTHING ENTERED FORCE NOTO           
         BE    EDTC018                                                          
         B     EDTC015                                                          
*                                                                               
EDTC012  CLI   5(R2),0             IF NOT ENTERED OR 'YES' LEAVE AS             
         BE    EDTC020              BINARY ZERO                                 
         CLI   8(R2),C'Y'                                                       
         BE    EDTC020                                                          
*                                                                               
EDTC015  CLI   8(R2),C'N'                                                       
         BNE   INVEND              IF 'NO' SET TO NO TOTALS                     
*                                                                               
EDTC018  OI    CADSTAT,CADSSUPT    UPDATE ELEMENT                               
*                                                                               
EDTC020  L     R2,ATNAM                                                         
         MVC   CADTNAM,BLANKS                                                   
         SR    R3,R3                                                            
         ICM   R3,1,5(R2)                                                       
         BZ    EDTC040                                                          
         TM    CADSTAT,CADSCONT    IS THIS A CONTINUATION ?                     
         BO    INVEND              YES, THIS ENTRY IS INVALID THEN              
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   CADTNAM(0),8(R2)    UPDATE THE ELEMENT                           
*                                                                               
EDTC040  L     R2,AINST                                                         
         MVI   CADTYPE,CADTWORK    SET TYPE TO DEFAULT - WORKCODE LIST          
         SR    R3,R3                                                            
         ICM   R3,1,5(R2)                                                       
         BZ    EDTCX                                                            
         TM    CADSTAT,CADSCONT    IS THIS A CONTINUATION ?                     
         BO    INVEND              YES, THIS ENTRY IS INVALID THEN              
*                                                                               
         CLC   0(2,R7),=X'FFFF'    INSTRUCTIONS ARE INVALID ON                  
         BE    SLUSHERR               SLUSH ACCOUNT                             
*                                                                               
         MVI   CADTYPE,CADTAGY     SET TYPE TO AGENCY                           
         CLI   5(R2),3             IS FIELD ONLY 3 BYTES ?                      
         BL    INVEND              TOO LOW, ERROR                               
         BH    EDTC050             TOO HIGH, CAN'T BE AGENCY THEN               
         CLC   8(3,R2),=C'AGY'     YES, IS IT AGENCY ?                          
         BE    EDTCX               YES, DONE                                    
*                                                                               
EDTC050  MVI   CADTYPE,CADTBUCK    SET TYPE TO BUCKET                           
         CLC   8(6,R2),=C'BUCKET'                                               
         BE    EDTCX               YES, DONE                                    
*                                                                               
EDTC060  MVI   CADTYPE,CADTFORM    SET TYPE TO FORMULA                          
         BAS   RE,VALINST          VALIDATE INSTRUCTIONS                        
         LA    R3,1(R3)            ADD 1 FOR LENGTH BYTE                        
         STC   R3,CADLINP                                                       
         AR    R1,R3               LENGTH IS MODIFIED BY INSTRUCTIONS           
         LA    RF,CADLINP(R3)      GET ADDRESS FOR FORMULA                      
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   CADINP(0),8(R2)                                                  
*                                                                               
         AR    R1,R5               ADD FORMULA LENGTH TO LENGTH SO FAR          
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),WORK                                                     
*                                                                               
EDTCX    STC   R1,CADLN                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SET BITS IN TWA AFTER UPDATE HAS BEEN COMPLETED. THIS IS DONE       *         
* AFTER  UPDATE DUE TO THE PROBLEMS ENCOUNTERED WITH DESCRIPTION      *         
* LOGIC                                                               *         
***********************************************************************         
*                                                                               
SETEDT   NTR1                                                                   
         L     R2,AFSTSEL                                                       
         SR    R3,R3               GET NUMBER OF LINES IN USE                   
         ICM   R3,1,NUMLINES                                                    
         BZ    SETE080                                                          
*                                                                               
SETE020  LA    R1,MAXFIELD         GET NUMBER OF FIELDS PER LINE                
         B     SETE060                                                          
*                                                                               
SETE040  BAS   RE,BUMP                                                          
*                                                                               
SETE060  TM    1(R2),X'20'         SKIP OVER PROTECTED FIELDS                   
         BO    *+12                                                             
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         RE-TRANSMIT THE FIELD                        
         BCT   R1,SETE040          REPEAT FOR EACH FIELD                        
*                                                                               
         BAS   RE,BUMP             GET NEXT SELECT FIELD                        
         BCT   R3,SETE020          AND REPEAT FOR EACH LINE                     
*                                                                               
SETE080  SR    R3,R3               GET NUMBER OF NEW LINES                      
         ICM   R3,1,NEWLINES                                                    
         BZ    SETEX                                                            
*                                                                               
SETE100  LA    R1,MAXFIELD                                                      
         B     SETE140                                                          
*                                                                               
SETE120  BAS   RE,BUMP                                                          
*                                                                               
SETE140  TM    1(R2),X'20'                                                      
         BO    *+12                                                             
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         BCT   R1,SETE120                                                       
*                                                                               
         BAS   RE,BUMP                                                          
         BCT   R3,SETE100                                                       
*                                                                               
SETEX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL VALIDATE INSTRUCTIONS                             *         
***********************************************************************         
*                                                                               
VALINST  NTR1                                                                   
         MVC   SAVEKEY,KEYSAVE     SAVE KEY FOR RETURN                          
         LR    R4,R2               SAVE STARTING POINT                          
         LA    R4,8(R2)            GET TO DATA                                  
         MVC   WORK,BLANKS         CLEAR WORK AREA                              
         LA    R5,WORK+1           ADDRESS DATA PORTION OF WORK AREA            
         MVI   0(R5),C'+'          ALWAYS START WITH PLUS                       
*                                                                               
         ST    R2,SAVER2           SAVE START FOR ERROR MESSAGE                 
         SR    R2,R2               CLEAR FOR TRANSLATE                          
         BCTR  R3,0                R3 = INPUT LENGTH                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R4),TRTAB                                                    
         BNZ   VALI020             NEED AT LEAST ONE OPERATION                  
         L     R2,SAVER2           RESTORE ADDRESS FOR ERROR                    
         B     INVEND                                                           
*                                                                               
VALI020  SR    R1,R4               GET LENGTH OF FIELD                          
         BZ    SCRERR              FIRST FIELD CAN'T BE OPERAND                 
         CLM   R2,1,=X'40'                                                      
         BE    SCRERR                                                           
*                                                                               
         STC   R2,OPERATOR         SAVE THE OPERATOR                            
*                                                                               
         BCTR  R1,0                ADJUST LENGTH FOR MOVE                       
         BAS   RE,READCAT          SEE IF CATEGORY EXISTS                       
         BNE   INSTERR                                                          
         LA    R1,1(R1)            BUMP R1 BACK UP                              
*                                                                               
         MVC   0(1,R5),OPERATOR                                                 
*                                                                               
         SR    R3,R1                                                            
         BZ    SCRERR                                                           
*                                                                               
         LA    R1,1(R1)                                                         
         AR    R4,R1                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R4),TRTAB                                                    
         BNZ   VALI020                                                          
*                                                                               
         LR    R1,R3               USE THIS LENGTH FOR MOVE                     
         BAS   RE,READCAT          READ LAST ENTRY                              
         BNE   INSTERR                                                          
*                                                                               
VALIX    LA    R1,WORK             PUT LENGTH OF DATA IN FIRST BYTE             
         SR    R5,R1                OF WORK AREA                                
         STC   R5,WORK                                                          
         MVC   KEYSAVE,SAVEKEY     RESTORE KEY AT RETURN                        
         XIT1  REGS=(R5)           RETURN REGISTER 5                            
         EJECT                                                                  
***********************************************************************         
* SET ADDRESSES OF FIRST SELECT LINE, PF LINE AND END OF SCREEN       *         
***********************************************************************         
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,CATSEL1H                                                      
         ST    R2,AFSTSEL                                                       
         LA    R0,MAXLINE                                                       
         LA    R1,MAXFIELD                                                      
         MR    R0,R0               LINES X FIELDS = # TO BUMP                   
*                                                                               
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
*                                                                               
         ST    R2,APFFLD           SAVE ADDRESS OF PFKEYS                       
         BAS   RE,BUMP                                                          
*                                                                               
         ST    R2,AENDSCR          SAVE ADDRESS OF END-OF-SCREEN                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET ADDRESSES OF FIELDS ON A LINE AND NEXT SELECT                   *         
* AT ENTRY R2 =A(SELECT FIELD HEADER)                                 *         
***********************************************************************         
*                                                                               
SETLINE  ST    RE,SAVERE                                                        
         LA    R0,MAXFIELD         NUMBER OF FIELDS PER LINE                    
         LA    R1,ASEL             SAVE ADDRESS OF SELECT                       
*                                                                               
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP             REPEAT FOR EACH FIELD                        
         BCT   R0,*-12                                                          
*                                                                               
         ST    R2,ANEXTSEL         LINE DONE, SAVE NEXT SELECT ADDRESS          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL GET READ CATEGORY RECORD INTO IOAREA2 (AIO2       *         
***********************************************************************         
*                                                                               
READCAT  NTR1                                                                   
         L     R3,ACODE                                                         
         LA    R6,0                                                             
         CLI   9(R3),C' '          IS CODE 1 BYTE ?                             
         BE    *+8                 YES                                          
         LA    R6,1(R6)            NO, MUST BE 2 BYTES                          
         CR    R6,R1               EQUAL TO L'CODE IN INSTRUCTION ?             
         BNE   READC020            NO                                           
         EX    R1,*+8              YES, IS DATA THE SAME ?                      
         B     *+10                                                             
         CLC   0(0,R4),8(R3)                                                    
         BNE   READC020            NO                                           
         BCTR  R1,0                YES, ADJUST LENGTH FOR ERROR                 
         B     SCRERR                                                           
*                                                                               
READC020 MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         USING ACCTKEY,R6                                                       
         XC    ACCTKEY,ACCTKEY                                                  
         MVI   ACCTRTYP,ACCTEQU                                                 
         MVI   ACCTSREC,ACCTSEQU                                                
         MVC   ACCTCUL,CUL                                                      
         MVC   ACCTSCH,QSCHM                                                    
         MVC   ACCTCODE,BLANKS                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACCTCODE(0),0(R4)                                                
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R5),0(R4)       PUT ENTRY IN WORK AREA ALSO                  
         LA    R5,3(R5)            GET READY FOR NEXT ENTRY                     
*                                                                               
         USING ACKEYD,R6                                                        
         MVC   ACLENGTH,=Y(ACRECORD-ACKEYD+1)                                   
         OI    DMINBTS,X'08'       READ DELETED RECORDS AS WELL                 
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         CLC   KEYSAVE(ACCTCODE-ACCTKEY+2),KEY                                  
         XIT1  REGS=(R5)                                                        
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL GET SCHEME RECORD, DELETE X'A6' ELEMENT,          *         
* AND FORMAT NEW ELEMENT                                              *         
***********************************************************************         
*                                                                               
GETA6    ST    RE,SAVERE                                                        
         LA    R6,KEY                                                           
         USING ACSHKEY,R6                                                       
         XC    ACSHKEY,ACSHKEY     RE-READ SCHEME RECORD                        
         MVI   ACSHRTYP,ACSHEQU                                                 
         MVI   ACSHSREC,ACSHSEQU                                                
         MVC   ACSHCUL,CUL                                                      
         MVC   ACSHCODE,QSCHM                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),SYSFIL,KEY,AIO,0                 
*                                                                               
         L     R1,AIO                                                           
         MVC   KEY,0(R1)                                                        
         CLC   ACSHKEY(ACSHCODE-ACSHKEY+8),KEYSAVE                              
         BE    *+6                 IT MUST BE THERE - READ IT BEFORE            
         DC    H'0'                                                             
*                                                                               
         GOTO1 VSAVPTRS,DMCB,(X'80',AIO),POINTERS                               
*                                                                               
         MVI   ELCODE,ACSCELQ                                                   
         BAS   RE,GETELIO                                                       
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING ACSCD,R6                                                         
         MVI   ACSCEL,ACSCELQ                                                   
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL PUT LENGTH INTO X'A6' ELEMENT AND ADD IT TO       *         
* RECORD IN AIO (SCHEME RECORD)                                       *         
***********************************************************************         
*                                                                               
REWRITE  NTR1                                                                   
         LA    R6,ELEMENT                                                       
         USING ACSCD,R6                                                         
         SLL   R3,1                                                             
         AH    R3,=H'2'                                                         
         STC   R3,ACSCLEN                                                       
*                                                                               
         GOTO1 ADDELEM                                                          
         BAS   RE,WRITEIT                                                       
*                                                                               
         MVI   INTMODE,DISLIST                                                  
         MVI   UPDATE,C'N'                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL UPDATE PERSONAL ELEMENT AND WRITE THE RECORD.     *         
***********************************************************************         
*                                                                               
WRITEIT  ST    RE,SAVERE                                                        
         GOTO1 PERSIN                                                           
         GOTO1 WRITE                                                            
*                                                                               
         L     RF,AIO                                                           
         TM    ACSTATUS-ACKEYD(RF),X'80'                                        
         BZ    WRIT5                                                            
         GOTO1 VCHGPTRS,DMCB,(X'80',0),POINTERS                                 
         B     WRITX                                                            
*                                                                               
WRIT5    GOTO1 VCHGPTRS,DMCB,(X'80',AIO),POINTERS                               
*                                                                               
WRITX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 3                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
INVEND   MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         SPACE 3                                                                
NOA6ERR  MVI   ERROR,NOA6                                                       
         B     ERREXIT                                                          
         SPACE 3                                                                
MOVEERR  MVI   ERROR,MOVERROR                                                   
         B     ERREXIT                                                          
         SPACE 3                                                                
NOCODE   MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
         SPACE 3                                                                
CANTADD  MVI   ERROR,RECEXIST                                                   
         B     ERREXIT                                                          
         SPACE 3                                                                
NOTLAST  MVI   ERROR,LASTSCR                                                    
         B     ERREXIT                                                          
         SPACE 3                                                                
SLUSHERR MVI   ERROR,SLUSHER                                                    
         B     ERREXIT                                                          
         SPACE 3                                                                
INSTERR  MVI   ERROR,NOTFOUND                                                   
         B     SCRERR2                                                          
         SPACE 3                                                                
SCRERR   MVI   ERROR,INVALID                                                    
         AR    R4,R1                                                            
*                                                                               
SCRERR2  SH    R4,=H'8'                                                         
         L     R2,SAVER2           GET FIELD ADDRESS BACK                       
         SR    R4,R2                                                            
         STC   R4,ERRNDX                                                        
*                                                                               
ERREXIT  GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
FULLMSG  DC    CL50'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                 
DONEMSG  DC    CL50'END OF DISPLAY'                                             
NEWMSG   DC    CL50'FULL SCREEN AVAILABLE FOR ADDS'                             
WAITMSG  DC    CL50'WAITING FOR REST OF MOVE'                                   
MOVEMSG  DC    CL50'MOVE COMPLETED'                                             
BLANKS   DC    CL132' '                                                         
ACCFIL   DC    CL8'ACCFIL  '                                                    
*                                                                               
TRTAB    DC    256X'40'                                                         
         ORG   TRTAB+C'+'                                                       
         DC    C'+'                                                             
         ORG   TRTAB+C'-'                                                       
         DC    C'-'                                                             
         ORG   TRTAB+C'A'                                                       
         DC    XL9'00'                                                          
         ORG   TRTAB+C'J'                                                       
         DC    XL9'00'                                                          
         ORG   TRTAB+C'S'                                                       
         DC    XL8'00'                                                          
         ORG   TRTAB+C'0'                                                       
         DC    XL10'00'                                                         
         ORG                                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROB1D                                                       
         DS    0F                                                               
*                                                                               
CLEARNEW DS    0CL(CLEAREND)       CLEAR THESE FIELDS WHEN KEY CHANGES          
NUMLINES DS    X                   N'LINES ON CURRENT SCREEN                    
NEWLINES DS    X                   NEW LINES FOR ADDS                           
*                                                                               
LLASTSCH DS    XL(L'SELKEY)        KEY OF LAST LINE ON SCREEN                   
LSELTAB  DS    XL(MAXLINE*SELTABL)                                              
*                                                                               
ACTTABLE DS    0XL6                                                             
MOVE     DS    XL2                                                              
AFTER    DS    XL2                                                              
BEFORE   DS    XL2                                                              
*                                                                               
MOVEHOLD DS    XL2                                                              
*                                                                               
FSTSEQ   DS    X                   DISPL OF 1ST CODE THIS SCREEN                
NXTSEQ   DS    X                   DISPL OF NEXT CODE THIS SCREEN               
LSTSEQ   DS    X                   DISPL OF LAST CODE IN TABLE                  
CLEAREND EQU   *-NUMLINES                                                       
*                                                                               
UPDATE   DS    CL1                 'Y' IF THERE ARE ADDS AND/OR DELETES         
DELETED  DS    CL1                 'Y' IF CATEGORY HAS BEEN DELETED             
*                                                                               
QSCHM    DS    CL(L'CATSCHM)                                                    
*                                                                               
SEQTABLE DS    XL254               TABLE                                        
*                                                                               
*                                                                               
MAXLINE  EQU   12                  MAXIMUM N'LINES PER SCREEN                   
MAXFIELD EQU   7                   MAXIMUM N'FIELDS PER LINE                    
*                                                                               
FSTLIST  EQU   1                   FIRST TIME SWITCH                            
DISLIST  EQU   2                   DISPLAY SWITCH                               
EDTLIST  EQU   3                   EDIT SWITCH                                  
NEWLIST  EQU   4                   FULL SCREEN ADD SWITCH                       
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0C                                                               
KEYCHG   DS    C                                                                
INTMODE  DS    X                   INTERNAL MODE                                
SCROLL   DS    X                   SCROLL AMOUNT                                
OPERATOR DS    C                   HOLD AREA FOR OPERATOR                       
*                                                                               
SAVERE   DS    A                   SAVE RE FOR SUB-ROUTINES                     
SAVER2   DS    A                   SAVE R2 FOR ERROR MESSAGE                    
SAVEKEY  DS    CL(L'KEYSAVE)                                                    
AFSTSEL  DS    A                   A(FIRST SELECT FIELD)                        
APFFLD   DS    A                   A(PF LINE)                                   
AENDSCR  DS    A                   A(LAST LINE)                                 
*                                                                               
ATHISLIN DS    A                   A(CURRENT LINE)                              
ANEXTSEL DS    A                   A(NEXT LINE)                                 
*                                                                               
ASEL     DS    A                   A(SELECT FIELD)                              
ACODE    DS    A                   A(CODE FIELD)                                
ANAME    DS    A                   A(NAME FIELD)                                
APRNT    DS    A                   A(PRINT FIELD)                               
ATOTS    DS    A                   A(TOTALS FIELD)                              
ATNAM    DS    A                   A(TOTAL NAME)                                
AINST    DS    A                   A(INSTRUCTION FIELD)                         
*                                                                               
POINTERS DS    XL(4*54+1)          PASSIVE POINTER BLOCK                        
POINTEND EQU   *                                                                
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
*                                                                               
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELACT   DS    C                                                                
SELKEY   DS    CL(ACCTCODE-ACCTKEY+2)                                           
SELTABL  EQU   *-SELTABD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACPRO41   03/03/11'                                      
         END                                                                    
