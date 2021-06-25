*          DATA SET ACPRO31    AT LEVEL 065 AS OF 04/24/07                      
*PHASE T60B31A                                                                  
         TITLE 'T60B31 - CATEGORY DETAIL'                                       
T60B31   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B31**,R8                                                    
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
         EJECT                                                                  
*                                                                               
* VALKEY  LOGIC                                                                 
*                                                                               
*                                                                               
CAT020   LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALHED           VALIDATE SCREEN                              
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* VALREC  LOGIC - DISPLAY OR CHANGE                                             
*                                                                               
*                                                                               
CAT040   BAS   RE,SETSCR           SET SCREEN LINE ADDRESSES                    
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    CAT060              YES                                          
         BAS   RE,PROCPF                                                        
         BE    CAT080              FORCE EDIT IF PF10                           
         BAS   RE,TSTEDT           TEST FOR ANYTHING TO EDIT                    
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
         L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         MVI   INTMODE,DISLIST                                                  
         MVC   NXTSEQ,FSTSEQ                                                    
         BAS   RE,DISLOGIC                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO CLEAR SCREEN, READ RECORDS AND DISPLAY THEM           *         
***********************************************************************         
*                                                                               
DISLOGIC NTR1                                                                   
         GOTO1 VCLEARF,DMCB,AFSTSEL,AENDSCR    CLEAR FIELDS AND TURN            
         GOTO1 (RF),(R1),(1,AFSTSEL),APFFLD     ON TRANSMIT BIT                 
         MVI   NUMLINES,0                                                       
         MVI   NEWLINES,0                                                       
*                                                                               
         CLI   INTMODE,NEWLIST                                                  
         BNE   DISL020                                                          
         MVC   FSTSEQ,NXTSEQ       IF NEW SCREEN REQUESTED, GET TO              
         MVI   INTMODE,EDTLIST      TO NEXT AVAILABLE SEQUENCE#                 
         MVC   CONHEAD(L'NEWMSG),NEWMSG                                         
         B     DISL060                                                          
*                                                                               
DISL020  CLI   INTMODE,FSTLIST                                                  
         BNE   DISL040                                                          
         XC    LLASTCAT,LLASTCAT   CLEAR OUT LAST ENTRY LISTED                  
*                                                                               
DISL040  BAS   RE,READLST                                                       
         L     R2,AFSTSEL                                                       
         CLI   NUMLINES,MAXLINE    IS SCREEN FULL ?                             
         BE    FULLMESS            YES, INDICATE MORE TO COME                   
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BNZ   *+8                                                              
         LA    R2,CATSCHMH                                                      
         XC    LLASTCAT,LLASTCAT   CLEAR OUT LAST ENTRY LISTED                  
         MVC   CONHEAD(L'DONEMSG),DONEMSG                                       
*                                                                               
DISL060  L     R2,AFSTSEL                                                       
         LA    R3,MAXLINE                                                       
         SR    R1,R1                                                            
         ICM   R1,1,NUMLINES                                                    
         BZ    DISL080                                                          
*                                                                               
         SR    R3,R1                                                            
         BNP   DISLOGX                                                          
*                                                                               
         MH    R1,=Y(MAXFIELD)     GET TO NEXT AVAILABLE LINE                   
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
*                                                                               
DISL080  STC   R3,NEWLINES                                                      
*                                                                               
DISL100  BAS   RE,SETLINE          IF LAST SCREEN, UNPROTECT REMAINING          
         L     R2,AWORK             LINES (ALL IF INTMODE=NEWLIST)              
         NI    1(R2),X'FF'-X'20'                                                
         L     R2,ANEXTSEL                                                      
         BCT   R3,DISL100                                                       
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
*                                                                               
***********************************************************************         
* DELETE ALL A8 ELEMENTS AND PUT THEM BACK IN NEW SEQUENCE            *         
***********************************************************************         
*                                                                               
MVELOGIC BAS   RE,READCAT          RE-READ CATEGORY RECORD                      
*                                                                               
         GOTO1 VSAVPTRS,DMCB,(X'80',AIO),POINTERS                               
*                                                                               
         L     R6,AIO2             MOVE DATA FROM AIO TO AIO2                   
         LA    R7,2000                                                          
         L     RE,AIO                                                           
         LA    RF,2000                                                          
         MVCL  R6,RE                                                            
*                                  DELETE ALL CWKELQ ELEMENTS                   
         GOTO1 HELLO,DMCB,(C'D',ACCFIL),(X'A8',AIO)                             
*                                                                               
         LA    R3,1                SET STARTING SEQUENCE #                      
         MVI   ELCODE,CWKELQ                                                    
         BAS   RE,GETELIO2         READ ELEMENTS IN IO2                         
         BE    *+6                                                              
         DC    H'0'                WE MUST HAVE SOME                            
         USING CWKELD,R6                                                        
*                                                                               
MVEL020  CLC   CWKSEQ,MOVE         IS THIS TO BE MOVED ?                        
         BE    MOVEIT              YES, FIND OUT WHERE                          
         CLC   CWKSEQ,AFTER        SHOULD SOMETHING FOLLOW THIS ?               
         BE    AFTERIT             YES, FIND OUT WHAT                           
         CLC   CWKSEQ,BEFORE       SHOULD SOMETHING COME BEFORE THIS?           
         BE    BEFOREIT            YES, FIND OUT WHAT                           
*                                                                               
MVEL040  STC   R3,CWKSEQ           UPDATE THE ELEMENT                           
*                                                                               
MVEL060  GOTO1 HELLO,DMCB,(C'P',ACCFIL),AIO,(R6)                                
*                                                                               
MVEL080  LA    R3,1(R3)            BUMP UP SEQUENCE #                           
*                                                                               
MVEL100  BAS   RE,NEXTEL2          GET NEXT ELEMENT                             
         BE    MVEL020                                                          
         BAS   RE,WRITEIT          WRITE BACK UPDATED RECORD                    
*                                                                               
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),POINTERS                               
*                                                                               
         XC    ACTTABLE,ACTTABLE   CLEAR THE ACTION TABLE                       
         MVI   INTMODE,DISLIST                                                  
         BAS   RE,DISLOGIC                                                      
         MVC   CONHEAD(L'MOVEMSG),MOVEMSG                                       
         B     XIT                                                              
         SPACE 3                                                                
MOVEIT   XC    MOVE,MOVE                                                        
         IC    R3,CWKSEQ           RESET SEQUENCE #                             
         OC    MOVESEQ,MOVESEQ     DO WE HAVE A PLACE TO MOVE IT TO ?           
         BZ    MOVE020             NO, NOT YET                                  
         MVC   CWKSEQ,MOVESEQ      YES, UPDATE ELEMENT                          
         XC    MOVESEQ,MOVESEQ                                                  
         B     MVEL060             ADD 'MOVED' ELEMENT                          
*                                                                               
MOVE020  MVC   MOVEHOLD,0(R6)      SAVE ELEMENT FOR WHEN WE FIND SPOT           
         B     MVEL100             GET NEXT ELEMENT                             
         SPACE 3                                                                
AFTERIT  STC   R3,NXTSEQ                                                        
         XC    AFTER,AFTER                                                      
         STC   R3,CWKSEQ           UPDATE THE ELEMENT                           
         LA    R3,1(R3)            BUMP UP SEQUENCE #                           
         STC   R3,MOVESEQ          SAVE THIS SPOT FOR MOVE AND PUT              
         GOTO1 HELLO,DMCB,(C'P',ACCFIL),AIO,(R6)         'AFTER' BACK           
         OC    MOVEHOLD,MOVEHOLD   DO WE HAVE DATA WAITING TO MOVE ?            
         BZ    MVEL080             NO, BUMP UP SEQUENCE                         
         DROP  R6                                                               
         USING CWKELD,R4                                                        
         LA    R4,MOVEHOLD                                                      
         MVC   CWKSEQ,MOVESEQ                                                   
         GOTO1 HELLO,DMCB,(C'P',ACCFIL),AIO,(R4)                                
         XC    MOVEHOLD,MOVEHOLD    CLEAR FIELDS                                
         XC    MOVESEQ,MOVESEQ                                                  
         B     MVEL080             GET NEXT ELEMENT                             
         SPACE 3                                                                
BEFOREIT STC   R3,NXTSEQ                                                        
         XC    BEFORE,BEFORE                                                    
         OC    MOVEHOLD,MOVEHOLD   DO WE HAVE DATA WAITING TO MOVE ?            
         BZ    BEF040              NO, NOT YET                                  
         LA    R4,MOVEHOLD         YES, USE THAT INSTEAD OF ELEMENT             
         STC   R3,CWKSEQ           UPDATE SEQUENCE #                            
         GOTO1 HELLO,DMCB,(C'P',ACCFIL),AIO,(R4)          ADD 'MOVED'           
         XC    MOVEHOLD,MOVEHOLD   CLEAR FIELD AFTER ADDING 'MOVE'              
*                                                                               
BEF020   LA    R3,1(R3)            BUMP UP SEQUENCE #                           
         B     MVEL040             ADD 'BEFORE' ELEMENT                         
*                                                                               
BEF040   STC   R3,MOVESEQ          SAVE SEQUENCE # FOR MOVE                     
         B     BEF020              BUMP SEQUENCE AND ADD BEFORE'                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO VALIDATE THE HEADING FIELD(S)                         *         
***********************************************************************         
*                                                                               
VALHED   NTR1                                                                   
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,0                                                         
*                                                                               
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
VALH020  LA    R2,CATCODEH         CATEGORY IS REQUIRED                         
         BAS   RE,TSTKEY                                                        
         GOTO1 VALCAT                                                           
         MVC   QCODE,CATEGORY                                                   
*                                                                               
         LA    R2,CATCNAMH                                                      
         MVC   8(L'ACCDNAME,R2),CATGNAME                                        
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
*                                                                               
         CLI   PFKEY,PF2           WORKCODE LIST                                
         BNE   PROCLINE                                                         
         MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'WORKCODE',=C'LIST',=C',',=C',',0                   
*                                                                               
PROCLINE L     R2,AFSTSEL                                                       
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    PROCPFN             NOTHING ON SCREEN TO PROCESS                 
*                                                                               
PROCPF10 CLI   PFKEY,PF10                                                       
         BNE   PROCPF9                                                          
         XC    ACTTABLE,ACTTABLE                                                
         BAS   RE,CLEARSEL         CLEAR MOVE DATA FROM SEL FIELDS              
         B     PROCPFY                                                          
*                                                                               
PROCPF9  CLI   PFKEY,PF9                                                        
         BNE   PROCPF8                                                          
         OC    LLASTCAT,LLASTCAT                                                
         BNZ   NOTLAST                                                          
         MVI   INTMODE,NEWLIST                                                  
         B     PROCPFN                                                          
*                                                                               
PROCPF8  CLI   PFKEY,PF8           SCROLL FORWARDS                              
         BNE   PROCPF7                                                          
         SR    R1,R1               IF USING SCROLL AMOUNT, DON'T                
         IC    R1,SCROLL            WORRY ABOUT LAST LINE                       
         LTR   R1,R1                                                            
         BNZ   PROCPF8A                                                         
         LA    R3,1                                                             
         OC    LLASTCAT,LLASTCAT   IF NO SCROLL AND END OF DISPLAY -            
         BZ    PROCPF78             START OVER                                  
         LA    R1,MAXLINE          IF NO SCROLL AND NOT END - USE MAX           
*                                                                               
PROCPF8A IC    R3,FSTSEQ           ADD STARTING NUMBER TO SCROLL OR MAX         
         AR    R3,R1                                                            
         SR    R1,R1                                                            
         IC    R1,NXTSEQ                                                        
         CR    R3,R1                                                            
         BNH   *+6                                                              
         LR    R3,R1                                                            
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,1                                                             
         B     PROCPF78                                                         
*                                                                               
PROCPF7  CLI   PFKEY,PF7           SCROLL BACKWARDS                             
         BNE   PROCPFN                                                          
         SR    R3,R3                                                            
         IC    R3,FSTSEQ                                                        
         SR    R1,R1                                                            
         IC    R1,SCROLL                                                        
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,MAXLINE          SUBTRACT MAXIMUM OR SCROLL AMOUNT            
         SR    R3,R1                PER SCREEN FROM STARTING #                  
         BP    *+8                  IF ZERO OR LESS, START OVER                 
         LA    R3,1                                                             
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
         L     R2,AFSTSEL          IF ACTION TABLE IS CLEAR, EXIT WITH          
         ST    R2,ACURFORC          NO.                                         
         OC    ACTTABLE,ACTTABLE   IF THERE IS BOTH A MOVE AND AN AFTER         
         BZ    TSTMNO               OR BEFORE ENTRY, EXIT WITH YES.             
         OC    MOVE,MOVE           IF THERE IS ONE BUT NOT THE OTHER,           
         BNZ   TSTM040              PRINT MESSAGE AND EXIT WITH NO.             
*                                                                               
TSTM020  MVC   CONHEAD(L'WAITMSG),WAITMSG                                       
         B     TSTMNO                                                           
*                                                                               
TSTM040  OC    AFTER(2),AFTER                                                   
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
         LA    R2,CATSCHMH                                                      
         BAS   RE,READCAT          RE-READ CATEGORY RECORD                      
*                                                                               
         SR    R3,R3                                                            
         IC    R3,NXTSEQ                                                        
*                                                                               
         CLI   INTMODE,DISLIST                                                  
         MVI   INTMODE,FSTLIST                                                  
         BE    READL040                                                         
         OC    LLASTCAT,LLASTCAT                                                
         BNZ   READL040                                                         
*                                                                               
         LA    R3,1                INITIALIZE SEQUENCE NUMBER                   
*                                                                               
READL040 STC   R3,FSTSEQ           SAVE SEQUENCE START FOR EDIT                 
         MVI   ELCODE,CWKELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   READLX                                                           
*                                                                               
         USING CWKELD,R6                                                        
READL060 BCT   R3,*+8                                                           
         B     READL080                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   READLX                                                           
         B     READL060                                                         
*                                                                               
READL080 IC    R3,FSTSEQ           GET BACK ELEMENT COUNT                       
*                                                                               
READL100 CLI   NUMLINES,MAXLINE    IS SCREEN FULL ?                             
         BE    READLX              YES, SAVE NEW ELEMENT COUNT                  
         MVC   LLASTCAT,CWKEL      SAVE THE ELEMENT                             
         L     R2,ATHISLIN         ADDRESS CURRENT LINE                         
         BAS   RE,SETLINE          SET ADDRESSES                                
         BAS   RE,DISPLAY          DISPLAY RECORD                               
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
*                                                                               
         SR    RE,RE               INCREMENT LINE COUNTER                       
         IC    RE,NUMLINES                                                      
         LR    R5,RE                                                            
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
*                                                                               
         LA    R3,1(R3)            INCREMENT ELEMENT COUNT                      
*                                                                               
         MVI   ELCODE,CWKELQ       RESET ELEMENT IDENTIFIER                     
         BAS   RE,NEXTEL           SEE IF ANY MORE                              
         BE    READL100             AND CONTINUE IF YES                         
*                                                                               
READLX   STC   R3,NXTSEQ           SAVE COUNT AS INDEX FOR NEXT TIME            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO DISPLAY DATA FROM A8 ELEMENT                          *         
***********************************************************************         
*                                                                               
DISPLAY  NTR1                                                                   
         L     R2,ASEL                                                          
         OI    4(R2),X'20'         SELECT VALIDATED                             
*                                                                               
         USING CWKELD,R6                                                        
         OC    ACTTABLE,ACTTABLE                                                
         BZ    DISP080                                                          
         CLC   CWKSEQ,MOVE         REDISPLAY SELECT FIELD FOR MOVE,             
         BNE   DISP020              BEFORE AND/OR AFTER                         
         MVI   9(R2),C'M'                                                       
         B     DISP060                                                          
*                                                                               
DISP020  CLC   CWKSEQ,AFTER                                                     
         BNE   DISP040                                                          
         MVI   9(R2),C'A'                                                       
         B     DISP060                                                          
*                                                                               
DISP040  CLC   CWKSEQ,BEFORE                                                    
         BNE   DISP080                                                          
         MVI   9(R2),C'B'                                                       
*                                                                               
DISP060  MVI   8(R2),C'*'                                                       
*                                                                               
*                                                                               
DISP080  L     R2,AWORK                                                         
         OI    4(R2),X'20'         WORKCODE VALIDATED                           
         OI    1(R2),X'20'         PROTECT WORK CODE ON DISPLAY                 
*                                                                               
         MVC   8(3,R2),=C'AGY'                                                  
         CLI   CWKTYPE,X'03'                                                    
         BE    DISP100                                                          
*                                                                               
         MVC   8(3,R2),=C'SUB'                                                  
         CLI   CWKTYPE,X'02'                                                    
         BE    DISP100                                                          
*                                                                               
         MVC   8(L'CATWRK,R2),CWKWORK     DISPLAY CODE AND SUFFIX               
*                                                                               
DISP100  L     R2,ACAT                                                          
         OI    4(R2),X'20'         CATEGORY VALIDATED                           
         MVC   8(L'CATCALC,R2),BLANKS                                           
         MVC   8(L'CWKCAT,R2),CWKCAT                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - -                         
         TM    CWKSTAT,CWKSPRE     PRESTO ONLY (DO NOT UPLOAD)?                 
         BNO   DISP120              NO                                          
         LA    R1,2                CAT CODE IS MAX OF TWO BYTES                 
         LA    R2,8(R2)            POINT TO FIELD                               
         CLI   0(R2),C' '          FIND FIRST SPACE AFTER CAT CALC CODE         
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         BCT   R1,*-12                                                          
         MVC   0(2,R2),=C'/N'      C'/N' INDICATES PRESTO ONLY                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - -                         
DISP120  L     R2,ADESC                                                         
         OI    4(R2),X'20'         DESCRIPTION VALIDATED                        
         SR    R3,R3                                                            
         IC    R3,CWKLN                                                         
         SH    R3,=YL2(CWKLN1Q)                                                 
         BZ    DISP140             NO DESCRIPTION, USE DEFAULT                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),CWKDESC                                                  
         B     DISPLAYX                                                         
*                                                                               
DISP140  MVC   KEY,BLANKS                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
         MVC   KEY+4(2),CWKWORK                                                 
         MVC   AIO,AIO2            SWAP BUFFERS SO WE DON'T LOSE                
         GOTO1 HIGH                 WHERE WE ARE                                
         CLC   KEYSAVE(L'ACKEYACC),KEY                                          
         BNE   DISP160                                                          
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETELIO                                                       
         BNE   *+10                                                             
         USING ACANALD,R6                                                       
         MVC   8(L'ACANDESC,R2),ACANDESC                                        
*                                                                               
DISP160  MVC   AIO,AIO1            SWAP BUFFERS BACK                            
*                                                                               
DISPLAYX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EDIT THE SCREEN.                                      *         
***********************************************************************         
*                                                                               
EDT      NTR1                                                                   
         MVI   UPDATE,C'N'         INITIALIZE SWITCH                            
         MVI   BRONLY,C'N'         NOT BRAND OCEAN ONLY                         
*                                                                               
         USING SCHRECD,R6          DETERMINE WHETHER SCHEME IS BO ONLY          
         LA    R6,KEY                                                           
         XC    SCHKEY,SCHKEY                                                    
         MVI   SCHKTYP,SCHKTYPQ                                                 
         MVI   SCHKSUB,SCHKSUBQ                                                 
         MVC   SCHKCUL,CUL                                                      
         MVC   SCHKCODE,QSCHM                                                   
         GOTO1 HIGH                READ FOR SCHEME RECORD                       
         CLC   KEYSAVE(SCHKCODE-SCHKEY+L'SCHKCODE),KEY                          
         BNE   WORKERR                                                          
         TM    SCHRSTA,SCHSMCSO    IS BO ONLY STATUS FLAG SET?                  
         BZ    *+8                                                              
         MVI   BRONLY,C'Y'         YES, BRAND OCEAN ONLY                        
*                                                                               
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         CLI   BRONLY,C'Y'         IS BO ONLY                                   
         BE    *+8                 DON'T READ ALL                               
         BAS   RE,READALL          READ ALL CATEGORY RECORDS                    
*                                                                               
         BAS   RE,READCAT          NOW READ THIS CATEGORY AGAIN                 
*                                                                               
         GOTO1 VSAVPTRS,DMCB,(X'80',AIO),POINTERS                               
*                                                                               
         SR    R3,R3               GET STARTING SEQUENCE CODE                   
         IC    R3,FSTSEQ                                                        
*                                                                               
         LA    R4,ELEMENT          R4=A(UPDATED ELEMENT)                        
*                                                                               
         SR    R5,R5               IF NO LINES ON SCREEN, SEE IF WE             
         ICM   R5,1,NUMLINES       ARE ADDING                                   
         BZ    EDT200                                                           
*                                                                               
         MVI   ELCODE,CWKELQ       GET FIRST ELEMENT AGAIN                      
         BAS   RE,GETELIO                                                       
         BE    *+6                 WE MUST HAVE AT LEAST 1                      
         DC    H'0'                                                             
*                                                                               
EDT020   BCT   R3,*+8                                                           
         B     EDT040                                                           
*                                                                               
         BAS   RE,NEXTEL           READ UNTIL 1ST ELEMENT ON SCREEN             
         BNE   EDT200                                                           
         B     EDT020                                                           
*                                                                               
EDT040   IC    R3,FSTSEQ           GET FIRST SEQUENCE NUMBER AGAIN              
         B     EDT080                                                           
*                                                                               
         USING CWKELD,R4                                                        
EDT060   BAS   RE,NEXTEL           WHEN EXHAUSTED, SEE IF ANY ADDS              
         BNE   EDT200                                                           
*                                                                               
EDT080   XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(CWKLN2Q),0(R6)   SAVE THE ELEMENT FOR UPDATE             
         BAS   RE,SETLINE          GET FIELD ADDRESSES FOR THIS LINE            
         L     R2,ASEL                                                          
         CLI   5(R2),0             GET READY FOR UPDATE FIRST ON                
         BE    EDT160               ALL BUT MOVE/BEFORE/AFTER                   
         CLI   8(R2),C'*'                                                       
         BE    EDT160                                                           
         CLI   8(R2),C'D'                                                       
         BE    EDT160                                                           
         CLI   8(R2),C'M'          HANDLE MOVE DATA FIRST, THEN CHECK           
         BE    EDT140              FOR CHANGES                                  
         CLI   8(R2),C'B'                                                       
         BE    EDT100                                                           
         CLI   8(R2),C'A'                                                       
         BNE   INVEND                                                           
*                                                                               
EDT100   OC    BEFORE,BEFORE       SAVE MOVE, BEFORE AND/OR AFTER               
         BNZ   MOVEERR              DATA                                        
         OC    AFTER,AFTER                                                      
         BNZ   MOVEERR                                                          
         CLI   8(R2),C'A'                                                       
         BE    EDT120                                                           
         STC   R3,BEFORE           USE UPDATED SEQUENCE #                       
         B     EDT160                                                           
*                                                                               
EDT120   STC   R3,AFTER                                                         
         B     EDT160                                                           
*                                                                               
EDT140   OC    MOVE,MOVE                                                        
         BNZ   MOVEERR                                                          
         STC   R3,MOVE                                                          
*                                                                               
EDT160   GOTO1 HELLO,DMCB,(C'D',ACCFIL),(X'A8',AIO),(8,CWKSEQ)                  
         CLI   8(R2),C'D'          MUST DELETE EVEN IF PROCESSED                
         BNE   EDT180                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,NXTSEQ           ADJUST NEXT SEQUENCE NUMBER                  
         BCTR  R1,0                                                             
         STC   R1,NXTSEQ                                                        
*                                                                               
         MVI   UPDATE,C'Y'         BACK, SET UPDATE FLAG, AND ADJUST            
*                                                                               
         MVC   9(1,R2),8(R2)                                                    
         MVI   8(R2),C'*'                                                       
*                                                                               
         L     R2,ANEXTSEL                                                      
         BCT   R5,EDT080           SKIP OVER ELEMENT UPDATE AND                 
         B     EDT200              NEXT READ                                    
*                                                                               
EDT180   BAS   RE,EDTCHG           EDIT AND UPDATE RECORD                       
         STC   R3,CWKSEQ           UPDATE SEQUENCE #                            
         GOTO1 HELLO,DMCB,(C'P',ACCFIL),AIO,(R4)                                
         CLI   12(R1),0                                                         
         BE    EDT182                                                           
         CLI   12(R1),5                                                         
         BE    HELLERR                                                          
         DC    H'0'                                                             
*                                                                               
EDT182   L     R2,ASEL                                                          
         CLI   5(R2),0                                                          
         BE    EDT190                                                           
         CLI   5(R2),C'*'                                                       
         BE    EDT190                                                           
*                                                                               
EDT185   MVC   9(1,R2),8(R2)                                                    
         MVI   8(R2),C'*'                                                       
*                                                                               
EDT190   L     R2,ANEXTSEL                                                      
         LA    R3,1(R3)            GET NEXT SEQUENCE NUMBER                     
         BCT   R5,EDT060                                                        
*                                                                               
EDT200   SR    R5,R5                                                            
         ICM   R5,1,NEWLINES       ANY CODES TO BE ADDED ?                      
         BNZ   EDT240              YES, GO DO THEM                              
*                                                                               
         CLI   UPDATE,C'Y'         NO, DO WE HAVE TO RESEQUENCE ?               
         BNE   EDTX                NO, ALL DONE THEN                            
*                                                                               
EDT220   BAS   RE,NEXTEL           YES, READ, MOVE, DELETE AND UPDATE           
         BNE   EDTX                 THE REST OF THE A8 ELEMENTS                 
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(CWKLN2Q),0(R6)                                           
         GOTO1 HELLO,DMCB,(C'D',ACCFIL),(X'A8',AIO),(8,CWKSEQ)                  
         STC   R3,CWKSEQ                                                        
         GOTO1 HELLO,DMCB,(C'P',ACCFIL),AIO,(R4)                                
*                                                                               
         CLI   12(R1),0                                                         
         BE    EDT230                                                           
         CLI   12(R1),5                                                         
         BE    HELLERR                                                          
         DC    H'0'                                                             
*                                                                               
EDT230   LA    R3,1(R3)                                                         
         B     EDT220                                                           
*                                                                               
EDT240   BAS   RE,SETLINE                                                       
         L     R2,ASEL                                                          
         CLI   5(R2),0                                                          
         BE    EDT380                                                           
         CLI   8(R2),C'N'                                                       
         BNE   INVEND                                                           
         L     R2,AWORK                                                         
         GOTO1 ANY                                                              
*                                                                               
         CLC   8(3,R2),=C'AGY'                                                  
         BE    EDT320                                                           
         CLC   8(3,R2),=C'SUB'                                                  
         BE    EDT320                                                           
*                                                                               
         CLI   WORK+2,C'N'         VERIFY SUFFIX                                
         BE    EDT260                                                           
         CLI   WORK+2,C'C'                                                      
         BE    EDT260                                                           
         CLI   WORK+2,C' '                                                      
         BNE   INVSUFF                                                          
         MVI   WORK+2,X'00'                                                     
*                                                                               
EDT260   LA    R7,WCTABLE                                                       
         LH    RF,WCCOUNT          IF WORKCODE, CHECK FOR DUPLICATE             
         LTR   RF,RF                                                            
         BZ    EDT300              NOTHING IN TABLE, OK TO ADD                  
*                                                                               
EDT280   CLC   0(3,R7),WORK                                                     
         BE    CANTADD                                                          
         CLC   0(2,R7),WORK                                                     
         BE    EDT400                                                           
         LA    R7,3(R7)                                                         
         BCT   RF,EDT280                                                        
*                                                                               
EDT300   MVC   AIO,AIO2            CHANGE BUFFERS FOR THIS                      
         MVC   KEY,BLANKS          MAKE SURE WORKCODE EXISTS                    
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
         MVC   KEY+4(2),WORK                                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1            RESTORE BUFFER                               
         CLC   KEYSAVE(L'ACKEYACC),KEY                                          
         BNE   WORKERR                                                          
*                                                                               
EDT320   LA    R4,ELEMENT          INITIALIZE ELEMNENT                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   CWKEL,CWKELQ                                                     
         MVI   CWKLN,CWKLN1Q                                                    
         MVI   CWKTYPE,X'03'                                                    
         CLC   8(3,R2),=C'AGY'                                                  
         BE    EDT340                                                           
         MVI   CWKTYPE,X'02'                                                    
         CLC   8(3,R2),=C'SUB'                                                  
         BE    EDT340                                                           
         MVI   CWKTYPE,X'01'                                                    
         MVC   CWKWORK,WORK                                                     
         MVC   CWKSUFF,WORK+2                                                   
*                                                                               
         CLI   BRONLY,C'Y'         IF BO ONLY, DON'T ADD WC TO TABLE            
         BE    EDT340                                                           
*                                                                               
         MVC   0(3,R7),WORK        UPDATE TABLE WITH NEW WORKCODE               
         LH    RF,WCCOUNT                                                       
         LA    RF,1(RF)                                                         
         STH   RF,WCCOUNT                                                       
         CLC   WCCOUNT,=YL2(MAXWC)                                              
         BH    WCERR               TOO MANY WORKCODES                           
*                                                                               
EDT340   BAS   RE,EDTCHG           VERIFY THE REST OF THE A8 ELEMENT            
         STC   R3,CWKSEQ           UPDATE SEQUENCE #                            
         GOTO1 HELLO,DMCB,(C'P',ACCFIL),AIO,(R4)                                
         CLI   12(R1),0                                                         
         BE    EDT350                                                           
         CLI   12(R1),5                                                         
         BE    HELLERR                                                          
         DC    H'0'                                                             
*                                                                               
EDT350   MVI   UPDATE,C'Y'         ALL OK, SET FLAG                             
*                                                                               
         LA    R3,1(R3)                                                         
*                                                                               
EDT360   L     R2,ANEXTSEL                                                      
         BCT   R5,EDT240                                                        
         B     EDTX                                                             
*                                                                               
EDT380   L     R1,AWORK            ALL FIELDS MUST BE BLANK IF NO               
         CLI   5(R1),0              SELECT CODE ENTERED                         
         BNE   INVEND                                                           
         L     R1,ADESC                                                         
         CLI   5(R1),0                                                          
         BNE   INVEND                                                           
         B     EDT360                                                           
*                                                                               
EDT400   CLI   2(R7),X'00'         DOES ENTRY IN TABLE HAVE SUFFIX ?            
         BE    INVSUFF             NO, WORKCODE BEING ADDED IS INVALID          
         CLI   2(R7),C'N'          YES, IS IT 'N' ?                             
         BNE   EDT420              NO, IT MUST BE A 'C'                         
         CLI   WORK+2,C'C'         YES, ONLY VALID SUFFIX THEN IS 'C'           
         BNE   INVSUFF                                                          
         B     EDT300              THIS ONE OK, KEEP CHECKING                   
*                                                                               
EDT420   CLI   WORK+2,C'N'         ONLY VALID SUFFIX NOW IS 'N'                 
         BNE   INVSUFF                                                          
         B     EDT300              THIS ON OK, KEEP CHECKING                    
*                                                                               
EDTX     BAS   RE,WRITEIT                                                       
*                                                                               
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),POINTERS                               
*                                                                               
         BAS   RE,SETEDT           SET BITS AFTER COMPLETED UPDATE              
         MVI   UPDATE,C'N'                                                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EDIT FIELD CHANGES AND SET A8 ELEMENT FOR 'PUT'       *         
* R4 CONTAINS ADDRESS OF ELEMENT                                      *         
***********************************************************************         
*                                                                               
EDTCHG   NTR1                                                                   
         USING CWKELD,R4                                                        
*                                                                               
         L     R2,ADESC                                                         
         TM    4(R2),X'20'         WAS DESCRIPTION VALIDATED ?                  
         BO    EDTC02              YES, SEE IF THERE'S A CATEGORY               
         MVI   CWKLN,CWKLN1Q       SET LENGTH FOR NO DESCRIPTION                
         SR    R3,R3                                                            
         ICM   R3,1,5(R2)                                                       
         BZ    EDTC02              NOTHING ENTERED                              
         MVC   CWKDESC,BLANKS                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   CWKDESC(0),8(R2)    UPDATE ELEMENT                               
         MVI   CWKLN,CWKLN2Q       ADJUST LENGTH                                
*                                                                               
EDTC02   L     R2,ACAT                                                          
         TM    4(R2),X'20'         WAS THIS VALIDATED ?                         
         BO    EDTCX               YES, DONE                                    
         MVC   CWKCAT,BLANKS       CLEAR IN CASE NOT ENTERED                    
         NI    CWKSTAT,X'FF'-CWKSPRE  TURN OFF PRESTO ONLY BIT                  
         SR    R3,R3                                                            
         ICM   R3,1,5(R2)                                                       
         BZ    EDTCX                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - -                       
         CLI   5(R2),3         LEN DETERMINES POSS POS FOR C'/N'                
         BL    EDTC02L                                                          
         BH    EDTC02H                                                          
         CLC   9(2,R2),=C'/N'    LEN=3 THEN C'/N' MUST BE IN BYTES 2,3          
         BNE   INVEND                  ELSE ERROR                               
         MVC   9(2,R2),BLANKS     SPACE OUT FOR CAT VALIDATION                  
         B     EDTC04                                                           
EDTC02L  CLC   8(2,R2),=C'/N'    LEN=1 OR 2 THEN C'/N' IN BYTES 1,2             
         BNE   EDTC06                       ELSE IT IS A CAT CODE               
         OI    CWKSTAT,CWKSPRE      TURN ON PRESTO ONLY BIT                     
         B     EDTCX                                                            
EDTC02H  CLC   10(2,R2),=C'/N'   LEN=4 THEN C'/N' MUST BE IN BYTES 3,4          
         BNE   INVEND                  ELSE ERROR                               
EDTC04   OI    CWKSTAT,CWKSPRE      TURN ON PRESTO ONLY BIT                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - -                       
EDTC06   GOTO1 ANY                                                              
         CLC   QCODE,WORK          CAN'T REFERENCE ITSELF                       
         BE    INVEND                                                           
         BAS   RE,VALIDCAT         DOES CATEGORY EXIST ?                        
         MVC   CWKCAT,WORK         YES, KEEP IT                                 
*                                                                               
EDTCX    B     XIT                                                              
         DROP  R4                                                               
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
* SET ADDRESSES OF FIRST SELECT LINE, PF LINE AND END OF SCREEN       *         
***********************************************************************         
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,CATSELH                                                       
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
* THIS ROUTINE WILL RE-READ THE CATEGORY RECORD                       *         
***********************************************************************         
*                                                                               
READCAT  NTR1                                                                   
         LA    R4,KEY                                                           
         USING ACCTKEY,R4                                                       
         XC    ACCTKEY,ACCTKEY                                                  
         MVI   ACCTRTYP,ACCTEQU                                                 
         MVI   ACCTSREC,ACCTSEQU                                                
         MVC   ACCTCUL,CUL                                                      
         MVC   ACCTSCH,QSCHM                                                    
         MVC   ACCTCODE,QCODE                                                   
         GOTO1 READ                                                             
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL READ ALL CATEGORY RECORDS                         *         
***********************************************************************         
*                                                                               
READALL  NTR1                                                                   
         USING ACCTKEY,R4                                                       
         LA    RE,WCTABLE          CLEAR TABLE                                  
         LA    RF,L'WCTABLE                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R0,MAXWC            MAXIMUM NUMBER OF ENTRIES                    
         LA    R3,0                WORKCODE COUNTER                             
         LA    R7,WCTABLE          WORKCODE TABLE                               
         LA    R4,KEY                                                           
         XC    ACCTKEY,ACCTKEY                                                  
         MVI   ACCTRTYP,ACCTEQU                                                 
         MVI   ACCTSREC,ACCTSEQU                                                
         MVC   ACCTCUL,CUL                                                      
         MVC   ACCTSCH,QSCHM                                                    
*                                                                               
READA020 GOTO1 HIGH                                                             
         CLC   KEYSAVE(ACCTCODE-ACCTKEY),KEY                                    
         BNE   READAX                                                           
         MVI   ELCODE,CWKELQ       READ WORKCODE ELEMENTS                       
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
READA040 BAS   RE,NEXTEL                                                        
         BNE   READA060                                                         
         USING CWKELD,R6                                                        
         CLI   CWKTYPE,X'01'       NOT CONCERNED WITH 'AGY' OR 'SUB'            
         BNE   READA040                                                         
         MVC   0(3,R7),CWKWORK                                                  
         LA    R7,3(R7)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,READA040                                                      
         BAS   RE,NEXTEL           ANY MORE?                                    
         BNE   READA060            NO                                           
         B     WCERR               YES, ERROR                                   
*                                                                               
READA060 MVI   ACCTCODE+2,X'FF'                                                 
         B     READA020                                                         
*                                                                               
READAX   STH   R3,WCCOUNT          SAVE THE COUNT                               
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL GET READ SCHEME RECORD IN IOAREA2 (AIO2) AND      *         
* DETERMINE FROM CATEGORY LIST WHETHER CATEGORY IS VALID OR NOT       *         
***********************************************************************         
*                                                                               
         USING ACSHKEY,R6                                                       
VALIDCAT NTR1                                                                   
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         XC    ACSHKEY,ACSHKEY                                                  
         MVI   ACSHRTYP,ACSHEQU                                                 
         MVI   ACSHSREC,ACSHSEQU                                                
         MVC   ACSHCUL,CUL                                                      
         MVC   ACSHCODE,QSCHM                                                   
         GOTO1 HIGH                                                             
*                                                                               
         USING ACKEYD,R6                                                        
         CLC   KEYSAVE(ACSHCODE-ACSHKEY+2),KEY                                  
         BNE   WORKERR                                                          
*                                                                               
         MVC   SAVEEL,ELCODE       SAVE OLD ELCODE                              
         MVI   ELCODE,ACSCELQ      FIND SEQUENCE ELEMENT                        
         BAS   RE,GETELIO2                                                      
         BNE   INVEND                                                           
*                                                                               
         USING ACSCD,R6                                                         
         SR    R3,R3                                                            
         IC    R3,ACSCLEN                                                       
         SH    R3,=H'2'                                                         
         BZ    INVEND              NO LIST, IMPOSSIBLE                          
         SRL   R3,1                DIVIDE BY 2 FOR NUMBER OF CATEGORIES         
*                                                                               
VALCAT2  LA    R6,L'QCODE(R6)                                                   
         CLC   0(2,R6),WORK        IS THIS THE REFERENCE CATEGORY ?             
         BE    VALCX               YES, IT'S OK                                 
         CLC   0(2,R6),QCODE       NO, IS IT THE HOME CATEGORY ?                
         BE    INVEND              YES, IT'S AN ERROR                           
         BCT   R3,VALCAT2                                                       
         B     INVEND                                                           
*                                                                               
VALCX    MVC   AIO,AIO1                                                         
         MVC   ELCODE,SAVEEL       RESTORE OLD ELEMENT                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSONAL ELEMENT AND WRITE BACK RECORD.                      *         
***********************************************************************         
*                                                                               
WRITEIT  ST    RE,SAVERE                                                        
         GOTO1 PERSIN                                                           
         GOTO1 WRITE                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 3                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
GETELIO2 L     R6,AIO2                                                          
         GETEL2 (R6),DATADISP,ELCODE                                            
         SPACE 3                                                                
INVEND   MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         SPACE 3                                                                
MOVEERR  MVI   ERROR,MOVERROR                                                   
         B     ERREXIT                                                          
         SPACE 3                                                                
CANTADD  MVI   ERROR,WCEXIST                                                    
         B     ERREXIT                                                          
         SPACE 3                                                                
NOTLAST  MVI   ERROR,LASTSCR                                                    
         B     ERREXIT                                                          
         SPACE 3                                                                
INVSUFF  MVI   ERROR,SUFFIX                                                     
         MVI   ERRNDX,X'02'                                                     
         B     ERREXIT                                                          
         SPACE 3                                                                
WCERR    MVI   ERROR,TOOWRK                                                     
         B     ERREXIT                                                          
         SPACE 3                                                                
WORKERR  MVI   ERROR,NOTFOUND                                                   
         B     ERREXIT                                                          
*                                                                               
HELLERR  MVI   ERROR,CATLRGE                                                    
*                                                                               
ERREXIT  GOTO1 VERRCUR                                                          
XIT      XIT1                                                                   
         EJECT                                                                  
FULLMSG  DC    CL50'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                 
DONEMSG  DC    CL50'END OF DISPLAY'                                             
NEWMSG   DC    CL50'FULL SCREEN AVAILABLE FOR ADDS'                             
WAITMSG  DC    CL50'WAITING FOR REST OF MOVE'                                   
MOVEMSG  DC    CL50'MOVE COMPLETED'                                             
*                                                                               
BLANKS   DC    CL132' '                                                         
ACCFIL   DC    CL8'ACCFIL  '                                                    
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
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROC1D                                                       
         DS    0F                                                               
*                                                                               
CLEARNEW DS    0CL(CLEAREND)       CLEAR THESE FIELDS WHEN KEY CHANGES          
LLASTCAT DS    XL(CWKLN2Q)         KEY OF LAST LINE ON SCREEN                   
*                                                                               
NUMLINES DS    X                   N'LINES ON CURRENT SCREEN                    
NEWLINES DS    X                   NEW LINES FOR ADDS                           
*                                                                               
ACTTABLE DS    0XL3                                                             
MOVE     DS    X                   CONTAINS SEQUENCE NUMBER OF DATA TO          
AFTER    DS    X                   BE MOVED BEFORE OR AFTER                     
BEFORE   DS    X                                                                
*                                                                               
FSTSEQ   DS    X                   SEQUENCE # OF 1ST CODE THIS SCREEN           
NXTSEQ   DS    X                   SEQUENCE # OF NEXT CODE THIS SCREEN          
*                                                                               
MOVEHOLD DS    XL(CWKLN2Q)                                                      
MOVESEQ  DS    X                                                                
WCCOUNT  DS    XL2                 COUNTER OF EXISTING WORKCODES                
CLEAREND EQU   *-LLASTCAT                                                       
*                                                                               
WCTABLE  DS    XL(MAXWC*(L'CWKWORK+L'CWKSUFF))                                  
*                                                                               
UPDATE   DS    C                   'Y' IF THERE WERE ADDS OR DELETES            
BRONLY   DS    C                   'Y' IF BRAND OCEAN ONLY                      
QSCHM    DS    CL(L'CATSCHM)                                                    
QCODE    DS    CL(L'CATCODE)                                                    
*                                                                               
MAXWC    EQU   255                                                              
MAXLINE  EQU   13                  MAXIMUM N'LINES PER SCREEN                   
*                                                                               
FSTLIST  EQU   1                   FIRST TIME SWITCH                            
DISLIST  EQU   2                   DISPLAY SWITCH                               
EDTLIST  EQU   3                   EDIT SWITCH                                  
NEWLIST  EQU   4                                                                
*                                                                               
POINTERS DS    XL(8*54+1)          PASSIVE POINTER BLOCK                        
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0C                                                               
KEYCHG   DS    C                                                                
SAVEEL   DS    C                   SAVEAREA FOR ELCODE                          
INTMODE  DS    X                   INTERNAL MODE                                
SCROLL   DS    XL1                 SCROLL AMOUNT                                
SAVERE   DS    A                   SAVE RE FOR SUB-ROUTINES                     
*                                                                               
AFSTSEL  DS    A                   A(FIRST SELECT FIELD)                        
APFFLD   DS    A                   A(PF LINE)                                   
AENDSCR  DS    A                   A(LAST LINE)                                 
*                                                                               
ATHISLIN DS    A                   A(CURRENT LINE)                              
ANEXTSEL DS    A                   A(NEXT LINE)                                 
*                                                                               
ASEL     DS    A                   A(SELECT FIELD)                              
AWORK    DS    A                   A(WORK FIELD)                                
ADESC    DS    A                   A(DESCRIPTION FIELD)                         
ACAT     DS    A                   A(CATEGORY FIELD)                            
MAXFIELD EQU   (*-ASEL)/4          MAXIMUM N'FIELDS PER LINE                    
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065ACPRO31   04/24/07'                                      
         END                                                                    
