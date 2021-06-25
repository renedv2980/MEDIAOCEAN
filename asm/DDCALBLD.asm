*          DATA SET DDCALBLD   AT LEVEL 088 AS OF 05/20/94                      
*                                                                               
* CALMAKER - PRODUCES A SET OF CALENDARS REPRESENTED AS BIT STRINGS             
* ONE CALENDAR IS CREATED AS A STANDARD CALENDAR, AND AN ADDITIONAL             
* FOR EACH REQUESTED FOR A SPECIFIC COUNTRY.  VALID KEYWORDS ARE:               
*                                                                               
* "START=" FOLLOWED BY VALID START YEAR. IF SUPPLIED, MUST BE FIRST IN          
* INPUT.  IF "START=" NOT SUPPLIED, DEFAULT IS 90.                              
*                                                                               
* "END=" FOLLOWED BY VALID END YEAR.  IF SUPPLIED, MUST BE DIRECTLY             
* AFTER "START=" OR FIRST IF "START=" NOT SUPPLIED.                             
* YEAR MUST BE GREATER THAN OR EQUAL TO START YEAR.                             
* IF NOT SUPPLIED, DEFAULT IS 95.                                               
*                                                                               
* "<STANDARD>" NOTIFIES PROGRAM THAT THE FOLLOWING DATES ARE TO BE              
* USED FOR THE STANDARD CALENDAR.  HOLIDAYS SPECIFIED FOR THE STANDARD          
* CAL ARE CONSIDERED HOLIDAYS FOR ALL **FOLLOWING** CALS.  THE                  
* *STANDARD* KEYWORD MAY BE SPECIFIED ANYWHERE AFTER START= AND END=            
* MUST BE SUPPLIED ONCE AND ONLY ONCE.                                          
*                                                                               
* "COUNTRY=" FOLLOWED BY A COUNTRY NAME (VALIDATED USING FACTRYTAB).            
* INPUT SHOULD THEN LIST HOLIDAYS SPECIFIC TO THAT COUNTRY.                     
*                                                                               
*                                                                               
* SYMBOLS CREATED IN OUTPUT:                                                    
* YMIN     (H)   - START YEAR (NOT INCLUDING CENTURY)                           
* YMAX     (H)   - END YEAR (NOT INCLUDING CENTURY)                             
* SPRDAYS  (H)   - NUMBER OF DAYS AT START OF CAL BEFORE 1/1/START YR           
* CAL###   (B)   - CALENDARS WHERE ### IS COUNTRY CODE OR 000 FOR STD           
* CTRYTAB  (A)   - TABLE OF ADDRESSES OF CALS INDEXED BY COUNTRY CODE           
*                  IF COUNTRY NOT SPECIFIED IN INPUT, ITS CAL IS STD            
* MAXCTRY  (EQU) - COUNTRY CODE OF LAST COUNTRY IN TABLE                        
*                                                                               
         TITLE 'CALMAKER -- GENERATE DDGETRETC'                                 
*PHASE PPAPCAL                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PERVAL                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
         SPACE 5                                                                
***********************************************************************         
* LEAVE THIS CODE ALONE                                                         
*                                                                               
CALMAKER CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*CALMAKER,=V(REGSAVE)                                          
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(CALMAKER),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
*                                                                               
         EJECT                                                                  
MAIN     DS    0H                                                               
         OPEN  (PANFIL,OUTPUT)                                                  
         MVC   P(24),=C'++UPDATE DDGETRETC,0,ALL'                               
         BAS   RE,PUTLINE                                                       
*                                                                               
* IF START YEAR SUPPLIED . . .                                                  
         MVC   P(L'STARTSYM),STARTSYM                                           
         BAS   RE,GETLINE                                                       
         CLC   CARD(6),=C'START='                                               
         BNE   OUTSTART        IF NOT SUPPLIED, OUTPUT DEFAULT                  
         MVC   CARD(6),=C'JAN01/'                                               
         GOTO1 =V(PERVAL),DMCB,(70,CARD),(X'41',PERVOUT)                        
         CLI   4(R1),PVRCINV1  CHECK RESULT CODE = OK                           
         BE    INVLINPT        IF INVALID, THEN EXIT                            
*                                                                               
         LA    R9,PERVOUT      R9=PERVALD BASE REG                              
         USING PERVALD,R9                                                       
*                                                                               
* GET NUMBER OF DAY OF WEEK (1 - 7), CONVERT TO (0 - -6)                        
* IF NOT MONDAY, BKUP TO PRECEDING MON, STORE IN ADSTRT AND STRTDATE            
         GOTO1 =V(GETDAY),DMCB,PVALESTA,WORK                                    
         ZIC   R2,0(R1)        GET RESULT                                       
         BCTR  R2,R0           DECR TO GET: (1-7) -> (0-6)                      
         MVC   P(L'SPARESYM),SPARESYM                                           
         EDIT  (R2),(1,P+17),FILL=0                                             
         BAS   RE,PUTLINE                                                       
         LNR   R2,R2           NEGATE                                           
         GOTO1 =V(ADDAY),DMCB,PVALESTA,ADSTRT,(R2)                              
         GOTO1 =V(DATCON),DMCB,(X'00',ADSTRT),(X'05',STRTDATE)                  
*                                                                               
* OUTPUT SYMBOL CONTAINING START DATE OF CALENDAR                               
         MVC   P(L'STARTSYM),STARTSYM                                           
         MVC   P+17(2),PVALESTA                                                 
OUTSTART BAS   RE,PUTLINE                                                       
         BAS   RE,GETLINE      DISCARD CARD WITH START DATE                     
*                                                                               
*                                                                               
* IF END DATE SUPPLIED . . .                                                    
ENDDATE  MVC   P(L'ENDSYM),ENDSYM                                               
         CLC   CARD(4),=C'END='                                                 
         BNE   OUTNDAYS        IF NOT SUPPLIED, OUTPUT DEFAULT                  
         MVC   WORK(5),CARD+4                                                   
         MVC   CARD(6),=C'DEC31/'                                               
         MVC   CARD+6(5),WORK                                                   
*                                                                               
* COMPUTE NUMBER OF DAYS IN CALENDAR AND OUTPUT IT AS A SYMBOL                  
         GOTO1 =V(PERVAL),DMCB,(70,DATES),(X'1',PERVOUT)                        
         CLI   4(R1),PVRCOK    CHECK RESULT CODE = OK                           
         BNE   INVLINPT        IF INVALID, THEN EXIT                            
         MVC   NUMDAYS,PVALNDYS                                                 
*                                                                               
         MVC   P+17(2),PVALEEND    PUT 2DIGIT YEAR INTO SYMBOL                  
OUTNDAYS BAS   RE,PUTLINE                                                       
         BAS   RE,GETLINE      DISCARD CARD WITH END DATE                       
         MVI   P,C'*'                                                           
         BAS   RE,PUTLINE                                                       
         DROP  R9                                                               
         EJECT                                                                  
********************************************************************            
* INITIALIZE VANILLA CALENDAR - CREATE A CALENDAR IN MEMORY                     
* WHERE DAYS ARE REPRESENTED AS CONSECUTIVE BITS, WEEKDAYS AS 1'S               
* AND WEEKENDS AS 0'S                                                           
*                                                                               
INITCAL  LA    R2,SCAL         R2=CALENDAR BASE                                 
         LH    R7,NUMDAYS      DAY COUNTER                                      
         LM    R0,R1,TEMPLATE                                                   
*                                                                               
INITLOOP ST    R0,0(R2)        STORE 32 DAYS IN CALENDAR                        
         LTR   R1,R1           SEE IF WE NEED TO RE-LOAD TEMPLATE               
         BNZ   *+8                                                              
         LM    R0,R1,TEMPLATE                                                   
         SLDL  R0,4            SHFT TEMPLATE BY 4=(32 MOD 7 DAYS/WEEK)          
         LA    R2,4(R2)        BUMP CAL POINTER                                 
         SH    R7,=H'32'       DECREMENT DAY COUNTER                            
         BNM   INITLOOP                                                         
*                                                                               
         EJECT                                                                  
MAINLOOP CLC   CARD(2),=C'/*'                                                   
         BE    OUTTABLE        END OF FILE?                                     
*                                                                               
* KEYWORD = STANDARD?  IF YES, READ DATES FOR STANDARD CALENDAR                 
CASE01   CLC   CARD(10),=C'<STANDARD>'                                          
         BNE   CASE02                                                           
*                                                                               
         MVI   STDYES,X'FF'                                                     
         MVC   P(L'STDCLSYM),STDCLSYM                                           
         BAS   RE,PUTLINE                                                       
         MVC   P(11),=C'* HOLIDAYS:'                                            
         BAS   RE,PUTLINE                                                       
ADD00    BAS   RE,GETLINE                                                       
         GOTO1 =A(ADDHOLI),DMCB,SCAL              SETS CC                       
         BE    ADD00           ADD UNTIL INVALID DATE IS FOUND                  
*                                                                               
         GOTO1 =A(OUTCAL),DMCB,SCAL                                             
         B     MAINLOOP                                                         
*                                                                               
* KEYWORD = COUNTRY?  IF YES, VALIDATE COUNTRY NAME, THEN READ DATES            
CASE02   CLC   CARD(8),=C'COUNTRY='                                             
         BNE   INVLINPT                                                         
*                                                                               
* FIND COUNTRY CODE IN COUNTRY TABLE, EXIT IF NOT VALID NAME                    
         LA    R9,CTRYTAB1     R9 = COUNTRY TABLE BASE                          
         USING CTRYTABD,R9                                                      
         LH    R2,CTRYTAB      GET LEN OF REC - INCR FOR BXLE                   
         L     R3,CTRYTAB+2    GET END OF TABLE - LIMIT FOR BXLE                
*                                                                               
         CLC   CTRYNAM,CARD+8  COMPARE KEY WITH COUNTRY NAME IN TABLE           
         BE    *+12            IF FOUND, DONE                                   
         BXLE  R9,R2,*-10      LOOP IF NOT AT END OF TABLE                      
         B     INVLINPT        IF NOT FOUND, INVALID                            
*                                                                               
* OUTPUT SYMBOL FOR CALENDAR COMPRISED OF CAL + CTRYCODE                        
         MVC   P(L'CCALSYM),CCALSYM                                             
         MVC   P+35(L'CTRYNAM),CTRYNAM        GIVE A COUNTRY COMMENT            
         EDIT  (1,CTRYCODE),(3,P+3),FILL=0    INS NUM OF CTRY IN SYMB           
         BAS   RE,PUTLINE                                                       
         ZIC   R1,CTRYCODE                    MARK THIS COUNTRY AS              
         STC   R1,CALTBLE(R1)                 NOT USING THE STANDRD CAL         
         DROP  R9                                                               
         MVC   P(11),=C'* HOLIDAYS:'                                            
         BAS   RE,PUTLINE                                                       
         OC    STDYES,STDYES                                                    
         BZ    *+14                                                             
         MVC   P(25),=C'* STANDARD HOLIDAYS, PLUS'                              
         BAS   RE,PUTLINE                                                       
*                                                                               
* COPY STD CAL TO WORK CAL, ADD THIS COUNTRY'S HOLIDAYS, AND OUTPUT             
         LA    R2,SCAL         ADDRESS OF SOURCE                                
         LH    R3,NUMDAYS      GET LEN OF STD CALENDAR IN DAYS                  
         LA    R3,7(R3)        ROUND UP                                         
         SRL   R3,3            DIVIDE BY BITS PER BYTE                          
         LA    R4,CAL          ADDRESS OF DEST                                  
         LR    R5,R3           COPY LEN FOR DEST                                
         MVCL  R4,R2           COPY CALENDAR                                    
*                                                                               
ADD      BAS   RE,GETLINE                                                       
         GOTO1 =A(ADDHOLI),DMCB,CAL             SETS CC                         
         BE    ADD             ADD UNTIL INVALID DATE IS FOUND                  
*                                                                               
         GOTO1 =A(OUTCAL),DMCB,CAL                                              
         B     MAINLOOP                                                         
         EJECT                                                                  
*******************************************************************             
* OUTPUT TABLE OF ADDRESSES OF CALENDARS SO EACH COUNTRY HAS A CALENDAR         
OUTTABLE MVC   P(L'SCADRSYM),SCADRSYM                                           
         BAS   RE,PUTLINE                                                       
         LA    R9,CTRYTAB1                                                      
         USING CTRYTABD,R9                                                      
         LH    R2,CTRYTAB      GET LEN OF REC - INCR FOR BXLE                   
         L     R3,CTRYTAB+2    GET END OF TABLE - LIMIT FOR BXLE                
         LA    R4,CALTBLE+1                                                     
*                                                                               
NEXTNTRY MVC   P(L'CCADRSYM),CCADRSYM                                           
         EDIT  (1,0(R4)),(3,P+20),FILL=0                                        
         MVC   P+35(L'CTRYNAM),CTRYNAM                                          
         BAS   RE,PUTLINE                                                       
         LA    R4,1(R4)                                                         
         BXLE  R9,R2,NEXTNTRY                                                   
*                                                                               
* OUTPUT EQUATE CONTAINING THE INDEX OF THE LAST ENTRY IN THE TABLE             
         LA    R0,CALTBLE+1                                                     
         SR    R4,R0                                                            
         MVC   P(L'MAXCSYM),MAXCSYM                                             
         EDIT  (R4),(2,P+17),ALIGN=LEFT                                         
         BAS   RE,PUTLINE                                                       
*                                                                               
         CLOSE PANFIL                                                           
         XBASE                                                                  
*                                                                               
         SPACE 5                                                                
****************************************************************                
* DEFAULT EXIT ON ERROR                                                         
INVLINPT MVC   P(L'INVLD),INVLD            WARN OF INVALID INPUT                
         MVC   P+L'INVLD(80),CARD                                               
         GOTO1 =V(PRINTER)                                                      
         XBASE (RC=8)          EXIT PROGRAM WITH RETURN CODE <> 0               
INVLD    DC    C'INVALID INPUT: '                                               
MSG      DS    CL80            LINE FOR ERROR MSG                               
*                                                                               
         EJECT                                                                  
*************************************************************                   
* ADDHOLI SUBROUTINE                                                            
* READS DATE FROM CONTAINED IN AREA "CARD" AND TURNS OFF ITS                    
* CORRESPONDING BITS IN THE CALENDAR, NUM OF DAYS IN CAL IN "NUMDAYS"           
* PARAM 1 - A(CALENDAR)                                                         
* IF DATE IN CARD IS VALID, CC WILL BE SET TO "EQUAL", ELSE "NOT EQUAL"         
*                                                                               
ADDHOLI  NTR1                                                                   
         L     R7,0(R1)        GET ADDRESS OF CAL                               
* VALIDATE DATE                                                                 
         GOTO1 =V(PERVAL),DMCB,(80,DATES),(1,PERVOUT)                           
         CLI   4(R1),PVRCOK    CHECK RESULT CODE = OK                           
         BE    *+10            IF INVALID, THEN DONE                            
         CR    RB,RD           "NOT EQUAL" RETURN CODE                          
         B     DONEHOLI                                                         
*                                                                               
         LA    R9,PERVOUT      R9=PERVALD BASE REG                              
         USING PERVALD,R9                                                       
         LH    R5,PVALNDYS     R5=NUM OF DAYS INCLUSIVE = END-START+1           
         DROP  R9                                                               
         CH    R5,NUMDAYS      COMPARE RESULT WITH NUM DAYS IN CAL              
         BH    DONEHOLI        IF HIGHER, INVALID (RC = "NOT EQUAL")            
*                                                                               
* COMPUTE BYTE NUMBER AND BIT NUMBER OF DATE                                    
         BCTR  R5,0            DECR BECAUSE PERVAL GIVES INCLUSIVE              
         SR    R4,R4                                                            
         D     R4,=F'8'        R5=BYTE NUM, R4=BIT POS                          
         AR    R7,R5           ADD OFFSET TO ADDRESS OF CALENDAR                
*                                                                               
* MAKE MASK TO TURN OFF BIT IN CALENDAR                                         
         LA    R1,HIBIT        R1 = MASK                                        
         LTR   R4,R4           IF ZERO, NO SHIFT                                
         BE    *+12                                                             
         SRL   R1,1            SHIFT RIGHT (BIT POS) TIMES                      
         BCT   R4,*-4                                                           
*                                                                               
         X     R1,=X'FFFFFFFF' FLIP MASK BITS                                   
         IC    R0,0(R7)        GET BYTE FROM CALENDAR                           
         NR    R0,R1           MASK OUT THE DATE                                
         STC   R0,0(R7)                                                         
         MVI   P,C'*'          OUTPUT COMMENT WITH HOLIDAY                      
         MVC   P+2(78),CARD                                                     
         BAS   RE,PUTLINE                                                       
         CR    R0,R0           "EQUAL" RETURN CODE                              
*                                                                               
DONEHOLI XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
* OUTCAL SUBROUTINE                                                             
* OUTPUT CALENDAR AS BIT STRINGS IN DC LINES TO FILE USING PUTLINE              
* NUM OF DAYS IN CAL IN "NUMDAYS"                                               
* PARAM 1 = A(CALENDAR)                                                         
*                                                                               
OUTCAL   NTR1                                                                   
         L     R2,0(R1)        R2= CALENDAR BASE                                
         LH    R8,NUMDAYS                                                       
         MVC   ADOUT(6),ADSTRT                                                  
         MVC   P(L'HEADER),HEADER                                               
         BAS   RE,PUTLINE      PRINT HEADER                                     
         MVC   P(L'LHS),LHS    SET UP DC STATEMENT                              
         LA    R4,P+L'LHS      R4= FIRST COLUMN OF BITS                         
         LA    R5,P+EOBITS     R5= LAST COLUMN                                  
*                                                                               
         SH    R2,=H'4'        BACKUP BEFORE STARTING                           
         SR    R3,R3           CLEAR SHIFT COUNT TO LOAD NEW WORD               
*                                                                               
BITOUT   SH    R8,=H'1'        DEC TOTAL BIT COUNT                              
         BM    ENDLINE         DONE?                                            
*                                                                               
         SH    R3,=H'1'        DEC SHIFT COUNT                                  
         BNM   *+16            IF > 0, DON'T LOAD ANOTHER WORD                  
         LA    R2,4(R2)        BUMP CALENDAR WORD POINTER                       
         L     R7,0(R2)        GET A WORD REPRESENTING 32 DAYS                  
         LA    R3,31           R3=SHIFT COUNT                                   
*                                                                               
         SR    R6,R6                                                            
         SLDL  R6,1            SHIFT ONE BIT INTO R6 FROM R7                    
         STC   R6,0(R4)        STORE RESULT AND                                 
         OI    0(R4),C'0'      MASK IN NUMERIC BASE                             
         LA    R4,1(R4)        BUMP P BASE                                      
         CR    R4,R5           PREPARED TO PRINT LINE?                          
         BL    BITOUT                                                           
*                                                                               
* PUT A DATE COMMENT ON THE P LINE AND PRINT IT                                 
ENDLINE  MVI   0(R4),C''''     CLOSE QUOTE FOR DC                               
         GOTO1 =V(DATCON),DMCB,(0,ADOUT),(5,WORK)                               
         MVC   P+64(2),WORK+3  DAY                                              
         MVC   P+66(3),WORK    MONTH                                            
         MVC   P+69(2),WORK+6  YEAR                                             
         BAS   RE,PUTLINE                                                       
         MVC   P(L'LHS),LHS    PREPARE NEXT P LINE                              
         LA    R4,P+L'LHS                                                       
* COPY DATE TO INPUT FOR ADDAY AND COMPUTE NEXT COMMENT DATE                    
         MVC   ADIN,ADOUT                                                       
         GOTO1 =V(ADDAY),DMCB,ADIN,ADOUT,F'56'                                  
         LTR   R8,R8           IF NOT DONE, LOOP                                
         BNM   BITOUT                                                           
*                                                                               
DONEOUT  MVC   P(14),=C'         EJECT'                                         
         BAS   RE,PUTLINE                                                       
         MVC   P(L'LHS),=C'*          '    REMOVE LHS FROM P                    
         PUT   PANFIL,P        OUTPUT COUPLE BLANK COMMENT LINES                
         BAS   RE,PUTLINE                                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************                          
* SUBROUTINE PUTLINE - PUTS P TO PANFILE AND CLEARS P                           
*                                                                               
PUTLINE  NTR1                                                                   
         PUT   PANFIL,P                                                         
         MVI   P,C' '                                                           
         MVC   P+1(79),P                                                        
         XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
*                                                                               
******************************************************                          
* SUBROUTINE GETLINE - GETS CARD FROM CARDS, IGNORING THOSE STARTING            
* WITH "*"                                                                      
GETLINE  NTR1                                                                   
G#LOOP   GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLI   CARD,C'*'                                                        
         BE    G#LOOP                                                           
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
******************************************************                          
HIBIT    EQU   X'80'           HIGH BIT OF A BYTE                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    17X                                                              
HEADER   DC    C'*     MTWTF..MTWTF..MTWTF..MTWTF..MTWTF..MTWTF..MTWTF.+        
               .MTWTF..'                                                        
LHS      DC    C' DC B'''                                                       
EOBITS   EQU   56+L'LHS        LAST COL OF P IN WHICH OUTPUT BITS               
TEMPLATE DS    0D              TEMPLATE OF 58 DAYS, STARTING ON MON             
         DC    B'11111001111100111110011111001111'                              
         DC    B'10011111001111100111110011110000'                              
*-------------------------------,                                               
DATES    DS    0CL90            ! INPUT DATES FOR PERVAL                        
STRTDATE DC    CL11'JAN01/90'   !  <-, THESE MUST BE CONSECUTIVE                
CARD     DS    CL80             !  <-'                                          
*-------------------------------'                                               
ADSTRT   DC    C'900101'       START DATE FOR COMMENTS                          
ADIN     DS    CL6'YYMMDD'     USED FOR RHS DATE COMMENTS, ADDAY INPUT          
ADOUT    DS    CL6'YYMMDD'     ADDAY OUTPUT                                     
PERVOUT  DS    XL60            PERVAL OUTPUT                                    
NUMDAYS  DC    H'2192'         NUMBER OF DAYS IN CAL                            
PANFIL   DCB   DDNAME=PANFIL,DSORG=PS,RECFM=FB,MACRF=PM,LRECL=80,      +        
               BLKSIZE=7280                                                     
*                                                                               
SPARESYM DC    C'SPRDAYS  DC    H''0''              DAYS BEFORE JAN 1 '         
STARTSYM DC    C'YMIN     DC    H''90''             YY START YEAR'              
ENDSYM   DC    C'YMAX     DC    H''95''             YY END YEAR'                
STDCLSYM DC    C'CAL000   DS    0C                  STANDARD'                   
CCALSYM  DC    C'CAL###   DS    0C'                                             
SCADRSYM DC    C'CTRYTAB  DC    A(CAL000)           STANDARD'                   
CCADRSYM DC    C'         DC    A(CAL###)'                                      
MAXCSYM  DC    C'MAXCTRY  DC    A(##)'                                          
*                                                                               
STDYES   DC    X'0'            BOOLEAN FOR STANDARD CAL                         
CALTBLE  DC    30X'00'         IF A COUNTRY USES STD CAL, ITS BOOL=0            
CALFQ    EQU   120             NUM OF WORDS IN CAL, ENOUGH FOR 20 YRS           
SCAL     DS    (CALFQ)F        STANDARD CALENDAR                                
CAL      DS    (CALFQ)F        COPY SCAL HERE TO MANIPULATE FOR CNTRY           
********************************************************************            
         EJECT                                                                  
         LTORG                                                                  
******************************************************                          
*                                                                               
*                                                                               
*                                                                               
**     INCLUDE FACTRYTAB                                                        
**     INCLUDE FACTRY                                                           
**     INCLUDE DDDPRINT                                                         
**     INCLUDE DDPERVALD                                                        
         PRINT OFF                                                              
       ++INCLUDE FACTRYTAB                                                      
       ++INCLUDE FACTRY                                                         
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088DDCALBLD  05/20/94'                                      
         END                                                                    
