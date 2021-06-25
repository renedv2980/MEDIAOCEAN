*          DATA SET DELMXP2    AT LEVEL 007 AS OF 02/28/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMXP2A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE QSORT                                                                  
*INCLUDE LMTIME                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
***********************************************************************         
* DFSORT USER EXIT FOR THE LOCAL MONTHLY PROGRAM AVERAGE.             *         
*                                                                     *         
* IT ASSIGNS A SUB-PROGRAM ID TO EACH SECTION OF A WEEKEND-ONLY       *         
* PROGRAM.                                                            *         
* THIS EXIT ALSO DETERMINES IF A "NORMAL" START AND END TIME EXIST,   *         
* AND IF SO IT RELEASES A "NORMAL" RECORD.                            *         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DELMICETPA, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.         *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* NOTE: CALLS TO EXTERNAL SUBROUTINES *MUST* BE MADE VIA              *         
*       CALL_DDS_SUBRTN, BECAUSE THIS EXIT'S USE OF REGISTERS IS      *         
*       NON-DDS-CONFORMING.                                           *         
***********************************************************************         
DELMXP2  CSECT                                                                  
*                                                                               
         ENTRY E35                 MUST BE "E35" (DFSORT OUTPUT EXIT)           
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E35,RC              RC = PROGRAM BASE REGISTER                   
E35      SAVE  (14,12),,DELMXP2                                                 
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
**********************************************************************          
* ASSIGN THE SUB-PROGRAM ID.                                                    
* DETERMINE IF A PROGRAM HAS A "NORMAL" START AND END TIME.                     
*                                                                               
* INPUT:  WORK RECORDS AT THE QH LEVEL WITH BOTH TIME PERIOD AND                
* -----   PROGRAM INFORMATION, SORTED BY PROGRAM NAME/PROGRAM ID/               
*         WEEKDAY/QUARTER HOUR.                                                 
*                                                                               
* OUTPUT: KEEP THE WORK RECORDS AND FILL IN THE SUB-PROGRAM ID SLOT.            
* ------  WHERE A "NORMAL" EXISTS, A SEPARATE "NORMAL" RECORD WILL BE           
*         CREATED WITH ALL THE INFORMATION PERTAINING TO IT.                    
*                                                                               
* RULES FOR SUB-PROGRAM IDS:                                                    
* -----   WEEKEND ONLY PROGRAMS WITH THE SAME PROGRAM NAME AND PROGRAM          
*         ID SHOULD BE SPLIT INTO DIFFERENT SUB-PROGRAMS FOR SATURDAY           
*         AND SUNDAY RESPECTIVELY. FURTHERMORE, IF THE TELECASTS FOR            
*         EITHER DAY ARE NOT CONTIGUOUS OR OVERLAPPING, THEY WOULD BE           
*         FURTHER SPLIT INTO SUB-PROGRAMS.                                      
*                                                                               
*         PROGRAMS THAT ALSO RUN ON WEEKDAYS WILL HAVE THE SAME                 
*         SUB-PROGRAM ID OF 1 ACROSS ALL TELECASTS FOR THAT PROGRAM             
*         NAME AND PROGRAM ID.                                                  
*                                                                               
**********************************************************************          
MAIN10   DS    0H                                                               
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
*                                                                               
         LTR   R3,R3               EOF?                                         
         BNZ   MAIN20              NO                                           
         CLI   LASTNOR,YESQ        DID WE CK THE LAST PRG FOR "NORMAL"?         
         BNE   NORCHECK            NO. GO CHECK                                 
         B     EOF                 YES. WE ARE DONE.                            
*                                                                               
         USING LMDSECT,R3                                                       
*                                  FIRST TIME IN,                               
MAIN20   OC    PREV_KDTYPE,PREV_KDTYPE  OR JUST FINISHD PROCESSING PREV         
         BZ    SAMESPGID           ADD TELECAST INFORMATION TO TABLE            
*                                                                               
MAIN30   CLC   W_KDTYPE,PREV_KDTYPE     CHANGE IN DATA TYPE,                    
         BNE   NEWSPGID                  NEW PROGRAM                            
         CLC   W_PNRPNAME,PREV_PNRPNAME CHANGE IN PROGRAM NAME,                 
         BNE   NEWSPGID                  NEW PROGRAM                            
         CLC   W_PNRPGID,PREV_PNRPGID   CHANGE IN PROGRAM ID                    
         BNE   NEWSPGID                  NEW PROGRAM                            
*                                                                               
         CLI   W_KDAY,SATURDAY     IF WEEKDAYS ARE PRESENT,                     
         BE    MAIN40                                                           
         CLI   W_KDAY,SUNDAY                                                    
         BE    MAIN40                                                           
         MVI   WEEKEND_ONLY_FLAG,NOQ THIS IS NOT A WEEKEND ONLY PROGRAM         
         B     SAMESPGID                                                        
*                                  WEEKEND AIRINGS,                             
MAIN40   CLI   WEEKEND_ONLY_FLAG,YESQ   CHECK TO SEE IF IT'S A                  
         BNE   SAMESPGID                WEEKEND-ONLY PROGRAM                    
         CLC   W_KDAY,PREV_KDAY    IF IT IS, TREAT EACH DAY                     
         BNE   NEWSPGID            INDIVIDUALLY,                                
*                                                                               
         ZIC   R1,W_KQHR           AND RE-EVALUATE EACH SECTION FOR             
         ZIC   R0,PREV_KQHR        CONTIGUOUS/OVERLAPPING QHS                   
         SR    R1,R0                                                            
         LTR   R1,R1               OVERLAPPING QHS (SAME QH AS PREV)?           
         BZ    SAMESPGID           YES. SAME PROGRAM SECTION                    
         CHI   R1,1                CONTIGUOUS QHS (QH GREATER BY ONE)?          
         BNE   NEWSPGID            NO. NEW PROGRAM SECTION                      
         CLI   W_KQHR,0            SPLIT AT 5:00 AM (QUARTER HOUR ZERO)         
         BE    NEWSPGID            NEW PROGRAN SECTION                          
         B     SAMESPGID           CONTIGUOUS QHS. SAME PROG SECTION            
*                                                                               
***********************************************************************         
* KEEP THE CURRENT SUB-PROGRAM ID.                                              
*                                                                               
* ADD THIS TELECAST TO A TABLE THAT WILL BE USED LATER TO DETERMINE             
* IF A "NORMAL" LINE EXISTS FOR THE CURRENT PROGRAM NAME/PROGRAM ID/            
* SUB-PROGRAM ID.                                                               
***********************************************************************         
SAMESPGID DS   0X                                                               
*                                                                               
         MVC   W_PNRSPGID,SUB_PGID  ADD THE ASSIGNED SUB-PROGID TO RECD         
*                                                                               
* KEEP A BINSEARCH TABLE OF TELECASTS.                                          
* BINSEARCH ELIMINATES DUPLICATE TELECASTS, SO THAT THE END RESULT              
* IS A TABLE OF UNIQUE TELECASTS.                                               
*                                                                               
         LA    R4,DUB                                                           
         USING TELTABD,R4                                                       
         MVC   TELNUM,W_PNRSEQ     TELECAST ID                                  
         MVC   TELSQH,W_PNRSQH     TELECAST START QUARTER HOUR                  
         MVC   TELEQH,W_PNREQH     TELECAST END QUARTER HOUR                    
*                                                                               
         CLI   W_KDAY,SATURDAY     FILL IN THE TYPE                             
         BNE   *+12                                                             
         MVI   TELTYPE,SATURDAYQ       SATURDAY                                 
         B     ADDTEL10                                                         
         CLI   W_KDAY,SUNDAY                                                    
         BNE   *+12                                                             
         MVI   TELTYPE,SUNDAYQ         SUNDAY                                   
         B     ADDTEL10                                                         
         MVI   TELTYPE,WEEKDAYQ        WEEKDAY (MON THROUGH FRI)                
*                                                                               
         DROP  R4                                                               
*                                                                               
ADDTEL10 GOTOR ,DMCB,(X'01',DUB),TELTAB,NUMVALS,TELTABQ,               +        
               ('TELNUM-TELTABD',L'TELNUM),TELTABL/TELTABQ                      
         L     RF,=V(BINSRCH)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         OC    0(4,R1),0(R1)       TABLE FULL?                                  
         BNZ   ADDTEL20                                                         
***      DC    H'0'                TELECASTS DON'T FIT IN TELTAB                
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR               ERROR EXIT                                   
*                                                                               
ADDTEL20 MVC   NUMVALS,DMCB+8      CURRENT NUMBER OF VALUES IN TABLE            
         MVC   PREV_KDTYPE,W_KDTYPE       SAVE PREVIOUS VALUES                  
         MVC   PREV_PNRPNAME,W_PNRPNAME                                         
         MVC   PREV_PNRPGID,W_PNRPGID                                           
         MVC   PREV_KDAY,W_KDAY                                                 
         MVC   PREV_KQHR,W_KQHR                                                 
*                                                                               
         B     KEEPREC             AND KEEP THE RECORD                          
*                                                                               
***********************************************************************         
* THIS IS THE BEGINNING OF A NEW PROGRAM, AS DEFINED BY PROGRAM NAME/           
* PROGRAM ID/ SUB-PROGRAM ID. CHECK TO SEE IF THE TELECASTS OF THE              
* PREVIOUS PROGRAM QUALIFY FOR A "NORMAL" LINE, RELEASE A "NORMAL" LINE         
* IF NEEDED, AND THEN ASSIGN A NEW SUB-PROGRAM ID FOR THE CURRENT               
* PROGRAM.                                                                      
***********************************************************************         
NEWSPGID DS    0X                                                               
*                                                                               
NORCHECK LTR   R3,R3               WAS THIS THE LAST RECORD?                    
         BNZ   *+8                                                              
         MVI   LASTNOR,YESQ        YES. FLAG THAT WE CHECKED THE LAST           
*                                   PROGRAM FOR "NORMAL"                        
         BAS   RE,NORSTIME         CHECK FOR "NORMAL" START TIME                
         BNE   NORNO                                                            
*                                                                               
NORYES   BAS   RE,NORETIME         DETERMINE THE "NORMAL" END TIME              
*                                                                               
         LA    R4,OUTREC           BUILD THE "NORMAL" LINE                      
         USING NOR_RECD,R4                                                      
*                                                                               
         MVC   NOR_SIGNAL,=C'** NOR **'                                         
         MVC   NOR_KDTYPE,PREV_KDTYPE     DATA TYPE                             
         MVC   NOR_PNAME,PREV_PNRPNAME    PROGRAM NAME                          
         MVC   NOR_PGID,PREV_PNRPGID      PROGRAM ID                            
         MVC   NOR_SPGID,SUB_PGID         SUB-PROGRAM ID                        
         MVC   NOR_SQH,NORSQH             "NORMAL" START QUARTER HOUR           
         MVC   NOR_EQH,NOREQH             "NORMAL" END QUARTER HOUR             
*                                                                               
* CONVERT "NORMAL" START QH TO TIME                                             
         LA    RE,NOR_SQH        "NORMAL" START QUARTER HOUR                    
         ST    RE,DUB                                                           
         LA    RE,NOR_STIME      OUTPUT AREA FOR TIME                           
         ST    RE,DUB+4                                                         
         BAS   RE,QHTOTIME                                                      
*                                                                               
* CONVERT "NORMAL" END QH TO TIME                                               
         ZIC   RF,NOR_EQH        "NORMAL" END QUARTER HOUR                      
         LA    RF,1(RF)          +1 TO DISPLAY THE FIRST MIN AFTER THE          
         STC   RF,HALF              END OF THE PROGRAM                          
         LA    RE,HALF           A(ADJUSTED END QH)                             
         ST    RE,DUB                                                           
         LA    RE,NOR_ETIME      OUTPUT AREA FOR END TIME                       
         ST    RE,DUB+4                                                         
         BAS   RE,QHTOTIME                                                      
*                                                                               
         DROP  R4                                                               
*                                                                               
         BAS   RE,RESETPRG       RESET SUB-PROGRAM ID. CLEAR VAR FIELDS         
         B     ADDREC            RELEASE THE "NORMAL" LINE                      
*                                                                               
*                                                                               
NORNO    DS    0X                                                               
         BAS   RE,RESETPRG       RESET SUB-PROGRAM ID. CLEAR VAR FIELDS         
         B     MAIN10            CONTINUE WITH THE CURRENT RECORD               
*                                                                               
***********************************************************************         
* DFSORT ACTIONS                                                                
***********************************************************************         
KEEPREC  DS    0H                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LR    R1,R3               SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4: DELETE RECORD                      
         B     GOBACK                                                           
*                                                                               
ADDREC   DS    0H                                                               
         LGHI  GRF,12              SET RC=12: INSERT RECORD                     
         SGR   GR1,GR1                                                          
         LA    R1,OUTREC           SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
ERROR    DS    0H                                                               
         LGHI  GRF,16              DFSORT WILL TERMINATE WITH RC=16             
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORT_HIGH_HALVES                                       
         LMH   GR2,GRE,DFSORT_HIGH_HALVES+8                                     
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BSM   0,RE                RETURN                                       
*                                                                               
***********************************************************************         
* PRINT OUT A MEMORY SNAP.                                                      
***********************************************************************         
SNAPIT   DS    0H                                                               
         ST    RE,SAVERE           SAVE RETURN ADDRESS                          
*                                                                               
         SR    RE,RC               DISPLACEMENT TO SNAPIT CALL                  
         STCM  RE,7,THREE          24-BIT ADDRESSING MODE ASSUMED               
         LA    R2,THREE                                                         
         LHI   R0,L'THREE                                                       
         LA    R1,HDR1HEXD         A(OUTPUT AREA)                               
SNAPIT10 LLC   RE,0(R2)                                                         
         SLL   RE,24                                                            
         SRDL  RE,28               ISOLATE HIGH-ORDER NIBBLE                    
         SRL   RF,28               ISOLATE LOW-ORDER NIBBLE                     
         LLC   RE,HEXTAB(RE)                                                    
         STC   RE,0(R1)                                                         
         LLC   RF,HEXTAB(RF)                                                    
         STC   RF,1(R1)                                                         
         LA    R2,1(R2)            BUMP TO NEXT BYTE                            
         LA    R1,2(R1)                                                         
         BCT   R0,SNAPIT10                                                      
*                                                                               
         ZIC   R1,4(RC)            LENGTH OF EXIT NAME                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HDR1NAME(0),5(RC)   MOVE EXIT NAME TO OUTPUT AREA                
*                                                                               
*                                     R3 = A(SORT RECORD)                       
         LA    R4,W_LMRECLQ(R3)    R4 = A(JUST BEYOND SORT RECORD)              
         OPEN  (SNAPDUMP,OUTPUT)   OPEN SNAP DUMP DATASET                       
*                                                                               
*                                  DUMP PSW, REGS, CSECT, AND SORT REC.         
         SNAP  DCB=SNAPDUMP,PDATA=(PSW,REGS,SA,SAH,SUBTASKS,JPA),      +        
               STORAGE=((R3),(R4)),STRHDR=HDR1L                                 
         LTR   RF,RF               WAS SNAP SUCCESSFUL?                         
         BZ    SNAPITX             YES                                          
         ABEND 301                 ABEND IF WE CAN'T GET A SNAP DUMP            
         CLOSE SNAPDUMP                                                         
SNAPITX  L     RE,SAVERE                                                        
         BSM   0,RE                                                             
*                                                                               
***********************************************************************         
* DETERMINE THE "NORMAL" START TIME, IF ONE EXISTS.                             
*                                                                               
* RULES FROM NIELSEN:                                                           
*   A. THE START QUARTER HOUR WITH THE GREATEST FREQUENCY PROVIDED              
*      THERE ARE AT LEAST TWO TELECASTS ACROSS ALL WEEKDAYS.                    
*   B. IF TWO OR MORE START QUARTER HOURS OCCUR WITH THE SAME                   
*      FREQUENCY, THE "NORMAL" START TIME IS THE EARLIEST QUARTER HOUR.         
*   C. IF THE START QUARTER HOUR FOR EACH DAY AND WEEK VARIES, THERE            
*      WILL BE NO "NORMAL" START TIME.                                          
*   D. IF THE START QUARTER HOURS AND END QUARTER HOURS ARE THE SAME            
*      FOR ALL DAYS AND WEEKS, THERE WILL BE NO "NORMAL" START TIME.            
*   E. ONLY WEEKDAY START TIMES ARE USED TO DETERMINE THE "NORMAL"              
*      START TIME FOR A PROGRAM THAT WAS TELECAST ON WEEKDAYS AND               
*      SATURDAY AND/OR SUNDAY.                                                  
*   F. A PROGRAM THAT WAS TELECAST ON SATURDAY AND/OR SUNDAY WILL USE           
*      ONLY THE SATURDAY TELECASTS TO DETERMINE A SATURDAY "NORMAL"             
*      START TIME, AND WILL USE ONLY THE SUNDAY TELECASTS TO DETERMINE          
*      THE SUNDAY "NORMAL" START TIME.                                          
*      (SINCE THE SUB-PROGRAM ID SEPARATES SATURDAY AND SUNDAY,                 
*       NO EXTRA CODING IS NEEDED FOR THIS RULE)                                
***********************************************************************         
NORSTIME DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
* SORT TELECASTS ASCENDING ON:                                                  
* DAY TYPE (WEEKDAY, SATURDAY, OR SUNDAY)/ START QH / END QH.                   
* WEEKEND AIRINGS GET SORTED AFTER THE WEEKDAY AIRINGS.                         
*                                                                               
NORST01  GOTOR ,DMCB,(0,TELTAB),NUMVALS,TELTABQ,                       +        
               L'TELTYPE+L'TELSQH+L'TELEQH,TELTYPE-TELTABD                      
         L     RF,=V(QSORT)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
* GO THROUGH THE SORTED LIST AND DETERMINE IF A "NORMAL" START TIME             
* EXISTS.                                                                       
*                                                                               
* RULE D. IS APPLIED ACROSS ALL TELECASTS, SO DO A FIRST RUN THROUGH            
* THE TABLE TO IDENTIFY PROGRAMS THAT HAVE THE SAME START AND END               
* QUARTER HOURS ACROSS ALL DAYS, AND THEN THROW THEM AWAY.                      
*                                                                               
         LA    R4,TELTAB                                                        
         USING TELTABD,R4                                                       
         L     R1,NUMVALS                                                       
         MVC   PREV_SQH,TELSQH     SAVE THE FIRST SQH                           
         MVC   PREV_EQH,TELEQH     SAVE THE FIRST EQH                           
         B     NORST10                                                          
*                                                                               
NORST05  CLC   TELSQH,PREV_SQH                                                  
         BNE   NORST15             DIFF SQH, PROG QUALIFIES FOR                 
         CLC   TELEQH,PREV_EQH      POSSIBLE NOR                                
         BNE   NORST15             DIFF EQH, PROG QUALIFIES FOR                 
*                                   POSSIBLE NOR                                
NORST10  LA    R4,TELTABQ(R4)                                                   
         BCT   R1,NORST05                                                       
         B     NORSTNO             ALL SQHS AND EQHS ARE THE SAME.              
*                                  PROGRAM DOESN'T QUALIFY FOR NOR              
         DROP  R4                                                               
*                                                                               
* IF TELECASTS ARE PRESENT FOR WEEKDAYS, THE SATURDAY AND SUNDAY                
* TELECASTS WILL NOT BE USED. THE SATURDAY AND SUNDAY TELECASTS WILL            
* ONLY BE USED IF THERE ARE NO WEEKDAYS PRESENT. (RULES E AND F)                
NORST15  LA    R4,TELTAB                                                        
         USING TELTABD,R4                                                       
*                                                                               
         MVI   WEEKDAY_FLG,NOQ     WEEKDAYS FLAG                                
         L     R1,NUMVALS          NUMBER OF ENTRIES IN THE TABLE               
*                                                                               
NORST20  MVC   NORSQH,TELSQH       START OUT WITH EARLIEST START QH             
         MVI   PREV_SQH,X'FF'      PREVIOUS START QH                            
         SR    R5,R5               MAXIMUM FREQUENCY SO FAR                     
         SR    R0,R0               CURRENT FREQUENCY                            
*                                                                               
NORST30  CLI   TELTYPE,WEEKDAYQ                                                 
         BNE   *+8                                                              
         MVI   WEEKDAY_FLG,YESQ    WEEKDAYS ARE PRESENT FOR THIS PROG           
*                                                                               
         CLI   TELTYPE,SATURDAYQ                                                
         BNE   NORST40                                                          
         CLI   WEEKDAY_FLG,YESQ    DID THIS PROG RUN ON WEEKDAYS?               
         BNE   NORST50             NO. USE THE SATURDAY TELECASTS               
         B     NORST90             YES. SKIP ALL THE SAT AND SUN                
*                                   TELECASTS LEFT IN THE TABLE                 
NORST40  CLI   TELTYPE,SUNDAYQ                                                  
         BNE   NORST50                                                          
         CLI   WEEKDAY_FLG,YESQ    DID THIS PROG RUN ON WEEKDAYS?               
         BNE   NORST50             NO. USE THE SATURDAY TELECASTS               
         B     NORST90             YES. SKIP ALL THE SUNDAY TELECASTS           
*                                    LEFT IN THE TABLE                          
*                                                                               
* GO THROUGH THE TELECASTS FOR ALL WEEKDAYS, OR SATURDAY, OR                    
* SUNDAY. TELECASTS ARE SORTED BY START QH/ END QH. KEEP TRACK OF               
* HIGHEST FREQUENCY OF THE START QH.                                            
NORST50  CLC   TELSQH,PREV_SQH                                                  
         BNE   NORST70                                                          
         AHI   R0,1                SAME SQH. UPDATE SQH FREQUENCY               
         B     NORST85                                                          
*                                                                               
NORST70  CR    R0,R5               DID PREV SQH HAVE THE HIGHEST FREQ?          
         BNH   NORST80                                                          
         MVC   NORSQH,PREV_SQH     YES. THIS COULD BE THE "NORMAL" SQH          
         LR    R5,R0               UPDATE MAXIMUM FREQUENCY                     
NORST80  LHI   R0,1                START NEW COUNT FOR THE NEW SQH              
*                                                                               
NORST85  MVC   PREV_SQH,TELSQH     SAVE PREVIOUS SQH                            
         LA    R4,TELTABQ(R4)                                                   
         BCT   R1,NORST30                                                       
*                                                                               
NORST90  CR    R0,R5               DID LAST SQH HAVE THE HIGHEST FREQ?          
         BNH   *+12                                                             
         MVC   NORSQH,PREV_SQH     YES. THIS COULD BE THE "NORMAL" SQH          
         LR    R5,R0               UPDATE MAXIMUM FREQUENCY                     
*                                                                               
         DROP  R4                                                               
*                                                                               
         CHI   R5,1                HIGHEST FREQUENCY IS 1, I.E.ONLY ONE         
         BE    NORSTNO             TELECAST, OR ALL THE SQH'S VARIED.           
*                                  NO "NORMAL" (RULES A AND C)                  
*                                                                               
NORSTYES CR    RB,RB                                                            
         B     NORSTX                                                           
NORSTNO  CHI   RB,0                                                             
*                                                                               
NORSTX   L     RE,SAVERE                                                        
         BSM   0,RE                                                             
*                                                                               
***********************************************************************         
* DETERMINE THE "NORMAL" END TIME.                                              
*                                                                               
* RULES FROM NIELSEN:                                                           
*   A. IN ORDER TO HAVE A "NORMAL" END TIME, A "NORMAL" START TIME MUST         
*      HAVE BEEN PREVIOUSLY ESTABLISHED.                                        
*   B. THE END QUARTER HOUR WITH THE GREATEST FREQUENCY OF OCCURENCE            
*      AMONG TELECASTS FOR ALL DAYS AND WEEKS STARTING AT THE "NORMAL"          
*      START TIME.                                                              
*   C. IF TWO OR MORE END QUARTER HOURS OCCUR WITH THE GREATEST                 
*      FREQUENCY, THEN THE "NORMAL" END TIME WILL BE THE EARLIEST QH.           
*   D. IF ALL END TIMES VARY, THEN THE "NORMAL" END TIME WILL BE THE            
*      EARLIEST OF THESE QUARTER HOURS.                                         
*   E. ONLY WEEKDAY END TIMES ARE USED TO DETERMINE THE "NORMAL" END            
*      TIME FOR A PROGRAM THAT WAS TELECAST ON WEEKDAYS AND SATURDAY            
*      AND/OR SUNDAY.                                                           
*   F. A PROGRAM THAT WAS TELECAST ON SATURDAY AND/OR SUNDAY WILL USE           
*      ONLY THE SATURDAY TELECASTS TO DETERMINE A SATURDAY "NORMAL" END         
*      TIME, AND WILL USE ONLY THE SUNDAY TELECASTS TO DETERMINE THE            
*      SUNDAY "NORMAL" END TIME.                                                
*      (SINCE THE SUB-PROGRAM ID SEPARATES SATURDAY AND SUNDAY,                 
*       NO EXTRA CODING IS NEEDED FOR THIS RULE)                                
*                                                                               
* INPUT: NORSQH HAS THE ESTABLISHED "NORMAL" START QUARTER HOUR.                
*        TELTAB HAS A LIST OF TELECASTS SORTED BY TYPE/SQH/EQH.                 
*                                                                               
***********************************************************************         
NORETIME DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
* USE ONLY THE TELECASTS THAT HAVE THE "NORMAL" START QH PREVIOUSLY             
* ESTABLISHED.                                                                  
         LA    R4,TELTAB                                                        
         USING TELTABD,R4                                                       
         L     R1,NUMVALS                                                       
*                                                                               
         MVC   NOREQH,TELEQH     START OUT WITH EARLIEST END QH                 
         MVC   NORTYPE,TELTYPE   THE FIRST TYPE IS ALL WE NEED                  
         MVI   PREV_EQH,X'FF'    PREVIOUS END QH                                
         SR    R5,R5             MAXIMUM FREQUENCY SO FAR                       
         SR    R0,R0             CURRENT FREQUENCY                              
*                                                                               
NORET10  CLC   NORSQH,TELSQH     SEARCH FOR THE FIRST TELECAST WITH             
         BE    NORET20             THE ALREADY ESTABLISHED "NORMAL"             
NORET15  LA    R4,TELTABQ(R4)      START QH                                     
         BCT   R1,NORET10                                                       
         DC    H'0'              SQH SHOULD BE IN TABLE                         
*                                                                               
NORET20  CLC   NORTYPE,TELTYPE   THE FIRST TYPE IS ALL WE NEED                  
         BNE   NORET50           WE'RE DONE WITH THE TELECASTS FOR              
         CLC   NORSQH,TELSQH      THIS TYPE AND SQH                             
         BNE   NORET50                                                          
*                                                                               
         CLC   TELEQH,PREV_EQH                                                  
         BNE   NORET30                                                          
         AHI   R0,1                SAME EQH. UPDATE EQH FREQUENCY               
         B     NORET40                                                          
*                                                                               
NORET30  CR    R0,R5               DID PREV EQH HAVE THE HIGHEST FREQ?          
         BNH   NORET35                                                          
         MVC   NOREQH,PREV_EQH     YES. THIS COULD BE THE "NORMAL" EQH          
         LR    R5,R0               UPDATE MAXIMUM FREQUENCY                     
NORET35  LHI   R0,1                START NEW COUNT FOR THE NEW EQH              
*                                                                               
NORET40  MVC   PREV_EQH,TELEQH     SAVE PREVIOUS EQH                            
         LA    R4,TELTABQ(R4)                                                   
         BCT   R1,NORET20                                                       
*                                                                               
NORET50  CR    R0,R5               DID LAST EQH HAVE THE HIGHEST FREQ?          
         BNH   *+12                                                             
         MVC   NOREQH,PREV_EQH     YES. THIS COULD BE THE "NORMAL" EQH          
         LR    R5,R0               UPDATE MAXIMUM FREQUENCY                     
*                                                                               
         DROP  R4                                                               
*                                                                               
NORETX   L     RE,SAVERE                                                        
         BSM   0,RE                                                             
*                                                                               
***********************************************************************         
* CLEAR TABLE OF TELECASTS AND OTHER RELATED FIELDS.                            
***********************************************************************         
RESETPRG DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
         XC    NUMVALS,NUMVALS     RESET NO OF TELECASTS                        
         XC    NUMVALNX,NUMVALNX   RESET NO OF TELECASTS                        
*                                                                               
         LA    RE,TELTAB           CLEAR TELECAST TABLE                         
         LHI   RF,TELTABL                                                       
         XCEF                                                                   
*                                                                               
         CLC   W_KDTYPE,PREV_KDTYPE                                             
         BNE   RESP10                                                           
         CLC   W_PNRPNAME,PREV_PNRPNAME    SAME PROGRAM NAME...                 
         BNE   RESP10                                                           
         CLC   W_PNRPGID,PREV_PNRPGID      AND PROGRAM ID...                    
         BNE   RESP10                                                           
         CLI   WEEKEND_ONLY_FLAG,YESQ      AND WEEKEND ONLY PROGRAM?            
         BNE   RESP10                                                           
         LH    R1,SUB_PGID         YES. BUMP THE SUB-PROGRAM ID                 
         AHI   R1,1                                                             
         STH   R1,SUB_PGID                                                      
         B     RESP20                                                           
*                                                                               
RESP10   MVC   SUB_PGID,=H'1'      NO. NEW PROGRAM.RESET SUB-PROGRAM ID         
*                                                                               
RESP20   XC    PREV_KDTYPE,PREV_KDTYPE                                          
         MVI   WEEKEND_ONLY_FLAG,YESQ                                           
*                                                                               
         L     RE,SAVERE                                                        
         BSM   0,RE                                                             
*                                                                               
***********************************************************************         
* CONVERT QUARTER HOUR TO TIME IN FORMAT HH:MM(AM/PM)                           
* INPUT: DUB(4) HAS ADDRESS OF START QUARTER HOUR                               
*        DUB+4(4) HAS ADDRESS OF OUTPUT AREA                                    
***********************************************************************         
*                                                                               
QHTOTIME DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
         MVC   DMCB,DUB          ADDRESS OF START QUARTER HOUR                  
         GOTO1 =V(QHTOHR2),DMCB,,(0,HALF)                                       
         SR    RE,RE                                                            
         ICM   RE,3,HALF                                                        
*                                                                               
         L     R5,DUB+4          ADDRESS OF OUTPUT AREA                         
         MVC   5(2,R5),=C'AM'                                                   
         CHI   RE,1200           1200 TO 2359 IS PM                             
         BL    QHTOTI10                                                         
         CHI   RE,2359                                                          
         BH    QHTOTI10                                                         
         MVC   5(2,R5),=C'PM'                                                   
*                                                                               
QHTOTI10 CHI   RE,1300           1300 TO 2400 INCLUDED NEEDS TO                 
         BL    *+8                  SUBTRACT 1200                               
         SHI   RE,1200                                                          
         CHI   RE,0059           0001 TO 0059 INCLUDED NEEDS TO                 
         BH    *+8                  ADD 1200                                    
         AHI   RE,1200                                                          
*                                                                               
         EDIT  (RE),(4,FULL),ZERO=NOBLANK,FILL=0                                
         MVC   0(2,R5),FULL       HH                                            
         MVI   2(R5),C':'         :                                             
         MVC   3(2,R5),FULL+2     MM                                            
*                                                                               
         L     RE,SAVERE                                                        
         BSM   0,RE                                                             
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
* CALL A DDS SUBROUTINE. THIS REQUIRES ESTABLISHING A NEW RD CHAIN,             
* BECAUSE THIS PROGRAM IS A DFSORT EXIT, AND DOES NOT CONFORM TO DDS            
* STANDARD REGISTER USAGE.                                                      
*                                                                               
* INPUT REGISTERS ARE STANDARD:                                                 
*   R1 = A(PARAMETER LIST)                                                      
*   RE = RETURN ADDRESS                                                         
*   RF = A(ROUTINE TO CALL)                                                     
*                                                                               
***********************************************************************         
*                                                                               
CALL_DDS_SUBRTN DS 0H                                                           
         STM   RE,RC,GPRSAVE       SAVE CALLER'S REGISTERS                      
         LR    R0,RD               SAVE CALLER'S RD LOCALLY                     
         DROP  RC                                                               
*                                                                               
         BASR  RB,0                                                             
         AHI   RB,-2                                                            
         USING *-6,RB              MAKE THIS ROUTINE ADDRESSABLE                
         L     RD,=V(REGSAVE)      DDS WORKING STORAGE                          
*                                                                               
         BASR  RE,RF               CALL EXTERNAL SUBROUTINE                     
*                                                                               
         LR    RD,R0               RESTORE CALLER'S RD                          
         LM    RE,RC,GPRSAVE       RESTORE CALLER'S REGISTERS                   
         BSM   0,RE                EXIT                                         
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
         ORG   DELMXP2+(((*-DELMXP2)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
GPRSAVE  DS    15F                 INTERNAL CALLER'S SAVED RE-RC                
SAVERE   DS    F                   SAVED RE                                     
DFSORT_HIGH_HALVES DS 16F                                                       
DMCB     DS    6F                  PARAMETERS TO CALL_DDS_SUBRTN                
         SPACE 3                                                                
SNAPDUMP DCB   DDNAME=SNAPDUMP,DSORG=PS,RECFM=VBA,MACRF=(W),LRECL=125, +        
               BLKSIZE=1632                                                     
*                                                                               
HEXTAB   DC    C'0123456789ABCDEF'                                              
*                                                                               
HDR1L    DC    AL1(HDR1LQ)         L'HEADER                                     
HDR1     DC    C'*** YOU DIED AT OFFSET '                                       
HDR1HEXD DS    CL6                 HEX OFFSET WITHIN CSECT                      
         DC    C' IN DFSORT EXIT '                                              
HDR1NAME DC    CL8' '              EXIT NAME                                    
         DC    C'. SORT RECORD FOLLOWS ***'                                     
HDR1LQ   EQU   *-HDR1                                                           
*                                                                               
WORK     DS    XL64                                                             
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
THREE    DS    XL3                                                              
*                                                                               
NOQ      EQU   0                                                                
YESQ     EQU   1                                                                
*                                                                               
PREV_KDTYPE DC XL(L'W_KDTYPE)'00'                                               
PREV_PNRPNAME DS CL(L'W_PNRPNAME)                                               
PREV_PNRPGID DS XL(L'W_PNRPGID)                                                 
PREV_KDAY DS CL(L'W_KDAY)                                                       
PREV_KQHR DS XL(L'W_KQHR)                                                       
*                                                                               
PREV_SQH DS    XL(L'TELSQH)                                                     
PREV_EQH DS    XL(L'TELEQH)                                                     
*                                                                               
SUB_PGID DC    H'1'                                                             
*                                                                               
NORSQH   DS    XL(L'TELSQH)   "NORMAL" START QH                                 
NOREQH   DS    XL(L'TELEQH)   "NORMAL" END QH                                   
NORTYPE  DS    XL(L'TELTYPE)  "NORMAL" TYPE (WEEKDAY, SAT, SUN)                 
*                                                                               
LASTNOR  DC    AL1(NOQ)       RELEASED LAST RECORD FLAG                         
*                                                                               
WEEKEND_ONLY_FLAG DC AL1(YESQ)  WEEKEND-ONLY PROGRAM FLAG                       
*                                                                               
WEEKDAY_FLG DS AL1            WEEKDAYS FLAG                                     
*                                                                               
NUMVALS  DC    A(0)           CURRENT NUMBER OF VALUES IN TABLE                 
NUMVALNX DC    A(0)           NUMBER OF VALUES FOR NEXT TYPE                    
*                                                                               
         DS    0L                                                               
OUTREC   DS    CL(W_LMRECLQ)  OUTPUT RECORD                                     
*                                                                               
TELTAB   DS    1000CL(TELTABQ)                                                  
TELTABL  EQU   *-TELTAB                                                         
*                                                                               
TELTABD  DSECT                                                                  
TELNUM   DS    CL4            TELECAST ID                                       
TELTYPE  DS    HL1            WEEKDAY TYPE                                      
WEEKDAYQ       EQU  0         WEEKDAY (MONDAY - FRIDAY)                         
SATURDAYQ      EQU  6         SATURDAY                                          
SUNDAYQ        EQU  7         SUNDAY                                            
TELSQH   DS    XL1            TELECAST START QH                                 
TELEQH   DS    XL1            TELECAST END QH                                   
TELTABQ  EQU   *-TELTABD                                                        
*                                                                               
         EJECT                                                                  
* INPUT RECORDS DSECT                                                           
       ++INCLUDE DELMDSECT                                                      
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DELMXP2   02/28/14'                                      
         END                                                                    
