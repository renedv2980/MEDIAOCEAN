*          DATA SET DELMXP9    AT LEVEL 002 AS OF 07/27/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMXP9B                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE QSORT                                                                  
*INCLUDE LMTIME2                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
***********************************************************************         
* DFSORT USER EXIT FOR THE LOCAL MONTHLY PROGRAM AVERAGE.             *         
*THIS EXIT IS A COPY OF DELMXP3 AND MODIFIED FOR 3AM START OF DAY               
*                                                                     *         
* IT DETERMINES THE OVERALL "REPORT" START AND END TIME FOR EACH      *         
* PROGRAM, AND WRITES THEM TO SEPARATE RECORDS.                       *         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DELMICETPA, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.         *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* NOTE: CALLS TO EXTERNAL SUBROUTINES *MUST* BE MADE VIA              *         
*       CALL_DDS_SUBRTN, BECAUSE THIS EXIT'S USE OF REGISTERS IS      *         
*       NON-DDS-CONFORMING.                                           *         
***********************************************************************         
DELMXP9  CSECT                                                                  
*                                                                               
         ENTRY E35                 MUST BE "E35" (DFSORT OUTPUT EXIT)           
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E35,RC              RC = PROGRAM BASE REGISTER                   
E35      SAVE  (14,12),,DELMXP9                                                 
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
**********************************************************************          
* DETERMINE THE OVERALL "REPORT" START AND END TIME FOR EACH PROGRAM.           
*                                                                               
* INPUT:  WORK RECORDS AT THE QH LEVEL, SORTED BY PROGRAM/DAY.                  
* -----   (A PROGRAM IS DEFINED BY THE COMBINATION OF PROGRAM NAME/             
*          PROGRAM ID/ SUB-PROGRAM ID)                                          
*                                                                               
* OUTPUT: KEEP THE WORK RECORDS.                                                
* ------  CREATE ADDITIONAL RECORDS THAT HOLD THE OVERALL "REPORT"              
*         START AND END TIMES.                                                  
*                                                                               
* RULES:  THE OVERALL "REPORT" START TIME IS THE EARLIEST OF THE MOST           
* -----   FREQUENT START TIMES ACROSS ALL DAYS AND WEEKS.                       
*                                                                               
*         FOR PROGRAMS THAT RAN ON WEEKDAYS AND SATURDAY AND/OR SUNDAY,         
*         ONLY THE WEEKDAYS ARE USED TO DETERMINE THE "REPORT" START            
*         AND END TIME.                                                         
*                                                                               
*         NIELSEN DOESN'T PROVIDE A DEFINITION FOR THE "REPORT" END             
*         TIME, AND NO SUCH TIME EXISTS ON THE VIP. HOWEVER, DDS WILL           
*         DERIVE A "REPORT" END TIME IN THE SAME WAY WE DERIVE THE              
*         START TIME.  WE APPLY THE SAME RULES TO THE END TIMES OF              
*         THE TELECASTS THAT START AT THE "REPORT" START TIME.                  
*                                                                               
*         WE DECIDED TO DERIVE THE "REPORT" END TIME BECAUSE ON THE             
*         VIP, WHERE THE END TIME IS NOT AVAILABLE, WE DERIVE IT FROM           
*         THE START TIME AND TOTAL DURATION, WHICH SOMETIMES YIELDS             
*         INCORRECT RESULTS. ON THE LOCAL MONTHLIES WE HAVE THE                 
*         INFORMATION AVAILABLE TO CREATE A MORE ACCURATE END TIME.             
*                                                                               
* NOTE 1: THE OVERALL "REPORT" START AND END TIMES ARE DERIVED FROM             
*         *ALL* THE TELECASTS OF THE PROGRAM, AND ARE NOT SPECIFIC TO           
*         THE KIND OF AVERAGE BEING COMPUTED. (FOR EXAMPLE, AN                  
*         INDIVIDUAL DAY AVERAGE FOR MONDAY WILL *NOT* REFLECT ONLY THE         
*         MONDAY AIRINGS, BUT ALL THE WEEKDAY AIRINGS.) AS A RESULT,            
*         THE SAME OVERALL "REPORT" START AND END TIME APPLY TO ALL             
*         AVERAGES FOR A GIVEN PROGRAM.                                         
*                                                                               
* NOTE 2: ALTHOUGH THE RULES FOR OVERALL "REPORT" TIMES AND "NORMAL"            
*         TIMES ARE VERY SIMILAR, THE "REPORT" TIMES EXIST FOR EVERY            
*         PROGRAM, WHILE ONLY SOME PROGRAMS HAVE A "NORMAL". HOWEVER            
*         IF A "NORMAL" DOES EXIST, THE "NORMAL" START/END TIMES WILL           
*         MATCH THE OVERALL "REPORT" START/END TIMES.                           
*                                                                               
**********************************************************************          
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
*                                                                               
         LTR   R3,R3               EOF?                                         
         BNZ   MAIN20              NO                                           
         CLI   LASTPRG,YESQ        DID WE DETERMINE "REP" TIMES FOR THE         
         BNE   REPTIMES            LAST PROGRAM? NO. GO DO IT.                  
         B     EOF                 YES. WE ARE DONE.                            
*                                                                               
         USING LMDSECT,R3                                                       
*                                                                               
MAIN20   OC    PREV_KDTYPE,PREV_KDTYPE   FIRST TIME IN,                         
         BNZ   MAIN30              OR JUST FINISHED PROCESSING REP TMS.         
         BZ    ADTELINF            ADD TELECAST INFORMATION TO TABLE            
*                                                                               
MAIN30   CLC   W_KDTYPE,PREV_KDTYPE     CHANGE IN DATA TYPE,                    
         BNE   REPTIMES                  NEW PROGRAM                            
         CLC   W_PNRPNAME,PREV_PNRPNAME CHANGE IN PROGRAM NAME,                 
         BNE   REPTIMES                  NEW PROGRAM                            
         CLC   W_PNRPGID,PREV_PNRPGID   CHANGE IN PROGRAM ID,                   
         BNE   REPTIMES                  NEW PROGRAM                            
         CLC   W_PNRSPGID,PREV_PNRSPGID CHANGE IN SUB-PROGRAM ID,               
         BNE   REPTIMES                  NEW PROGRAM                            
         B     ADTELINF            KEEP TRACK OF TELECAST DAYS/TIMES            
*                                                                               
***********************************************************************         
* ADD THIS TELECAST TO A TABLE THAT WILL BE USED LATER TO DETERMINE             
* THE OVERALL "REPORT" START AND END TIMES.                                     
***********************************************************************         
ADTELINF DS    0X                                                               
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
         B     ADDTOT10                                                         
         CLI   W_KDAY,SUNDAY                                                    
         BNE   *+12                                                             
         MVI   TELTYPE,SUNDAYQ         SUNDAY                                   
         B     ADDTOT10                                                         
         MVI   TELTYPE,WEEKDAYQ        WEEKDAY (MON THROUGH FRI)                
         DROP  R4                                                               
*                                                                               
ADDTOT10 GOTOR ,DMCB,(X'01',DUB),TELTAB,NUMVALS,TELTABQ,               +        
               ('TELNUM-TELTABD',L'TELNUM),TELTABL/TELTABQ                      
         L     RF,=V(BINSRCH)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         OC    0(4,R1),0(R1)       TABLE FULL?                                  
         BNZ   ADDTOT20                                                         
***      DC    H'0'                TELECASTS DON'T FIT IN TELTAB                
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR               ERROR EXIT                                   
*                                                                               
ADDTOT20 MVC   NUMVALS,DMCB+8      CURRENT NUMBER OF VALUES IN TABLE            
         MVC   PREV_KDTYPE,W_KDTYPE       SAVE PREVIOUS VALUES                  
         MVC   PREV_PNRPNAME,W_PNRPNAME                                         
         MVC   PREV_PNRPGID,W_PNRPGID                                           
         MVC   PREV_PNRSPGID,W_PNRSPGID                                         
*                                                                               
         B     KEEPREC             AND KEEP THE RECORD                          
*                                                                               
***********************************************************************         
* ANALYZE THE TELECASTS OF THE PREVIOUS PROGRAM AND DETERMINE THE               
* OVERALL "REPORT" START AND END TIMES. CREATE A "REPORT" TIMES RECORD.         
***********************************************************************         
REPTIMES DS    0X                                                               
*                                                                               
         LTR   R3,R3               WAS THIS THE LAST RECORD?                    
         BNZ   *+8                                                              
         MVI   LASTPRG,YESQ        YES. WE ARE CHECKING THE LAST PROG           
*                                                                               
         BAS   RE,REPSTIME         DERIVE THE "REPORT" START TIME               
*                                                                               
         BAS   RE,REPETIME         DERIVE THE "REPORT" END TIME                 
*                                                                               
         LA    R4,OUTREC           BUILD THE REPORT TIMES RECORD                
         USING REP_RECD,R4                                                      
*                                                                               
         MVC   REP_SIGNAL,=C'** REPORT TIMES **'                                
         MVC   REP_KDTYPE,PREV_KDTYPE     DATA TYPE                             
         MVC   REP_PNAME,PREV_PNRPNAME    PROGRAM NAME                          
         MVC   REP_PGID,PREV_PNRPGID      PROGRAM ID                            
         MVC   REP_SPGID,PREV_PNRSPGID    SUB-PROGRAM ID                        
         MVC   REP_SQH,REPSQH             "REPORT" START QUARTER HOUR           
         MVC   REP_EQH,REPEQH             "REPORT" END QUARTER HOUR             
*                                                                               
* CONVERT OVERALL "REPORT" START QH TO TIME                                     
         LA    RE,REP_SQH        "REPORT" START QUARTER HOUR                    
         ST    RE,DUB                                                           
         LA    RE,REP_STIME      OUTPUT AREA FOR TIME                           
         ST    RE,DUB+4                                                         
         BAS   RE,QHTOTIME                                                      
*                                                                               
* CONVERT OVERALL "REPORT" END QH TO TIME                                       
         ZIC   RF,REP_EQH        "REPORT" END QUARTER HOUR                      
         LA    RF,1(RF)          +1 TO DISPLAY THE FIRST MIN AFTER THE          
         STC   RF,HALF              END OF THE PROGRAM                          
         LA    RE,HALF           A(ADJUSTED END QH)                             
         ST    RE,DUB                                                           
         LA    RE,REP_ETIME      OUTPUT AREA FOR END TIME                       
         ST    RE,DUB+4                                                         
         BAS   RE,QHTOTIME                                                      
*                                                                               
         DROP  R4                                                               
*                                                                               
         BAS   RE,RESETPRG       CLEAR THE TELECAST TABLE                       
*                                                                               
         B     ADDREC            RELEASE THE "REPORT" TIMES RECORD              
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
* DETERMINE THE OVERALL "REPORT" START TIME.                                    
***********************************************************************         
REPSTIME DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
* SORT TELECASTS ASCENDING ON:                                                  
* TYPE(WEEKDAY/SAT/SUN) / START QH / END QH.                                    
* WEEKEND AIRINGS GET SORTED AFTER THE WEEKDAY AIRINGS.                         
*                                                                               
REPST01  GOTOR ,DMCB,(0,TELTAB),NUMVALS,TELTABQ,                       +        
               L'TELTYPE+L'TELSQH+L'TELEQH,TELTYPE-TELTABD                      
         L     RF,=V(QSORT)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
* IF TELECASTS ARE PRESENT FOR WEEKDAYS, THE SATURDAY AND SUNDAY                
* TELECASTS WILL NOT BE USED. THE SATURDAY AND SUNDAY TELECASTS WILL            
* ONLY BE USED IF THERE ARE NO WEEKDAYS PRESENT.                                
*                                                                               
REPST15  LA    R4,TELTAB                                                        
         USING TELTABD,R4                                                       
*                                                                               
         MVI   WEEKDAY_FLG,NOQ     WEEKDAYS FLAG                                
         L     R1,NUMVALS          NUMBER OF ENTRIES IN THE TABLE               
*                                                                               
REPST20  MVC   REPSQH,TELSQH       START OUT WITH EARLIEST START QH             
         MVI   PREV_SQH,X'FF'      PREVIOUS START QH                            
         SR    R5,R5               MAXIMUM FREQUENCY SO FAR                     
         SR    R0,R0               CURRENT FREQUENCY                            
*                                                                               
REPST30  CLI   TELTYPE,WEEKDAYQ                                                 
         BNE   *+8                                                              
         MVI   WEEKDAY_FLG,YESQ    WEEKDAYS ARE PRESENT FOR THIS PROG           
*                                                                               
         CLI   TELTYPE,SATURDAYQ                                                
         BNE   REPST40                                                          
         CLI   WEEKDAY_FLG,YESQ    DID THIS PROG RUN ON WEEKDAYS?               
         BNE   REPST50             NO. USE THE SATURDAY TELECASTS               
         B     REPST90             YES. SKIP ALL THE SAT AND SUN                
*                                   TELECASTS LEFT IN THE TABLE                 
REPST40  CLI   TELTYPE,SUNDAYQ                                                  
         BNE   REPST50                                                          
         CLI   WEEKDAY_FLG,YESQ    DID THIS PROG RUN ON WEEKDAYS?               
         BNE   REPST50             NO. USE THE SATURDAY TELECASTS               
         B     REPST90             YES. SKIP ALL THE SUNDAY TELECASTS           
*                                    LEFT IN THE TABLE                          
*                                                                               
* GO THROUGH THE TELECASTS FOR ALL WEEKDAYS, OR SATURDAY, OR                    
* SUNDAY. TELECASTS ARE SORTED BY START QH/ END QH. KEEP TRACK OF               
* HIGHEST FREQUENCY OF THE START QH.                                            
REPST50  CLC   TELSQH,PREV_SQH                                                  
         BNE   REPST70                                                          
         AHI   R0,1                SAME SQH. UPDATE SQH FREQUENCY               
         B     REPST85                                                          
*                                                                               
REPST70  CR    R0,R5               DID PREV SQH HAVE THE HIGHEST FREQ?          
         BNH   REPST80                                                          
         MVC   REPSQH,PREV_SQH     YES. THIS COULD BE THE "REPORT" SQH          
         LR    R5,R0               UPDATE MAXIMUM FREQUENCY                     
REPST80  LHI   R0,1                START NEW COUNT FOR THE NEW SQH              
*                                                                               
REPST85  MVC   PREV_SQH,TELSQH     SAVE PREVIOUS SQH                            
         LA    R4,TELTABQ(R4)                                                   
         BCT   R1,REPST30                                                       
*                                                                               
REPST90  CR    R0,R5               DID LAST SQH HAVE THE HIGHEST FREQ?          
         BNH   *+10                                                             
         MVC   REPSQH,PREV_SQH     YES. THIS IS THE "REPORT" SQH                
*                                                                               
         DROP  R4                                                               
*                                                                               
         L     RE,SAVERE                                                        
         BSM   0,RE                                                             
*                                                                               
***********************************************************************         
* DETERMINE THE OVERALL "REPORT" END TIME.                                      
***********************************************************************         
REPETIME DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
* USE ONLY THE TELECASTS THAT HAVE THE OVERALL "REPORT" START QH                
* PREVIOUSLY ESTABLISHED.                                                       
         LA    R4,TELTAB                                                        
         USING TELTABD,R4                                                       
         L     R1,NUMVALS                                                       
*                                                                               
         MVC   REPEQH,TELEQH       START OUT WITH EARLIEST END QH               
         MVC   REPTYPE,TELTYPE     THE FIRST TYPE IS ALL WE NEED                
         MVI   PREV_EQH,X'FF'      PREVIOUS END QH                              
         SR    R5,R5               MAXIMUM FREQUENCY SO FAR                     
         SR    R0,R0               CURRENT FREQUENCY                            
*                                                                               
REPET10  CLC   REPSQH,TELSQH     SEARCH FOR THE FIRST TELECAST WITH             
         BE    REPET20             THE ALREADY ESTABLISHED "REPORT"             
REPET15  LA    R4,TELTABQ(R4)      START QH                                     
         BCT   R1,REPET10                                                       
         DC    H'0'              TYPE AND SQH SHOULD BE IN TABLE                
*                                                                               
REPET20  CLC   REPTYPE,TELTYPE   THE FIRST TYPE IS ALL WE NEED                  
         BNE   REPET50           WE'RE DONE WITH THE TELECASTS FOR              
         CLC   REPSQH,TELSQH      THIS TYPE AND SQH                             
         BNE   REPET50                                                          
*                                                                               
         CLC   TELEQH,PREV_EQH                                                  
         BNE   REPET30                                                          
         AHI   R0,1                SAME EQH. UPDATE EQH FREQUENCY               
         B     REPET40                                                          
*                                                                               
REPET30  CR    R0,R5               DID PREV EQH HAVE THE HIGHEST FREQ?          
         BNH   REPET35                                                          
         MVC   REPEQH,PREV_EQH     YES. THIS COULD BE THE "REPORT" EQH          
         LR    R5,R0               UPDATE MAXIMUM FREQUENCY                     
REPET35  LHI   R0,1                START NEW COUNT FOR THE NEW EQH              
*                                                                               
REPET40  MVC   PREV_EQH,TELEQH     SAVE PREVIOUS EQH                            
         LA    R4,TELTABQ(R4)                                                   
         BCT   R1,REPET20                                                       
*                                                                               
REPET50  CR    R0,R5               DID LAST EQH HAVE THE HIGHEST FREQ?          
         BNH   *+10                                                             
         MVC   REPEQH,PREV_EQH     YES. THIS IS THE "REPORT" EQH                
*                                                                               
         DROP  R4                                                               
*                                                                               
         L     RE,SAVERE                                                        
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
         XC    PREV_KDTYPE,PREV_KDTYPE                                          
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
*                                                                               
         MVI   BYTE,1            3A START TIME                                  
*                                                                               
         GOTO1 =V(QHTOHR3),DMCB,,(0,HALF),BYTE                                  
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
         LTORG                                                                  
         SPACE 3                                                                
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
         ORG   DELMXP9+(((*-DELMXP9)/256)+1)*256  FOR I-CACHE PIPELINE          
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
BYTE     DS    X                                                                
*                                                                               
NOQ      EQU   0                                                                
YESQ     EQU   1                                                                
*                                                                               
PREV_KDTYPE DC XL(L'W_KDTYPE)'00'                                               
PREV_PNRPNAME DS CL(L'W_PNRPNAME)                                               
PREV_PNRPGID DS XL(L'W_PNRPGID)                                                 
PREV_PNRSPGID DS XL(L'W_PNRSPGID)                                               
*                                                                               
PREV_SQH DS    XL(L'TELSQH)                                                     
PREV_EQH DS    XL(L'TELEQH)                                                     
*                                                                               
REPSQH   DS    XL(L'TELSQH)   "REPORT" START QH                                 
REPEQH   DS    XL(L'TELEQH)   "REPORT" END QH                                   
REPTYPE  DS    XL(L'TELTYPE)   TYPE (WEEKDAY, SAT, SUN)                         
*                                                                               
LASTPRG  DC    AL1(NOQ)       RELEASED LAST RECORD FLAG                         
*                                                                               
WEEKDAY_FLG DS AL1            WEEKDAYS FLAG                                     
*                                                                               
NUMVALS  DC    A(0)           CURRENT NUMBER OF VALUES IN TABLE                 
NUMVALNX DC    A(0)           NUMBER OF VALUES FOR NEXT TYPE                    
ANXTTYPE DS    A              A(NEXT TYPE TO PROCESS)                           
*                                                                               
         DS    0L                                                               
OUTREC   DS    CL(W_LMRECLQ)  OUTPUT RECORD                                     
*                                                                               
TELTAB   DS    1000CL(TELTABQ)                                                  
TELTABL  EQU   *-TELTAB                                                         
*                                                                               
TELTABD  DSECT                                                                  
TELNUM   DS    CL4            TELECAST ID                                       
TELTYPE  DS    HL1            TYPE                                              
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
**PAN#1  DC    CL21'002DELMXP9   07/27/15'                                      
         END                                                                    
