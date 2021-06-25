*          DATA SET DELMXP7    AT LEVEL 010 AS OF 05/08/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMXP7A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE QSORT                                                                  
***********************************************************************         
* DFSORT USER EXIT FOR THE LOCAL MONTHLY PROGRAM AVERAGE.             *         
*                                                                     *         
* IT CALCULATES THE MULTI-DAY "NORMAL" AVERAGES WHERE THEY EXIST.     *         
*                                                                     *         
* THE EXIT CAN BE CALLED 2 DIFFERENT WAYS, IDENTIFIED BY FIELD        *         
* W_WEEKEND_AVG:                                                      *         
*   1. CREATE MULTI-DAY "NORMAL" AVERAGES INCLUDING WEEKENDS.         *         
*   2. CREATE MULTI-DAY "NORMAL" AVERAGES EXCLUDING WEEKENDS.         *         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DELMICETPA, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.         *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* NOTE: CALLS TO EXTERNAL SUBROUTINES *MUST* BE MADE VIA              *         
*       CALL_DDS_SUBRTN, BECAUSE THIS EXIT'S USE OF REGISTERS IS      *         
*       NON-DDS-CONFORMING.                                           *         
***********************************************************************         
DELMXP7  CSECT                                                                  
*                                                                               
         ENTRY E35                 MUST BE "E35" (DFSORT OUTPUT EXIT)           
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E35,RC              RC = PROGRAM BASE REGISTER                   
E35      SAVE  (14,12),,DELMXP7                                                 
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
**********************************************************************          
*                                                                               
* INPUT:  ALL QUARTER HOURS OF PROGRAMS THAT HAVE "NORMALS" AND                 
* -----   MULTI-DAY AVERAGES. THE RECORDS HAVE A FLAG THAT INDICATES            
*         IF THE QUARTER HOUR FALLS INSIDE OR OUTSIDE THE "NORMAL"              
*         BOUNDARIES. THE RECORDS ARE SORTED SO THAT THE QHS THAT               
*         FALL INSIDE THE BOUNDARIES COME BEFORE THE QHS THAT FALL              
*         OUTSIDE THE BOUNDARIES.                                               
*         WITHIN THE BOUNDARIES, RECORDS ARE SORTED BY DAY.                     
*                                                                               
* OUTPUT: FOR PROGRAMS THAT QUALIFY FOR A MULTI-DAY "NORMAL" AVERAGE,           
* ------  CREATE AN AVERAGE RECORD BY AVERAGING TOGETHER THE                    
*         QUARTER HOURS THAT FALL INSIDE THE "NORMAL" BOUNDARIES.               
*                                                                               
* RULES:  A PROGRAM QUALIFIES FOR A MULTI-DAY "NORMAL" AVERAGE UNDER            
* -----   THE FOLLOWING CONDITIONS:                                             
*                                                                               
*         - THERE MUST BE AT LEAST ONE QUARTER HOUR ACROSS ALL                  
*           DAYS THAT IS OUTSIDE THE "NORMAL" START TIME-END TIME.              
*               AND                                                             
*         - IF AVERAGE EXCLUDES WEEKENDS:                                       
*           THERE MUST BE AT LEAST TWO DIFFERENT WEEKDAYS THAT                  
*           CONTRIBUTE QUARTER HOURS THE THE "NORMAL" AVERAGE.                  
*                                                                               
*           IF AVERAGE INCLUDES WEEKENDS:                                       
*           THERE MUST BE AT LEAST TWO DIFFERENT WEEKDAYS AND AT LEAST          
*           ONE WEEKEND DAY THAT CONTRIBUTE QUARTER HOURS THE "NORMAL"          
*           AVERAGE.                                                            
*                                                                               
**********************************************************************          
MAIN10   DS    0H                                                               
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BNZ   MAIN20              NO                                           
         B     EOF                 EXIT                                         
*                                                                               
         USING LMDSECT,R3                                                       
*                                                                               
MAIN20   DS    0H                                                               
         CLC   W_KDTYPE,PREV_KDTYPE        CHANGE IN DATA TYPE                  
         BNE   NEW_NOR                                                          
         CLC   W_PNRPNAME,PREV_PNRPNAME    CHANGE IN PROGRAM NAME               
         BNE   NEW_NOR                                                          
         CLC   W_PNRPGID,PREV_PNRPGID      CHANGE IN PROGRAM ID                 
         BNE   NEW_NOR                                                          
         CLC   W_PNRSPGID,PREV_PNRSPGID    CHANGE IN SUB-PROGRAM ID             
         BNE   NEW_NOR                                                          
         CLC   W_NORSQH,PREV_NORSQH        CHANGE IN "NORMAL" START QH          
         BNE   NEW_NOR                                                          
         CLC   W_NOREQH,PREV_NOREQH        CHANGE IN "NORMAL" END QH            
         BNE   NEW_NOR                                                          
         B     SAME_NOR                                                         
*                                                                               
NEW_NOR  DS    0H                                                               
         MVC   PREV_KDTYPE,W_KDTYPE    SAVE PREVIOUS VALUES                     
         MVC   PREV_PNRPNAME,W_PNRPNAME                                         
         MVC   PREV_PNRPGID,W_PNRPGID                                           
         MVC   PREV_PNRSPGID,W_PNRSPGID                                         
         MVC   PREV_NORSQH,W_NORSQH                                             
         MVC   PREV_NOREQH,W_NOREQH                                             
*                                                                               
         MVI   PREV_KDAY,0                                                      
         MVI   WKDAY_COUNT,0                                                    
         MVI   WKEND_COUNT,0                                                    
*                                                                               
         MVC   ACTWEEKS,BLANKS     RESET ACTIVITY WEEKS                         
         MVC   ACTDAYS,BLANKS      RESET DAY MAP                                
         XC    ADDCOUNT,ADDCOUNT   RESET AVERAGED RECORDS COUNT                 
         LA    RE,QDRBUFF          CLEAR DMA BUFFER                             
         LHI   RF,QDRBUFFL                                                      
         XCEF                                                                   
         LA    RE,HPTBUFF          CLEAR HUT BUFFER                             
         LHI   RF,HPTBUFFL                                                      
         XCEF                                                                   
         LA    RE,HPSBUFF          CLEAR MARKET TOTAL BUFFER                    
         LHI   RF,HPSBUFFL                                                      
         XCEF                                                                   
         LA    RE,STRBUFF          CLEAR TSA BUFFER                             
         LHI   RF,STRBUFFL                                                      
         XCEF                                                                   
*                                                                               
         XC    NUMVALS,NUMVALS     RESET NO OF TELECASTS                        
         L     RE,=A(TELTAB)       CLEAR TELECAST TABLE                         
         LHI   RF,TELTABL                                                       
         XCEF                                                                   
*                                                                               
         MVI   SKIP_FLAG,NOQ         RESET SKIP FLAG                            
*                                                                               
         CLI   W_NORINCL,W_NORINCNQ  IS FIRST QH OUTSIDE THE BNDRIES?           
         BE    *+12                                                             
         BAS   RE,UPDAYCNT           NO. UPDATE THE DAY COUNTS AND              
         B     ADDTOAVG               ADD RECD TO AVERAGE                       
*                                                                               
         MVI   SKIP_FLAG,YESQ        YES. IT MEANS THERE WERE NO                
         B     DELREC                 QHS INSIDE THE BOUNDARIES. SKIP           
*                                     ALL RECORDS FOR THIS "NORMAL"             
SAME_NOR DS    0H                                                               
         CLI   SKIP_FLAG,YESQ        FLAG TO SKIP THE REMAINDER OF              
         BE    DELREC                RECDS FOR THIS "NORMAL"                    
*                                                                               
         CLI   W_NORINCL,W_NORINCYQ  IS QH INSIDE THE BNDRIES?                  
         BNE   SAME_NO5                                                         
         BAS   RE,UPDAYCNT           YES. UPDATE THE DAY COUNTS                 
         B     ADDTOAVG                AND ADD RECD TO AVG                      
*                                                                               
SAME_NO5 MVI   SKIP_FLAG,YESQ        NO. THIS IS A RECD OUTSIDE BNDRS           
         B     RELAVG                 FOLLOWING A RECD INSIDE BNDRS.            
*                                     RELEASE THE "NORMAL" AVERAGE AND          
*                                     SKIP THE REMAINDER RECORDS.               
***********************************************************************         
* ACCUMULATE DEMOS, QH DURATION, DAY AND WEEK MAP FOR THE "NORMAL"              
* AVERAGE.                                                                      
***********************************************************************         
ADDTOAVG DS    0H                                                               
*                                                                               
         LA    RE,OUTREC           CLEAR ENTIRE OUTPUT AREA                     
         LA    RF,W_LMRECLQ                                                     
         XCEF                                                                   
*                                                                               
         LA    RE,OUTREC           MOVE CURRENT RECORD TO OUTREC                
         LA    RF,W_RECNORLQ                                                    
         LR    R0,R3                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         JO    *+2                 DESTRUCTIVE MOVE!                            
*                                                                               
         LH    R1,ADDCOUNT         UPDATE THE NUMBER OF RECORDS THAT            
         LA    R1,1(R1)             GO INTO THE AVERAGE                         
         STH   R1,ADDCOUNT                                                      
*                                                                               
         LA    RE,W_KAWKS          ACCUMULATE THE WEEKS THE PROGRAM RAN         
         LA    RF,ACTWEEKS          IN ACTWEEKS                                 
         LHI   R0,4                MAX 4 WEEKS                                  
ADDTA05  CLI   0(RE),C' '                                                       
         BE    *+10                                                             
         MVC   0(1,RF),0(RE)       ADD WEEK INDICATOR TO ACTWEEKS               
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,ADDTA05                                                       
*                                                                               
         LA    RE,W_KDAYMAP        ACCUMULATE THE DAYS THE PROGRAM RAN          
         LA    RF,ACTDAYS           IN ACTDAYS                                  
         LHI   R0,28               MAX 28 DAYS                                  
ADDTA10  CLI   0(RE),C' '                                                       
         BE    *+10                                                             
         MVC   0(1,RF),0(RE)       ADD DAY INDICATOR TO ACTDAYS                 
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,ADDTA10                                                       
*                                  ACCUMULATE DMA IMPRESSIONS                   
         LA    R4,W_QDRDEMOS       POINT TO FIRST DEMO ON THE RECORD            
         LA    R5,QDRBUFF          POINT TO FIRST DEMO ON THE                   
*                                   ACCUMULATOR BUFFER                          
         LHI   R0,NDEMSQ           NUMBER OF DEMOS TO BE ADDED                  
ADDTA20  ICM   R1,15,0(R4)         RE = DEMO VALUE TO BE ADDED                  
         LM    RE,RF,0(R5)         DOUBLE-WORD ACCUMULATOR                      
         ALR   RF,R1               ADD NEW DEMO AND SET CARRY IN CC             
         ALC   RE,=F'0'            ADD CARRY TO HI-ORDER PORTION OF SUM         
         STM   RE,RF,0(R5)         SET RESULT BACK IN ACCUMULATOR BUFF          
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,ADDTA20                                                       
*                                  ACCUMULATE HUT/PUT VALUES                    
         LA    R4,W_HPTDEMOS       POINT TO FIRST DEMO ON THE RECORD            
         LA    R5,HPTBUFF          POINT TO FIRST DEMO ON THE                   
*                                   ACCUMULATOR BUFFER                          
         LHI   R0,NHDEMSQ          NUMBER OF DEMOS TO BE ADDED                  
ADDTA25  ICM   R1,15,0(R4)         RE = DEMO VALUE TO BE ADDED                  
         LM    RE,RF,0(R5)         DOUBLE-WORD ACCUMULATOR                      
         ALR   RF,R1               ADD NEW DEMO AND SET CARRY IN CC             
         ALC   RE,=F'0'            ADD CARRY TO HI-ORDER PORTION OF SUM         
         STM   RE,RF,0(R5)         SET RESULT BACK IN ACCUMULATOR BUFF          
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,ADDTA25                                                       
*                                  ACCUMULATE MARKET TOTALS                     
         LA    R4,W_HPSDEMOS       POINT TO FIRST DEMO ON THE RECORD            
         LA    R5,HPSBUFF          POINT TO FIRST DEMO ON THE                   
*                                   ACCUMULATOR BUFFER                          
         LHI   R0,NQDEMSQ          NUMBER OF DEMOS TO BE ADDED                  
ADDTA27  ICM   R1,15,0(R4)         RE = DEMO VALUE TO BE ADDED                  
         LM    RE,RF,0(R5)         DOUBLE-WORD ACCUMULATOR                      
         ALR   RF,R1               ADD NEW DEMO AND SET CARRY IN CC             
         ALC   RE,=F'0'            ADD CARRY TO HI-ORDER PORTION OF SUM         
         STM   RE,RF,0(R5)         SET RESULT BACK IN ACCUMULATOR BUFF          
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,ADDTA27                                                       
*                                  ACCUMULATE TSA IMPRESSIONS                   
         LA    R4,W_STRDEMOS       POINT TO FIRST DEMO ON THE RECORD            
         LA    R5,STRBUFF          POINT TO FIRST DEMO ON THE                   
*                                   ACCUMULATOR BUFFER                          
         LHI   R0,NTDEMSQ           NUMBER OF DEMOS TO BE ADDED                 
ADDTA30  ICM   R1,15,0(R4)         RE = DEMO VALUE TO BE ADDED                  
         LM    RE,RF,0(R5)         DOUBLE-WORD ACCUMULATOR                      
         ALR   RF,R1               ADD NEW DEMO AND SET CARRY IN CC             
         ALC   RE,=F'0'            ADD CARRY TO HI-ORDER PORTION OF SUM         
         STM   RE,RF,0(R5)         SET RESULT BACK IN ACCUMULATOR BUFF          
         LA    R4,4(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,ADDTA30                                                       
*                                                                               
* KEEP A BINSRCH TABLE OF TELECASTS AND THEIR START AND END QHS.                
* BINSRCH ELIMINATES DUPLICATE TELECASTS, SO THAT THE END RESULT                
* IS A TABLE OF UNIQUE TELECASTS AND THEIR START AND END QHS.                   
         LA    R4,DUB                                                           
         USING TELTABD,R4                                                       
         MVC   TELSQH,W_PNRSQH     TELECAST START QUARTER HOUR                  
         MVC   TELEQH,W_PNREQH     TELECAST END QUARTER HOUR                    
         MVC   TELNUM,W_PNRSEQ                                                  
         DROP  R4                                                               
         L     RE,=A(TELTAB)                                                    
         ST    RE,DMCB+4                                                        
         GOTOR ,DMCB,(X'01',DUB),,NUMVALS,TELTABQ,('TELNUM-TELTABD',   +        
               L'TELNUM),TELTABL/TELTABQ                                        
         L     RF,=V(BINSRCH)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         OC    0(4,R1),0(R1)       TABLE FULL?                                  
         BNZ   ADDTA40                                                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR               ERROR EXIT                                   
*                                                                               
ADDTA40  MVC   NUMVALS,DMCB+8      CURRENT NUMBER OF VALUES IN TABLE            
*                                                                               
         B     DELREC              DISCARD THIS RECORD                          
*                                                                               
***********************************************************************         
* COMPUTE AND RELEASE THE PREVIOUS AVERAGE RECORD.                              
*                                                                               
* COMPUTE THE AVERAGE BY DIVIDING THE ACCUMULATED DEMOS BY THE                  
* NUMBER OF RECORDS THAT WENT INTO IT. TRUNCATE THE RESULT.                     
***********************************************************************         
RELAVG   DS    0H                                                               
*                                                                               
         ZIC   R0,WKDAY_COUNT     DID IT INCLUDE AT LEAST 2 WEEKDAYS?           
         CHI   R0,2                                                             
         BL    MAIN10             NO. DO NOT RELEASE AVERAGE                    
*                                                                               
         CLI   W_WEEKEND_AVG,W_WKND_INCL   IF THIS AN AVG THAT SHOULD           
         BNE   RELAVG05           INCLUDE WEEKENDS,                             
         ZIC   R0,WKEND_COUNT     DID IT INCLUDE AT LEAST 1 WEEKEND             
         CHI   R0,1               DAY?                                          
         BL    MAIN10             NO. DO NOT RELEASE AVERAGE                    
*                                                                               
* AVERAGE CHECKS OUT. RELEASE AVERAGE                                           
RELAVG05 DS    0H                                                               
*                                                                               
         LA    R7,OUTREC                                                        
OUT      USING LMDSECT,R7                                                       
*                                   ACCUMULATOR BUFFER                          
         LH    R1,ADDCOUNT                                                      
         LTR   R1,R1                                                            
         BNZ   RELAVG07                                                         
*                                  COUNT IS ZERO FOR UNKNOWN REASON             
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR               ERROR EXIT                                   
*                                                                               
RELAVG07 LA    R4,OUT.W_QDRDEMOS   OUTPUT AREA FOR DMA IMPRESSIONS              
         LA    R5,QDRBUFF          POINT TO FIRST DEMO ON THE                   
*                                   ACCUMULATOR BUFFER                          
         LHI   R0,NDEMSQ           NUMBER OF DEMOS TO BE AVERAGED               
RELAVG10 LM    RE,RF,0(R5)         DOUBLE-WORD ACCUMULATED VALUE                
         DR    RE,R1               / NUMBER OF RECORDS                          
         ST    RF,0(R4)            =TRUNCATED AVERAGE                           
*                                                                               
         LA    R4,4(R4)            NEXT DEMO                                    
         LA    R5,8(R5)                                                         
         BCT   R0,RELAVG10                                                      
*                                                                               
         LA    R4,OUT.W_HPTDEMOS   OUTPUT AREA FOR HUT/PUTS                     
         LA    R5,HPTBUFF          POINT TO FIRST DEMO ON THE                   
*                                   ACCUMULATOR BUFFER                          
         LHI   R0,NHDEMSQ          NUMBER OF DEMOS TO BE AVERAGED               
RELAVG15 LM    RE,RF,0(R5)         DOUBLE-WORD ACCUMULATED VALUE                
         DR    RE,R1               / NUMBER OF RECORDS                          
         ST    RF,0(R4)            =TRUNCATED AVERAGE                           
*                                                                               
         LA    R4,4(R4)            NEXT DEMO                                    
         LA    R5,8(R5)                                                         
         BCT   R0,RELAVG15                                                      
*                                                                               
         LA    R4,OUT.W_HPSDEMOS   OUTPUT AREA FOR MARKET TOTAL                 
         LA    R5,HPSBUFF          POINT TO FIRST DEMO ON THE                   
*                                   ACCUMULATOR BUFFER                          
         LHI   R0,NQDEMSQ          NUMBER OF DEMOS TO BE AVERAGED               
RELAVG17 LM    RE,RF,0(R5)         DOUBLE-WORD ACCUMULATED VALUE                
         DR    RE,R1               / NUMBER OF RECORDS                          
         ST    RF,0(R4)            =TRUNCATED AVERAGE                           
*                                                                               
         LA    R4,4(R4)            NEXT DEMO                                    
         LA    R5,8(R5)                                                         
         BCT   R0,RELAVG17                                                      
*                                                                               
         LA    R4,OUT.W_STRDEMOS   OUTPUT AREA FOR TSA IMPRESSIONS              
         LA    R5,STRBUFF          POINT TO FIRST DEMO ON THE                   
*                                   ACCUMULATOR BUFFER                          
         LHI   R0,NTDEMSQ          NUMBER OF DEMOS TO BE AVERAGED               
RELAVG20 LM    RE,RF,0(R5)         DOUBLE-WORD ACCUMULATED VALUE                
         DR    RE,R1               / NUMBER OF RECORDS                          
         ST    RF,0(R4)            =TRUNCATED AVERAGE                           
*                                                                               
         LA    R4,4(R4)            NEXT DEMO                                    
         LA    R5,8(R5)                                                         
         BCT   R0,RELAVG20                                                      
*                                                                               
         MVC   OUT.W_KAWKS,ACTWEEKS    ADD ACTUAL WEEKS TO AVERAGE RECD         
         MVC   OUT.W_KDAYMAP,ACTDAYS   ADD DAY MAP TO AVERAGE RECD              
         MVC   OUT.W_PNRLQH,ADDCOUNT   ADD TOTAL NUMBER OF QHS                  
*                                                                               
* DERIVE THE DETAILED "REPORT" START QUARTER HOUR                               
         BAS   RE,DREPSTIM                                                      
*                                                                               
* DERIVE THE DETAILED "REPORT" END QUARTER HOUR                                 
         BAS   RE,DREPETIM                                                      
*                                                                               
         CLI   OUT.W_WEEKEND_AVG,W_WKND_EXCL       SET THE DAY CODE             
         BNE   RELAVG30                     ALPHA FOR THIS AVERAGE              
         CLI   WKDAY_COUNT,5                                                    
         BNE   RELAVG30                     IT WILL BE EITHER                   
         CLI   WKEND_COUNT,0                                                    
         BNE   RELAVG30                                                         
         CLC   OUT.W_NORAVALF,=AL3(MF_ALF)                                      
         BNE   RELAVG30                                                         
         MVC   OUT.W_KDAY_ALF,=AL3(MF_ALF)  A 'M-F' AVERAGE                     
         B     RELAVG40                                                         
*                                                                               
RELAVG30 ZIC   R0,WKDAY_COUNT                                                   
         ZIC   R1,WKEND_COUNT                OR                                 
         AR    R0,R1                                                            
         CHI   R0,2                         AN 'AV' AVERAGE                     
         BNE   *+10                                                             
         MVC   OUT.W_KDAY_ALF,=AL3(AV2_ALF)                                     
         CHI   R0,3                                                             
         BNE   *+10                                                             
         MVC   OUT.W_KDAY_ALF,=AL3(AV3_ALF)                                     
         CHI   R0,4                                                             
         BNE   *+10                                                             
         MVC   OUT.W_KDAY_ALF,=AL3(AV4_ALF)                                     
         CHI   R0,5                                                             
         BNE   *+10                                                             
         MVC   OUT.W_KDAY_ALF,=AL3(AV5_ALF)                                     
         CHI   R0,6                                                             
         BNE   *+10                                                             
         MVC   OUT.W_KDAY_ALF,=AL3(AV6_ALF)                                     
         CHI   R0,7                                                             
         BNE   *+10                                                             
         MVC   OUT.W_KDAY_ALF,=AL3(AV7_ALF)                                     
*                                                                               
RELAVG40 B     ADDREC              RELEASE "NORMAL" AVERAGE                     
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
         SPACE 3                                                                
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
         LLC   R1,4(RC)            LENGTH OF EXIT NAME                          
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
* THE DETAILED "REPORT" START QH IS DERIVED ONLY FROM THE TELECASTS             
* THAT GO INTO THE CURRENT AVERAGE.                                             
* IT IS DEFINED AS THE EARLIEST OF THE MOST FREQUENT START QHS.                 
*                                                                               
* THE DETAILED "REPORT" START TIME WAS NOT PRESENT ON THE VIP, BUT              
* WE THOUGHT IT MIGHT BE USEFUL TO STORE THIS INFORMATION ON THE NEW            
* DEMO RECORDS CREATED FROM THE LOCAL MONTHLY.                                  
***********************************************************************         
*                                                                               
DREPSTIM DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
* STEP1: SORT TELECASTS ASCENDING BY START AND END QUARTER HOURS.               
*                                                                               
         L     RE,=A(TELTAB)                                                    
         ST    RE,DMCB                                                          
         GOTOR ,DMCB,,NUMVALS,TELTABQ,L'TELSQH+L'TELEQH,               +        
               TELSQH-TELTABD                                                   
         L     RF,=V(QSORT)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
* STEP2: GO THROUGH THE SORTED LIST AND DETERMINE THE EARLIEST OF               
*        THE MOST FREQUENT START QUARTER HOURS.                                 
*                                                                               
         L     R4,=A(TELTAB)                                                    
         USING TELTABD,R4                                                       
         L     R1,NUMVALS                                                       
*                                                                               
         MVC   OUT.W_PNRSQHD,TELSQH START OUT WITH EARLIEST QH                  
         MVI   PREV_SQH,X'FF'      PREVIOUS QH                                  
         SR    R5,R5               MAXIMUM FREQUENCY SO FAR                     
         SR    R0,R0               CURRENT FREQUENCY                            
*                                                                               
DREPST05 CLC   TELSQH,PREV_SQH                                                  
         BNE   DREPST10            CHANGE IN QH                                 
         AHI   R0,1                                                             
         B     DREPST20                                                         
*                                                                               
DREPST10 CR    R0,R5               DID PREV QH HAVE THE HIGHEST FREQ?           
         BNH   DREPST15                                                         
         MVC   OUT.W_PNRSQHD,PREV_SQH  YES. USE IT AS "REPORT" START TM         
         LR    R5,R0               UPDATE MAXIMUM FREQUENCY                     
DREPST15 LHI   R0,1                START NEW COUNT                              
*                                                                               
DREPST20 MVC   PREV_SQH,TELSQH     SAVE PREVIOUS QH                             
         LA    R4,TELTABQ(R4)                                                   
         BCT   R1,DREPST05                                                      
*                                                                               
         CR    R0,R5               DID LAST QH HAVE THE HIGHEST FREQ?           
         BNH   *+10                                                             
         MVC   OUT.W_PNRSQHD,PREV_SQH  YES. USE IT AS "REPORT" START TM         
         DROP  R4                                                               
*                                                                               
         L     RE,SAVERE                                                        
         BSM   0,RE                                                             
*                                                                               
***********************************************************************         
* THE DETAILED "REPORT" END QH IS DERIVED ONLY FROM THE TELECASTS               
* THAT GO INTO THE CURRENT AVERAGE.                                             
* IT IS DEFINED AS THE EARLIEST OF THE MOST FREQUENT END QHS ACROSS             
* THE TELECASTS THAT STARTED AT THE ALREADY ESTABLISHED DETAILED                
* "REPORT" START QH.                                                            
*                                                                               
* THE DETAILED "REPORT" END TIME WAS NOT PRESENT ON THE VIP, BUT                
* WE THOUGHT IT MIGHT BE USEFUL TO STORE THIS INFORMATION ON THE NEW            
* DEMO RECORDS CREATED FROM THE LOCAL MONTHLY.                                  
***********************************************************************         
*                                                                               
DREPETIM DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
* STEP1: GO THROUGH THE SORTED LIST BY START AND END QUARTER HOURS, AND         
*        FIND THE FIRST TELECAST STARTING AT THE "REPORT" START QH.             
*                                                                               
         L     R4,=A(TELTAB)                                                    
         USING TELTABD,R4                                                       
         L     R1,NUMVALS                                                       
*                                                                               
DREPET01 CLC   OUT.W_PNRSQHD,TELSQH                                             
         BE    *+14                                                             
         LA    R4,TELTABQ(R4)                                                   
         BCT   R1,DREPET01                                                      
         DC    H'0'               "REPORT" START TM SHOULD BE IN TABLE          
*                                                                               
* STEP2: DETERMINE THE EARLIEST OF THE MOST FREQUENT END QUARTER HOURS          
*        FOR TELECASTS STARTING AT THE "REPORT" START QH.                       
*                                                                               
         MVC   OUT.W_PNREQHD,TELEQH START OUT WITH EARLIEST QH                  
         MVI   PREV_EQH,X'FF'      PREVIOUS QH                                  
         SR    R5,R5               MAXIMUM FREQUENCY SO FAR                     
         SR    R0,R0               CURRENT FREQUENCY                            
*                                                                               
DREPET05 CLC   OUT.W_PNRSQHD,TELSQH                                             
         BNE   DREPET30            NO MORE TELECSTS FOR THIS REPORT SQH         
*                                                                               
         CLC   TELEQH,PREV_EQH                                                  
         BNE   DREPET10            CHANGE IN QH                                 
         AHI   R0,1                                                             
         B     DREPET20                                                         
*                                                                               
DREPET10 CR    R0,R5               DID PREV QH HAVE THE HIGHEST FREQ?           
         BNH   DREPET15                                                         
         MVC   OUT.W_PNREQHD,PREV_EQH  YES. USE IT AS "REPORT" END QH           
         LR    R5,R0               UPDATE MAXIMUM FREQUENCY                     
DREPET15 LHI   R0,1                START NEW COUNT                              
*                                                                               
DREPET20 MVC   PREV_EQH,TELEQH     SAVE PREVIOUS QH                             
         LA    R4,TELTABQ(R4)                                                   
         BCT   R1,DREPET05                                                      
*                                                                               
DREPET30 CR    R0,R5               DID LAST QH HAVE THE HIGHEST FREQ?           
         BNH   *+10                                                             
         MVC   OUT.W_PNREQHD,PREV_EQH  YES. USE IT AS "REPORT" END QH           
         DROP  R4                                                               
*                                                                               
         L     RE,SAVERE                                                        
         BSM   0,RE                                                             
*                                                                               
***********************************************************************         
* UPDATE DAY COUNTS OF TELECASTS THAT GO ONTO THE AVERAGE                       
***********************************************************************         
UPDAYCNT DS    0H                                                               
*                                                                               
         CLC   W_KDAY,PREV_KDAY                                                 
         BE    UPDCNTX             SAME DAY AS BEFORE. NO NEED TO               
         CLI   W_KDAY,SATURDAY     UPDATE                                       
         BE    UPDC10                                                           
         CLI   W_KDAY,SUNDAY                                                    
         BE    UPDC10                                                           
         ZIC   R0,WKDAY_COUNT      UPDATE WEEKDAY COUNT                         
         AHI   R0,1                                                             
         STC   R0,WKDAY_COUNT                                                   
         B     UPDCNTX                                                          
*                                                                               
UPDC10   ZIC   R0,WKEND_COUNT      UPDATE WEEKEND DAY COUNT                     
         AHI   R0,1                                                             
         STC   R0,WKEND_COUNT                                                   
*                                                                               
UPDCNTX  MVC   PREV_KDAY,W_KDAY                                                 
         BSM   0,RE                                                             
*                                                                               
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
         ORG   DELMXP7+(((*-DELMXP7)/256)+1)*256  FOR I-CACHE PIPELINE          
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
PREV_KDTYPE   DC CL(L'W_KDTYPE)' '                                              
PREV_PNRPNAME DC CL(L'W_PNRPNAME)' '                                            
PREV_PNRPGID  DC CL(L'W_PNRPGID)' '                                             
PREV_PNRSPGID  DC CL(L'W_PNRSPGID)' '                                           
PREV_KDAY     DC CL(L'W_KDAY)' '                                                
PREV_NORSQH   DC XL(L'W_NORSQH)'FF'                                             
PREV_NOREQH   DC XL(L'W_NOREQH)'FF'                                             
*                                                                               
PREV_SQH DS    HL(L'TELSQH)                                                     
PREV_EQH DS    HL(L'TELEQH)                                                     
*                                                                               
ACTWEEKS DC    CL4' '         ACTUAL WEEKS THE PROGRAM RAN                      
ACTDAYS  DC    CL28' '        ACTUAL DAYS THE PROGRAM RAN                       
*                                                                               
NUMVALS  DC    A(0)           CURRENT NUMBER OF VALUES IN TABLE                 
ADDCOUNT DC    H'0'           COUNTER FOR RECORDS THAT GO INTO AVERAGE          
*                                                                               
WKDAY_COUNT DS HL1            COUNTER FOR WEEKDAYS (MON THROUGH FRI)            
WKEND_COUNT DS HL1            COUNTER FOR WEEKEND DAYS (SAT AND SUN)            
*                                                                               
SKIP_FLAG DC   AL1(NOQ)                                                         
*                                                                               
BLANKS   DC    CL256' '                                                         
*                                                                               
QDRBUFF  DS    (NDEMSQ)D'0'   BUFFER FOR SUMMED DMA IMPS (24 DEMOS)             
NDEMSQ   EQU   24              24 DMA DEMOS                                     
QDRBUFFL EQU   *-QDRBUFF                                                        
*                                                                               
HPTBUFF  DS    (NHDEMSQ)D'0'  BUFFER FOR SUMMED HUT/PUTS (24 DEMOS)             
NHDEMSQ  EQU   24              24 DMA DEMOS                                     
HPTBUFFL EQU   *-HPTBUFF                                                        
*                                                                               
STRBUFF  DS    (NTDEMSQ)D'0'  BUFFER FOR SUMMED TSA IMPS (22 DEMOS)             
NTDEMSQ  EQU   22              22 TSA DEMOS                                     
STRBUFFL EQU   *-STRBUFF                                                        
*                                                                               
HPSBUFF  DS    (NQDEMSQ)D'0'  BUFFER FOR MARKET TOTALS   (22 DEMOS)             
NQDEMSQ  EQU   22              22 TSA DEMOS                                     
HPSBUFFL EQU   *-HPSBUFF                                                        
*                                                                               
         DS    0L                                                               
OUTREC   DS    CL(W_LMRECLQ)  OUTPUT RECORD                                     
*                                                                               
TELTAB   DS    1000CL(TELTABQ)                                                  
TELTABL  EQU   *-TELTAB                                                         
*                                                                               
TELTABD  DSECT                                                                  
TELSQH   DS    HL1            TELECAST START QH                                 
TELEQH   DS    HL1            TELECAST END QH                                   
TELNUM   DS    CL4            TELECAST ID                                       
TELTABQ  EQU   *-TELTABD                                                        
*                                                                               
         EJECT                                                                  
* INPUT RECORDS DSECT                                                           
         PRINT OFF                                                              
       ++INCLUDE DELMDSECT                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010DELMXP7   05/08/17'                                      
         END                                                                    
