*          DATA SET DELMXT2    AT LEVEL 007 AS OF 05/08/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMXT2A                                                                 
***********************************************************************         
* DFSORT USER EXIT FOR THE LOCAL MONTHLY TIME PERIOD AVERAGES.        *         
* IT DETERMINES WHEN A 4-WEEK AVERAGE NEEDS TO BE CREATED, COMPUTES   *         
* THE AVERAGE, AND RETURNS A RECORD WITH ALL THE INFORMATION          *         
* PERTAINING TO THE AVERAGE. ALL RECORDS THAT GO INTO THE AVERAGE ARE *         
* DISCARDED.                                                          *         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DELMICETPA, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.         *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
***********************************************************************         
DELMXT2  CSECT                                                                  
*                                                                               
         ENTRY E15                 MUST BE "E15" (DFSORT INPUT EXIT)            
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E15,RC              RC = PROGRAM BASE REGISTER                   
E15      SAVE  (14,12),,DELMXT2                                                 
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
**********************************************************************          
* DETERMINES WHEN AN 4-WEEK AVERAGE SHOULD BE CREATED.                          
*                                                                               
* RULES: -CREATE A 4-WEEK AVERAGE WHEN MORE THAN ONE PROGRAM RAN IN             
* -----   THE SAME WEEKDAY AND QUARTER HR(FOR THE STATION, MARKET,ETC).         
*                                                                               
* INPUT:  RECORDS ARE SORTED BY WEEKDAY/QUARTER HOUR/PROGRAM NAME               
* -----   (WITHIN STATION/ MARKET/ ETC).                                        
*                                                                               
* OUTPUT: AN AVERAGE LINE WITH COMPUTED AVERAGE DEMOS, AND ALL THE              
* ------  OTHER INFORMATION PERTAINING TO THE AVERAGE.                          
*         THE RECORDS THAT GO INTO THE AVERAGE ARE DISCARDED.                   
*                                                                               
**********************************************************************          
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BNZ   MAIN20              NO                                           
         CLI   RELLAST,YES         WAS THE LAST AVERAGE RELEASED YET?           
         BE    EOF                 YES: EXIT                                    
         B     RELAVG              NO: RELEASE THE LAST AVERAGE                 
*                                                                               
         USING LMDSECT,R3                                                       
*                                                                               
MAIN20   OC    PREV_KDTYPE,PREV_KDTYPE     FIRST RECORD FOR THIS QH             
         BZ    ADDTOAVG                     ADD TO AVERAGE                      
         CLC   W_KDTYPE,PREV_KDTYPE        CHANGE IN DATA TYPE,                 
         BNE   RELAVG                       RELEASE AVERAGE                     
         CLC   W_KDAY,PREV_KDAY            CHANGE IN WEEKDAY                    
         BNE   RELAVG                       RELEASE AVERAGE                     
         CLC   W_KQHR,PREV_KQHR            CHANGE IN QUARTER HOUR               
         BNE   RELAVG                       RELEASE AVERAGE                     
*                                                                               
         CLC   W_PNRPNAME,PREV_PNRPNAME    CHANGE IN PROGRAM NAME               
         BNE   MAIN30                                                           
         CLC   W_PNAMEBK,PREV_PNAMEBK      OR PROGRAM BREAKOUT FLAG             
         BE    *+8                                                              
MAIN30   MVI   MULTI_PROGS,YES             SET FLAG FOR MULTIPLE PROGRS         
*                                                                               
***********************************************************************         
* ADD RECORD TO THE AVERAGE BUFFER, AND DISCARD THE CURRENT RECORD.             
***********************************************************************         
ADDTOAVG DS    0H                                                               
*                                                                               
         LA    RE,OUTREC           ..MOVE CURRENT RECORD TO OUTREC              
         LA    RF,W_LMRECLQ                                                     
         LR    R0,R3                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         JO    *+2                 DESTRUCTIVE MOVE!                            
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
*                                  ACCUMULATE TSA IMPRESSIONS                   
         LA    R4,W_STRDEMOS       POINT TO FIRST DEMO ON THE RECORD            
         LA    R5,STRBUFF          POINT TO FIRST DEMO ON THE                   
*                                   ACCUMULATOR BUFFER                          
         LHI   R0,NTDEMSQ          NUMBER OF DEMOS TO BE ADDED                  
ADDTA30  ICM   R1,15,0(R4)         RE = DEMO VALUE TO BE ADDED                  
         LM    RE,RF,0(R5)         DOUBLE-WORD ACCUMULATOR                      
         ALR   RF,R1               ADD NEW DEMO AND SET CARRY IN CC             
         ALC   RE,=F'0'            ADD CARRY TO HI-ORDER PORTION OF SUM         
         STM   RE,RF,0(R5)         SET RESULT BACK IN ACCUMULATOR BUFF          
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,ADDTA30                                                       
*                                                                               
         MVC   PREV_KDTYPE,W_KDTYPE       SAVE AS PREVIOUS VALUES               
         MVC   PREV_KDAY,W_KDAY                                                 
         MVC   PREV_KQHR,W_KQHR                                                 
         MVC   PREV_PNRPNAME,W_PNRPNAME                                         
         MVC   PREV_PNAMEBK,W_PNAMEBK                                           
         LH    R1,ADDCOUNT         UPDATE THE NUMBER OF RECORDS THAT            
         LA    R1,1(R1)             GO INTO THE AVERAGE                         
         STH   R1,ADDCOUNT                                                      
*                                                                               
         B     DELREC              DISCARD THIS RECORD                          
*                                                                               
***********************************************************************         
* IF MULTIPLE PROGRAMS RAN IN THE SAME QUARTER HOUR, COMPUTE AND                
* RELEASE THE AVERAGE RECORD.                                                   
*                                                                               
* COMPUTE THE AVERAGE BY DIVIDING THE ACCUMULATED DEMOS BY THE                  
* NUMBER OF RECORDS THAT WENT INTO IT. TRUNCATE THE RESULT.                     
***********************************************************************         
RELAVG   DS    0H                                                               
*                                                                               
OUT      USING LMDSECT,OUTREC                                                   
*                                  IF NOT MULTI-PROGRAM, FLAG THIS              
         CLI   MULTI_PROGS,YES      AVERAGE AS TEMPORARY. IT WILL BE            
         BE    *+8                  DISCARDED BY THE ICETOOL JOB.               
         MVI   OUT.W_RECTYPE,RECTYPE_TEMP                                       
*                                                                               
         MVI   OUT.W_PNRAREA,C' '   PUT BLANKS IN PROGRAM INFORMATION           
         MVC   OUT.W_PNRAREA+1(L'OUT.W_PNRAREA-1),OUT.W_PNRAREA                 
*                                                                               
         LH    R1,ADDCOUNT                                                      
         LTR   R1,R1                                                            
         BNZ   RELAVG05                                                         
*                                  COUNT IS ZERO FOR UNKNOWN REASON             
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR               ERROR EXIT                                   
*                                                                               
RELAVG05 LA    R4,OUT.W_QDRDEMOS   OUTPUT AREA FOR DMA IMPRESSIONS              
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
         MVC   OUT.W_KAWKS,ACTWEEKS                                             
         MVC   OUT.W_KDAYMAP,ACTDAYS                                            
*                                                                               
RELAVG50 MVC   ACTWEEKS,BLANKS     RESET ACTIVITY WEEKS                         
         MVC   ACTDAYS,BLANKS      RESET DAY MAP                                
         XC    ADDCOUNT,ADDCOUNT   RESET RECORD COUNT                           
         MVI   MULTI_PROGS,NO      RESET FLAG FOR MULTIPLE PROGRS               
         XC    PREV_KDTYPE,PREV_KDTYPE                                          
*                                                                               
         LA    RE,QDRBUFF          CLEAR DMA DEMO BUFFER                        
         LHI   RF,QDRBUFFL                                                      
         XCEF                                                                   
         LA    RE,STRBUFF          CLEAR TSA DEMO BUFFER                        
         LHI   RF,STRBUFFL                                                      
         XCEF                                                                   
*                                                                               
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         MVI   RELLAST,YES         WE ARE RELEASING THE LAST AVERAGE            
*                                                                               
         B     ADDREC              ADD AVERAGE RECORD IN OUTREC                 
*                                                                               
***********************************************************************         
* DFSORT ACTIONS                                                                
***********************************************************************         
*                                                                               
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
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         ORG   DELMXT2+(((*-DELMXT2)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
SAVERE   DS    F                   SAVED RE                                     
DFSORT_HIGH_HALVES DS 16F                                                       
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
THREE    DS    XL3                                                              
*                                                                               
PREV_KDTYPE DC XL(L'W_KDTYPE)'00'                                               
PREV_PNRPNAME DS CL(L'W_PNRPNAME)                                               
PREV_PNAMEBK DS CL(L'W_PNAMEBK)                                                 
PREV_KDAY DS CL(L'W_KDAY)                                                       
PREV_KQHR DS CL(L'W_KQHR)                                                       
*                                                                               
RELLAST  DC    AL1(NO)        RELEASED LAST RECORD FLAG                         
MULTI_PROGS DC AL1(NO)                                                          
NO       EQU   0                                                                
YES      EQU   1                                                                
*                                                                               
ACTWEEKS DC    CL4' '         ACTUAL WEEKS THE PROGRAM RAN                      
ACTDAYS  DC    CL28' '        ACTUAL DAYS THE PROGRAM RAN                       
*                                                                               
ADDCOUNT DC    H'0'           COUNTER FOR RECORDS THAT GO INTO AVERAGE          
*                                                                               
QDRBUFF  DS    (NDEMSQ)D'0'   BUFFER FOR SUMMED DMA IMPS (24 DEMOS)             
NDEMSQ   EQU   24              24 DMA DEMOS                                     
QDRBUFFL EQU   *-QDRBUFF                                                        
*                                                                               
STRBUFF  DS    (NTDEMSQ)D'0'  BUFFER FOR SUMMED TSA IMPS (22 DEMOS)             
NTDEMSQ  EQU   22              22 TSA DEMOS                                     
STRBUFFL EQU   *-STRBUFF                                                        
*                                                                               
BLANKS   DC    CL256' '                                                         
*                                                                               
         DS    0L                                                               
OUTREC   DS    CL(W_LMRECLQ)  OUTPUT RECORD                                     
*                                                                               
         EJECT                                                                  
* INPUT RECORDS DSECT                                                           
         PRINT OFF                                                              
       ++INCLUDE DELMDSECT                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DELMXT2   05/08/17'                                      
         END                                                                    
