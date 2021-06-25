*          DATA SET DELMXT1    AT LEVEL 006 AS OF 05/08/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMXT1A                                                                 
***********************************************************************         
* DFSORT USER EXIT FOR THE LOCAL MONTHLY M-F WEEKLY AVERAGE.          *         
* IT DERIVES THE PROGRAM NAME FOR THE WEEK BASED ON THE PREDOMINANT   *         
* PROGRAM.                                                            *         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DELMICETPA, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.         *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
***********************************************************************         
ICETAX1  CSECT                                                                  
*                                                                               
         ENTRY E15                 MUST BE "E15" (DFSORT INPUT EXIT)            
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E15,RC              RC = PROGRAM BASE REGISTER                   
E15      STM   RE,RC,12(RD)        SAVE ALL REGS EXCEPT RD                      
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
**********************************************************************          
* DETERMINE THE PREDOMINANT PROGRAM IN A WEEK.                                  
*                                                                               
* INPUT: RECORDS ARE SORTED SO THAT THE 5 RECORDS FOR MON THROUGH FRI           
* -----  FOR A QH AND A WEEK ARE GROUPED TOGETHER. WITHIN THE WEEK,             
*        RECORDS WITH THE SAME PROGRAM NAMES ARE GROUPED TOGETHER.              
*                                                                               
*        THE 5 RECORDS FOR MON THROUGH FRI ARE FOLLOWED BY AN AVERAGE           
*        LINE, IDENTIFIED AS RECORD TYPE RECTYPE_MF_W.                          
*                                                                               
*        EXAMPLE: 10:00 PM, WEEK 1, CSI:MIAMI-CBS     ,THU                      
*                 10:00 PM, WEEK 1, CSI:NY-CBS        ,TUE                      
*                 10:00 PM, WEEK 1, ELEVNTH HR-CBS    ,FRI                      
*                 10:00 PM, WEEK 1, NUMB3RS-CBS       ,WED                      
*                 10:00 PM, WEEK 1, WITH-TRACE-CBS    ,MON                      
*                 AVERAGE LINE      ................   M-F                      
*                                                                               
* OUTPUT:THE PROGRAM NAME SLOT ON THE AVERAGE LINE WILL BE FILLED WITH          
* ------ THE PREDOMINANT PROGRAM NAME IN THE PREVIOUS 5 RECORDS.                
*        THE DAY MAP WILL REFLECT ALL DAYS THAT GO INTO THE AVERAGE.            
*                                                                               
*        RULES TO DETERMINE THE PREDOMINANT PROGRAM NAME:                       
*        - IF THE SAME PROGRAM RAN ALL TIMES, USE THAT PROG NAME.               
*        - IF THERE IS A PROGRAM THAT RAN AT LEAST 3 TIMES, BUT NOT ALL         
*          TIMES, USE THAT PROGRAM NAME AND FILL THE PROGRAM NAME               
*          BREAKOUT FLAG WITH THE '#' SIGN.                                     
*        - USE 'VARIOUS' IN ALL OTHER CASES (IT WILL BE 'VARIOUS'               
*          IN THE EXAMPLE ABOVE).                                               
*                                                                               
**********************************************************************          
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES: DO NOT RETURN                           
*                                                                               
         USING LMDSECT,R3                                                       
*                                                                               
CW10     CLI   W_RECTYPE,RECTYPE_MF_W   M-F AVERAGE LINE?                       
         BE    CW100             YES. GO FILL IN THE PROGRAM NAME               
*                                                                               
*                                NO. KEEP COUNT OF PROGRAM NAMES                
         CLC   PREV_PNAME,W_PNRPNAME                                            
         BNE   CW20                                                             
         ZIC   R1,CURR_COUNT     COUNT RECORDS WITH SAME PROGRAM NAME           
         AHI   R1,1                                                             
         STC   R1,CURR_COUNT                                                    
         B     CW40                                                             
CW20     OC    PREV_PNAME,PREV_PNAME  UNLESS IT'S THE 1ST PROG FOR THIS         
         BZ    *+8                     QUARTER HOUR,                            
         MVI   MULT_PROGS_FLAG,YESQ   FLAG MULTIPLE PROGRAM NAMES               
         CLC   CURR_COUNT,MAX_COUNT   CHECK AGAINST MAXIMUM                     
         BL    CW30                   LOWER THAN MAX. IGNORE IT.                
         BE    CW25                                                             
         MVC   MAX_COUNT,CURR_COUNT   HIGHER THAN MAX. THIS IS NEW MAX          
         MVC   PREDOM_PNRAREA,PREV_PNRAREA  SAVE PREDOMINANT PROG INFO          
         B     CW30                                                             
*                                     SAME COUNT AS MAX MEANS VARIOUS           
*                                     UNLESS WE FIND A NEW MAX.                 
CW25     XC    PREDOM_PNRAREA,PREDOM_PNRAREA                                    
*                                                                               
CW30     MVI   CURR_COUNT,1                                                     
         MVC   PREV_PNRAREA,W_PNRAREA                                           
*                                                                               
CW40     LA    RE,W_KDAYMAP        ACCUMULATE THE WEEKDAYS                      
         LA    RF,ACTDAYS           IN ACTDAYS                                  
         LHI   R0,28               MAX 28 DAYS                                  
CW45     CLI   0(RE),C' '                                                       
         BE    *+10                                                             
         MVC   0(1,RF),0(RE)       ADD DAY INDICATOR TO ACTDAYS                 
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,CW45                                                          
*                                                                               
         B     DELREC            DROP INDIVIDUAL DAY RECORDS                    
*                                                                               
*                                                                               
CW100    CLC   CURR_COUNT,MAX_COUNT   CHECK AGAINST MAXIMUM                     
         BL    CW110              LESS THAN MAX. IGNORE IT                      
         BE    CW115                                                            
         MVC   MAX_COUNT,CURR_COUNT  HIGHER THAN MAX, THIS IS NEW MAX           
         MVC   PREDOM_PNRAREA,PREV_PNRAREA  SAVE PREDOMINANT PROGRAM            
         B     CW110                                                            
*                                     SAME COUNT AS MAX MEANS VARIOUS           
CW115    XC    PREDOM_PNRAREA,PREDOM_PNRAREA                                    
*                                                                               
CW110    LA    RE,OUTREC                                                        
         LHI   RF,W_LMRECLQ                                                     
         LR    R0,R3                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0            MOVE INPUT RECORD TO OUTPUT AREA                
         JO    *+2              DESTRUCTIVE MOVE!                               
*                                                                               
         LA    R3,OUTREC                                                        
*                                                                               
         CLI   MAX_COUNT,3      LESS THAN 3 DAYS IS VARIOUS                     
         BL    CW140                                                            
         OC    PREDOM_PNRAREA,PREDOM_PNRAREA    NO PREDMOINANT PROGRAM          
         BZ    CW140            MUST BE VARIOUS                                 
*                                                                               
         CLI   MULT_PROGS_FLAG,YESQ   MULTIPLE PROGRAMS?                        
         BE    CW130                                                            
         MVC   W_PNRAREA,PREDOM_PNRAREA   NO. USE THE PREDOMINANT PROG          
         MVI   W_PNAMEBK,PNAME_SP                                               
         B     CW150                                                            
*                                                                               
CW130    MVC   W_PNRAREA,PREDOM_PNRAREA                                         
         MVI   W_PNAMEBK,PNAME_#                                                
         B     CW150                                                            
*                                  VARIOUS PROGRAMS                             
CW140    DS    0H                                                               
         MVC   W_PNRAREA,BLANKS                                                 
         MVC   W_PNRPNAME,=CL14'VARIOUS'    PNAME_VARIOUS                       
         MVI   W_PNAMEBK,PNAME_SP                                               
*                                                                               
CW150    MVC   W_KDAYMAP,ACTDAYS                                                
         DROP  R3                                                               
*                                                                               
         MVC   ACTDAYS,BLANKS      RESET DAY MAP                                
         MVI   CURR_COUNT,1        RESET CURRENT COUNT                          
         MVI   MAX_COUNT,0         RESET MAXIMUM COUNT                          
         XC    PREV_PNAME,PREV_PNAME   RESET PROGRAM NAME                       
         MVI   MULT_PROGS_FLAG,NOQ  RESET FLAG FOR MULTIPLE PROGRAMS            
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
         LTORG                                                                  
         EJECT                                                                  
         ORG   ICETAX1+(((*-ICETAX1)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
DFSORT_HIGH_HALVES DS 16F                                                       
         SPACE 3                                                                
*                                                                               
PREV_PNRAREA DC XL(L'W_PNRAREA)'00'   PREVIOUS PROGRAM'S INFORMATION            
         ORG   PREV_PNRAREA+(W_PNRPNAME-W_PNRAREA)                              
PREV_PNAME DS  CL(L'W_PNRPNAME)                                                 
         ORG                                                                    
*                                                                               
PREDOM_PNRAREA DS CL(L'W_PNRAREA)     PREDOMINANT PROGRAM'S INFORMATION         
         ORG   PREDOM_PNRAREA+(W_PNRPNAME-W_PNRAREA)                            
PREDOM_PNAME DS CL(L'W_PNRPNAME)                                                
         ORG                                                                    
*                                                                               
CURR_COUNT DC  HL1'1'                                                           
MAX_COUNT  DC  HL1'0'                                                           
ACTDAYS  DC    CL28' '        ACTUAL DAYS THE PROGRAM RAN                       
*                                                                               
MULT_PROGS_FLAG DC AL1(NOQ)    MULTIPLE PROGRAMS FLAG                           
NOQ      EQU   0                                                                
YESQ     EQU   1                                                                
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
**PAN#1  DC    CL21'006DELMXT1   05/08/17'                                      
         END                                                                    
